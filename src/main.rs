use std::{
    fmt::{Display, Write},
    io::Cursor,
    sync::Arc,
};

use poise::CreateReply;
use serenity::{builder::CreateAttachment, client::ClientBuilder, prelude::GatewayIntents};
use smallvec::SmallVec;
use thiserror::Error;
use typst::{eval::Tracer, visualize::Rgb};

mod arg_parser;
mod calc;
mod oeis;
mod ordliste;
mod world;
mod xsampa;

struct Data {
    world: Arc<world::SandboxedWorld>,
    dictionary: ordliste::Dictionary<'static>,
    oeis: oeis::Context,
}

type Error = TypstBotError;
type Context<'a> = poise::Context<'a, Data, Error>;

#[derive(Debug)]
struct SourceErrors(Vec<typst::diag::SourceDiagnostic>);

impl Display for SourceErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // this will have to do for now
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl From<Vec<typst::diag::SourceDiagnostic>> for SourceErrors {
    fn from(value: Vec<typst::diag::SourceDiagnostic>) -> Self {
        Self(value)
    }
}

impl std::error::Error for SourceErrors {}

impl SourceErrors {
    fn as_ansi_block(&self, source: &str, template_len: usize, world: &dyn typst::World) -> String {
        use ariadne::{Label, Report, ReportKind, Source};
        use std::io::Write;

        let text_source = &source[template_len..];

        static COLORS: &[ariadne::Color] = &[
            ariadne::Color::Red,
            ariadne::Color::Green,
            ariadne::Color::Yellow,
            ariadne::Color::Blue,
            ariadne::Color::Cyan,
        ];

        struct Config {
            color: bool,
            hints: bool,
        }

        let try_generate = |config: Config| {
            let Config { color, hints } = config;

            let mut report =
                Report::build(ReportKind::Error, "source.typ", 0).with_message("Compilation error");
            let mut color_idx = 0;
            for error in &self.0 {
                let source = world.source(error.span.id().unwrap()).unwrap();
                let mut range = source.range(error.span).unwrap();
                range.start -= template_len;
                range.end -= template_len;

                let mut label = Label::new(("source.typ", range))
                    .with_color(COLORS[color_idx])
                    .with_message(&error.message);

                if hints {
                    for hint in &error.hints {
                        label = label.with_message(hint);
                    }
                }

                report = report.with_label(label);
                color_idx = (color_idx + 1) % COLORS.len();
            }

            let mut output = Vec::new();
            let mut output_cursor = Cursor::new(&mut output);

            output_cursor.write_all(b"```ansi\n").unwrap();
            report
                .with_config(ariadne::Config::default().with_color(color))
                .finish()
                .write_for_stdout(
                    ("source.typ", Source::from(text_source)),
                    &mut output_cursor,
                )
                .unwrap();

            output_cursor.write_all(b"```\n").unwrap();

            eprintln!("Len: {}", output.len());

            (output.len() < 2000).then(|| String::from_utf8(output).unwrap())
        };

        #[cfg_attr(rustfmt, rustfmt_skip)]
        try_generate(Config { color: true, hints: true })
            .or_else(|| try_generate(Config { color: false, hints: true }))
            .or_else(|| try_generate(Config { color: false, hints: false }))
            .or_else(|| Some(format!("{:?}", self.0)))
            .unwrap()
    }
}

#[derive(Error, Debug)]
enum RenderError {
    #[error("Compilation errors: {0}")]
    SourceErrors(#[from] SourceErrors),
    #[error("PNG encoding error: {0}")]
    PngEncodingError(#[from] png::EncodingError),
    #[error("Too many pages")]
    TooManyPages,
}

#[derive(Error, Debug)]
enum TypstBotError {
    #[error("Join error: {0}")]
    JoinError(#[from] tokio::task::JoinError),
    #[error("Render error: {0}")]
    RenderError(#[from] RenderError),
    #[error("Serenity error: {0}")]
    SerenityError(#[from] serenity::Error),
    #[error("Calculation error: {0}")]
    CalcError(#[from] calc::CalcError),
    #[error("Reqwest error: {0:?}")]
    ReqwestError(#[from] reqwest::Error),
    #[error("Argument parser error: {0}")]
    ArgParserError(#[from] arg_parser::ArgParserError),
}

#[derive(Clone, Copy)]
enum Theme {
    Light,
    Dark,
    Black,
    Transparent,
}

impl Theme {
    fn background_colour(self) -> typst::visualize::Color {
        match self {
            Theme::Light => typst::visualize::Color::WHITE,
            Theme::Dark => {
                typst::visualize::Color::Rgb(Rgb::new(49. / 255., 51. / 255., 56. / 255., 1.))
            }
            Theme::Black => typst::visualize::Color::BLACK,
            Theme::Transparent => typst::visualize::Color::Rgb(Rgb::new(0., 0., 0., 0.)),
        }
    }

    fn foreground_colour(self) -> &'static str {
        match self {
            Theme::Light => "black",
            Theme::Dark | Theme::Black | Theme::Transparent => "white",
        }
    }
}

#[derive(Clone, Copy)]
enum Format {
    Png,
    Svg,
}

struct RenderConfig {
    format: Format,
    theme: Theme,
    fw: bool,
}

fn template(rest: &str, config: &RenderConfig) -> (String, usize) {
    let mut templated = String::new();

    if config.fw {
        templated += "#set page(width: 300pt, height: auto, margin: 10pt)\n";
    } else {
        templated += "#set page(width: auto, height: auto, margin: 0.5cm)\n";
    }

    templated += "#set text(";
    templated += config.theme.foreground_colour();
    templated += ")\n";

    templated += "
        #show <inline>: box
    ";

    templated += "\n";

    let template_len = templated.len();
    templated.reserve(rest.len());
    let fix = |ch: char| match ch {
        '‛' => '\'',
        '“' => '"',
        '”' => '"',
        ch => ch,
    };
    for ch in rest.chars() {
        templated.push(fix(ch));
    }

    (templated, template_len)
}

/// Renders Typst code in a sandbox.
///
/// Usage: -typst [flags...] (code)
///
/// Available flags:
/// - --svg:   generate svg
/// - --png:   generate png [default]
///
/// - --light: light bg
/// - --dark:  dark bg [default]
/// - --black: black bg
/// - --trans: transparent bg
/// - --fw:    fixed width [default]
/// - --no-fw: disable fixed width
/// - --prose: implies --fw, --light
#[poise::command(prefix_command, track_edits, broadcast_typing)]
async fn typst(
    ctx: Context<'_>,
    #[description = "Code"]
    #[rest]
    rest: String,
) -> Result<(), Error> {
    use arg_parser::ArgParser;

    let world = ctx.data().world.clone();

    let arg_parser = ArgParser::with_state(RenderConfig {
        format: Format::Png,
        theme: Theme::Transparent,
        fw: true,
    })
    .arg("svg", |cfg| cfg.format = Format::Svg)
    .arg("png", |cfg| cfg.format = Format::Png)
    .arg("light", |cfg| cfg.theme = Theme::Light)
    .arg("dark", |cfg| cfg.theme = Theme::Dark)
    .arg("black", |cfg| cfg.theme = Theme::Black)
    .arg("trans", |cfg| cfg.theme = Theme::Transparent)
    .arg("fw", |cfg| cfg.fw = true)
    .arg("no-fw", |cfg| cfg.fw = false)
    .arg("prose", |cfg| {
        cfg.theme = Theme::Transparent;
        cfg.fw = true
    });

    let (config, source) = arg_parser.run(&rest)?;
    let (templated_source, template_len) = template(source, &config);

    let with_source = Arc::new(world.with_source(&templated_source));

    struct CompileResult {
        pages: SmallVec<[Vec<u8>; 4]>,
    }

    let image = tokio::task::spawn_blocking({
        let with_source = with_source.clone();
        move || {
            let mut tracer = Tracer::new();
            let document =
                typst::compile(&*with_source, &mut tracer).map_err(|a| SourceErrors(a.to_vec()))?;

            if document.pages.len() > 4 || document.pages.len() < 1 {
                return Err(RenderError::TooManyPages);
            }

            let mut pages = SmallVec::new();
            for page in document.pages {
                match config.format {
                    Format::Svg => {
                        let data = typst_svg::svg(&page.frame);
                        pages.push(data.into());
                    }
                    Format::Png => {
                        let pixmap = typst_render::render(
                            &page.frame,
                            10.,
                            config.theme.background_colour(),
                        );
                        pages.push(pixmap.encode_png()?);
                    }
                }
            }

            Ok(CompileResult { pages })
        }
    })
    .await?;

    let filename = match config.format {
        Format::Svg => "typst.svg",
        Format::Png => "typst.png",
    };

    match image {
        Ok(CompileResult { pages }) => {
            let mut reply = CreateReply::default();
            for page in pages {
                reply = reply.attachment(CreateAttachment::bytes(page, filename));
            }
            ctx.send(reply.reply(true)).await?;
        }
        Err(RenderError::SourceErrors(errors)) => {
            ctx.send(
                CreateReply::default()
                    .content(errors.as_ansi_block(&templated_source, template_len, &*with_source))
                    .reply(true),
            )
            .await?;
        }
        Err(err) => return Err(err.into()),
    };

    Ok(())
}

/// Prints the list of loaded fonts
///
/// Usage: -fonts [with_variants]
#[poise::command(prefix_command)]
async fn fonts(ctx: Context<'_>, #[flag] with_variants: bool) -> Result<(), TypstBotError> {
    let world = ctx.data().world.clone();

    let mut message = String::new();

    for (name, fonts) in world.fontbook.families() {
        writeln!(&mut message, "Family: {name}").unwrap();

        if with_variants {
            for font_info in fonts {
                writeln!(
                    &mut message,
                    "-> Style: {:?}, weight: {:?}, strech: {:?}",
                    font_info.variant.style, font_info.variant.weight, font_info.variant.stretch,
                )
                .unwrap();
            }
        }
    }

    ctx.send(
        CreateReply::default()
            .attachment(CreateAttachment::bytes(message, "fonts.txt"))
            .reply(true),
    )
    .await?;

    Ok(())
}

/// Evaluates an expression
///
/// Usage: -calc expression
///
/// Python syntax. Use -calc-s for LISP syntax
#[poise::command(prefix_command)]
async fn calc(ctx: Context<'_>, #[rest] expr: String) -> Result<(), TypstBotError> {
    let value = {
        let expression_tree = calc::parse_python(&expr)?;

        calc::evaluate(expression_tree, &calc::DEFAULT_LOOKUP_CONTEXT)
            .map_err(calc::CalcError::EvaluationError)?
    };

    ctx.send(
        CreateReply::default()
            .reply(true)
            .content(format!("{value}")),
    )
    .await?;

    Ok(())
}

/// Lexes an expression
///
/// Usage: -lex expression
#[poise::command(prefix_command)]
async fn lex(ctx: Context<'_>, #[rest] expr: String) -> Result<(), TypstBotError> {
    let tokens = calc::python::lexer::lex(&expr).map_err(calc::CalcError::PythonLexerError)?;

    let mut result = String::new();
    for token in tokens {
        write!(result, "{token} ").unwrap();
    }
    result.pop();

    ctx.send(CreateReply::default().content(result).reply(true))
        .await?;

    Ok(())
}

/// Parses an expression
///
/// Usage: -parse expression
#[poise::command(prefix_command)]
async fn parse(ctx: Context<'_>, #[rest] expr: String) -> Result<(), TypstBotError> {
    let expression_tree = calc::parse_python(&expr)?;

    ctx.send(
        CreateReply::default()
            .content(format!("{expression_tree:?}"))
            .reply(true),
    )
    .await?;

    Ok(())
}

#[poise::command(prefix_command)]
async fn help(
    ctx: Context<'_>,
    #[description = "Command to show help about"] command: Option<String>,
) -> Result<(), TypstBotError> {
    poise::builtins::help(ctx, command.as_deref(), Default::default()).await?;

    Ok(())
}

#[poise::command(prefix_command)]
async fn version(ctx: Context<'_>) -> Result<(), TypstBotError> {
    ctx.send(
        CreateReply::default()
            .content("typst-bot using Typst version 0.11.0")
            .reply(true),
    )
    .await?;

    Ok(())
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let world = world::SandboxedWorld::new();

    let token = std::env::var("BOT_TOKEN").expect("Missing BOT_TOKEN env var");
    let intents = GatewayIntents::non_privileged() | GatewayIntents::MESSAGE_CONTENT;

    let framework = poise::Framework::builder()
        .options(poise::FrameworkOptions {
            commands: vec![
                typst(),
                fonts(),
                calc(),
                lex(),
                parse(),
                help(),
                version(),
                ordliste::commands::trans(),
                oeis::commands::oeis(),
                xsampa::commands::xsampa(),
            ],
            prefix_options: poise::PrefixFrameworkOptions {
                prefix: Some("-".into()),
                edit_tracker: Some(Arc::new(poise::EditTracker::for_timespan(
                    std::time::Duration::from_secs(180),
                ))),
                ..Default::default()
            },
            ..Default::default()
        })
        .setup(|_ctx, _ready, _framework| {
            // poise::builtins::register_globally(http, commands)
            Box::pin(async move {
                Ok(Data {
                    world: Arc::new(world),
                    dictionary: ordliste::Dictionary::new_default(),
                    oeis: oeis::Context::new(),
                })
            })
        })
        .build();

    let mut client = ClientBuilder::new(token, intents)
        .framework(framework)
        .await
        .unwrap();

    if let Err(err) = client.start().await {
        println!("Fatal error: {err:?}");
    }
}
