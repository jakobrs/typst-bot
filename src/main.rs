use std::{fmt::Display, fmt::Write, io::Cursor, sync::Arc};

use poise::CreateReply;
use serenity::{builder::CreateAttachment, client::ClientBuilder, prelude::GatewayIntents};
use thiserror::Error;
use typst::{eval::Tracer, visualize::Rgb};

mod calc;
mod world;

struct Data {
    world: Arc<world::SandboxedWorld>,
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
    fn as_ansi_block(
        &self,
        source: &str,
        template_len: usize,
        _world: &dyn typst::World,
    ) -> String {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        use std::io::Write;

        let source = &source[template_len..];

        let mut output = Vec::new();
        let mut output_cursor = Cursor::new(&mut output);
        // let mut colors = ColorGenerator::new();
        for error in &self.0 {
            output_cursor.write(b"```ansi\n").unwrap();

            // let mut range = (*world).range(error.span).unwrap();
            let mut range = 0..0;
            range.start -= template_len;
            range.end -= template_len;

            Report::build(ReportKind::Error, "source.typ", range.start)
                .with_message("Compilation error")
                .with_label(
                    Label::new(("source.typ", range))
                        .with_color(Color::Yellow)
                        .with_message(&error.message),
                )
                .finish()
                .write_for_stdout(("source.typ", Source::from(source)), &mut output_cursor)
                .unwrap();

            output_cursor.write(b"```\n").unwrap();
        }

        String::from_utf8(output).unwrap()
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
}

#[derive(Clone, Copy)]
enum Theme {
    Light,
    Dark,
    Black,
    Transparent,
}

impl Theme {
    fn from_command_name(name: &str) -> Self {
        match name {
            "typst-light" | "typst-prose" => Theme::Light,
            "typst-dark" => Theme::Dark,
            "typst-black" => Theme::Black,
            "typst-transparent" | "typst-trans" => Theme::Transparent,
            _ => Theme::Dark,
        }
    }

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

struct RenderConfig {
    theme: Theme,
    prose: bool,
}

fn template(rest: &str, config: &RenderConfig) -> (String, usize) {
    let mut templated = String::new();

    if config.prose {
        templated += "#set page(width: 15cm, height: auto, margin: 1cm)\n";
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
    templated += rest;

    (templated, template_len)
}

/// Renders Typst code in a sandbox.
///
/// Usage: -typst (code)
///
/// Use one of these aliases to customise the template and theme:
///
/// Aliases:
/// - typst-light
/// - typst-dark
/// - typst-black
/// - typst-transparent
/// - typst-trans
/// - typst-prose
#[poise::command(
    prefix_command,
    track_edits,
    broadcast_typing,
    aliases(
        "typst-light",
        "typst-dark",
        "typst-black",
        "typst-transparent",
        "typst-trans",
        "typst-prose"
    )
)]
async fn typst(
    ctx: Context<'_>,
    #[description = "Code"]
    #[rest]
    rest: String,
) -> Result<(), Error> {
    let world = ctx.data().world.clone();

    let config = RenderConfig {
        theme: Theme::from_command_name(ctx.invoked_command_name()),
        prose: ctx.invoked_command_name() == "typst-prose",
    };

    let (templated_source, template_len) = template(&rest, &config);

    let with_source = Arc::new(world.with_source(&templated_source));

    let image = tokio::task::spawn_blocking({
        let with_source = with_source.clone();
        move || {
            let mut tracer = Tracer::new();
            let document =
                typst::compile(&*with_source, &mut tracer).map_err(|a| SourceErrors(a.to_vec()))?;
            if let [page] = &document.pages[..] {
                let pixmap =
                    typst_render::render(&page.frame, 10., config.theme.background_colour());
                Ok(pixmap.encode_png()?)
                // Err(RenderError::TooManyPages) // TODO
            } else {
                Err(RenderError::TooManyPages)
            }
        }
    })
    .await?;

    match image {
        Ok(image) => {
            ctx.send(
                CreateReply::default()
                    .attachment(CreateAttachment::bytes(image, "typst.png"))
                    .reply(true),
            )
            .await?;
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

        calc::evaluate(expression_tree, &*calc::DEFAULT_LOOKUP_CONTEXT)
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
/// Usage: -lex expression
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

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let world = world::SandboxedWorld::new();

    let token = std::env::var("BOT_TOKEN").expect("Missing BOT_TOKEN env var");
    let intents = GatewayIntents::non_privileged() | GatewayIntents::MESSAGE_CONTENT;

    let framework = poise::Framework::builder()
        .options(poise::FrameworkOptions {
            commands: vec![typst(), fonts(), calc(), lex(), parse(), help()],
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
