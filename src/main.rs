use std::{borrow::Cow, fmt::Display, io::Cursor, sync::Arc};

use poise::serenity_prelude::{AttachmentType, GatewayIntents};
use thiserror::Error;

mod world;

struct Data {
    world: Arc<world::SandboxedWorld>,
}

type Error = TypstBotError;
type Context<'a> = poise::Context<'a, Data, Error>;

#[derive(Debug)]
struct SourceErrors(Vec<typst::diag::SourceError>);

impl Display for SourceErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // this will have to do for now
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl From<Vec<typst::diag::SourceError>> for SourceErrors {
    fn from(value: Vec<typst::diag::SourceError>) -> Self {
        Self(value)
    }
}

impl std::error::Error for SourceErrors {}

impl SourceErrors {
    fn as_ansi_block(&self, source: &str, template_len: usize, world: &dyn typst::World) -> String {
        use ariadne::{Color, Label, Report, ReportKind, Source};
        use std::io::Write;

        let source = &source[template_len..];

        let mut output = Vec::new();
        let mut output_cursor = Cursor::new(&mut output);
        // let mut colors = ColorGenerator::new();
        for error in &self.0 {
            output_cursor.write(b"```ansi\n").unwrap();

            let mut range = error.range(world);
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

    fn background_colour(self) -> typst::geom::Color {
        match self {
            Theme::Light => typst::geom::Color::WHITE,
            Theme::Dark => typst::geom::Color::Rgba(typst::geom::RgbaColor {
                r: 0x31,
                g: 0x33,
                b: 0x38,
                a: 0xff,
            }),
            Theme::Black => typst::geom::Color::BLACK,
            Theme::Transparent => typst::geom::Color::Rgba(typst::geom::RgbaColor {
                r: 0,
                g: 0,
                b: 0,
                a: 0,
            }),
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
            let document = typst::compile(&*with_source).map_err(|a| SourceErrors(*a))?;
            if let [page] = &document.pages[..] {
                let pixmap = typst::export::render(page, 10., config.theme.background_colour());
                Ok(pixmap.encode_png()?)
            } else {
                Err(RenderError::TooManyPages)
            }
        }
    })
    .await?;

    match image {
        Ok(image) => {
            ctx.send(|reply| {
                reply
                    .attachment(AttachmentType::Bytes {
                        data: Cow::Owned(image),
                        filename: "typst.png".into(),
                    })
                    .reply(true)
            })
            .await?;
        }
        Err(RenderError::SourceErrors(errors)) => {
            ctx.send(|reply| {
                reply
                    .content(errors.as_ansi_block(&templated_source, template_len, &*with_source))
                    .reply(true)
            })
            .await?;
        }
        Err(err) => return Err(err.into()),
    };

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

    let framework = poise::Framework::builder()
        .token(std::env::var("BOT_TOKEN").expect("Missing BOT_TOKEN env var"))
        .intents(GatewayIntents::non_privileged() | GatewayIntents::MESSAGE_CONTENT)
        .options(poise::FrameworkOptions {
            commands: vec![typst(), help()],
            prefix_options: poise::PrefixFrameworkOptions {
                prefix: Some("-".into()),
                edit_tracker: Some(poise::EditTracker::for_timespan(
                    std::time::Duration::from_secs(180),
                )),
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
        });

    if let Err(err) = framework.run().await {
        println!("Fatal error: {err:?}");
    }
}
