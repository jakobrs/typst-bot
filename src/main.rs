use std::{borrow::Cow, fmt::Display, sync::Arc};

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
        // has to do for now
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl From<Vec<typst::diag::SourceError>> for SourceErrors {
    fn from(value: Vec<typst::diag::SourceError>) -> Self {
        Self(value)
    }
}

impl std::error::Error for SourceErrors {}

#[derive(Error, Debug)]
enum RenderError {
    #[error("Compilation errors: {0}")]
    SourceError(#[from] SourceErrors),
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

fn template(rest: &str, config: &RenderConfig) -> String {
    let mut templated = String::new();

    if config.prose {
        templated += "#set page(width: 15cm, height: auto, margin: 1cm)\n";
    } else {
        templated += "#set page(width: auto, height: auto, margin: 0.5cm)\n";
    }

    templated += "#set text(";
    templated += config.theme.foreground_colour();
    templated += ")\n";

    templated += "\n";
    templated += rest;

    templated
}

/// Renders Typst code in a sandbox
#[poise::command(
    prefix_command,
    track_edits,
    broadcast_typing,
    aliases(
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

    let theme = match ctx.invoked_command_name() {
        "typst-transparent" | "typst-trans" => Theme::Transparent,
        "typst-black" => Theme::Black,
        "typst-dark" => Theme::Dark,
        _ => Theme::Light,
    };
    let prose = ctx.invoked_command_name() == "typst-prose";
    let config = RenderConfig { theme, prose };

    let templated_source = template(&rest, &config);

    let with_source = world.with_source(&templated_source);

    let image = tokio::task::spawn_blocking(move || {
        let document = typst::compile(&with_source).map_err(|a| SourceErrors(*a))?;
        if let [page] = &document.pages[..] {
            let pixmap = typst::export::render(page, 10., config.theme.background_colour());
            Ok(pixmap.encode_png()?)
        } else {
            Err(RenderError::TooManyPages)
        }
    })
    .await??;

    ctx.send(|reply| {
        reply
            .attachment(AttachmentType::Bytes {
                data: Cow::Owned(image),
                filename: "typst.png".into(),
            })
            .reply(true)
    })
    .await?;

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
            commands: vec![typst()],
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
