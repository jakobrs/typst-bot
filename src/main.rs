use std::{borrow::Cow, fmt::Display, sync::Arc};

use poise::serenity_prelude::{AttachmentType, GatewayIntents};
use thiserror::Error;

mod world;

struct Data {
    world: Arc<world::SandboxedWorld>,
}

type Error = Box<dyn std::error::Error + Send + Sync>;
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

/// Renders Typst code in a sandbox
#[poise::command(prefix_command, track_edits, broadcast_typing)]
async fn typst(
    ctx: Context<'_>,
    #[description = "Code"]
    #[rest]
    rest: String,
) -> Result<(), Error> {
    let world = ctx.data().world.clone();

    let with_source = world.with_source(&rest);

    let result = tokio::task::spawn_blocking(move || {
        let document = typst::compile(&with_source).map_err(|a| SourceErrors(*a))?;
        if let [page] = &document.pages[..] {
            let pixmap = typst::export::render(page, 10., typst::geom::Color::WHITE);
            Ok(pixmap.encode_png()?)
        } else {
            Err(RenderError::TooManyPages)
        }
    })
    .await;

    match result {
        Ok(Ok(image)) => {
            match ctx
                .send(|reply| {
                    reply
                        .attachment(AttachmentType::Bytes {
                            data: Cow::Owned(image),
                            filename: "typst.png".into(),
                        })
                        .reply(true)
                })
                .await
            {
                Ok(_) => (),
                Err(err) => eprintln!("Failed to send message: {err:?}"),
            }
        }
        Ok(Err(sth)) => {
            match ctx
                .send(|reply| reply.content(format!("{sth}")).reply(true))
                .await
            {
                Ok(_) => (),
                Err(err) => eprintln!("Failed to reply with error message: {err:?}"),
            }
        }
        Err(err) => eprintln!("Join error: {err:?}"),
    }

    Ok(())
}

#[tokio::main]
async fn main() {
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
