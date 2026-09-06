use std::{path::Path, sync::Arc};

use chrono::Datelike;
use typst::{
    foundations::{Bytes, Datetime},
    syntax::{FileId, Source, VirtualPath},
    text::{Font, FontBook},
    utils::LazyHash,
    Library, LibraryExt, World,
};

pub struct SandboxedWorld {
    library: LazyHash<Library>,
    pub(crate) fontbook: LazyHash<FontBook>,
    fonts: Vec<Font>,

    source_id: FileId,
}

pub struct InitialisedWorld {
    sandbox: Arc<SandboxedWorld>,
    source: Source,
    now: chrono::DateTime<chrono::Utc>,
}

impl SandboxedWorld {
    pub fn new() -> Self {
        let mut fontbook = FontBook::new();
        let mut fonts = vec![];

        #[cfg(feature = "embed-fonts")]
        {
            #[cfg(feature = "embed-emoji")]
            const FONT_COUNT: usize = 2;
            #[cfg(not(feature = "embed-emoji"))]
            const FONT_COUNT: usize = 1;

            const EMBEDDED_FONTS: [&[u8]; FONT_COUNT] = [
                #[cfg(feature = "embed-emoji")]
                include_bytes!("../assets/fonts/NotoColorEmoji.ttf"),
                include_bytes!("../assets/fonts/nasin-nanpa-4.0.2.otf"),
            ];

            for file in typst_assets::fonts() {
                for font in Font::iter(Bytes::new(file)) {
                    fontbook.push(font.info().clone());
                    fonts.push(font);
                }
            }

            for file in EMBEDDED_FONTS {
                for font in Font::iter(Bytes::new(file)) {
                    fontbook.push(font.info().clone());
                    fonts.push(font);
                }
            }
        }

        #[cfg(feature = "load-fonts")]
        {
            if let Err(err) = Self::load_fonts(&mut fontbook, &mut fonts) {
                tracing::error!("Error while loading fonts: {err:?}");
            }
        }

        let source_id = FileId::new_fake(VirtualPath::new("<source>"));

        Self {
            library: LazyHash::new(typst::Library::default()),
            fontbook: LazyHash::new(fontbook),
            fonts,
            source_id,
        }
    }

    pub fn load_fonts(
        fontbook: &mut FontBook,
        fonts: &mut Vec<Font>,
    ) -> Result<(), std::io::Error> {
        let font_paths = ["/usr/share/fonts", "/usr/local/share/fonts"].map(Path::new);

        for dir in font_paths {
            // Intentially ignoring TOCTOU attacks here
            if dir.exists() {
                for entry in walkdir::WalkDir::new(dir).follow_links(true) {
                    let entry = entry?;

                    match entry.path().extension().and_then(|s| s.to_str()) {
                        Some("ttf" | "otf" | "ttc" | "otc" | "TTF" | "OTF" | "TTC" | "OTC") => {
                            let contents = std::fs::read(entry.path())?;

                            for font in Font::iter(Bytes::new(contents)) {
                                fontbook.push(font.info().clone());
                                fonts.push(font);
                            }
                        }
                        _ => (),
                    }
                }
            }
        }

        Ok(())
    }

    pub fn with_source(self: Arc<Self>, source: &str) -> InitialisedWorld {
        let source_id = self.source_id;

        InitialisedWorld {
            sandbox: self,
            source: Source::new(source_id, source.into()),
            now: chrono::Utc::now(),
        }
    }
}

impl World for InitialisedWorld {
    fn library(&self) -> &LazyHash<Library> {
        &self.sandbox.library
    }

    fn main(&self) -> typst::syntax::FileId {
        self.source.id()
    }

    fn source(&self, id: typst::syntax::FileId) -> typst::diag::FileResult<typst::syntax::Source> {
        if id == self.source.id() {
            Ok(self.source.clone())
        } else {
            Err(typst::diag::FileError::AccessDenied)
        }
    }

    fn book(&self) -> &LazyHash<FontBook> {
        &self.sandbox.fontbook
    }

    fn font(&self, id: usize) -> Option<Font> {
        self.sandbox.fonts.get(id).cloned()
    }

    fn file(&self, _id: typst::syntax::FileId) -> typst::diag::FileResult<Bytes> {
        Err(typst::diag::FileError::AccessDenied)
    }

    fn today(&self, _offset: Option<i64>) -> Option<Datetime> {
        let now = self.now.with_timezone(&chrono::Local).fixed_offset();

        Datetime::from_ymd(now.year() as _, now.month() as _, now.day() as _)
    }
}
