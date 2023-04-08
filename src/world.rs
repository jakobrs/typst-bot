use std::{path::Path, sync::Arc};

use comemo::Prehashed;
use typst::{
    eval::Library,
    font::{Font, FontBook},
    syntax::Source,
    World,
};

pub struct SandboxedWorld {
    library: Prehashed<Library>,
    pub(crate) fontbook: Prehashed<FontBook>,
    fonts: Vec<Font>,
}

pub struct WithSource {
    sandbox: Arc<SandboxedWorld>,
    pub source: Source,
}

impl SandboxedWorld {
    pub fn new() -> Self {
        let mut fontbook = FontBook::new();
        let mut fonts = vec![];

        #[cfg(feature = "embed-fonts")]
        {
            #[cfg(feature = "embed-emoji")]
            const FONT_COUNT: usize = 16;
            #[cfg(not(feature = "embed-emoji"))]
            const FONT_COUNT: usize = 14;

            const EMBEDDED_FONTS: [&[u8]; FONT_COUNT] = [
                include_bytes!("../assets/fonts/DejaVuSansMono.ttf"),
                include_bytes!("../assets/fonts/DejaVuSansMono-Bold.ttf"),
                include_bytes!("../assets/fonts/DejaVuSansMono-Oblique.ttf"),
                include_bytes!("../assets/fonts/DejaVuSansMono-BoldOblique.ttf"),
                include_bytes!("../assets/fonts/LinLibertine_R.ttf"),
                include_bytes!("../assets/fonts/LinLibertine_RB.ttf"),
                include_bytes!("../assets/fonts/LinLibertine_RBI.ttf"),
                include_bytes!("../assets/fonts/LinLibertine_RI.ttf"),
                include_bytes!("../assets/fonts/NewCM10-Regular.otf"),
                include_bytes!("../assets/fonts/NewCM10-Bold.otf"),
                include_bytes!("../assets/fonts/NewCM10-Italic.otf"),
                include_bytes!("../assets/fonts/NewCM10-BoldItalic.otf"),
                include_bytes!("../assets/fonts/NewCMMath-Book.otf"),
                include_bytes!("../assets/fonts/NewCMMath-Regular.otf"),
                #[cfg(feature = "embed-emoji")]
                include_bytes!("../assets/fonts/NotoColorEmoji.ttf"),
                #[cfg(feature = "embed-emoji")]
                include_bytes!("../assets/fonts/TwitterColorEmoji.ttf"),
            ];

            for file in EMBEDDED_FONTS {
                for font in Font::iter(typst::util::Buffer::from_static(file)) {
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

        Self {
            library: Prehashed::new(typst_library::build()),
            fontbook: Prehashed::new(fontbook),
            fonts,
        }
    }

    pub fn load_fonts(
        fontbook: &mut FontBook,
        fonts: &mut Vec<Font>,
    ) -> Result<(), std::io::Error> {
        let font_paths = ["/usr/share/fonts", "/usr/local/share/fonts"].map(Path::new);

        for dir in font_paths {
            for entry in walkdir::WalkDir::new(dir).follow_links(true) {
                let entry = entry?;

                match entry.path().extension().and_then(|s| s.to_str()) {
                    Some("ttf" | "otf" | "ttc" | "otc" | "TTF" | "OTF" | "TTC" | "OTC") => {
                        // NOTE: this is probably a bad idea
                        let contents: &'static [u8] = std::fs::read(entry.path())?.leak();

                        for font in Font::iter(typst::util::Buffer::from_static(contents)) {
                            fontbook.push(font.info().clone());
                            fonts.push(font);
                        }
                    }
                    _ => (),
                }
            }
        }

        Ok(())
    }

    pub fn with_source(self: Arc<Self>, source: &str) -> WithSource {
        WithSource {
            sandbox: self,
            source: Source::detached(source),
        }
    }
}

impl World for WithSource {
    fn library(&self) -> &Prehashed<Library> {
        &self.sandbox.library
    }

    fn main(&self) -> &typst::syntax::Source {
        &self.source
    }

    fn resolve(&self, _path: &std::path::Path) -> typst::diag::FileResult<typst::syntax::SourceId> {
        Err(typst::diag::FileError::AccessDenied)
    }

    fn source(&self, id: typst::syntax::SourceId) -> &typst::syntax::Source {
        if id == self.source.id() {
            &self.source
        } else {
            panic!("No")
        }
    }

    fn book(&self) -> &Prehashed<FontBook> {
        &self.sandbox.fontbook
    }

    fn font(&self, id: usize) -> Option<typst::font::Font> {
        self.sandbox.fonts.get(id).cloned()
    }

    fn file(&self, _path: &std::path::Path) -> typst::diag::FileResult<typst::util::Buffer> {
        Err(typst::diag::FileError::AccessDenied)
    }
}
