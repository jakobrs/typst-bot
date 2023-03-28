use std::sync::Arc;

use comemo::Prehashed;
use typst::{
    eval::Library,
    font::{Font, FontBook},
    syntax::Source,
    World,
};

pub struct SandboxedWorld {
    library: Prehashed<Library>,
    fontbook: Prehashed<FontBook>,
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

        const EMBEDDED_FONTS: [&[u8]; 8] = [
            include_bytes!("../assets/fonts/LinLibertine_R.ttf"),
            include_bytes!("../assets/fonts/LinLibertine_RB.ttf"),
            include_bytes!("../assets/fonts/LinLibertine_RBI.ttf"),
            include_bytes!("../assets/fonts/LinLibertine_RI.ttf"),
            include_bytes!("../assets/fonts/NewCMMath-Book.otf"),
            include_bytes!("../assets/fonts/NewCMMath-Regular.otf"),
            include_bytes!("../assets/fonts/DejaVuSansMono.ttf"),
            include_bytes!("../assets/fonts/DejaVuSansMono-Bold.ttf"),
        ];

        for file in EMBEDDED_FONTS {
            for font in Font::iter(typst::util::Buffer::from_static(file)) {
                fontbook.push(font.info().clone());
                fonts.push(font);
            }
        }

        Self {
            library: Prehashed::new(typst_library::build()),
            fontbook: Prehashed::new(fontbook),
            fonts,
        }
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
