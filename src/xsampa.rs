use once_cell::sync::Lazy;
use std::collections::HashMap;

fn x_sampa_to_ipa(x_sampa: &str) -> String {
    static MAPPING: Lazy<HashMap<&str, &str>> = Lazy::new(|| {
        [
            ("a", "a"),
            ("b", "b"),
            ("b_<", "ɓ"),
            ("c", "c"),
            ("d", "d"),
            ("d`", "ɖ"),
            ("e", "e"),
            ("f", "f"),
            ("g", "g"),
            ("g_<", "ɠ"),
            ("h", "h"),
            (r"h\", "ɦ"),
            ("i", "i"),
            ("j", "j"),
            (r"j\", "ʝ"),
            ("k", "k"),
            ("l", "l"),
            ("l`", "ɭ"),
            (r"l\", "ɺ"),
            ("m", "m"),
            ("n", "n"),
            ("n`", "ɳ"),
            ("o", "o"),
            ("p", "p"),
            (r"p\", "ɸ"),
            ("q", "q"),
            ("r", "r"),
            ("r`", "ɽ"),
            (r"r\", "ɹ"),
            (r"r\`", "ɻ"),
            ("s", "s"),
            ("s`", "ʂ"),
            (r"s\", "ɕ"),
            ("t", "t"),
            ("t`", "ʈ"),
            ("u", "u"),
            ("v", "v"),
            (r"v\", "ʋ"),
            ("P", "ʋ"),
            ("w", "w"),
            ("x", "x"),
            (r"x\", "ɧ"),
            ("y", "y"),
            ("z", "z"),
            ("z`", "ʐ"),
            (r"z\", "ʑ"),
            // Capital symbols
            ("A", "ɑ"),
            ("B", "β"),
            (r"B\", "ʙ"),
            ("C", "ç"),
            ("D", "ð"),
            ("E", "ɛ"),
            ("F", "ɱ"),
            ("G", "ɣ"),
            (r"G\", "ɢ"),
            (r"G\_<", "ʛ"),
            ("H", "ɥ"),
            (r"H\", "ʜ"),
            ("I", "ɪ"),
            (r"I\", "ᵻ"), // non-ipa
            ("J", "ɲ"),
            (r"J\", "ɟ"),
            (r"J\_<", "ʄ"),
            ("K", "ɬ"),
            (r"K\", "ɮ"),
            ("L", "ʎ"),
            (r"L\", "ʟ"),
            ("M", "ɯ"),
            (r"M\", "ɰ"),
            ("N", "ŋ"),
            (r"N\", "ɴ"),
            ("O", "ɔ"),
            (r"O\", "ʘ"),
            // P is listed above
            ("Q", "ɒ"),
            ("R", "ʁ"),
            (r"R\", "ʀ"),
            ("S", "ʃ"),
            ("T", "θ"),
            ("U", "ʊ"),
            (r"U\", "ᵿ"),
            ("V", "ʌ"),
            ("W", "ʍ"),
            ("X", "χ"),
            (r"X\", "ħ"),
            ("Y", "ʏ"),
            ("Z", "ʒ"),
            // Other symbols
            (".", "."),
            ("\"", "ˈ"),
            ("'", "ˈ"),
            ("%", "ˌ"),
            (",", "ˌ"),
            // todo: _j
            (":", "ː"),
            (r":\", "ˑ"),
            // todo: -
            ("@", "ə"),
            (r"@\", "ɘ"),
            ("@`", "ɚ"),
            ("{", "æ"),
            ("}", "ʉ"),
            ("1", "ɨ"),
            ("2", "ø"),
            ("3", "ɜ"),
            (r"3\", "ɞ"),
            ("4", "ɾ"),
            ("5", "ɫ"),
            ("6", "ɐ"),
            ("7", "ɤ"),
            ("8", "ɵ"),
            ("9", "œ"),
            ("&", "ɶ"),
            ("?", "ʔ"),
            (r"?\", "ʕ"),
            // todo: *
            // todo: /
            // todo: <
            (r"<\", "ʢ"),
            // todo: >
            (r">\", "ʡ"),
            ("^", "ꜛ"),
            ("!", "ꜜ"),
            (r"!\", "ǃ"),
            ("|", "|"),
            (r"|\", "ǀ"),
            ("||", "‖"),
            (r"|\|\", "ǁ"),
            (r"=\", "ǂ"),
            (r"-\", "‿"),
        ]
        .into_iter()
        .collect()
    });

    let mut i = 0;
    let mut out = String::new();
    'a: while i < x_sampa.len() {
        for j in (i + 1..=x_sampa.len().min(i + 4)).rev() {
            if let Some(replacement) = MAPPING.get(&x_sampa[i..j]) {
                out.push_str(replacement);
                i = j;
                continue 'a;
            }
        }
        out.push(x_sampa.as_bytes()[i] as char);
        i += 1;
    }

    out
}

pub(crate) mod commands {
    use crate::{Context, TypstBotError};

    use super::x_sampa_to_ipa;

    #[poise::command(prefix_command, track_edits)]
    /// Converts from X-SAMPA to unicode
    pub(crate) async fn xsampa(
        ctx: Context<'_>,
        #[rest] rest: String,
    ) -> Result<(), TypstBotError> {
        let ipa = x_sampa_to_ipa(&rest);
        ctx.reply(ipa).await?;

        Ok(())
    }
}
