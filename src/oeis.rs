use serde::{Deserialize, Serialize};

#[derive(Deserialize, Clone, Debug, Serialize)]
struct Response {
    // greeting: String,
    // query: String,
    // count: usize,
    // start: usize,
    results: Option<Vec<Record>>,
}

#[derive(Deserialize, Clone, Debug, Serialize)]
pub struct Record {
    pub number: i32,
    pub data: String,
    pub name: String,
    pub comment: Option<Vec<String>>,
    pub link: Option<Vec<String>>,
    pub example: Option<Vec<String>>,
    pub mathematica: Option<Vec<String>>,
    pub xref: Option<Vec<String>>,
    pub keyword: String,
    pub offset: String,
    pub author: Option<String>,
    pub references: i32,
    pub revision: i32,
    // time: DateTime,
    // created: DateTime,
}

pub struct Context {
    client: reqwest::Client,
    url: reqwest::Url,
}

impl Context {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
            url: reqwest::Url::parse("https://oeis.org/search").expect("OEIS URL failed to parse"),
        }
    }

    pub async fn lookup(&self, query: &str, n: Option<i32>) -> Result<Vec<Record>, reqwest::Error> {
        let params = [
            ("fmt", "json"),
            ("q", query),
            ("n", &format!("{}", n.unwrap_or(3))),
        ];

        let req = self.client.get(self.url.clone()).query(&params);

        Ok(req
            .send()
            .await?
            .json::<Response>()
            .await?
            .results
            .unwrap_or_else(Vec::new))
    }
}

pub mod commands {
    use std::fmt::Write;

    use poise::CreateReply;

    use crate::{Context, TypstBotError};

    #[poise::command(prefix_command)]
    /// Search for OEIS sequences
    ///
    /// Usage: -oeis [-n count] search terms
    pub async fn oeis(ctx: Context<'_>, #[rest] query: String) -> Result<(), TypstBotError> {
        let mut words = query.split_ascii_whitespace();
        let mut n = Some(3);
        let mut trimmed_query = &query[..];
        if matches!(words.next(), Some("-n")) {
            if let Some(n_str) = words.next() {
                match n_str.parse() {
                    Ok(n_val) => {
                        n = Some(n_val);

                        let skipped =
                            n_str.as_ptr() as usize - query.as_ptr() as usize + n_str.len() + 1;
                        trimmed_query = &query[skipped.min(query.len())..];
                    }
                    Err(err) => {
                        ctx.send(
                            CreateReply::default()
                                .content(&format!("Unable to parse -n argument: {err}"))
                                .reply(true),
                        )
                        .await?;
                        return Ok(());
                    }
                }
            }
        }

        let results = ctx.data().oeis.lookup(trimmed_query, n).await?;

        let mut response = String::new();
        for record in &results {
            writeln!(
                response,
                "[A{number:06}](https://oeis.org/A{number:06}): {name}\n{data}",
                number = record.number,
                name = record.name,
                data = record.data
            )
            .unwrap();
        }

        if response.is_empty() {
            response += "Literally no results, try harder";
        }

        ctx.send(CreateReply::default().content(&response).reply(true))
            .await?;

        Ok(())
    }
}
