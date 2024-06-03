pub struct Dictionary<'a> {
    // TODO: <br> handling
    terms: Vec<[&'a str; 3]>,
}

fn levenshtein(w1: &str, w2: &str) -> usize {
    let w1 = w1.as_bytes();
    let w2 = w2.as_bytes();

    let n = w1.len();
    let m = w2.len();

    let mut dp = vec![vec![0; m + 1]; n + 1];

    for i in 0..=n {
        for j in 0..=m {
            dp[i][j] = if i == 0 {
                j
            } else if j == 0 {
                i
            } else if w1[i - 1] == w2[j - 1] {
                dp[i - 1][j - 1]
            } else {
                1 + [dp[i - 1][j], dp[i - 1][j - 1], dp[i][j - 1]]
                    .iter()
                    .copied()
                    .min()
                    .unwrap()
            };
        }
    }

    dp[n][m]
}

impl Dictionary<'static> {
    pub fn new_default() -> Self {
        Self::new(std::include_str!("../assets/dictionary.csv"))
    }
}

impl<'a> Dictionary<'a> {
    pub fn new(text: &'a str) -> Self {
        // aaa i have to parse csv

        Self {
            terms: text
                .lines()
                .map(|line| {
                    let mut parts = line.split_terminator(',');
                    [
                        parts.next().unwrap(),
                        parts.next().unwrap(),
                        parts.next().unwrap(),
                    ]
                })
                .collect(),
        }
    }

    pub fn lookup(&self, term: &str) -> [&'a str; 3] {
        self.terms
            .iter()
            .min_by_key(|w| w.iter().map(|s| levenshtein(s, term)).min().unwrap())
            .copied()
            .unwrap()
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn idk() {
        use super::levenshtein;

        eprintln!("{}", levenshtein("2-category", "hovedidealområde"));
        eprintln!("{}", levenshtein("hovedidealområde", "hovedidealområde"));

        assert!(
            levenshtein("2-category", "hovedidealområde")
                > levenshtein("hovedidealområde", "hovedidealområde"),
            "ummm no"
        );
    }

    #[test]
    fn idk1() {
        use super::*;

        let dict = Dictionary::new_default();
        let a = dict.lookup("hovedidealområde");
        assert_eq!(a[0], "hovedidealområde");
    }
}
