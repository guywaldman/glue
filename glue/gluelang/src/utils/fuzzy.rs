pub fn fuzzy_match<'a, T>(input: &str, candidates: &'a Vec<T>, limit: usize) -> Vec<(&'a T, i64)>
where
    T: AsRef<str>,
{
    let scores = candidates.iter().filter_map(|candidate| {
        // Use Levenshtein distance for naive fuzzy matching
        let score = {
            let dist = levenshtein(input, candidate.as_ref());
            let max_len = input.len().max(candidate.as_ref().len());
            Some(100 - (dist as i64 * 100 / max_len as i64)) // Normalize to 0-100
        };
        score.map(|score| (score, candidate))
    });
    let mut scored_candidates: Vec<_> = scores.collect();
    scored_candidates.sort_by(|a, b| b.0.cmp(&a.0)); // Sort by score descending
    scored_candidates.into_iter().take(limit).map(|(score, candidate)| (candidate, score)).collect()
}

fn levenshtein(a: &str, b: &str) -> usize {
    let mut costs = (0..=b.len()).collect::<Vec<_>>();

    for (i, ca) in a.chars().enumerate() {
        costs[0] = i + 1;
        let mut last_cost = i;
        for (j, cb) in b.chars().enumerate() {
            let new_cost = if ca == cb { last_cost } else { 1 + last_cost.min(costs[j]).min(costs[j + 1]) };
            last_cost = costs[j + 1];
            costs[j + 1] = new_cost;
        }
    }
    costs[b.len()]
}
