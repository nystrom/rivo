use std::collections::HashMap;
use std::collections::HashSet;
use std::time::Instant;
use std::cmp::{max, min};
use std::fmt;

pub type StatCount = u64;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Aggregate {
    Sum(String),
    Count(String),
    Max(String),
    Min(String),
}

#[derive(Debug)]
pub struct Stats {
    map: HashMap<Aggregate, StatCount>,
    times: HashSet<String>,
}

impl Stats {
    pub fn new() -> Stats {
        Stats {
            map: HashMap::new(),
            times: HashSet::new(),
        }
    }

    pub fn start_timer(&self) -> Instant {
        Instant::now()
    }

    pub fn end_timer(&mut self, key: &str, value: Instant) {
        let duration = value.elapsed();
        let nanosec = (duration.as_secs() * 1_000_000_000) as StatCount + StatCount::from(duration.subsec_nanos());
        self.accum(key, nanosec);
        self.times.insert(key.to_owned());
    }

    pub fn accum(&mut self, key: &str, value: StatCount) {
        self.map.insert(Aggregate::Sum(key.to_owned()), value + self.map.get(&Aggregate::Sum(key.to_owned())).unwrap_or(&0));
        self.map.insert(Aggregate::Count(key.to_owned()), 1 + self.map.get(&Aggregate::Count(key.to_owned())).unwrap_or(&0));
        self.map.insert(Aggregate::Max(key.to_owned()), max(value, *self.map.get(&Aggregate::Max(key.to_owned())).unwrap_or(&value)));
        self.map.insert(Aggregate::Min(key.to_owned()), min(value, *self.map.get(&Aggregate::Min(key.to_owned())).unwrap_or(&value)));
    }

    pub fn get_count(&self, key: &str) -> Option<StatCount> {
        self.map.get(&Aggregate::Count(key.to_owned())).cloned()
    }
    pub fn get_sum(&self, key: &str) -> Option<StatCount> {
        self.map.get(&Aggregate::Sum(key.to_owned())).cloned()
    }
    pub fn get_max(&self, key: &str) -> Option<StatCount> {
        self.map.get(&Aggregate::Max(key.to_owned())).cloned()
    }
    pub fn get_min(&self, key: &str) -> Option<StatCount> {
        self.map.get(&Aggregate::Min(key.to_owned())).cloned()
    }

    pub fn dump(&self) {
        let mut stats: Vec<String> = self.map.keys().map(|k| match k {
            Aggregate::Sum(s) => s.clone(),
            Aggregate::Count(s) => s.clone(),
            Aggregate::Max(s) => s.clone(),
            Aggregate::Min(s) => s.clone(),
        }).collect();

        stats.sort_unstable();
        stats.dedup();

        println!();
        println!("Statistics");
        println!("----------");
        println!("{:<40} | {:<11} | {:<11} | {:<11} | {:<11} | {:<11}", "Stat", "Count", "Sum", "Min", "Max", "Mean");
        println!("{:-<40} | {:-<11} | {:-<11} | {:-<11} | {:-<11} | {:-<11}", "", "", "", "", "", "");

        for stat in stats {
            let cnt = self.map.get(&Aggregate::Count(stat.clone())).unwrap_or(&0);
            let sum = self.map.get(&Aggregate::Sum(stat.clone())).unwrap_or(&0);
            let max = self.map.get(&Aggregate::Max(stat.clone())).unwrap_or(&0);
            let min = self.map.get(&Aggregate::Min(stat.clone())).unwrap_or(&0);

            if self.times.contains(&stat) {
                let sum_time = (*sum as f64) * 1e-9;
                let max_time = (*max as f64) * 1e-9;
                let min_time = (*min as f64) * 1e-9;
                let mean_time = if *cnt != 0 { sum_time / (*cnt as f64) } else { f64::from(0) };
                println!("{:<40} | {:>11} | {:>11.6} | {:>11.6} | {:>11.6} | {:>11.6} ", stat, cnt, sum_time, min_time, max_time, mean_time);
            }
            else {
                let mean = if *cnt != 0 { (*sum as f64) / (*cnt as f64) } else { f64::from(0) };
                println!("{:<40} | {:>11} | {:>11} | {:>11} | {:>11} | {:>11.2}", stat, cnt, sum, min, max, mean);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_none() {
        let stats = Stats::new();
        assert_eq!(stats.get_count("foo"), None);
    }

    #[test]
    fn test_aggregates() {
        let mut stats = Stats::new();
        stats.accum("foo", 200);
        stats.accum("foo", 300);
        stats.accum("foo", 100);

        assert_eq!(stats.get_count("foo"), Some(3));
        assert_eq!(stats.get_sum("foo"), Some(600));
        assert_eq!(stats.get_max("foo"), Some(300));
        assert_eq!(stats.get_min("foo"), Some(100));

        stats.dump();
    }

    #[test]
    fn test_time() {
        let mut stats = Stats::new();
        let t = stats.start_timer();
        stats.end_timer("time", t);

        assert_eq!(stats.get_count("time"), Some(1));

        assert_eq!(stats.get_sum("time").is_some(), true);
        assert_eq!(stats.get_max("time").is_some(), true);
        assert_eq!(stats.get_min("time").is_some(), true);

        assert_eq!(stats.get_sum("time"), stats.get_max("time"));
        assert_eq!(stats.get_sum("time"), stats.get_min("time"));
        assert_eq!(stats.get_max("time"), stats.get_min("time"));

        stats.dump();
    }
}
