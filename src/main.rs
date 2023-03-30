use ahash::{HashMap, HashMapExt};
use itertools::Itertools;
use smallvec::{smallvec, SmallVec};
use std::collections::{hash_map::Entry, VecDeque};
use std::fmt;
use std::ops::{Index, IndexMut};
use tinyset::Set64;

pub fn main() {
    let inverse_siblings_only = std::env::args().skip(1).any(|s| s == "-i");
    if inverse_siblings_only {
        println!("only checking inverse siblings");
    } else {
        println!("pass -i as an argument to only check inverse siblings");
    }
    println!();
    println!("enter kohnert diagram, followed by `*`");

    let mut s = String::new();
    loop {
        let mut line = String::new();
        std::io::stdin()
            .read_line(&mut line)
            .expect("error reading stdin");
        if line.starts_with("*") {
            break;
        } else {
            s += &line;
        }
    }
    let k = Kohnert::from_string(&s);

    println!("initial diagram:\n{k}");
    println!("constructing poset ...");
    let poset = Poset::new(k);
    println!("found {} total elements", poset.elements.len());
    println!("found {} minimal elements", poset.minimal_elements().len());
    println!("press ENTER to search for counterexamples");
    std::io::stdin().read_line(&mut String::new()).unwrap();
    println!("searching ...");

    let mut tally: HashMap<usize, usize> = HashMap::new();

    fn print_counterexample(poset: &Poset, a: ElemId, b: ElemId, lca: &Set64<ElemId>) {
        println!("COUNTEREXAMPLE");
        println!("a:\n{}", poset[a]);
        println!("b:\n{}", poset[b]);
        for e in lca.iter() {
            println!("lca:\n{}", poset[e]);
        }
        println!();
        println!();
        println!();
    }

    for a in poset.iter() {
        for b in (0..a.0).map(ElemId) {
            if a == b {
                continue;
            }
            if inverse_siblings_only
                && intersect(&poset[a].children, &poset[b].children)
                    .next()
                    .is_none()
            {
                continue;
            }
            let lca = poset.least_common_ancestors(a, b);
            *tally.entry(lca.len()).or_default() += 1;
            if lca.len() > 1 {
                print_counterexample(&poset, a, b, &lca)
            }
        }
    }

    println!("{tally:?}")
}

#[derive(Debug, Clone)]
struct PosetElem {
    diagram: Kohnert,
    parents: Set64<ElemId>,
    children: Set64<ElemId>,
}
impl fmt::Display for PosetElem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "diagram:\n{}", self.diagram)?;
        writeln!(
            f,
            "parents: [{}]",
            self.parents.iter().map(|s| s.0).join(", "),
        )?;
        writeln!(
            f,
            "children: [{}]",
            self.children.iter().map(|s| s.0).join(", "),
        )?;
        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
struct Poset {
    elements: Vec<PosetElem>,
    ids: HashMap<Kohnert, ElemId>,
}
impl Index<ElemId> for Poset {
    type Output = PosetElem;

    fn index(&self, index: ElemId) -> &Self::Output {
        &self.elements[index.0 as usize]
    }
}
impl IndexMut<ElemId> for Poset {
    fn index_mut(&mut self, index: ElemId) -> &mut Self::Output {
        &mut self.elements[index.0 as usize]
    }
}
impl Poset {
    fn new(seed: Kohnert) -> Self {
        let mut poset = Self {
            elements: vec![],
            ids: HashMap::new(),
        };
        let seed_id = poset.add(seed.clone()).unwrap();

        let mut queue = VecDeque::new();
        queue.push_back(seed_id);
        while let Some(elem_id) = queue.pop_back() {
            let diagram = poset[elem_id].diagram.clone();
            for succ in diagram.successors() {
                match poset.add(succ) {
                    Ok(succ_id) => {
                        queue.push_back(succ_id);
                        poset.add_edge(elem_id, succ_id);
                    }
                    Err(succ_id) => poset.add_edge(elem_id, succ_id),
                }
            }
        }

        poset
    }

    fn iter(&self) -> impl Iterator<Item = ElemId> {
        (0..self.elements.len() as u32).map(ElemId)
    }

    fn minimal_elements(&self) -> Vec<ElemId> {
        self.iter()
            .filter(|&i| self[i].children.is_empty())
            .collect()
    }

    fn least_common_ancestors(&self, a: ElemId, b: ElemId) -> Set64<ElemId> {
        let common_ancestors = self.common_ancestors(a, b);
        let mut ret: Set64<ElemId> = common_ancestors.iter().copied().collect();
        for elem in common_ancestors {
            for parent in self[elem].parents.iter() {
                ret.remove(&parent);
            }
        }
        ret
    }
    fn common_ancestors(&self, a: ElemId, b: ElemId) -> Vec<ElemId> {
        intersect(&self.ancestors(a), &self.ancestors(b)).collect()
    }
    fn ancestors(&self, seed: ElemId) -> Set64<ElemId> {
        let mut ancestors = Set64::new();
        let mut queue = vec![seed];
        while let Some(e) = queue.pop() {
            if !ancestors.contains(e) {
                ancestors.insert(e);
                queue.extend(self[e].parents.iter());
            }
        }
        ancestors
    }

    fn add(&mut self, elem: Kohnert) -> Result<ElemId, ElemId> {
        let new_id = ElemId(self.elements.len() as u32);

        match self.ids.entry(elem) {
            Entry::Occupied(entry) => Err(*entry.get()),
            Entry::Vacant(entry) => {
                self.elements.push(PosetElem {
                    diagram: entry.key().clone(),
                    parents: Set64::new(),
                    children: Set64::new(),
                });
                entry.insert(new_id);
                Ok(new_id)
            }
        }
    }
    fn add_edge(&mut self, parent: ElemId, child: ElemId) {
        self[parent].children.insert(child);
        self[child].parents.insert(parent);
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct ElemId(u32);
impl fmt::Display for ElemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl tinyset::Fits64 for ElemId {
    unsafe fn from_u64(x: u64) -> Self {
        Self(x as u32)
    }

    fn to_u64(self) -> u64 {
        self.0 as u64
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, Hash)]
struct Row(u32);
impl fmt::Display for Row {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(b) = self.last_box() {
            for x in 0..=b {
                if self.has_box(x) {
                    write!(f, " x")?;
                } else {
                    write!(f, "  ")?;
                }
            }
        }
        Ok(())
    }
}
impl Row {
    fn from_string(s: &str) -> Self {
        let mut ret = 0;
        let mut bit = 1;
        for c in s.chars() {
            if c != ' ' {
                ret |= bit;
            }
            bit <<= 1
        }
        Row(ret)
    }

    fn last_box(self) -> Option<usize> {
        (self.0 != 0).then(|| 31 - self.0.leading_zeros() as usize)
    }
    fn add_box(&mut self, x: usize) {
        self.0 |= 1_u32 << x;
    }
    fn remove_box(&mut self, x: usize) {
        self.0 &= !(1_u32 << x);
    }
    fn has_box(self, x: usize) -> bool {
        self.0 & (1 << x) != 0
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct Kohnert {
    rows: SmallVec<[Row; 8]>,
}
impl fmt::Display for Kohnert {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in self.rows.iter().rev() {
            writeln!(f, "|{row}")?;
        }
        write!(f, "+")?;
        let width = self
            .rows
            .iter()
            .filter_map(|r| r.last_box())
            .max()
            .unwrap_or(0);
        for _ in 0..=width {
            write!(f, "--")?;
        }
        writeln!(f,)?;
        Ok(())
    }
}
impl Kohnert {
    fn from_string(s: &str) -> Kohnert {
        let mut rows = smallvec![];
        for line in s.lines() {
            rows.push(Row::from_string(line));
        }
        rows.reverse();
        Self { rows }
    }

    fn successors(&self) -> impl '_ + Iterator<Item = Kohnert> {
        (0..self.rows.len()).filter_map(|move_start| {
            let x = self.rows[move_start].last_box()?;
            let move_end = (0..move_start)
                .rev()
                .take_while(|&i| self.rows[i].last_box() != Some(x))
                .find(|&i| !self.rows[i].has_box(x))?;
            let mut new_diagram = self.clone();
            new_diagram.rows[move_start].remove_box(x);
            new_diagram.rows[move_end].add_box(x);
            Some(new_diagram)
        })
    }
}

fn intersect<'a>(a: &'a Set64<ElemId>, b: &'a Set64<ElemId>) -> impl 'a + Iterator<Item = ElemId> {
    a.iter().filter(|e| b.contains(e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_last_box() {
        assert_eq!(Some(4), Row(0b11100).last_box());
        assert_eq!(None, Row(0).last_box());
    }
}
