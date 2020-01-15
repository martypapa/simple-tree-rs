use std::collections::{BTreeSet};


/// An ID that stores an index and a version
/// `index` is used to retrieve the data from the internal tree arrays
/// `version` allows reuse of previous indexes that have been deleted
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct NodeID {
    index: usize,
    version: usize,
}

/// Stores all data and relationships between nodes
#[derive(Clone, PartialEq, Eq)]
pub struct Tree<T: Clone> {
    roots: Vec<NodeID>,
    parents: Vec<Option<NodeID>>,
    children: Vec<Vec<NodeID>>,
    data: Vec<Option<T>>,
    versions: Vec<usize>,
    empty: BTreeSet<usize>,
}

/// Used internally to keep track of iterator position
struct IterStack<'a> {
    group: &'a Vec<NodeID>,
    next_index: usize,
}

/// For iterating over the tree, depth-first
pub struct DepthIter<'a, T: Clone> {
    tree: &'a Tree<T>,
    stacks: Vec<IterStack<'a>>,
}


/// For iterating over the tree, depth-first
pub struct BreadthIter<'a, T: Clone> {
    tree: &'a Tree<T>,
    depth: usize,
    current: Vec<NodeID>,
    next: Vec<NodeID>,
    next_index: usize,
}


impl<T: Clone> Tree<T> {
    /// Returns a new empty tree
    pub fn new() -> Self {
        Self {
            roots: vec![],
            parents: vec![],
            children: vec![],
            data: vec![],
            versions: vec![],
            empty: Default::default()
        }
    }

    /// Check if a `node id` points to a valid, non-deleted node in the tree.
    /// Returns true if the node id is valid.
    /// # Arguments
    /// * `node` - the NodeID to check
    pub fn is_valid(&self, node: NodeID) -> bool {
        node.index < self.data.len() && !self.empty.contains(&node.index) && self.versions[node.index] == node.version
    }


    /// Mark a node in the tree, and all of its children as invalid.
    /// This allows the reuse of these indexes by other nodes created later.
    /// Returns the number of total nodes that were deleted.
    pub fn delete(&mut self, node: NodeID) -> usize {
        if !self.is_valid(node) {
            return 0;
        }
        let mut total = 1;
        let ids = self.children_ids_unchecked(node).clone();
        for child in ids {
            total += self.delete(child);
        }
        if let Some(parent) = self.parents[node.index] {
            if let Some(index) = self.children[parent.index].iter().position(|&x| x == node) {
                self.children[parent.index].remove(index);
            }
        } else {
            if let Some(index) = self.roots.iter().position(|&x| x == node) {
                self.roots.remove(index);
            }
        }

        self.data[node.index] = None;
        self.parents[node.index] = None;
        self.children[node.index].clear();
        self.empty.insert(node.index);
        total
    }

    pub fn add_root(&mut self, val: T) -> NodeID {
        let i = NodeID{index: self.data.len(), version: 0};
        self.data.push(Some(val));
        self.roots.push(i);
        self.children.push(vec![]);
        self.parents.push(None);
        self.versions.push(0);
        i
    }

    pub fn get(&self, node: NodeID) -> Option<&T> {
        match self.is_valid(node) {
            true => self.data[node.index].as_ref(),
            false => None,
        }
    }
    pub fn get_mut(&mut self, node: NodeID) -> Option<&mut T> {
        match self.is_valid(node) {
            true => self.data[node.index].as_mut(),
            false => None,
        }
    }
    pub fn get_unchecked(&self, node: NodeID) -> &T {
        self.data[node.index].as_ref().unwrap()
    }
    pub fn get_unchecked_mut(&mut self, node: NodeID) -> &mut T {
        self.data[node.index].as_mut().unwrap()
    }

    pub fn set(&mut self, node: NodeID, val: T) {
        if self.is_valid(node) {
            self.data[node.index] = Some(val);
        }
    }

    pub fn add_child(&mut self, node: NodeID, val: T) -> Option<NodeID> {
        match self.is_valid(node) {
            true => Some(self.add_child_unchecked(node, val)),
            false => None
        }
    }
    pub fn add_child_unchecked(&mut self, node: NodeID, val: T) -> NodeID {
        let x = match self.empty.iter().next().cloned() {
            Some(x) => {
                self.data[x] = Some(val);
                self.parents[x] = Some(node);
                let mut version = self.versions.get_mut(x).unwrap();
                *version += 1;
                self.empty.remove(&x);
                NodeID{ index: x, version: *version }
            },
            _ => {
                self.data.push(Some(val));
                self.parents.push(Some(node));
                self.children.push(vec![]);
                self.versions.push(0);
                NodeID{index: self.data.len() - 1, version: 0 }
            },
        };
        self.children[node.index].push(x);
        x

    }

    pub fn children_ids_unchecked(&self, node: NodeID) -> &Vec<NodeID> {
        &self.children[node.index]
    }

    pub fn root_ids(&self) -> &Vec<NodeID> {
        &self.roots
    }

    pub fn parent_id_unchecked(&self, node: NodeID) -> Option<NodeID> {
        self.parents[node.index]
    }

    pub fn parent_id(&self, node: NodeID) -> Option<NodeID> {
        match self.is_valid(node) {
            true => self.parents[node.index],
            false => None,
        }
    }

    pub fn iter_depth(&self) -> DepthIter<T> {
        DepthIter{
            tree: &self,
            stacks: vec![IterStack{group: &self.roots, next_index: 0}],
        }
    }
    pub fn iter_breadth(&self) -> BreadthIter<T> {
        BreadthIter{
            tree: &self,
            current: self.roots.clone(),
            next_index: 0,
            next: vec![],
            depth: 0,
        }
    }

    pub fn path(&self, branch: NodeID) -> Vec<NodeID> {
        let mut path: Vec<NodeID> = vec![branch];
        let mut cur = branch;
        loop {
            match self.parents[cur.index] {
                Some(x) => {
                    path.push(x);
                    cur = x;
                },
                None => break,
            }
        }
        path.reverse();
        path
    }
    pub fn path_values_ref(&self, branch: NodeID) -> Vec<&T> {
        self.path(branch).iter().map(|&x| self.get_unchecked(x)).collect::<Vec<_>>()
    }
    pub fn path_values(&self, branch: NodeID) -> Vec<T> {
        self
            .path(branch)
            .into_iter()
            .map(|x| self.get_unchecked(x).clone())
            .collect()
    }
}


#[derive(Debug, Eq, PartialEq)]
pub struct Node<'a, T> {
    pub id: NodeID,
    pub value: &'a T,
    pub depth: usize,
}


/// Implement depth-first iteration
impl<'a, T: Clone> Iterator for DepthIter<'a, T> {
    type Item = Node<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.stacks.is_empty(){
                return None;
            }
            if {
                let stack = self.stacks.last().unwrap();
                stack.next_index >= stack.group.len()
            }{
                // Finished checking this group - pop up a level and go to next one
                self.stacks.pop();
                if self.stacks.is_empty() {
                    return None;
                }
                let stack = self.stacks.last_mut().unwrap();
                stack.next_index += 1;
            } else {
                // return value at next_index:
                // Increment stack to next sibling
                let depth = self.stacks.len() - 1;
                let mut stack = self.stacks.last_mut().unwrap();
                let branch = stack.group[stack.next_index];
                let children = self.tree.children_ids_unchecked(branch);
                if !children.is_empty() {
                    self.stacks.push(IterStack{
                        next_index: 0,
                        group: children,
                    })
                } else {
                    stack.next_index += 1;
                }
                return Some(Self::Item {
                    id: branch,
                    value: self.tree.get_unchecked(branch),
                    depth,
                });
            }
        }
    }
}



/// Implement breadth-first iteration
impl<'a, T: Clone> Iterator for BreadthIter<'a, T> {
    type Item = Node<'a, T>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current.is_empty() {
                return None;
            }
            if self.next_index >= self.current.len() {
                self.next_index = 0;
                self.current = self.next.clone();
                self.next.clear();
                self.depth += 1;
                continue;
            }

            let node = self.current[self.next_index];
            let children = self.tree.children_ids_unchecked(node);
            self.next.append(&mut children.clone());
            self.next_index += 1;

            return Some(Self::Item{
                value: self.tree.get_unchecked(node),
                depth: self.depth,
                id: node,
            });
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn depth_first_iteration() {
        let mut tree: Tree<i32> = Tree::new();
        let root = tree.add_root(0);
        let child1 = tree.add_child_unchecked(root, 1);
        let child2 = tree.add_child_unchecked(root, 2);
        let child1_1 = tree.add_child_unchecked(child1, 11);

        let expected: Vec<(i32, usize)> = vec![(0, 0), (1, 1), (11, 2), (2, 1)];
        let real = tree.iter_depth().map(|x|{
            (*x.value, x.depth)
        }).collect::<Vec<_>>();
        assert_eq!(real, expected);
    }

    #[test]
    fn breadth_first_iteration() {
        let mut tree: Tree<i32> = Tree::new();
        let root = tree.add_root(0);
        let child1 = tree.add_child_unchecked(root, 1);
        let child2 = tree.add_child_unchecked(root, 2);
        let child1_1 = tree.add_child_unchecked(child1, 11);

        let expected: Vec<(i32, usize)> = vec![(0, 0), (1, 1), (2, 1), (11, 2)];
        let real = tree.iter_breadth().map(|x|{
            (*x.value, x.depth)
        }).collect::<Vec<_>>();
        assert_eq!(real, expected);
    }
}