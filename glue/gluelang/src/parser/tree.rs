use std::{
    collections::HashMap,
    sync::{
        RwLock,
        atomic::{AtomicUsize, Ordering},
    },
};

const ORDERING: Ordering = Ordering::Relaxed;

const ROOT_NODE_ID: usize = 0;
const DEFAULT_NODE_ID: usize = 1; // We reserve 0 for the root node

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TreeNodeId(pub usize);
impl Default for TreeNodeId {
    fn default() -> Self {
        TreeNodeId(DEFAULT_NODE_ID) // We reserve 0 for the root node.
    }
}

impl From<usize> for TreeNodeId {
    fn from(value: usize) -> Self {
        TreeNodeId(value)
    }
}

impl std::fmt::Display for TreeNodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub trait TreeNode {
    fn id(&self) -> TreeNodeId;
    fn set_id(&mut self, id: TreeNodeId);
    fn parent_id(&self) -> Option<TreeNodeId>;
    fn set_parent_id(&mut self, parent_id: Option<TreeNodeId>);
}

struct TreeInner<T> {
    root: TreeNodeId,
    nodes: HashMap<TreeNodeId, T>,
    adjacency: HashMap<TreeNodeId, Vec<TreeNodeId>>,
}

pub struct Tree<T>
where
    T: std::fmt::Debug,
{
    inner: RwLock<TreeInner<T>>,
    node_id_counter: AtomicUsize,
}

impl<T> Clone for Tree<T>
where
    T: Clone + std::fmt::Debug + TreeNode,
{
    fn clone(&self) -> Self {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        Self {
            inner: RwLock::new(TreeInner {
                root: inner.root,
                nodes: inner.nodes.clone(),
                adjacency: inner.adjacency.clone(),
            }),
            node_id_counter: AtomicUsize::new(self.node_id_counter.load(ORDERING)),
        }
    }
}

impl<T> Tree<T>
where
    T: Clone + std::fmt::Debug + TreeNode,
{
    pub fn new_with_root(node: T) -> Self {
        let mut nodes = HashMap::new();
        let root_node_id = TreeNodeId(ROOT_NODE_ID);
        nodes.insert(root_node_id, node);
        let mut adjacency = HashMap::new();
        adjacency.insert(root_node_id, Vec::new());
        Self {
            inner: RwLock::new(TreeInner {
                root: root_node_id,
                nodes,
                adjacency,
            }),
            node_id_counter: AtomicUsize::new(1),
        }
    }

    pub fn set_root(&self, node_id: TreeNodeId) {
        let mut inner = self.inner.write().expect("Failed to acquire write lock on tree state");
        inner.root = node_id;
    }

    pub fn add_node(&self, mut node: T) -> TreeNodeId {
        let id = self.node_id_counter.fetch_add(1, ORDERING);
        let node_id = TreeNodeId(id);
        let mut inner = self.inner.write().expect("Failed to acquire write lock on tree state");
        node.set_id(node_id);
        inner.nodes.insert(node_id, node);
        node_id
    }

    pub fn append_child(&self, parent_id: TreeNodeId, child_id: TreeNodeId) {
        let mut inner = self.inner.write().expect("Failed to acquire write lock on tree state");
        inner.adjacency.entry(parent_id).or_default().push(child_id);
        if let Some(child) = inner.nodes.get_mut(&child_id) {
            child.set_parent_id(Some(parent_id));
        }
    }

    pub fn update_node<F, R>(&self, node_id: TreeNodeId, f: F) -> Option<R>
    where
        F: FnOnce(&mut T) -> R,
    {
        let mut inner = self.inner.write().expect("Failed to acquire write lock on tree state");
        inner.nodes.get_mut(&node_id).map(f)
    }

    pub fn get_node(&self, node_id: TreeNodeId) -> Option<T> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        inner.nodes.get(&node_id).cloned()
    }

    pub fn find(&self, f: impl Fn(&T) -> bool) -> Vec<T> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        // BFS to find the node.
        let mut queue = vec![inner.root];
        let mut results = Vec::new();

        while let Some(current_id) = queue.pop() {
            if let Some(node) = inner.nodes.get(&current_id)
                && f(node)
            {
                results.push(node.clone());
            }
            if let Some(children) = inner.adjacency.get(&current_id) {
                for &child_id in children {
                    queue.push(child_id);
                }
            }
        }

        results
    }

    pub fn get_first_ancestor_fn<F>(&self, node_id: TreeNodeId, f: F) -> Option<T>
    where
        F: Fn(&T) -> bool,
    {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        let mut current_id = node_id;

        while let Some((parent_id, _)) = inner.adjacency.iter().find(|(_, children)| children.contains(&current_id)) {
            if let Some(node) = inner.nodes.get(parent_id)
                && f(node)
            {
                return Some(node.clone());
            }
            current_id = *parent_id;
        }

        None
    }

    pub fn get_ancestor_ids(&self, node_id: TreeNodeId) -> Vec<TreeNodeId> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        let mut ancestors = Vec::new();
        let mut current_id = node_id;

        while let Some((parent_id, _)) = inner.adjacency.iter().find(|(_, children)| children.contains(&current_id)) {
            ancestors.push(*parent_id);
            current_id = *parent_id;
        }

        ancestors
    }

    pub fn get_children_ids(&self, node_id: TreeNodeId) -> Option<Vec<TreeNodeId>> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        inner.adjacency.get(&node_id).cloned()
    }

    pub fn get_children(&self, node_id: TreeNodeId) -> Option<Vec<T>> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        inner
            .adjacency
            .get(&node_id)
            .map(|children_ids| children_ids.iter().filter_map(|id| inner.nodes.get(id).cloned()).collect())
    }

    pub fn get_children_fn(&self, node_id: TreeNodeId, f: impl Fn(&T) -> bool) -> Option<Vec<T>> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        inner
            .adjacency
            .get(&node_id)
            .map(|children_ids| children_ids.iter().filter_map(|id| inner.nodes.get(id)).filter(|node| f(node)).cloned().collect())
    }

    pub fn get_root(&self) -> TreeNodeId {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        inner.root
    }

    pub fn nodes_by_ids(&self, ids: Vec<TreeNodeId>) -> Vec<T> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        ids.iter().filter_map(|id| inner.nodes.get(id).cloned()).collect()
    }

    pub fn nodes(&self) -> Vec<T> {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        inner.nodes.values().cloned().collect()
    }

    pub fn visit(&self, f: impl Fn(&T)) {
        let inner = self.inner.read().expect("Failed to acquire write lock on tree state");
        let mut stack = vec![inner.root];

        while let Some(node_id) = stack.pop() {
            if let Some(node) = inner.nodes.get(&node_id) {
                f(node);
            }
            if let Some(children) = inner.adjacency.get(&node_id) {
                for &child_id in children {
                    stack.push(child_id);
                }
            }
        }
    }

    pub fn to_mermaid(&self) -> String {
        self.to_mermaid_with_formatter(None::<fn(&T) -> String>)
    }

    pub fn to_mermaid_with_formatter<F>(&self, formatter: Option<F>) -> String
    where
        F: Fn(&T) -> String,
    {
        let inner = self.inner.read().expect("Failed to acquire read lock on tree state");
        let root = self.get_root();
        let mut result = String::from("graph TD\n");

        fn get_node_color(node_id: TreeNodeId) -> &'static str {
            let colors = ["#4a0e0e", "#0f2a25", "#2d7a91", "#629e7a", "#ccb885", "#b366b3", "#66a89a", "#c4b058", "#9567a3", "#5a9bc4"];
            colors[node_id.0 % colors.len()]
        }

        fn build_mermaid<T: std::fmt::Debug, F>(
            inner: &TreeInner<T>,
            node_id: TreeNodeId,
            result: &mut String,
            visited: &mut std::collections::HashSet<TreeNodeId>,
            formatter: &Option<F>,
        ) where
            F: Fn(&T) -> String,
        {
            if visited.contains(&node_id) {
                return;
            }
            visited.insert(node_id);

            if let Some(node) = inner.nodes.get(&node_id) {
                let label = if let Some(f) = formatter { f(node) } else { format!("{node:?}") };
                let color = get_node_color(node_id);
                result.push_str(&format!("    {}[\"{}\"]\n", node_id.0, label));
                result.push_str(&format!("    style {} fill:{},stroke:#333,stroke-width:2px,color:#fff\n", node_id.0, color));

                if let Some(children) = inner.adjacency.get(&node_id) {
                    for &child_id in children {
                        result.push_str(&format!("    {} --> {}\n", node_id.0, child_id.0));
                        build_mermaid(inner, child_id, result, visited, formatter);
                    }
                }
            }
        }

        let mut visited = std::collections::HashSet::new();
        build_mermaid(&inner, root, &mut result, &mut visited, &formatter);
        result
    }
}

#[cfg(test)]
#[allow(unused_variables)]
mod tests {
    use std::sync::{Arc, Barrier};
    use std::thread;

    use crate::Span;

    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq)]
    enum TestNodeKind {
        Root,
        Branch,
        Leaf,
    }

    #[allow(dead_code)]
    #[derive(Debug, Clone)]
    struct TestNode {
        id: TreeNodeId,
        parent_id: Option<TreeNodeId>,
        span: Span,
        kind: TestNodeKind,
        value: Option<usize>, // Added state for leaf nodes
    }

    impl TreeNode for TestNode {
        fn id(&self) -> TreeNodeId {
            self.id
        }

        fn set_id(&mut self, id: TreeNodeId) {
            self.id = id;
        }

        fn parent_id(&self) -> Option<TreeNodeId> {
            self.parent_id
        }

        fn set_parent_id(&mut self, parent_id: Option<TreeNodeId>) {
            self.parent_id = parent_id;
        }
    }

    impl TestNode {
        fn new(kind: TestNodeKind) -> Self {
            Self {
                id: TreeNodeId::default(),
                parent_id: None,
                span: Default::default(),
                kind,
                value: None,
            }
        }

        fn new_with_value(kind: TestNodeKind, value: usize) -> Self {
            Self {
                id: TreeNodeId::default(),
                parent_id: None,
                span: Default::default(),
                kind,
                value: Some(value),
            }
        }

        fn value(&self) -> Option<usize> {
            self.value
        }
    }

    #[test]
    fn test_leaf_node_with_value() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let leaf_value = 42;
        let leaf = TestNode::new_with_value(TestNodeKind::Leaf, leaf_value);
        let leaf_id = tree.add_node(leaf);

        let retrieved = tree.get_node(leaf_id).unwrap();
        assert_eq!(retrieved.kind, TestNodeKind::Leaf);
        assert_eq!(retrieved.value(), Some(leaf_value));
    }

    #[test]
    fn test_set_and_get_root() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let node = TestNode::new(TestNodeKind::Root);
        let node_id = tree.add_node(node);

        tree.set_root(node_id);
        assert_eq!(tree.get_root(), node_id);
    }

    #[test]
    fn test_root_children() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let child1 = TestNode::new(TestNodeKind::Branch);
        let child2 = TestNode::new(TestNodeKind::Leaf);
        let grandchild1 = TestNode::new(TestNodeKind::Leaf);

        let child1_id = tree.add_node(child1);
        let child2_id = tree.add_node(child2);
        let grandchild1_id = tree.add_node(grandchild1);

        let root_id = tree.get_root();

        tree.append_child(root_id, child1_id);
        tree.append_child(root_id, child2_id);
        tree.append_child(child1_id, grandchild1_id);

        let children = tree.get_children_ids(root_id).unwrap();

        assert_eq!(children.len(), 2);
        assert!(children.contains(&child1_id));
        assert!(children.contains(&child2_id));

        let grandchild1_children = tree.get_children_ids(child1_id).unwrap();
        assert_eq!(grandchild1_children.len(), 1);
        assert!(grandchild1_children.contains(&grandchild1_id));
    }

    #[test]
    fn test_append_child_and_get_children() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let parent = TestNode::new(TestNodeKind::Root);
        let child1 = TestNode::new(TestNodeKind::Branch);
        let child2 = TestNode::new_with_value(TestNodeKind::Leaf, 100);

        let parent_id = tree.add_node(parent);
        let child1_id = tree.add_node(child1);
        let child2_id = tree.add_node(child2);

        tree.append_child(parent_id, child1_id);
        tree.append_child(parent_id, child2_id);

        let children = tree.get_children_ids(parent_id).unwrap();
        assert_eq!(children.len(), 2);
        assert!(children.contains(&child1_id));
        assert!(children.contains(&child2_id));

        // Verify child values are preserved
        let retrieved_child1 = tree.get_node(child1_id).unwrap();
        let retrieved_child2 = tree.get_node(child2_id).unwrap();
        assert_eq!(retrieved_child1.value(), None);
        assert_eq!(retrieved_child2.value(), Some(100));
    }

    #[test]
    fn test_get_children_nonexistent_parent() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        assert_eq!(tree.get_children_ids(TreeNodeId(999)), None);
    }

    #[test]
    fn test_get_node_nonexistent() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let node = tree.get_node(TreeNodeId(999));
        assert!(node.is_none());
    }

    #[test]
    fn test_multiple_nodes_unique_ids() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let node1 = TestNode::new(TestNodeKind::Branch);
        let node2 = TestNode::new_with_value(TestNodeKind::Leaf, 25);

        let id1 = tree.add_node(node1);
        let id2 = tree.add_node(node2);

        assert_ne!(id1, id2);

        let retrieved1 = tree.get_node(id1).unwrap();
        let retrieved2 = tree.get_node(id2).unwrap();
        assert_eq!(retrieved1.value(), None);
        assert_eq!(retrieved2.value(), Some(25));
    }

    #[test]
    fn test_leaf_values_preservation() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let values = [10, 20, 30, 40, 50];
        let mut leaf_ids = Vec::new();

        // Add multiple leaf nodes with different values
        for (i, &value) in values.iter().enumerate() {
            let leaf = TestNode::new_with_value(TestNodeKind::Leaf, value);
            let leaf_id = tree.add_node(leaf);
            leaf_ids.push(leaf_id);
        }

        // Verify all values are preserved correctly
        for (i, &leaf_id) in leaf_ids.iter().enumerate() {
            let retrieved = tree.get_node(leaf_id).unwrap();
            assert_eq!(retrieved.kind, TestNodeKind::Leaf);
            assert_eq!(retrieved.value(), Some(values[i]));
        }
    }

    #[test]
    fn test_ancestors() {
        let tree: Tree<TestNode> = Tree::new_with_root(TestNode::new(TestNodeKind::Root));
        let parent = TestNode::new(TestNodeKind::Branch);
        let child = TestNode::new(TestNodeKind::Leaf);
        let grandchild = TestNode::new(TestNodeKind::Leaf);

        let parent_id = tree.add_node(parent);
        let child_id = tree.add_node(child);
        let grandchild_id = tree.add_node(grandchild);

        tree.append_child(parent_id, child_id);
        tree.append_child(child_id, grandchild_id);

        let ancestors_of_grandchild = tree.get_ancestor_ids(grandchild_id);
        assert_eq!(ancestors_of_grandchild.len(), 2);
        assert_eq!(ancestors_of_grandchild[0], child_id);
        assert_eq!(ancestors_of_grandchild[1], parent_id);

        let ancestors_of_child = tree.get_ancestor_ids(child_id);
        assert_eq!(ancestors_of_child.len(), 1);
        assert_eq!(ancestors_of_child[0], parent_id);

        let ancestors_of_parent = tree.get_ancestor_ids(parent_id);
        assert_eq!(ancestors_of_parent.len(), 0);
    }

    #[test]
    fn test_concurrent_access() {
        let tree = Arc::new(Tree::<TestNode>::new_with_root(TestNode::new(TestNodeKind::Root)));
        let barrier = Arc::new(Barrier::new(4));
        let mut handles = vec![];

        // Thread 1: Add nodes with values
        let tree_clone = Arc::clone(&tree);
        let barrier_clone = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            barrier_clone.wait();
            for i in 0..10 {
                let node = TestNode::new_with_value(TestNodeKind::Leaf, i * 10);
                tree_clone.add_node(node);
            }
        }));

        // Thread 2: Add more nodes and set root
        let tree_clone = Arc::clone(&tree);
        let barrier_clone = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            barrier_clone.wait();
            let node = TestNode::new(TestNodeKind::Root);
            let root_id = tree_clone.add_node(node);
            tree_clone.set_root(root_id);
        }));

        // Thread 3: Add children with values
        let tree_clone = Arc::clone(&tree);
        let barrier_clone = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            barrier_clone.wait();
            let parent = TestNode::new(TestNodeKind::Branch);
            let child = TestNode::new_with_value(TestNodeKind::Leaf, 999);
            let parent_id = tree_clone.add_node(parent);
            let child_id = tree_clone.add_node(child);
            tree_clone.append_child(parent_id, child_id);

            // Verify the child value is correct
            let retrieved_child = tree_clone.get_node(child_id).unwrap();
            assert_eq!(retrieved_child.value(), Some(999));
        }));

        // Thread 4: Read operations and verify values
        let tree_clone = Arc::clone(&tree);
        let barrier_clone = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            barrier_clone.wait();
            for _ in 0..20 {
                let _ = tree_clone.get_root();
                if let Some(node) = tree_clone.get_node(TreeNodeId(ROOT_NODE_ID)) {
                    // If we get a leaf node, verify it has the expected value
                    if node.kind == TestNodeKind::Leaf {
                        assert_eq!(node.value(), Some(0));
                    }
                }
                let _ = tree_clone.get_children_ids(TreeNodeId(DEFAULT_NODE_ID));
            }
        }));

        for handle in handles {
            handle.join().unwrap();
        }

        for i in 1..8 {
            // sample a few ids
            if let Some(node) = tree.get_node(TreeNodeId(i)) {
                // Any early leaf nodes have a value that is either a multiple of 10 (producer thread)
                // or the special 999 value from the parent/child thread.
                if node.kind == TestNodeKind::Leaf {
                    let val = node.value().unwrap();
                    assert!(val % 10 == 0 || val == 999, "unexpected leaf value {val} for id {i}");
                }
            }
        }
    }
}
