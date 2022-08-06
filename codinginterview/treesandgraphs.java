class TreesAndGraphs {
    class Node {
        public String name;
        public Node[] children;
    }
    class Tree {
        public Node root;
    }
    static class BinaryNode {
        public String name;
        public BinaryNode left, right;
        public BinaryNode(String name, BinaryNode left, BinaryNode right) {
            this.name = name;
            this.left = left;
            this.right = right;
        }
    }
    class BinaryTree {
        public BinaryNode root;
    }

    class Graph {
        public GraphNode[] nodes;
    }

    class GraphNode {
        public String node;
        public Node[] children;
    }

    static void inOrderTraversal(BinaryNode node) {
        if (node != null) {
            inOrderTraversal(node.left);
            System.out.println(node.name);
            inOrderTraversal(node.right);
        }
    }

    static void preOrderTraversal(BinaryNode node) {
        if (node != null) {
            System.out.println(node.name);
            preOrderTraversal(node.left);
            preOrderTraversal(node.right);
        }
    }

    static void postOrderTraversal(BinaryNode node) {
        if (node != null) {
            System.out.println(node.name);
            postOrderTraversal(node.left);
            postOrderTraversal(node.right);
        }
    }

    static void dfs(GraphNode root) {
        // use recursion
    }

    static void bfs(GraphNode root) {
        // use queue
    }

    public static void main(String[] args) {
        BinaryNode tree = new BinaryNode(
                "foo",
                new BinaryNode(
                        "bar",
                        new BinaryNode(
                                "baz",
                                null,
                                null), null), null);
        inOrderTraversal(tree);
    }
}