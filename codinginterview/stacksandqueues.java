class StacksAndQueues {
    static class Stack<T> {
        static class Node<T> {
            private T data;
            private Node<T> next;
            public Node(T data) {
                this.data = data;
            }
        }

        private Node<T> top;

        public T pop() {
            if (top == null) {
                throw new RuntimeException("Stack is empty");
            }
            T data = top.data;
            top = top.next;
            return data;
        }

        public void push(T data) {
            Node<T> n = new Node<>(data);
            n.next = top;
            top = n;
        }

        public T peek() {
            if (top == null) {
                throw new RuntimeException("Stack is empty");
            }
            return top.data;
        }

        public boolean isEmpty() {
            return top == null;
        }
    }

    static class Queue<T> {
        static class Node<T> {
            private T data;
            private Node<T> next;
            public Node(T data) {
                this.data = data;
            }
        }

        private Node<T> first;
        private Node<T> last;

        public void add(T item) {
            Node<T> n = new Node<>(item);
            if (last != null) {
                last.next = n;
            }
            last = n;
            if (first == null) {
                first = last;
            }
        }

        public T remove() {
            if (first == null) {
                throw new RuntimeException("Stack is empty");
            }
            T data = first.data;
            first = first.next;
            if (first == null) {
                last = null;
            }
            return data;
        }

        public T peek() {
            if (first == null) {
                throw new RuntimeException("Stack is empty");
            }
            return first.data;
        }

        @Override
        public String toString() {
            Node<T> n = first;
            if (n == null) return "(null)";
            StringBuilder sb = new StringBuilder(n.data.toString());
            while ((n = n.next) != null) {
                sb.append(" -> ").append(n.data);
            }
            return sb.toString();
        }
    }

    public static void main(String[] args) {
        Stack<Integer> s = new Stack<>();
        s.push(1);
        s.push(2);
        s.push(3);
        System.out.println(s.pop());
        Queue<Integer> q = new Queue<>();
        q.add(1);
        q.add(2);
        q.add(3);
        q.remove();
        System.out.println(q);
    }
}