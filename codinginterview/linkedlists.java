class LinkedLists {
    static class Node {
        int data;
        Node next;

        public Node(int d) {
            data = d;
        }

        void appendToTail(int d) {
            Node end = new Node(d);
            Node n = this;
            while (n.next != null) {
                n = n.next;
            }
            n.next = end;
        }

        Node deleteNode(Node head, int d) {
            Node n = head;
            if (n.data == d) {
                return n.next;
            }
            while (n.next != null) {
                if (n.next.data == d) {
                    n.next = n.next.next;
                    return head;
                }
                n = n.next;
            }
            return head;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder(Integer.toString(data));
            Node n = this;
            do {
                n = n.next;
                if (n == null) break;
                sb.append(" -> ").append(n.data);
            } while (n.next != null);
            return sb.toString();
        }
    }

    public static void main(String[] args) {
        Node n = new Node(1);
        n.appendToTail(2);
        n.appendToTail(3);
        n.deleteNode(n, 2);
        System.out.println(n);
    }
}