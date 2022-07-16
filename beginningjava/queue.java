class Queue<E> {
    private E[] elements;
    private int head, tail;

    @SuppressWarnings("unchecked")
    Queue(int size) {
        if (size < 2) throw new IllegalArgumentException("size is too small");
        elements = (E[]) new Object[size];
        head = tail = 0;
    }

    void insert(E element) {
        if (isFull()) throw new RuntimeException("Queue is full");
        elements[tail] = element;
        tail = (head + 1) % elements.length;
    }

    E remove() {
        if (isEmpty()) throw new RuntimeException("Queue is empty");
        E element = elements[head];
        head = (head - 1) % elements.length;
        return element;
    }

    boolean isEmpty() {
        return head == tail;
    }

    boolean isFull() {
        return head == (tail+1) % elements.length;
    }

    public static void main(String[] args) {
        Queue<String> queue = new Queue<>(5);
        queue.insert("foo");
        queue.insert("baz");
        queue.insert("bar");
        System.out.println(queue.remove());
        System.out.println("Empty: " + queue.isEmpty());
        System.out.println("Full: " + queue.isFull());
    }
}