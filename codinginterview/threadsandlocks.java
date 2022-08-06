class ThreadsAndLocks {
//    public interface Runnable {
//        void run();
//    }

    public static class RunnableThreadExample implements Runnable {
        public int count = 0;

        public void run() {
            System.out.println("RunnableThread starting.");
            try {
                while (count < 5) {
                    Thread.sleep(500);
                    count++;
                }
            } catch (InterruptedException exc) {
                System.out.println("RunnableThread interrupted.");
            }
            System.out.println("RunnableThread terminating.");
        }
    }

    public static void main(String[] args) {
        RunnableThreadExample rte = new RunnableThreadExample();
        Thread t = new Thread(rte);
        t.start();

        /* waits until above thread counts to 5 (slowly) */
        while (rte.count != 5) {
            try {
                Thread.sleep(250);
            } catch (InterruptedException ie) {
                ie.printStackTrace();
            }
        }
    }
}