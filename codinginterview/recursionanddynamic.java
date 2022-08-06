class RecursionAndDynamic {
    static int fibBasic(int n) {
        if (n == 0 || n == 1) return n;
        return fibBasic(n-1) + fibBasic(n-2);
    }

    static int fib(int n) {
        int[] memo = new int[n+1];
        return fib(n, memo);
    }

    static int fib(int i, int[] memo) {
        if (i == 0 || i == 1) return i;
        if (memo[i] == 0) {
            memo[i] = fib(i-1, memo) + fib(i-2, memo);
        }
        return memo[i];
    }

    static int fibRecursive(int n) {
        if (n == 0 || n == 1) return n;
        int[] memo = new int[n+1];
        memo[1] = 1;
        for (int i = 2; i <= n; i++) {
            memo[i] = memo[i-1] + memo[i-2];
        }
        return memo[n];
    }

    static int fibOptimized(int n) {
        if (n == 0) return 0;
        int a = 0;
        int b = 1;
        int c = 0;
        for (int i = 2; i <= n; i++) {
            c = a + b;
            a = b;
            b = c;
        }
        return c;
    }

    public static void main(String[] args) {
        int n = 41;
        System.out.println("fib (optimized) = " + fibOptimized(n));
        System.out.println("fib (recursive) = " + fibRecursive(n));
        System.out.println("fib (memoized) = " + fib(n));
        System.out.println("fib (basic) = " + fibBasic(n));
    }
}