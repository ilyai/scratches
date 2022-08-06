import java.util.Arrays;

class BigO {
    static class Node {
        int value;
        Node left, right;
        Node(int value, Node left, Node right) {
            this.value = value;
            this.left = left;
            this.right = right;
        }
    }

    // O(n) T(n/2)
    static void reverse(int[] array) {
        for (int i = 0; i < array.length / 2; i++) {
            int other = array.length - 1 - i;
            int temp = array[i];
            array[i] = array[other];
            array[other] = temp;
        }
    }

    // O(n)
    static int sum(Node node) {
        if (node == null) return 0;
        return sum(node.left) + node.value + sum(node.right);
    }

    // O(sqrt(n))
    static boolean isPrime(int n) {
        for (int x = 2; x * x <= n; x++) {
            if (n % x == 0) return false;
        }
        return true;
    }

    // O(n)
    static int factorial(int n) {
        if (n < 0) return -1;
        if (n == 0) return 1;
        return n * factorial(n - 1);
    }

    static void permutation(String str) {
        permutation(str, "");
    }

    // O(n * n!) or O(n*n * n!)???
    static void permutation(String str, String prefix) {
        if (str.length() == 0) System.out.println(prefix);
        else {
            for (int i = 0; i < str.length(); i++) {
                String rem = str.substring(0, i) + str.substring(i + 1);
                permutation(rem, prefix + str.charAt(i));
            }
        }
    }

    // O(2^n)
    static int fib(int n) {
        if (n <= 0) return 0;
        if (n == 1) return 1;
        return fib(n-1) + fib(n-2);
    }

    // O(n)
    static void allFib(int n) {
        int[] memo = new int[n+1];
        for (int i = 0; i < n; i++) {
            System.out.println("fib(" + i + ") = " + fib(i, memo));
        }
    }

    // fib with memoization
    static int fib(int n, int[] memo) {
        if (n <= 0) return 0;
        if (n == 1) return 1;
        if (memo[n] != 0) return memo[n];
        memo[n] = fib(n-1, memo) + fib(n-2, memo);
        return memo[n];
    }

    // O(log(n))
    static int powersOf2(int n) {
        if (n < 1) {
            return 0;
        }
        if (n == 1) {
            System.out.println(1);
            return 1;
        }
        int prev = powersOf2(n/2);
        int curr = prev * 2;
        System.out.println(curr);
        return curr;
    }

    // O(b)
    static int product(int a, int b) {
        int sum = 0;
        for (int i = 0; i < b; i++) {
            sum += a;
        }
        return sum;
    }

    // O(b)
    static int power(int a, int b) {
        if (b < 0) return -1;
        if (b == 0) return 1;
        return a * power(a, b-1);
    }

    // O(1)
    static int mod(int a, int b) {
        if (b <= 0) return -1;
        int div = a / b;
        return a - div * b;
    }

    // O(a/b)
    static int div(int a, int b) {
        int count = 0;
        int sum = 0;
        while (sum <= a) {
            sum += b;
            count++;
        }
        return count;
    }

    // O(log(n))
    static int sqrt(int n) {
        return sqrtHelper(n, 1, n);
    }

    static int sqrtHelper(int n, int min, int max) {
        if (max < min) return -1;
        int guess = (min+max)/2;
        if (guess * guess == n) return guess;
        if (guess * guess < n) {
            return sqrtHelper(n, guess + 1, max);
        } else {
            return sqrtHelper(n, min, guess - 1);
        }
    }

    // O(log(n))
    static int sumDigits(int n) {
        int sum = 0;
        while (n > 0) {
            sum += n % 10;
            n /= 10;
        }
        return sum;
    }

    public static void main(String[] args) {
        int[] a = { 1, 2, 3 };
        reverse(a);
        System.out.println(Arrays.toString(a));
        Node x = new Node(1,
            new Node(2, null, null),
            new Node(3, null, null)
        );
        System.out.println(sum(x));
        System.out.println(isPrime(7));
        System.out.println(factorial(3));
        permutation("abc");
        System.out.println(fib(12));
        allFib(7);
        powersOf2(64);
        System.out.println(product(5,5));
        System.out.println(power(2, 6));
        System.out.println(mod(10,3));
        System.out.println(sqrt(25));
        System.out.println(sumDigits(27));
    }
}