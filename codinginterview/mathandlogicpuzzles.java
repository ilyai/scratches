class MathAndLogicPuzzles {
    static boolean primeSlightlyBetter(int n) {
        if (n < 2) {
            return false;
        }
        int sqrt = (int) Math.sqrt(n);
        for (int i = 2; i <= sqrt; i++) {
            if (n % i == 0) return false;
        }
        return true;
    }

    static boolean[] sieveOfEratosthenes(int max) {
        boolean[] flags = new boolean[max+1];
        for (int i = 2; i < flags.length; i++) {
            flags[i] = true;
        }
        int prime = 2;
        while (prime <= Math.sqrt(max)) {
            crossOff(flags, prime);
            prime = getNextPrime(flags, prime);
        }
        return flags;
    }

    static void crossOff(boolean[] flags, int prime) {
        for (int i = prime*prime; i < flags.length; i += prime) {
            flags[i] = false;
        }
    }

    static int getNextPrime(boolean[] flags, int prime) {
        int next = prime+1;
        while (next < flags.length && !flags[next]) {
            next++;
        }
        return next;
    }

    public static void main(String[] args) {
        int max = 100;
        boolean[] primes = sieveOfEratosthenes(max);
        for (int i = 0; i < max; i++) {
            System.out.println(i + ": " + primes[i]);
        }
    }
}