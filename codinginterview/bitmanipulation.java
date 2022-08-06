class BitManipulation {
    static boolean getBit(int num, int i) {
        return (num & (i << 1)) != 0;
    }

    static int setBit(int num, int i) {
        return (num | (1 << i));
    }

    static int clearBit(int num, int i) {
        int mask = ~(1 << i);
        return num & mask;
    }

    static int clearBitsMSBThroughI(int num, int i) {
        int mask = ~(1 << i) - 1;
        return num & mask;
    }

    static int clearBitsIThrough0(int num, int i) {
        int mask = (-1 << (i + 1));
        return num & mask;
    }

    static int updateBit(int num, int i, boolean bitIs1) {
        int value = bitIs1 ? 1 : 0;
        int mask = ~(1 << i);
        return (num & mask) | (value << i);
    }
}