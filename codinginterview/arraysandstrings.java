import java.util.*;

class ArraysAndStrings {
    static ArrayList<String> merge(String[] words, String[] more) {
        // amortized insertion time O(1)
        ArrayList<String> sentence = new ArrayList<>();
        for (String w : words) sentence.add(w);
        for (String w : more) sentence.add(w);
        return sentence;
    }

    static String joinWords(String[] words) {
        StringBuilder sb = new StringBuilder();
        for (String w : words) sb.append(w);
        return sb.toString();
    }

    // O(n)
    static boolean isUniqueChars(String str) {
        int R = 128;
        if (str.length() > R) return false;
        boolean[] alpha = new boolean[R];
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (alpha[ch]) return false;
            alpha[ch] = true;
        }
        return true;
    }

    // O(n)
    static boolean isUniqueCharsBS(String str) {
        int bitset = 0;
        for (int i = 0; i < str.length(); i++) {
            int ch = str.charAt(i) - 'a';
            if ((bitset & (1 << ch)) > 0) return false;
            bitset |= 1 << ch;
        }
        return true;
    }

    // O(n^2) / O(1)
    static boolean isUniqueCharsBF(String str) {
        for (int i = 0; i < str.length(); i++) {
            for (int j = i+1; j < str.length(); j++) {
                if (str.charAt(i) == str.charAt(j)) return false;
            }
        }
        return true;
    }

    // O(n log(n))
    static boolean isUniqueCharsSort(String str) {
        char[] chars = str.toCharArray();
        Arrays.sort(chars);
        for (int i = 1; i < chars.length; i++) {
            if (chars[i-1] == chars[i]) return false;
        }
        return true;
    }

    @FunctionalInterface
    interface StringSorter {
        String sort(String str);
    }

    // O(a+b log(a+b))
    static boolean arePermutations(String a, String b) {
        StringSorter sorter = (String str) -> {
            char[] chars = str.toCharArray();
            Arrays.sort(chars);
            return new String(chars);
        };
        return sorter.sort(a).equals(sorter.sort(b));
    }


    // O(a+b)
    static boolean arePermutationsCC(String a, String b) {
        if (a.length() != b.length()) return false;
        int R = 128;
        int[] charCount = new int[R];
        for (int i = 0; i < a.length(); i++) {
            charCount[a.charAt(i)]++;
        }
        for (int i = 0; i < b.length(); i++) {
            charCount[b.charAt(i)]--;
            if (charCount[b.charAt(i)] < 0) return false;
        }
        return true;
    }

    // O(n)
    static String urlify(String str, int len) {
        char[] s = str.toCharArray();
        for (int i = len-1, j = s.length-1; i >= 0 && j >= 0; i--, j--) {
            if (s[i] == ' ') {
                s[j--] = '0';
                s[j--] = '2';
                s[j] = '%';
            } else {
                s[j] = s[i];
            }
        }
        return new String(s);
    }

    static boolean isPalindromePermutation(String str) {
        HashMap<Character, Boolean> parityMap = new HashMap<>();
        int len = 0;
        for (int i = 0; i < str.length(); i++) {
            char ch = Character.toLowerCase(str.charAt(i));
            if (ch == ' ') continue;
            len++;
            if (parityMap.containsKey(ch)) {
                parityMap.put(ch, !parityMap.get(ch));
            } else {
                parityMap.put(ch, true);
            }
        }
        int count = 0;
        for (boolean b : parityMap.values()) {
            if (b) count++;
        }
        if (count == 0 && len % 2 == 0) return true;
        else return count == 1 && len % 2 != 0;
    }

    static class PermutationUtils {
        /* Check that no more than one character has an odd count. */
        static boolean checkMaxOneOdd(int[] table) {
            boolean foundOdd = false;
            for (int count : table) {
                if (count % 2 != 0) {
                    if (foundOdd) return false;
                    foundOdd = true;
                }
            }
            return true;
        }
        /* Map each character to a number. a -> 0, b -> 1, c -> 2, etc.
         * This is case insensitive. Non-letter characters map to -1. */
        static int getCharNumber(Character c) {
            int a = Character.getNumericValue('a');
            int z = Character.getNumericValue('z');
            int val = Character.getNumericValue(c);
            if (val >= a && val <= z) {
                return val - a;
            }
            return -1;
        }
        /* Count how many times each character appears */
        static int[] buildCharFrequencyTable(String phrase) {
            int[] table = new int[Character.getNumericValue('z') -
                    Character.getNumericValue('a') + 1];
            for (char c : phrase.toCharArray()) {
                int x = getCharNumber(c);
                if (x > -1) {
                    table[x]++;
                }
            }
            return table;
        }
        /* Create a bit vector for the string. For each letter with value i, toggle the
         * ith bit */
        static int createBitVector(String phrase) {
            int bitVector = 0;
            for (char c : phrase.toCharArray()) {
                int x = getCharNumber(c);
                bitVector = toggle(bitVector, x);
            }
            return bitVector;
        }
        /* Toggle the ith bit in the integer */
        static int toggle(int bitVector, int index) {
            if (index < 0) return bitVector;
            int mask = 1 << index;
            if ((bitVector & mask) == 0) {
                bitVector |= mask;
            } else {
                bitVector &= ~mask;
            }
            return bitVector;
        }
        /* Check that exactly one bit is set by subtracting one from the integer and
         * ANDing it with the original integer */
        static boolean checkExactlyOneBitSet(int bitVector) {
            return (bitVector & (bitVector - 1)) == 0;
        }
        /* Check if s1 and s2 differ only by one character at most */
        static boolean oneEditReplace(String s1, String s2) {
            boolean foundDifference = false;
            for (int i = 0; i < s1.length(); i++) {
                if (s1.charAt(i) != s2.charAt(i)) {
                    if (foundDifference) return false;
                    foundDifference = true;
                }
            }
            return true;
        }
        /* Check if you can insert a character into s1 to make s2 */
        static boolean oneEditInsert(String s1, String s2) {
            for (int i = 0, j = 0; i < s1.length() && j < s2.length(); j++) {
                if (s1.charAt(i) != s2.charAt(j)) {
                    if (i != j) return false;
                } else {
                    i++;
                }
            }
            return true;
        }
    }

    static boolean isPermutationOfPalindrome(String phrase) {
        int[] table = PermutationUtils.buildCharFrequencyTable(phrase);
        return PermutationUtils.checkMaxOneOdd(table);
    }

    static boolean isPermutationOfPalindromeOptimized(String phrase) {
        int countOdd = 0;
        int[] table = new int[Character.getNumericValue('z') -
                Character.getNumericValue('a') + 1];
        for (char c : phrase.toCharArray()) {
            int x = PermutationUtils.getCharNumber(c);
            if (x != 1) {
                table[x]++;
                if (table[x] % 2 == 1) {
                    countOdd++;
                } else {
                    countOdd--;
                }
            }
        }
        return countOdd <= 1;
    }

    static boolean isPermutationOfPalindromeOptimizedBS(String phrase) {
        int bitVector = PermutationUtils.createBitVector(phrase);
        return bitVector == 0 || PermutationUtils.checkExactlyOneBitSet(bitVector);
    }

    static boolean isOneEditAway(String orig, String edit) {
        if (Math.abs(orig.length() - edit.length()) > 1) return false;
        int i, j, mismatches = 0;
        for (i = 0, j = 0; i < orig.length()-1 && j < edit.length()-1; i++, j++) {
            if (orig.charAt(i) != edit.charAt(j)) {
                mismatches++;
                if (orig.charAt(i+1) == edit.charAt(j)) i++;
                else if (orig.charAt(i) == edit.charAt(j+1)) j++;
                else if (orig.charAt(i+1) == edit.charAt(j+1)) {
                    i++;
                    j++;
                }
            }
        }
        if (orig.charAt(i) != edit.charAt(j)) mismatches++;
        return mismatches <= 1;
    }

    static boolean oneEditAway(String first, String second) {
        if (first.length() == second.length()) {
            return PermutationUtils.oneEditReplace(first, second);
        }
        else if (first.length()+1 == second.length()) {
            return PermutationUtils.oneEditInsert(first, second);
        }
        else if (first.length() == second.length()+1) {
            return PermutationUtils.oneEditInsert(second, first);
        }
        return false;
    }

    static boolean oneEditAwayMerged(String first, String second) {
        /* Length checks */
        if (Math.abs(first.length() - second.length()) > 1) {
            return false;
        }
        /* Get shorter and longer string */
        String s = first.length() < second.length() ? first : second;
        String l = first.length() < second.length() ? second : first;

        int i = 0, j = 0;
        boolean foundDifference = false;
        while (j < l.length() && i < s.length()) {
            if (s.charAt(i) != l.charAt(j)) {
                /* Ensure that this is the first difference found */
                if (foundDifference) return false;
                foundDifference = true;
                /* On replace, move shorter pointer */
                if (s.length() == l.length()) i++;
            } else {
                i++;    // If matching, move shorter pointer
            }
            j++;    // Always move pointer for longer string
        }
        return true;
    }

    static String compress(String s) {
        int repeats = 1;
        StringBuilder compressed = new StringBuilder();
        if (s.length() == 0) return "";
        else if (s.length() == 1) return s;
        for (int i = 1; i < s.length(); i++) {
            if (s.charAt(i-1) != s.charAt(i)) {
                compressed.append(s.charAt(i-1)).append(repeats);
                repeats = 1;
            } else {
                repeats++;
            }
        }
        if (repeats > 0) compressed.append(s.charAt(s.length()-1)).append(repeats);
        return compressed.length() < s.length() ? compressed.toString() : s;
    }

    static String compressRV(String str) {
        int countConsecutive = 0;
        StringBuilder compressed = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            countConsecutive++;
            if (i + 1 >= str.length() || str.charAt(i) != str.charAt(i+1)) {
                compressed.append(str.charAt(i)).append(countConsecutive);
                countConsecutive = 0;
            }
        }
        return compressed.length() < str.length() ? compressed.toString() : str;
    }

    static String compressRV2(String str) {
        /* Check final length and return input if it would be longer */
        int finalLength = countCompression(str);
        if (finalLength >= str.length()) return str;
        int countConsecutive = 0;
        StringBuilder compressed = new StringBuilder(finalLength);
        for (int i = 0; i < str.length(); i++) {
            countConsecutive++;
            if (i + 1 >= str.length() || str.charAt(i) != str.charAt(i+1)) {
                compressed.append(str.charAt(i)).append(countConsecutive);
                countConsecutive = 0;
            }
        }
        return compressed.toString();
    }

    static int countCompression(String str) {
        int compressedLength = 0;
        int countConsecutive = 0;
        for (int i = 0; i < str.length(); i++) {
            countConsecutive++;
            if (i + 1 >= str.length() || str.charAt(i) != str.charAt(i+1)) {
                compressedLength += 1 + String.valueOf(countConsecutive).length();
                countConsecutive = 0;
            }
        }
        return compressedLength;
    }

    static boolean rotate(int[][] matrix) {
        if (matrix.length == 0 || matrix[0].length != matrix.length) return false;
        int n = matrix.length;
        for (int layer = 0; layer < n / 2; layer++) {
            int first = layer;
            int last = n-1-layer;
            for (int i = first; i < last; i++) {
                int offset = i - first;
                int top = matrix[first][i];
                matrix[first][i] = matrix[last-offset][first];  // left -> top
                matrix[last-offset][first] = matrix[last][last-offset]; // bottom -> left
                matrix[last][last-offset] = matrix[i][last];    // right -> bottom
                matrix[i][last] = top;  // right <- saved top
            }
        }
        return true;
    }

    static void nullifyRow(int[][] matrix, int row) {
        for (int i = 0; i < matrix[0].length; i++) {
            matrix[row][i] = 0;
        }
    }

    static void nullifyColumn(int[][] matrix, int col) {
        for (int i = 0; i < matrix.length; i++) {
            matrix[i][col] = 0;
        }
    }

    static void setZeros(int[][] matrix) {
        boolean[] row = new boolean[matrix.length];
        boolean[] column = new boolean[matrix[0].length];
        // Store the row and column index with value 0
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[0].length; j++) {
                if (matrix[i][j] == 0) {
                    row[i] = true;
                    column[j] = true;
                }
            }
        }
        // Nullify rows
        for (int i = 0; i < row.length; i++) {
            if (row[i]) nullifyRow(matrix, i);
        }
        // Nullify columns
        for (int i = 0; i < column.length; i++) {
            if (column[i]) nullifyColumn(matrix, i);
        }
    }

    static void setZerosV2(int[][] matrix) {
        boolean rowHasZero = false;
        boolean colHasZero = false;
        // Check if first row has a zero
        for (int i = 0; i < matrix[0].length; i++) {
            if (matrix[0][i] == 0) {
                rowHasZero = true;
                break;
            }
        }
        // Check if first column has a zero
        for (int i = 0; i < matrix.length; i++) {
            if (matrix[i][0] == 0) {
                colHasZero = true;
                break;
            }
        }
        // Check for zeros in the rest of the array
        for (int i = 1; i < matrix.length; i++) {
            for (int j = 1; j < matrix[0].length; j++) {
                if (matrix[i][j] == 0) {
                    matrix[i][0] = 0;
                    matrix[0][j] = 0;
                }
            }
        }
        // Nullify rows based on values in first column
        for (int i = 1; i < matrix.length; i++) {
            if (matrix[i][0] == 0) nullifyRow(matrix, i);
        }
        // Nullify columns based on values in first row
        for (int i = 1; i < matrix[0].length; i++) {
            if (matrix[0][i] == 0) nullifyColumn(matrix, i);
        }
        // Nullify first row
        if (rowHasZero) nullifyRow(matrix, 0);
        // Nullify first column
        if (colHasZero) nullifyColumn(matrix, 0);
    }

    static boolean isRotation(String s1, String s2) {
        return s1.length() == s2.length() && (s1+s1).contains(s2);
    }

    static <E> void dedup(LinkedList<E> list) {
        HashSet<E> set = new HashSet<>();
        for (E item : list) {
            if (set.contains(item)) list.remove(item);
            else set.add(item);
        }
    }

    static <E> void dedupV2(LinkedList<E> list) {
        LinkedList<E> roCopy = (LinkedList<E>) list.clone();
        Iterator<E> it = roCopy.iterator();
        for (int i = 0; it.hasNext(); i++) {
            E item = it.next();
            for (ListIterator<E> iter = roCopy.listIterator(i + 1); iter.hasNext(); ) {
                E e = iter.next();
                if (e == item) list.remove(e);
            }
        }
    }

    static <E> int printKthToLast(Iterator<E> iter, E item, int k) {
        if (!iter.hasNext()) return 0;
        int index = printKthToLast(iter, iter.next(), k) + 1;
        if (index == k) {
            System.out.println(k + " th to last node is " + item);
        }
        return index;
    }

    static <E> E nthToLast(LinkedList<E> list, int k) {
        Iterator<E> iter1 = list.iterator();
        Iterator<E> iter2 = list.iterator();
        for (int i = 0; i <= k; i++) {
            if (!iter1.hasNext()) return null;  // Out of bounds
            iter1.next();
        }
        while (iter1.hasNext()) {
            iter1.next();
            iter2.next();
        }
        return iter2.next();
    }

    static <E> boolean deleteNode(LinkedList<E> list) {
        Iterator<E> ascIter = list.iterator();
        Iterator<E> descIter = list.descendingIterator();
        while (ascIter.hasNext() && descIter.hasNext()) {
            if (ascIter.next() == descIter.next()) {
                ascIter.remove();
                return true;
            }
        }
        return false;
    }

    static <E extends Comparable<E>> LinkedList<E> partition(LinkedList<E> list, E pivot) {
        LinkedList<E> lessList = new LinkedList<>();
        LinkedList<E> greaterList = new LinkedList<>();
        for (E item : list) {
            if (item.compareTo(pivot) < 0) lessList.add(item);
            else greaterList.add(item);
        }
        lessList.add(pivot);
        lessList.addAll(greaterList);
        return lessList;
    }

    public static void main(String[] args) {
        String[] a = { "foo" };
        String[] b = { "bar", "quux" };
        System.out.println(merge(a,b));
        System.out.println(joinWords(b));
        System.out.println(isUniqueCharsSort("qwertyuiopasdfghjklzxcvbnm"));
        System.out.println(arePermutationsCC("foo", "ofo"));
        System.out.println(urlify("Mr John Smith    ", 13));
//        System.out.println(isPalindromePermutation("tactcoapapa"));
//        System.out.println(isPermutationOfPalindrome("tactcoapapa"));
//        System.out.println(isPermutationOfPalindromeOptimized("tactcoapapa"));
//        System.out.println(isPermutationOfPalindromeOptimizedBS("tactcoapapa"));
        System.out.println(oneEditAwayMerged("pale", "ple"));
        System.out.println(oneEditAwayMerged("pales", "pale"));
        System.out.println(oneEditAwayMerged("pale", "bale"));
        System.out.println(oneEditAwayMerged("pale", "bae"));
        System.out.println(compressRV2("aabcccccaaa"));
        System.out.println(compressRV2("abca"));
        System.out.println(compressRV2("ab"));
        System.out.println(compressRV2("a"));
        System.out.println(compressRV2("aaa"));
        System.out.println(compressRV2("aaab"));
        int[][] matrix = { { 0, 1 }, { 2, 3} };
        System.out.println(Arrays.deepToString(matrix));
        rotate(matrix);
        System.out.println(Arrays.deepToString(matrix));
        setZerosV2(matrix);
        System.out.println(Arrays.deepToString(matrix));
        System.out.println(isRotation("waterbottle", "erbottlewat"));
        LinkedList<Integer> ll = new LinkedList<>();
        ll.add(3);
        ll.add(1);
        ll.add(2);
//        dedupV2(ll);
//        System.out.println(ll);
//        printKthToLast(ll.iterator(), null, 2);
//        System.out.println(nthToLast(ll, 2));
//        deleteNode(ll);
        System.out.println(ll);
        System.out.println(partition(ll, 2));
    }
}