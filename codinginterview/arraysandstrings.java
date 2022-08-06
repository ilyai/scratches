import java.util.ArrayList;
import java.util.Arrays;

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

    public static void main(String[] args) {
        String[] a = { "foo" };
        String[] b = { "bar", "quux" };
        System.out.println(merge(a,b));
        System.out.println(joinWords(b));
    }
}