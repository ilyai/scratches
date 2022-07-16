import java.sql.Driver;
import java.util.*;

class Image {
    Image() {
        System.out.println("Image() called");
    }

    Image(String filename) {
        this(filename, null);
        System.out.println("Image(String filename) called");
    }

    Image(String filename, String imageType) {
        System.out.println("Image(String filename, String imageType) called");
        if (filename != null) {
            System.out.println("reading "+filename);
        }
        if (imageType != null) {
            System.out.println("interpreting "+filename+" as storing a "+imageType+" image");
        }
    }
}

class Vehicle {
    String make;
    String model;
    int numDoors = 4;
    static int counter;
    Vehicle(String make, String model) {
        this(make, model, 4);
    }
    Vehicle(String make, String model, int nDoors) {
        this.make = make;
        this.model = model;
        numDoors = nDoors;
        counter++;
    }
    void printDetails() {
        System.out.println("Make = "+make);
    }
}

class Car extends Vehicle {
    private int numWheels;
    Car(String make, String model, int numWheels) {
        super(make, model);
        this.numWheels = numWheels;
    }
}

class JDBCFilterDriver {
    static private Driver d;
    static {
        try {
            Class c = Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
            d = (Driver) c.newInstance();
//            DriverManager.registerDriver(new JDBCFilterDriver());
        } catch (Exception e) {
            System.out.println(e);
        }
    }
}

class Graphics {
    double[] sines;
    double[] cosines;
    {
        sines = new double[360];
        cosines = new double[sines.length];
        for (int i = 0; i < sines.length; i++) {
            sines[i] = Math.sin(Math.toRadians(i));
            cosines[i] = Math.cos(Math.toRadians(i));
        }
    }
}

abstract class Shape {
    abstract void draw();
}

class A {}
class B extends A {
    void m() {}
}

interface Colors {
    int RED = 1;
    int GREEN = 1;
    int BLUE = 1;
}

interface Drawable extends Colors {
    void draw(int color);
}

class EnclosingClass {
    private static int i;
    private static void m1() {
        System.out.println(i);
    }
    private void m2() {
        EnclosedClass.accessEnclosedClass();
    }
    static class EnclosedClass {
        static void accessEnclosedClass() {
            i = 1;
            m1();
        }
    }
}

class EnclosingNonstaticClass {
    private int i;
    private void m() {
        System.out.println(i);
    }
    class EnclosedClass {
        void accessEnclosingClass() {
            i = 1;
            m();
        }
    }
}

abstract class Speaker {
    abstract void speak();
}

class ACDemo {
    static void demo(String s) {
        new Speaker() {
            @Override
            void speak() {
                System.out.println(s);
            }
        }.speak();
    }
}

class EnclosingLocalClass {
    void m(int x) {
        int y = x*2;
        class LocalClass {
            int a = x;
            int b = y;
        }
        LocalClass lc = new LocalClass();
        System.out.println(lc.a);
        System.out.println(lc.b);
    }
}

class X {
    interface  A {

    }
    @Deprecated
    static interface B {

    }
}

@interface Stub {

}

class Deck {
    @Stub
    public void shuffle() {}
}

class QueueEmptyException extends Exception {}
class QueueFullException extends Exception {}

class InvalidMediaFormatException extends Exception {
    private final String expectedFormat;
    private final String existingFormat;
    public InvalidMediaFormatException(String expectedFormat, String existingFormat) {
        if (expectedFormat == null) {
            throw new IllegalArgumentException("Expected format is undefined");
        }
        this.expectedFormat = expectedFormat;
        this.existingFormat = existingFormat;
    }
    public String getExistingFormat() {
        return existingFormat;
    }
    public String getExpectedFormat() {
        return expectedFormat;
    }
}

final class Media {
    public static void convert(String srcName, String dstName) {

    }
}

class Queue<E> {
    private E[] elements;
    private int head, tail;
    @SuppressWarnings("unchecked")
    Queue(int size) {
        elements = (E[]) new Object[size];
        head = 0;
        tail = 0;
    }
    void insert(E element) throws QueueFullException {
        if (isFull()) throw new QueueFullException();
        elements[tail] = element;
        tail = (tail+1) % elements.length;
    }
    E remove() throws QueueEmptyException {
        if (isEmpty()) throw new QueueEmptyException();
        E element = elements[head];
        head = (head+1) % elements.length;
        return element;
    }
    boolean isEmpty() {
        return head == tail;
    }
    boolean isFull() {
        return (tail+1) % elements.length == head;
    }
}

interface Directions {
    int NORTH = 0;
}

class TrafficFlow implements Directions {
    static void showDirection(int dir) {
//        import static java.lang.Math.*;
        switch (dir) {
            case NORTH:
                System.out.println("Moving north");
                break;
        }
    }
}

abstract class Rectangle {
    abstract double getX();
    static class Double extends Rectangle {
        double getX() { return 0; }
    }
    static class Float extends Rectangle {
        double getX() { return 0; }
    }
}

class Circle extends Shape implements Comparable<Circle> {
    @Override
    void draw() {

    }
    @Override
    public int compareTo(Circle o) {
        return 0;
    }
}

class SortedShapesList<S extends Shape & Comparable<S>> {

}

//class Coin {
//    final static int PENNY = 0;
//    final static int NICKEL = 1;
//    final static int DIME = 2;
//    final static int QUARTER = 3;
//}

enum Coin {
    PENNY(1),
    NICKEL(5),
    DIME(10),
    QUARTER(25);

    private final int denomValue;
    Coin(int denomValue) {
        this.denomValue = denomValue;
    }
    int denomValue() {
        return denomValue;
    }
    int toDenomination(int numPennies) {
        return numPennies / denomValue;
    }
}

enum Token {
    IDENTIFIER("ID"),
    INTEGER("INT"),
    LPAREN("("),
    RPAREN(")"),
    COMMA(",");

    private final String tokValue;
    Token(String tokValue) {
        this.tokValue = tokValue;
    }
    @Override
    public String toString() {
        return tokValue;
    }
}

enum TempConversion {
    C2F("Celsius to Fahrenheit") {
        @Override
        double convert(double value) {
            return value*9.0/5.0+32.0;
        }
    },
    F2C("Fahrenheit to Celsius") {
        @Override
        double convert(double value) {
            return (value-32.0)*5.0/9.0;
        }
    };

    TempConversion(String desc) {

    }
    private String desc;
    @Override
    public String toString() {
        return desc;
    }
    abstract double convert(double value);
}

enum Weekday { SUNDAY, SATURDAY }

class Scratch {
    public static void main(String[] args) {
        assert args.length > 0;
        outer:
        for (int i = 0; i < args.length; i++) {
            switch (args[i]) {
                case "-v":
                case "-V":
                    System.out.println("Version: 1.0");
                    break outer;
                default:
                    System.out.println("Usage: ...");
            }
        }
        Scratch s = new Scratch();
        Image image = new Image();
        System.out.println();
        image = new Image("image.png");
        System.out.println();
        image = new Image("image.png", "PNG");
        (new EnclosingNonstaticClass()).new EnclosedClass().accessEnclosingClass();
        ACDemo.demo("speak");
        new EnclosingLocalClass().m(2);
        s.collections();
        Rectangle r = new Rectangle.Double();
        System.out.println(r.getX());
        System.out.println(Integer.toBinaryString(12));
    }

    double sum(double... values) {
        int total = 0;
        for (int i = 0; i < values.length; i++) {
            total += values[i];
        }
        return total;
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
    }

    void downcastDemo() {
        A a = new A();
        B b = (B) a;
        b.m();
    }

    void copyList(List<? extends String> src, List<? super String> dest) {
        for (int i = 0; i < src.size(); i++) {
            dest.add(src.get(i));
        }
    }

    <T> void copyList2(List<T> src, List<T> dest) {
        for (int i = 0; i < src.size(); i++) {
            dest.add(src.get(i));
        }
    }

    void outputList(List<?> list) {
        for (int i = 0; i < list.size(); i++) {
            System.out.println(list.get(i));
        }
        System.out.println();
    }

    /*void copy(String srcFile, String dstFile) {
        try (FileInputStream fis = new FileInputStream(srcFile);
             FileOutputStream fos = new FileOutputStream(dstFile)) {
            int b;
            while ((b = fis.read()) != -1) fos.write(b);
        }
    }*/

    void collections() {

        String[] weekDays = { "Sun", "Mon" };
        List<String> ls = new ArrayList<>();
        for (String weekDay : weekDays) {
            ls.add(weekDay);
        }
        Iterator<String> iter = ls.iterator();
        while (iter.hasNext()) {
            System.out.println(iter.next());
        }
        List<String> ll = new LinkedList<>();
        SortedSet<String> ss = new TreeSet<>();
        NavigableSet<String> ns = new TreeSet<>();
        Set<String> hs = new HashSet<>();
        Set<Weekday> daysOff = EnumSet.of(Weekday.SATURDAY, Weekday.SUNDAY);
        Set<Weekday> weekDaysSet = EnumSet.range(Weekday.SATURDAY, Weekday.SUNDAY);
        Set<Weekday> allWeekDays = EnumSet.allOf(Weekday.class);
        BitSet bs = new BitSet();
        Queue<String> q = new Queue<>(10);
        PriorityQueue<Integer> qi = new PriorityQueue<>();
        Deque<String> stack = new ArrayDeque<>();
        Map<String,Integer> tm = new TreeMap<>();
        Map<String,Integer> hm = new HashMap<>();
        Map<String,Integer> ihm = new IdentityHashMap<>();
        Map<String,Integer> whm = new WeakHashMap<>();
        EnumMap<Weekday, Integer> em = new EnumMap<Weekday, Integer>(Weekday.class);
        SortedMap<String,Integer> sm = new TreeMap<>();
        NavigableMap<String,Integer> nm = new TreeMap<>();
        Vector<String> v = new Vector<>();
        Hashtable<String,Integer> ht = new Hashtable<>();
        Arrays.asList("Robin", "Oriole");
        Arrays.binarySearch(weekDays, "Sun");
        Arrays.fill(weekDays, " ");
        Arrays.sort(weekDays);
        Collections.min(ls);
        Collections.reverse(ls);
        Collections.singletonList(null);
        Collections.synchronizedSet(ss);
        Collections.unmodifiableMap(tm);
        Collections.emptyList();

    }
}