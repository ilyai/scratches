import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Vector;

class Java {
    public static abstract class Shape {
        public void printMe() {
            System.out.println("I am a shape.");
        }
        public abstract double computeArea();
    }

    public static class Circle extends Shape {
        private double rad = 5;
        public void printMe() {
            System.out.println("I am a circle.");
        }

        public double computeArea() {
            return rad * rad * 3.15;
        }
    }
    public static class Ambiguous extends Shape {
        private double area = 10;
        public double computeArea() {
            return area;
        }
    }

    public static class IntroductionOverriding {
        public static void main(String[] args) {
            Shape[] shapes = new Shape[2];
            Circle circle = new Circle();
            Ambiguous ambiguous = new Ambiguous();

            shapes[0] = circle;
            shapes[1] = ambiguous;

            for (Shape s : shapes) {
                s.printMe();
                System.out.println(s.computeArea());
            }
        }
    }

    public static void main(String[] args) {
        IntroductionOverriding.main(args);

        ArrayList<String> al = new ArrayList<>();
        Vector<String> v = new Vector<>();
        LinkedList<String> ll = new LinkedList<>();
        HashMap<String,String> hm = new HashMap<>();
    }
}