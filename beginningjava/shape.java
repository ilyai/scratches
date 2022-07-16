abstract class Shape {

}

class Circle extends Shape implements Comparable<Circle> {
    private double x, y, radius;

    Circle(double x, double y, double radius) {
        this.x = x;
        this.y = y;
        this.radius = radius;
    }

    @Override
    public int compareTo(Circle o) {
        if (radius < o.radius) return -1;
        if (radius > o.radius) return 1;
        return 0;
    }
}

class SortedShapesList<S extends Shape & Comparable<S>> {
    @SuppressWarnings("unchecked")
    private S[] shapes = (S[]) new Shape[2];
}