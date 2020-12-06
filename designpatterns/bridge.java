class Client {
    public static void main(String[] args) {
        Shape r1, r2;
        Drawing dp;

        dp = new DrawingV1();
        r1 = new Rectangle(dp, 1, 1, 2, 2);

        dp = new DrawingV2();
        r2 = new Circle(dp, 2, 2, 3);

        r1.draw();
        r2.draw();
    }
}

abstract class Drawing {
    abstract public void drawLine(double x1, double y1,
                                  double x2, double y2);
    abstract public void drawCircle(double x, double y, double r);
}

class DrawingImpl1 {
    static void drawThisLine(double x1, double y1, double x3, double y2) {
        System.out.println("dp1: drawing this line");
    }
    static void drawThisCircle(double x, double y, double r) {
        System.out.println("dp1: drawing this circle");
    }
}

class DrawingImpl2 {
    static void drawLine(double x1, double y1, double x3, double y2) {
        System.out.println("dp2: drawing line");
    }
    static void drawCircle(double x, double y, double r) {
        System.out.println("dp2: drawing circle");
    }
}

class DrawingV1 extends Drawing {
    @Override
    public void drawLine(double x1, double y1, double x2, double y2) {
        DrawingImpl1.drawThisLine(x1, y1, x2, y2);
    }

    @Override
    public void drawCircle(double x, double y, double r) {
        DrawingImpl1.drawThisCircle(x, y, r);
    }
}

class DrawingV2 extends Drawing {
    @Override
    public void drawLine(double x1, double y1, double x2, double y2) {
        DrawingImpl2.drawLine(x1, y1, x2, y2);
    }

    @Override
    public void drawCircle(double x, double y, double r) {
        DrawingImpl2.drawCircle(x, y, r);
    }
}

abstract class Shape {
    abstract public void draw();
    private final Drawing _dp;

    Shape(Drawing dp) {
        _dp = dp;
    }

    public void drawLine(double x1, double y1,
                         double x2, double y2) {
        _dp.drawLine(x1, y1, x2, y2);
    }

    public void drawCircle(double x, double y, double r) {
        _dp.drawCircle(x, y, r);
    }
}

class Rectangle extends Shape {
    private final double _x1, _y1, _x2, _y2;
    public Rectangle(Drawing dp,
                     double x1, double y1,
                     double x2, double y2) {
        super(dp);
        _x1 = x1; _x2 = x2;
        _y1 = y1; _y2 = y2;
    }

    @Override
    public void draw() {
        drawLine(_x1, _y1, _x2, _y1);
        drawLine(_x2, _y1, _x2, _y2);
        drawLine(_x2, _y2, _x1, _y2);
        drawLine(_x1, _y2, _x1, _y1);
    }
}

class Circle extends Shape {
    private final double _x, _y, _r;
    public Circle(Drawing dp,
                  double x, double y, double r) {
        super(dp);
        _x = x; _y = y; _r = r;
    }

    @Override
    public void draw() {
        drawCircle(_x, _y, _r);
    }
}