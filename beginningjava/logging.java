interface Logger {
    int CONSOLE = 0;
    int FILE = 1;
    boolean connect();
    boolean disconnect();
    boolean log(String msg);
}

class Console implements Logger {
    private String dstName;
    Console(String dstName) {
        this.dstName = dstName;
    }
    @Override
    public boolean connect() {
        return true;
    }

    @Override
    public boolean disconnect() {
        return true;
    }

    @Override
    public boolean log(String msg) {
        System.out.println(msg);
        return true;
    }
}

abstract class LoggerFactory {
    public static Logger newLogger(int dstType, String... dstName) {
        switch (dstType) {
            case Logger.CONSOLE: return new Console(dstName.length  == 0 ? null : dstName[0]);
            default: return null;
        }
    }
}

class TestLogger {
    public static void main(String[] args) {
        Logger logger = LoggerFactory.newLogger(Logger.CONSOLE);
        if (logger.connect()) {
            logger.log("test message #1");
            logger.disconnect();
        }
    }
}