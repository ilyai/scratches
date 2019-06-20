import org.joda.time.DateTime
import java.io.File
import java.nio.file.{Files, Paths}


val a = DateTime.now.getMillis
new java.util.Date(a).getTime

new DateTime("255366395-07-26T01:49:48+0400").getMillis

val f = new File("/etc/passwd")
val p = f.toPath

s"${ p }/foo"

Files.size(Paths.get("/tmp/widget.wgt"))
new File("/tmp/widget.wgt").length()


"""
    $ curl --trace-ascii - http://localhost:9000/tickStream
    == Info:   Trying 127.0.0.1...
    == Info: TCP_NODELAY set
    == Info: Connected to localhost (127.0.0.1) port 9000 (#0)
    => Send header, 88 bytes (0x58)
    0000: GET /tickStream HTTP/1.1
    001a: Host: localhost:9000
    0030: User-Agent: curl/7.54.0
    0049: Accept: */*
    0056:
    <= Recv header, 17 bytes (0x11)
    0000: HTTP/1.1 200 OK
    <= Recv header, 32 bytes (0x20)
    0000: Access-Control-Allow-Origin: *
    <= Recv header, 40 bytes (0x28)
    0000: Access-Control-Allow-Credentials: true
    <= Recv header, 85 bytes (0x55)
    0000: Access-Control-Allow-Headers: Origin, Authorization, Content-Typ
    0040: e, X-Requested-With
    <= Recv header, 27 bytes (0x1b)
    0000: Server: akka-http/10.0.10
    <= Recv header, 37 bytes (0x25)
    0000: Date: Mon, 28 May 2018 16:30:58 GMT
    <= Recv header, 28 bytes (0x1c)
    0000: Transfer-Encoding: chunked
    <= Recv header, 32 bytes (0x20)
    0000: Content-Type: application/json
    <= Recv header, 2 bytes (0x2)
    0000:
    <= Recv data, 6 bytes (0x6)
    0000: 1
    0003: [
    [<= Recv data, 15 bytes (0xf)
    0000: a
    0003: {"tick":1}
    {"tick":1}<= Recv data, 6 bytes (0x6)
    0000: 1
    0003: ,
    ,<= Recv data, 15 bytes (0xf)
    0000: a
    0003: {"tick":2}
    {"tick":2}<= Recv data, 6 bytes (0x6)
    0000: 1
    0003: ,
    ,<= Recv data, 15 bytes (0xf)
    0000: a
    0003: {"tick":3}
    {"tick":3}<= Recv data, 6 bytes (0x6)
""".stripMargin