import akka.http.scaladsl.server.directives.ContentTypeResolver
import java.io.File
import java.nio.file.Paths

val file = new File("/Users/iig/tmp/e.css")
file.toURI.toString
file.toPath.toString
file.getAbsolutePath
file.getName
ContentTypeResolver.Default.apply(file.getName)

val p = Paths.get(new File("/Users/iig/IdeaProjects/pixelart-cms-appservice/player-files/xmp/static_files/widget.wg").getCanonicalPath)
p.getFileName
