import simplehtmlwriterinscala.Html5._
import simplehtmlwriterinscala.Html5.Attr.{ classes, charset }
import simplehtmlwriterinscala.Implicits._

// Data classes example

trait ParElt
case class Text (value:String) extends ParElt
case class Bold (value:String) extends ParElt

trait PostElt
case class Paragraph (elements:List[ParElt]) extends PostElt
case class Image (src:String, alt:String) extends PostElt

case class Post (title:String, author:Option[String], content:List[PostElt])

object Example {

  // Data to HTML methods

  def paragraphToHTML (par:Paragraph) = p(par.elements map {
    case Text(v) => span(v)
    case Bold(v) => span(classes("bold"))(v) })

  def postToHTML (post:Post) = seq(
    h2(post.title),
    post.author map (a => seq("author :" + a)) getOrElse empty,
    for (elt <- post.content) yield elt match {
      case p:Paragraph => paragraphToHTML(p)
      case Image(src, alt) => img(Attr.src := src, Attr.alt := alt)
    })


  def main (args:Array[String]) :Unit = {

    // example data 
    val posts = List(
      Post("a first post", author = None, List(
        Paragraph(List(Text("normal text "), Bold("bold text"))),
        Image(src = "example-image.jpg", alt = "example image"))),

      Post("a second post", author = Some("Bob"), List(
        Paragraph(List(Text("other normal text "), Bold("other bold text"))),
        Image(src = "example-image.jpg", alt = "example image"))),

      Post("a last post", author = Some("Bob"), List(
        Paragraph(List(Text("still normal text "), Bold("still bold text"))),
        Image(src = "example-image.jpg", alt = "example image"))))


    val indexHTML = html(
      head(
        meta(charset := "UTF-8"), title("Index"),
        style("span.bold { font-weight: bold }")),
      body(
        h1("Index"),
        ul {
          val psh = posts filter (_.author.nonEmpty) map postToHTML
          psh.childs map (li(_)) }),
      if ("today" != "yesterday") empty else footer("this is footer"))
    
    println("<!DOCTYPE html>" + indexHTML)
  }
}
