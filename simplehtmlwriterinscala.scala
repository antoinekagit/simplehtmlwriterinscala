import scala.language.implicitConversions

package simplehtmlwriterinscala {

  // Here are classes for building basic html nodes
  object Basic {

    trait AbsNode // common trait of html nodes
    trait AbsAttr // common trait of html attributes

    // a String attribute
    case class AttrStr (name:String, value:String) extends AbsAttr {
      override def toString = "%s=\"%s\"" format (name, value)
    }
    // a list of string attribute (for ex: 'class' attribute)
    case class AttrListStr (name:String, values:Seq[String]) extends AbsAttr {
      override def toString = "%s=\"%s\"" format (name, values mkString " ")
    }

    // a basic 'block' node
    case class Node (tag:String, attrs:Seq[AbsAttr], childs:Seq[AbsNode])
        extends AbsNode {
      override def toString = {
        val attStr = if (attrs.isEmpty) "" else attrs mkString (" ", " ", " ")
        s"<$tag$attStr>${childs.mkString}</$tag>" }
    }

    // a basic 'autoclosing' node (no childrens)
    case class Leaf (tag:String, attrs:Seq[AbsAttr]) extends AbsNode {
      override def toString = {
        val attStr = if (attrs.isEmpty) "" else attrs mkString (" ", " ", " ")
        s"<$tag$attStr/>" }
    }

    // a collection of nodes, no wrapper node printed
    case class SeqNode (childs:Seq[AbsNode]) extends AbsNode {
      override def toString = childs.mkString
    }

    // a string data, no wrapper node printed
    case class StrData (strData:String) extends AbsNode {
      override def toString = strData
    }

    // an empty node, nothing printed
    object EmptyNode extends AbsNode {
      override def toString = ""
    }
  }


  // Here are some classes builded on Basic ones
  // They provide a less strict API, they are used by Html5 object (see below)
  // for ex you don't have to write 'Seq()' when attributes are empty
  object LessStrict {

    import Basic._

    // for attributes
    case class AttrStrBuilder (name:String) {
      def apply (value:String) = AttrStr(name, value)
      def := = apply _
    }
    case class AttrListStrBuilder (name:String) {
      def apply (value:String) = AttrListStr(name, Seq(value))
      def apply (values:String*) = AttrListStr(name, values)
      def := = apply _
    }

    // for nodes
    case class NodeBuilder (tag:String) {
      def apply (childs:AbsNode*) = Node(tag, Nil, childs)
      def apply (attr:AbsAttr, attrs:AbsAttr*) (childs:AbsNode*) =
        Node(tag, attr +: attrs, childs)
    }
    case class LeafBuilder (tag:String) {
      def apply (attrs:AbsAttr*) = Leaf(tag, attrs)
    }
    case class SeqNodeBuilder () {
      def apply (childs:AbsNode*) = SeqNode(childs)
    }
  }

  // Here are two implicits conversion methods
  // In a separate object to prevent 'surprise implicits'
  object Implicits {
    import Basic._
    // conversion from string to string data (see Basic above)
    implicit def string_to_AbsNode (str:String) = StrData(str)
    // conversion from sequence to seq node (see Basic above)
    implicit def seq_to_AbsNode (sn:Seq[AbsNode]) = SeqNode(sn)
    implicit def mutiter_to_AbsNode [X<:AbsNode] (in:scala.collection.mutable.Iterable[X]) :SeqNode = SeqNode(in.toSeq)
  }

  // Here are a collection of methods to build HTML5 nodes and attributes
  // You can update it or create your own, the model is simple
  object Html5 {

    import LessStrict._

    // attributes in a separate object because these names are common
    object Attr {
      lazy val alt = AttrStrBuilder("alt")
      lazy val id = AttrStrBuilder("id")
      lazy val charset = AttrStrBuilder("charset")
      lazy val classes = AttrListStrBuilder("class")
      lazy val href = AttrStrBuilder("href")
      lazy val rel = AttrStrBuilder("rel")
      lazy val src = AttrStrBuilder("src")
      lazy val titleAttr = AttrStrBuilder("title")
      def data (name:String) = AttrStrBuilder("data-" + name)
    }

    // 'block' nodes
    lazy val a = NodeBuilder("a")
    lazy val body = NodeBuilder("body")
    lazy val div = NodeBuilder("div")
    lazy val footer = NodeBuilder("footer")
    lazy val h1 = NodeBuilder("h1")
    lazy val h2 = NodeBuilder("h2")
    lazy val h3 = NodeBuilder("h3")
    lazy val h4 = NodeBuilder("h4")
    lazy val h5 = NodeBuilder("h5")
    lazy val h6 = NodeBuilder("h6")
    lazy val head = NodeBuilder("head")
    lazy val html = NodeBuilder("html")
    lazy val i = NodeBuilder("i")
    lazy val li = NodeBuilder("li")
    lazy val p = NodeBuilder("p")
    lazy val pre = NodeBuilder("pre")
    lazy val section = NodeBuilder("section")
    lazy val span = NodeBuilder("span")
    lazy val strong = NodeBuilder("strong")
    lazy val style = NodeBuilder("style")
    lazy val title = NodeBuilder("title")
    lazy val ul = NodeBuilder("ul")

    // 'auto-closing' nodes
    lazy val base = LeafBuilder("base")
    lazy val br = LeafBuilder("br")
    lazy val img = LeafBuilder("img")
    lazy val link = LeafBuilder("link")
    lazy val meta = LeafBuilder("meta")

    // the two special nodes
    lazy val empty = Basic.EmptyNode
    lazy val seq = SeqNodeBuilder()
  }
}
