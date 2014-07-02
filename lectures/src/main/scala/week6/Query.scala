package week6

object Query {
  
  case class Book(title: String, authors: List[String])

  def main(args: Array[String]): Unit = {
    val books: List[Book] = List(
        Book(title = "Animal", authors = List("a", "b", "c")),
        Book(title = "Bird", authors = List("c", "d")),
        Book(title = "Cherry", authors = List("a", "d")),
        Book(title = "Demo", authors = List("b", "d", "e")))
    println(for (b <- books; a <- b.authors; if a startsWith("a")) yield b.title)
    /* Translate into higher order form */
    println(books.flatMap(book => book.authors.withFilter(_.startsWith("a")).map(_ => book.title)))
    println(for {b1 <- books; b2 <- books; if b1 != b2; a1 <- b1.authors; a2 <- b2.authors; if a1 == a2} yield a1)
  }

}