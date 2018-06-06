package test

object TestTest extends App {
  
  val r = Test2.test(Some(123).map(_ + 1))
  
  println(r)
  
}
