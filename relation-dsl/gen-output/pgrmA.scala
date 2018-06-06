{
  val x_0 = new java.io.File("data/En.csv");
  val x_1 = new java.util.Scanner(x_0);
  val x_2 = new scala.collection.mutable.ArrayBuffer[java.lang.String]();
  val x_3 = new scala.collection.mutable.ArrayBuffer[java.lang.String]();
  while (x_1.hasNext()) 
    {
      val line_4 = x_1.nextLine();
      val x_5 = scala.Predef.augmentString(line_4);
      val x_6 = x_5.split("|".charAt(0));
      val x_7 = scala.Predef.refArrayOps[java.lang.String](x_6);
      val x_8 = x_7.toList;
      x_2.+=(x_8.apply(0));
      x_3.+=(x_8.apply(1));
      ()
    }
  ;
  val x_9 = scala.Predef.intWrapper(0);
  val x_10 = x_2.length;
  val x_11 = x_9.until(x_10);
  x_11.foreach[scala.Unit](((i_12: scala.Int) => {
    val x_13 = x_2.apply(i_12);
    val x_14 = x_3.apply(i_12);
    if (x_13.==("one"))
      scala.Predef.println(relation.Row.apply(scala.collection.immutable.List.apply[java.lang.String](x_14, x_13), 2))
    else
      ()
  }))
}
