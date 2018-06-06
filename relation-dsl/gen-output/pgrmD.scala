{
  val x_0 = new java.io.File("data/EnFr.csv");
  val x_1 = new java.util.Scanner(x_0);
  val x_2 = new scala.collection.mutable.ArrayBuffer[java.lang.String]();
  val x_3 = new scala.collection.mutable.ArrayBuffer[java.lang.String]();
  val x_4 = new scala.collection.mutable.ArrayBuffer[java.lang.String]();
  while (x_1.hasNext()) 
    {
      val line_5 = x_1.nextLine();
      val x_6 = scala.Predef.augmentString(line_5);
      val x_7 = x_6.split("|".charAt(0));
      val x_8 = scala.Predef.refArrayOps[java.lang.String](x_7);
      val x_9 = x_8.toList;
      x_2.+=(x_9.apply(0));
      x_3.+=(x_9.apply(1));
      x_4.+=(x_9.apply(2));
      ()
    }
  ;
  val x_10 = scala.Predef.intWrapper(0);
  val x_11 = x_2.length;
  val x_12 = x_10.until(x_11);
  x_12.foreach[scala.Unit](((i_13: scala.Int) => {
    val x_14 = x_2.apply(i_13);
    val x_15 = x_3.apply(i_13);
    val x_16 = x_4.apply(i_13);
    if (x_14.==("one"))
      scala.Predef.println(relation.Row.apply(scala.collection.immutable.List.apply[java.lang.String](x_15, x_14), 2))
    else
      ()
  }))
}
