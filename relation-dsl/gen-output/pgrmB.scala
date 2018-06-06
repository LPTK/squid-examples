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
  val x_9 = new java.io.File("data/Fr.csv");
  val x_10 = new java.util.Scanner(x_9);
  val x_11 = new scala.collection.mutable.ArrayBuffer[java.lang.String]();
  val x_12 = new scala.collection.mutable.ArrayBuffer[java.lang.String]();
  while (x_10.hasNext()) 
    {
      val line_13 = x_10.nextLine();
      val x_14 = scala.Predef.augmentString(line_13);
      val x_15 = x_14.split("|".charAt(0));
      val x_16 = scala.Predef.refArrayOps[java.lang.String](x_15);
      val x_17 = x_16.toList;
      x_11.+=(x_17.apply(0));
      x_12.+=(x_17.apply(1));
      ()
    }
  ;
  val x_18 = new scala.Array[scala.Int](4096);
  val x_19 = scala.collection.mutable.ArrayBuffer.fill[java.lang.String](4096)(((null): java.lang.String));
  val x_20 = scala.collection.mutable.ArrayBuffer.fill[java.lang.String](4096)("");
  val x_21 = scala.collection.mutable.ArrayBuffer.fill[java.lang.String](4096)("");
  var count_22: scala.Int = 0;
  val x_23 = scala.Predef.intWrapper(0);
  val x_24 = x_23.until(4096);
  x_24.foreach[scala.Unit](((i_25: scala.Int) => x_18.update(i_25, -1)));
  val x_26 = scala.Predef.intWrapper(0);
  val x_27 = x_2.length;
  val x_28 = x_26.until(x_27);
  x_28.foreach[scala.Unit](((i_29: scala.Int) => {
    val x_30 = x_2.apply(i_29);
    val x_31 = x_3.apply(i_29);
    var v_32: scala.Int = x_31.hashCode().%(4096);
    while ({
      val x_33 = v_32;
      val x_34 = x_18.apply(x_33);
      x_34.!=(-1).&&({
        val x_35 = v_32;
        val x_36 = x_18.apply(x_35);
        val x_37 = x_19.apply(x_36);
        x_37.!=(x_31)
      })
    }) 
      {
        val x_38 = v_32;
        v_32 = x_38.+(1).%(4096)
      }
    ;
    val x_39 = v_32;
    val x_40 = x_18.apply(x_39);
    if (x_40.!=(-1))
      {
        val index_41 = count_22;
        val x_42 = count_22;
        count_22 = x_42.+(1);
        x_18.update(x_39, index_41);
        x_19.update(index_41, x_31);
        x_20.update(index_41, x_30);
        x_21.update(index_41, x_31)
      }
    else
      ()
  }));
  val x_43 = scala.Predef.intWrapper(0);
  val x_44 = x_11.length;
  val x_45 = x_43.until(x_44);
  x_45.foreach[scala.Unit](((i_46: scala.Int) => {
    val x_47 = x_11.apply(i_46);
    val x_48 = x_12.apply(i_46);
    var v_49: scala.Int = x_47.hashCode().%(4096);
    while ({
      val x_50 = v_49;
      val x_51 = x_18.apply(x_50);
      x_51.!=(-1).&&({
        val x_52 = v_49;
        val x_53 = x_18.apply(x_52);
        val x_54 = x_19.apply(x_53);
        x_54.!=(x_47)
      })
    }) 
      {
        val x_55 = v_49;
        v_49 = x_55.+(1).%(4096)
      }
    ;
    val x_56 = v_49;
    val x_57 = x_18.apply(x_56);
    if (x_57.!=(-1))
      {
        var v_58: scala.Int = x_47.hashCode().%(4096);
        while ({
          val x_59 = v_58;
          val x_60 = x_18.apply(x_59);
          x_60.!=(-1).&&({
            val x_61 = v_58;
            val x_62 = x_18.apply(x_61);
            val x_63 = x_19.apply(x_62);
            x_63.!=(x_47)
          })
        }) 
          {
            val x_64 = v_58;
            v_58 = x_64.+(1).%(4096)
          }
        ;
        val x_65 = v_58;
        val x_66 = x_18.apply(x_65);
        val x_67 = x_20.apply(x_66);
        val x_68 = x_21.apply(x_66);
        scala.Predef.println(relation.Row.apply(scala.collection.immutable.List.apply[java.lang.String](x_67, x_68, x_47, x_48), 4))
      }
    else
      ()
  }))
}
