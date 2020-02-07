val _ = let
  val out = map (fn x => ((Int.toString o Char.ord) x) ^ "\n") (String.explode (TextIO.inputAll (TextIO.stdIn)))
  val size = length out
  val _ = app print (rev out)
in print ("the size is: " ^ (Int.toString size) ^ "\n") end
