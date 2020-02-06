open TextIO; open LargeInt; open CommandLine

infixr << ; fun f << x = f (x)

fun getInt x = let 
  fun tmp (SOME a) = fromString a
    | tmp  NONE    = NONE
in tmp << inputLine x end

fun cf x = x div 3 - 2

fun rcf x = let
  fun tmp (sum, y) = let
    val res = cf y
  in if res <= 0 then sum else tmp (sum + res, res) end
in tmp (0, x) end

fun outputFuel s = let
  fun readFuel (SOME(x), sum, recSum) = readFuel (getInt s, cf x + sum, rcf x + recSum)
    | readFuel (NONE,    sum, recSum) = [sum, recSum]
in readFuel (getInt s, 0, 0) end

val _ = let
  val openFile = openIn << hd << arguments ()
  val result = outputFuel << openFile
in
  print << concat
    ["part 1 result was: ", toString <<       hd result, "\n"
    ,"part 2 result was: ", toString << hd << tl result, "\n"]
end
