open TextIO; open CommandLine; open Array2

infixr <\ fun f <\ x = f (x)

fun readInst filename = let
  val fileStream = openIn filename
  val contents = map (valOf o Int.fromString) <\ String.tokens Char.isPunct <\ inputAll fileStream
  val _ = closeIn fileStream
  fun splitTable (x,y) = List.nth (contents, (x*4)+y)
  val instTable = tabulate RowMajor ((length contents) div 4,4,splitTable)
in instTable end

fun updateVal (func,arr,a,b,r) =
  update (arr, r div 4, r mod 4,
          func (sub (arr, a div 4, a mod 4),
                sub (arr, b div 4, b mod 4)))

fun runInst arr #[1,a,b,r] = SOME(updateVal (op +,arr,a,b,r))
  | runInst arr #[2,a,b,r] = SOME(updateVal (op *,arr,a,b,r))
  | runInst arr #[_,_,_,_] = NONE

fun update1D (arr,pos,new) = update (arr, pos div 4, pos mod 4, new)

fun main a b = let
  val insts = readInst <\ hd <\ arguments ()
  val _ = (update1D (insts,1, a); update1D (insts,2, b))
  fun tmp (rowN, SOME(prev)) = tmp (rowN+1, runInst insts (row (insts, rowN)))
    | tmp (_, NONE) = NONE
  val _ = tmp (0, SOME(()))
in Vector.sub (row (insts, 0), 0) end

fun loop () = let
  fun tmp (a,b) =
    if (main a b) = 19690720 then (100*a+b)
    else if b = 99 then tmp (a+1,0)
         else tmp (a,b+1)
in tmp (0,0) end

val _ = OS.Process.exit let
  val _ = print <\ "part A: " ^ (Int.toString <\ main 12 2) ^ "\n"
  val _ = print <\ "part B: " ^ (Int.toString <\ loop ()) ^ "\n"
in OS.Process.success end
