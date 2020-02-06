open TextIO
open CommandLine
open Int
open List

datatype direction = Up | Right | Down | Left

infix  3 \> fun f \> y = f y
infixr 3 </ fun x </ f = f x

fun parseWire stream = let
  val str = inputAll stream
  val wireList = String.tokens Char.isSpace str
  val instList = map (String.tokens Char.isPunct) wireList
  val _ = closeIn stream
in instList end

fun readInst inst =
  case (String.sub (inst, 0), String.extract (inst, 1, NONE)) of
    (#"U", y) => (Up,    valOf \> fromString y)
  | (#"R", x) => (Right, valOf \> fromString x)
  | (#"D", y) => (Down,  valOf \> fromString y)
  | (#"L", x) => (Left,  valOf \> fromString x)
  | _ => raise Match

fun genInsts prog =
  map (map readInst) prog

fun execInst (Up,    y) = (fn (a,b) => (a, b + y))
  | execInst (Right, x) = (fn (a,b) => (a + x, b))
  | execInst (Down,  y) = (fn (a,b) => (a, b - y))
  | execInst (Left,  x) = (fn (a,b) => (a - x, b))

fun applyFuns funList = map (fn f => f (0,0)) funList

fun sumCoord ((a1,b1),(a2,b2)) = (a1+a2,b1+b2)

datatype orientation = V | H

fun orientation ((x1, y1), (x2, y2)) =
  case (x1 = x2, y1 = y2) of
    (true, false) => V
  | (false, true) => H
  | _ => raise Match

fun doesIntersect (la,sa) (lb,sb) = let
  fun cmpPnt ((x1,y1),(x2,y2)) = (compare (x1,x2), compare (y1,y2))

  fun canIntersect a b =
    case (orientation a, orientation b) of
      (V,H) => SOME (V,H)
    | (H,V) => SOME (H,V)
    | (_,_) => NONE

  fun compareLines (((ax,ay),(bx,by)),((cx,cy),(dx,dy)),sa,sb) = let
    val a = (ax,ay)
    val b = (bx,by)
    val c = (cx,cy)
    val d = (dx,dy)
  in
    case canIntersect (a,b) (c,d) of
      SOME (V,H) => SOME (cmpPnt (a,d), cmpPnt (c,b), (ax,cy))
    | SOME (H,V) => SOME (cmpPnt (c,b), cmpPnt (a,d), (cx,ay))
    | NONE       => NONE
    | _          => raise Match
  end

  fun doIntersect a b =
    case compareLines (a,b,sa,sb) of
      SOME ((GREATER,GREATER),(GREATER,GREATER),p) => (true,sa,sb,p,la,lb)
    | SOME ((LESS   ,   LESS),(LESS   ,   LESS),p) => (true,sa,sb,p,la,lb)
    | SOME ((LESS   ,GREATER),(LESS   ,GREATER),p) => (true,sa,sb,p,la,lb)
    | SOME ((GREATER,   LESS),(GREATER,   LESS),p) => (true,sa,sb,p,la,lb)
    | SOME ((_      ,      _),(_      ,      _),_) => (false,sa,sb,(0,0),la,lb)
    | NONE                                         => (false,sa,sb,(0,0),la,lb)
in doIntersect la lb end

fun scan f (a, (x::xs), b) = scan f (f (a, x), xs, b @ [f (a, x)])
  | scan f (_, [     ], b) = b

val tmpWires = parseWire \> openIn "day3.txt"
val resWires = genInsts tmpWires;
val fnlistA  = map execInst \> hd resWires
val fnlistB  = map execInst \> hd (tl resWires)
val resListA = applyFuns fnlistA
val resListB = applyFuns fnlistB
val foldedA = scan sumCoord ((0,0), resListA, nil)
val foldedB = scan sumCoord ((0,0), resListB, nil)

fun pointsToLines x = ListPair.zip (take (x, (length x) - 1), tl x)

val stepsA = ListPair.zip (pointsToLines \> (0,0)::foldedA, scan (op +) (0,(map (fn (x,y) => y) \> hd resWires),nil))
val stepsB = ListPair.zip (pointsToLines \> (0,0)::foldedB, scan (op +) (0,(map (fn (x,y) => y) \> last resWires),nil))

fun stripBools fin nil = fin
  | stripBools nil ((_,x1,x2,x3,x4,x5)::xs) = stripBools ((x1, x2, x3, x4, x5)::nil) xs
  | stripBools tmp ((_,x1,x2,x3,x4,x5)::xs) = stripBools ((x1, x2, x3, x4, x5)::tmp) xs

fun printersection (sa,sb,(a,b),((ax1,ay1),(ax2,ay2)),((bx1,by1),(bx2,by2))) =
  app print [
    "(", toString ax1, ",", toString ay1, ")",
    " -> ",
    "(", toString ax2, ",", toString ay2, ")",
    " intersects with ",
    "(", toString bx1, ",", toString by1, ")",
    " -> ",
    "(", toString bx2, ",", toString by2, ")",
    " at point ",
    "(", toString a,   ",", toString b,   ")",
    " after ",
    toString \> sa+sb, " steps",
    "\n"
  ]

fun calcIntersect a b = let
  (*val linesA = pointsToLines \> (0,0)::a
  val linesB = pointsToLines \> (0,0)::b*)
  val intersections = map (fn x => map (doesIntersect x) stepsB) stepsA
  val trueIntersections = map (fn a => filter (fn x => #1 x) a) intersections
  val strippedIntersections = map (stripBools []) trueIntersections
in strippedIntersections end

fun comparePoint ((a1,a2),(b1,b2)) =
  compare (abs a1 + abs a2, abs b1 + abs b2)

fun sort x y = let
  fun merge cmp ([], ys) = ys
    | merge cmp (xs, []) = xs
    | merge cmp (xs as x::xs', ys as y::ys') =
        case cmp (x, y) of GREATER => y :: merge cmp (xs, ys')
                         | _       => x :: merge cmp (xs', ys)

  fun merge_sort cmp [] = []
    | merge_sort cmp [x] = [x]
    | merge_sort cmp xs = let
        val ys = List.take (xs, length xs div 2)
        val zs = List.drop (xs, length xs div 2)
      in
        merge cmp (merge_sort cmp ys, merge_sort cmp zs)
      end
in merge_sort x y end

fun getResultP1 x = let
  fun getPoint (_,_,a,_,_) = a
  val points = concat (map (map getPoint) \> filter (not o null) x)
in
  hd \> sort comparePoint points
end

fun getResultP2 x = let
  fun getPoint (a,b,_,_,_) = (a+b)
  val points = concat (map (map getPoint) \> filter (not o null) x)
in
  hd \> sort compare points
end

val _ = OS.Process.exit let
  val res = calcIntersect foldedA foldedB
  val _ = app (app printersection) res
  val _ = print
      ("The part 1 result is: "
      ^ (toString \> (abs o op +) (getResultP1 res))
      ^ "\n")
  val _ = print
      ("The part 2 result is: "
      ^ (toString (getResultP2 res))
      ^ "\n")
in OS.Process.success end

