infixr 0 $
fun f $ x = (f x)
infix 2 o

functor VM (VMWord : WORD) = struct
  local open VMWord in
  datatype inst
    = VInst of (word list -> word list)
    | VWord of word
    | VJmp
    | VJez
    | VPut
    | VPutRaw

  val mem = Array.array (256, fromInt 0)

  fun println x = print $ x ^ "\n"
  fun tuppend x (a,b) = (x,a,b)
  fun tupswap (a,b) = (b,a)

  local open Int in
  fun splitAt (l, i) = loop (i, [], l)
  and loop (0, ac, l) = (rev ac, l)
    | loop (_, _, []) = raise Match
    | loop (i, ac, x::l) = loop (i - 1, x::ac, l)
  end

  fun jmp ([], _, _) = raise Match
    | jmp (x::xs, y, z) =
      let val full = List.revAppend (z, y)
      in tuppend xs $ tupswap $ splitAt (full, toInt x)
      end

  fun unwrap (VInst x) = x
    | unwrap _ = raise Match

  fun pop (x::xs) = (x, xs)
    | pop _ = raise Match

  fun drop (x::xs) = xs
    | drop _ = raise Match

  fun push x xs = (x::xs)

  fun equ (a::b::xs) =
      if compare (a, b) = EQUAL
      then (push $ fromInt 1) xs
      else (push $ fromInt 0) xs
    | equ _ = raise Match

  fun dup (x::xs) = (x::x::xs)
    | dup _ = raise Match

  fun dup2 (a::b::xs) = (a::b::a::b::xs)
    | dup2 _ = raise Match

  fun swap (a::b::xs) = (b::a::xs)
    | swap _ = raise Match

  fun rot (a::b::c::xs) = (c::a::b::xs)
    | rot _ = raise Match

  fun add (a::b::xs) = ((b + a)::xs)
    | add _ = raise Match

  fun sub (a::b::xs) = ((b - a)::xs)
    | sub _ = raise Match

  fun store (a::b::xs) = (Array.update (mem, toInt b, a); xs)
    | store _ = raise Match

  fun load (x::xs) = ((Array.sub (mem, toInt x))::xs)
    | load _ = raise Match
  
  fun reduct (a, (VJmp::xs), fin) = reduct $ jmp (a, xs, VJmp::fin)
    | reduct (a, (VJez::xs), fin) = jez (a, xs, fin)
    | reduct (a::cs, (VPut::xs), fin) = (println $ fmt StringCvt.DEC a; reduct (cs, xs, VPut::fin))
    | reduct (a::cs, (VPutRaw::xs), fin) = (print o Char.toString o Char.chr o toInt $ a; reduct (cs, xs, VPutRaw::fin))
    | reduct (a, (VWord x)::xs, fin) = reduct (push x a, xs, (VWord x)::fin)
    | reduct (a, (x::xs), fin) = reduct (unwrap x $ a, xs, x::fin)
    | reduct (a, [], fin) = (a, fin)
  and jez (a::b::cs, xs, fin) =
      if compare (b, fromInt 0) = EQUAL
      then reduct $ jmp (a::cs, xs, VJez::fin)
      else reduct (cs, xs, VJez::fin)
    | jez _ = raise Match

  fun vm code = reduct ([], code, [])
  end
end

structure VM8 = VM (Word8)
structure VM32 = VM (Word32)
structure VM64 = VM (LargeWord)

open VM64

val insts =
  [VWord 0w1
  ,VWord 0w1
  ,VInst dup2
  ,VInst add
  ,VInst dup
  ,VInst dup
  ,VPut
  ,VWord 0w190392490709135
  ,VInst equ
  ,VWord 0w2
  ,VJez]

val ubertest =
  [VWord 0w0 (*  -- a *)
  ,VWord 0w1 (*  -- a *)
  ,VInst add (* a b -- c *)
  ,VInst dup (* a -- a a *)
  ,VPut (* a --  *)
  ,VWord 0w1 (*  -- a *)
  ,VJmp (* a --  *)
  ]

val _ = vm insts
val _ = vm ubertest
