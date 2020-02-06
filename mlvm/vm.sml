functor VM (VMWord : WORD) = struct
  local open VMWord in
  datatype inst
    = VInst of (word list -> word list)
    | VInt of int
    | VWord of word
    | VJmp
    | VJez
    | VPut

  val &< = fromInt
  val &> = toInt

  val mem = Array.array (256, &<0)

  fun println x = print (x ^ "\n")

  fun jmp [] _ _ = raise Match
    | jmp (x::xs) y z =
      let val full = List.revAppend (z, y)
      in (xs, List.drop (full, toInt (x - &<1)), List.take (full, toInt (x - &<1)))
      end

  fun unwrap (VInst x) = x
    | unwrap _ = raise Match

  fun pop (x::xs) = (x, xs)
    | pop _ = raise Match

  fun drop (x::xs) = xs
    | drop _ = raise Match

  fun push x xs = (x::xs)

  fun equ (a::b::xs) = if (compare (a, b) = EQUAL)
                       then (push (&<1) xs)
                       else (push (&<0) xs)
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

  fun store (a::b::xs) = (Array.update (mem, &>b, a); xs)
    | store _ = raise Match

  fun load (x::xs) = ((Array.sub (mem, &>x))::xs)
    | load _ = raise Match

  fun reduct (a::cs, (VJmp::xs), fin) = reduct (jmp cs xs (VJmp::fin))
    | reduct (a::b::cs, (VJez::xs), fin) = if (compare (b, &<0) = EQUAL)
                                           then reduct (jmp (a::cs) xs (VJez::fin))
                                           else reduct (cs, xs, (VJez::fin))
    | reduct (a::cs, (VPut::xs), fin) = (println (fmt StringCvt.DEC a); reduct (cs, xs, VPut::fin))
    | reduct (a::cs, (VPutRaw::xs), fin) = (print (Char.chr a); reduct (cs, xs, VPutRaw::fin))
    | reduct (a, ((VInt x)::xs), fin) = reduct (push (&<x) a, xs, ((VInt x)::fin))
    | reduct (a, ((VWord x)::xs), fin) = reduct (push x a, xs, ((VWord x)::fin))
    | reduct (a, (x::xs), fin) = reduct ((unwrap x) a, xs, (x::fin))
    | reduct (a, [], fin) = (a, fin)

  fun vm code = reduct ([], code, [])
  end
end

structure VM8 = VM (Word8)
structure VM32 = VM (Word32)
structure VM64 = VM (LargeWord)

open VM64
val insts =
[VWord 0w1,VWord 0w1,VInst dup2,VInst add,VPut,VInst dup,VWord 0w190392490709135,VInst equ,VInt 3,VJez]

val _ = vm insts
