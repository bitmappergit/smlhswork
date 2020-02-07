infixr 0 $
fun f $ x = (f x)
infix 2 o

structure VM = struct
  local open LargeWord in
  datatype inst
    = VInst of (word list -> word list)
    | VWord of word
    | VJmp
    | VJez
    | VJnz
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
  
  fun ssize xs = ((fromInt (length xs))::xs)
  
  fun reduct (a, (VJmp::xs), fin) = reduct $ jmp (a, xs, VJmp::fin)
    | reduct (a, (VJez::xs), fin) = jif (a, xs, fin, (fn x => x = 0w0), VJez)
    | reduct (a, (VJnz::xs), fin) = jif (a, xs, fin, (fn x => not (x = 0w0)), VJnz)
    | reduct (a::cs, (VPut::xs), fin) = (println $ fmt StringCvt.DEC a; reduct (cs, xs, VPut::fin))
    | reduct (a::cs, (VPutRaw::xs), fin) = (TextIO.output1 (TextIO.stdOut, Char.chr o toInt $ a); reduct (cs, xs, VPutRaw::fin))
    | reduct (a, (VWord x)::xs, fin) = reduct (push x a, xs, (VWord x)::fin)
    | reduct (a, (x::xs), fin) = reduct (unwrap x $ a, xs, x::fin)
    | reduct (a, [], fin) = (a, fin)
  and jif (a::b::cs, xs, fin, cmp, typ) =
      if cmp b
      then reduct $ jmp (a::cs, xs, typ::fin)
      else reduct (cs, xs, typ::fin)
    | jif _ = raise Match

  fun vm code = reduct ([], code, [])
  end
end

structure Loader = struct
  open Word8
  open VM
  open BinIO

  infix 3 >> << andb orb xorb notb

  fun joinWord (a,b,c,d,e,f,g,h) = let
    open LargeWord
  in
    (Word8.toLarge h << 0w00) orb
    (Word8.toLarge g << 0w08) orb
    (Word8.toLarge f << 0w16) orb
    (Word8.toLarge e << 0w24) orb
    (Word8.toLarge d << 0w32) orb
    (Word8.toLarge c << 0w40) orb
    (Word8.toLarge b << 0w48) orb
    (Word8.toLarge a << 0w56)
  end

  fun parse (0w00::0w01::xs) fin = parse xs ((VInst equ)::fin)
    | parse (0w00::0w02::xs) fin = parse xs ((VInst dup)::fin)
    | parse (0w00::0w03::xs) fin = parse xs ((VInst dup2)::fin)
    | parse (0w00::0w04::xs) fin = parse xs ((VInst swap)::fin)
    | parse (0w00::0w05::xs) fin = parse xs ((VInst rot)::fin)
    | parse (0w00::0w06::xs) fin = parse xs ((VInst add)::fin)
    | parse (0w00::0w07::xs) fin = parse xs ((VInst sub)::fin)
    | parse (0w00::0w08::xs) fin = parse xs ((VInst store)::fin)
    | parse (0w00::0w09::xs) fin = parse xs ((VInst load)::fin)
    | parse (0w00::0w10::xs) fin = parse xs (VJmp::fin)
    | parse (0w00::0w11::xs) fin = parse xs (VJez::fin)
    | parse (0w00::0w12::xs) fin = parse xs (VPut::fin)
    | parse (0w00::0w13::xs) fin = parse xs (VPutRaw::fin)
    | parse (0w00::0w14::xs) fin = parse xs ((VInst drop)::fin)
    | parse (0w00::0w15::xs) fin = parse xs (VJnz::fin)
    | parse (0w00::0w16::xs) fin = parse xs ((VInst ssize)::fin)
    | parse (0w00::0w00::a::b::c::d::e::f::g::h::xs) fin = parse xs ((VInst (push (joinWord (a,b,c,d,e,f,g,h))))::fin)
    | parse [] fin = List.rev fin
    | parse _ _ = raise Match

  fun toList v = Word8Vector.foldr (op::) [] v

  fun run fin = let
    val infile = BinIO.openIn fin
    val binary = BinIO.inputAll infile
    val code = parse (toList binary) []
  in ignore (vm code) end
    handle Match => print "invalid binary\n"
end

val _ = let
  val [fin] = CommandLine.arguments ()
in Loader.run fin end
  handle Bind => print "invalid arguments, please provide an input file\n"
