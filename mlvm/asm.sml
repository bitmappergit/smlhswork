local open Word8 in
infix 3 >> << andb orb xorb notb

fun splitWord NONE = raise Match
  | splitWord (SOME x) = let
    open LargeWord
in
   map Word8.fromLarge
     [(x >> 0w00) andb 0wxFF
     ,(x >> 0w08) andb 0wxFF
     ,(x >> 0w16) andb 0wxFF
     ,(x >> 0w24) andb 0wxFF
     ,(x >> 0w32) andb 0wxFF
     ,(x >> 0w40) andb 0wxFF
     ,(x >> 0w48) andb 0wxFF
     ,(x >> 0w56) andb 0wxFF
     ]
end


fun parse ("drop"::xs)  fin = parse xs (0w00::0w00::fin)
  | parse ("equ"::xs)   fin = parse xs (0w01::0w00::fin)
  | parse ("dup"::xs)   fin = parse xs (0w02::0w00::fin)
  | parse ("dup2"::xs)  fin = parse xs (0w03::0w00::fin)
  | parse ("swap"::xs)  fin = parse xs (0w04::0w00::fin)
  | parse ("rot"::xs)   fin = parse xs (0w05::0w00::fin)
  | parse ("add"::xs)   fin = parse xs (0w06::0w00::fin)
  | parse ("sub"::xs)   fin = parse xs (0w07::0w00::fin)
  | parse ("store"::xs) fin = parse xs (0w08::0w00::fin)
  | parse ("load"::xs)  fin = parse xs (0w09::0w00::fin)
  | parse ("jmp"::xs)   fin = parse xs (0w10::0w00::fin)
  | parse ("jez"::xs)   fin = parse xs (0w11::0w00::fin)
  | parse ("."::xs)     fin = parse xs (0w12::0w00::fin)
  | parse ("emit"::xs)  fin = parse xs (0w13::0w00::fin)
  | parse (x::xs)       fin = parse xs ((splitWord (StringCvt.scanString (LargeWord.scan StringCvt.DEC) x))@(0w14::0w00::fin))
  | parse []            fin = fin

fun assemble fin fout = let
  val infile = TextIO.openIn fin
  val outfile = BinIO.openOut fout
  val binary = rev (parse (String.tokens Char.isSpace (TextIO.inputAll infile)) [])
  val _ = app (fn x => BinIO.output1 (outfile, x)) binary
in BinIO.flushOut outfile end

val _ = let
  val [fin] = CommandLine.arguments ()
  val fout = (hd (String.tokens Char.isPunct fin)) ^ ".bin"
in assemble fin fout end
  handle Bind => print "invalid arguments, please provide an input file\n"
end
