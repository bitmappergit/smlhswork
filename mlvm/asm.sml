structure Assembler = struct
  local
    open String
    open Char
  in
  datatype inst
    = VInst of (word list -> word list)
    | VInt of int
    | VJmp
    | VJnz
    | VPut

  fun inter [a,b] lst = (lst@[a^"\n,",b])
    | inter (a::b::xs) lst = inter xs (lst@[a^"\n,",b^"\n,"])
    | inter [x] lst = (lst@[x])
    | inter _ _ = raise Match

  fun parse "drop" = "VInst drop"
    | parse "equ" = "VInst equ"
    | parse "dup" = "VInst dup"
    | parse "dup2" = "VInst dup2"
    | parse "swap" = "VInst swap"
    | parse "rot" = "VInst rot"
    | parse "add" = "VInst add"
    | parse "sub" = "VInst sub"
    | parse "store" = "VInst store"
    | parse "load" = "VInst load"
    | parse "jmp" = "VJmp"
    | parse "jez" = "VJez"
    | parse "show" = "VPut"
    | parse "." = "VPutRaw"
    | parse x = "VWord 0w" ^ x

  fun asm x = "[" ^ String.concat (inter (List.map parse (tokens isSpace x)) []) ^ "\n]\n" 
  end
end

fun main () = let
  val inputfile = TextIO.openIn (hd (CommandLine.arguments ()))
  val outputfile = TextIO.openOut (hd (tl (CommandLine.arguments ())))
  val code = TextIO.input inputfile
  val binary = Assembler.asm code
  val _ = TextIO.output (outputfile, binary)
in TextIO.flushOut outputfile
end handle Match => print "unknown directive\n"

val _ = main ()
