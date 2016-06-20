{
open Lexing
open Edn_parser

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}
let digit = ['0'-'9']
let int = (['-' '+']? (digit as v)) | (['-']? ['1'-'9'] digit* as v) | (['+']? (['1'-'9'] digit* as v))

let big_int = int 'N'

let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = int frac? exp?

(* part 3 *)
let white = [' ' '\t' ',']+
let discard = "#_"
let newline = '\r' | '\n' | "\r\n"
let first_symbol = ['a'-'z' 'A'-'Z' '*' '!' '_' '?' '$' '%' '&' '=' '<' '>']
let non_numeric = first_symbol | [':' '#' '-' '+' '.']
let symbol = (first_symbol (non_numeric | ['0'-'9'])*) | (['-' '+' '.'] (non_numeric (non_numeric | ['0'-'9'])*)?)
let q_symbol = (symbol as prefix) '/' (symbol as v)

let keyword = ':' (symbol as v)
let q_keyword = ':' (symbol as prefix) '/' (symbol as v)

let tag_first_symbol = first_symbol # '_'
let tag_non_numeric = tag_first_symbol | [':' '#' '-' '+' '.']
let tag_body = (tag_first_symbol (tag_non_numeric | [ '0'-'9'])*) | (['-' '+' '.'] tag_non_numeric (tag_non_numeric | [ '0'-'9'])*)
let tag = '#' (tag_body as v)
let q_tag = '#' (tag_body as prefix) '/' (symbol as v)

rule read =
  parse
  | white    { read lexbuf }
  | discard  { read lexbuf |> ignore; read lexbuf }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "nil"    { NIL }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string v) }
  | big_int  { BIG_INT v }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | symbol   { SYMBOL (Lexing.lexeme lexbuf) }
  | q_symbol { Q_SYMBOL (prefix, v) }
  | keyword  { KEYWORD (v) }
  | q_keyword{ Q_KEYWORD (prefix, v) }
  | tag      { TAG (v) }
  | q_tag    { Q_TAG (prefix, v) }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | "#{"     { SET_START }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | _ { raise (Common.Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Common.Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Common.Error ("String is not terminated")) }
