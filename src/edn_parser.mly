%token <int> INT
%token <string> BIG_INT
%token <float> FLOAT
%token <string> STRING
%token <string> SYMBOL
%token <string * string> Q_SYMBOL
%token <string> KEYWORD
%token <string * string> Q_KEYWORD
%token <string> TAG
%token <string * string> Q_TAG
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token SET_START
%token TRUE
%token FALSE
%token NIL
%token EOF
%start <Common.value option> prog
%%
prog:
  | EOF       { None }
  | v = value { Some v }
  ;

value:
  | SET_START; set = forms; RIGHT_BRACE
    { `Set set }
  | LEFT_PAREN; list = forms; RIGHT_PAREN
    { `List list }
  | LEFT_BRACK; vector = forms; RIGHT_BRACK
    { `Vector vector }
  | LEFT_BRACE; map = map_fields; RIGHT_BRACE
    { `Assoc map }
  | s = SYMBOL
    { `Symbol (None, s) }
  | s = Q_SYMBOL
    { `Symbol (Some (fst s), (snd s)) }
  | s = KEYWORD
    { `Keyword (None, s) }
  | s = Q_KEYWORD
    { `Keyword (Some (fst s), (snd s)) }
  | t = TAG; f = value
    { `Tag (None, t, f) }
  | t = Q_TAG; f = value
    { `Tag (Some (fst t), (snd t), f) }
  | s = STRING
    { `String s }
  | i = INT
    { `Int i }
  | i = BIG_INT
    { `BigInt i }
  | x = FLOAT
    { `Float x }
  | TRUE
    { `Bool true }
  | FALSE
    { `Bool false }
  | NIL
    { `Null }
  ;

map_fields: map_fields = rev_map_fields { List.rev map_fields };

rev_map_fields:
  | (* empty *) { [] }
  | map_fields = rev_map_fields; k = value; v = value
    { (k, v) :: map_fields }
  ;

forms: forms = rev_forms { List.rev forms };

rev_forms:
  | (* empty *) { [] }
  | forms = rev_forms; v = value
    { v :: forms }
  ;