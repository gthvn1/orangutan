(** Global variable to enable/disable debug info *)
let debug = ref true

type t = {
    input : string
  ; position : int
        (** current position in input (points to the current char) *)
  ; read_position : int
        (** current reading position in input (after current char) *)
  ; ch : char  (** current char under examination *)
}

(** [debug_lexer label lexer] prints the state of the lexer prepending a
    [label]. *)
let debug_lexer (label : string) (lexer : t) : unit =
  if !debug then
    Printf.eprintf "[%s] pos=%d read_pos=%d ch=%C\n" label lexer.position
      lexer.read_position lexer.ch

(** [is_letter ch] returns true if [ch] is a letter. '_' is a valid letter. It
    returns false otherwise. *)
let is_letter ch =
  match ch with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

(** [read_char lexer] returns a new lexer where field "ch" is set with the new
    character. The new lexer has the new position. *)
let read_char (lexer : t) : t =
  (* read char before updating the lexer *)
  let ch =
    if lexer.read_position >= String.length lexer.input then '\000'
    else lexer.input.[lexer.read_position]
  in
  let next_lexer_state =
    {
      lexer with
      position = lexer.read_position
    ; read_position = lexer.read_position + 1
    ; ch
    }
  in
  debug_lexer "read_char" next_lexer_state;
  next_lexer_state

(** [skip_whitespace lexer] returns the lexer after skipping white spaces. *)
let rec skip_whitespace (lexer : t) : t =
  if lexer.ch = ' ' || lexer.ch = '\t' || lexer.ch = '\n' || lexer.ch = '\r'
  then skip_whitespace (read_char lexer)
  else lexer

(** [create input] returns a lexer initialized with string [input]. It returns a
    fully working state. *)
let create (input : string) : t =
  let init_state = { input; position = 0; read_position = 0; ch = '\000' } in
  read_char init_state

(** [read_identifier lexer] returns the indentifer as a string and the new
    lexer. It raises an exception if something goes wrong. *)
let read_identifier (lexer : t) : string * t =
  let position = lexer.position in
  let rec aux l = if is_letter l.ch then aux (read_char l) else l in
  let l = aux lexer in
  (* after aux, l.ch is the first non-letter *)
  assert (l.position > position);
  let identifier = String.sub l.input position (l.position - position) in
  (identifier, l)

(** [next_token lexer] returns a tuple that is the token found and the new
    lexer. It raises an expection if something goes wrong. *)
let next_token (lexer : t) : Token.t * t =
  debug_lexer "next_token (start)" lexer;
  let new_token (ch : char) (tt : Token.token_type) : Token.t =
    { ty = tt; literal = String.make 1 ch }
  in
  let next_lexer = skip_whitespace lexer in
  debug_lexer "next_token (after skip_whitespace)" next_lexer;
  match next_lexer.ch with
  | '=' -> (new_token next_lexer.ch Assign, read_char next_lexer)
  | ';' -> (new_token next_lexer.ch Semicolon, read_char next_lexer)
  | '(' -> (new_token next_lexer.ch Lparen, read_char next_lexer)
  | ')' -> (new_token next_lexer.ch Rparen, read_char next_lexer)
  | ',' -> (new_token next_lexer.ch Comma, read_char next_lexer)
  | '+' -> (new_token next_lexer.ch Plus, read_char next_lexer)
  | '{' -> (new_token next_lexer.ch Lbrace, read_char next_lexer)
  | '}' -> (new_token next_lexer.ch Rbrace, read_char next_lexer)
  | '\000' -> ({ ty = Eof; literal = "" }, next_lexer)
  | c ->
      if is_letter c then
        let ident_str, new_lexer = read_identifier next_lexer in
        let ident_type = Token.lookup_ident ident_str in
        ({ ty = ident_type; literal = ident_str }, new_lexer)
      else (
        Printf.eprintf "Error: char <%C> not recognized\n" c;
        (new_token c Illegal, read_char next_lexer))
