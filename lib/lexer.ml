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

(** [is_whitespace ch] returns true if ch is considered as a whitespace. False
    otherwise. *)
let is_whitespace ch =
  match ch with
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

(** [read_char lexer] returns a new lexer where field "ch" is set with the new
    character. The new lexer has the new position. *)
let read_char (lexer : t) : t =
  (* read char before updating the lexer *)
  let at_eof = lexer.read_position >= String.length lexer.input in
  let ch = if at_eof then '\000' else lexer.input.[lexer.read_position] in
  let next =
    {
      lexer with
      position = lexer.read_position
    ; read_position = lexer.read_position + 1
    ; ch
    }
  in
  debug_lexer "read_char" next;
  next

(** [skip_whitespace lexer] returns the lexer after skipping white spaces. *)
let rec skip_whitespace (lexer : t) : t =
  if is_whitespace lexer.ch then skip_whitespace (read_char lexer) else lexer

(** [create input] returns a lexer initialized with string [input]. It returns a
    fully working state. *)
let create (input : string) : t =
  let init_state = { input; position = 0; read_position = 0; ch = '\000' } in
  read_char init_state

(** [read_identifier lexer] returns the indentifer as a string and the new
    lexer. The new lexer character is the first non-letter. It raises an
    exception if something goes wrong. *)
let read_identifier (lexer : t) : string * t =
  let start = lexer.position in
  let rec consume l = if is_letter l.ch then consume (read_char l) else l in
  let next_lexer = consume lexer in
  assert (next_lexer.position > start);
  let ident_len = next_lexer.position - start in
  (String.sub next_lexer.input start ident_len, next_lexer)

(** [next_token lexer] returns a tuple that is the token found and the new
    lexer. It raises an exception if something goes wrong. *)
let next_token (lexer : t) : Token.t * t =
  debug_lexer "next_token (start)" lexer;
  let simple_token (lexer : t) (tt : Token.token_type) : Token.t * t =
    ({ ty = tt; literal = String.make 1 lexer.ch }, read_char lexer)
  in
  let next_lexer = skip_whitespace lexer in
  debug_lexer "next_token (after skip_whitespace)" next_lexer;
  match next_lexer.ch with
  | '=' -> simple_token next_lexer Assign
  | ';' -> simple_token next_lexer Semicolon
  | '(' -> simple_token next_lexer Lparen
  | ')' -> simple_token next_lexer Rparen
  | ',' -> simple_token next_lexer Comma
  | '+' -> simple_token next_lexer Plus
  | '{' -> simple_token next_lexer Lbrace
  | '}' -> simple_token next_lexer Rbrace
  | '\000' -> ({ ty = Eof; literal = "" }, next_lexer)
  | c ->
      if is_letter c then
        let ident_str, new_lexer = read_identifier next_lexer in
        let ident_type = Token.lookup_ident ident_str in
        ({ ty = ident_type; literal = ident_str }, new_lexer)
      else (
        Printf.eprintf "Error: char <%C> not recognized\n" c;
        simple_token next_lexer Illegal)
