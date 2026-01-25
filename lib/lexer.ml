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

(** [is_digit ch] returns true if ch is a digit, false otherwise. *)
let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

(** [is_letter ch] returns true if [ch] is a letter. '_' is a valid letter. It
    returns false otherwise. *)
let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

(** [is_whitespace ch] returns true if ch is considered as a whitespace. False
    otherwise. *)
let is_whitespace = function
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

(** [read_while pred lexer] reads characters while [pred] holds. It returns the
    consumed substring and the new lexer. The new lexer character is the first
    one for which [pred] is false. It can raise an exception if something wrong
    happens. *)
let read_while (pred : char -> bool) (lexer : t) : string * t =
  let start = lexer.position in
  let rec consume l = if pred l.ch then consume (read_char l) else l in
  let next_lexer = consume lexer in
  let len = next_lexer.position - start in
  assert (len > 0);
  (String.sub next_lexer.input start len, next_lexer)

(** [read_identifier lexer] returns the indentifer as a string and the new
    lexer. The new lexer character is the first non-letter. It raises an
    exception if something goes wrong. *)
let read_identifier (lexer : t) : string * t = read_while is_letter lexer

(** [read_number lexer] returns the number as a string and the new lexer. It
    reads integer. The new lexer character is the first non digit letter. It
    raises an exception if something goes wrong *)
let read_number (lexer : t) : string * t = read_while is_digit lexer

(** [next_token lexer] returns a tuple that is the token found and the new
    lexer. It raises an exception if something goes wrong. *)
let next_token (lexer : t) : Token.t * t =
  debug_lexer "next_token (start)" lexer;

  let single_char_token (lexer : t) (tt : Token.token_type) : Token.t * t =
    ({ ty = tt; literal = String.make 1 lexer.ch }, read_char lexer)
  in

  let lexer = skip_whitespace lexer in
  debug_lexer "next_token (after skip_whitespace)" lexer;

  match lexer.ch with
  | '=' -> single_char_token lexer Assign
  | ';' -> single_char_token lexer Semicolon
  | '(' -> single_char_token lexer Lparen
  | ')' -> single_char_token lexer Rparen
  | ',' -> single_char_token lexer Comma
  | '+' -> single_char_token lexer Plus
  | '-' -> single_char_token lexer Minus
  | '!' -> single_char_token lexer Bang
  | '/' -> single_char_token lexer Slash
  | '*' -> single_char_token lexer Asterisk
  | '<' -> single_char_token lexer Lt
  | '>' -> single_char_token lexer Gt
  | '{' -> single_char_token lexer Lbrace
  | '}' -> single_char_token lexer Rbrace
  | '\000' -> ({ ty = Eof; literal = "" }, lexer)
  | c ->
      if is_letter c then
        let ident_str, lexer' = read_identifier lexer in
        let ident_type = Token.lookup_ident ident_str in
        ({ ty = ident_type; literal = ident_str }, lexer')
      else if is_digit c then
        let literal, lexer' = read_number lexer in
        ({ ty = Int; literal }, lexer')
      else (
        Printf.eprintf "Error: char <%C> not recognized\n" c;
        single_char_token lexer Illegal)
