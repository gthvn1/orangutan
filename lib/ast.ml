module T = Token

module Expression = struct
  type t = ToDo
end

type identifier = { token : Token.t; value : string }
type let_stmt = { token : Token.t; name : identifier; value : Expression.t }
type return_stmt = { token : Token.t; value : Expression.t }
type expr_stmt = { token : Token.t; value : Expression.t }

module Statement = struct
  type t = Let of let_stmt | Return of return_stmt | Expr of expr_stmt

  let token_literal s : string =
    match s with
    | Let l -> T.to_string l.token
    | Return r -> T.to_string r.token
    | Expr e -> T.to_string e.token
end

module Program = struct
  type t = { stmts : Statement.t list; errors : string list }

  let token_literal _p : string = failwith "token literal not implemented"
end
