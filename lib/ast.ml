module Expression = struct
  type t
end

type identifier = { token : Token.t; value : string }

type let_stmt = { token : Token.t; name : identifier; value : Expression.t }
type return_stmt = { token : Token.t; value : Expression.t }
type expr_stmt = { token : Token.t; value : Expression.t }

module Statement = struct
  type t = Let of let_stmt | Return of return_stmt | Expr of expr_stmt

  let token_literal s : string =
    match s with
    | Let _l -> failwith "token literal for let not implemented"
    | Return _r -> failwith "token literal for return not implemented"
    | Expr _e -> failwith "token literal for expression not implemented"
end

module Program = struct
  type t = Statement.t list

  let token_literal _p : string = failwith "token literal not implemented"
end
