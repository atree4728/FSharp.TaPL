(**
# B(Untyped)

## Syntax

(term)  t ::= true | false | if t then t else t
(value) v ::= true | false

## Evaluation: t -> t'

(E-IFTRUE)  if true then t_2 else t_3 -> t_2
(E-IFFALSE) if false then t_2 else t_3 -> t_3
(E-IF) if t_1 then t_2 else t_3 -> if t'_1 then t_2 else t_3, when t_1 -> t'_1
*)
module B =
    type Term =
        | TmTrue // Constanct True
        | TmFalse // Constant False
        | TmIf of Term * Term * Term // Conditional
        static member pretty(t: Term) =
            match t with
            | TmTrue -> "true"
            | TmFalse -> "false"
            | TmIf (t1, t2, t3) ->
                [ "if"
                  Term.pretty t1
                  "then"
                  Term.pretty t2
                  "else"
                  Term.pretty t3 ]
                |> String.concat " "

    let isValue t =
        match t with
        | (TmTrue
        | TmFalse) -> true
        | _ -> false

    let rec eval' t =
        match t with
        | TmIf (TmTrue, t2, _) -> t2 // E-IFTRUE
        | TmIf (TmFalse, _, t3) -> t3 // E-IFFALSE
        | TmIf (t1, t2, t3) -> TmIf((eval' t1), t2, t3) // E-IF
        | t when isValue t -> t
        | _ -> failwith "failed in eval'."

    let rec eval t = if isValue t then t else eval (eval' t)

open B
let s = TmIf(TmTrue, TmFalse, TmFalse)
let t = TmIf(s, TmTrue, TmTrue)
let u = TmIf(TmFalse, TmTrue, TmTrue)

Seq.map (Term.pretty) [| s; t; u |]
|> printfn "%A"
// seq
//  ["if true then false else false";
//   "if if true then false else false then true else true";
//   "if false then true else true"]

Seq.map (eval >> Term.pretty) [| s; t; u |]
|> printfn "%A"
// seq ["false"; "true"; "true"]
