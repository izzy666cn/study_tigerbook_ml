type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))


fun max(a, b) = if a > b then a else b

fun max_list [] = 0
  | max_list (x::xs) = foldl max x xs

fun maxargs (CompoundStm (s1, s2)) = max(maxargs(s1), maxargs(s2))
  | maxargs (AssignStm (_, e)) = maxargs_exp(e)
  | maxargs (PrintStm l) = max(max_list(map maxargs_exp l), List.length l)
  and maxargs_exp (IdExp id) = 0
    | maxargs_exp (NumExp i) = 0
    | maxargs_exp (OpExp (e1, b, e2)) = max(maxargs_exp(e1), maxargs_exp(e2))
    | maxargs_exp (EseqExp (s, e)) = max(maxargs(s), maxargs_exp(e))
  
val r = maxargs(prog)

type table = (id * int) list
fun update(t, k, v) = (k, v) :: t
fun lookup([], id) = raise Fail ("Unknown Id: " ^ id)
  | lookup((k, v) :: t, id) = if k = id then v else lookup(t, id)

fun binop2fn Plus = op +
  | binop2fn Minus = op -
  | binop2fn Times = op *
  | binop2fn Div = op div

fun interp_stm (CompoundStm (s1, s2), env) = interp_stm(s2, interp_stm(s1, env))
  | interp_stm (AssignStm(id, exp), env) = let val (v, e) = interp_exp(exp, env) 
                                           in
                                                update(e, id, v)
                                           end
  | interp_stm (PrintStm l, env) = foldl (fn (x, e) => 
                                                let val (vv, ee) = interp_exp(x, e) 
                                                    val _ = print(Int.toString vv ^ " ")
                                                in
                                                    ee
                                                end) env l
  and interp_exp (IdExp id, env) = (lookup(env, id), env)
    | interp_exp (NumExp i, env) = (i, env)
    | interp_exp (OpExp(e1, b, e2), env) = let val (e1_v, e1_e) = interp_exp(e1, env)
                                               val (e2_v, e2_e) = interp_exp(e2, e1_e)
                                           in
                                               (binop2fn(b)(e1_v, e2_v), e2_e)
                                           end
    | interp_exp (EseqExp (s, exp), env) = interp_exp(exp, interp_stm(s, env))

fun interp prog = interp_stm(prog, nil)

val a = interp(prog)