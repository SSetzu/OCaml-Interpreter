open List

type ide=string

(* Espressioni *)
type exp = Int of int
	 | Bool of bool
	 | String of string
	 | Var of ide
         | Sum of exp * exp
         | Sub of exp * exp
         | Mul of exp * exp
	 | Div of exp * exp
	 | Neg of exp
         | If of exp * exp * exp
         | Eq of exp * exp
         | Leq of exp * exp
         | And of exp * exp
         | Or of exp * exp
	 | Not of exp
	 | LetIn of ide * exp * exp
	 | Let of ide * exp 
	 | Fun of ide * exp
	 | Call of exp * exp
         | ApplyOver of exp * exp
         | Update of (ide list) * exp * exp
         | Select of (ide list) * exp * exp
         | Tree of tree 
and tree= Empty
         | Node of ide * exp * tree * tree
(* estensione con i comandi, un comando contiene un espressione e un altro comando, l'ultimo comando è None *)
and commands=
| Code of commands
| None
| Command of exp * commands
(* tipi valutati *)
and eval =
  | EInt of int
  | EBool of bool
  | EString of string
  | EFun of exp
  | ETree of etree
  | Unbound
and etree = EEmpty
          | ENode of ide * eval * etree * etree

(* controllo dei tipi *)
let typecheck (x, y) = match x with
  | "int" -> (match y with
	     | EInt(u) -> true
	     | _ -> false)
  | "bool" -> (match y with
	      | EBool(u) -> true
	      | _ -> false)
  | "string" -> (match y with
	      | EString(u) -> true
	      | _ -> false)
  | "tree"  -> (match y with
	      | ETree(u) -> true
	      | _ -> false)
  | _ -> failwith "Not a type"

(* operazioni base *)
let and_ck (x, y) = if typecheck ("bool", x) && typecheck ("bool", y) then
		  (match (x, y) with
                   | (EBool(u), EBool(w)) -> EBool(u && w)
                   | _ -> failwith "Type error")
		else failwith "Type error"

let or_ck (x, y) = if typecheck ("bool", x) && typecheck ("bool", y) then
		  (match (x, y) with
                   | (EBool(u), EBool(w)) -> EBool(u || w)
                   | _ -> failwith "Type error")
		 else failwith "Type error"

let not_ck x = if typecheck ("bool", x) then
	      (match x with EBool(u) -> EBool(not u)|_ -> failwith "Type error")
	      else failwith "Type error"

let sum_ck (x, y) = if typecheck ("int", x) && typecheck ("int", y) ||
		(typecheck ("string", x) && typecheck ("string", y)) then
		    (match (x, y) with
                     | (EInt(u), EInt(w)) -> EInt(u + w)
		     | (EString(u), EString(w)) -> EString(u ^ w)
                     | _ -> failwith "Type error")
		  else failwith "Type error"


let sub_ck(x, y) = if typecheck ("int", x) && typecheck ("int", y) then
		     (match (x, y) with
                      | (EInt(u), EInt(w)) -> EInt(u - w)
                      | _ -> failwith "Type error")
		   else failwith "Type error"

let mul_ck (x, y) = if typecheck ("int", x) && typecheck ("int", y) then
		     (match (x, y) with
                      | (EInt(u), EInt(w)) -> EInt(u * w)
                      | _ -> failwith "Type error")
		   else failwith "Type error"

let div_ck (x, y) = if typecheck ("int", x) && typecheck ("int", y) then
		      (match (x, y) with
		       | (EInt(u), EInt(w)) -> if w != 0 then
						 EInt(u / w)
					       else
						 failwith "Division by zero"
                       | _ -> failwith "Type error")
		    else failwith "Type error"

let neg_ck x = if typecheck ("int", x) then
		 (match x with EInt(u) -> EInt(-u)|_-> failwith "Type errror")
		 else failwith "Type error"


let rec search (k,v) y = match y with
                | (k1,v1)::ys -> if (k=k1 && v=v1) then true else search (k,v) ys
                | [] -> false 

let rec ck x y = match x with
                | (k,v)::xs -> if ((search (k,v) y)=true) then ck xs y else false 
                | [] -> true

let eq_ck (x, y) = if (typecheck ("int", x) && typecheck ("int", y)) ||
		(typecheck ("bool", x) && typecheck ("bool", y)) ||
		(typecheck ("string", x) && typecheck ("string", y)) then
		     (match (x, y) with
		      | (EInt(u), EInt(w)) -> EBool(u = w)
		      | (EBool(u), EBool(w)) -> EBool(u = w)
		      | (EString(u), EString(w)) -> EBool(u = w)
                      | _ -> failwith "Type error")
		   else failwith "Type error"

let leq_ck (x, y) = if typecheck ("int", x) && typecheck ("int", y) ||
		(typecheck ("bool", x) && typecheck ("bool", y)) ||
		(typecheck ("string", x) && typecheck ("string", y))  then
		      (match (x, y) with
		       | (EInt(u), EInt(w)) -> EBool(u <= w)
		       | (EString(u), EString(w)) -> EBool(u <= w)
                       | _ -> failwith "Type error")
		    else failwith "Type error"


(* ambiente implementato come lista *)
module Env =
struct
     type 't env= (string * 't) list
     (* ambiente vuoto *)
     let emptyenv = [("", Unbound)]

     (* applico l'ambiente su una coppia (nome,espressione) *)
     let rec applyenv(x, y) = match x with
	| [("", Unbound)] -> Unbound
	| (i1, e1) :: x1 -> if y = i1 then e1 
                            else applyenv(x1, y)
	| [] -> failwith("wrong env")
     (* aggiungo una coppia (nome, espressione) all'ambiente r *)      
     let bind(r, l, e) = (l, e) :: r
end


open Env


(* Restituisce il primo elemento della coppia (exp, eval env), restituisce un espressione *)
let expret x = match x with
  | (a, b) -> a


(* Resituisce il secondo elemento della coppia (exp, eval env), restituisce un ambiente *)
let envret x = match x with
  | (a, b) -> b


(* valutazione di un albero di espressioni nell'ambiente valutato r *)
let rec treeval ((n : tree),(r : eval env)) = match n with
                                              | Empty -> EEmpty
                                              | Node(ide,e,ls,rs) -> ENode(ide,
                                                                     expret(sem (e,r)),
                                                                     (treeval (ls,r)),
                                                                   (treeval (rs,r)))
(* funzione che porta da un albero valutato a un albero non valutato *)
and etreetotree (n:etree) = match n with
| EEmpty -> Empty
| ENode(a,EInt(b),c,d) -> Node(a,Int(b),(etreetotree c),(etreetotree d))
| ENode(a,EBool(b),c,d) -> Node(a,Bool(b),(etreetotree c),(etreetotree d))
| ENode(a,EString(b),c,d) -> Node(a,String(b),(etreetotree c),(etreetotree d))
| ENode(a,ETree(b),c,d) -> Node(a,Tree(etreetotree b),(etreetotree c),(etreetotree d))
| _ -> failwith "type not correct"

and sem ((e : exp), (r : eval env)) = (match e with
    (* La semantica resituisce una coppia (exp, eval env) per
       permettere di eseguire una sequenza di comandi separati
       sullo stesso ambiente *)
  | Int n -> (EInt(n), r)
  | Bool n -> (EBool(n), r)
  | String n -> (EString(n),r)
  | Tree n -> (ETree(treeval(n,r)),r)
  | Var n -> (applyenv(r, n), r)
  | And(a, b) -> (and_ck((expret(sem(a, r))), (expret(sem(b, r)))), r)
  | Or(a, b) -> (or_ck((expret(sem(a, r))), (expret(sem(b, r)))), r)
  | Not n -> (not_ck((expret(sem(n, r)))), r)
  | Sum(a, b) -> (sum_ck((expret(sem(a, r))), (expret((sem(b, r))))), r)
  | Sub(a, b) -> (sub_ck((expret(sem(a, r))), (expret((sem(b, r))))), r)
  | Mul(a, b) -> (mul_ck((expret(sem(a, r))), (expret((sem(b, r))))), r)
  | Div(a, b) -> (div_ck((expret(sem(a, r))), (expret((sem(b, r))))), r)
  | Neg n -> (neg_ck(expret(sem(n, r))), r)
  | Eq(a, b) -> (eq_ck((expret(sem(a, r))), (expret((sem(b, r))))), r)
  | Leq(a, b) -> (leq_ck((expret(sem(a, r))), (expret((sem(b, r))))), r)
  | If(a, b, c) -> let g = expret(sem(a, r)) in
                   if typecheck("bool", g) then
                     (if g = EBool(true) then sem(b, r) else sem(c, r))
                   else
                     failwith "Nonboolean guard"
  (* valuta l'espressione e2 nell'ambiente r esteso con la coppia (nome,e1) *)
  | LetIn(nome, e1, e2) -> let u = sem(e1, r) in
                       (match u with
                       | (Unbound, _) -> failwith "Invalid right value"
                       | _ -> sem(e2, bind(r, nome, (expret(u)))))
  (* restituisce l'ambiente aggiungendo con la coppia (nome,e) *)
  | Let(nome, e) -> let u = sem(e, r) in
                  (match u with
                  | (Unbound, _) -> failwith "Invalid right value"
                  | _ -> (Unbound, bind(r, nome, (expret(u)))))
  (* risolvo i riferimenti non locali nel corpo della funzione e la aggiungo all'ambiente, 
  restituisco il corpo della funzione compilato *) 
  | Fun(x,e) -> let rec compile(body,env) = match body with
                  (* se il nome della variabile coincide con quello dell'argomento
                   non risolvo il riferimento legato a quel nome *)
		  | Var n -> if n = x then Var n
                 		else 
					let u = (applyenv(env, n)) in
                               		(match u with
                                 	| EInt n -> Int n
                                 	| EBool n -> Bool n
					| EString n -> String n
                                        | ETree n -> Tree(etreetotree n)
					| _ -> failwith "Ref not found")
		   | And(z, w) -> And(compile(z, env), compile(w, env))
                   | Or(z, w) -> Or(compile(z, env), compile(w, env))
                   | Not n -> Not(compile(n, env))
                   | Sum(z, w) -> Sum(compile(z, env), compile(w, env))
                   | Sub(z, w) -> Sub(compile(z, env), compile(w, env))
                   | Mul(z, w) -> Mul(compile(z, env), compile(w, env))
                   | Div(z, w) -> Div(compile(z, env), compile(w, env))
                   | Neg n -> Neg(compile(n, env))
                   | Eq(z, w) -> Eq(compile(z, env), compile(w, env))
                   | Leq(z, w) -> Leq(compile(z, env), compile(w, env))
                   | If(z, w, j) -> If(compile(z, env),
                                       compile(w, env), compile(j, env))
                   | Fun(z, w) -> Fun(z, compile(w, env))
                   | _ -> body 
		in (EFun(Fun(x, compile(e, r))), r)
  (* chiamata di funzione, valuto il corpo della funzione nell'ambiente esteso con i parametri attuali *)
  | Call(nome, p_att) -> let g = sem(nome, r) in
        (match (expret(g)) with
         | EFun(Fun(p_for, body)) -> sem(body, bind(envret g, p_for,(expret(sem(p_att, r)))))
         | _ -> failwith "Type error")
  (* applico la funzione f, valutata nell'ambiente r, all'albero tree, anch'esso valutato nell'ambiente r *)
  | ApplyOver(f,t) -> (let fval=(expret(sem(f,r))) in
                      (match (fval,(expret(sem(t,r)))) with
                      | (EFun(Fun(p_for, body)), ETree(x)) -> let rec applytree (fn,tr) = 
                                                                (match tr with
                                                                | EEmpty -> EEmpty
                                                                | ENode(ide,v,ls,rs) ->
                                                                        ENode(ide,
                                                                        (expret(sem(body, bind(r,p_for,v)))),
                                                                        (applytree(fn,ls)),
                                                                        (applytree(fn,rs))))
                                                              in (ETree(applytree(fval,x)),r)
                      | (_,_) -> failwith "Type error"))

  (* aggiorno i nodi appartenenti al percorso definito da lst, nell'albero t valutato in r con la funzione f, 
  anch'essa valutata nell'ambiente r *)
  | Update(lst,f,t) -> (let fval=(expret(sem(f,r))) in
                      (match (fval,(expret(sem(t,r)))) with
                      | (EFun(Fun(p_for, body)), ETree(x)) -> 
                                let rec applytree (lstone,fn,tr) = 
                                        (match tr with
                                        | EEmpty -> EEmpty
                                        | ENode(ide,v,ls,rs) -> match lstone with
                                                                | el::els -> if el=ide then 
                                                                                ENode(ide,
                                                                                (expret(sem(body, 
                                                                                bind(r,p_for,v)))),
                                                                                (applytree(els,fn,ls)),
                                                                                (applytree(els,fn,rs)))
                                                                               else 
                                                                                ENode(ide,v,ls,rs)
                                                                | [] ->  ENode(ide,v,ls,rs))
                                                                                
                                 in (ETree(applytree(lst,fval,x)),r)
                      | (_,_) -> failwith "Type error"))

  | Select(lst,f,t) -> (let fval=(expret(sem(f,r))) in
                      (match (fval,(expret(sem(t,r)))) with
                      | (EFun(Fun(p_for, body)), ETree(x)) -> 
                                let rec applytree (lstone,fn,tr) =
                                        (match lstone with
                                         | [] -> tr
                                         | el::els -> (match (tr,els) with
                                                      | (EEmpty,_) -> EEmpty
                                                      | (ENode(ide,v,ls,rs),a::b) -> if (ide=el) then
                                                                                      (match  (expret(sem(body, 
                                                                                          bind(r,p_for,v)))) with
                                                                                   | EBool(n) -> if (n) then
                                                                                                ENode(ide,v,ls,rs)
                                                                                                else 
                                                                                                let st=applytree(els,fn,ls) in
                                                                                                (match st with
                                                                                                | EEmpty -> applytree(els,fn,rs)
                                                                                                | nodo -> nodo)
                                                                                   | _ -> failwith "expected boolean function")
                                                                                                                        
                                                                                    else EEmpty
                                
                                                      | (ENode(ide,v,ls,rs),[]) -> if (ide=el) then
                                                                                   (match  (expret(sem(body, 
                                                                                          bind(r,p_for,v)))) with
                                                                                   | EBool(n) -> if (n) then
                                                                                                ENode(ide,v,ls,rs)
                                                                                                else 
                                                                                                let st=applytree(els,fn,ls) in
                                                                                                (match st with
                                                                                                | EEmpty -> applytree(els,fn,rs)
                                                                                                | nodo -> nodo)
                                                                                   | _ -> failwith "expected boolean function")

                                                                                   else EEmpty

                                                      | _ -> failwith "is not valid Tree")
                                        | _ -> failwith "is not a list")
                                in (ETree(applytree(lst,fval,x)),r)
                      | (_,_) -> failwith "Type error"))

 | _ -> failwith "Not exp")
      
                             
let rec semlist ((e : commands), (r : eval env)) = match e with
  (* Scorre la lista dei comandi in modo ricorsivo valutando l'espressione
     nell'ambiente resituito dall'ultima espressione valutata *)
  | None -> failwith "Empty command"
  | Command(x,None) -> expret(sem(x, r))
  | Command(x,xs) -> let v = sem(x, r) in
             semlist(xs, (envret v))
  | Code(x) -> semlist(x,r)	
