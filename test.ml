#use "main.ml";;

let test0 =
  let e = (Code(Command(Let("a", Int 4),
                  Command(Let("b", Int 9),
                        Command( Let("c", LetIn("i", Int 7, Sum(Var "a", Var "i"))),
                                Command(Let("f", Fun("x", If(Leq(Var "x", Var "c"), Var "a", Var "b"))),
                                      Command(Call(Var "f", Int 1), None ))))))) in  semlist(e, emptyenv) 



let test2 =
  let e = (Code(Command(Let("a", Int 4),
                        Command(Let("b", Int 9),
                                Command(Let("c", LetIn("i", Int 7, Sum(Var "a", Var "i"))),
                                        Command(Let("f", Fun("x", If(Leq(Var "x", Var "c"), Var "a", Var "b"))),
                                         Command(Let("a", Int 0),
                                                Command(Var "f", None)))))))) in
                                                       semlist(e, emptyenv)

let test3 =
  let e = (Code(Command(Let("a", Fun("x", If(Leq(Var "x", Int 4), Var "x", Call(Var "a", Sub(Var "x", Int 1))))),
	        Command(Call(Var "a", Int 8), None )))) in
                                semlist(e, emptyenv)

let test4 =
  let e = (Code(Command(Let("i", LetIn("a", Fun("x", If(Leq(Var "x", Int 4), Var "x", 
        Call(Var "a", Sub(Var "x", Int 1)))),Call(Var "a", Int 8))),
	Command(Var "i", None))))  in semlist(e, emptyenv)


let test5 =
   let e = (Code(Command(Let("x", Tree(Node("root",Int 0,Empty,Empty))),
        Command(Var "x" , None)))) in semlist(e, emptyenv)

let test6 =
   let e = (Code(Command(Let("a", Int 4),
            Command(Let("b", Int 9),
            Command(Let("c", LetIn("i", Int 7, Sum(Var "a", Var "i"))),
            Command(Let("f", Fun("x", If(Leq(Var "x", Var "c"), Var "a", Var "b"))),
            Command(Let("x", Tree(Node("root",Int 12,(Node("root",Int 12,Empty,(Node("root",Int 12,Empty,Empty)))),(Node("root",Int 12,Empty,Empty))))),
         Command(ApplyOver(Var "f", Var "x"), None)))))))) in semlist(e, emptyenv)


let test7 =
   let e = (Code(Command(Let("a", Int 4),
            Command(Let("b", Int 9),
            Command(Let("c", LetIn("i", Int 7, Sum(Var "a", Var "i"))),
            Command(Let("f", Fun("x", If(Leq(Var "x", Var "c"), Var "a", Var "b"))),
            Command(Let("x", Tree(Node("root",Int 12,(Node("root",Int 12,Empty,(Node("root",Int 12,Empty,Empty)))),(Node("root",Int 12,Empty,Empty))))),
         Command(Update(["root";"root"], Var "f", Var "x"),None)))))))) in semlist(e, emptyenv)


let test8 =
   let e = (Code(Command(Let("a", Bool(true)),
            Command(Let("b", Bool(false)),
            Command(Let("c", Int(7)),
            Command(Let("f", Fun("x", If(Leq(Var "x", Var "c"), Var "a", Var "b"))),
            Command(Let("x", Tree(Node("a",Int 8,(Node("b",Int 2,Empty,(Node("c",Int 5,Empty,Empty)))),(Node("b",Int 12,Empty,Empty))))),
         Command(Select(["a";"b";"c"], Var "f", Var "x"),None)))))))) in semlist(e, emptyenv)
