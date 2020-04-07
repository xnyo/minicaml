type ide = string;;
type exp = 
  | Eint of int
  | Ebool of bool
  | Estring of string
  
  | Den of ide
  | Prod of exp * exp
  | Sum of exp * exp
  | Diff of exp * exp
  | Eq of exp * exp
  | Minus of exp
  | IsZero of exp
  | Or of exp * exp
  | And of exp * exp
  | Not of exp
  | Ifthenelse of exp * exp * exp
  | Let of ide * exp * exp
  | Fun of ide * exp
  | FunCall of exp * exp
  | FunCallVal of exp * evT
  | Letrec of ide * exp * exp

  (* Dizionario.
  (exp (chiave) * exp (valore)) list *)
  | Dict of (exp * exp) list    
  
  (* Sceglie un valore memorizzato in un dizionario a partire dalla chiave 
     exp (dizionario) * exp (chiave) *)
  | DictSelect of exp * exp

  (* Inserisce/sostituisce un valore in un dizionario.
  exp (dizionario) * exp (chiave) * exp (valore) *)
  | DictInsert of exp * exp * exp

  (* Elimina un valore da un dizionario a partire dalla chiave.
  exp (dizionario) * exp (chiave) *)
  | DictDelete of exp * exp

  (* Determina se una chiave è presente in un dizionario.
  exp (dizionario) * exp (chiave) *)
  | DictHasKey of exp * exp

  (* Restituisce un nuovo dizionario senza le chiavi fornite.
  exp list (chiavi da rimuovere) * exp (dizionario) *)
  | DictFilter of exp list * exp

  (* Applica una funzione a tutti gli elementi di un dizionario e restituisce il nuovo dizionario.
  exp (funzione) * exp (dizionario) *)
  | DictIterate of exp * exp

  (* Applica una funzione (f) a tutti gli elementi di un dizionario e
  restituisce la somma di tutti i valori ritornati da f.
  f deve essere int -> int.
  Questa operazione funziona solo su dizionari che contengono solamente valori interi.
  exp (funzione) * exp (dizionario) *)
  | DictFold of exp * exp
and 't env = ide -> 't
and evT =
  | Int of int
  | Bool of bool
  | String of string
  | Unbound
  | FunVal of evFun
  | RecFunVal of ide * evFun

  (* Valore memorizzato in un dizionario.
  Qualsiasi valore esprimibile (evT) può essere usato come chiave e come valore
  evT (chiave) * evT (valore) *)
  | DictItem of evT * evT

  (* Dizionario.
  (evT (chiave) * evT (valore)) list *)
  | DictVal of (evT * evT) list
and evFun = ide * exp * evT env;;

(*ambiente polimorfo*)
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool =
  match s with
    | "int" -> (match v with
      | Int(_) -> true
      | _ -> false)
    | "bool" -> (match v with
      | Bool(_) -> true
      | _ -> false)
    | "dict" -> (match v with
      | DictVal(_) -> true
      | _ -> false)
    | _ -> failwith("not a valid type")
;;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
  then (
    match (x,y) with
    (Int(n),Int(u)) -> Int(n*u)
  ) else failwith("Type error")
;;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
  then (
    match (x,y) with
    (Int(n),Int(u)) -> Int(n+u)
  ) else failwith("Type error")
;;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
  then (
    match (x,y) with
    (Int(n),Int(u)) -> Int(n-u)
  ) else failwith("Type error")
;;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
  then (
    match (x,y) with
    (Int(n),Int(u)) -> Bool(n=u)
  ) else failwith("Type error")
;;

let minus x = if (typecheck "int" x) 
  then (
    match x with
    Int(n) -> Int(-n)
  ) else failwith("Type error")
;;

let iszero x = if (typecheck "int" x)
  then (
    match x with
    Int(n) -> Bool(n=0)
  ) else failwith("Type error")
;;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (
    match (x,y) with
    (Bool(b),Bool(e)) -> (Bool(b||e))
  ) else failwith("Type error")
;;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (
    match (x,y) with
    (Bool(b),Bool(e)) -> Bool(b&&e)
  ) else failwith("Type error")
;;

let non x = if (typecheck "bool" x)
  then (
    match x with
    | Bool(true) -> Bool(false)
    | Bool(false) -> Bool(true)
   ) else failwith("Type error")
;;

(*interprete*)

(* funzione che prende un dizionario (lista coppie chiave * valore) e
   restituisce il valore associato alla chiave k fornita come parametro.
   Restituisce Unbound se la nel dizionario non c'è un elemento di chiave k *)
let rec dictResolve (d : (evT * evT) list) (k : evT) : evT =
  match d with
    | [] -> Unbound
    | (k', v')::tl -> if k = k' then v' else dictResolve tl k
;;

(* Ritorna true se in d esiste un elemento di chiave k, false altrimenti *)
let dictHasKey (d : (evT * evT) list) (k : evT) : bool = 
  match dictResolve d k with
    | Unbound -> false
    | _ -> true
;;

(* Inserisce una nuova coppia chiave * valore all'interno di un dizionario *)
let rec dictInsert (d : (evT * evT) list) (k : evT) (v : evT) : (evT * evT) list =
  match d with
    | [] -> [(k, v)]
    | hd::tl -> let (k', v') = hd in if k' = k then (k, v)::tl else hd::dictInsert tl k v
;;

(* Restituisce un dizionario d' a partire da un dizionario d fornito come parametro
in cui vengono rimosse tutte le coppie chiave * valore la cui chiave = k, passato come parametro *)
let rec dictFilter (d : (evT * evT) list) (k : evT) : (evT * evT) list =
  match d with
    | [] -> []
    | hd::tl -> let (k', v') = hd in if k' = k then tl else hd::dictFilter tl k
;;

(* ritorna true se needle è nella lista haystack, false altrimenti *)
let rec listContains needle haystack =
  match haystack with
    | [] -> false
    | hd::tl -> if hd = needle then true else listContains needle tl
;;

(* Come dictFilter, ma usa una lista di chiavi invece di una singola chiave *)
let rec dictFilterKeys (d: (evT * evT) list) (fl : evT list) : (evT * evT) list =
  match d with
    | [] -> []
    | hd::tl -> let (k, v) = hd in if listContains k fl then dictFilterKeys tl fl else hd::dictFilterKeys tl fl
;;

let rec eval (e : exp) (r : evT env) : evT = 
  (* evalDict valuta una lista di coppie (exp * exp)
  e restituisce una lista di coppie (evT * evT)),
  che sono le coppie chiave-valore di un dizionario. *)
  let rec evalDict (l : (exp * exp) list) (r : evT env) : (evT * evT) list = match l with
    | [] -> []
    | (k, v)::tl -> (eval k r, eval v r)::evalDict tl r

  (* evalList valuta una lista di exp e restituisce una lista di evT *)
  in let rec evalList (l : exp list) (r : evT env) : evT list = match l with
    | [] -> []
    | hd::tl -> (eval hd r)::evalList tl r

  (* dictFold applica una funzione f (int -> int) a ogni elemento di un dizionario (che deve avere solo valori interi),
  e restituisce la somma dei valori prodotti dalle singole chiamate delle f *)
  in let rec dictFold (l: (evT * evT) list) (f) (r : evT env) : evT = match l with
    | [] -> Int 0
    | (k, v)::tl -> (
      let current, rest = eval (FunCallVal(f, v)) r, dictFold tl f r in
        match current, rest with
          | Int x, Int r -> Int (x + r)
          | _ -> failwith("the fold function must return an integer")
    )
  in
  match e with

  (* Dizionario. Valuta tutte le coppie chiave valore. *)
  | Dict l -> let v = evalDict l r in DictVal(v)

  (* Selezione elemento da dizionario per chiave, con typecheck dinamico. *)
  | DictSelect(d, k) ->
    let d', k' = eval d r, eval k r in
      (
        match d' with
        | DictVal(l) -> dictResolve l k'
        | _ -> failwith("non-dict used for DictSelect")
      )

  (* Restituisce true se la chiave k è nel dizionario d, false altrimento. Con typecheck dinamico. *)
  | DictHasKey(d, k) ->
    let d', k' = eval d r, eval k r in
      (
        match d' with
        | DictVal(l) -> if (dictHasKey l k') then Bool(true) else Bool(false)
        | _ -> failwith("non-dict used for DictHasKey")
      )

  (* Inserisce una coppia chiave-valore
  (o aggiorna il valore associato a una chiave, se la chiave esiste già), con typecheck dinamico. *)
  | DictInsert(d, k, v) ->
    let d', k', v' = eval d r, eval k r, eval v r in
      (
        match d' with
          | DictVal(l) -> DictVal(dictInsert l k' v')
          | _ -> failwith("non-dict used for DictInsert")
      )

  (* Restituisce un nuovo dizionario senza la chiave k, a partire dal dizionario d. Typecheck dinamico. *)
  | DictDelete(d, k) ->
    let d', k' = eval d r, eval k r in
      (
        match d' with
          | DictVal(l) -> DictVal(dictFilter l k')
          | _ -> failwith("non-dict used for DictDelete")
      )

  (* Restituisce un nuovo dizionario senza le chiavi in filterList, a partire dal dizionario d. Typechecking dinamico. *)
  | DictFilter(filterList, d) -> (
    match eval d r with
      | DictVal(l) -> DictVal(dictFilterKeys l (evalList filterList r))
      | _ -> failwith("non-dict used for DictFilter")
    )

  (* Valuta l'operazione map.
  Usa FunCallVal (come FunCall, ma il parametro è già valutato)
  Typechecking dinamico. *)
  | DictIterate(f, d) -> (
    match eval d r with
      | DictVal(l) -> DictVal(List.map (fun (k, v) -> (k, eval (FunCallVal(f, v)) r)) l)
      | _ -> failwith("non-dict used for DictIterate")
  )

  (* Valuta l'operazione fold. Typechecking dinamico. *)
  | DictFold(f, d) -> (
    match eval d r with
      | DictVal(l) -> dictFold l f r
      | _ -> failwith("non-dict used for DictFold")
  )
  (* Come FunCall, ma l'argomento è già valutato. *)
  | FunCallVal(f, evtArg) -> 
  let fClosure = (eval f r) in (
    match fClosure with
      | FunVal(arg, fBody, fDecEnv) -> 
        eval fBody (bind fDecEnv arg evtArg)
      | RecFunVal(g, (arg, fBody, fDecEnv)) -> 
        let aVal = evtArg in
          let rEnv = (bind fDecEnv g fClosure) in
            let aEnv = (bind rEnv arg aVal) in
              eval fBody aEnv
      | _ -> failwith("non functional value")
    )
  | Estring s -> String s

  (* Interprete visto a lezione... *)
  | Eint n -> Int n
  | Ebool b -> Bool b
  | IsZero a -> iszero (eval a r)
  | Den i -> applyenv r i
  | Eq(a, b) -> eq (eval a r) (eval b r)
  | Prod(a, b) -> prod (eval a r) (eval b r)
  | Sum(a, b) -> sum (eval a r) (eval b r)
  | Diff(a, b) -> diff (eval a r) (eval b r)
  | Minus a -> minus (eval a r)
  | And(a, b) -> et (eval a r) (eval b r)
  | Or(a, b) -> vel (eval a r) (eval b r)
  | Not a -> non (eval a r)
  | Ifthenelse(a, b, c) -> 
    let g = (eval a r) in
      if (typecheck "bool" g) 
        then (if g = Bool(true) then (eval b r) else (eval c r))
        else failwith ("nonboolean guard")
  | Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r))
  | Fun(i, a) -> FunVal(i, a, r)
  | FunCall(f, eArg) -> 
    let fClosure = (eval f r) in (
      match fClosure with
        | FunVal(arg, fBody, fDecEnv) -> 
          eval fBody (bind fDecEnv arg (eval eArg r))
        | RecFunVal(g, (arg, fBody, fDecEnv)) -> 
          let aVal = (eval eArg r) in
            let rEnv = (bind fDecEnv g fClosure) in
              let aEnv = (bind rEnv arg aVal) in
                eval fBody aEnv
        | _ -> failwith("non functional value")
      )
  | Letrec(f, funDef, letBody) ->
        (match funDef with
            Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                                          eval letBody r1 |
            _ -> failwith("non functional def"))

(* Funzione che converte un evT in una stringa *)
let rec string_of_evt (v : evT) = 
  (* Funzione che converte una lista (chiave, valore) di un dizionario in una stringa. *)
  let rec string_of_kv (l : (evT * evT) list) : string = match l with
    | [] -> ""
    | (k, v)::tl -> Printf.sprintf " %s: %s," (string_of_evt k) (string_of_evt v) ^ (string_of_kv tl)
  in
  match v with
    | Int(x) -> string_of_int x
    | Bool(x) -> string_of_bool x
    | String(x) -> x
    | DictVal(l) -> "{" ^ string_of_kv l ^ " }"
    | _ -> "unknown"
;;

(* Funzione che stampa a video un evT *)
let print_evt (v : evT) = Printf.printf "%s\n" (string_of_evt v);;
    
(* =============================  TESTS  ================= *)

let test (s : string) (e : exp) (r : evT env) f = let v = (Printf.printf "%s" s); eval e r in match f v with
  | false -> failwith(s)
  | true -> Printf.printf("\tpass\n")
;;

let env0 = emptyenv Unbound;;
let magazzino = Dict([(Estring "mele", Eint 430); (Estring "banane", Eint 312); (Estring "arance", Eint 525); (Estring "pere", Eint 217)]);;

let e = DictSelect(magazzino, Estring "arance") in
test "Select arance" e env0 (fun v -> match v with
  | Int(x) -> x = 525
  | _ -> false
);;

let e = DictSelect(
  Dict[(Eint 3, Eint 30); (Eint 4, Eint 40)],
  Eint 3
) in test "Dict with int as key" e env0 (fun v -> match v with
  | Int(x) -> x = 30
  | _ -> false
);;

let e = DictSelect(magazzino, Estring "pesche") in
test "Select non-existing" e env0 (fun v -> v == Unbound);;

let e = DictHasKey(magazzino, Estring "banane") in
test "IsIn existing" e env0 (fun v -> match v with
  | Bool(x) -> x
  | _ -> false
);;

let e = DictHasKey(magazzino, Estring "pesche") in
test "IsIn non-existing" e env0 (fun v -> match v with
  | Bool(x) -> not x
  | _ -> false
);;

let e = DictSelect(
  DictInsert(magazzino, Estring "fragole", Eint 150),
  Estring "fragole"
)in test "Insert add key" e env0 (fun v -> match v with
  | Int(x) -> x = 150
  | _ -> false
);;

let e = DictSelect(
  DictInsert(magazzino, Estring "mele", Eint 900),
  Estring "mele"
) in test "Insert update value" e env0 (fun v -> match v with
  | Int(x) -> x = 900
  | _ -> false
);;

let e = DictHasKey(
  DictDelete(magazzino, Estring "banane"),
  Estring "banane"
) in test "Delete existing" e env0 (fun v -> match v with
  | Bool(x) -> not x
  | _ -> false
);;


Printf.printf("Deleting non-existing key");;
let e =  DictDelete(magazzino, Estring "uva") in
try eval e env0 with Failure x -> failwith("delete non-existing failed");;
Printf.printf("\tpass\n");;

let e = FunCall(
  Fun(
    "d",
    And(
      Not(DictHasKey(Den "d", Estring "mele")),
      And(
        Not(DictHasKey(Den "d", Estring "banane")),
        DictHasKey(Den "d", Estring "arance")
      )
    )
  ),
  DictFilter(
    [Estring "mele"; Estring "banane"],
    magazzino
  )
) in test "Filter" e env0 (fun v -> match v with
  | Bool(x) -> x
  | _ -> false
);;

let e = FunCall(
  Fun(
    "d",
    And(
      Eq(DictSelect(Den "d", Estring "mele"), Eint 431),
      And(
        Eq(DictSelect(Den "d", Estring "banane"), Eint 313),
        And(
          Eq(DictSelect(Den "d", Estring "arance"), Eint 526),
          Eq(DictSelect(Den "d", Estring "pere"), Eint 218)
        )
      )
    )
  ),
  DictIterate(
    Fun(
      "y",
      Sum(Den "y", Eint 1)
    ),
    magazzino
  )
) in test "Map" e env0 (fun v -> match v with
  | Bool(x) -> x
  | _ -> false
);;

let e = DictFold(
  Fun("x", Sum(Den "x", Eint 1)),
  magazzino
) in test "Fold" e env0 (fun v -> match v with
  | Int(x) -> x = 1488
  | _ -> false
);;