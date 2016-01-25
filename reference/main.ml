open Core.Std
(**)
type empty

type 'a term =
  | App of int * 'a term array
  | Var of var
  | UVar of 'a
and var = int
and ref_self = Cyc of ref_self term option ref
with sexp, compare
(**)

(**)
let vars_to_uvars (type t) (type u) (on : int -> t term) =
  let rec vars_to_uvars : u term -> t term = function
      App (f,xs) -> App (f, Array.map ~f:vars_to_uvars xs)
    | Var v -> on v
    | UVar _ -> assert false
  in vars_to_uvars
(**)

(**)
let rec instantiate_term (lookup : 'a -> 'a term) = function
    App (f,xs) -> App (f, Array.map ~f:(instantiate_term lookup) xs)
  | Var v -> Var v
  | UVar u -> lookup u

(**)
type 'a rule =
  { vars : int
  ; prems : 'a term list
  ; concl : 'a term
  }
with sexp, compare
(**)

(**)
module type SUBST =
sig
  type key
  type subst
  val fresh : int -> subst -> key array * subst
  val empty : subst
  val lookup : subst -> key -> key term option
  val inst : key -> key term -> subst -> subst option
end
(**)
(**)
module MapSubst : SUBST with type key = int =
struct
  type key = int
  type subst = key term Int.Map.t * int
  let fresh n ((x,y) : subst) = (Array.init n ~f:(fun n -> y + n), (x,y+n))
  let empty = (Int.Map.empty,0)
  let lookup (s,_) = Int.Map.find s
  let inst (k : key) (t : 'a term) ((s,mx) : subst) : subst option =
    try
      Some (Int.Map.change s k (function None -> Some t
                                       | Some _ -> raise (Failure "")), mx)
    with
    | Failure _ -> None
end
(**)
module RefSubst : SUBST with type key = ref_self =
struct
  type key = ref_self
  type subst = unit
  let empty = ()
  let fresh n () = (Array.init n ~f:(fun _ -> Cyc (ref None)), ())
  let lookup : subst -> key -> key term option =
    function () -> function Cyc k -> !k
  let inst : key -> key term -> subst -> subst option =
    function Cyc k -> fun t -> function () ->
    match !k with
    | None -> k := Some t ; Some ()
    | Some _ -> None
end


module TacMonad (Subst : SUBST) =
struct
  type uvar = Subst.key

  exception UnifyFailure

  (**)
  let rec fn_unify (cmp : 'a -> 'a -> int) (a : 'a term) (b : 'a term) (s : Subst.subst) : Subst.subst =
    match a , b with
    | App(f,xs) , App (g,ys) when f = g && Array.length xs = Array.length ys ->
      Array.fold2_exn xs ys ~init:s ~f:(fun s a b -> fn_unify cmp a b s)
    | Var l , Var r when Int.equal l r -> s
    | UVar l , UVar r when cmp l r = 0 -> s
    | UVar u , x
    | x , UVar u ->
      begin
        match Subst.inst u x s with
        | None -> raise UnifyFailure
        | Some s -> s
      end
    | _ , _ -> raise UnifyFailure
  (**)

  (**)
  module SubstMonad =
  struct
    type 'a t = Subst.subst -> 'a * Subst.subst
    let return x = fun s -> (x,s)
    let bind c k = fun s ->
      let (v,s) = c s in k v s
    let map = `Define_using_bind
  end;;

  module With_Subst = Monad.Make (SubstMonad) ;;
  include With_Subst

  type 'a with_subst = 'a SubstMonad.t
  (**)

  (*!*)
  let fresh num : uvar array with_subst =
    fun s -> Subst.fresh num s
  (*!*)

  (*!*)
  let inst (u : uvar) (t : uvar term) : bool with_subst =
    fun s ->
      match Subst.inst u t s with
      | None -> false , s
      | Some ss -> true , ss
  (*!*)

  (**)
  let get : Subst.subst with_subst =
    fun x -> (x, x)
  let put (s : Subst.subst) : unit with_subst =
    fun _ -> ((), s)
  (**)

  (**)
  let unify (a : uvar term) (b : uvar term) : bool with_subst =
    With_Subst.(get >>= fun sub ->
                try put (fn_unify compare a b sub) >>| (fun _ -> true)
                with UnifyFailure -> return false)
  (**)
  (**)
  let instantiate (a : uvar term) : uvar term with_subst =
    With_Subst.(get >>= fun sub ->
                return (instantiate_term (fun u -> match Subst.lookup sub u with
                    | None -> UVar u
                    | Some t -> t) a))
  (**)

  (**)
  let run_subst (type t) (tac : t with_subst) =
    fst (tac Subst.empty)
  (**)

  (**)
  let rec bind_all (k : uvar term -> uvar term list option with_subst) (acc : uvar term list) =
    function
    | [] -> With_Subst.return (Some [])
    | gl :: gls ->
      With_Subst.(k gl >>= function None -> return None
                                  | Some xs -> bind_all k (acc @ xs) gls)
  (**)

  (**)
  let bind_each (type t)
  : (t term -> t term list option with_subst) list -> t term list ->
    t term list -> t term list option with_subst =
    let rec bind_each  =
      function [] -> fun acc -> (function [] -> return (Some acc)
                                        | _ :: _ -> assert false)
             | t::ts ->
               let fix = bind_each ts in
               fun acc ->
                 function [] -> assert false
                        | gl :: gls -> t gl >>=
                          function None -> return None
                                 | Some ls -> fix (ls @ acc) gls
    in bind_each
         (**)
end

module HL =
struct
  type ('a,'b) hl =
    | APPLY of 'b rule
    | CALL of 'a
    | FAIL
    | IDTAC
    | OR of ('a,'b) hl * ('a,'b) hl
    | SEQ of ('a,'b) hl * ('a,'b) hc
    | ASSERT of 'b term * ('a,'b) hl
    | K of ('a,'b) hc
  and ('a,'b) hc =
    | ALL of ('a,'b) hl
    | EACH of ('a,'b) hl array
    | IDTACK
    | FAILK
  with sexp, compare

  let map_hl (type v) (type t) (type u) (f : t -> u) : (t,v) hl -> (u,v) hl =
    let rec map_hl =
      function OR (l,r) -> OR (map_hl l, map_hl r)
             | SEQ (l,r) -> SEQ (map_hl l, map_hc r)
             | ASSERT (t,c) -> ASSERT (t, map_hl c)
             | K c -> K (map_hc c)
             | CALL x -> CALL (f x)
             | FAIL -> FAIL
             | IDTAC -> IDTAC
             | APPLY r -> APPLY r
    and map_hc : (t,v) hc -> (u,v) hc =
      function ALL x -> ALL (map_hl x)
             | EACH x -> EACH (Array.map ~f:map_hl x)
             | IDTACK -> IDTACK
             | FAILK -> FAILK
    in map_hl

  type 'a tac = (string, 'a) hl
  type 'a tacC = (string, 'a) hc

  type 'a hl_prog =
    { entry : string
    ; names : int String.Map.t
    ; functions : (int, 'a) hl Int.Map.t
    }
  with sexp

  let makeProg (type t) (funcs : t tac String.Map.t) (entry : string) : t hl_prog =
    let (names, functions, _) =
      String.Map.fold_right funcs
        ~init:(String.Map.empty, Int.Map.empty, 0)
        ~f:(fun ~key ~data (names,offsets,next) ->
            (String.Map.add names ~key:key ~data:next,
             Int.Map.add offsets ~key:next ~data:data,
             next+1))
    in
    { entry = entry
    ; names = names
    ; functions = Int.Map.map functions ~f:(map_hl (String.Map.find_exn names)) }

  let rec first = function
      [] -> FAIL
    | [t] -> t
    | t :: ts -> OR (t,first ts)
end


module CompileHL (Subst : SUBST) =
struct
  open HL
  module TacMonad = TacMonad (Subst)

  let compile_hlp (prg : TacMonad.uvar hl_prog)
  : TacMonad.uvar term -> TacMonad.uvar term list option TacMonad.with_subst =
    let env = ref Int.Map.empty in
    let memo_hl = Hashtbl.Poly.create () in
    let memo_hc = Hashtbl.Poly.create () in
    let rec compile_hl (t : (int, TacMonad.uvar) hl)
    : TacMonad.uvar term -> TacMonad.uvar term list option TacMonad.with_subst =
      TacMonad.(
        Hashtbl.Poly.find_or_add memo_hl t
          ~default:(fun _ ->
              match t with
              | FAIL -> fun _ -> return None
              | IDTAC -> fun gl -> return (Some [gl])
              | OR (l, r) ->
                let l = compile_hl l in
                let r = compile_hl r in
                fun gl ->
                  With_Subst.(l gl >>= function None -> r gl
                                              | x -> return x)
              | APPLY r ->
                fun gl ->
                  fresh r.vars >>= fun vars ->
                  let v2u = vars_to_uvars (fun x -> UVar (Array.unsafe_get vars x)) in
                  unify gl (v2u r.concl) >>=
                  (function true ->
                    With_Subst.all (List.map r.prems ~f:(fun x -> instantiate (v2u x))) >>| (fun x -> Some x)
                          | false -> return None)
              | SEQ (l,r) ->
                let l = compile_hl l in
                let r = compile_hc r in
                With_Subst.(fun gl ->
                    l gl >>= function None -> return None
                                    | Some gls -> r gls)
              | CALL name ->
                fun gl -> Int.Map.find_exn !env name gl
              | ASSERT (_,k) -> compile_hl k
              | K c ->
                let c = compile_hc c in
                fun gl -> c [ gl ])
      )
    and compile_hc (tc : (int, TacMonad.uvar) hc)
    : TacMonad.uvar term list -> TacMonad.uvar term list option TacMonad.with_subst =
      TacMonad.(
        Hashtbl.Poly.find_or_add memo_hc tc
          ~default:(fun () ->
              match tc with
              | ALL t ->
                let t = compile_hl t in
                bind_all t []
              | EACH ts ->
                let ts = List.map (Array.to_list ts) ~f:compile_hl in
                bind_each ts []
              | IDTACK -> fun gls -> return (Some gls)
              | FAILK -> fun _ -> return None)
      )
    in
    let compiled = Int.Map.map ~f:compile_hl prg.functions in
    env := compiled ;
    Int.Map.find_exn compiled (String.Map.find_exn prg.names prg.entry)

  let smartK = function IDTACK -> IDTAC
                      | ALL x -> x
                      | EACH [| x |] -> x
                      | _ -> FAIL
  let smartSeq = function IDTAC -> smartK
                        | FAIL -> fun _ -> FAIL
                        | x -> function IDTACK -> x
                                      | y -> SEQ (x,y)

  let smartAll = function IDTAC -> IDTACK
                        | FAIL -> FAILK
                        | K k -> k
                        | x -> ALL x

  let rec normalize_hl k =
    function OR (l,r) -> OR (normalize_hl k l, normalize_hl k r)
           | FAIL -> FAIL
           | IDTAC -> smartK k
           | APPLY r when r.prems = [] -> APPLY r
           | APPLY r ->
             SEQ (APPLY r,
                  EACH (Array.of_list_map ~f:(fun t -> ASSERT (t, smartK k)) r.prems))
           | SEQ (c,cc) ->
             normalize_hl (normalize_hc k cc) c
           | CALL n -> smartSeq (CALL n) k
           | K r -> smartK (normalize_hc k r)
           | ASSERT (t,c) -> ASSERT (t,normalize_hl k c)
  and normalize_hc k =
    function ALL t -> smartAll (normalize_hl k t)
           | EACH ts -> EACH (Array.map ~f:(normalize_hl k) ts)
           | IDTACK -> k
           | FAILK -> FAILK
  let normalize_prog (prg : TacMonad.uvar hl_prog) : TacMonad.uvar hl_prog=
    { prg with
      functions = Int.Map.map prg.functions ~f:(normalize_hl IDTACK) }

end

(*
module LL =
struct
  type 'a m = PHANTOM of 'a
  type _ comp =
    | BIND : 'a m comp * 'b m comp -> 'b m comp
    | RET  : 'a -> 'a m comp
    | FAIL : 'a m comp
    | SET  : int comp * term comp -> unit m comp
    | VAR  : int -> 'a comp
    | CASE : term comp * 'a comp * 'a comp * 'a comp -> 'a comp
    | FIX  : 'a comp -> 'a comp
    | EQ   : int comp * int comp -> bool comp
    | GET  : int comp * 'a array comp -> 'a comp
    | IF   : bool comp * 'a comp * 'a comp -> 'a comp
    | MK_APP : int comp * term array comp -> term comp
    | MK_VAR : int comp -> term comp

  let lift (type a) (n : int) (b : a comp) : a comp =
    b

  (** We would like to have an explicit handle on the substitution.
   ** The underlying language will sometimes use imperative updates
   ** and sometimes use functional updates.
   **)
  let unify : (term -> term -> unit m) comp =
    let do_uvar (u : uvar comp) (t : term comp) : unit m comp =
      assert false
    in
    FIX (let a : term comp = VAR 1 in
         let b : term comp = VAR 2 in
         let recurse : (term * term -> unit m) comp = VAR 2 in
         CASE (a,
               CASE (lift 2 b,
                     IF (EQ (VAR 1, VAR 3),
                         FAIL,
                         FAIL),
                     FAIL,
                     do_uvar (VAR 0) (lift 1 a)),
               CASE (lift 1 b,
                     FAIL,
                     IF (EQ (VAR 0, VAR 1),
                         RET (),
                         FAIL),
                     do_uvar (VAR 0) (lift 1 a)),
               do_uvar (VAR 0) (lift 1 b)))
end

(* HL -> LL *)
module HL_LL =
struct
  let compile_hl (t : HL.hl) : unit LL.m LL.comp =
    match t with
    | HL.FAIL -> LL.FAIL
    | HL.IDTAC -> LL.RET ()
    | HL.APPLY r ->
      LL.(BIND (unify
    | HL.OR (l,r) -> assert false
    | _ -> assert false

*)

module Demo_EvenOdd (Subst : SUBST) =
struct
  open Subst
  let even (t : 'a term) = App (0, [| t |])
  let odd (t : 'a term) = App (1, [| t |])
  let succ (t : 'a term) = App (2, [| t |])
  let zero = App (3, [| |])
  let rec make_n (n : int) =
    if n = 0 then
      App (3, [| |])
    else
      succ (make_n (n - 1))

  let rule_EO =
    { vars = 1
    ; prems = [ odd (Var Int.zero) ]
    ; concl = even (succ (Var Int.zero))
    }
  let rule_OE =
    { vars = 1
    ; prems = [ even (Var Int.zero) ]
    ; concl = odd (succ (Var Int.zero))
    }
  let rule_O =
    { vars = 0
    ; prems = [ ]
    ; concl = even zero
    }

  let even_odd_tac : Subst.key HL.hl_prog =
    HL.(makeProg
          (String.Map.of_alist_exn
             [("even_odd",
               SEQ (first [ APPLY rule_EO
                          ; APPLY rule_OE
                          ; APPLY rule_O
                          ], ALL (CALL "even_odd")))])
          "even_odd")

  let even_odd_tac_opt : Subst.key HL.hl_prog =
    HL.(makeProg
          (String.Map.of_alist_exn
             [("odd",
               SEQ (APPLY rule_OE, ALL (CALL "even")))
             ;("even",
               first [ SEQ (APPLY rule_EO, ALL (CALL "odd")) ;
                       APPLY rule_O ])
             ;("even_odd",
               first [ CALL "even" ; CALL "odd" ])])
          "even_odd")

  let custom =
    let rec check_even =
      function App (2, [| t |]) -> check_odd t
             | App (3, _) -> true
             | _ -> false
    and check_odd =
      function App (2, [| t |]) -> check_even t
             | _ -> false
    in
    function App (0, [| t |]) -> check_even t
           | App (1, [| t |]) -> check_odd t
           | _ -> false
end

let print_sexp t = print_string ((Sexp.to_string_hum t) ^ "\n")

module CompileMap = CompileHL (MapSubst)
module DemoMap = Demo_EvenOdd (MapSubst)

module CompileRef = CompileHL (RefSubst)
module DemoRef = Demo_EvenOdd (RefSubst)

let run_it (t : int HL.hl_prog) =
  print_string "Starting with:\n" ;
  print_sexp (HL.sexp_of_hl_prog Int.sexp_of_t t) ;
  let t_opt = CompileMap.normalize_prog t in
  print_string "Normalized to:\n" ;
  print_sexp (HL.sexp_of_hl_prog Int.sexp_of_t t_opt)

let tac = CompileMap.compile_hlp DemoMap.even_odd_tac
let tac_man = CompileMap.compile_hlp (DemoMap.even_odd_tac_opt)
let tac_opt = CompileMap.compile_hlp (CompileMap.normalize_prog DemoMap.even_odd_tac)
let rtac = CompileRef.compile_hlp DemoRef.even_odd_tac
let rtac_man = CompileRef.compile_hlp (DemoRef.even_odd_tac_opt)
let rtac_opt = CompileRef.compile_hlp (CompileRef.normalize_prog DemoRef.even_odd_tac)
let compiled = DemoMap.custom

let time str x =
  let start = Time.now () in
  x () ;
  let stop = Time.now () in
  Printf.printf "%s: %f\n" str (Time.Span.to_float (Time.diff stop start))

let do_n n t () =
  let problem = DemoMap.(even (make_n 10000)) in
  for i = 0 to n do
    match CompileMap.TacMonad.run_subst (t problem) with
    | None -> ()
    | Some _ -> ()
  done

let do_n_ref n t () =
  let problem = DemoRef.(even (make_n 10000)) in
  for i = 0 to n do
    match CompileRef.TacMonad.run_subst (t problem) with
    | None -> ()
    | Some _ -> ()
  done

let () =
  let n = 100 in
  time "tac" (do_n n tac) ;
  time "tac_man" (do_n n tac_man) ;
  time "tac_opt" (do_n n tac_opt) ;
  time "ref_tac" (do_n_ref n rtac) ;
  time "ref_tac_man" (do_n_ref n rtac_man) ;
  time "ref_tac_opt" (do_n_ref n rtac_opt) ;
  time "compiled" (fun x ->
      let problem = DemoMap.(even (make_n 10000)) in
      for i = 0 to n do
        match CompileMap.TacMonad.run_subst (if compiled problem then
                                               CompileMap.TacMonad.return (Some [])
                                             else CompileMap.TacMonad.return None) with
        | None -> ()
        | Some _ -> ()
      done) ;
  run_it DemoMap.even_odd_tac
