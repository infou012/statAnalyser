open Abstract_syntax_tree
open Value_domain
open Domain
       
       
module Intervals = (struct
                     
                     
(*** Arithmetic of bounds ***)
(* Z ∪ {±∞} *)
                     
type bound =
  | Int of Z.t
  (* Z *)
  | PINF
  (* +∞ *)
  | MINF
(* −∞ *)

(* −a *)
let bound_neg (a:bound) : bound = match a with
  | MINF -> PINF | PINF -> MINF | Int i -> Int (Z.neg i)
                                               
(* a x b *)
let bound_mul (a:bound) (b:bound) : bound = match a,b with
  | MINF,PINF | PINF,MINF -> invalid_arg "bound_mul" (* (+∞) * (−∞) *)
  | MINF, Int i | Int i, MINF when Z.gt i Z.zero -> MINF
  | MINF, Int i | Int i, MINF when Z.lt i Z.zero -> PINF
  | PINF, Int i | Int i, PINF when Z.gt i Z.zero -> PINF
  | PINF, Int i | Int i, PINF when Z.lt i Z.zero -> MINF
  | _, Int i | Int i, _ when Z.equal i Z.zero -> Int  Z.zero
  | Int i, Int j -> Int (Z.mul i j)

let bound_div (a:bound) (b:bound) : bound = match a,b with
  | MINF,PINF | PINF,MINF -> invalid_arg "bound_div" (* (+∞) * (−∞) *)
  | PINF,PINF -> Int Z.zero
  | MINF, Int i -> if Z.gt i Z.zero then MINF else PINF
  | PINF, Int i -> if Z.gt i Z.zero then PINF else MINF
  | Int i, MINF | Int i, PINF -> Int Z.zero
  | Int i, Int j -> Int (Z.div i j)
                        
(* a + b *)
let bound_add (a:bound) (b:bound) : bound = match a,b with
  | MINF,PINF | PINF,MINF -> invalid_arg "bound_add" (* (+∞) + (−∞) *)
  | MINF,_ | _,MINF -> MINF
  | PINF,_ | _,PINF -> PINF
  | Int i, Int j -> Int (Z.add i j)
                        
(* compare a et b, retourne -1, 0 ou 1 *)
let bound_cmp (a:bound) (b:bound) : int = match a,b with
  | MINF,MINF | PINF,PINF -> 0
  | MINF,_ | _,PINF -> -1
  | PINF,_ | _,MINF -> 1
  | Int i, Int j -> Z.compare i j
                              
let min_bound a b = if bound_cmp a b > 0 then b else a
                                                         
let max_bound a b = if bound_cmp a b > 0 then a else b
                                                         
                                                         
let proj f a (c, d) =
  let r1 = f a c in
  let r2 = f a d in
  r1, r2
        
(* { [a, b] | a ≤ b } ∪ {⊥} *)
type t = Itv of bound * bound | BOT
                                  
(* utilities *)
(***************************)
                                  
(* extension de f par f (⊥) = ⊥ *)
let lift1 f x = match x with
  | Itv (a,b) -> f a b
  | BOT -> BOT
             
(* idem pour f (⊥, y ) = f (x, ⊥) = ⊥ *)
let lift2 f x y = match x,y with
  | BOT, _ | _, BOT -> BOT
  | Itv (a,b), Itv (c,d) ->
     f (a, b) (c, d)
       
let bound_to_string a = match a with
  | MINF -> "MINF"
  | PINF -> "PINF"
  | Int x -> Z.to_string x
                         
(* interval interface *)
(********************)
                         
(*** top = [MINF, PINF] ***)
let top = Itv (MINF, PINF)
              
(*** bottom ***)
let bottom = BOT
               
(* constant: {c} *)
let const c = Itv (Int c, Int c)
                  
(* interval: [a,b] *)
                  
let rand a b = if Z.compare a b > 0 then BOT
               else Itv (Int a, Int b)
                        
                        
(* arithmetic operations in interval domain*)
                        
(* −x  *)
let neg (x:t) : t =
  lift1 (fun a b -> Itv (bound_neg b, bound_neg a)) x
        
(*  [a,b] + [c,d]  *)              
let add x y =
  lift2 (fun (a,b) (c,d) -> Itv (bound_add a c, bound_add b d)) x y
        
(*  [a,b] - [c,d]  *)              
let sub x y = 
  lift2 (fun (a,b) (c,d) ->
      Itv (bound_add a (bound_neg d), bound_add b (bound_neg c))) x y
        
(*  [a,b] * [c,d]  *)                         
let mul x y =
  lift2
    (fun (a,b) (c,d) ->
      let ac, ad = proj bound_mul a (c,d) in
      let bc, bd = proj bound_mul b (c,d) in
      Itv (min_bound (min_bound ac ad) (min_bound bc bd),
           max_bound (max_bound ac ad) (max_bound bc bd))
    ) x y
                    
(*  [a,b] / [c,d]  *)
(*
([a, b] / ([c, d] ∩ [1, +∞])) ∪ ([a, b] / ([c, d] ∩ [−∞, −1])) with
[a,b] / [c,d] = [min(a/c, a/d), max(b/c, b/d)] si 1 ≤ c] or
[a,b] / [c,d] = [min(b/c, b/d), max(a/c, a/d)] si d ≤ −1
 *)

let div x y =
  lift2
    (fun (a,b) (c,d) ->
      let ac, ad = proj bound_div a (c, d) in
      let bc, bd = proj bound_div b (c, d) in
      if bound_cmp c (Int Z.one) > 0 then
        Itv (min_bound ac ad, max_bound bc bd)
      else if bound_cmp (Int Z.minus_one) d > 0 then
        Itv (min_bound bc bd, max_bound ac ad)
      else BOT 
    ) x y  
    
(*  [a,b] % [c,d] **TODO  *)                         
let rem x y = y
                
(* set-theoretic operations *)

let join x y : t = match x,y with
  | BOT, i | i, BOT -> i
  | Itv (a,b), Itv (c,d) -> Itv (min_bound a c, max_bound b d)
  
                      
let meet x y : t = match x, y with
  | BOT, i | i, BOT -> BOT
  | Itv (a,b), Itv (c,d) ->
     let max = max_bound a c in
     let min = min_bound b d in
     if bound_cmp max min > 0 then BOT else Itv (max, min)
        
(* x ⊆ y in interval domain *)
                                                
let subset (x:t) (y:t) : bool = match x,y with
| BOT,_ -> true
| _,BOT -> false
| Itv (a,b), Itv (c,d) -> bound_cmp a c >=0 && bound_cmp b d <= 0

 (* emptyness testing *)
let is_bottom (x:t) : bool = match x with
  | BOT -> true
  | _ -> false
        
(* print abstract element *)
let print fmt x: unit =
  match x with
  | BOT -> Format.fprintf fmt "bottom"
  | Itv (a,b) -> Format.fprintf fmt "[%s;%s]"
                                (bound_to_string a) (bound_to_string b)

      
 
(* comparison operations (filters) *)

let eq x y =
  let r = meet x y in r, r
 
let neq x y =
  match x, y with
  | BOT, _ | _, BOT -> BOT, BOT
  | Itv (a,b), Itv (c,d) when bound_cmp a c = 0 && bound_cmp b d = 0
    -> BOT, BOT
  | _, _ -> x, y
                               
let leq x y =
  match x, y with
  | BOT, _ | _, BOT -> x, y
  | Itv (a,b),  Itv (c, d) -> if bound_cmp a d > 0 then BOT, BOT
                              else Itv (a, min_bound b d),
                                   Itv (max_bound a c, d)
                                                        
let lt x y =
  let x', y' = leq x y in
  x',y'
                  
(* unary operation *)
let unary x unop : t =  match unop with
  | AST_UNARY_PLUS  -> x
  | AST_UNARY_MINUS -> neg x

(* binary operation *)
let binary x y binop : t = match binop with
  | AST_PLUS     -> add x y
  | AST_MINUS    -> sub x y
  | AST_MULTIPLY -> mul x y
  | AST_DIVIDE   -> div x y
  | AST_MODULO   -> rem x y
        
(* widening, for loops TODO *)
let widen x y : t = y


    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)

                     
let compare x y op :(t * t) = match op with
  | AST_EQUAL         -> eq x y
  | AST_NOT_EQUAL     -> neq x y
  | AST_GREATER_EQUAL -> leq y x
  | AST_GREATER       -> lt y x
  | AST_LESS_EQUAL    -> leq x y
  | AST_LESS          -> lt x y

(* TODO
       the following, more advanced operations are useful to handle
       complex tests more precisely
 *)

        
    (* backards unary operation *)
    (* [bwd_unary x op r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x

       it is safe, as first approximation, to implement it as the identity:
       let bwd_unary x _ _ = x
     *)
let bwd_unary x unop y : t = y

  
     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r

       it is safe, as first approximation, to implement it as the identity:
       let bwd_binay x y _ _ = (x,y)
      *)
let bwd_binary x y binop z : (t * t) = x, y
  

end : VALUE_DOMAIN)
