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
  | MINF,PINF | PINF,MINF -> invalid_arg "bound_mul" (* (+∞) + (−∞) *)
  | MINF, Int i | Int i, MINF when i>0 -> MINF
  | MINF, Int i | Int i, MINF when i<0 -> PINF
  | PINF, Int i | Int i, PINF when i>0 -> PINF
  | PINF, Int i | Int i, PINF when i<0 -> MINF
  | _, Int i | Int i, _ when i=0 -> Int  Z.zero
  | Int i, Int j -> Int (Z.mul i j)
                        
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
                              
let min_bound (a,b) = if bound_cmp a b > 0 then b else a

let max_bound (a,b) = if bound_cmp a b > 0 then a else b

                                                       
let proj a (c, d) =
  let r1 = bound_mul a c in
  let r2 = bound_mul a d in
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

let rand a b = if Z.compare a b > 0 then Itv (Int b, Int a)
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
  let f (a,b) (c,d) =
    let min, max =
      let ac, ad = proj a (c,d) in
      let bc, bd = proj b (c,d) in
      (min_bound (min_bound ac ad) (min_bound bc bd)),
      (max_bound (max_bound ac ad) (max_bound bc bd)) 
        
                
(*  [a,b] / [c,d]  *)                         
let div x y = y

(*  [a,b] % [c,d]  *)                         
let rem x y = y
                
(* set-theoretic operations *)

let join x y : t = match x,y with
  | BOT, i | i, BOT -> i
  | Itv (a,b), Itv (c,d) -> intv ((min_bound a c), (max_bound b d))
  
                      
let meet x y : t = match x y with
  | BOT, i | i, BOT -> BOT
  | Itv (a,b), It (c,d) ->
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
  | Itv (a,b) -> Format.fprintf fmt "[%s, %s]"
                                (bound_to_string a) (bound_to_string b)

      
 
(* comparison operations (filters) *)

let eq x y =
  let r = meet x y in r, r
 
let neq x y =
  match x, y with
  | BOT, i | i, BOT -> x, y
  | Itv (a,b), Itv (c,d) when bound_cmp a c = 0 && bound_cmp b d = 0
    -> BOT, BOT
  | _, _ -> x, y
                            
                            
let geq x y =
  match x, y with
  | BOT, _ | _, BOT -> BOT, BOT
  | Itv (a,b), Itv (c,d) when bound_cmp a c = 0 && bound_cmp b d = 0
    -> BOT, BOT
  | _, _ -> x, y
              | Cst x, Cst y -> if (x>=y) then a, b else BOT, BOT
  | TOP, _ | _, TOP -> a, b
                            
                            
let gt a b =
  match a, b with
  | BOT, _ | _, BOT -> BOT, BOT
  | Cst x, Cst y -> if (x>y) then a, b else BOT, BOT
  | TOP, _ | _, TOP -> a, b
                            
                                                        
(* operator dispatch *)
                            
let unary x op = match op with
  | AST_UNARY_PLUS  -> x
  | AST_UNARY_MINUS -> neg x
                           
let binary x y op = match op with
  | AST_PLUS     -> add x y
  | AST_MINUS    -> sub x y
  | AST_MULTIPLY -> mul x y
  | AST_DIVIDE   -> div x y
  | AST_MODULO   -> rem x y
                        
let compare x y op = match op with
  | AST_EQUAL         -> eq x y
  | AST_NOT_EQUAL     -> neq x y
  | AST_GREATER_EQUAL -> geq x y
  | AST_GREATER       -> gt x y
  | AST_LESS_EQUAL    -> let y',x' = geq y x in x',y'
  | AST_LESS          -> let y',x' = gt y x in x',y'
                                                    
                                                    
(* unary operation *)
let unary x unop : t = x

(* binary operation *)
let binary x y binop : t = y
        
(* widening, for loops *)
let widen x y : t = y


    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)

                     
let compare x y op :(t * t) = x,y



    (* 
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
