module L
: sig
  type ('a,'n) t = private 'a list
  val tol: ('a,'n) t -> 'a list
  val nil: ('a,unit) t
  val cons: 'a -> ('a,'n) t -> ('a,unit->'n) t
  val hd: ('a,unit->'n) t -> 'a
  val tl: ('a,unit->'n) t -> ('a,'n) t
  val zip: ('a,'n) t -> ('b,'n) t -> ('a*'b,'n) t
  val unzip: ('a*'b,'n) t -> ('a,'n) t * ('b,'n) t
end
= struct
  type ('a,'b) t = 'a list
  let tol l = l
  let nil = []
  let cons a l = a::l
  let hd (a::_) = a
  let tl (_::q) = q
  let zip = List.combine
  let unzip = List.split
end

let li = L.( cons 1 (cons 2 (cons 3 nil)) )
let li' = L.cons 0 li
let ls = L.( cons "Aa," (cons "Bb;" (cons "Cc." (cons "Zzz…" nil))) )

module type T
= sig
  type ('a,'n) t
  val tol: ('a,'n) t -> 'a list
  val nil: ('a,unit) t
  val cons: 'a -> ('a,'n) t -> ('a,unit->'n) t
  val hd: ('a,unit->'n) t -> 'a
  val tl: ('a,unit->'n) t -> ('a,'n) t
  val zip: ('a,'n) t -> ('b,'n) t -> ('a*'b,'n) t
  val unzip: ('a*'b,'n) t -> ('a,'n) t * ('b,'n) t
end

module T
= struct
  type (_,_) t =
    | Nil:  ('a,unit) t
    | Cons: 'a * ('a,'n) t -> ('a,unit->'n) t
  let nil = Nil
  let cons a l = Cons (a, l)
  let hd (Cons(a,_)) = a
  let tl (Cons(_,q)) = q
  let rec zip  :type a b n. ((a,n) t -> (b,n) t -> (a*b,n) t)
  = function
    | Nil       -> (function Nil -> Nil)
    | Cons(a,p) -> (function Cons(b,q) -> (Cons((a,b),zip p q)))
  let rec unzip   :type a b n. (a*b,n) t -> (a,n) t * (b,n) t
  = function
    | Nil           -> Nil, Nil
    | Cons((a,b),q) ->
      let (l,l') = unzip q in
      Cons(a,l), Cons(b,l')
  let rec len   :type a n. (a,n) t -> int
  = function
    | Nil      -> 0
    | Cons(_,q)-> 1 + len q
  (*let rec nth   :type a n. int -> (a,unit->n) t -> a
  = function
    | 0 -> hd
    | n -> (function Cons(a,q) -> nth (n-1) q)*)
  let rec nth   :type a. int -> (a,unit->_) t -> a
  = function
    | 0 -> hd
    | n ->
      let f :type n. (a,unit->unit->n) t -> a
      = (function Cons(a,q) -> nth (n-1) q)
      in f
  let rec tol  :type a n. (a,n) t -> a list
  = function
    | Nil       -> []
    | Cons(a,q) -> a :: tol q
  (*let rec froml   :type a n. a list -> (a,n) t
  = function
    | []   -> Nil
    | a::q -> Cons(a, froml q)*)
end

let tli = T.( cons 1 (cons 2 (cons 3 nil)) )
let tli' = T.cons 0 tli
let tls = T.( cons "Aa," (cons "Bb;" (cons "Cc." (cons "Zzz…" nil))) )
