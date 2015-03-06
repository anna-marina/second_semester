// 3 5
type 't tree = Nil | Node of 't * 't tree * 't tree //декартово произведение множеств
 
let rec insert x t =
    match t with
    | Nil -> Node (x, Nil, Nil)
    | Node (k, l, r) when x > k -> Node (k, l, (insert x r))
    | Node (k, l, r) when x < k -> Node (k, (insert x l), r)
    | _ -> t
 
let rec remove x t =
    match t with
    | Nil -> Nil
    | Node (k, l, r) when x < k -> Node(k, (remove x l), r)
    | Node (k, l, r) when x > k -> Node(k, l, (remove x r))
    | Node (_, Nil, r) -> r //узел, где х=к (мы нашли узел)
    | Node (_, l, Nil) -> l //узел, где х=к
    | Node (k, l, r) -> //узел, где х=к
        let rec min t = //находит минимальное число в дереве (тип int option = int + None)
            match t with
            | Nil -> None
            | Node (k, Nil, _) -> Some k
            | Node (_, l, _) -> min l
        match min r with
        | None -> Nil
        | Some k' -> Node (k', l, (remove k' r))
                               
let rec print_LCR t =
   match t with
   | Nil -> ()
   | Node(k, l, r) -> print_LCR l; printf "%A " k; print_LCR r
   
let rec print_CLR t =
   match t with
   | Nil -> ()
   | Node(k, l, r) -> printf "%A " k; print_CLR l; print_CLR r
 
let rec print_LRC t =
   match t with
   | Nil -> ()
   | Node(k, l, r) -> print_LRC l; print_LRC r; printf "%A " k
 
 
let rec pretty t =
   match t with
   | Nil -> "Leaf"
   | Node(k, l, r) -> "Node " + (string k) + " {" + (pretty l) + "; " + (pretty r) + "}"
//9 val List.iter : (('a -> unit) -> 'a list -> unit)
 
//10
let reverse l = List.fold (fun a x -> x :: a) [] l
//12
let map f l = List.foldBack (fun x a -> (f x) :: a) l []
//11
let filter f l = List.foldBack (fun x a -> if f x then x :: a else a) l []
//13
let horner x l = List.fold (fun s a -> s * x + a) 0 l//s -накопл. сумма а - коэффициент
 
//15
let rec tree_map f t =
   match t with
   | Nil -> Nil
   | Node (k, l, r) -> Node (f k, tree_map f l, tree_map f r)
 
//16
let rec tree_fold f a t =
    match t with
    | Nil -> a
    | Node (k, l, r) -> f k (tree_fold f a l) (tree_fold f a r)

//17
let tree_sum t = tree_fold (fun x l r -> x + l + r) 0 t

//18
let tree_min t = tree_fold (fun x l r -> min (min x l) r) 1000000 t

//19
let tree_copy t = tree_fold (fun x l r -> Node (x, l ,r)) Nil t

let build l = List.fold (fun a x -> insert x a) Nil l
 
[<EntryPoint>]
let main args =
   let t = build [1.1; 2.3; 3.5; 10.0; 12.0]
   print_LCR t
   printfn ""
   let newt = insert 5.1 t
   print_LCR newt
   printfn ""
   let newt = remove 12.1 t
   print_LCR newt
   printfn ""
   print_CLR newt
   printfn ""
   print_LRC newt 
   printfn ""
   printfn "%i" (horner 2 [1;0;1])
   printfn "%A" [1;2;3]
   printfn "%A" (reverse [1;2;3])
   printfn "%A" (map (fun x -> x * 3) [1;2;3])
   printfn "%A" (filter(fun x -> x > 1) [1;2;3])
   let q = build [1..5]
   printfn "%A" (pretty (tree_map (fun x -> x * 2) q))
   printfn "%A" (tree_sum q)
   printfn "%A" (tree_min q)
   printfn "%A" (pretty (tree_copy q))
   0
