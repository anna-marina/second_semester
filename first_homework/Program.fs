//3 6
type peano = Zero | S of peano

let rec add x y =
    match y with
    | Zero -> x
    | S y' -> S (add x y')

let rec sub x y = 
    match (x, y) with
    | (Zero, _) -> Zero
    | (S x', Zero) -> S x'
    | (S x' , S y') -> sub x' y'
                
let rec dec x =
    match x with
    | Zero -> 0
    | S x' -> dec x' + 1

let rec mul x y =
    match y with
    | Zero -> Zero
    | S y' -> add (mul x y') x // x*(y+1)=x*y+x, mul x (S x) = add (mul x y) x

let rec pow x y =
    match y with
    | Zero -> S Zero
    | S y' -> mul (pow x y') x

type tree = Nil | Node of int * tree * tree //декартово произведение множеств

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
    | Node(k, l, r) -> print_LCR l; printf "%d " k; print_LCR r
    
let rec print_CLR t =
    match t with
    | Nil -> ()
    | Node(k, l, r) -> printf "%d " k; print_CLR l; print_CLR r

let rec print_LRC t =
    match t with
    | Nil -> ()
    | Node(k, l, r) -> print_LRC l; print_LRC r; printf "%d " k


let rec build l =
    match l with
    | [] -> Nil
    | x :: l' -> insert x (build l')

let rec pretty t =
    match t with
    | Nil -> "Leaf"
    | Node(k, l, r) -> "Node " + (string k) + " {" + (pretty l) + "; " + (pretty r) + "}"

[<EntryPoint>]
let main args =
    let two = Zero |> S |> S
    let three = Zero |> S |> S|> S
    let five = add two three
    printfn "%i" (dec five)
    let eight = pow two three
    printfn "%i" (dec eight)
    let t = build [1; 2; 3; 10; 12]
    printfn "%s" (pretty t)
    let newt = insert 5 t
    printfn "%s" (pretty newt)
    let newt = remove 12 t
    printfn "%s" (pretty newt)
    print_LCR newt
    printfn ""
    print_CLR newt
    printfn ""
    print_LRC newt
    0

