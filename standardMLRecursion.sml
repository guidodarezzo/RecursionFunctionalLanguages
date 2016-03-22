



Control.Print.printDepth := 100;
Control.Print.printLength := 100;


(* Problem 1 *)
fun partition x [] = ([], [])
	| partition x (s::e) = 
	let val (lhs, rhs) = partition x e 
	in
		if s < x then (s::lhs, rhs)
		else (lhs, s::rhs)
		end

(* Problem 2 *)		

fun partitionSort [] = []
	| partitionSort (x::xs) = 
	let val (left, right) = partition x xs
	in
		partitionSort(left) @ [x] @ partitionSort (right)
	end

(* Problem 3 *)

fun Sort (op <) [] = []
	| Sort (op <) [x] = [x]
	| Sort (op <) (x::xs) = 
	let fun partition (lhs, rhs, []) = (lhs, rhs)
		| partition (lhs, rhs, s::e) = 
		if s < x 
		then partition (s::lhs, rhs, e)
		else partition (lhs, s::rhs, e)
			val (lhs, rhs) = partition ([], [x], xs)
		in
			Sort (op <) lhs @ Sort (op <) rhs
		end



(* Problem 4 *)

datatype 'a tree = 
empty | leaf of 'a | node of 'a * ('a tree) * ('a tree)


(* Problem 5 *)

exception EmptyTree

fun maxTree (op <) empty = raise EmptyTree
	| maxTree (op <) (leaf (a)) = a
	| maxTree (op <) (node(x,empty,r)) = maxTree (op <) (node(x,r,r))
	| maxTree (op <) (node(x,l,empty)) = maxTree (op <) (node(x,l,l))
	| maxTree (op <) (node (x, l, r)) = 
	let 
		val track_l = maxTree (op <) l
		val track_r = maxTree (op <) r
	in
		if (op <) (track_r, x) andalso (op <) (track_l, x)
			then x
		else if (op <) (track_l, track_r)
			then track_r
		else track_l

		end




(* Problem 6 *)


fun preorder x lhs rhs = x::(lhs @ rhs)
fun inorder x lhs rhs = lhs @ (x::rhs)
fun postorder x lhs rhs = (lhs @ rhs) @ [x]

fun Labels order (empty) = []
	| Labels order (leaf (x)) = [x]
	| Labels order (node(x,left,right)) = 

	let
	 		val main_label = x
	 		val l = Labels order left
	 		val r = Labels order right
	 		 
	 	in
	 		order main_label l r 
	
	 	    end


(* Problem 7 *)

fun lexLess (op <) [] [] = false
	| lexLess (op <) (x::xs) [] = false
	| lexLess (op <) [] (x::xs) = true
	| lexLess (op <) (x::xs) (y::ys) = 

	if x < y then true
	else if y < x then false
	else lexLess (op <) xs ys



(* Problem 8 *)


fun sortTreeList (op <) [] = []
	| sortTreeList (op <) [x] = [x]
	| sortTreeList (op <) (x::xs) = 

	let fun list_sort (lhs, rhs, []) = (lhs, rhs)
			| list_sort (lhs, rhs, s::e) = 
			if lexLess (op <) (Labels inorder s) (Labels inorder x)
			then list_sort (s::lhs, rhs, e)
			else list_sort (lhs, s::rhs, e)
				val (lhs, rhs) = list_sort ([], [x], xs) 
			in 
				sortTreeList (op <) lhs @ sortTreeList (op <) rhs
			end


























