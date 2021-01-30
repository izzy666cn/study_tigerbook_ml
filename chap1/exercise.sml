(* BST *)
type key = string
datatype tree = LEAF | TREE of tree * key * tree
val empty = LEAF

fun insert (key, LEAF) = TREE(LEAF, key, LEAF)
  | insert (key, TREE(l, k, r)) =
                 if key < k
                    then TREE(insert(key, l), k, r)
                 else if key > k
                    then TREE(l, k, insert(key, r))
                 else TREE(l, key, r)

(* exercise 1.1a *)
fun member(_, LEAF) = false 
  | member(key, TREE(l, k, r)) =
                 if key < k
                    then member(key, l)
                 else if key > k
                    then member(key, r)
                 else true

(* exercise 1.1b *)
datatype 'a tree = LEAF | TREE of 'a tree * (key * 'a) * 'a tree
fun insert(LEAF, k, v) = TREE(LEAF, (k, v), LEAF)
  | insert(TREE(l, (k, v), r), key, value) = 
                 if key < k
                    then TREE(insert(l, key, value), (k, v), r)
                 else if key > k
                    then TREE(l, (k, v), insert(r, key, value))
                 else TREE(l, (k, v), r)

fun lookup(LEAF, key) = raise Fail("Unknown key : " ^ key)
  | lookup(TREE(l, (k, v), r), key) = 
                if key < k
                    then lookup(l, key)
                else if key > k
                    then lookup(r, key)
                else 
                    v


(* exercise 1.1d  balanced search tree *)
datatype 'a tree = LEAF | TREE of int * 'a tree * (key * 'a) * 'a tree

fun max(a, b) = if a > b then a else b

fun height LEAF = 0
  | height (TREE(h, _, _, _)) = h

fun mk_tree(l, m, r) = TREE(max(height l, height r) + 1, l, m, r)

fun rotate_l (TREE(h, l, m, TREE(rh, rl, rm, rr))) = mk_tree(mk_tree(l, m ,rl), rm, rr)

fun rotate_r (TREE(h, TREE(lh, ll, lm, lr), m, r)) = mk_tree(ll, lm, mk_tree(lr, m, r))

fun balance (TREE(h, l, m, r)) = 
            if height l + 1 < height r then
                rotate_l(TREE(h, l, m, r))
            else if height r + 1 < height l then
                rotate_r(TREE(h, l, m, r))
            else 
                TREE(h, l, m, r)

fun insert(LEAF, k, v) = TREE(1, LEAF, (k, v), LEAF)
  | insert(TREE(h, l, (k, v), r), key, value) = 
                 if key < k then balance(mk_tree(insert(l, key, value), (k, v), r))
                 else if key > k then balance(mk_tree(l, (k, v), insert(r, key, value)))
                 else TREE(h, l, (k, v), r)

fun lookup(LEAF, key) = raise Fail("Unknown key : " ^ key)
  | lookup(TREE(_, l, (k, v), r), key) = 
                if key < k
                    then lookup(l, key)
                else if key > k
                    then lookup(r, key)
                else 
                    v

val a = [
    ("a", 1), 
    ("b", 2),
    ("c", 3),
    ("d", 4)
    ]

val t = foldl (fn(x, acc) => insert(acc, #1(x), #2(x))) LEAF a
