;; CSC-151-01 (Spring '23)
;; Lab: Binary Trees (binary-trees.rkt)
;; Authors: Shabab Kabir and David Rhoades
;; Date: 19 April 2023
;; Acknowledgements:
;;   The weird quirky trees on the white board near me; also just Ben.

; +-----------------------------------------+------------------------
; | Provided code: The definition of a tree |
; +-----------------------------------------+

;; A tree is either:
;;   + Empty (a leaf) or
;;   + Non-empty (a node) with a value, left subtree, and right subtree.
(struct leaf ())
(struct node (value left right))

;;; (tree? v) -> boolean?
;;;   v: any
;;; Returns #t iff v is a tree.
(define tree?
  (lambda (v) (or (leaf? v) (node? v))))

;;; (singleton v) -> tree?
;;;   v: any
;;; Returns a tree consisting of a single value, i.e., a node with no children.
(define singleton
  (lambda (v)
    (node v (leaf) (leaf))))

; +-----------------------------+------------------------------------
; | Provided code: tree->string |
; +-----------------------------+

(define tree-level->string
  (let ([bullets (vector "* " "+ " "- " ". ")]
        [make-spaces (lambda (n)
                       (list->string (make-list n #\space)))])
    (lambda (level tree)
      (let* ([spaces (make-spaces (* 2 level))]
             [bullet
               (string-append
                 spaces
                 (vector-ref bullets (remainder level (vector-length bullets))))])
        (match tree
          [(leaf) ""]
          [(node value (leaf) (leaf)) (string-append bullet value)]
          [(node value left (leaf))
           (string-append
             (string-append bullet value)
             "\n"
             (tree-level->string (+ level 1) left))]
          [(node value (leaf) right)
           (string-append
             (string-append bullet value)
             "\n"
             (tree-level->string (+ level 1) right))]
          [(node value left right)
          (string-append
            (string-append bullet value)
            "\n"
            (tree-level->string (+ 1 level) left)
            "\n"
            (tree-level->string (+ 1 level) right))])))))

(define tree->string
  (lambda (tree)
    (tree-level->string 0 tree)))

; +----------------------------------------------+-------------------
; | Provided code: The legendary management tree |
; +----------------------------------------------+

(define management-tree
  (node
    "Board"
    (leaf)
    (node
      "CEO"
      (node
        "Head of Engineering"
        (node "Software Developer" (leaf) (leaf))
        (node "Tester" (leaf) (leaf)))
      (node
        "Head of Legal"
        (leaf)
        (node "Lawyer" (leaf) (leaf))))))

(tree->string management-tree)

; +-----------+------------------------------------------------------
; | Exercises |
; +-----------+

; ------------------------
"Exercise 1: Making trees"
; ------------------------

; (Partner A drives!)

; a. Consider the following trees of numbers drawn with ASCII art:

; i. tree-i
;           "b"
;           / \
;          /   \
;        "a"   "c"

; ii. tree-ii
;            "e"
;            / \
;           /   \
;          "b"  "f"
;          / \    \
;         /   \    \
;       "a"   "c"  "g"
;             /
;            "d"

; iii. tree-iii
;           "f"
;           /
;         "e"
;         /
;       "d"
;       /
;     "c"
;     /
;   "b"
;   /
; "a"

; For each of these trees, use the tree-making functions from the
; reading to complete the definitions of `tree-i`, `tree-ii`, and
; `tree-iii` below. 

; Make sure to check that each thing you enter is a binary tree with
; `binary-tree?` and that it has the right form with `display-binary-tree`

;     > (tree? tree-i)
;     #t
;     > (tree->string tree-i)
;     * b
;       + a
;       + c
;     > (binary-tree? tree-ii)
;     #t
;     > (display-binary-tree tree-ii)
;     * 5
;       + 2
;         - 1
;         - 4
;       + 7
;         - 9
;           . 8
;     > (binary-tree? tree-iii)
;     #t
;     > (display-binary-tree tree-iii)
;     * 5
;       + 4
;         - 3
;           . 2
;             . 1
;               . 0

(define tree-i
    (node
      "b"
      (node
        "a"
        (leaf)
        (leaf))
      (node
        "c"
        (leaf)
        (leaf))))
(tree? tree-i)
(tree->string tree-i)

(define tree-ii
    (node
      "e"
      (node
        "b"
        (node "a" (leaf) (leaf))
        (node "c" (node "d" (leaf) (leaf)) (leaf)))
      (node
        "f"
        (node "g" (leaf) (leaf))
        (leaf))))
(tree? tree-ii)
(tree->string tree-ii)

(define tree-iii
    (node
      "f"
      (node
        "e"
        (node
          "d"
            (node 
              "c"
              (node
                "b"
                (node "b"
                  (node "a" (leaf) (leaf))
                  (leaf))
                (leaf))
              (leaf))
            (leaf))
        (leaf))
        (leaf)))
(tree? tree-iii)
(tree->string tree-iii)

; b. Note that tree-iii is a left-leaning tree. That is, all its children are
; left children. Complete the definition of tree-iv below which is
; the same as tree-iii except that its leaves grow to the right rather
; than the left.

; "a"
;   \
;   "b"
;     \
;     "c"
;       \ 
;       "d"
;         \ 
;         "e"
;           \
;           "f"

; > (tree? tree-iv)
; #t
; > (tree->string tree-iv)
; * 5
;   + 4
;     - 3
;       . 2
;         * 1
;           + 0

(define tree-iv
    (node
      "a"
      (node
        "b"
        (node
          "c"
            (node 
              "d"
              (node
                "e"
                (node "f"
                  (node "g" (leaf) (leaf))
                  (leaf))
                (leaf))
              (leaf))
            (leaf))
        (leaf))
        (leaf)))
(tree? tree-iii)
(tree->string tree-iii)

; d. Finally, in the space below describe in a few sentences the
; differences and similarities between tree-iii and tree-iv. Do you
; consider these trees to be the same tree or different trees?
; Why?

; In theory, the trees are different due to the direction they are "growing" towards, 
; but in practice they are basically the same,e spically since the print does not differentiate them.

; ------------------
"Exercise 2: Leaves"
; ------------------

; (Partner B drives!)

; a. As you may have noted, in the sample code, we use the very verbose
;
; (node val (leaf) (leaf))
;
; To create a node with no children. This is a bit tedious! Write a
; helper function 'node-nc' (short for "no children") that takes a value
; as input and produces a node with no children as output. Document
; the function appropriately.

(define node-nc
  (node (leaf)
        (leaf)
        (leaf)))

; Now, write a function 'childless?' that takes a tree as input and
; returns #t if and only if the tree has no children. Use the query
; functions produced by our struct declarations leaf? and node? in
; conjunction with operations over booleans for this task.  Document
; and test this function appropriately.

(define childless?
  (lambda (tree)
          (equal? node-nc tree)))
        
; ----------------------------
"Exercise 3: Traversing trees"
; ----------------------------

; (Partner C drives!)

; Recall from our discussion of structs that we can get out the
; fields of a struct in two ways:
;
; 1. Using struct projection functions.
; 2. Pattern matching.
;
; For our tree structs, we have:
;
; + The (leaf) pattern to match a leaf.
; + The (node-value n), (node-left n), and (node-right n) functions
;   to retrieve the value, left child, and right child of a node.
;   We also have the pattern (node value left right) to pattern
;   match a node and bind its value, left, and right fields all
;   at once.
;
; In the space below, write two expressions to retrieve the given
; value from the trees you created above:
;
; + One expression using combinations of projection functions.
; + One expression using a pattern matching consisting of a single
;   pattern. Recall that you can nest patterns inside of other
;   patterns.

(define search 
(lambda (item tree)
  (cond
    [(leaf? tree) #f] ; If the tree is a leaf, the item is not found
    [(equal? item (node-value tree)) tree] ; If the item matches the node value, return the node
    [(string<=? item (node-value tree)) (search item (node-left tree))] ; If the item is smaller than the node value, search in the left subtree
    [else (search item (node-right tree))]))) ; If the item is greater than the node value, search in the right subtree

(define contains-item? 
(lambda (item tree)
  (not (equal? #f (search item tree)))))

(define get-subtree-value 
(lambda (item tree)
  (if (contains-item? item tree)
      (substring (tree->string (search item tree)) 2 (+ 2 (string-length item)))
      #f)))

(define get-subtree 
(lambda (item tree)
  (if (contains-item? item tree)
      (search item tree)
      #f)))


; a. "b" from tree-i

(node-value tree-i)
(get-subtree-value "b" tree-i)

; b. "c" from tree-ii

(node-value (node-right (node-left tree-ii)))
(get-subtree-value "c" tree-ii)

; c. "b" from tree-iii

(node-value (node-left (node-left (node-left (node-left (node-left tree-iii))))))
(get-subtree-value "b" tree-iii)

; d. "Head of Legal" from management-tree

(node-value (node-right (node-right management-tree)))
(get-subtree-value "Head of Legal" management-tree)

; e. "Software Developer" from management-tree

(node-value (node-left (node-left (node-right management-tree))))

; ------------------------------------
"Exercise 4: Exploring tree recursion"
; ------------------------------------

; (Partner D drives!) D?

; From the reading, we noted that a binary tree is recursively defined like a
; list. A binary tree is either:

; + *Empty*, or
; + *Non-empty* where the tree contains a value and up to two *children*
;   (*subtrees*) that are, themselves, trees.

; Like lists, our tree operations mirror this recursive decomposition of
; the list. As a first example, consider the following function which
; computes the *size* of the input tree, *i.e.*, the number of values it
; contains.

;;; (tree-size tree) -> integer?
;;;   tree : tree?
;;; Determine how many values are in binary tree.
(define tree-size
  (lambda (t)
    (match t
      [(leaf) 0]
      [(node _ l r) (+ 1 (tree-size l) (tree-size r))])))

; a. For reference, copy and paste your definitions from tree-i and
; tree-ii from a previous problem within this comment below:

; (define tree-i undefined)
; (define tree-i
;     (node
;       "b"
;       (node
;         "a"
;         (leaf)
;         (leaf))
;       (node
;         "c"
;         (leaf)
;         (leaf))))
; (tree? tree-i)
; (tree->string tree-i)


; ; (define tree-ii undefined)
; (define tree-ii
;     (node
;       "e"
;       (node
;         "b"
;         (node "a" (leaf) (leaf))
;         (node "c" (node "d" (leaf) (leaf)) (leaf)))
;       (node
;         "f"
;         (node "g" (leaf) (leaf))
;         (leaf))))
; Now, use your mental model of computation to give an evaluation trace
; of the following expression in the space below. In your derivation,
; you may take the following short-cuts:
;
; + You may evaluate a recursive call to tree-size directly to the
;   branch of the pattern match that is selected.
; + You may elide the contents of the tree's children during evaluation.
;
; Make sure to check your work in the explorations pane when you are
; done!

(tree-size tree-i)

(tree-size tree-ii)

; ----------------------------------------------
"Exercise 5: Additional Tree Recursion Problems"
; ----------------------------------------------

;; For the following two functions, follow the prompts to implement the
;; functions using tree recursion. Make sure to write a test suite for
;; the functions using the trees you created from exercise 1.

;; (A side drives!)

;; Consider the following recursive skeleton for a function that computes the
;; combined product of all the elements in a tree of numbers.
;;
;; To compute the product of a binary tree:
;; + If the tree is empty, the product is 1.
;; + If the tree is non-empty, the product is the value at the root of the tree
;;   times the products of left and right subtrees.
;;
;; Use this skeleton to complete the implementation of tree-product below:

;;; (tree-product t) -> number
;;;   t: tree? of numbers
;;; Returns the product of all the elements in the tree t.
(define tree-product
 (lambda (t)
    (match t
      [(leaf) 1] 
      [(node v l r) (* v (tree-product l) (tree-product r))])))

(define tree-v
    (node
      1
      (node
        2
        (leaf)
        (leaf))
      (node
        3
        (leaf)
        (leaf))))

; TODO: write your test suite here!
(test-case "simple test-product test" equal? 6 (tree-product tree-v))

;; (B side drives!)

;; Consider the following recursive skeleton for a function that computes the
;; number of leaves in a tree.
;;
;; To compute the number of leaves in the tree:
;; + If the tree is empty, the tree is a leaf, so the number of leaves is 1.
;; + If the tree is non-empty, the number of leaves is the sum of the number of
;;   leaves in the left and right subtrees
;;
;; Use this skeleton to complete the implementation of tree-leaf-count below:

;; (tree-leaf-count t) -> number
;;   t: tree?
;; Returns a count of the number of leaves in tree t.
(define tree-leaf-count
 (lambda (t)
    (match t
      [(leaf) 1]
      [(node _ l r) (+ (tree-leaf-count l) (tree-leaf-count r))]))) 

; TODO: write your test suite here!
(test-case "simple tree-leaf-count test" equal? 4 (tree-leaf-count tree-v))