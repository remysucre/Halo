;;
;; Problem A
;;

;; a functional version of f-imperative
(define f-functional (y)
        (letrec ((change_x (lambda (x y)
                        (if (p x y)
                                (change_x (g x y) y)
                                x))))
         (h (change_x e y) y)))

;;
;; Problem 9
;;

;; B

(check-expect (max* '(1)) 1)
(check-expect (max* '(1 2 3)) 3)
(check-expect (max* '(1 7 3)) 7)
(check-expect (max* '(9 7 3)) 9)

;; finds the maximum of a non-empty list of integers
(define max* (xs)
        (foldr max (car xs) xs))

;; C

(check-expect (gcd* '(7)) 7)
(check-expect (gcd* '(14 21 35)) 7)
(check-expect (gcd* '(14 21 41)) 1)

;; finds the greatest common divisor of a non-empty list of integers
(define gcd* (xs)
        (foldr gcd (car xs) xs))

;; D

(check-expect (lcm* '(7)) 7)
(check-expect (lcm* '(14 21 35)) 210)
(check-expect (lcm* '(14 21 41)) (* 41 42))

;; finds the least common multiple of a non-empty list of integers
(define lcm* (xs)
        (foldr lcm (car xs) xs))

;; E

(check-expect (sum '(0)) 0)
(check-expect (sum '(-1)) -1)
(check-expect (sum '(1 -1)) 0)
(check-expect (sum '(1 2 3)) 6)

;; sums a list of integer
(define sum (xs)
        (foldr + 0 xs))

;; F

(check-expect (product '(0)) 0)
(check-expect (product '(-1)) -1)
(check-expect (product '(1 -1)) -1)
(check-expect (product '(1 2 3)) 6)

;; finds the product of a list of integer
(define product (xs)
        (foldr * 1 xs))

;; G

(check-expect (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (append '() '()) '())
(check-expect (append '() '(1)) '(1))

;; append list xs to ys
(define append (xs ys)
        (foldr cons ys xs))

;; I

(check-expect (reverse '()) '())
(check-expect (reverse '(1 2 3)) '(3 2 1))

;; reverse a list
(define reverse (xs)
        (foldl cons '() xs))

;;
;; Problem 10
;;

(check-expect (length '()) 0)
(check-expect (length '(())) 1)
(check-expect (length '(() ())) 2)
(check-expect (length '(() 1 3)) 3)

;; finds the length of the given list
(define length (xs)
        (foldr (lambda (x n) (+ 1 n)) 0 xs))

(check-expect (map (lambda (x) (* x x)) '(1 2 3)) '(1 4 9))
(check-expect (map (lambda (x) (* x x)) '()) '())

;; apply f to each element in xs, return the list of results`
(define map (f xs)
        (foldr (lambda (x ys) (cons (f x) ys)) '() xs))

(check-expect (filter number? '(a 2 b 4)) '(2 4))
(check-expect (filter number? '(a b)) '())
(check-expect (filter number? '()) '())

;; return a list of members of xs which satisfy predicate p
(define filter (p xs)
        (foldr (lambda (x ys) (if (p x) (cons x ys) ys)) '() xs))

(check-expect (exists? number? '(a 2 b 4)) #t)
(check-expect (exists? number? '(a b)) #f)
(check-expect (exists? number? '()) #f)

;; tells if there is an element in xs satisfying predicate p
(define exists? (p xs)
        (foldr (lambda (x y) (or (p x) y)) #f xs))

(check-expect (all? number? '(a 2 b 4)) #f)
(check-expect (all? number? '(1 2)) #t)
(check-expect (all? number? '()) #f)

;; tells if all elements in xs satisfy predicate p
(define all? (p xs)
        (foldr (lambda (x y) (and (p x) y)) (not (null? xs)) xs))

;;
;; Problem 15
;;

;; a)

;; tests membership of x in set s
(define member? (x s) (s x))
;; emptyset has no member
(define emptyset (x) #f)
;; add an element y to set ys, given an indicator of equivalence (eqfun)
(define add-element (eqfun y ys)
        (lambda (x)
                (if (ys y)
                        ys
                        (or (eqfun y x) (ys y)))))
;; union of sets xs and ys
(define union (xs ys)
        (lambda (x) (or (xs x) (ys x))))
;; intersection of sets xs and ys
(define inter (xs ys)
        (lambda (x) (and (xs x) (ys x))))
;; subset of xs that is not in ys
(define diff (xs ys)
        (lambda (x) (and (xs x) (not (ys x)))))

(check-expect (member? 'a 
        (add-element = 'a (add-element = 'b (add-element = 'c emptyset)))) #t)
(check-expect (member? 'a (add-element = 'a emptyset)) #t)

;; b)

;; returns a list of functions for set polymorphism, according to eqfun. 
(define mk-set-ops (eqfun)
        (list6 emptyset
               member?
               (lambda (y ys) (add-element eqfun y ys))
               union
               inter
               diff))
;;
;; Problem S
;;

;; returns a quicksort function that sorts the list according to the given order
;; e.g. qsort(>) returns a function that sorts the list from large to small
(define qsort (comp)
        (lambda (l)
                (letrec ((qsort_app (lambda (l tail) ; sort l and append to tail
                                ; tail is sorted and all e in tail < all e in l
                        (if (null? l)
                                tail
                        (let* ((p (car l)) ; choose the first one as pivot
                               (!comp (o not ((curry comp) p))) ; not comp
                               (R (filter ((curry comp) p) (cdr l)))
                               (L (filter !comp (cdr l))))
                         (qsort_app L (cons p (qsort_app R tail))))))))
                 (qsort_app l '()))))

;; termination: qosrt terminates, because for each recursion qsort_app on L
;; and R, the union of them will have one fewer element than the original list
;; (because they are built on (cdr l)), and evetually they will become empty
;; and make qsort_app go into the base case. 

;;
;; Problem 22
;;

;; with a boolean formula f, a fail continuation and a success continuation, 
;; try to produce a partial assignment to make the formula true. 
(define make-formula-true (f fail succ)
        (letrec ((make-formula (lambda (formula bool cur fail succeed)
                ;; with partial assignment cur, try to make formula produce
                ;; the value bool. 
                        (if (atom? formula)
                                (make-lit formula bool cur fail succeed)
                        (if (= 'not (car formula))
                                (make-formula (cadr formula) (not bool) 
                                              cur fail succeed)
                        (if (= 'and (car formula))
                                (if bool
                                        (make-all (cdr formula) bool 
                                                cur fail succeed)
                                        (make-any (cdr formula) bool 
                                                cur fail succeed))
                        ;; else the head is or
                                (if bool
                                        (make-any (cdr formula) bool
                                                cur fail succeed)
                                        (make-all (cdr formula) bool
                                                cur fail succeed)))))))
                 (make-all (lambda (formulas bool cur fail succeed)
                ;; with partial assignment cur, try to make all formulas produce
                ;; the value bool. 
                        (if (null? formulas)
                                (succeed cur fail)
                                (make-formula (car formulas) bool cur fail 
                                        (lambda (cur resume)
                                                (make-all (cdr formulas) bool 
                                                        cur resume succeed))))))
                 (make-any (lambda (formulas bool cur fail succeed)
                ;; with partial assignment cur, try to make any formula in 
                ;; formulas produce the value bool. 
                        (if (null? formulas)
                                (fail)
                                (make-formula (car formulas) bool cur
                                        (lambda () 
                                                (make-any (cdr formulas) bool
                                                        cur fail succeed))
                                        succeed))))
                 (make-lit (lambda (lit bool cur fail succeed)
                ;; with partial assignment cur, try literal lit
                ;; produce the value bool. 
                        (if (satisfies? lit bool cur)
                                (succeed cur fail)
                                (if (binds? lit cur)
                                        (fail)
                                        (succeed (bind lit bool cur) fail)))))
                 (satisfies? (lambda (lit bool alist)
                ;; tells if a partial assignment alist binds lit to bool
                        (let ((result (find lit alist)))
                                (if (null? result)
                                        #f
                                        (= result bool)))))
                 (binds? (lambda (var al)
                ;; tells if var is bound in assignment list al
                                (not (null? (find var al))))))
        (make-formula f #t '() fail succ)))
