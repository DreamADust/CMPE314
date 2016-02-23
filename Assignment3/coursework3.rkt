#lang plai-typed

;;Yasin Onur Yücel
;;CMPE 314 - ASSINGMENT 3
;;http://cs.brown.edu/courses/cs173/2012/book/Everything__We_Will_Say__About_Parsing.html
;;https://github.com/chrisstephenson/CMPE314-2016/blob/master/msl-starter.plai

;; msl -> number
;; msl -> msl+msl
;; msl -> msl*msl
;; msl -> (msl)
;; Alphabet: [+, *, (), **, -, number]


;; msl is a typed defined as follows,
;; msl = <number>
;; msl = (add <msl> <msl>)
;;       (mul <msl> <msl>)
;;       (sub <msl> <msl>)
;;       (exp <msl> <msl>)

;;tests
;;(msl-num 7)
;;(msl-div (msl-num 5) (msl-num 5))
;;(msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 30))

(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl) (rhs : msl)]
  [msl-exp (lhs : msl) (rhs : msl)])

;; exponentiation function
(define(** u t)
  (cond
    ((= u 1) t)
    (else
     (* t(**(sub1 u) t)))))

;;Tests
(msl-num 7)
(msl-add (msl-num 3) (msl-num 4))
(msl-mul (msl-num 4) (msl-num 3))
(msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 30))
(msl-mul (msl-add (msl-num 3) (msl-num 4)) (msl-num 5))

;;----------------------------------------------------------

;; eval : msl -> number
;; Purpose: Arithmetic expression evaluator for msl type.
;; Template:
;;(define (eval [expr : msl])
;;(type-case msl expr
;;[msl-num (n) n]
;;[msl-add (lhs rhs) (...)]
;;[msl-mul (lhs rhs) (...)]
;;[msl-sub (lhs rhs) (...)]
;;[msl-exp (lhs rhs) (...)]))

;;(test (eval (msl-num 5))  5)
;;(test (eval (msl-add (msl-num 3) (msl-num 4)))  7)
;;(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)



(define (eval [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (eval lhs) (eval rhs))]
    [msl-mul (lhs rhs) (* (eval lhs) (eval rhs))]
    [msl-sub (lhs rhs) (- (eval lhs) (eval rhs))]
    [msl-exp (lhs rhs) (** (eval lhs) (eval rhs))]))

;;Tests
(test (eval (msl-num 7))  7)
(test (eval (msl-num 5))  5)
(test (eval (msl-exp (msl-num 2) (msl-num 4)))  16)
(test (eval (msl-add (msl-num 3) (msl-num 4)))  7)
(test (eval (msl-sub (msl-num 5) (msl-num 2)))  3)
(test (eval (msl-mul (msl-num 8) (msl-num 1)))  8)
(test (eval (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))  42)
(test (eval (msl-mul (msl-add (msl-num 3) (msl-num 4)) (msl-num 5)))  35)
(test (eval (msl-add (msl-sub (msl-num 20) (msl-num 5)) (msl-num 35)))  50)

;;----------------------------------------------------------------------------------

;; PARSER FOR PREFIX
;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(+ 3 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(+ (+ 3 4) 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))

(define (parse-prefix [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(*) (msl-mul (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(-) (msl-sub (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(**) (msl-exp (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [else (error 'parse-prefix "invalid list input")]))]
    [else (error 'parse-prefix "invalid input")]))


(test (parse-prefix '7) (msl-num 7))
(test (parse-prefix '2) (msl-num 2))
(test (parse-prefix '(+ 3 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse-prefix '(- 20 4)) (msl-sub (msl-num 20) (msl-num 4)))
(test (parse-prefix '(* 2 8)) (msl-mul (msl-num 2) (msl-num 8)))
(test (parse-prefix '(+ (+ 3 4) 35)) (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35)))
(test (parse-prefix '(+ (- 3 1) 35)) (msl-add (msl-sub (msl-num 3) (msl-num 1)) (msl-num 35)))
(test (parse-prefix '(- (* 3 4) 6)) (msl-sub (msl-mul (msl-num 3) (msl-num 4)) (msl-num 6)))
(test (parse-prefix '(+ (- 3) (* 5 (- (* 7 6))))) (msl-add (msl-num 3)(msl-mul (msl-num 5)(msl-mul (msl-num 7)(msl-num 6)))))

"---------------------------------------------------------------------------------------"

;; PARSER FOR INFIX
;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 -> (msl-num 7)
;; '(3 + 4) -> (msl-add (msl-num 3) (msl-num 4))
;; '(2 + 3) -> (msl-add (msl-num 2) (msl-num 3))
;; '(2 - (3 * 4)) (msl-sub (msl-num 2)(msl-mul (msl-num 3)(msl-num 4))))
;; '((3 + 4) + 35) -> (msl-add (msl-add (msl-num 3) (msl-num 4)) (msl-num 35))
;; '((2 + 3) ** (3 * 4))) (msl-exp (msl-add (msl-num 2)(msl-num 3))(msl-mul (msl-num 3)(msl-num 4))))

(define (parse-infix [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([s1 (s-exp->list s)])
       (case (s-exp->symbol (second s1))
         [(+) (msl-add (parse-infix (first s1)) (parse-infix (third s1)))]
         [(*) (msl-mul (parse-infix (first s1)) (parse-infix (third s1)))]
         [(-) (msl-sub (parse-infix (first s1)) (parse-infix (third s1)))]
         [(**) (msl-exp (parse-infix (first s1)) (parse-infix (third s1)))]
         [else (error 'parse-infix "invalid list input")]))]
    [else (error 'parse-infix "invalid input")]))

;;Tests
(test (parse-infix '2) (msl-num 2))
(test (parse-infix '5) (msl-num 5))
(test (parse-infix '6) (msl-num 6))
(test (parse-infix '7) (msl-num 7))
(test (parse-infix '8) (msl-num 8))
(test (parse-infix '(3 + 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse-infix '(2 - 5)) (msl-sub (msl-num 2) (msl-num 5)))
(test (parse-infix '(6 * 7)) (msl-mul (msl-num 6) (msl-num 7)))
(test (parse-infix '(2 ** 3)) (msl-exp (msl-num 2) (msl-num 3)))
(test (parse-infix '(3 + 4)) (msl-add (msl-num 3) (msl-num 4)))
(test (parse-infix '(2 - (3 * 4))) (msl-sub (msl-num 2) (msl-mul (msl-num 3)(msl-num 4))))
(test (parse-infix '((6 * 7) + 35)) (msl-add (msl-mul (msl-num 6)(msl-num 7))(msl-num 35)))
(test (parse-infix '((1 - 2) * 5)) (msl-mul (msl-sub (msl-num 1)(msl-num 2))(msl-num 5)))
(test (parse-infix '((2 ** 3) - 5)) (msl-sub (msl-exp (msl-num 2)(msl-num 3))(msl-num 5)))
(test (parse-infix '((3 + 4) - 5)) (msl-sub (msl-add (msl-num 3)(msl-num 4))(msl-num 5)))
(test (parse-infix '((10 - 9) ** (3 * 4))) (msl-exp (msl-sub (msl-num 10)(msl-num 9))(msl-mul (msl-num 3)(msl-num 4))))
(test (parse-infix '((2 + 3) + (3 * 4))) (msl-add (msl-add (msl-num 2)(msl-num 3))(msl-mul (msl-num 3)(msl-num 4))))
(test (parse-infix '((5 + 5) - (3 + 4))) (msl-sub (msl-add (msl-num 5)(msl-num 5))(msl-add (msl-num 3)(msl-num 4))))
(test (parse-infix '((1 + 1) * (5 * 6))) (msl-mul (msl-add (msl-num 1)(msl-num 1))(msl-mul (msl-num 5)(msl-num 6))))
(test (parse-infix '((2 + 3) ** (3 * 4))) (msl-exp (msl-add (msl-num 2)(msl-num 3))(msl-mul (msl-num 3)(msl-num 4))))

"-----------------------------------------------------------------------------------------------------------------------"

;; UNPARSE-PREFIX
;; unparse-prefix: msl -> s-exp
;; Purpose: To unparse given msl to s-expression.
;; Tests:
;; (test (unparse-prefix (parse-prefix '(+ 2 3))) '(+ 2 3))
;; (test (unparse-prefix (parse-prefix '(+ 2 (* 3 4)))) '(+ 2 (* 3 4)))
;; (test (unparse-prefix (parse-prefix '(- (+ 2 3) (* 3 4)))) '(- (+ 2 3) (* 3 4)))

(define (unparse-prefix (expr : msl)) : s-expression
  (type-case msl expr
    (msl-num (n) (number->s-exp n))
    (msl-add (lhs rhs) (list->s-exp (list (symbol->s-exp '+) (unparse-prefix lhs) (unparse-prefix rhs))))
    (msl-mul (lhs rhs) (list->s-exp (list (symbol->s-exp '*) (unparse-prefix lhs) (unparse-prefix rhs))))
    (msl-sub (lhs rhs) (list->s-exp (list (symbol->s-exp '-) (unparse-prefix lhs) (unparse-prefix rhs))))
    (msl-exp (lhs rhs) (list->s-exp (list (symbol->s-exp '**) (unparse-prefix lhs) (unparse-prefix rhs))))))

(test (unparse-prefix (parse-prefix '(+ 2 3))) '(+ 2 3))
(test (unparse-prefix (parse-prefix '(+ 2 (* 3 4)))) '(+ 2 (* 3 4)))
(test (unparse-prefix (parse-prefix '(- (+ 2 3) (* 3 4)))) '(- (+ 2 3) (* 3 4)))

"------------------------------------------------------------------------------------------------------------------"

;; UNPARSE-INFIX
;; unparse-infix: msl -> s-exp
;; Purpose: To unparse given msl to s-expression.
;; Tests
;; (test (unparse-infix (parse-infix '(2 + 3))) '(2 + 3))
;; (test (unparse-infix (parse-infix '(2 - (3 * 4)))) '(2 - (3 * 4)))
;; (test (unparse-infix (parse-infix '((2 + 3) + (3 * 4)))) '((2 + 3) + (3 * 4)))

(define (unparse-infix (expr :  msl)) : s-expression
  (type-case msl expr
    (msl-num (n) (number->s-exp n))
    (msl-add (lhs rhs) (list->s-exp (list (unparse-infix lhs) (symbol->s-exp '+) (unparse-infix rhs))))
    (msl-mul (lhs rhs) (list->s-exp (list (unparse-infix lhs) (symbol->s-exp '*) (unparse-infix rhs))))
    (msl-sub (lhs rhs) (list->s-exp (list (unparse-infix lhs) (symbol->s-exp '-) (unparse-infix rhs))))
    (msl-exp (lhs rhs) (list->s-exp (list (unparse-infix lhs) (symbol->s-exp '**) (unparse-infix rhs))))))

(test (unparse-infix (parse-infix '(2 + 3))) '(2 + 3))
(test (unparse-infix (parse-infix '(2 - (3 * 4)))) '(2 - (3 * 4)))
(test (unparse-infix (parse-infix '((2 + 3) + (3 * 4)))) '((2 + 3) + (3 * 4)))


;;---------------------------------------------------------------------------------------

;;output-reverse-polish msl -> list of s-expression
;;output the msl as the reverse polish commands needed to evaluate it
;; template
;;(define (output-reverse-polish [expr : msl])
;; (type-case msl expr
;; [msl-num (n) ..]
;; [msl-add (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]
;; [msl-mul (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]
;; [msl-sub (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]
;; [msl-exp (lhs rhs)(... (output-reverse-polish lhs)(output-reverse-polish rhs))...]

;; examples
;; (msl-num 7) -> '(7)
;; (msl-add (msl-num 3) (msl-num 4)) -> '(4 3 +)
;; (msl-mul (msl-num 3) (msl-num 4)) -> '(4 3 *)
;; (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)) -> '(3 4 * 9 +)
;; (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9))) -> '(3 4 9 + *)


(define (output-reverse-polish [expr : msl])
  (type-case msl expr
    [msl-num (n) (list (number->s-exp n))]
    [msl-add (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '+)))]
    [msl-mul (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '*)))]
    [msl-sub (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '-)))]
    [msl-exp (lhs rhs) (append (append (output-reverse-polish lhs) (output-reverse-polish rhs)) (list (symbol->s-exp '**)))]))

(test (output-reverse-polish (msl-num 1)) (s-exp->list '(1)))
(test (output-reverse-polish (msl-num 2)) (s-exp->list '(2)))
(test (output-reverse-polish (msl-num 3)) (s-exp->list '(3)))
(test (output-reverse-polish (msl-num 4)) (s-exp->list '(4)))
(test (output-reverse-polish (msl-num 5)) (s-exp->list '(5)))
(test (output-reverse-polish (msl-add (msl-num 5) (msl-num 7))) (s-exp->list '(5 7 +)))
(test (output-reverse-polish (msl-add (msl-num 10) (msl-num 12))) (s-exp->list '(10 12 +)))
(test (output-reverse-polish (msl-add (msl-num 3) (msl-num 4))) (s-exp->list '(3 4 +)))
(test (output-reverse-polish (msl-sub (msl-num 4) (msl-num 5))) (s-exp->list '(4 5 -)))
(test (output-reverse-polish (msl-mul (msl-num 2) (msl-num 6))) (s-exp->list '(2 6 *)))
(test (output-reverse-polish (msl-mul (msl-num 10) (msl-num 2))) (s-exp->list '(10 2 *)))
(test (output-reverse-polish (msl-add (msl-num 3) (msl-num 4))) (s-exp->list '(3 4 +)))
(test (output-reverse-polish (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9))) (s-exp->list '(3 4 * 9 +)))
(test (output-reverse-polish (msl-sub (msl-num 5) (msl-add (msl-num 6) (msl-num 7)))) (s-exp->list '(5 6 7 + -)))
(test (output-reverse-polish (msl-add (msl-num 10) (msl-add (msl-num 1) (msl-num 2)))) (s-exp->list '(10 1 2 + +)))
(test (output-reverse-polish (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9))) (s-exp->list '(3 4 * 9 +)))
(test (output-reverse-polish (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9))) (s-exp->list '(3 4 * 9 +)))

"Example outputs"
(output-reverse-polish (msl-num 7))
(output-reverse-polish (msl-add (msl-num 3) (msl-num 4)))
(output-reverse-polish (msl-mul (msl-num 3) (msl-num 4)))
(output-reverse-polish (msl-add (msl-mul (msl-num 3) (msl-num 4)) (msl-num 9)))
(output-reverse-polish (msl-mul (msl-num 3) (msl-add (msl-num 4) (msl-num 9))))

"Parser -> reverse polish output" 
(output-reverse-polish (parse-prefix '(+ 99 (* 5 8))))
"Parser -> evaluation" 
(eval (parse-prefix '(+ 99 (* 5 8))))

;;----------------------- ASSINGMENT 2 -------------------------------

;; msl -> number
;; msl -> msl+msl
;; msl -> msl*msl
;; msl -> msl-msl
;; msl -> -msl
;; Alphabet: [number, +, *, - , ()]

;; purpose -> taking unary minus
;; msl = <number>
;; msl = (add <msl> <msl>)
;;       (bminusS <msl> <msl>)
;;       (mul <msl> <msl>)

;; tests
;;(numS 7)
;;(plusS (numS 5) (numS 5))
;;(multS (plusS (numS 3) (numS 4)) (numS 30))

(define-type ArithS
  [numS (n : number)]
  [plusS (lhs : ArithS) (rhs : ArithS)]
  [bminusS (lhs : ArithS) (rhs : ArithS)]
  [multS (lhs : ArithS) (rhs : ArithS)]
  [uminusS (e : ArithS)])

;; desugar function
;; translates ArithS values into msl ones.

;;(define (desugar [as : ArithS]) : msl
  ;;(type-case ArithS as
    ;;[numS (n) (...)]
    ;;[plusS (lhs rhs) (...)]
    ;;[multS (lhs rhs) (...)]
    ;;[bminusS (lhs rhs) (...)]
    ;;[uminusS (e) (...)]))

;; Tests
;; (test (desugar (numS 9)) (msl-num 9))
;; (test (desugar (plusS (msl-num 1)(msl-num 3)) (msl-add (desugar 1)(desugar 3))))
;; (test (desugar (bminusS ((msl-num 1)(msl-num 3))) (msl-add (desugar msl-num 1) (msl-mul (msl-num -1)(desugar msl-num 3)))))



"------------------------------------------------------------------------------------------------------------------"


(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [bminusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [uminusC (e : ExprC)])



(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])


;;(if test-expr expr expr)
;;(cond [test-expr expr] ...)
;;(cond [test-expr expr] ... [else expr])



;(type-case ExprC in
;  [numC (n) in]
;  [idC (s) (cond
;             [(symbol=? s for) what]
;             [else in])]
;  [appC (f a) (appC f (subst what for a))]
;  [plusC (l r) (plusC (subst what for l)
;                      (subst what for r))]
;  [multC (l r) (multC (subst what for l)
;                      (subst what for r))])

;[appC (f a) (local ([define fd (get-fundef f fds)])
;              (subst a
;                     (fdC-arg fd)
;                     (fdC-body fd)))]

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))
