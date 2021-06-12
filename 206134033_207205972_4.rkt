#lang pl

#| role : we do all the assianment together |#

;;-----------------------------------------------Part A------------------------------------------------------
 
;; ---------------------------------------------Question 1---------------------------------------------------

#| This question was not difficult for us and took us an average of 2 minutes

<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>} ;;intersect between 2 SOLS
        |  { union <SOL> <SOL> } ;; union between 2 SOLS
        |  <id>
        |  { with {<id> <SOL>  <id> <SOL>} <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

        |  True
        |  False ;; false is SOL too caz its in the valid expression 
        | { if <SOL> then <SOL> else <SOL> }
        | { equal? <SOL> <SOL> }

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;;This question was not difficult for us and took us about 2 minutes

;; The abstract syntax tree SOL
(define-type SET = (Listof Number))

(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET] ;; Set is a list of numbers so he got Set
  [Smult Number SOL] ;; Smul- miltiply by scalar so he got number and SOL
  [Inter SOL SOL] ;;Inter is a binary operator so he needs to get 2 SOL
  [Union SOL SOL] ;;Union is a binary operator so he needs to get 2 SOL
  [Id    Symbol]
  ;;    [With  Symbol Symbol SOL SOL SOL] -- not to be used, syntactic sugar for ...
  [Fun   Symbol Symbol SOL] ;; Fun gets 2 symbols and body
  [CallS SOL SOL SOL] ;; (CallS(Fun (name1 name2) parse-expr(body)) parse-expr(named1) parse-expr(named2))  
  [CallD SOL SOL SOL] ;; (CallD(Fun (name1 name2) parse-expr(body)) parse-expr(named1) parse-expr(named2))
  [Bool Boolean] ;; True/False
  [If SOL SOL SOL] ;; (If (condition-SOL1) (do SOL2-if the condition exist) (do SOL3-if the condition not exist) 
  [Equal SOL SOL])


;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5))))
(test (not (ismember? 1 '( 3 2 3 5 6))))
(test (ismember? 1 '(3 4 5 1 3 4)))
(test (ismember? 1 '(1)))



(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
        [else (cons (first l) (remove-duplicates (rest l)))]))

(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '()) => '())
(test (remove-duplicates '(3 4 5 1 4 3 7)) => '(5 1 4 3 7))



(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <)))

(test (create-sorted-set '(3 4 5)) => '(3 4 5))
(test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
(test (create-sorted-set '()) => '())
(test (create-sorted-set '(3 2 3 5 9 2 6)) => '(2 3 5 6 9))



(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B)))

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5)) 
(test (set-union '(3 4 5) '()) => '(3 4 5))
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))
(test (set-union '(3 4 5) '(1 2)) => '(1 2 3 4 5))
(test (set-union '(3 4 1) '(1 2)) => '(1 2 3 4))
(test (set-union '(3 4 1) '(1 2 1)) => '(1 2 3 4))
(test (set-union '(1 2 3) '(4 2 3)) => '(1 2 3 4))



(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '(3 4 6) '(3 4 5)) => '(3 4))
(test (set-intersection '(3 4 6) '(6 3 4 5)) => '(3 4 6))
(test (set-intersection '(4 3 6 4) '(6 3 4 5)) => '(3 4 6))
  


;; -----------------------------------Question 2------------------------------------------
;; Parser
#|
Input : Sexpr
output : SOL
This function receives Sexpr and converts it to SOL.
First we check the Sexpr we received and second we send it to the appropriate constructor
This question was not difficult for us and took us an average of half hour.
|#

(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
    ['True (Bool true)] ;; true constructor
    ['False (Bool false)] ;; false constructor
    [(symbol: name) (Id name)]
    [(cons 'with more) ;; if we have a pair that his first element is the word 'with and the second elem is something
       (match sexpr ;; we need to check this sexpr
         [(list 'with (list (symbol: name1) named1 (symbol: name2) named2) body)
          (CallS (Fun name1 name2 (parse-sexpr body)) (parse-sexpr named1) (parse-sexpr named2))] ;;; There is no With constructor. Replace it with existing constructors...
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])] ;;invalid sexpr 
    [(cons 'fun more);; if we have a pair that his first element is the word 'fun and the second elem is something
       (match sexpr ;; we need to check this sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body) ;; valid sexpr 
          (if (equal? name1 name2)
              (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice
              (Fun name1 name2 (parse-sexpr body)))] ;; if name1 and name2 not equal
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])] ;; invalid sexpr    
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))];;calling 'Smult' cons with sc and send rhs to 'parse-sexprS'  
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))];; calling 'Inter' const and send lhs,rhs to 'parse-sexprS' 
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))];; calling 'Union' const and send lhs,rhs to 'parse-sexprS'
    [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'if cond 'then true-cond 'else false-cond) (If (parse-sexpr cond) (parse-sexpr true-cond) (parse-sexpr false-cond))] 
    [(list 'equal? sol1 sol2) (Equal (parse-sexpr sol1) (parse-sexpr sol2))] 
    [else (error 'parse-sexpr "parse-sexpr: bad `with' syntax in ~s" sexpr)]))


(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

  
;;; Tests for parse
 
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'c
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Set '())))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} S1 {union {1 2 3} {4 2 3}}}
                          {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters
(test (parse "True") => (Bool true))
(test (parse "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") =>
      (If (Equal (Set '(1 2 3)) (Set '(1 2))) (Set '(1 2 3)) (Set '(1 2))))
(test (parse "False") => (Bool false))
(test (parse "{fun {x y} z y}") =error> "parse-sexpr: bad `fun' syntax in (fun (x y) z y)")
(test (parse "{piter x y r}") =error> "parse-sexpr: bad `with' syntax in (piter x y r)")
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}
               S1
               {union {1 2 3} {4 2 3}}
               {union {1 2 3} {4 2 3}}}
                          {fun {x y} z}}")
      =error> "parse-sexpr: bad `with' syntax in (with (S (intersect (1 2 3) (4 2 3)) S1 (union (1 2 3) (4 2 3)) (union (1 2 3) (4 2 3))) (fun (x y) z))")
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-dynamic {fun {x y} {union x S}}
                               {if {equal? S {scalar-mult 3 S}}
                                   then S
                                   else {4 5 7 6 9 8 8 8}}
                               {}}}")
=> (CallS (Fun 'S 'c
               (CallD (Fun 'x 'y (Union (Id 'x) (Id 'S)))
                      (If (Equal (Id 'S) (Smult 3 (Id 'S)))
                          (Id 'S)
                          (Set '(4 5 6 7 8 9)))
                      (Set '())))
          (Inter (Set '(1 2 3)) (Set '(2 3 4)))
          (Set '())))
(test (parse "{with {shani and yarden} check}") =error> "parse-sexpr: bad `with' syntax in (with (shani and yarden) check")


;;-----------------------------------------------Part B------------------------------------------------------
 
;; ---------------------------------------------Question 3---------------------------------------------------
;; Evaluation 
#|
This question took us an average of 2 hours, the dificult was to understand the second extend of callD/S

Evaluation rules:
    eval({ N1 N2 ... Nl })      = sort( create-set({ N1 N2 ... Nl })) ;; where create-set removes all duplications from
                                                                         the sequence (list) and sort is a sorting procedure

    eval({scalar-mult K E})     = { K*N1 K*N2 ... K*Nl }              ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2})     = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})         = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({fun {x1 x2} E},env)   = <{fun {x1 x2} E}, env>

;; we send Ef(body fun) to eval. extend gets 3 parameters (id,VAL,ENV) and id=x2, VAL=eval(E2,env),ENV=extend(x1,eval(E1,env),envf)  
    eval({call-static E-op E1 E2},env)                                                             ;; ENV has constructor 'extend'
                                = eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),envf)))
                                                      if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
                                = error!              otherwise

;; we send Ef(body fun) to eval. extend gets 3 parameters (id,VAL,ENV) and id=x2, VAL=eval(E2,env),ENV=extend(x1,eval(E1,env),env) 
    eval({call-dynamic E-op E1 E2},env)
                                = eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),env)))
                                                      if eval(E-op,env) = <{fun {x1 x2} Ef}, envf> ;; 
                                = error!              otherwise

    eval(True,env)              = true
    eval(False,env)             = false
    eval({if E1 then E2 else E3},env)
                                = eval(E3, env)       if eval(E1,env) = false
                                = eval(E2, env)     otherwise ;; otherwise- E1=true then eval(E2, env)

    eval({equal? E1 E2},env)    = true                if eval(E1,env) is equal in content to eval(E2,env)
                                = false               otherwise

|#


;; ---------------------------------------------Question 4---------------------------------------------------

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV]) ;; extend the environment with id,value,env

(define-type VAL
  [SetV SET]
  [FunV Symbol Symbol SOL ENV]
  [BoolV Boolean]) ;; true/false

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 


#|
Input : VAL 
output : SET
This function returns SET. this function checks if the VAL is SetV type, if yes- return the set S,
else- throw an error (if we got Boolv/ Funv we throw error).
|# 
(: SetV->set : VAL -> SET)
(define (SetV->set v)
  (cases v
    [(SetV S) S]
    [else (error 'SetV->set "expects a set, got: ~s" v)]))

(test (SetV->set (FunV 'x 'y (Id 'x) (EmptyEnv))) =error> "SetV->set: expects a set")

#|
Input : Number VAL 
output : VAL
This function returns VAL. this function multiplies the entire SET (s) by the number (n) it received.
The 'mult-op' function performs a multiply of two numbers, we call map function that operate the 'mult-op'
function on every element in the set.
s originally in type VAL and we convert it to SET using 'SetV->set' function.
At the end we convert our result 'afterMul' to type VAL (SetV constructor) caz 'smult-set' return VAL   
This question took us an average of 30 minutes, the dificult was how to use 'map' function. 
|#
(: smult-set : Number VAL -> VAL)
(define (smult-set n s)
  (: mult-op : Number -> Number)
  (define (mult-op k)
    (* k n))
  (let([afterMul (map mult-op (SetV->set s))])
   (SetV afterMul)))

(test (smult-set 3 (SetV '(3 4 5))) => (SetV '(9 12 15)))
(test (smult-set -3 (SetV '(3 4 5))) => (SetV '(-9 -12 -15)))
 

#|
Input : (SET SET -> SET) VAL VAL 
output : VAL
This function returns VAL. this function gets binary SET operator(union/intersection) so the input
of this binary operator should be this caz its work on 2 SETS.
This question took us an average of 5 minutes.
|#
(: set-op : (SET SET -> SET) VAL VAL -> VAL)
;; gets a binary SET operator, and uses it within a SetV
;; wrapper
(define (set-op op val1 val2)
  (SetV (op (SetV->set val1) (SetV->set val2))))



;; ---------------------------------------------Question 5---------------------------------------------------

;;---------  the eval procedure ------------------------------

#|
Input : SOL ENV
output : VAL
This function returns VAL. it calculates the SOL according to the constructor
(an explanation of each possible constructor pattern is written in the code).
This question took us an average of 45 minutes.
|#
(: eval : SOL ENV -> VAL)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr env)
  (cases expr
    [(Set S) (SetV (create-sorted-set S))]
    [(Smult n set) (smult-set n (eval set env))] ;; calling 'smult-set' function with n and set
    [(Inter l r) (set-op set-intersection (eval l env) (eval r env))] ;;set-intersection is the first argument in 'set-op' and after that we send 2 VALS 
    [(Union l r) (set-op set-union (eval l env) (eval r env))] ;; same as Inter
    [(Id name) (lookup name env)]
    [(Fun bound-id1 bound-id2 bound-body)
     (FunV bound-id1 bound-id2 bound-body env)]
    [(CallS fun-expr arg-expr1 arg-expr2) ;; if we got CallS expression so-
     (let ([fval (eval fun-expr env)]) ;; we calculate the body of the fun('fun-expr') and save it with 'fval'
       (cases fval ;; checking the type of 'fval'
         [(FunV bound-id1 bound-id2 bound-body f-env);;if 'fval' is 'FunV' type- we do the next line
          (eval bound-body (Extend bound-id2 (eval arg-expr2 env) (Extend bound-id1 (eval arg-expr1 env) f-env)))]
         [else (error 'eval "`call-static' expects a function, got: ~s" ;; 'fval' is not 'FunV' type
                      fval)]))] 
    [(CallD fun-expr arg-expr1 arg-expr2);; if we got CallD expression so-
     (let ([fval (eval fun-expr env)]);; we calculate the body of the fun('fun-expr') and save it with 'fval'
       (cases fval;; checking the type of 'fval'
         [(FunV bound-id1 bound-id2 bound-body f-env);;if 'fval' is 'FunV' type- we do the next line
          (eval bound-body (Extend bound-id2 (eval arg-expr2 env) (Extend bound-id1 (eval arg-expr1 env) env)))]
         [else (error 'eval "`call-dynamic' expects a function, got: ~s" ;; 'fval' is not 'FunV' type
                      fval)]))]
    [(Bool b) (BoolV b) ] ;; calling BoolV constructor 
    [(If cond true-cond false-cond);; if we got If expression so-
     (let ([cval (eval cond env)]);; we calculate the 'cond' and save it with 'cval'
       (cases cval ;; checking the type of 'cval'
         [(BoolV b) (if(equal? #t b) (eval true-cond env) (eval false-cond env))] ;; b is a Boolean value 
         [else (error 'eval "`if' expects a function, got: ~s"  
                      cval)]))]  
    [(Equal l r) (if (equal? (eval l env) (eval r env)) (BoolV #t) (BoolV #f))]))

;; Tests
(test (eval (CallD (Set '(6 7)) (Set '()) (Set '())) (EmptyEnv)) =error> "`call-dynamic' expects a function, got: #(struct:SetV (6 7))")


;; ---------------------------------------------Question 6---------------------------------------------------

#|
Input : nothing
output : ENV
This function returns ENV, it create a non empty global environment.
We used Extend constructor that gets 3 parameters (symbol, VAL, ENV), so the symbol are 'second/'first/'cons
and the VAL are FunV (id1 id2 SOL(using CallS) EmptyEnv) and finally ENV is EmptyEnv.
This question took us an average of one hour.
|#
(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend 'second (FunV 'x 'y (CallS (Id 'x) (Fun 'frst 'scnd (Id 'scnd)) (Set '())) (EmptyEnv))
          (Extend 'first (FunV 'x 'y (CallS (Id 'x) (Fun 'frst 'scnd (Id 'frst)) (Set '())) (EmptyEnv))
          (Extend 'cons (FunV 'x 'y (Fun 'slctr1 'slctr2 (CallS (Id 'slctr1) (Id 'x) (Id 'y))) (EmptyEnv))  
                          (EmptyEnv)))))

#|
Input : String
output : (U SET VAL Boolean)
This function run the entire program, we call 'eval' function with our string(str) and 
the non empty global environment(createGlobalEnv).  
This question took us an average of 15 minutes.
|#
(: run : String -> (U SET VAL Boolean))
;; evaluate a SOL program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result ;; checking result's type 
      [(SetV S) S] ;;if its SetV type we return S
      [(BoolV b)  b ] ;;if its BoolV type we return b
      [else result]))) ;;if its FunV type we return result
 
 ;; b is SOL we wont convert him to boolean


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))


(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
               {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
                    S1 {}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}
                     S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}
                   S1 {}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}
                     S1 {}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (run "True") => #t)
(test (run "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") => '(1 2))
(test (run "{equal? {union {1 2 3} {4 2 3}} {1 2 3 4}}") => #t)
(test (run "{union {equal? {4} {4}} {4 2 3}}") =error> "SetV->set: expects a set, got: #(struct:BoolV #t)")
(test (run "x") =error> "lookup: no binding for x")
(test (run "{if {equal? {1 2 3} {1 2 3}} then {1 2 3} else {1 2}}") => '(1 2 3))
(test (run "{fun {x y} x}") =>(FunV 'x 'y (Id 'x)
                                    (Extend 'second (FunV 'x 'y (CallS (Id 'x) (Fun 'frst 'scnd (Id 'scnd)) (Set '())) (EmptyEnv))
                                            (Extend 'first (FunV 'x 'y (CallS (Id 'x) (Fun 'frst 'scnd (Id 'frst)) (Set '())) (EmptyEnv))
                                                    (Extend 'cons (FunV 'x 'y (Fun 'slctr1 'slctr2 (CallS (Id 'slctr1) (Id 'x) (Id 'y))) (EmptyEnv))
                                                            (EmptyEnv))))))



;;-----------------------------------------------Part C------------------------------------------------------

#|
1. What are the types that we have now (after you are done) in the SOL language?
   Answer- the types we have are: (SET), (Symbol Symbol SOL ENV), (Boolean) 

2. Explain where in the solution of section 2 (when parsing with expressions)
   you called a function dynamically/statically – what was the importance of your choices?
   Answer-**********************************************************************************************

3. Explain where in the solution of section 6 you used call-dynamic and
   where you used call-static – what was the importance of your choices?
   Answer- we used call-static in 'cons, 'first and 'second.
   The call for 'cons can be call-dynamic/call-static caz its will be called on first or second which is
   in the same clousure that they override.
   The call for 'first and 'second must be call-static caz if we used call-dynamic it was running in the
   global environment and was not recognized the id's hence we would get an exception(caz the 'first/'second
   will be called when the clouser will be finished.

4. Would there be any difference if we used call-dynamic in some places in
   the following test? Explain your answer.
(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
       S1 {}}
     {with {S {intersect {call-static first p {}}
                         {call-static second p {}}}
            S1 {}}
       {call-static {fun {x y} {union x S}}
                    {scalar-mult 3 S}
                    {4 5 7 6 9 8 8 8}}}}")
   => '(2 3 6 9)
   Answer- 





|#
