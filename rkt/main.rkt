#lang typed/racket

(struct built-in-type ([type : Symbol]) #:transparent)
(struct atom-symbol ([sym : Symbol]) #:transparent)
(struct the ([type : Expr] [expr : Expr]) #:transparent)

(define built-in-type-Atom (built-in-type 'Atom))

(define-type Variable Symbol)

(define-type Context (HashTable Variable Expr))

(define-type Expr (U the
                     atom-symbol
                     built-in-type))

;; Synthesizes a type for `expr`.
;; Returns a `the` expression.
(: synth (-> Expr Context Expr))
(define (synth expr context)
  (match expr
    ;; Rule: [The]
    [(the X expr)
     (define X^ (elaborate-type X))
     (define expr^ (check-type expr X^ context))
     (the X^ expr^)]

    ;; Rule: [Hypothesis]
    [(? symbol? x)
      (the (lookup context x) x)]
    
    ;; Rule: [AtomI]
    [(atom-symbol sym)
      (the built-in-type-Atom expr)]

    [_ (error 'synth "Unsupported expression.")]))

(define (elaborate-type type-expr)
  (match type-expr
    ;; Rule: [AtomF]
    [(built-in-type 'Atom)
     (built-in-type 'Atom)]))

(: check-type (-> Expr Expr Context Expr))
(define (check-type expr type context)
  ;; Rule: [Switch]
  (define synth-expr (synth expr context))
  (match synth-expr
    [(the type^ expr^)
      (when (not (same-type? type type^))
        (error 'check-type "Not same type."))
      
      expr^]))

(: same? (-> Expr Expr Expr Boolean))
(define (same? expr1 expr2 type)
  ;; TODO:
  ;;  - Should follow rules: [SameSymm], [SameTrans], [HypothesisSame], [AtomSame-Tick]

  (match* (expr1 expr2 type)
    ;; Rule: [TheSame]
    [((the X e) _ X)
      (same? e expr2 type)]
    [(_ _ _) (error 'synth "Unsupported expression.")]))

(define (same-type? type1 type2)
  (displayln "TODO")
  ;; TODO:
  ;;  - Should follow rules: [AtomSame-Atom]
  #t)

(: lookup (-> Context Variable Expr))
(define (lookup context x)
  ;; Rules: [LookupStop] and [LookupPop]
  (if (hash-has-key? context x)
      (hash-ref context x)
      (error 'lookup "Unbound variable.")))


(define (context? ctx)
  ;; TODO: (might not be needed)
  ;; Rules: [EmptyCtx] and [ExtCtx]
  #t)


;; Testing:

(define p1
  '((claim + (-> Nat Nat Nat))
    (define +
      (lambda (x y)
        (iter-Nat
          x
          y
          (lambda (b)
            (add1 b)))))))

(pretty-print
  (pretty (synth (the built-in-type-Atom (atom-symbol 'hello)) (hash))))

(pretty-print
  (pretty (synth (atom-symbol 'hello) (hash))))

(: pretty (-> Expr Any))
(define (pretty e)
  (match e
    [(the e1 e2)
      `(the ,(pretty e1) ,(pretty e2))]
   [(built-in-type t) t]
   [(atom-symbol sym)
    `',sym]))