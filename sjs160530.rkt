#lang racket
(provide (all-defined-out))
#|
      Title:

            Programming Assignment #1
            Functional Programming Racket

      Author:
            Stuart Small
            Computer Science Undergraduate
            sjs160530@utdallas.edu

      Description:
            This file contains multiple procedures that perform seperate tasks.

       Date:
            10/03/2019
|#

;; 1 .Procedure that takes an integer as an argument and returns a function that indicates
;; whether its integer argument is evenly divisible by the first.
(define (divisible-by-x? x)
  (lambda (num)
    (if (= 0 ( modulo num x))
        #t
        #f)))


;; 2. Procedure that takes a function as an argument and passes the number 9 to that function.
(define function-9
  (lambda (num)
    (num 9)))


;; 3. Procedure that duplicates the functionality of map from the standard library recursively
(define (my-map procedure lst)
  (cond
    ;check for empty lst
    [(empty? lst) empty]
    ;get first value from list and append to lst
        [else (cons (procedure (first lst))
                    ;call for remaining list
                    (my-map procedure (rest lst)))]))


;; 4. Procedure that takes two lists as arguments and returns a single list of pairs.
;; The first pair should be both first elements from the respective lists.
;; The second should be the second element from the respective lists, and so on.
(define (pair-up lst1 lst2)
  ;lst1 empty
  (cond ((null? lst1) '())
        ;lst2 empty
        ((null? lst2) '())
        (else
         ;get first element from both lists
         (cons (list (car lst1) (car lst2))
               ;returns the first item of list composed of lst1 and lst2 first elements
               (pair-up (cdr lst1)(cdr lst2))))))

;; 5. Function that takes a procedure that executes a boolean test on an atomic value and a list of elements as argumets. It should return
;; a list containing two sublists, the first sublist containing the elemetnts from the original list in which each member passes the Boolean test as True,
;; and the second sublist containing the elements from the original list in which each member passes the boolean test as false.
(define (separate proc lst)
  (list (get-valid-by-proc proc lst)(get-false-by-proc proc lst)))
      

(define (get-valid-by-proc proc lst)
  (if (empty? lst)
     '()
     (if (proc (car lst))
         (cons (car lst)(get-valid-by-proc proc (cdr lst)))
         (get-valid-by-proc proc (cdr lst)))))

(define (get-false-by-proc proc lst)
  (if (empty? lst)
      '()
      (if (not (proc (car lst)))
         (cons (car lst)(get-false-by-proc proc (cdr lst)))
         (get-false-by-proc proc (cdr lst)))))
  
  
  

;; 6. Function that takes two arguments, a list and an expression, which may be  atomic or a list. Function should return true (#t) if the element is a member of
;; the list and false (#f) if it does not.
(define (is-member? atom lst)
  (if(empty? lst)
     #f
  (if (equal? atom (car lst))
      #t
      (is-member? atom (cdr lst)))))



;; 7. Function that takes two arguments-- a comparision function and a list. It should return a boolean (i.e. #t or #f) indicating whether the list is sorted
;; according to the comparision function.
(define (my-sorted? comp lst)
  (if (eq? comp <)
      (numCompare lst)
      (stringCompare lst)))

(define (numCompare lst)
  (if (<= (length lst)1)
      #t
      (and (<= (car lst)(cadr lst))(numCompare(cdr lst)))))

(define (stringCompare lst)
  (if (<= (length lst)1)
      #t
      (and (string<=?(car lst)(cadr lst))(stringCompare(cdr lst)))))

;; 8. Function that duplicates the functionality of flatten from the standard library
(define (my-flatten lst)
  (cond ((null? lst)'())
        ((pair? lst)
         (append (my-flatten(car lst))(my-flatten(cdr lst))))
        (else (list lst))))

;; 9. Function that takes two arguments, a list of numbers and a single number (the threshold). It should return a new list that has the same number as the input list,
;; but  with all elements greater than the threshhold number removed.
(define (upper-threshold lst threshold)
  (cond ((null? lst) '())
        ((< (car lst) threshold)
         (cons (car lst)
               (upper-threshold (cdr lst) threshold)))
        (else (upper-threshold (cdr lst) threshold))))


;; 10. Procedure that duplications the functionality of list-ref from the standard library
(define (my-list-ref lst n)
  (cond ((null? lst)(error "ERROR: Index out of bounds"))
        ((= n 0)(car lst))
        (else (my-list-ref (cdr lst) (- n 1)))))
                    




  