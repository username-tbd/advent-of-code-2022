#lang scheme

(define cal-strs ; ("11" "" "7")
  (string-split
    (file->string "../inputs/input-01.txt")
    "\n"))

(define strlist->intlist ; ("11" "" "7") -> (11 () 7)
  (lambda (l)
    (cond
      ((null? l) '())
      ((string=? "" (car l)) (cons '() (strlist->intlist (cdr l))))
      (else (cons (string->number (car l)) (strlist->intlist (cdr l)))))))

(define aggregate-cals
  (lambda (l acc)
    (cond
      ((null? l) (reverse acc))
      ((null? (car l)) (aggregate-cals (cdr l) (cons 0 acc)))
      (else (aggregate-cals (cdr l)
                      (cons (+ (car l) (car acc))
                            (cdr acc)))))))

(define cals-agg
  (sort
    (aggregate-cals (strlist->intlist cal-strs) '(0))
    >))

(car cals-agg)
(apply + (take cals-agg 3))
