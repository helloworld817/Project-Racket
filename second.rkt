#lang racket


(struct band (name pass date time venue cost status) #:mutable #:transparent)
(struct fans (name password))


(define b-1 (band "A" "123" "12/09/2025" "12" "London" 800 "available"))
(define b-2 (band "B" "456" "20/09/2025" "20" "Paris" 400 "available"))
(define b-3 (band "c" "123" "12/09/2025" "12" "London" 800 "available"))
(define list-concerts (list b-1 b-2 b-3))
(define selected-conc (make-parameter '()))

(define edit (λ (name pass date1 time1 venue1 cost1 status1) (for ([i list-concerts]) (cond
                                                                                        ((and(string=? name (band-name i)) (string=? pass (band-pass i))) (begin
                                                                                                                                                            (set-band-date! i date1)
                                                                                                                                                            (set-band-time! i time1)
                                                                                                                                                            (set-band-venue! i venue1)
                                                                                                                                                            (set-band-cost! i cost1)
                                                                                                                                                            (set-band-status! i status1)))))))



(define search (λ (name date time venue cost status) (filter (λ (b) (or (equal? name (band-name b)) (equal? date (band-date b)) (equal? time (band-time b)) (equal? venue (band-venue b))
                                                                        (equal? cost (band-cost b)) (equal? status (band-status b)))) list-concerts)))


(define add (λ (name date time venue cost status) (for ([i list-concerts]) (cond
                                                                             ((and (equal? name (band-name i)) (equal? date (band-date  i)) (equal? time (band-time i)) (equal? venue (band-venue i))
                                                                                  (equal? cost (band-cost i)) (equal? status (band-status i))) (selected-conc (cons i (selected-conc))))))))


(define remove (λ (name date time venue cost status) (selected-conc
     (filter (λ (b)
               (not (and (equal? name (band-name b))
                         (equal? date (band-date b))
                         (equal? time (band-time b))
                         (equal? venue (band-venue b))
                         (equal? cost (band-cost b))
                         (equal? status (band-status b)))))
             (selected-conc)))))
                                                                  
                                                               
                                                               
               