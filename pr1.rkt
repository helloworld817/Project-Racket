#lang racket

(define database (make-parameter '()))
(define dummy (make-parameter '()))
(define future-ref (make-parameter '()))

(define band%
  (class object%
    ;;constructor
    (init-field band-name date time venue cost password)
    ;;add everything to the database 
    (begin
      (database
       (cons this (database))))
    ;;getter
    (define/public get-band (λ () band-name))
    (define/public get-pass (λ () password))
    (define/public get-date (λ () date))
    (define/public get-cost (λ () cost))
    (define/public get-venue (λ () venue))
    (define/public get-time (λ () time))

    ;;setters
    (define/public set-venue (λ (v) (set! venue v)))
    (define/public set-cost (λ (v) (set! cost v)))
    (define/public set-time (λ (v) (set! time v)))
    (define/public set-date (λ (v) (set! date v)))
    (define/public set-pass (λ (v) (set! password v)))
    
    (define/public (edit name new-venue new-cost new-pass new-time)
      (for-each (λ (b)
                  (cond ((string=? (send b get-band) name) (begin
                                                            
                          (if (not(string=? new-venue "")) (send b set-venue new-venue) (void))
                          (if (number? new-cost) (send b set-cost new-cost) (void))
                          (if (not(string=? new-time "")) (send b set-time new-time) (void))
                          (if (not (string=? new-pass "")) (send b set-pass new-pass) (void)))
                                                           (void))))

                  (database)))
    (super-new)))
  
(define people%
  (class object%
    (init-field name surename)

    (define/public (search name date cost venue time)

       (let ([result
              (filter
       (λ (b)
         (and
          (if (not (string=? name "")) (string=? (send b get-band) name) (void))
          (if (not (string=? date "")) (string=? (send b get-date) date) (void))
          (if (number? cost) (equal? (send b get-cost) cost) (void))
          (if (not (string=? venue "")) (string=? (send b get-venue) venue) (void))
          (if (not (string=? time "")) (string=? (send b get-time) time) (void))))
       (database))])
         (dummy (append (dummy) result))
         result))
    (define/public (add-item name date cost venue time)
      (let ([dumm
             (filter
              (λ (b)
                (and
                 (string=? (send b get-band) name)
                 (string=? (send b get-date) date)
                 (equal? (send b get-cost) cost)
                 (string=? (send b venue) venue)
                 (string=? (send b time) time)))
                (dummy))])
        (future-ref (append (future-ref) dumm))
        dumm))
        
                           
              
       
    (super-new)))

    
          

    
    
                                                        



(define p (new band% [band-name "A"] [date "12/09/2025"] [time "12"] [venue "London"] [cost 12] [password "123"]))
(define d (new band% [band-name "B"] [date "12/09/2025"] [time "12"] [venue "Paris"] [cost 800] [password "124"]))


(define user1 (new people% [name "ramtin"] [surename "lll"]))

(define result (send user1 search "" "12/09/2025" "" "" ""))
(send user1 add-item "B" "12/09/2025" "12" 800 "Paris")

(map (λ (b)
       (list
        (send b get-band)
        (send b get-date)
        (send b get-time)
        (send b get-cost)
        (send b get-venue)))
     (future-ref))
       
            
             



           



