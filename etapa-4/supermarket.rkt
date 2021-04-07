#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue closed) #:transparent)

(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0  empty-queue 0))


(define (update f counters index)
  (map (λ (x) (if (equal? (counter-index x) index) (f x) x))
       counters))


(define tt+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue closed)
         (struct-copy counter C [tt (+ minutes (counter-tt C))])]
        )
      )
    ))


(define et+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue closed)
         (struct-copy counter C [et (+ minutes (counter-et C))])]
        )
      )
    ))


(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
      [(counter index tt et queue closed)
       (struct-copy counter C
                    [tt (+ tt items)]
                    [et (if (queue-empty? queue)
                            (+ et items)
                            et)]
                    [queue (enqueue (cons name items) queue)])]
      )))


(define (min-helper min-index min-value counters f)
  (if (null? counters)
      (cons min-index min-value)
      
      (if (< (f (car counters)) min-value)
          (min-helper (counter-index (car counters)) (f (car counters)) (cdr counters) f)
          (min-helper min-index min-value (cdr counters) f))))

(define (min-tt counters)
  (min-helper -1 999999999 counters counter-tt)) ; folosind funcția de mai sus

(define (min-et counters)
  (min-helper -1 999999999 counters counter-et)) ; folosind funcția de mai sus



(define (remove-first-from-counter C)   ; testată de checker

  (if (<= (+ (queue-size-l (counter-queue C)) (queue-size-r (counter-queue C))) 1)
      
      (struct-copy counter C
                   [tt 0]
                   [et 0]
                   [queue empty-queue])
      
      (struct-copy counter C [tt (- (counter-tt C) (counter-et C))]
                   [et (cdr (top (dequeue (counter-queue C))))]
                   [queue (dequeue (counter-queue C))]
                   )))


(define (pass-time-through-counter minutes)
  (λ (C)
    (match C [(counter index tt et queue closed)
              (struct-copy counter C [tt (if (< minutes tt) (- tt minutes) 0)]
                           [et (if (< minutes et) (- et minutes) 0)])]
      )))


; Functie care returneaza daca un element se afla intr-o lista
(define (member? x list)
  (if (null? list) #f                               
      (if (equal? x (car list)) #t                  
          (member? x (cdr list)))))                 


; Functie care intarzie o casa C cu minutes
(define (delay C minutes)
  (match C
    [(counter index tt et queue closed)
     (struct-copy counter C
                  [tt (+ (counter-tt C) minutes)]
                  [et (+ (counter-et C) minutes)])]))


; Functie care adauga case noi pana la atingerea unei anumite medii
(define (add-slow-counters average slow-counters fast-counters)
  (let* ((num-counters (length (filter (λ (C) (zero? (counter-closed C)))
                                       (append fast-counters slow-counters))))
         (avg_actual (/ (apply + (map (λ (C) (counter-tt C))
                                      (filter (λ (C) (zero? (counter-closed C)))
                                              (append fast-counters slow-counters)))) num-counters))
         (num-total (length (append fast-counters slow-counters))))
     
    (if (<= avg_actual average)
        slow-counters
        (add-slow-counters average (append slow-counters (list (empty-counter (+ 1 num-total)))) fast-counters)
        )
    )
  )


; Functie care "trece timpul" printr-o casa si scoate clientii corespunzatori
(define (pass-time-remove x)
  (λ (C)
    (let it ([x x] [C C])
      (if (= x 0)
          C
          (it (sub1 x) (if (= (counter-et C) 1)
                           (remove-first-from-counter C)
                           ((pass-time-through-counter 1) C)
                           )
              )
          )
      )
    )
  )


; Functie care returneaza o lista cu clientii care ies de la o anumita casa
(define removed-clients
  (λ (C)
    (if (and (= (counter-et C) 1) (not (queue-empty? (counter-queue C))))
        (list (cons (counter-index C) (car (top (counter-queue C)))))
        '())
    )
  )


; Functie care adauga intr-o lista clientii care au iesit de la case dupa un numar de minute
(define (dequeue-clients counters x)
  (let it ([x x] [counters counters])
    (if (= x 0)
        '()
        (append (foldr append '() (map removed-clients counters))
                (it (sub1 x) (map (pass-time-remove 1) counters)))
        )
    )
  )

; Functie care inchide o casa
(define (close-counter C)
  (match C
    [(counter index tt et queue closed)
     (struct-copy counter C
                  [closed 1]
      )]))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))


(define (serve-helper requests fast-counters slow-counters crono-list)
  (if (null? requests)
      
      (cons crono-list (map (λ (C) (cons (counter-index C) (counter-queue C)))
                            (filter (λ (C) (not (queue-empty? (counter-queue C))))
                                    (append fast-counters slow-counters))))
      
      (match (car requests)
        [(list 'close index)
         (serve-helper (cdr requests) (update (λ (C) (close-counter C)) fast-counters index)
                       (update (λ (C) (close-counter C)) slow-counters index) crono-list)]

        
        [(list 'ensure average)
         (serve-helper (cdr requests) fast-counters (add-slow-counters average slow-counters fast-counters) crono-list)]

        
        [(list name n-items)
         (define mintt_idx (car (min-tt (filter (λ (C) (zero? (counter-closed C))) (append fast-counters slow-counters)))))
         (define mintt_idx_slow (car (min-tt (filter (λ (C) (zero? (counter-closed C))) slow-counters))))
         
         (if (and (<= n-items ITEMS) (member? mintt_idx (map (λ (C) (counter-index C)) fast-counters)))
             (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters mintt_idx) slow-counters crono-list)
             (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters mintt_idx_slow) crono-list))]

        
        [(list 'delay index minutes)
             (serve-helper (cdr requests) (update (λ (C) (delay C minutes)) fast-counters index)
                           (update (λ (C) (delay C minutes)) slow-counters index) crono-list)]

        
        [x
          (define counters (append fast-counters slow-counters))
          (serve-helper (cdr requests) (map (pass-time-remove x) fast-counters)
                        (map (pass-time-remove x) slow-counters)
                        (append crono-list (dequeue-clients (append fast-counters slow-counters) x)))]
        )))
