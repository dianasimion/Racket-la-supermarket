#lang racket
(require racket/match)
(require "queue.rkt")
(require racket/trace)	

(provide (all-defined-out))

(define ITEMS 5)


;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))


(define (update f counters index)
  (map (λ (x) (if (equal? (counter-index x) index) (f x) x))
       counters))


(define tt+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [tt (+ minutes (counter-tt C))])]
        )
      )
    ))


(define et+
  (λ (minutes)
    (λ (C)
      (match C
        [(counter index tt et queue)
         (struct-copy counter C [et (+ minutes (counter-et C))])]
        )
      )
    ))


(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
      [(counter index tt et queue)
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
      
      (struct-copy counter C [tt 0]
                   [et 0]
                   [queue empty-queue])
      
      (struct-copy counter C [tt (- (counter-tt C) (counter-et C))]
                   [et (cdr (top (dequeue (counter-queue C))))]
                   [queue (dequeue (counter-queue C))]
                   )))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (match C [(counter index tt et queue)
              (struct-copy counter C [tt (if (< minutes tt) (- tt minutes) 0)]
                           [et (if (< minutes et) (- et minutes) 0)])]
      )))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

; Functie care returneaza daca un element se afla intr-o lista
(define (member? x list)
  (if (null? list) #f                               
      (if (equal? x (car list)) #t                  
          (member? x (cdr list)))))                 


; Functie care intarzie o casa C cu minutes
(define (delay C minutes)
  (match C
    [(counter index tt et queue)
     (struct-copy counter C
                  [tt (+ (counter-tt C) minutes)]
                  [et (+ (counter-et C) minutes)])]))


; Functie care adauga case noi pana la atingerea unei anumite medii
(define (add-slow-counters average slow-counters fast-counters)
  (let* ((num-counters (length (append fast-counters slow-counters)))
         (avg_actual (/ (apply + (map (λ (C) (counter-tt C))
                                      (append fast-counters slow-counters))) num-counters)))
     
    (if (<= avg_actual average)
        slow-counters
        (add-slow-counters average (append slow-counters (list (empty-counter (+ 1 num-counters)))) fast-counters)
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
  (lambda (C)
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
       (append (foldr append '()
                      (map removed-clients counters)) (it (sub1 x) (map (pass-time-remove 1) counters)))
    )
   )
  )


(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))


(define (serve-helper requests fast-counters slow-counters crono-list)
  (if (null? requests)
      (cons crono-list (append fast-counters slow-counters))
      
      (match (car requests)
        
        [(list 'ensure average)
         (serve-helper (cdr requests) fast-counters (add-slow-counters average slow-counters fast-counters) crono-list)
         ]
        
        [(list name n-items)
         (define mintt_idx (car (min-tt (append fast-counters slow-counters))))
         
         (if (and (<= n-items ITEMS) (member? mintt_idx (map (λ (C) (counter-index C)) fast-counters)))
             (serve-helper (cdr requests) (update (add-to-counter name n-items) fast-counters mintt_idx) slow-counters crono-list)
             (serve-helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) crono-list)
             )]

        [(list 'delay index minutes)
         (if (member? index (map (λ (C) (counter-index C)) fast-counters))
             (serve-helper (cdr requests) (update (λ (C) (delay C minutes)) fast-counters index) slow-counters crono-list)
             (serve-helper (cdr requests) fast-counters (update (λ (C) (delay C minutes)) slow-counters index) crono-list))]
        
        [ x
          (define counters (append fast-counters slow-counters))
          (serve-helper (cdr requests) (map (pass-time-remove x) fast-counters)
                        (map (pass-time-remove x) slow-counters)
                        (append crono-list (dequeue-clients (append fast-counters slow-counters) x)))]
        )))
