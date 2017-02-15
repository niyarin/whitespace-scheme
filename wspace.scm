#!/usr/local/bin/gosh

(import (scheme base) (scheme write) (scheme read) (scheme file) (scheme process-context))

(cond-expand 
  (gauche (import (scheme base)(scheme write)(scheme read)(scheme file)(srfi 11)(scheme process-context ))))

(define (vis-wscode code)
  (let loop ((code code))
    (cond 
      ((null? code) '())
      ((char=? (car code) #\space) (display 'S) (loop (cdr code)))
      ((char=? (car code) #\tab) (display 'T) (loop (cdr code)))
      ((char=? (car code) #\newline) (display 'L)(loop (cdr code)))
      (else (display (car code))(loop (cdr code))))))


(define (ws-decode-number code)
  (let ((sign (if (char=? (car code) #\space) 1 -1)))
    (let loop ((code (cdr code))(num 0))
        (cond 
          ((char=? (car code) #\space) (loop (cdr code) (* num 2)))
          ((char=? (car code) #\tab) (loop (cdr code) (+ (* num 2 ) 1)))
          ((char=? (car code) #\newline) (values (* num sign) (cdr code)))))))


(define (ws-decode-stack-manipulation code breaker)
  (cond 
    ;[space] Push the number onto the stack.
    ((and (char=? (car code) #\space))
     (let-values (((number code) (ws-decode-number (cdr code))))
       (values 'push number code)))
    ;[lf][space] Duplicate the top item on the stack.
    ((and (char=? (car code) #\newline) (char=? (cadr code) #\space))
     (values `copy '() (cddr code)))
    ;[lf][tab] Swap the top two items on the stack
    ((and (char=? (car code) #\newline)(char=? (cadr code) #\tab))
     (values `swap '() (cddr code)))
    ;[lf][lf] Discard the top item on the stack
    ((and (char=? (car code) #\newline)(char=? (cadr code) #\newline))
     (values `peek '() (cddr code)))
    (else (breaker "ERROR:UNDEFINED CODE [SPACE][TAB]") )))


(define (ws-decode-heap code breaker)
  (cond 
    ;[space] store
    ((char=? (car code) #\space)
     (values 'store '() (cdr code)))
    ;[tab] retrieve
    ((char=? (car code) #\tab)
     (values 'load '() (cdr code)))
    (else (breaker "ERROR:UNDEFINED CODE [TAB][TAB][LF]"))))


(define (ws-decode-arithmetic code breaker)
  (cond 
    ;[space][space] addition
    ((and (char=? (car code) #\space)(char=? (cadr code) #\space))
     (values 'add '() (cddr code)))
    ;[space][tab] subtraction
    ((and (char=? (car code) #\space)(char=? (cadr code) #\tab))
     (values 'sub '() (cddr code)))
    ;[space][lf] multiplication
    ((and (char=? (car code) #\space) (char=? (cadr code) #\newline))
     (values 'mul '() (cddr code)))
    ;[tab][space] integer division
    ((and (char=? (car code) #\tab)(char=? (cadr code) #\space))
     (values 'div '() (cddr code)))
    ;[tab][tab] modulo
    ((and (char=? (car code) #\tab)(char=? (cadr code) #\tab))
     (values 'mod '() (cddr code)))
    (else (breaker "ERROR:UNDEFINED CODE [TAB][SPACE]<?>" ))))


(define (ws-decode-io code breaker)
  (cond 
    ;[space][space] Output the character at the top of the stack
    ((and (char=? (car code) #\space) (char=? (cadr code) #\space))
     (values 'print-char '() (cddr code)))
    ;[space][tab] Output the number at the top of the stack
    ((and (char=? (car code) #\space) (char=? (cadr code) #\tab))
     (values 'print-number '() (cddr code)))
    ;[tab][space] Read a character and place it in the location given by the top of the stack
    ((and (char=? (car code) #\tab) (char=? (cadr code) #\space))
     (values 'read-char '() (cddr code)))
    ;[tab][tab] Read a number and place it in the location given by the top of the stack
    ((and (char=? (car code) #\tab) (char=? (cadr code) #\tab))
     (values 'read-int '() (cddr code)))
    (else (breaker "ERROR:UNDEFINE CODE [TAB][LF]<?>"))))
    

    

(define (ws-decode-flow-control code breaker)
  (cond 
    ;[space][space] Mark a location in the program.
    ((and (char=? (car code) #\space) (char=? (cadr code) #\space))
     (let-values (((number code) (ws-decode-number (cddr code))))
        (values 'mark number code)))
    ;[space][tab] Call a subroutine
    ((and (char=? (car code) #\space) (char=? (cadr code) #\tab))
     (let-values (((number code) (ws-decode-number (cddr code))))
        (values 'call number code)))
    ;[space][lf] Jump unconditionally to a label
    ((and (char=? (car code) #\space)(char=? (cadr code) #\newline))
     (let-values (((number code) (ws-decode-number (cddr code))))
       (values 'jump number code)))
    ;[tab][space] Jump to a label if the top of the stack is zero
    ((and (char=? (car code) #\tab)(char=? (cadr code) #\space))
     (let-values (((number code) (ws-decode-number (cddr code))))
      (values 'jz number code)))
    ;[tab][tab] Jump to a label if the top of the stack is negative
    ((and (char=? (car code) #\tab)(char=? (cadr code) #\tab))
     (let-values (((number code) (ws-decode-number (cddr code))))
      (values 'jmi number code)))
    ;[tab][lf] End a subroutine and transfer control back to the caller
    ((and (char=? (car code) #\tab)(char=? (cadr code) #\newline))
     (values 'return '() (cddr code)))
    ;[lf][lf] End the program
    ((and (char=? (car code) #\newline)(char=? (cadr code) #\newline))
     (values 'end '() (cddr code)))
    (else (breaker "ERROR:UNDEFINE CODE [LF]<?>") )))



(define (ws-decode-code code ret breaker)
  (if (or (null? code)
          (and (null? (cdr code))(char=? (car code) #\newline))
          (and (not (null? (cdr code)))(null? (cddr code))(char=? (car code) #\newline)(char=? (cadr code) #\newline)))
    (reverse (cons '(end )ret))
    (let-values (((cmd param code)
                  (cond 
                    ;[space] stack manipulation
                    ((and (char=? (car code) #\space)(cdr code)) 
                     (ws-decode-stack-manipulation (cdr code) breaker))
                    ;[tab][space] arithmetic
                    ((and (char=? (car code) #\tab)(char=? (cadr code) #\space))
                     (ws-decode-arithmetic (cddr code) breaker))
                    ;[tab][tab] heap access
                    ((and (char=? (car code) #\tab)(char=? (cadr code) #\tab)) 
                     (ws-decode-heap (cddr code) breaker))
                    ;[lf] flow control
                    ((char=? (car code) #\newline) 
                     (ws-decode-flow-control (cdr code) breaker))
                    ;[tab][lf] I/O
                    ((and (char=? (car code) #\tab)(char=? (cadr code) #\newline))
                     (ws-decode-io (cddr code) breaker))
                    )))
        ;(display cmd)(display " ")(vis-wscode code)(newline)
        (ws-decode-code code (cons (cons cmd param) ret) breaker))))


(define (ws-generate-tree-code code breaker)
  (define (copy-code code)
    (let loop ((code code))
      (cond 
        ((null? code) '())
        ((or (eqv? (caar code) 'jump) (eqv? (caar code) 'call) (eqv? (caar code) 'jz) (eqv? (caar code) 'jmi))
         (cons (cons (caar code) (cdar code)) (loop (cdr code))))
        (else (cons (car code) (loop (cdr code)))))))
  (define (search-cut-labels! code)
    (let ((_code (cons #f code)))
      (values
        (let loop ((code code)(prev _code))
          (cond
            ((null? code) '())
            ((eqv? (caar code) 'mark)
             (let ((alist (loop (cdr code) code)))
               (set-cdr! prev (cdr code))
               (if (assv (cdar code) alist)
                 (breaker "ERROR:MARK LABEL ERROR")
                 (cons (cons (cdar code) (cdr code)) alist))))
            (else (loop (cdr code) code))))
        (cdr _code))))


  (let ((code (copy-code code)))
    (let-values (((alist code) (search-cut-labels! code)))
       (let loop ((code code))
         (cond 
           ((null? code) '())
           ((or (eqv? (caar code) 'jump) (eqv? (caar code) 'call) (eqv? (caar code) 'jz) (eqv? (caar code) 'jmi))
             (cond ((assoc (cdar code) alist)
                    => (lambda (addr-code)(set-cdr! (car code) (cdr addr-code))))
                   (else (breaker "ERROR:MARK ERROR")))
             (loop (cdr code)))
           (else (loop (cdr code)))))
       code
       )))




(define (vm-set-heap! heap addr data)
  (if (> (vector-length heap) addr)
    (begin
      (vector-set! heap addr data)
      heap)
    (let ((heap (vector-append heap (make-vector (+ (- addr (vector-length heap)) 1)))))
      (vector-set! heap addr data)
      heap)))


(define (vm-get-heap heap addr breaker)
  (if (> (vector-length heap) addr)
    (vector-ref heap addr)
    (breaker "ERROR:HEAP ERROR")))


(define (vm-pop-stack stack breaker)
  (if (null? stack)
    (breaker "ERROR:STACK IS EMPTY")
    (values (car stack) (cdr stack))))


(define (ws-vm code stack heap call-stack breaker)
  ;(display "VM:")(display (caar code))(newline)
  (case (caar code)
    ((push) 
     (ws-vm (cdr code) (cons (cdar code) stack) heap call-stack breaker))
    ((copy)
     (if (null? stack)
       (breaker "ERROR STACK IS EMPTY")
       (ws-vm (cdr code) (cons (car stack) stack) heap call-stack breaker)))
    ((swap)
     (if (or (null? stack) (null? (cdr stack)))
       (breaker "ERROR STACK IS EMPTY")
       (ws-vm (cdr code) (cons (cadr stack) (cons (car stack) (cddr stack))))))
    ((peek) 
     (if (null? stack)
       (breaker "ERROR STACK IS EMPTY")
       (ws-vm (cdr code) (cdr stack) heap call-stack breaker)))
    ((store)
     (let-values (((obj stack) (vm-pop-stack stack breaker)));replace obj <-> addr
        (let-values (((addr stack) (vm-pop-stack stack breaker)))
           (ws-vm (cdr code) stack (vm-set-heap! heap addr obj) call-stack breaker ))))
    ((load)
     (let-values (((addr stack) (vm-pop-stack stack breaker)))
        (let ((v (vm-get-heap heap addr breaker)))
          (ws-vm (cdr code) (cons v stack) heap call-stack breaker))))
    ((add)
     (let-values (((a stack) (vm-pop-stack stack breaker)))
        (let-values (((b stack) (vm-pop-stack stack breaker)))
           (ws-vm (cdr code)(cons (+ a b) stack) heap call-stack breaker))))
    ((sub)
     (let-values (((a stack) (vm-pop-stack stack breaker)))
        (let-values (((b stack) (vm-pop-stack stack breaker)))
           (ws-vm (cdr code) (cons (- a b) stack) heap call-stack breaker))))
    ((mul) 
     (let-values (((a stack) (vm-pop-stack stack breaker)))
        (let-values (((b stack) (vm-pop-stack stack breaker)))
           (ws-vm (cdr code) (cons (* a b) stack) heap call-stack breaker))))
    ((div)
     (let-values (((a stack) (vm-pop-stack stack breaker)))
         (let-values (((b stack) (vm-pop-stack stack breaker)))
             (ws-vm (cdr code) (cons (quotient a b) stack) heap call-stack breaker))))
    ((mod)
     (let-values (((a stack) (vm-pop-stack stack breaker)))
        (let-values (((b stack) (vm-pop-stack stack breaker)))
           (ws-vm (cdr code) (cons (modulo a b) stack) heap call-stack breaker))))
    
    ((jump)
     (ws-vm (cdar code) stack heap call-stack breaker))
    ((call)
     (ws-vm (cdar code) stack heap (cons (cdr code) call-stack) breaker))
    ((jz)
     (let-values (((n stack) (vm-pop-stack stack breaker)))
        (if (zero? n)
          (ws-vm (cdar code) stack heap call-stack breaker)
          (ws-vm (cdr code) stack heap call-stack breaker))))
    ((jmi)
     (let-values (((n stack) (vm-pop-stack stack breaker)))
        (if (< n 0)
          (ws-vm (cdar code) stack heap call-stack breaker)
          (ws-vm (cdr code) stack heap call-stack breaker))))
    ((return)
     (let-values (((rcode call-stack) (vm-pop-stack call-stack breaker)))
        (ws-vm rcode stack heap call-stack breaker)))
    ((end)
     #f)
    ((print-number)
     (let-values (((n stack) (vm-pop-stack stack breaker)))
        (display n)(newline)
        (ws-vm (cdr code) stack heap call-stack breaker)))
    ((print-char)
     (let-values (((c stack) (vm-pop-stack stack breaker)))
        (display (integer->char c))
        (ws-vm (cdr code) stack heap call-stack breaker)))
    ((read-char)
     (let-values (((addr stack) (vm-pop-stack stack breaker)))
        (ws-vm (cdr code) stack (vm-set-heap! heap addr 97 ) call-stack breaker))) ;char!!!!!!!!!!! 
    ((read-number)
     (let-values (((addr stack) (vm-pop-stack stack breaker)))
        (ws-vm (cdr code) stack (vm-set-heap! heap addr 97 ) call-stack breaker)))
    ))
       

(define (run filename)
  (define (read-ws-file filename)
    (call-with-input-file 
      filename 
      (lambda (fp)
        (let loop ()
          (let ((c (read-char fp)))
            (cond 
              ((eof-object? c) '())
              ((or (char=? c #\space) (char=? c #\tab) (char=? c #\newline))
               (cons c (loop)))
              (else (loop))))))))
  (cond 
    (
      (call/cc 
        (lambda (breaker)
          (ws-vm
            (ws-generate-tree-code 
              (ws-decode-code (read-ws-file filename) '() breaker) 
              breaker) 
            '()
            #()
            '()
            breaker)))
      =>
      (lambda (err)
        (display err)(newline)))))
    




(define (main)
  (let ((argv  (command-line)))
    (if (= (length argv) 2)
      (run (cadr argv))
      (begin
        (display "Whitespace Interpreter")(newline)
        (display "Usage: wspace.scm [file]")(newline))))) 

(main)
