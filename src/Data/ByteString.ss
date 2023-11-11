#!chezscheme
(library (Data.ByteString foreign)
  (export bytestring-ref
          bytestring-ref-code-point
          bytestring-ref-code-unit
          bytestring-length
          (rename (make-bytestring-of-length make-bytestring))
          bytestring?
          bytestring=?
          substring
          bytestring
          bytestring-append
          bytestring->list
          bytestring->number
          number->bytestring
          bytestring->symbol
          string->bytestring

          (rename (bytestring-length length))
          (rename (string->bytestring fromString))
          (rename (bytestring=? eqImpl))
          (rename (bytestring-append concatImpl))
          toCodePointList
          toCodePointArray
          unconsCodeUnitImpl
          unconsCodePointImpl
          showByteString
          codePointAtImpl
          sliceImpl)
  (import (except (chezscheme) substring)
          (prefix (purs runtime srfi :214) srfi:214:)
          (prefix (purs runtime lib) rt:))

  ;; Immutable UTF-8 encoded slice into a bytevector buffer
  (define-structure
    (bytestring
      ;; the UTF-8 encoded bytevector
      buffer
      ;; start offset of the slice
      offset
      ;; size of the slice in bytes (code units)
      length))

  (define (bytestring-empty? bs)
    (fx=? (bytestring-length bs) 0))

  ;; this is our version of `make-string`
  (define (make-bytestring-of-length n)
    (make-bytestring (make-bytevector n) 0 n))

  ;; Assumes the buffers have the same length
  (define (bytestring-equal-code-units? x y)
    (let loop ([n 0] [tailx x] [taily y])
      (let-values ([(hx tx) (bytestring-uncons-code-unit tailx)]
                   [(hy ty) (bytestring-uncons-code-unit taily)])
        (assert hx)
        (assert hy)
        (if (bytestring-empty? tx)
          #t
          (if (fx=? hx hy)
            (loop (fx1+ n) tx ty)
            #f)))))

  (define (bytestring=? x y)
    (if (fx=? (bytestring-length x) (bytestring-length y))
      (cond
        ;; Do they point to the same object in memory?
        [(and (fx=? (bytestring-offset x) (bytestring-offset y))
              (eq? (bytestring-buffer x) (bytestring-buffer y))) #t]
        [(bytestring-equal-code-units? x y) #t]
        [else #f])
      #f))

  (define (string->bytestring s)
    (let ([bv (string->utf8 s)])
      (make-bytestring bv 0 (bytevector-length bv))))

  (define (bytestring->string bs)
    (let ([buf (make-bytevector (bytestring-length bs))])
      (bytevector-copy! (bytestring-buffer bs) (bytestring-offset bs) buf 0 (bytestring-length bs))
      (utf8->string buf)))

  (define (bytestring-read-byte bs)
    (bytevector-s8-ref (bytestring-buffer bs) (bytestring-offset bs)))

  (define (bytestring-forward bs n)
    (make-bytestring
      (bytestring-buffer bs)
      (fx+ (bytestring-offset bs) n)
      (fx- (bytestring-length bs) n)))

  (define (bytestring-uncons-code-unit bs)
    (if (bytestring-empty? bs)
      (values #f '())
      (let ([head (bytestring-read-byte bs)]
            [tail (bytestring-forward bs 1)])
        (values head tail))))

  (define (bytestring-ref-code-unit bs n)
    (let loop ([i 0]
               [cur bs])
      (let-values ([(head tail) (bytestring-uncons-code-unit cur)])
        (assert head)
        (if (fx=? i n)
          head
          (loop (fx1+ i) tail)))))

  (define (bytestring-uncons-code-point bs)
    (if (bytestring-empty? bs)
      (values #f '())
      (let* ([buf (bytestring-buffer bs)]
             [b1 (bytevector-s8-ref buf (bytestring-offset bs))])
        (cond
          [(fx=? #xf0 (logand #xf8 b1))
           (if (fx>= (bytestring-length bs) 4)
             (let* ([b2 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 1))]
                    [b3 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 2))]
                    [b4 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 3))]
                    [head (fxlogor (fxsll (fxlogand b1 #b111) 16)
                                  (fxsll (fxlogand b2 #b1111) 12)
                                  (fxsll (fxlogand b3 #b111111) 6)
                                  (fxlogand b4 #b111111))]
                    [tail (bytestring-forward bs 4)])
               (values head tail))
             (values #f '()))]
          [(fx=? #xe0 (logand #xf0 b1))
           (if (fx>= (bytestring-length bs) 3)
             (let* ([b2 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 1))]
                    [b3 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 2))]
                    [head (fxlogor (fxsll (fxlogand b1 #b1111) 12)
                                  (fxsll (fxlogand b2 #b111111) 6)
                                  (fxlogand b3 #b111111))]
                    [tail (bytestring-forward bs 3)])
               (values head tail))
             (values #f '()))]
          [(fx=? #xc0 (logand #xe0 b1))
           (if (fx>= (bytestring-length bs) 2)
             (let* ([b2 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 1))]
                    [head (fxlogor (fxsll (fxlogand b1 #b11111) 6) (fxlogand b2 #b111111))]
                    [tail (bytestring-forward bs 2)])
                (values head tail))
             (values #f '()))]
          [else
            (let ([head b1]
                  [tail (bytestring-forward bs 1)])
              (values  head tail))]))))

  (define (bytestring-ref-code-point bs n)
    (let loop ([i 0]
               [cur bs])
      (let-values ([(head tail) (bytestring-uncons-code-point cur)])
        (assert head)
        (if (fx=? i n)
          head
          (loop (fx1+ i) tail)))))

  ;; Coerce a UTF-8 code unit to a scheme char
  (define (s8->char c)
    (if (and (fx>= c 0) (fx<= c 127))
        (integer->char c)
        (integer->char 65533)))

  ;; Constant-time ref, like string->ref.
  ;; Returns a scheme `char`.
  (define (bytestring-ref bs n)
    (s8->char (bytestring-ref-code-unit bs n)))

  (define (substring bs start end)
    (make-bytestring
      (bytestring-buffer bs)
      (fx+ (bytestring-offset bs) start)
      (fx- end start)))

  (define (bytestring-append x y)
    (let* ([len (fx+ (bytestring-length x) (bytestring-length y))]
           [buf (make-bytevector len)])
      (bytevector-copy! (bytestring-buffer x) (bytestring-offset x) buf 0 (bytestring-length x))
      (bytevector-copy! (bytestring-buffer y) (bytestring-offset y) buf (bytestring-length x) (bytestring-length y))
      (make-bytestring buf 0 len)))

  ;; linear-time, returns a list of `char`s
  (define (bytestring->list bs)
    (let loop ([tail bs]
               [ls '()])
      (let-values ([(h t) (bytestring-uncons-code-point tail)])
        (if (not h)
          (reverse ls)
          (loop t (cons (integer->char h) ls))))))

  (define bytestring->number
    (case-lambda
      [(bs) (string->number (bytestring->string bs))]
      [(bs radix) (string->number (bytestring->string bs) radix)]))

  (define number->bytestring
    (case-lambda
      [(n) (string->bytestring (number->string n))]
      [(n radix) (string->bytestring (number->string n radix))]))

  (define (bytestring->symbol bs)
    (string->symbol (bytestring->string bs)))

  (define (bytestring . chars)
    (string->bytestring (apply string chars)))

  ;; ------------------------------------------------------------ 
  ;; PureScript FFI
  ;; ------------------------------------------------------------

  (define showByteString
    (lambda (bs)
      (format "~a" (bytestring->string bs))))

  (define unconsCodeUnitImpl
    (lambda (bs Just Nothing)
      (if (bytestring-empty? bs)
        Nothing
        (let* ([head (bytestring-read-byte bs)]
               [tail (bytestring-forward bs 1)])
          (Just (rt:make-object (cons "head" (s8->char head)) (cons "tail" tail)))))))

  (define unconsCodePointImpl
    (lambda (bs Just Nothing)
      (let-values ([(head tail) (bytestring-uncons-code-point bs)])
        (if (not head)
          Nothing
          (Just (rt:make-object (cons "head" head) (cons "tail" tail)))))))

  (define codePointAtImpl
    (lambda (Just Nothing n bs)
      (let loop ([i 0]
                 [cur bs])
        (let-values ([(head tail) (bytestring-uncons-code-point cur)])
          (if (not head)
            Nothing
            (if (fx=? i n)
              (Just head)
              (loop (fx1+ i) tail)))))))

  (define sliceImpl
    (lambda (start end bs)
      (make-bytestring
        (bytestring-buffer bs)
        (fx+ (bytestring-offset bs) start)
        (fx- end start))))
      
  (define (toCodePointList bs)
    (let loop ([tail bs]
               [ls '()])
      (let-values ([(h t) (bytestring-uncons-code-point tail)])
        (if (not h)
          (reverse ls)
          (loop t (cons h ls))))))

  (define (toCodePointArray bs)
    (srfi:214:list->flexvector (toCodePointList bs)))

  )

