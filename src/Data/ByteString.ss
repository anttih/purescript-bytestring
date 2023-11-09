#!chezscheme
(library (Data.ByteString foreign)
  (export bytestring-ref
          bytestring-codepoint-ref
          unconsCodeUnitImpl
          unconsCodePointImpl
          fromString
          showByteString
          codePointAtImpl)
  (import (chezscheme)
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

  (define (bytestring-read-byte bs)
    (bytevector-s8-ref (bytestring-buffer bs) (bytestring-offset bs)))

  (define (bytestring-forward bs n)
    (make-bytestring
      (bytestring-buffer bs)
      (fx+ (bytestring-offset bs) n)
      (fx- (bytestring-length bs) n)))

  (define (bytestring->string bs)
    (let ([buf (make-bytevector (bytestring-length bs))])
      (bytevector-copy! (bytestring-buffer bs) (bytestring-offset bs) buf 0 (bytestring-length bs))
      (utf8->string buf)))

  (define (bytestring-uncons-codepoint bs)
    (if (bytestring-empty? bs)
      (values #f '())
      (let* ([buf (bytestring-buffer bs)]
             [b1 (bytevector-s8-ref buf (bytestring-offset bs))])
        (cond
          [(fx=? #16rf0 (logand #16rf8 b1))
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
          [(fx=? #16re0 (logand #16rf0 b1))
           (if (fx>= (bytestring-length bs) 3)
             (let* ([b2 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 1))]
                    [b3 (bytevector-s8-ref buf (fx+ (bytestring-offset bs) 2))]
                    [head (fxlogor (fxsll (fxlogand b1 #b1111) 12)
                                  (fxsll (fxlogand b2 #b111111) 6)
                                  (fxlogand b3 #b111111))]
                    [tail (bytestring-forward bs 3)])
               (values head tail))
             (values #f '()))]
          [(fx=? #16rc0 (logand #16re0 b1))
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

  (define (bytestring-codepoint-ref bs n)
    (let loop ([i 0]
               [cur bs])
      (let-values ([(head tail) (bytestring-uncons-codepoint cur)])
        (if (not head)
          ;; TODO raise continuable
          #f
          (if (fx=? i n)
            head
            (loop (fx1+ i) tail))))))

  (define (bytestring-ref bs n)
    (integer->char (bytestring-codepoint-ref bs n)))

  ;; TODO
  ;; string-append
  ;; string->number
  ;; substring

  ;; ------------------------------------------------------------ 
  ;; PureScript FFI
  ;; ------------------------------------------------------------

  (define fromString
    (lambda (s)
      (let ([buffer (string->utf8 s)])
        (make-bytestring buffer 0 (bytevector-length buffer)))))

  (define showByteString
    (lambda (bs)
      (format "~a" (bytestring->string bs))))

  (define unconsCodeUnitImpl
    (lambda (bs Just Nothing)
      (if (bytestring-empty? bs)
        Nothing
        (let* ([head (bytestring-read-byte bs)]
               [tail (bytestring-forward bs 1)])
          (Just (rt:make-object (cons "head" head) (cons "tail" tail)))))))

  (define unconsCodePointImpl
    (lambda (bs Just Nothing)
      (let-values ([(head tail) (bytestring-uncons-codepoint bs)])
        (if (not head)
          Nothing
          (Just (rt:make-object (cons "head" head) (cons "tail" tail)))))))

  (define codePointAtImpl
    (lambda (Just Nothing n bs)
      (let loop ([i 0]
                 [cur bs])
        (let-values ([(head tail) (bytestring-uncons-codepoint cur)])
          (if (not head)
            Nothing
            (if (fx=? i n)
              (Just head)
              (loop (fx1+ i) tail)))))))

  )

