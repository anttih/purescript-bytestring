#!chezscheme
(library (Bench.Main foreign)
  (export unconsCodePointImpl readTextFile lengthNative stringAppend)
  (import (chezscheme)
          (prefix (purs runtime lib) rt:))

  (define unconsCodePointImpl
    (lambda (s Just Nothing)
      (let ([len (string-length s)])
        (if (fx=? len 0)
          Nothing
          (let ([head (string-ref s 0)]
                [tail (substring s 1 len)])
            (Just (rt:make-object (cons "head" (char->integer head)) (cons "tail" tail))))))))

  (define readTextFile
    (lambda (path)
      (lambda ()
        (with-input-from-file path
          (lambda ()
            (let loop ((chars '())
                       (next-char (read-char)))
               (if (eof-object? next-char)
                   (list->string (reverse chars))
                   (loop (cons next-char chars)
                         (read-char)))))))))

  (define lengthNative string-length)

  (define stringAppend string-append)

  )
