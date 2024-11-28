(import (chicken io))

(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '())
                 (next-line (read-line)))
        (if (eof-object? next-line)
            (reverse lines)
            (loop (cons next-line lines)
                  (read-line)))))))
