#lang racket/base

(require racket/class
         racket/gui
         framework)

(define (apply-mixins % . mxs)
  (for/fold ([% %]) ([mx (in-list mxs)])
    (mx %)))

(define repl:menus-mixin
  (mixin (frame:standard-menus<%>)
         (frame:standard-menus<%>)
    (define/override (file-menu:create-new?) #f)
    (define/override (file-menu:create-open?) #f)
    (define/override (file-menu:create-open-recent?) #f)
    (define/override (file-menu:create-close?) #f)
    (define/override (edit-menu:create-preferences?) #f)
    (super-new)))


;; frame:editor-mixin does this sizing as well, but it also pulls in
;; extra functions that are not needed
(define frame:size-mixin
  (mixin (window<%>)
         (window<%>)
    (define-values (screen-width screen-height) (get-display-size))

    (init [width  (min 600 (- screen-width 65))]
          [height (min 650 (- screen-height 65))])
    (super-new [width width] [height height])))

(define repl:frame%
  (apply-mixins
    frame%
    frame:basic-mixin
    frame:size-mixin
    frame:standard-menus-mixin
    repl:menus-mixin))

(define window
  (new repl:frame%
       [label "repl"]))

(define repl:text%
  (apply-mixins text:basic%
                editor:standard-style-list-mixin
                text:wide-snip-mixin
                text:ports-mixin))

(define text
  (new repl:text%))

(define canvas
  (new canvas:basic%
       [parent (send window get-area-container)]
       [editor text]))

(frame:remove-empty-menus window)
(send window show #t)
(send canvas focus)


(module* main #f
  (require racket/format
           music/data/chord/main
           music/data/note/main
           music/data/instrument/main
           music/notation/image/chord-chart)

  (define notes
    (hash "AFLAT"  (flat  (A 2))
          "A"             (A 2)
          "ASHARP" (sharp (A 2))

          "BFLAT"  (flat  (B 2))
          "B"             (B 2)

          "C"             (C 2)
          "CSHARP" (sharp (C 2))

          "DFLAT"  (flat  (D 2))
          "D"             (D 2)
          "DSHARP" (sharp (D 2))

          "EFLAT"  (flat  (E 2))
          "E"             (E 2)

          "F"             (F 2)
          "FSHARP" (sharp (F 2))

          "GFLAT"  (flat  (G 2))
          "G"             (G 2)
          "GSHARP" (sharp (G 2))))

  (define (note-name? n)
    (hash-has-key? notes (string-upcase n)))

  (define (find-note n)
    (hash-ref notes (string-upcase n)))

  (define chords
    (hash "MAJOR" major-triad
          "MINOR" minor-triad))

  (define (chord-name? c)
    (hash-has-key? chords (string-upcase c)))

  (define (find-chord c)
    (hash-ref chords (string-upcase c)))

  (define (repl)
    (define instrument guitar-strings)
    (define instrument-name "guitar")

    (define (cmd-prompt)
      (display (~a instrument-name "> ")))

    (define (parse-request s)
      (match (string-split s)
        [(list (? note-name? note-name))
         (values (find-note note-name) major-triad)]
        [(list (? note-name? note-name) (? chord-name? chord-name))
         (values (find-note note-name) (find-chord chord-name))]))

    (cmd-prompt)
    (for ([line (in-lines)])
      (define-values (a-note a-chord) (parse-request line))
      (define layouts
        (min-stretch-chord-layouts
          instrument (chord a-note a-chord)))
      (for ([c (in-list layouts)])
        (displayln (guitar-chord-chart c)))
      (cmd-prompt)))

  (void
    (thread
      (lambda ()
        (parameterize ([current-input-port  (send text get-in-port)]
                       [current-output-port (send text get-out-port)]
                       [current-error-port  (send text get-err-port)])
          (repl))))))


