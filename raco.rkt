#lang racket/base

; don't run this file for testing:
(module test racket/base)

(require racket/cmdline
         racket/path
         racket/list
         racket/file
         racket/string
         raco/command-name
         "main.rkt"
         "private/config.rkt")

(define current-in-place? (make-parameter #f))
(define current-stdin-source-path (make-parameter #f))
(define current-all? (make-parameter #f))
(define current-project-root (make-parameter (current-directory)))

(define raco-name
  (format "raco ~a" (current-command-name)))

(define files
  (command-line
   #:program raco-name
   #:usage-help
   "A code formatter for Racket."
   #:once-any
   [("-s" "--stdin")
    source-path
    ("Read program source from stdin, "
     "assuming the corresponding path is <source-path>")
    (current-stdin-source-path source-path)]
   [("-a" "--all")
    "Format all files under the project root"
    (current-all? #t)]

   #:once-each
   [("-p" "--project-root")
    proj-dir
    "Change the project root. `-` means no project root. "
    "(the default project root is current directory)"
    (current-project-root proj-dir)]
   [("-i" "--in-place")
    "Modify files in-place"
    (current-in-place? #t)]

   #:args args
   (unless (empty? args)
     (when (current-stdin-source-path)
       (raise-user-error "The stdin mode expects no further arguments"))
     (when (current-all?)
       (raise-user-error "The format all mode expects no further arguments")))

   args))

(define proj-root-dir (simple-form-path (current-project-root)))

(define (do-format content #:source-path source-path)
  (define config-map
    (case (current-project-root)
      [("-") (λ (path) #f)]
      [else
       (define config-path (build-path proj-root-dir "fmt-config.rkt"))
       (cond
         [(file-exists? config-path) (dynamic-require config-path 'config-map)]
         [else (λ (path) #f)])]))

  (define out
    (with-config (default-config-map source-path)
      (format-code content
                   #:source-path source-path
                   #:setup (λ (thk)
                             (with-config (config-map source-path)
                               (thk))))))

  (when out
    (case (current-in-place?)
      [(#f) (displayln out)]
      [(#t) (with-output-to-file source-path
              #:exists 'must-truncate
              (λ () (displayln out)))])))

;; NOTE: use file->lines to handle CRLF on Windows
;;
;; See https://github.com/sorawee/fmt/issues/41
(cond
  [(current-all?)
   (for ([path (in-directory proj-root-dir)])
     (do-format (string-join (file->lines path) "\n")
                #:source-path path))]
  [(current-stdin-source-path)
   (when (current-in-place?)
     (raise-user-error "The stdin mode can't be used with in-place"))
   (do-format (string-join (for/list ([line (in-lines)]) line) "\n")
              #:source-path (current-stdin-source-path))]
  [else
   (for ([path (in-list files)])
     (do-format (string-join (file->lines path) "\n")
                #:source-path path))])
