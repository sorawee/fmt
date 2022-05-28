;;; racket-fmt-format.el --- Format Racket code using the fmt package -*- lexical-binding: t -*-


;; Homepage: https://gitlab.com/sorawee/fmt
;; Version: 0.0.0
;; Keywords: languages
;; Package-Requires: ((emacs "24.3") (reformatter "0.6"))



;;; Commentary:


;; Emacs Lisp library for formatting Racket source code files
;; by using the Racket "fmt" package.

;; WARNING: Do not add the racket-fmt-format-on-save-mode function to
;; "racket-mode" if you use "racket-mode" for sources with syntax other than
;; pure Racket, for example Scribble, because "raco fmt" may not work properly
;; with your .scrbl source files.
;; If you want to run "fmt" on file save, you may be interested in adding
;; following piece of code to your personal Emacs configuration instead:
;; (add-to-list 'auto-mode-alist '("\\.rkt\\'" . (lambda () (racket-mode) (racket-fmt-format-on-save-mode))))

;; The Racket part uses Sorawee's "fmt" package.
;; https://github.com/sorawee/fmt

;; The ELisp part uses Steve Purcell's "emacs-reformatter" package.
;; https://github.com/purcell/emacs-reformatter



;;; Code:


(require 'reformatter)


(defgroup racket-fmt-format nil
  "Racket source code formatting using the fmt package."
  :group 'languages)


;; To avoid "file not found" error the executable invoked by reformatter
;; is set to "raco", which will call "fmt".

(defcustom racket-fmt-format-raco-command "raco"
  "Command to run Racket's \"raco\".
Should be \"raco\" or the complete path to \"raco\" executable."
  :safe 'stringp
  :type 'file
  :group 'racket-fmt-format)


;; Command arguments
;; https://docs.racket-lang.org/fmt/#%28part._.Running_raco_fmt%29

(defcustom racket-fmt-format-indent 1
  "Racket source code indentation level for subsequent lines."
  :type 'numberp
  :group 'racket-fmt-format)

(defcustom racket-fmt-format-max-blank-lines 1
  "Racket source code maximum consecutive blank lines limit."
  :type 'numberp
  :group 'racket-fmt-format)

(defcustom racket-fmt-format-width 80
  "Racket source maximum code page width."
  :type 'numberp
  :group 'racket-fmt-format)

(defun racket-fmt-format--create-arg-list ()
  "Gather `racket-fmt-format' customized variables and create arguments list."
  (list "fmt"
        "--indent"
        (number-to-string racket-fmt-format-indent)
        "--max-blank-lines"
        (number-to-string racket-fmt-format-max-blank-lines)
        "--width"
        (number-to-string racket-fmt-format-width)))

(defcustom racket-fmt-format-args (racket-fmt-format--create-arg-list)
  "Arguments passed to \"raco\" to invoke the \"fmt\" subcommand.
This variable may not be safe to customize by hand."
  :type '(repeat string)
  :group 'racket-fmt-format)


;; Reformatter definition and commands
;; https://github.com/purcell/emacs-reformatter/blob/master/README.md

;;;###autoload (autoload 'racket-fmt-format-buffer "racket-fmt-format" nil t)
;;;###autoload (autoload 'racket-fmt-format-region "racket-fmt-format" nil t)
;;;###autoload (autoload 'racket-fmt-format-on-save-mode "racket-fmt-format" nil t)
(reformatter-define racket-fmt-format
  :args racket-fmt-format-args
  :program racket-fmt-format-raco-command
  :lighter " FMT")


(provide 'racket-fmt-format)



;;; racket-fmt-format.el ends here
