;;; -----------------------------
;;;  Common Lisp translation of update_examples (Perl script)
;;;  Requires: pregexp  (https://ds26gte.github.io/pregexp/index.html)
;;;  No other dependencies.
;;;
;;;  Reads texinfo source from *standard-input* (or files on the command line),
;;;  runs Maxima on collected example-input blocks, replaces example-output
;;;  blocks with fresh Maxima output, and writes the result to *standard-output*.
;;;  Exits with the error count as the exit code.
;;; -----------------------------

;;; ---------------------------------------------------------------------------
;;; Portability helpers (replaces compat.lisp)
;;; ---------------------------------------------------------------------------

(defun getcwd ()
  "Return the current working directory as a string."
  #+cmucl (namestring (ext:default-directory))
  #+sbcl  (namestring (sb-posix:getcwd))
  #+ccl   (namestring (ccl:current-directory))
  #+ecl   (namestring (ext:getcwd))
  #+clisp (namestring (ext:default-directory))
  #-(or cmucl sbcl ccl ecl clisp)
  (namestring *default-pathname-defaults*))

(defun getenv (name)
  "Return the value of environment variable NAME, or NIL if unset."
  #+cmucl (cdr (assoc name ext:*environment-list* :test #'string=))
  #+sbcl  (sb-ext:posix-getenv name)
  #+ccl   (ccl:getenv name)
  #+ecl   (ext:getenv name)
  #+clisp (ext:getenv name)
  #-(or cmucl sbcl ccl ecl clisp) nil)

(defun tmpnam ()
  "Return a fresh temporary filename string."
  #+cmucl (format nil "/tmp/update-examples-~a-~a"
                  (unix:unix-getpid) (get-universal-time))
  #+sbcl  (format nil "/tmp/update-examples-~a-~a"
                  (sb-posix:getpid) (get-universal-time))
  #+ccl   (namestring (ccl:temp-pathname))
  #+ecl   (ext:mkstemp "update-examples-XXXXXX")
  #+clisp (let ((f (ext:mkstemp nil))) (close f) (namestring f))
  #-(or cmucl sbcl ccl ecl clisp)
  (format nil "/tmp/update-examples-~a" (get-universal-time)))

(defun run-shell-command (cmd)
  "Run CMD in a shell; return the exit code (0 = success)."
  #+cmucl (ext:process-exit-code
           (ext:run-program "/bin/sh" (list "-c" cmd) :wait t))
  #+sbcl  (sb-ext:process-exit-code
           (sb-ext:run-program "/bin/sh" (list "-c" cmd) :wait t))
  #+ccl   (nth-value 1 (ccl:external-process-exit-code
                        (ccl:run-program "/bin/sh" (list "-c" cmd) :wait t)))
  #+ecl   (ext:system cmd)
  #+clisp (ext:shell cmd)
  #-(or cmucl sbcl ccl ecl clisp) (error "run-shell-command not implemented"))

(defun slurp-lines (filename)
  "Return a list of lines from FILENAME (with newlines appended), or NIL on error."
  (handler-case
      (with-open-file (s filename :direction :input)
        (loop for line = (read-line s nil nil)
              while line
              collect (concatenate 'string line (string #\Newline))))
    (error () nil)))

(defun delete-file-if-exists (filename)
  "Delete FILENAME if it exists; return T on success, NIL on failure."
  (handler-case
      (progn (delete-file filename) t)
    (error () nil)))

(defun quit (code)
  "Exit the Lisp process with exit CODE."
  #+cmucl (ext:quit code)
  #+sbcl  (sb-ext:exit :code code)
  #+ccl   (ccl:quit code)
  #+ecl   (ext:quit code)
  #+clisp (ext:exit code)
  #-(or cmucl sbcl ccl ecl clisp) (error "quit not implemented"))


;;; ---------------------------------------------------------------------------
;;; Text::Tabs expand - replace tabs with spaces (tab-stop every 8 columns)
;;; ---------------------------------------------------------------------------

(defun expand-tabs (str &optional (tab-width 8))
  "Expand tab characters in STR to spaces (Text::Tabs expand semantics)."
  (with-output-to-string (out)
    (let ((col 0))
      (loop for ch across str do
        (cond
          ((char= ch #\Tab)
           (let ((spaces (- tab-width (mod col tab-width))))
             (dotimes (_ spaces) (write-char #\Space out))
             (incf col spaces)))
          (t
           (write-char ch out)
           (if (char= ch #\Newline)
               (setf col 0)
               (incf col))))))))

;;; ---------------------------------------------------------------------------
;;; Pre-compiled regexp patterns and constant strings
;;; ---------------------------------------------------------------------------

;;; Maxima wraps each prompt in ^B...^E (ASCII 2 and 5).
;;; The raw output looks like: ^B(%i1) ^E\n(%o1) result\n^B(%i2) ^E
(defparameter *ctrl-b* (string (code-char 2))
  "The ^B character (ASCII 2) that Maxima prints before each input prompt.")
(defparameter *ctrl-e* (string (code-char 5))
  "The ^E character (ASCII 5) that Maxima prints after each input prompt.")

;;; rem-codes patterns: applied to every line of Maxima output to clean it up
;;; for inclusion in texinfo source.
(defparameter *re-ctrl-b* (pregexp:pregexp (string (code-char 2)))
  "Matches the ^B prompt-start marker emitted by Maxima; removed from output.")
(defparameter *re-ctrl-e* (pregexp:pregexp (string (code-char 5)))
  "Matches the ^E prompt-end marker emitted by Maxima; removed from output.")
(defparameter *re-at* (pregexp:pregexp "@")
  "Matches a literal @ sign; replaced with @@ to escape it in texinfo output.")
(defparameter *re-open-brace* (pregexp:pregexp "\\{")
  "Matches a literal { ; replaced with @{ to escape it in texinfo output.")
(defparameter *re-close-brace* (pregexp:pregexp "\\}")
  "Matches a literal } ; replaced with @} to escape it in texinfo output.")

;;; r-trim pattern
(defparameter *re-trailing-ws* (pregexp:pregexp "\\s+$")
  "Matches trailing whitespace at end of string; replaced with a single newline.")

;;; Main loop patterns: tested against every line of the texinfo input file.
(defparameter *re-example-input-beg* (pregexp:pregexp "^@c ===beg===")
  "Marks the start of an example input block in the texinfo source.")
(defparameter *re-example-input-end* (pregexp:pregexp "^@c ===end===")
  "Marks the end of an example input block in the texinfo source.")
(defparameter *re-example-output-end* (pregexp:pregexp "^@end[ \\t]+example[ \\t]*")
  "Matches the @end example line that closes an existing example output block.")
(defparameter *re-at-c* (pregexp:pregexp "@c *")
  "Matches a texinfo comment marker @c (with optional trailing spaces).")
(defparameter *re-at-c-input* (pregexp:pregexp "^@c input:")
  "Matches the @c input: prefix used for explicitly-marked input lines.")

;;; Merge-loop patterns: tested against lines of raw Maxima output.
(defparameter *re-starts-ctrl-b*
  (pregexp:pregexp (concatenate 'string "^" (string (code-char 2))))
  "Matches a line that begins with ^B -- i.e. the start of a new Maxima prompt.")
(defparameter *re-ctrl-e-at-end*
  (pregexp:pregexp (concatenate 'string (string (code-char 5)) "$"))
  "Matches a line whose last character is ^E -- the prompt line ends here,
   with no output following on the same line.")
(defparameter *re-starts-nonws* (pregexp:pregexp "^[\\S]")
  "Matches a line that starts with a non-whitespace character; used to detect
   the end of continuation input lines (which are indented with whitespace).")


;;; ---------------------------------------------------------------------------
;;; rem-codes - mirrors Perl sub rem_codes
;;; ---------------------------------------------------------------------------

(defun rem-codes (str strip-topdir)
  "Remove control codes, strip STRIP-TOPDIR prefix, escape texinfo special
   characters, and expand tabs - exactly as the Perl rem_codes sub does."
  (let* ((res (pregexp:pregexp-replace* *re-ctrl-b* str ""))   ; remove ^B
         (res (pregexp:pregexp-replace* *re-ctrl-e* res ""))   ; remove ^E
         (res (if (and strip-topdir (> (length strip-topdir) 0))
                  (pregexp:pregexp-replace* (pregexp:pregexp-quote strip-topdir) res "")
                  res))
         (res (pregexp:pregexp-replace* *re-at* res "@@"))   ; escape @
         (res (pregexp:pregexp-replace* *re-open-brace* res "@{"))    ; escape {
         (res (pregexp:pregexp-replace* *re-close-brace* res "@}"))    ; escape }
         (res (expand-tabs res)))
    res))

;;; ---------------------------------------------------------------------------
;;; r-trim - mirrors Perl sub r_trim
;;; ---------------------------------------------------------------------------

(defun r-trim (str)
  "Strip trailing whitespace and replace it with a single newline."
  (pregexp:pregexp-replace* *re-trailing-ws* str (string #\Newline)))


;;; ---------------------------------------------------------------------------
;;; Input source - like Perl's diamond operator <>
;;; ---------------------------------------------------------------------------

(defun make-input-stream (filenames)
  "Return a thunk that yields one line at a time from FILENAMES (or
   *standard-input* if FILENAMES is empty), returning NIL at EOF."
  (if (null filenames)
      (lambda () (read-line *standard-input* nil nil))
      (let ((files filenames)
            (current-stream nil))
        (labels ((next-line ()
                   (when (null current-stream)
                     (when (null files) (return-from next-line nil))
                     (setf current-stream
                           (open (pop files) :direction :input
                                             :if-does-not-exist :error)))
                   (let ((line (read-line current-stream nil nil)))
                     (cond
                       (line line)
                       (t (close current-stream)
                          (setf current-stream nil)
                          (next-line))))))
          #'next-line))))

;;; ---------------------------------------------------------------------------
;;; Queue helper: pop from the front of an adjustable vector
;;; ---------------------------------------------------------------------------

(defun vector-pop-front (vec)
  "Remove and return the first element of adjustable fill-pointer vector VEC."
  (let ((val (aref vec 0)))
    (loop for i from 0 below (1- (fill-pointer vec))
          do (setf (aref vec i) (aref vec (1+ i))))
    (decf (fill-pointer vec))
    val))

;;; ---------------------------------------------------------------------------
;;; Main
;;; ---------------------------------------------------------------------------

(defun main (&optional input-file &key maxima-command)
  "Process texinfo source from INPUT-FILE (or *standard-input* if not given),
   replacing example output blocks with fresh Maxima output.
   Writes to *standard-output* and returns the error count.
   :MAXIMA-COMMAND overrides the MAXIMA_EXAMPLE_COMMAND environment variable
   and the default of '../../maxima-local --no-init -p maxima-init.lisp'."
  (let* (;; Determine topdir (mirrors: $topdir = substr(`pwd`, 0, -1); ...)
         (cwd (getcwd))
         (topdir (if (search "/maxima/doc/info" cwd)
                     (pregexp:pregexp-replace* "/maxima/doc/info.*$" cwd "")
                     ""))

         (strip-topdir   (or (getenv "STRIP_TOPDIR") topdir))
         (maxima-command (or maxima-command
                             (getenv "MAXIMA_EXAMPLE_COMMAND")
                             "../../maxima-local --no-init -p maxima-init.lisp"))

         ;; Regexp patterns — use the pre-compiled defparameters
         (example-input-beg  *re-example-input-beg*)
         (example-input-end  *re-example-input-end*)
         (example-output-end *re-example-output-end*)

         ;; State
         (line-cnt    0)
         (error-cnt   0)
         (warning-cnt 0)

         (in-example-input  nil)
         (in-example-output nil)

         (example-input-buf  '())
         (example-output-buf '())

         (next-line (make-input-stream (when input-file (list input-file)))))

    (loop
      (let ((raw-line (funcall next-line)))
        (unless raw-line
          ;; EOF
          (cond
            (in-example-input
             (incf error-cnt)
             (format *error-output*
                     "Error: line ~a - EOF while end of example input is expected.~%"
                     line-cnt))
            (in-example-output
             (incf error-cnt)
             (format *error-output*
                     "Error: line ~a - EOF while end of example is expected.~%"
                     line-cnt)
             (dolist (l (reverse example-output-buf))
               (write-string l))))
          (return))

        (let ((line (if (and (> (length raw-line) 0)
                             (char= (char raw-line (1- (length raw-line))) #\Newline))
                        raw-line
                        (concatenate 'string raw-line (string #\Newline)))))
          (incf line-cnt)

          (cond
            ;; ---- inside @c ===beg=== ... @c ===end=== ----
            (in-example-input
             (cond
               ((pregexp:pregexp-match example-input-end line)
                (write-string line)
                (setf in-example-input  nil)
                (setf in-example-output t))

               ((pregexp:pregexp-match *re-at-c* line)
                (let ((fixed (if (pregexp:pregexp-match *re-at-c-input* line)
                                 (subseq line 9)
                                 (subseq line 3))))
                  (write-string line)
                  (push (r-trim fixed) example-input-buf)))

               (t
                (incf warning-cnt)
                (format *error-output*
                        "Warning: line ~a - example input lines must begin with '@c '.~%"
                        line-cnt)
                (write-string line))))

            ;; ---- inside @example ... @end example ----
            (in-example-output
             (cond
               ((pregexp:pregexp-match example-output-end line)
                (let* ((tempf (tmpnam))
                       (com (with-output-to-string (s)
                              (format s "~a > ~a << \\EOF~%" maxima-command tempf)
                              (dolist (l (reverse example-input-buf))
                                (write-string l s))
                              (format s "EOF"))))

                  (cond
                    ;; Maxima invocation failed
                    ((not (zerop (run-shell-command com)))
                     (incf error-cnt)
                     (format *error-output*
                             "Error: line ~a - maxima invocation failed.~%"
                             line-cnt)
                     (dolist (l (reverse example-output-buf))
                       (write-string l))
                     (write-string "@end example")
                     (write-char #\Newline))

                    (t
                     (let ((result-lines (slurp-lines tempf)))
                       (cond
                         ((null result-lines)
                          (incf error-cnt)
                          (format *error-output*
                                  "Error: line ~a - can't open temp file ~a~%"
                                  line-cnt tempf)
                          (dolist (l (reverse example-output-buf))
                            (write-string l))
                          (write-string "@end example")
                          (write-char #\Newline))

                         (t
                          (unless (delete-file-if-exists tempf)
                            (incf error-cnt)
                            (format *error-output*
                                    "Error: line ~a - can't delete temp file ~a~%"
                                    line-cnt tempf))

                          (write-string "@example maxima")
                          (write-char #\Newline)

                          (let ((result-buf
                                 (make-array (length result-lines)
                                             :adjustable t :fill-pointer t
                                             :initial-contents result-lines))
                                (input-buf
                                 (make-array (length example-input-buf)
                                             :adjustable t :fill-pointer t
                                             :initial-contents (reverse example-input-buf)))
)

                            ;; Skip lines before the first ^B
                            (loop while (and (> (fill-pointer result-buf) 0)
                                            (not (pregexp:pregexp-match
                                                  *re-starts-ctrl-b*
                                                  (aref result-buf 0))))
                                  do (vector-pop-front result-buf))

                            ;; Main merge loop
                            (loop while (and (> (fill-pointer result-buf) 0)
                                            (> (fill-pointer input-buf)  0))
                              do
                              (let ((group '()))

                                ;; Collect result lines up to (but not incl.) ^E
                                (loop while (and (> (fill-pointer result-buf) 0)
                                                (not (pregexp:pregexp-match
                                                      *re-ctrl-e*
                                                      (aref result-buf 0))))
                                      do (let ((coded (rem-codes (vector-pop-front result-buf)
                                                                 strip-topdir)))
                                           (unless (string= coded (string #\Newline))
                                             (push coded group))))

                                ;; Process the line containing ^E (the prompt line)
                                (when (> (fill-pointer input-buf) 0)
                                  (let* ((res-line    (aref result-buf 0))
                                         (ctrl-e-pos  (position (code-char 5) res-line))
                                         (res-part    (rem-codes (subseq res-line 0 ctrl-e-pos)
                                                                  strip-topdir))
                                         (inp-line    (aref input-buf 0))
                                         (inp-part    (rem-codes
                                                       (subseq inp-line 0
                                                               (max 0 (1- (length inp-line))))
                                                       strip-topdir)))
                                    (push (concatenate 'string res-part inp-part
                                                       (string #\Newline))
                                          group)
                                    (vector-pop-front input-buf)
                                    (if (and (pregexp:pregexp-match *re-ctrl-e* res-line)
                                             (not (pregexp:pregexp-match
                                                   *re-ctrl-e-at-end*
                                                   res-line)))
                                        (setf (aref result-buf 0)
                                              (subseq res-line ctrl-e-pos))
                                        (vector-pop-front result-buf))))

                                ;; Drain continuation input lines (start with whitespace)
                                (loop while (and (> (fill-pointer input-buf) 0)
                                                (not (pregexp:pregexp-match
                                                      *re-starts-nonws*
                                                      (aref input-buf 0))))
                                      do (let* ((l       (aref input-buf 0))
                                                (stripped (subseq l 0 (max 0 (1- (length l))))))
                                           (push (concatenate 'string
                                                              (rem-codes stripped strip-topdir)
                                                              (string #\Newline))
                                                 group)
                                           (vector-pop-front input-buf)))

                                ;; Drain result lines until next ^B
                                (loop while (and (> (fill-pointer result-buf) 0)
                                                (not (pregexp:pregexp-match
                                                      *re-starts-ctrl-b*
                                                      (aref result-buf 0))))
                                      do (let ((coded (rem-codes (vector-pop-front result-buf)
                                                                 strip-topdir)))
                                           ;; Skip blank lines (Maxima emits one after input echo)
                                           (unless (string= coded (string #\Newline))
                                             (push coded group))))

                                (let ((group-lines (reverse group)))
                                  (when (>= (length group-lines) 2)
                                    (write-string "@group")
                                    (write-char #\Newline))
                                  (dolist (gl group-lines)
                                    (write-string gl))
                                  (when (>= (length group-lines) 2)
                                    (write-string "@end group")
                                    (write-char #\Newline)))))

                            (write-string "@end example")
                            (write-char #\Newline)))))))

                  ;; Reset state
                  (setf example-input-buf  '())
                  (setf example-output-buf '())
                  (setf in-example-output  nil)))

               (t
                (push line example-output-buf))))

            ;; ---- scanning for @c ===beg=== ----
            ((pregexp:pregexp-match example-input-beg line)
             (write-string line)
             (setf in-example-input t))

            ;; ---- plain line, pass through ----
            (t
             (write-string line))))))

    error-cnt))

