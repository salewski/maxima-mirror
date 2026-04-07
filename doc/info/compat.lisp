;;; compat.lisp — implementation portability shim
;;;
;;; Provides clean, implementation-neutral functions for every operation that
;;; would otherwise require #+impl / #-impl reader conditionals scattered
;;; through the main scripts.  Load this file once before loading either
;;; build-index.lisp or update-examples.lisp.
;;;
;;; Unicode / character-width summary
;;; -----------------------------------
;;;   SBCL   — Unicode; file-position on UTF-8 stream returns byte offset.
;;;   CCL    — Unicode; file-position on UTF-8 stream returns byte offset.
;;;   ECL    — Unicode; file-position on UTF-8 stream returns byte offset.
;;;   CMUCL  — Unicode (UTF-16 internally) since 20a.  Requires >=21e.
;;;             Default external format is :utf-8 since 21e, so no explicit
;;;             encoding setup is needed.  file-position on a UTF-8 stream
;;;             returns byte offset (fixed in 20e).
;;;   CLISP  — Unicode; file-position on a character stream returns a
;;;             character offset, not a byte offset.  The Maxima info files
;;;             are ASCII/Latin-1 in practice, so char == byte holds.
;;;   GCL    — 8-bit only; no encoding concept; char == byte throughout.

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require :sb-posix))

;;; ---------------------------------------------------------------------------
;;; Process / environment
;;; ---------------------------------------------------------------------------

(defun getenv (name)
  "Return the value of environment variable NAME, or NIL if unset."
  #+sbcl  (sb-posix:getenv name)
  #+ccl   (ccl:getenv name)
  #+ecl   (si:getenv name)
  #+clisp (ext:getenv name)
  #+cmu   (unix:unix-getenv name)
  #+gcl   (si:getenv name)
  #-(or sbcl ccl ecl clisp cmu gcl)
  (error "getenv: not implemented for this CL implementation"))

(defun getcwd ()
  "Return the current working directory as a namestring (no trailing slash)."
  #+sbcl  (namestring (sb-posix:getcwd))
  #+ccl   (namestring (ccl:current-directory))
  #+ecl   (namestring (si::getcwd))
  #+clisp (namestring (ext:default-directory))
  #+cmu   (namestring (ext:default-directory))
  ;; GCL: no direct binding; prefer $PWD, fall back to a subprocess
  #+gcl   (or (si:getenv "PWD")
              (let ((tmp (format nil "/tmp/gcl-pwd-~a" (get-universal-time))))
                (si:system (format nil "pwd > ~a" tmp))
                (with-open-file (s tmp :direction :input)
                  (prog1 (read-line s nil "")
                    (delete-file tmp)))))
  #-(or sbcl ccl ecl clisp cmu gcl)
  (error "getcwd: not implemented for this CL implementation"))

(defun run-shell-command (cmd)
  "Run CMD via /bin/sh.  Return the integer exit code (0 = success)."
  #+sbcl
  (sb-ext:process-exit-code
   (sb-ext:run-program "/bin/sh" (list "-c" cmd) :wait t))
  #+ccl
  (nth-value 1 (ccl:external-process-status
                (ccl:run-program "/bin/sh" (list "-c" cmd) :wait t)))
  #+ecl   (ext:system cmd)
  #+clisp (ext:run-shell-command cmd)
  #+cmu
  (ext:process-exit-code
   (ext:run-program "/bin/sh" (list "-c" cmd) :wait t))
  #+gcl   (si:system cmd)
  #-(or sbcl ccl ecl clisp cmu gcl)
  (error "run-shell-command: not implemented for this CL implementation"))

(defun tmpnam ()
  "Return a fresh temporary file path that does not yet exist."
  #+sbcl (sb-posix:mktemp "/tmp/cl-maxima-XXXXXX")
  #+ccl  (namestring (ccl:temp-pathname))
  ;; ECL, CLISP, CMUCL, GCL: construct a name and verify non-existence
  #+(or ecl clisp cmu gcl)
  (loop for candidate = (format nil "/tmp/cl-maxima-~a-~a"
                                (get-universal-time) (random 999999))
        unless (probe-file candidate) return candidate)
  #-(or sbcl ccl ecl clisp cmu gcl)
  (format nil "/tmp/cl-maxima-~a" (get-universal-time)))

(defun quit (&optional (code 0))
  "Exit the process with CODE (default 0)."
  #+sbcl  (sb-ext:exit :code code)
  #+ccl   (ccl:quit code)
  #+ecl   (ext:quit code)
  #+clisp (ext:quit code)
  #+cmu   (ext:quit code)
  #+gcl   (si:quit code)
  #-(or sbcl ccl ecl clisp cmu gcl)
  (error "quit: not implemented for this CL implementation"))

;;; ---------------------------------------------------------------------------
;;; Standard-output encoding
;;; ---------------------------------------------------------------------------

(defun set-stdout-utf-8 ()
  "Ensure *standard-output* and *error-output* use UTF-8 encoding where the
   implementation's default is not already UTF-8."
  #+sbcl
  (progn
    (setf (sb-impl::fd-stream-external-format *standard-output*) :utf-8)
    (setf (sb-impl::fd-stream-external-format *error-output*)    :utf-8))
  #+ccl
  (let ((utf8 (ccl:make-external-format :character-encoding :utf-8)))
    (setf (ccl:stream-external-format *standard-output*) utf8)
    (setf (ccl:stream-external-format *error-output*)    utf8))
  #+ecl
  (progn
    (ext:set-stream-external-format *standard-output* :utf-8)
    (ext:set-stream-external-format *error-output*    :utf-8))
  #+clisp
  (progn
    (setf (stream-external-format *standard-output*) charset:utf-8)
    (setf (stream-external-format *error-output*)    charset:utf-8))
  ;; CMUCL >=21e: default external format is already :utf-8 — nothing to do.
  #+cmu nil
  ;; GCL: 8-bit streams, no encoding concept — nothing to do.
  #+gcl nil
  #-(or sbcl ccl ecl clisp cmu gcl) nil)

;;; ---------------------------------------------------------------------------
;;; UTF-8 / character file I/O
;;; ---------------------------------------------------------------------------

(defun open-utf-8-input (path)
  "Open PATH for character input.  On CMUCL >=21e and GCL no :external-format
   argument is passed (the default is already correct for each).  The caller
   must close the returned stream."
  #+sbcl  (open path :direction :input :external-format :utf-8)
  #+ccl   (open path :direction :input
                     :external-format (ccl:make-external-format
                                       :character-encoding :utf-8))
  #+ecl   (open path :direction :input :external-format :utf-8)
  #+clisp (open path :direction :input :external-format charset:utf-8)
  ;; CMUCL >=21e: default is already :utf-8.
  ;; GCL: 8-bit, no :external-format accepted.
  #+(or cmu gcl) (open path :direction :input)
  #-(or sbcl ccl ecl clisp cmu gcl) (open path :direction :input))

(defun read-entire-file (path)
  "Return the entire contents of PATH as a string."
  (let ((s (open-utf-8-input path)))
    (unwind-protect
         (let* ((len (file-length s))
                (buf (make-string len))
                (n   (read-sequence buf s)))
           (subseq buf 0 n))
      (close s))))

(defun file-byte-length (path)
  "Return the byte length of PATH."
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (file-length s)))

(defun tell-bytes-after-chars (path n-chars)
  "Open PATH, consume N-CHARS characters, and return the resulting byte offset.

   SBCL, CCL, ECL, CMUCL: file-position on a UTF-8 character stream returns
   the byte offset directly.

   CLISP: file-position returns a character offset.  The Maxima info files are
   ASCII/Latin-1 so character offset == byte offset in practice.

   GCL: 8-bit, so character offset == byte offset by definition."
  #+(or sbcl ccl ecl cmu)
  (let ((s (open-utf-8-input path)))
    (unwind-protect
         (progn
           (when (> n-chars 0)
             (let ((buf (make-string n-chars)))
               (read-sequence buf s)))
           (file-position s))
      (close s)))
  ;; CLISP and GCL: identity mapping (char offset == byte offset)
  #-(or sbcl ccl ecl cmu) n-chars)

(defun read-file-from-byte-offset (path byte-offset)
  "Return the contents of PATH starting at BYTE-OFFSET as a string.
   If BYTE-OFFSET is at or beyond EOF, returns an empty string."
  (let* ((file-size (file-byte-length path))
         (byte-offset (max 0 (min byte-offset file-size))))
    (let ((s (open-utf-8-input path)))
      (unwind-protect
           (progn
             (file-position s byte-offset)
             (let* ((remaining (- file-size byte-offset))
                    (buf (make-string remaining))
                    (n   (read-sequence buf s)))
               (subseq buf 0 n)))
        (close s)))))

;;; ---------------------------------------------------------------------------
;;; Misc file utilities
;;; ---------------------------------------------------------------------------

(defun slurp-lines (path)
  "Return the lines of PATH as a list of newline-terminated strings,
   or NIL if the file does not exist."
  (when (probe-file path)
    (with-open-file (s path :direction :input :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              while line
              collect (concatenate 'string line (string #\Newline)))))))

(defun delete-file-if-exists (path)
  "Delete PATH if it exists.  Returns T on success, NIL if absent."
  (when (probe-file path)
    (delete-file path)
    t))
