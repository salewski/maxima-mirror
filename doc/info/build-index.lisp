;;; Common Lisp translation of build_index.pl
;;; Requires: pregexp  (https://ds26gte.github.io/pregexp/index.html)
;;;
;;; Usage: (build-index "maxima.info")
;;;        (build-index "maxima.info" :pprint t)
;;;        (build-index "maxima.info" :info-installation-path "/usr/share/info/")
;;;
;;; Output format:
;;;
;;;   (in-package :cl-info)
;;;   (let ((deffn-defvr-pairs '(...))
;;;         (section-pairs '(...)))
;;;     (load-info-hashtables <path> deffn-defvr-pairs section-pairs))
;;;
;;; deffn-defvr-pairs is an alist mapping index topic strings to
;;; (filename byte-offset nchars node-name), where byte-offset is the
;;; byte position in the file of the item text, and nchars is the
;;; character length of the item text.
;;;
;;; section-pairs is an alist mapping section title strings to
;;; (filename byte-offset nchars), where byte-offset is the byte
;;; position of the section heading line and nchars is the character
;;; length of the section content up to the next node separator.

;;; ---------------------------------------------------------------------------
;;; I/O helpers
;;; ---------------------------------------------------------------------------

(defvar *bytes-cache* (make-hash-table :test #'equal)
  "Cache of filename -> byte vector to avoid reading the same file multiple times.")

(defun slurp-bytes (filename)
  "Return the entire contents of FILENAME as a (unsigned-byte 8) vector.
   Results are cached in *bytes-cache* so each file is read only once."
  (or (gethash filename *bytes-cache*)
      (setf (gethash filename *bytes-cache*)
            (with-open-file (s filename :direction :input
                                        :element-type '(unsigned-byte 8))
              (let* ((len (file-length s))
                     (buf (make-array len :element-type '(unsigned-byte 8))))
                (read-sequence buf s)
                buf)))))

;;; UTF-8 decoder for implementations that lack stream:octets-to-string.
;;; No error checking -- assumes the input is always valid UTF-8.
(defun utf8-bytes-to-string (bytes start end)
  (let* ((n   (- end start))
         (out (make-array n :element-type 'character :fill-pointer 0)))
    (let ((i start))
      (loop while (< i end)
            do (let ((b (aref bytes i)))
                 (vector-push
                  (code-char
                   (cond
                     ((< b #x80)                    ; 1-byte (ASCII)
                      (incf i)
                      b)
                     ((< b #xE0)                    ; 2-byte
                      (prog1 (logior (ash (logand b #x1F) 6)
                                     (logand (aref bytes (1+ i)) #x3F))
                        (incf i 2)))
                     ((< b #xF0)                    ; 3-byte
                      (prog1 (logior (ash (logand b #x0F) 12)
                                     (ash (logand (aref bytes (1+ i)) #x3F) 6)
                                     (logand (aref bytes (+ i 2)) #x3F))
                        (incf i 3)))
                     (t                             ; 4-byte
                      (prog1 (logior (ash (logand b #x07) 18)
                                     (ash (logand (aref bytes (1+ i)) #x3F) 12)
                                     (ash (logand (aref bytes (+ i 2)) #x3F) 6)
                                     (logand (aref bytes (+ i 3)) #x3F))
                        (incf i 4)))))
                  out))))
    (coerce out 'string)))

(defun bytes-to-string (bytes &optional (start 0) end)
  "Convert a region of a (unsigned-byte 8) vector to a UTF-8 string."
  (let ((end (or end (length bytes))))
    #+cmucl (stream:octets-to-string bytes :start start :end end :external-format :utf-8)
    #+sbcl  (sb-ext:octets-to-string bytes :start start :end end :external-format :utf-8)
    #+ccl   (ccl:decode-string-from-octets bytes :start start :end end :external-format :utf-8)
    #+ecl   (ext:octets-to-string bytes :start start :end end :external-format :utf-8)
    #+clisp (ext:convert-string-from-bytes bytes charset:utf-8 :start start :end end)
    #-(or cmucl sbcl ccl ecl clisp) (utf8-bytes-to-string bytes start end)))

;;; ---------------------------------------------------------------------------
;;; Regexp helpers built on pregexp
;;; ---------------------------------------------------------------------------

(defun re-match (pattern string &optional (start 0))
  (pregexp:pregexp-match pattern string start))

(defun re-match-positions (pattern string &optional (start 0))
  (pregexp:pregexp-match-positions pattern string start))

(defun re-search-all (pattern string)
  "Return a list of all non-overlapping matches of PATTERN in STRING."
  (let ((results '())
        (pos 0)
        (len (length string)))
    (loop
      (let ((m (pregexp:pregexp-match-positions pattern string pos)))
        (unless m (return))
        (let* ((whole (car m))
               (start (car whole))
               (end   (cdr whole)))
          (push (mapcar (lambda (p)
                          (if p (subseq string (car p) (cdr p)) nil))
                        m)
                results)
          ;; Advance past match; step 1 if zero-length to avoid infinite loop.
          (setf pos (if (> end start) end (1+ end)))
          (when (>= pos len) (return)))))
    (nreverse results)))

(defun re-replace-all (pattern string replacement)
  (pregexp:pregexp-replace* pattern string replacement))

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defparameter *us-byte* 31
  "The GNU info unit-separator byte (ASCII 31, ^_). Each info node
   is preceded by a newline and this byte in the file.")

;;; ---------------------------------------------------------------------------
;;; Global state
;;; ---------------------------------------------------------------------------

(defvar *main-info* nil
  "Filename of the top-level info file (e.g. \"maxima.info\").")
(defvar *info-installation-path* nil
  "Directory where info files are installed, embedded in the output.
   When NIL the output uses (maxima::maxima-load-pathname-directory).")
(defvar *makeinfo-major-version* nil
  "Major version of makeinfo that built the info files,
   detected from the header of the main info file.")
(defvar *makeinfo-minor-version* nil
  "Minor version of makeinfo that built the info files.")
(defvar *info-filenames* '()
  "List of all info filenames (main file plus maxima.info-N files),
   populated by part-1-1a.")
(defvar *node-offset*   (make-hash-table :test #'equal)
  "Maps node-name -> (filename byte-offset). byte-offset is the byte
   position of the \\n just before the unit separator that precedes
   the node -- the starting point for line-skipping in part-1-2.")
(defvar *topic-locator* (make-hash-table :test #'equal)
  "Maps index topic string -> (node-name filename byte-offset nchars).
   Initially populated by part-1-1b with (node-name lines-offset),
   then resolved to absolute byte offsets and lengths by part-1-2.")
(defvar *node-locator*  (make-hash-table :test #'equal)
  "Maps section title -> (filename byte-offset nchars). byte-offset is
   the start of the N.M heading line; nchars is the character length up
   to the next unit separator.")
(defvar *item-cnt*    0
  "Count of deffn/defvr entries emitted; used for the empty-index warning.")
(defvar *section-cnt* 0
  "Count of section heading entries emitted; used for the empty-index warning.")

(defvar *verbose* nil
  "Verbosity level for build-index progress messages written to *error-output*.
   NIL = silent, T or 1 = high-level phase messages, 2 = per-file detail.")

(defun verbose-p (level)
  "Return true if the current verbosity level is at least LEVEL."
  (cond ((eq *verbose* t) t)
        ((integerp *verbose*) (>= *verbose* level))
        (t nil)))

(defun progress (level fmt &rest args)
  "Print a progress message to *error-output* if verbosity >= LEVEL."
  (when (verbose-p level)
    (apply #'format *error-output* fmt args)
    (finish-output *error-output*)))


;;; ---------------------------------------------------------------------------
;;; Pre-compiled regexp patterns
;;; ---------------------------------------------------------------------------

(defparameter *re-deffn-marker*
  (pregexp:pregexp "^ -- \\S")
  "Matches a @deffn/@defvr definition line: a line starting with
   ' -- ' followed by a non-whitespace character, e.g.
     -- Function: foo (x)
   Used only in the makeinfo 4.x workaround in part-1-2.")

(defparameter *re-index-entry*
  (pregexp:pregexp "\\* (?!Menu)(\\S+|[^:]+):\\s+(.*?)\\.\\s+\\(line\\s+(\\d+)\\)")
  "Matches a single index entry line in the info index node, e.g.:
     * foo:                          Some Node.   (line  42)
   Group 1: the index topic (e.g. \"foo\").
   Group 2: the node name  (e.g. \"Some Node\").
   Group 3: the line offset within the node (e.g. \"42\").
   The (?!Menu) negative lookahead skips the '* Menu:' separator lines.")

(defparameter *re-escape-quote*
  (pregexp:pregexp "\"")
  "Matches a double-quote character; used to escape quotes in index
   topic and section title strings when writing the output.")

;;; ---------------------------------------------------------------------------
;;; PART 1 - Build index for @deffn and @defvr items
;;; ---------------------------------------------------------------------------

;;; (1.1a) Scan *.info* files; populate *node-offset*.

(defparameter *node-prefix-bytes*
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code "Node: ")
  "The ASCII bytes of \"Node: \", pre-allocated for fast byte-level
   scanning of info file node headers without string allocation.")

(defparameter *file-prefix-bytes*
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code "File: ")
  "The ASCII bytes of \"File: \", pre-allocated for fast byte-level
   scanning of info file node headers without string allocation.")

(defun parse-node-name-from-header (bytes start end)
  "Parse the node name from a File:/Node: header in BYTES[START..END).
   Real node headers have File: and Node: on the same line.
   Tag table entries have bare Node: lines without File: -- we skip those.
   Returns the node name string, or NIL if not found."
  ;; Outer loop: scan byte positions looking for the start of "File: ".
  ;; We require "File: " before accepting "Node: " because tag table
  ;; entries have bare "Node: FooNNN" lines with no "File: " prefix,
  ;; and we must not mistake them for real node headers.
  ;; When "File: " is found we process its line and immediately return --
  ;; there is only one File:/Node: header per region.
  (loop for i from start below (- end 6)
        when (loop for j from 0 below 6   ; does bytes[i..i+5] == "File: "?
                   always (= (aref bytes (+ i j)) (aref *file-prefix-bytes* j)))
        return (let* (;; Find the newline ending the File:/Node: header line.
                      (line-end (or (position (char-code #\Newline) bytes
                                              :start (+ i 6) :end end)
                                   end))
                      ;; Inner loop: scan the same line for "Node: ".
                      ;; Returns the byte index of the N in "Node: ", or NIL.
                      (node-pos (loop for k from (+ i 6) below (max (+ i 6) (- line-end 6))
                                      when (loop for j from 0 below 6
                                                 always (= (aref bytes (+ k j))
                                                            (aref *node-prefix-bytes* j)))
                                      return k
                                      finally (return nil))))
                 (when node-pos
                   (let* ((name-start (+ node-pos 6))  ; skip past "Node: "
                          ;; Innermost loop: find end of node name.
                          ;; The name is terminated by a comma or newline,
                          ;; e.g. "Node: Foo,  Next: Bar" yields "Foo".
                          (name-end   (loop for k from name-start below line-end
                                            when (let ((b (aref bytes k)))
                                                   (or (= b (char-code #\,))
                                                       (= b (char-code #\Newline))))
                                            return k
                                            finally (return line-end))))
                     ;; Header is ASCII so code-char is safe here.
                     (map 'string #'code-char (subseq bytes name-start name-end)))))
        finally (return nil)))

(defun scan-info-file-for-nodes (filename bytes)
  "Scan BYTES (raw contents of FILENAME) for unit-separator boundaries and
   File:/Node: headers.  Populates *node-offset* with (filename byte-offset)
   for each node.  byte-offset is the byte position Perl ends up at after
   'read FH, $buf, $nl-char-pos' with UTF-8 encoding -- i.e. the byte
   offset of the newline just before the unit separator.

   Works entirely in bytes: the US byte (31) and the surrounding \\n and
   File:/Node: header are all ASCII, so no UTF-8 decoding is needed.
   The nl-byte-pos stored is the byte position corresponding to reading
   nl-char-pos characters from the start of the file, which we compute
   by walking the byte array one UTF-8 character at a time."
  (let ((last-node-name nil)
        (blen           (length bytes))
        (pos            0)
        (char-count     0)
        (nl-char-count  0)
        (nl-byte-pos    0))
    (loop
      (let ((us-pos (position *us-byte* bytes :start pos)))
        (unless us-pos (return last-node-name))
        (let ((nl (if (and (> us-pos 0)
                           (= (aref bytes (1- us-pos)) #x0A))
                      (1- us-pos)
                      us-pos)))
          ;; Walk pos forward to nl byte by byte, counting UTF-8 characters
          ;; to keep char-count in sync -- we need both the byte position and
          ;; the character offset (what Perl's pos() returns).
          ;; Fast path: skip ASCII runs using position-if.
          (loop while (< pos nl)
                do (let ((next-high (position-if (lambda (b) (>= b #x80))
                                                 bytes :start pos :end nl)))
                     (cond
                       ((null next-high)
                        ;; Rest of run to nl is ASCII: count = bytes.
                        (incf char-count (- nl pos))
                        (setf pos nl))
                       ((> next-high pos)
                        ;; ASCII run from pos to next-high.
                        (incf char-count (- next-high pos))
                        (setf pos next-high))
                       (t
                        ;; Multi-byte sequence.
                        (incf char-count)
                        (incf pos (let ((b (aref bytes pos)))
                                    (cond ((< b #xE0) 2)
                                          ((< b #xF0) 3)
                                          (t          4))))))))
          (setf nl-byte-pos   pos)
          (setf nl-char-count char-count)
          (let* ((header-start (1+ us-pos))
                 (next-us      (or (position *us-byte* bytes :start header-start) blen))
                 (header-end   (min next-us (+ us-pos 512)))
                 (node-name    (parse-node-name-from-header bytes header-start header-end)))
            (when node-name
              (setf last-node-name node-name))
            (when last-node-name
              (setf (gethash last-node-name *node-offset*)
                    (list filename nl-byte-pos))))
          (setf pos        (1+ us-pos))
          (setf char-count (1+ nl-char-count)))))))

(defun part-1-1a ()
  "Scan the main info file and all subsidiary *.info-N files.
   Populates *info-filenames* and *node-offset*.
   Returns the last node name seen across all files (the index node)."
  (let* ((main-bytes     (slurp-bytes *main-info*))
         (second-us      (let ((u1 (position *us-byte* main-bytes)))
                           (if u1
                               (or (position *us-byte* main-bytes :start (1+ u1))
                                   (length main-bytes))
                               (length main-bytes))))
         (header-str     (bytes-to-string main-bytes 0 (min second-us 8192)))
         (last-node-name nil))

    (let ((m (re-match "makeinfo version (\\d+)\\.(\\d+)" header-str)))
      (when m
        (setf *makeinfo-major-version* (parse-integer (cadr m)))
        (setf *makeinfo-minor-version* (parse-integer (caddr m)))))

    (push *main-info* *info-filenames*)
    (progress 2 ";   scanning ~a (~a bytes)...~%" *main-info* (length main-bytes))
    (setf last-node-name (scan-info-file-for-nodes *main-info* main-bytes))

    (let* ((pattern (concatenate 'string
                                 (re-replace-all "\\." *main-info* "\\.")
                                 "-(\\d+): (\\d+)"))
           (matches (re-search-all pattern header-str)))
      (dolist (match matches)
        (let ((sub-filename (format nil "~a-~a" *main-info* (cadr match))))
          (push sub-filename *info-filenames*)
          (let ((sub-bytes (slurp-bytes sub-filename)))
            (progress 2 ";   scanning ~a (~a bytes)...~%" sub-filename (length sub-bytes))
            (let ((n (scan-info-file-for-nodes sub-filename sub-bytes)))
              (when n (setf last-node-name n)))))))

    last-node-name))

;;; (1.1b) Read the info index (last node); populate *topic-locator*.

(defun part-1-1b (index-node-name)
  "Read the index node INDEX-NODE-NAME and populate *topic-locator*
   with (node-name lines-offset) entries."
  (let* ((loc            (gethash index-node-name *node-offset*))
         (index-filename (car loc))
         (node-byte-pos  (cadr loc))
         (index-bytes    (slurp-bytes index-filename))
         (blen           (length index-bytes))
         ;; node-byte-pos is the \n before the US; skip past the US itself
         ;; to find the end of this node.
         (next-us        (or (position *us-byte* index-bytes :start (+ node-byte-pos 2)) blen))
         (content        (bytes-to-string index-bytes node-byte-pos next-us))
         (node-start
          (let ((m (re-match-positions
                    (format nil "File:.*?Node: ~a" index-node-name)
                    content)))
            (if m (cdr (car m)) 0))))
    (dolist (match (re-search-all
                    *re-index-entry*
                    (subseq content node-start)))
      (let ((topic-name   (cadr match))
            (node-name    (caddr match))
            (lines-offset (parse-integer (cadddr match))))
        (setf (gethash topic-name *topic-locator*)
              (list node-name lines-offset))))))

;;; (1.2) Resolve (node-name, lines-offset) -> (node-name, filename, byte-offset, length).

(defun count-chars (bytes start end)
  "Count the number of UTF-8 characters encoded in BYTES from START to END.
   Uses position to skip ASCII runs quickly -- most info file content is ASCII."
  ;; If there are no high bytes at all, char count == byte count.
  (if (null (position-if (lambda (b) (>= b #x80)) bytes :start start :end end))
      (- end start)
      ;; Otherwise walk byte by byte, but skip ASCII runs with position.
      (let ((pos start)
            (n   0))
        (loop while (< pos end)
              do (let ((next-high (position-if (lambda (b) (>= b #x80))
                                               bytes :start pos :end end)))
                   (cond
                     ((null next-high)
                      ;; Rest is ASCII.
                      (incf n (- end pos))
                      (setf pos end))
                     ((> next-high pos)
                      ;; ASCII run from pos to next-high.
                      (incf n (- next-high pos))
                      (setf pos next-high))
                     (t
                      ;; Multi-byte sequence at pos.
                      (incf n)
                      (incf pos (let ((b (aref bytes pos)))
                                  (cond ((< b #xE0) 2)
                                        ((< b #xF0) 3)
                                        (t          4))))))))
        n)))

(defun item-text-length (bytes start)
  "Return the character length of the item text starting at byte offset START
   in BYTES, matching what Perl computes with length($1).
   Scans bytes directly for the termination conditions."
  ;; Replicates Perl: (.*?)(?:\n\n(?= -- )|\n(?=[0-9])|(?=\x1f))
  ;; with /s (dot matches newlines).  Three terminators:
  ;;   \x1f        -- unit separator: end of node
  ;;   \n[0-9]     -- next item number begins
  ;;   \n\n<space>--<space> -- next @deffn/@defvr begins
  (let ((pos  start)
        (blen (length bytes)))
    (loop
      (when (>= pos blen)
        (return (count-chars bytes start blen)))
      (let ((b (aref bytes pos)))
        (cond
          ((= b *us-byte*)           ; end of node
           (return (count-chars bytes start pos)))
          ((= b #x0A)
           (let ((next (if (< (1+ pos) blen) (aref bytes (1+ pos)) 0)))
             (cond
               ((and (>= next (char-code #\0)) (<= next (char-code #\9))) ; \n[0-9]
                (return (count-chars bytes start pos)))
               ((= next #x0A)        ; \n\n -- check for " -- "
                (let ((after (+ pos 2)))
                  (when (and (< (+ after 3) blen)
                             (= (aref bytes after)       (char-code #\Space))
                             (= (aref bytes (+ after 1)) (char-code #\-))
                             (= (aref bytes (+ after 2)) (char-code #\-))
                             (= (aref bytes (+ after 3)) (char-code #\Space)))
                    (return (count-chars bytes start pos)))))
               (t nil))))
          (t nil)))
      (incf pos))))

(defun read-lines-from-bytes (bytes start count)
  "Skip COUNT newline-terminated lines in BYTES starting at byte offset START.
   Returns the byte offset after the last skipped newline."
  (let ((pos start)
        (len (length bytes)))
    (dotimes (_ count pos)
      (let ((nl (position #x0A bytes :start pos)))
        (setf pos (if nl (1+ nl) len))))))

(defun part-1-2 ()
  "For every entry in *topic-locator*, resolve to an absolute byte offset
   and compute the length of the item text.  Each file is slurped once.
   Within each file, entries are sorted by (node-byte-offset, lines-offset)
   so that line-skipping is done incrementally -- each line is visited once."
  (let ((entries '()))
    (maphash (lambda (key val)
               (destructuring-bind (node-name lines-offset) val
                 (let ((loc (gethash node-name *node-offset*)))
                   (when loc
                     (push (list key node-name (car loc) (cadr loc) lines-offset)
                           entries)))))
             *topic-locator*)

    (let ((by-file (make-hash-table :test #'equal)))
      (dolist (e entries)
        (push e (gethash (third e) by-file)))

      (maphash
       (lambda (filename file-entries)
         ;; Sort by node-byte-offset then lines-offset so we can skip
         ;; lines incrementally within each node rather than re-seeking.
         (let* ((sorted (sort file-entries
                              (lambda (a b)
                                (or (< (fourth a) (fourth b))
                                    (and (= (fourth a) (fourth b))
                                         (< (fifth a) (fifth b)))))))
                (bytes  (slurp-bytes filename))
                (cur-node-offset -1)
                (cur-lines-done   0)
                (cur-byte-pos     0))
           (progress 2 ";   resolving ~a entries in ~a...~%"
                     (length sorted) filename)
           (let ((entry-cnt 0))
           (dolist (e sorted)
             (incf entry-cnt)
             (when (and (verbose-p 2) (zerop (mod entry-cnt 100)))
               (progress 2 ";     ~a/~a entries done...~%" entry-cnt (length sorted)))
             (destructuring-bind (key node-name _filename node-byte-offset lines-offset)
                 e
               (declare (ignore _filename))
               (let ((item-byte-offset
                      (cond
                        ;; Makeinfo 4.x bug workaround
                        ((and *makeinfo-major-version*
                              (= *makeinfo-major-version* 4))
                         (let ((x          -1)
                               (line-start node-byte-offset)
                               (blen       (length bytes)))
                           (loop for i from 1 to (1+ lines-offset)
                                 do (let* ((line-end (or (position #x0A bytes :start line-start)
                                                         blen))
                                           (line-str (bytes-to-string bytes line-start line-end)))
                                      (when (re-match *re-deffn-marker* line-str)
                                        (setf x line-start))
                                      (setf line-start (min blen (1+ line-end)))))
                           (if (= x -1) line-start x)))
                        ;; Normal path (makeinfo 5+): skip lines incrementally.
                        (t
                         (when (/= node-byte-offset cur-node-offset)
                           (setf cur-node-offset node-byte-offset)
                           (setf cur-lines-done   0)
                           (setf cur-byte-pos     node-byte-offset))
                         (setf cur-byte-pos
                               (read-lines-from-bytes bytes cur-byte-pos
                                                      (- lines-offset cur-lines-done)))
                         (setf cur-lines-done lines-offset)
                         cur-byte-pos))))
                 (let ((text-length (item-text-length bytes item-byte-offset)))
                   (setf (gethash key *topic-locator*)
                         (list node-name filename item-byte-offset text-length)))))))))
       by-file))))

;;; (1.3) Collect deffn/defvr pairs as an alist.

(defun part-1-3 ()
  "Return an alist of (key . (filename byte-offset nchars node-name)) for all
   deffn/defvr entries, sorted by key."
  (let ((keys (sort (loop for k being the hash-keys of *topic-locator* collect k)
                    #'string<))
        (pairs '()))
    (dolist (key keys)
      (incf *item-cnt*)
      (let* ((entry       (gethash key *topic-locator*))
             (node-name   (first  entry))
             (file-name   (second entry))
             (byte-offset (third  entry))
             (nchars      (fourth entry))
             (bad?        (or (string= key "")
                              (string= file-name "")
                              (< byte-offset 0)
                              (< nchars 0)
                              (string= node-name ""))))
        (when bad?
          (format *error-output*
                  "build-index.lisp: bad entry for key=~s~%" key))
        (push (cons key (list file-name byte-offset nchars node-name)) pairs)))
    (nreverse pairs)))


;;; ---------------------------------------------------------------------------
;;; PART 2 - Build index for @node items (section headings "N.M Title")
;;; ---------------------------------------------------------------------------

(defun ascii-digit-p (b)
  (and (>= b (char-code #\0)) (<= b (char-code #\9))))

(defun match-section-heading (bytes blen pos)
  "If BYTES at byte offset POS starts with N.M<space> (ASCII), return the
   byte index of the first title character.  Otherwise return NIL."
  (let ((i pos))
    (unless (and (< i blen) (ascii-digit-p (aref bytes i)))
      (return-from match-section-heading nil))
    (loop while (and (< i blen) (ascii-digit-p (aref bytes i))) do (incf i))
    (unless (and (< i blen) (= (aref bytes i) (char-code #\.)))
      (return-from match-section-heading nil))
    (incf i)
    (unless (and (< i blen) (ascii-digit-p (aref bytes i)))
      (return-from match-section-heading nil))
    (loop while (and (< i blen) (ascii-digit-p (aref bytes i))) do (incf i))
    (unless (and (< i blen) (= (aref bytes i) (char-code #\Space)))
      (return-from match-section-heading nil))
    (1+ i)))

(defun part-2-1 ()
  "Scan all info files for section headings of the form 'N.M Title'.
   Populates *node-locator* with (filename byte-offset length) entries."
  (dolist (filename *info-filenames*)
    (progress 2 ";   scanning sections in ~a...~%" filename)
    (let* ((bytes (slurp-bytes filename))
           (blen  (length bytes))
           (pos   0))
      (loop
        (when (>= pos blen) (return))
        (let ((nl (position #x0A bytes :start pos)))
          (unless nl (return))
          (let* ((heading-start (1+ nl))
                 (title-start   (match-section-heading bytes blen heading-start)))
            (cond
              (title-start
               (let* ((title-end   (or (position #x0A bytes :start title-start) blen))
                      (node-title  (let ((s (make-string (- title-end title-start))))
                                     (dotimes (i (- title-end title-start) s)
                                       (setf (char s i)
                                             (code-char (aref bytes (+ title-start i)))))))
                      (us-pos      (or (position *us-byte* bytes :start heading-start) blen))
                      (node-length (count-chars bytes heading-start us-pos)))
                 (setf (gethash node-title *node-locator*)
                       (list filename heading-start node-length))
                 (setf pos (1+ title-end))))
              (t
               (setf pos (1+ nl))))))))))

;;; (2.2) Collect section pairs as an alist.

(defun part-2-2 ()
  "Return an alist of (title . (filename byte-offset nchars)) for all
   section headings, sorted by title."
  (let ((keys (sort (loop for k being the hash-keys of *node-locator* collect k)
                    #'string<))
        (pairs '()))
    (dolist (key keys)
      (incf *section-cnt*)
      (destructuring-bind (filename begin-node-offset length)
          (gethash key *node-locator*)
        (let ((bad? (or (string= key "")
                        (string= filename "")
                        (< begin-node-offset 0)
                        (< length 0))))
          (when bad?
            (format *error-output*
                    "build-index.lisp: bad entry for title=~s~%" key))
          (push (cons key (list filename begin-node-offset length)) pairs))))
    (nreverse pairs)))


;;; (2.3) Build and emit the complete output form.

(defun emit-index (deffn-pairs sect-pairs prettify)
  "Emit the complete index as a Lisp form to *standard-output*.
   If PRETTIFY is true, use pprint with *print-case* :downcase.
   Otherwise write compactly, one entry per line."
  (when (zerop (+ *item-cnt* *section-cnt*))
    (format *error-output* "WARNING: Empty index. Not sure what's going on.~%"))
  (let ((path-form (if *info-installation-path*
                       (pathname (format nil "~a/" *info-installation-path*))
                       '(maxima::maxima-load-pathname-directory)))
        (*print-case*   :downcase)
        (*print-length* nil)
        (*print-level*  nil))
    (if prettify
        (pprint
         `(progn
            (in-package :cl-info)
            (let ((deffn-defvr-pairs ',deffn-pairs)
                  (section-pairs ',sect-pairs))
              (load-info-hashtables ,path-form
                                    deffn-defvr-pairs
                                    section-pairs))))
        (progn
          (format t "(in-package :cl-info)~%(let (~%")
          (format t "(deffn-defvr-pairs '(~%")
          (format t "; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))~%")
          (dolist (pair deffn-pairs)
            (format t "(~s . ~s)~%" (car pair) (cdr pair)))
          (format t "))~%~%(section-pairs '(~%")
          (format t "; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))~%")
          (dolist (pair sect-pairs)
            (format t "(~s . ~s)~%" (car pair) (cdr pair)))
          (format t ")))~%")
          (format t "(load-info-hashtables ~s deffn-defvr-pairs section-pairs))~%"
                  path-form)))))

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------


(defun reset-state ()
  "Reset all global state so repeated calls to BUILD-INDEX produce correct results."
  (setf *main-info*              nil)
  (setf *info-installation-path* nil)
  (setf *makeinfo-major-version* nil)
  (setf *makeinfo-minor-version* nil)
  (setf *info-filenames*         '())
  (clrhash *node-offset*)
  (clrhash *topic-locator*)
  (clrhash *node-locator*)
  (setf *item-cnt*               0)
  (setf *section-cnt*            0)
  (setf *verbose*                nil)
  (clrhash *bytes-cache*))

(defun build-index (main-info &key (info-installation-path "./") pprint verbose)
  "Build the info index for MAIN-INFO, writing Lisp s-expressions to
   *standard-output*.  :INFO-INSTALLATION-PATH, if supplied, is embedded in
   the emitted load-info-hashtables call; otherwise the Maxima default is used.
   If :PPRINT T is given, pretty-print the output with lowercase symbols.
   :VERBOSE controls progress messages to *error-output*:
     NIL (default) -- silent
     T or 1        -- high-level phase messages
     2             -- phase messages plus per-file scanning detail"
  (reset-state)
  (setf *main-info*              main-info)
  (setf *info-installation-path* info-installation-path)
  (setf *verbose*                verbose)
  (progress 1 "~&; build-index: scanning info files...~%")
  (let ((index-node-name (part-1-1a)))
    (progress 1 "; build-index: reading index node ~s...~%" index-node-name)
    (part-1-1b index-node-name))
  (progress 1 "; build-index: ~a index topics found, resolving byte offsets...~%"
            (hash-table-count *topic-locator*))
  (part-1-2)
  (progress 1 "; build-index: scanning for section headings...~%")
  (let ((deffn-pairs (part-1-3)))
    (part-2-1)
    (progress 1 "; build-index: ~a deffn/defvr entries, ~a section headings, emitting...~%"
              *item-cnt* (hash-table-count *node-locator*))
    (let ((sect-pairs (part-2-2)))
      (emit-index deffn-pairs sect-pairs pprint)))
  (progress 1 "; build-index: done (~a deffn/defvr, ~a sections).~%"
            *item-cnt* *section-cnt*))
