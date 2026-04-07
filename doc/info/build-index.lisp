;;; -----------------------------
;;;  Common Lisp translation of build_index.pl
;;;  Requires: pregexp  (https://ds26gte.github.io/pregexp/index.html)
;;; -----------------------------
;;;
;;; Usage:
;;;   sbcl --load compat.lisp --script build-index.lisp \
;;;        <main-info-file> [info-installation-path]
;;;
;;; The script writes Lisp s-expressions to *standard-output*, exactly as
;;; the original Perl script does.
;;;
;;; Load compat.lisp before loading this file.
;;; Then call (build-index "maxima.info") or (build-index "maxima.info" "./").

;;; ---------------------------------------------------------------------------
;;; I/O helpers
;;; ---------------------------------------------------------------------------

(defun slurp-bytes (filename)
  "Return the entire contents of FILENAME as a (unsigned-byte 8) vector."
  (with-open-file (s filename :direction :input
                              :element-type '(unsigned-byte 8))
    (let* ((len (file-length s))
           (buf (make-array len :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      buf)))

(defun bytes-to-string (bytes &optional (start 0) end)
  "Convert a region of a (unsigned-byte 8) vector to a string via UTF-8."
  (stream:octets-to-string bytes :start start :end (or end (length bytes))
                                 :external-format :utf-8))

;;; ---------------------------------------------------------------------------
;;; Regexp helpers built on pregexp
;;; ---------------------------------------------------------------------------

(defun re-match (pattern string &optional (start 0))
  "Return the list of substrings matched by PATTERN in STRING (starting at
   START), or NIL if no match.  Group 0 is the whole match; 1..n are captures."
  (pregexp:pregexp-match pattern string start))

(defun re-match-positions (pattern string &optional (start 0))
  "Like re-match but returns position pairs (start . end)."
  (pregexp:pregexp-match-positions pattern string start))

(defun re-search-all (pattern string)
  "Return a list of all non-overlapping matches of PATTERN in STRING.
   Each element is a list of group strings (group 0 first)."
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
          (setf pos (if (> end start) end (1+ end)))
          (when (>= pos len) (return)))))
    (nreverse results)))

(defun re-replace-all (pattern string replacement)
  "Replace every occurrence of PATTERN in STRING with REPLACEMENT."
  (pregexp:pregexp-replace* pattern string replacement))

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defparameter *us-byte* 31)   ; unit-separator as byte

;;; ---------------------------------------------------------------------------
;;; Global state
;;; ---------------------------------------------------------------------------

(defvar *main-info* nil)
(defvar *info-installation-path* nil)

(defvar *makeinfo-major-version* nil)
(defvar *makeinfo-minor-version* nil)

(defvar *info-filenames* '())
(defvar *node-offset*   (make-hash-table :test #'equal))
(defvar *topic-locator* (make-hash-table :test #'equal))
(defvar *node-locator*  (make-hash-table :test #'equal))

(defvar *item-cnt*    0)
(defvar *section-cnt* 0)

;;; ---------------------------------------------------------------------------
;;; Pre-compiled regexp patterns
;;; ---------------------------------------------------------------------------

(defparameter *re-deffn-marker*
  (pregexp:pregexp "^ -- \\S")
  "Matches a deffn/defvr definition line.")

(defparameter *re-index-entry*
  (pregexp:pregexp "\\* (?!Menu)(\\S+|[^:]+):\\s+(.*?)\\.\\s+\\(line\\s+(\\d+)\\)")
  "Matches a single index entry line in the info index node.")

(defparameter *re-escape-quote*
  (pregexp:pregexp "\"")
  "Matches a double-quote character.")

;;; ---------------------------------------------------------------------------
;;; PART 1 - Build index for @deffn and @defvr items
;;; ---------------------------------------------------------------------------

;;; (1.1a) Scan *.info* files; populate *node-offset*.

(defparameter *node-prefix-bytes*
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code "Node: ")
  "ASCII bytes for 'Node: ', pre-allocated for fast header scanning.")

(defparameter *file-prefix-bytes*
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code "File: ")
  "ASCII bytes for 'File: ', pre-allocated for fast header scanning.")

(defun parse-node-name-from-header (bytes start end)
  "Parse the node name from a File:/Node: header in BYTES[START..END).
   Real node headers have File: and Node: on the same line.
   Tag table entries have bare Node: lines without File: -- we skip those.
   Returns the node name string, or NIL if not found."
  (loop for i from start below (- end 6)
        when (loop for j from 0 below 6
                   always (= (aref bytes (+ i j)) (aref *file-prefix-bytes* j)))
        return (let* ((line-end (or (position (char-code #\Newline) bytes
                                              :start (+ i 6) :end end)
                                   end))
                      (node-pos (loop for k from (+ i 6) below (max (+ i 6) (- line-end 6))
                                      when (loop for j from 0 below 6
                                                 always (= (aref bytes (+ k j))
                                                            (aref *node-prefix-bytes* j)))
                                      return k
                                      finally (return nil))))
                 (when node-pos
                   (let* ((name-start (+ node-pos 6))
                          (name-end   (loop for k from name-start below line-end
                                            when (let ((b (aref bytes k)))
                                                   (or (= b (char-code #\,))
                                                       (= b (char-code #\Newline))))
                                            return k
                                            finally (return line-end))))
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
        (pos            0)    ; current byte position
        (char-count     0)    ; characters counted so far (= char offset of pos)
        (nl-char-count  0)    ; char offset of last nl-byte-pos stored
        (nl-byte-pos    0))   ; byte offset stored for last-node-name
    (loop
      (let ((us-pos (position *us-byte* bytes :start pos)))
        (unless us-pos (return last-node-name))
        ;; Walk byte-pos forward from pos to the \n before the US,
        ;; counting characters to maintain char-count in sync.
        (let ((nl (if (and (> us-pos 0)
                           (= (aref bytes (1- us-pos)) #x0A))
                      (1- us-pos)
                      us-pos)))
          (loop while (< pos nl)
                do (let ((width (let ((b (aref bytes pos)))
                                  (cond ((< b #x80) 1)
                                        ((< b #xE0) 2)
                                        ((< b #xF0) 3)
                                        (t          4)))))
                     (incf char-count)
                     (incf pos width)))
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
         ;; The makeinfo version is in the first line; the indirect table
         ;; is between the first and second US bytes. Decode up to the
         ;; second US (or 8KB, whichever is smaller) to cover both.
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
    (setf last-node-name (scan-info-file-for-nodes *main-info* main-bytes))

    (let* ((pattern (concatenate 'string
                                 (re-replace-all "\\." *main-info* "\\.")
                                 "-(\\d+): (\\d+)"))
           (matches (re-search-all pattern header-str)))
      (dolist (match matches)
        (let ((sub-filename (format nil "~a-~a" *main-info* (cadr match))))
          (push sub-filename *info-filenames*)
          (let ((n (scan-info-file-for-nodes sub-filename (slurp-bytes sub-filename))))
            (when n (setf last-node-name n))))))

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
         ;; node-byte-pos is the \n before the US. The node content starts
         ;; after the US (\n at node-byte-pos, US at node-byte-pos+1, \n at +2,
         ;; then File:/Node: header). Find the NEXT US after this one.
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
   This avoids allocating a string just to call LENGTH on it."
  (let ((pos start)
        (n   0))
    (loop while (< pos end)
          do (incf n)
             (incf pos
                   (let ((b (aref bytes pos)))
                     (cond ((< b #x80) 1)
                           ((< b #xE0) 2)
                           ((< b #xF0) 3)
                           (t          4)))))
    n))

(defun item-text-length (bytes start)
  "Return the character length of the item text starting at byte offset START
   in BYTES, matching what Perl computes with length($1) after:
     if ($stuff =~ m/(.*?)(?:\\n\\n(?= -- )|\\n(?=[0-9])|(?=$unit_separator))/cgsm)
   We scan bytes directly for the termination conditions, then count UTF-8
   characters in the matched region without allocating an intermediate string."
  (let ((pos  start)
        (blen (length bytes)))
    (loop
      (when (>= pos blen)
        (return (count-chars bytes start blen)))
      (let ((b (aref bytes pos)))
        (cond
          ;; US byte (\x1f) — item ends here (zero-width lookahead in Perl).
          ((= b *us-byte*)
           (return (count-chars bytes start pos)))
          ;; \n — check for \n\n(?= -- ) or \n(?=[0-9])
          ((= b #x0A)
           (let ((next (if (< (1+ pos) blen) (aref bytes (1+ pos)) 0)))
             (cond
               ;; \n followed by digit — ends here (Perl: \n(?=[0-9]))
               ((and (>= next (char-code #\0)) (<= next (char-code #\9)))
                (return (count-chars bytes start pos)))
               ;; \n\n — check for ' -- ' after the second \n
               ((= next #x0A)
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
   so that line-skipping is done incrementally — each line is visited once."
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
         (let* ((sorted (sort file-entries
                              (lambda (a b)
                                (or (< (fourth a) (fourth b))
                                    (and (= (fourth a) (fourth b))
                                         (< (fifth a) (fifth b)))))))
                (bytes  (slurp-bytes filename))
                ;; Incremental line-skip state: track position within current node.
                (cur-node-offset -1)
                (cur-lines-done   0)
                (cur-byte-pos     0))
           (dolist (e sorted)
             (destructuring-bind (key node-name _filename node-byte-offset lines-offset)
                 e
               (declare (ignore _filename))
               (let ((item-byte-offset
                      (cond
                        ;; Makeinfo 4.x bug workaround — always re-scan from node start.
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
                           ;; New node — reset incremental state.
                           (setf cur-node-offset node-byte-offset)
                           (setf cur-lines-done   0)
                           (setf cur-byte-pos     node-byte-offset))
                         ;; Skip only the additional lines since last entry.
                         (setf cur-byte-pos
                               (read-lines-from-bytes bytes cur-byte-pos
                                                      (- lines-offset cur-lines-done)))
                         (setf cur-lines-done lines-offset)
                         cur-byte-pos))))
                 (let ((text-length (item-text-length bytes item-byte-offset)))
                   (setf (gethash key *topic-locator*)
                         (list node-name filename item-byte-offset text-length))))))))
       by-file))))

;;; (1.3) Emit Lisp for deffn/defvr pairs.

(defun part-1-3 ()
  (format t "(in-package :cl-info)~%")
  (format t "(let (~%")
  (format t "(deffn-defvr-pairs '(~%")
  (format t "; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))~%")

  (let ((keys (sort (loop for k being the hash-keys of *topic-locator* collect k)
                    #'string<)))
    (dolist (key keys)
      (incf *item-cnt*)
      (let* ((entry         (gethash key *topic-locator*))
             (node-name     (first  entry))
             (file-name     (second entry))
             (byte-offset   (third  entry))
             (nchars        (fourth entry))
             (sanitized-key (re-replace-all *re-escape-quote* key "\\\""))
             (bad?          (or (string= sanitized-key "")
                                (string= file-name "")
                                (< byte-offset 0)
                                (< nchars 0)
                                (string= node-name ""))))
        (when bad?
          (format *error-output*
                  "build-index.lisp: something seems wrong for key=\"~a\"; emit it anyway.~%"
                  sanitized-key)
          (format *error-output*
                  "build-index.lisp: sanitized-key=\"~a\", file-name=\"~a\", byte-offset=~a, nchars=~a, node-name=\"~a\"~%"
                  sanitized-key file-name byte-offset nchars node-name)
          (format t ";; build-index.lisp: something seems wrong for this next item~%"))
        (format t "(\"~a\" . (\"~a\" ~a ~a \"~a\"))~%"
                sanitized-key file-name byte-offset nchars node-name))))

  (format t "))~%~%"))

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
   Populates *node-locator* with (filename byte-offset length) entries.
   Works entirely in bytes: scans for newline + N.M<space> in the byte array."
  (dolist (filename *info-filenames*)
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
                      ;; node-length must be in characters (like Perl's length),
                      ;; not bytes, since the Perl script uses string length.
                      (node-length (count-chars bytes heading-start us-pos)))
                 (setf (gethash node-title *node-locator*)
                       (list filename heading-start node-length))
                 (setf pos (1+ title-end))))
              (t
               (setf pos (1+ nl)))))))))  )

;;; (2.2) Emit Lisp for section pairs.

(defun part-2-2 ()
  (format t "(section-pairs '(~%")
  (format t "; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))~%")

  (let ((keys (sort (loop for k being the hash-keys of *node-locator* collect k)
                    #'string<)))
    (dolist (key keys)
      (incf *section-cnt*)
      (destructuring-bind (filename begin-node-offset length)
          (gethash key *node-locator*)
        (let* ((sanitized-title (re-replace-all *re-escape-quote* key "\\\""))
               (bad?            (or (string= sanitized-title "")
                                    (string= filename "")
                                    (< begin-node-offset 0)
                                    (< length 0))))
          (when bad?
            (format *error-output*
                    "build-index.lisp: something seems wrong for title=\"~a\"; emit it anyway.~%"
                    sanitized-title)
            (format *error-output*
                    "build-index.lisp: sanitized-title=\"~a\", filename=\"~a\", begin-node-offset=~a, length=~a~%"
                    sanitized-title filename begin-node-offset length)
            (format t ";; build-index.lisp: something seems wrong for this next item~%"))
          (format t "(\"~a\" . (\"~a\" ~a ~a))~%"
                  sanitized-title filename begin-node-offset length)))))

  (format t ")))~%"))

;;; (2.3) Close the LET and call load-info-hashtables.

(defun part-2-3 ()
  (let ((info-path-or-how
         (if *info-installation-path*
             (format nil "#p\"~a/\"" *info-installation-path*)
             "(maxima::maxima-load-pathname-directory)")))
    (format t "(load-info-hashtables ~a deffn-defvr-pairs section-pairs))~%"
            info-path-or-how))

  (when (zerop (+ *item-cnt* *section-cnt*))
    (format *error-output* "WARNING: Empty index. Not sure what's going on.~%")))

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
  (setf *section-cnt*            0))

(defun build-index (main-info &optional info-installation-path)
  "Build the info index for MAIN-INFO, writing Lisp s-expressions to
   *standard-output*.  INFO-INSTALLATION-PATH, if supplied, is embedded in
   the emitted load-info-hashtables call; otherwise the Maxima default is used."
  (set-stdout-utf-8)
  (reset-state)
  (setf *main-info*              main-info)
  (setf *info-installation-path* info-installation-path)
  (let ((index-node-name (part-1-1a)))
    (part-1-1b index-node-name))
  (part-1-2)
  (part-1-3)
  (part-2-1)
  (part-2-2)
  (part-2-3))
