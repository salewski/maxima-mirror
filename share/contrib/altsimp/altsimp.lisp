;; Author: Barton Willis with help from Richard Fateman
;; Updated Feb 2025

#|
To simplify a sum with n terms, the standard simplus function calls
great O(n^2) times. By using sorting more effectively, this code
reduces the calls to O(n log_2(n)).

When the option variable `use_extended_real_arithmetic` is true, 
this code tries to be "infinity correct" for addition. By this I
mean inf + inf --> inf, inf + number --> inf, inf + minf --> und, 
and so on. Since -1 * inf does not simplify to minf, this code doesn't simplify
inf - inf to und; consequently, this portion of the code is largely
untested. There are other problems too.  For one, arguably 
f(x) - f(x) --> 0 is wrong unless we know that the range of 'f' excludes
an infinity (comment from Stavros Macrakis). I don't know
how far we can go with such things without making a mess. You could
argue that Maxima should do

   f(x) - f(x) --> if finitep(f(x)) then 0 else und 

instead of f(x) - f(x) --> 0.

There is a great deal more that could be done. We could tag each
infinity with a unique identifier (idea from RJF). That way we could
have x : inf, x - x --> 0 and x : inf, y : inf, x - y --> und, for
example.

In short, this code is highly experimental; you should not use it for
anything that is important. I place it in /share/contrib because
others encouraged me to; also I thought it might be useful to others
who would like to experiment with similar code. Since a great deal of
work has gone into the current simplus code, I'm not sure that a total
rewrite is the best route.

Maybe the special case dispatch part of this code makes the task of
extending simplus (to intervals, for example) easier. The basic design
of this code is due to Barton Willis.
|#

#| 

As of Feb 2025, altsimp (using SBCL and with use_extended_real_arithmetic set to false) runs the
testsuite with seven failures and one success:

Error summary:
Error(s) found:
  rtest14.mac problems:  (62 153)
  rtest_gamma.mac problems:  (384 390)
  rtest_expintegral.mac problems:  (175 176)
  rtest_vect.mac problem:   (74)

Tests that were expected to fail but passed:
  rtest3.mac problem:   (146)

7 tests failed out of 19,012 total tests.

The two rtest_gamma failures are present without using simplus, so only five failures are due to simplus. 

And with use_extended_real_arithmetic set to true:

Error(s) found:
   rtest14.mac problems:    (62 153)
   rtest_gamma.mac problems:    (384 390)
   rtest_expintegral.mac problems:    (175 176)
   rtest_powerseries.mac problems:    (53 63)
   rtest_simplify_sum.mac problems:
    (3 4 5 6 7 10 11 12 13 14 16 17 19 20 21 22 23 28 29 53 67 68 69 70 71 72)
  linearalgebra/rtest_linalg.mac problems:    (92 94)
  rtest_abs_integrate.mac problems:    (74 89 139)
  vector/rtest_vect.mac problem:    (74)

Tests that were expected to fail but passed:
   rtest3.mac problem:   (146)
   rtest_maxmin.mac problem:   (109)
   rtest_limit_extra.mac problem:  (125)

Speculation on how to speed up simplification of sums:

The altsimp algorithm uses sorting to speed up simplification of expressions with a 
large number of summands, but at least for running the testsuites, simplus is mostly 
called with just two summands. If one of the two summands is a sum, say XXX, altsimp 
re-examines the terms of XXX for common terms. That is, of course, wasteful. If the 
summands were marked in a way that indicated that pairs of terms were not common, that 
would possibly speed the code.

|#

(in-package :maxima)
(declaim (optimize (speed 3) (safety 0)))

;; When the option variable $use_extended_real_arithmetic is true, simplus adds the extended real 
;; numbers (minf,inf,infinity, und, ind) in a mathematically logical way. But since simptimes and
;; simpexpt do not have an option of doing correct extended real number arithmetic, this feature of
;; simplus is limited.

(defmvar $use_extended_real_arithmetic nil)

(define-modify-macro mincf (&optional (i 1)) addk)

;; Return true if z = 0, 0.0, or a bigfloat zero.
(defun mzerop (z)
  (and (mnump z)
       (or (and (numberp z)(= z 0))
	         (and (bigfloatp z)(= (cadr z) 0))))) ;bigfloat zeros may be diff precisions

;; Return true if x has the form integer^(rational)
(defun surd-p (x)
  (and (mexptp x) (integerp (cadr x)) ($ratnump (caddr x))))

(defun generalized-surd-p (x)
  (and (mexptp x) (integerp (cadr x)) (mplusp (caddr x)) 
         (or ($ratnump (second (caddr x))) (integerp (second (caddr x))))))

(defun surd-convert (x)
   (cond ((or (integerp x) (mnump x))
           (cons 1 x))
         ((surd-p x) 
          ;; x = a^(p/q) = a^n * a^(p/q - n), where n = floor(p/q). 
            (let ((a (cadr x)) (p (caddr x)) (q) (n))
              (setq q ($denom p))
              (setq p ($num p))         
              (setq n (floor p q))
              (cons (ftake 'mexpt a (sub (caddr x) n)) (ftake 'mexpt a n))))
          ((generalized-surd-p x) ; x = ((mexpt) a ((mplus) n a1 a2 ...)))
           (let ((a (second x)) (n (second (third x))) (p) (q))
              (setq p ($num n))
              (setq q ($denom n))
              (setq n (floor p q))
              (cons (ftake 'mexpt a (sub (third x) n)) (ftake 'mexpt a n))))            

          (t (cons x 1))))
  
;; Convert a term (a non sum expression) to the form (e . coefficient), where
;; coefficient is a rational or floating point number. Factors of the form
;; integer^(rational) are converted by surd-convert.
(defun convert-to-coeff-form (x)  
  (let ((c 1) (xx) (qq 1))
    (cond ((mnump x) (cons 1 x))
          ((or (surd-p x) (generalized-surd-p x))
            (surd-convert x))

	        ((mtimesp x) 
	          (pop x)  ;remove (car x) which is (mtimes ..)
            (while (or (mnump (car x)) (surd-p (car x)) (generalized-surd-p (car x)))
               (setq xx (surd-convert (car x)))
               (setq c (mul c (cdr xx)))
               (setq qq (mul qq (car xx)))
               (pop x))

            (when (not (eql qq 1))
                (push qq x))

            (when (null x)
               (push 1 x))

	          (if (null (cdr x)); if only one more item, that's it.
		              (cons (car x) c)
		              (cons (fapply 'mtimes x) c)))
          (t (cons x 1)))))

;; Previously there was a specialized function for multiplying a number times an expression. The
;; motivation was, I think, speed. But the specialized function was responsible for 22 testsuite
;; failures (May 2021) and it didn't contribute to running the testsuite any faster. So let us 
;; replace the specialized function. But, due to recent changes (circa Feb 2025), sometimes
;; the second argument to `number-times-expr` isn't simplified, and this causes trouble. Thus for
;; now, this code calls ftake* instead of ftake. This extra simplification slows down the code.
(defun number-times-expr (cf e)
  (ftake* 'mtimes cf e))
 
;; Add an expression x to a list of equalities l.
(defun add-expr-mequal (x l)
  (setq l (mapcar 'cdr l))
  (push (list x x) l)
  (setq l (list (reduce #'add (mapcar 'first l)) (reduce #'add (mapcar 'second l))))
  (fapply 'mequal l))
  
(defun add-expr-mrat (x l)
  (ratf (cons '(mplus) (cons (ratf x) l))))

(defun add-expr-taylor (x l)
  ($taylor (cons '(mplus) (cons x l))))

(defun add-expr-mlist (x l)
  (setq l (if (cdr l) (reduce 'addmx l) (car l)))
  (fapply 'mlist (mapcar #'(lambda (s) (add x s)) (cdr l))))

;; Simple demo showing how to define addition for a new object.
;; We could append simplification rules for intervals:
;;  (a) interval(a,a) --> a,
;;  (b) if p > q then interval(p,q) --> standardized empty interval?

(defun add-expr-interval (x l)
  (setq l (mapcar #'(lambda (s) `((mlist) ,@(cdr s))) l))
  (setq l (if (cdr l) (reduce #'addmx l) (car l)))
  (fapply '$interval (mapcar #'(lambda (s) (add x s)) (cdr l))))
 
;; Add an expression x to a list of matrices l. The Maxima function mxplusc
;; calls pls. Here is a version that doesn't call pls. I'm not sure I've captured all 
;; features of mxplusc.
(defun add-expr-matrix (x l)
  "Add expression x to a CL list of matrices l."
  (setq l (cond ((cdr l)
                  (setq l (let (($errormsg nil)) (errcatch (reduce #'addmx l))))
                  (if l (car l) (merror (intl:gettext "Attempt to add noncomformable matrices"))))
                (t (car l))))
  (cond ((and $listarith ($matrixp l))
          (fapply '$matrix (cdr (add x ($args l)))))
        (t (list (get 'mplus 'msimpind) x l))))

;; We use a hashtable to represent the addition table for extended reals. Currently, this
;; table suports the extended reals {minf, inf, ind, und, infinity}. Extending this table 
;; to include zeroa and zerob is an easy exercise, but doing so results in about 50 testsuite 
;; failures, some of them being bad bugs. Internally, Maxima uses prin-inf (see defint.lisp) 
;; to represent $inf. I haven't tried, but we could include prin-inf as an extended real.
(defvar *extended-real-add-table* (make-hash-table :test #'equal :size 16))

(mapcar #'(lambda (a) (setf (gethash (list (first a) (second a)) *extended-real-add-table*) (third a)))
   (list (list '$minf '$minf '$minf)
         (list '$minf '$inf '$und)
         (list '$minf '$infinity '$und)
         (list '$minf '$und '$und)
         (list '$minf '$ind '$minf)
         
         (list '$inf '$inf '$inf)
         (list '$inf '$infinity '$und)
         (list '$inf '$und '$und)
         (list '$inf '$ind '$inf)

         (list '$infinity '$infinity '$und)
         (list '$infinity '$und '$und)
         (list '$infinity '$ind '$infinity)

         (list '$ind '$ind '$ind)
         (list '$ind '$und '$und)

         (list '$und '$und '$und)))

(defun add-extended-real(a b)  
  (gethash (list a b) *extended-real-add-table* (gethash (list b a) *extended-real-add-table* '$und)))

;; Add an expression x to a list of infinities. We do explicit number + extended real --> extended real, 
;; but for a general expression XXX we do XXX + extended real --> nounform.
(defun add-expr-infinities (x l) 
  (setq l (if l (reduce #'add-extended-real l) (car l)))
  (if (mnump x) l (list (get 'mplus 'msimpind) x l)))

;; The functions pls & plusin are parts of the standard simplus code. Let's issue
;; errors when these functions are called. Unfortunately, code in share\affine calls
;; pls directly, so the affine code will not run using altsimp.	
(defun pls (x out)
    (mtell "in pls; x = ~M,  out = ~M ~%" x out)
    (merror "Error: called pls ~%"))

(defun plusin (x fm) 
   (mtell "in plusin; x = ~M,  fm = ~M ~%" x fm)
   (merror "Error: called plusin ~%"))

;; I assumed that if a list of distinct members is sorted using great,
;; then it's still sorted after multiplying each list member by a nonzero
;; maxima number. I'm not sure this is true.

;; If l has n summands, simplus calls great O(n log_2(n)) times. All
;; other spendy functions are called O(n) times. The standard simplus
;; function calls great O(n^2) times, I think.

;; The binary64 value of %e.
(defvar %e-float64 (exp 1.0d0))

(defun simplus (l w z)
  (declare (ignore w))
  
  (let ((acc nil) (cf) (x) (num-sum 0) (do-over nil) (mequal-terms nil) (mrat-terms nil) 
	(inf-terms nil) (matrix-terms nil) (mlist-terms nil) (taylor-terms nil) (interval-terms nil) 
  (op) (atom-hash (make-hash-table :test #'eq :size 8)))

  (setq l (margs l))
  ;; simplify and flatten
    (dolist (li l)
    	(setq li (simplifya li z))
     ;; When numer is true, simplifya converts %pi & %phi to their binary64 values,
     ;; but only converts %e when both numer & %enumer are true. Here we convert
     ;; %e to its binary64 value.
     (when (and $numer (atom li) (eq li '$%e)) ; $%e --> 2.718...
        (setq li  %e-float64))
    	(if (mplusp li) (setq acc (append acc (cdr li))) (push li acc)))

    (setq l acc)
    (setq acc nil)
    (dolist (li l)
      (cond ((mnump li) (mincf num-sum li))
	          ;; factor out infrequent cases.
          	((and (consp li) (consp (car li)) (member (caar li) '(mequal mrat $matrix mlist $interval)))
	                (setq op (caar li))
	                (cond ((eq op 'mequal)
		                      (push li mequal-terms))
		                    (($taylorp li)
	                  	    (push li taylor-terms))
		                    ((eq op 'mrat)
		                       (push li mrat-terms))
		                    ((eq op '$matrix)
		                       (push li matrix-terms))
		                    ((eq op '$interval)
		                       (push li interval-terms))
		                    ((eq op 'mlist)
		                      (if $listarith (push li mlist-terms) (push (convert-to-coeff-form li) acc))))) 

	            ;; Put non-infinite atoms into a hashtable; push infinite atoms into inf-terms.
	            ((atom li)
	                (if (and $use_extended_real_arithmetic (member li '($minf $inf $infinity $und $ind)))
		                     (push li inf-terms)
	                    (progn
		                      (setq cf (gethash li atom-hash))
		                      (setf (gethash li atom-hash) (if cf (1+ cf) 1)))))

	        (t (push (convert-to-coeff-form li) acc))))

     ;; push atoms in the hashtable into the accumulator acc; sort acc.
    (maphash #'(lambda (cf a) (push (cons cf a) acc)) atom-hash)
    (setq l (sort acc 'great :key 'car))
 
    ;; common term crunch: when the new coefficient is -1 or 1 (for example, 5*a - 4*a),
    ;; set the "do-over" flag to true. In this case, the sum needs to be re-simplified.
    ;; Without the do over flag, a + 5*a - 4*a --> a + a. Last I checked, the testsuite
    ;; does not test the do-over scheme.

    (setq acc nil)
    (while l
      (setq x (pop l))
      (setq cf (cdr x))
      (setq x (car x))
      (while (and l (like x (caar l)))
      	(mincf cf (cdr (pop l))))
        (if (and (or (eql cf 1) (eql cf -1)) (mplusp x)) (setq do-over t))
        (setq x (number-times-expr cf x))
        (cond ((mnump x) (mincf num-sum x))
	            ((not (mzerop x)) (push x acc))))

    ;; Do x + 0 --> x, x + 0.0 --> x, and x + 0.0b0 --> x.
    (if (not (mzerop num-sum)) (push num-sum acc))
   
    (setq acc
	  (cond (do-over (simplifya `((mplus) ,@acc) nil))
      		((null acc) num-sum)
		      ((null (cdr acc)) (car acc))
		      (t (cons '(mplus simp) acc))))
    
    ;; special case dispatch
    (when mequal-terms
	     (setq acc (add-expr-mequal acc mequal-terms)))
    (when taylor-terms
    	(setq acc (add-expr-taylor acc taylor-terms)))
    (when mrat-terms
	      (setq acc (add-expr-mrat acc mrat-terms)))
    (when mlist-terms
	      (setq acc (add-expr-mlist acc mlist-terms)))
    (when interval-terms
      	(setq acc (add-expr-interval acc interval-terms)))
    (when matrix-terms
	      (setq acc (add-expr-matrix acc matrix-terms)))
    (when inf-terms
      	(setq acc (add-expr-infinities acc inf-terms)))   
    acc))