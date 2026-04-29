;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;
;;; This file is part of the Maxima computer algebra project
;;; (https://sourceforge.net/projects/maxima/) 
;;; SPDX-License-Identifier: GPL-2.0-or-later 
;;;
;;; Maxima is copyrighted by its authors and licensed under the GNU
;;; General Public License.  This program is distributed WITHOUT ANY
;;; WARRANTY. See COPYING and AUTHORS for details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defun simpsum (x y z)
  (let (($ratsimpexpons t))
    (setq y (maybe-simplifya (sum-arg x) z)))
  (simpsum1 y (sum-index x) (maybe-simplifya (sum-lower x) z)
	    (maybe-simplifya (sum-upper x) z)))

; This function was SIMPSUM1 until the sum/product code was revised Nov 2005.
; The revised code punts back to this function since this code knows
; some simplifications not handled by the revised code. -- Robert Dodier

(defun simpsum1-save (exp i lo hi)
  (cond ((not (symbolp i)) (merror (intl:gettext "sum: index must be a symbol; found ~M") i))
	((equal lo hi) (mbinding ((list i) (list hi)) (meval exp)))
	((and (atom exp)
	      (not (eq exp i))
	      (getl '%sum '($outative $linear)))
	 (freesum exp lo hi 1))
	((null $simpsum) (list (get '%sum 'msimpind) exp i lo hi))
	((and (or (eq lo '$minf)
		  (alike1 lo '((mtimes simp) -1 $inf)))
	      (equal hi '$inf))
	 (let ((pos-part (simpsum2 exp i 0 '$inf))
	       (neg-part (simpsum2 (maxima-substitute (m- i) i exp) i 1 '$inf)))
	   (cond
	     ((or (eq neg-part '$und)
		  (eq pos-part '$und))
	      '$und)
	     ((eq pos-part '$inf)
	      (if (eq neg-part '$minf) '$und '$inf))
	     ((eq pos-part '$minf)
	      (if (eq neg-part '$inf) '$und '$minf))
	     ((or (eq neg-part '$inf) (eq neg-part '$minf))
	      neg-part)
	     (t (m+ neg-part pos-part)))))
	((or (eq lo '$minf)
	     (alike1 lo '((mtimes simp) -1 '$inf)))
	 (simpsum2 (maxima-substitute (m- i) i exp) i (m- hi) '$inf))
	(t (simpsum2 exp i lo hi))))

(defun simpsum1 (e k lo hi)
  (with-new-context (context)
    (let ((acc 0) (n) (sgn) ($prederror nil) (i (gensym)) (ex))
      (setq lo ($ratdisrep lo))
      (setq hi ($ratdisrep hi))

      (setq n ($limit (add 1 (sub hi lo))))
      (setq sgn ($sign n))

      (if (not (eq t (csign lo))) (mfuncall '$assume `((mgeqp) ,i ,lo)))
      (if (not (eq t (csign hi))) (mfuncall '$assume `((mgeqp) ,hi ,i)))

      (setq ex (subst i k e))
      (setq ex (subst i k ex)) ; Why substitute again?

      (setq acc
            (cond ((and (eq n '$inf) ($freeof i ex))
                   (setq sgn (csign ex))
                   (cond ((eq sgn '$pos) '$inf)
                         ((eq sgn '$neg) '$minf)
                         ((eq sgn '$zero) 0)
                         (t `((%sum simp) ,ex ,i ,lo ,hi))))

                  ((and (mbagp e) $listarith)
                   (simplifya
                    `((,(caar e)) ,@(mapcar #'(lambda (s) (mfuncall '$sum s k lo hi)) (margs e))) t))

                  ((or (eq sgn '$neg) (eq sgn '$zero) (eq sgn '$nz)) 0)

                  ((like ex 0) 0)

                  (($freeof i ex) (mult n ex))

                  ((and (integerp n) (eq sgn '$pos) $simpsum)
                   (dotimes (j n acc)
                     (setq acc (add acc (resimplify (subst (add j lo) i ex))))))

                  (t
                   `((%sum simp) ,(subst k i ex) ,k ,lo ,hi))))

      (setq acc (subst k i acc))

      ;; If expression is still a summation,
      ;; punt to previous simplification code.

      (if (and $simpsum (op-equalp acc '%sum))
        (let* ((args (cdr acc)) (e (first args)) (i (second args)) (i0 (third args)) (i1 (fourth args)))
          (setq acc (simpsum1-save e i i0 i1))))

      ; If the expression is no longer a %sum, resimplify.
      ; Ordering of expressions may change due to the gensym -> index substitution.
      (unless (op-equalp acc '%sum)
        (setq acc (resimplify acc)))

      acc)))

(defun simpprod1 (e k lo hi)
  (with-new-context (context)
    (let ((acc 1) (n) (sgn) ($prederror nil) (i (gensym)) (ex) (ex-mag) (realp))

      (setq lo ($ratdisrep lo))
      (setq hi ($ratdisrep hi))
      (setq n ($limit (add 1 (sub hi lo))))
      (setq sgn ($sign n))

      (if (not (eq t (csign lo))) (mfuncall '$assume `((mgeqp) ,i ,lo)))
      (if (not (eq t (csign hi))) (mfuncall '$assume `((mgeqp) ,hi ,i)))

      (setq ex (subst i k e))
      (setq ex (subst i k ex)) ; Why substitute again?

      (setq acc
            (cond
              ((like ex 1) 1)

              ((and (eq n '$inf) ($freeof i ex))
               (setq ex-mag (mfuncall '$cabs ex))
               (setq realp (mfuncall '$imagpart ex))
               (setq realp (mevalp `((mequal) 0 ,realp)))

               (cond ((eq t (mevalp `((mlessp) ,ex-mag 1))) 0)
                     ((and (eq realp t) (eq t (mevalp `((mgreaterp) ,ex 1)))) '$inf)
                     ((eq t (mevalp `((mgreaterp) ,ex-mag 1))) '$infinity)
                     ((eq t (mevalp `((mequal) 1 ,ex-mag))) '$und)
                     (t `((%product) ,e ,k ,lo ,hi))))

              ((or (eq sgn '$neg) (eq sgn '$zero) (eq sgn '$nz))
               1)

              ((and (mbagp e) $listarith)
               (simplifya
                `((,(caar e)) ,@(mapcar #'(lambda (s) (mfuncall '$product s k lo hi)) (margs e))) t))

              (($freeof i ex) (power ex n))

              ((and (integerp n) (eq sgn '$pos) $simpproduct)
               (dotimes (j n acc)
                 (setq acc (mult acc (resimplify (subst (add j lo) i ex))))))

              (t
               `((%product simp) ,(subst k i ex) ,k ,lo ,hi))))

      ;; Hmm, this is curious... don't call existing product simplifications
      ;; if index range is infinite -- what's up with that??

      (if (and $simpproduct (op-equalp acc '%product) (not (like n '$inf)))
        (let* ((args (cdr acc)) (e (first args)) (i (second args)) (i0 (third args)) (i1 (fourth args)))
          (setq acc (simpprod1-save e i i0 i1))))

      (setq acc (subst k i acc))

      ; If the expression is no longer a %product, resimplify.
      ; Ordering of expressions may change due to the gensym -> index substitution.
      (unless (op-equalp acc '%product)
        (setq acc (resimplify acc)))

      acc)))

; This function was SIMPPROD1 until the sum/product code was revised Nov 2005.
; The revised code punts back to this function since this code knows
; some simplifications not handled by the revised code. -- Robert Dodier

(defun simpprod1-save (exp i lo hi)
  (let (u)
    (cond ((not (symbolp i)) (merror (intl:gettext "product: index must be a symbol; found ~M") i))
	  ((alike1 lo hi)
	   (let ((valist (list i)))
	     (mbinding (valist (list hi))
		       (meval exp))))
	  ((eq ($sign (setq u (m- hi lo))) '$neg)
	   (cond ((eq ($sign (add2 u 1)) '$zero) 1)
		 (t (merror (intl:gettext "product: lower bound ~M greater than upper bound ~M") lo hi))))
	  ((atom exp)
	   (cond ((null (eq exp i))
		  (power* exp (list '(mplus) hi 1 (list '(mtimes) -1 lo))))
		 ((let ((lot (asksign lo)))
		    (cond ((equal lot '$zero) 0)
			  ((eq lot '$positive)
			   (m// (list '(mfactorial) hi)
				(list '(mfactorial) (list '(mplus) lo -1))))
			  ((m* (list '(mfactorial)
				     (list '(mabs) lo))
			       (cond ((member (asksign hi) '($zero $positive) :test #'eq)
				      0)
				     (t (prog1
					    (m^ -1 (m+ hi lo 1))
					  (setq hi (list '(mabs) hi)))))
			       (list '(mfactorial) hi))))))))
	  ((list '(%product simp) exp i lo hi)))))

;; Summation stuff

(defun simpsum2 (exp i lo hi)
  (prog (*plus *times $simpsum u)
     (setq *plus (list 0) *times 1)
     (when (or (and (eq hi '$inf) (eq lo '$minf))
	       (equal 0 (m+ hi lo)))
       (setq $simpsum t lo 0)
       (setq *plus (cons (m* -1 *times (maxima-substitute 0 i exp)) *plus))
       (setq exp (m+ exp (maxima-substitute (m- i) i exp))))
     (cond ((eq ($sign (setq u (m- hi lo))) '$neg)
	    (if (equal u -1)
		(return 0)
		(merror (intl:gettext "sum: lower bound ~M greater than upper bound ~M") lo hi)))
	   ((free exp i)
	    (return (m+l (cons (freesum exp lo hi *times) *plus))))

	   ((progn (multiple-value-setq (exp *plus) (sumsum exp i lo hi *plus *times)) exp)
	    (setq exp (m* *times (dosum (cadr exp) (caddr exp)
					(cadddr exp) (cadr (cdddr exp)) t :evaluate-summand nil))))
	   (t (return (m+l *plus))))
     (return (m+l (cons exp *plus)))))

(let (combin-sum combin-usum)
  (defun adsum (e)
    (push (simplify e) combin-sum))
  (defun adusum (e)
    (push (simplify e) combin-usum))

  (defun fpolysum (e lo hi poly-var)	;returns *combin-ans*
    ;; Sums of polynomials using
    ;;   bernpoly(x+1, n) - bernpoly(x, n) = n*x^(n-1)
    ;; which implies
    ;;   sum(k^n, k, A, B) = 1/(n+1)*(bernpoly(B+1, n+1) - bernpoly(A, n+1))
    ;;
    ;; fpoly1 returns 1/(n+1)*(bernpoly(foo+1, n+1) - bernpoly(0, n+1)) for each power
    ;; in the polynomial e
    (labels
	((fpoly1 (e lo)
	   (cond ((smono e poly-var)
		  (fpoly2 *a *n e lo))
		 ((eq (caar e) 'mplus)
		  (cons '(mplus) (mapcar #'(lambda (x) (fpoly1 x lo)) (cdr e))))
		 (t (adusum e) 0)))
	 (fpoly2 (a n e lo)
	   (cond ((null (and (integerp n) (> n -1))) (adusum e) 0)
		 ((equal n 0)
		  (m* (cond ((signp e lo)
			     (m1+ 'foo))
			    (t 'foo))
		      a))
		 (($ratsimp
		   (m* a (list '(rat) 1 (1+ n))
		       (m- ($bernpoly (m+ 'foo 1) (1+ n))
			   (ftake '%bern (1+ n)))))))))
      (let ((a (fpoly1 (setq e ($expand ($ratdisrep ($rat e poly-var)))) lo))
	    ($prederror))
	(cond ((null a) 0)
	      ((member lo '(0 1))
	       (maxima-substitute hi 'foo a))
	      (t
	       (list '(mplus) (maxima-substitute hi 'foo a)
		     (list '(mtimes) -1 (maxima-substitute (list '(mplus) lo -1) 'foo a))))))))

  (defun fbino (e y lo hi poly-var)
    ;; fbino can do these sums:
    ;;  a) sum(binomial(n,k),k,0,n) -> 2^n
    ;;  b) sum(binomial(n-k,k,k,0,n) -> fib(n+1)
    ;;  c) sum(binomial(n,2k),k,0,n) -> 2^(n-1)
    ;;  d) sum(binomial(a+k,b),k,l,h) -> binomial(h+a+1,b+1) - binomial(l+a,b+1)
    ;; e=binomial(n,d)
    (prog (n d l h)
       ;; check that n and d are linear in poly-var
       (when (null (setq n (m2 (cadr e) (list 'n 'linear* poly-var))))
	 (return (adusum e)))
       (setq n (cdr (assoc 'n n :test #'eq)))
       (when (null (setq d (m2 (caddr e) (list 'd 'linear* poly-var))))
	 (return (adusum e)))
       (setq d (cdr (assoc 'd d :test #'eq)))

       ;; binomial(a+b*k,c+b*k) -> binomial(a+b*k, a-c)
       (when (equal (cdr n) (cdr d))
	 (setq d (cons (m- (car n) (car d)) 0)))

       (cond
	 ;; substitute k with -k in sum(binomial(a+b*k, c-d*k))
	 ;; and sum(binomial(a-b*k,c))
	 ((and (numberp (cdr d))
	       (or (minusp (cdr d))
		   (and (zerop (cdr d))
			(numberp (cdr n))
			(minusp (cdr n)))))
	  (rplacd d (- (cdr d)))
	  (rplacd n (- (cdr n)))
	  (setq l (m- hi)
		h (m- lo)))
	 (t (setq l lo  h hi)))

       (cond

	 ;; sum(binomial(a+k,c),k,l,h)
	 ((and (equal 0 (cdr d)) (equal 1 (cdr n)))
	  (adsum (m* y (m- (list '(%binomial) (m+ h (car n) 1) (m+ (car d) 1))
			   (list '(%binomial) (m+ l (car n)) (m+ (car d) 1))))))

	 ;; sum(binomial(n,k),k,0,n)=2^n
	 ((and (equal 1 (cdr d)) (equal 0 (cdr n)))
	  ;; sum(binomial(n,k+c),k,l,h)=sum(binomial(n,k+c+l),k,0,h-l)
	  (let ((h1 (m- h l))
		(c (m+ (car d) l)))
	    (if (and (integerp (m- (car n) h1))
		     (integerp c))
		(progn
		  (adsum (m* y (m^ 2 (car n))))
		  (when (member (asksign (m- (m+ h1 c) (car n))) '($zero $negative) :test #'eq)
		    (adsum (m* -1 y (dosum (list '(%binomial) (car n) poly-var)
					   poly-var (m+ h1 c 1) (car n) t :evaluate-summand nil))))
		  (when (> c 0)
		    (adsum (m* -1 y (dosum (list '(%binomial) (car n) poly-var)
					   poly-var 0 (m- c 1) t :evaluate-summand nil)))))
		(adusum e))))

	 ;; sum(binomial(b-k,k),k,0,floor(b/2))=fib(b+1)
	 ((and (equal -1 (cdr n)) (equal 1 (cdr d)))
	  ;; sum(binomial(a-k,b+k),k,l,h)=sum(binomial(a+b-k,k),k,l+b,h+b)
	  (let ((h1 (m+ h (car d)))
		(l1 (m+ l (car d)))
		(a1 (m+ (car n) (car d))))
	    ;; sum(binomial(a1-k,k),k,0,floor(a1/2))=fib(a1+1)
	    ;; we only do sums with h>floor(a1/2)
	    (if (and (integerp l1)
		     (member (asksign (m- h1 (m// a1 2))) '($zero $positive) :test #'eq))
		(progn
		  (adsum (m* y ($fib (m+ a1 1))))
		  (when (> l1 0)
		    (adsum (m* -1 y (dosum (list '(%binomial) (m- a1 poly-var) poly-var)
					   poly-var 0 (m- l1 1) t :evaluate-summand nil)))))
		(adusum e))))

	 ;; sum(binomial(n,2*k),k,0,floor(n/2))=2^(n-1)
	 ;; sum(binomial(n,2*k+1),k,0,floor((n-1)/2))=2^(n-1)
	 ((and (equal 0 (cdr n)) (equal 2 (cdr d)))
	  ;; sum(binomial(a,2*k+b),k,l,h)=sum(binomial(a,2*k),k,l+b/2,h+b/2), b even
	  ;; sum(binomial(a,2*k+b),k,l,h)=sum(binomial(a,2*k+1),k,l+(b-1)/2,h+(b-1)/2), b odd
	  (let ((a (car n))
		(r1 (if (oddp (car d)) 1 0))
		(l1 (if (oddp (car d))
			(m+ l (truncate (1- (car d)) 2))
			(m+ l (truncate (car d) 2)))))
	    (when (and (integerp l1)
		       (member (asksign (m- a hi)) '($zero $positive) :test #'eq))
	      (adsum (m* y (m^ 2 (m- a 1))))
	      (when (> l1 0)
		(adsum (m* -1 y (dosum (list '(%binomial) a (m+ poly-var poly-var r1))
				       poly-var 0 (m- l1 1) t :evaluate-summand nil)))))))

	 ;; other sums we can't do
	 (t
	  (adusum e)))))

  (defun isum (e lo poly-var)
    (labels
	((isum-giveup (e)
	   (cond ((atom e) nil)
		 ((eq (caar e) 'mexpt)
		  (not (or (free (cadr e) poly-var)
			   (ratp (caddr e) poly-var))))
		 ((member (caar e) '(mplus mtimes) :test #'eq)
		  (some #'identity (mapcar #'isum-giveup (cdr e))))
		 (t)))
	 (isum1 (e lo)
	   (cond ((free e poly-var)
		  (unless (eq (asksign e) '$zero)
		    (throw 'isumout 'divergent)))
		 ((ratp e poly-var)
		  (adsum (ipolysum e lo)))
		 ((eq (caar e) 'mplus)
		  (mapc #'(lambda (x) (isum1 x lo)) (cdr e)))
		 ( (isgeo e lo))
		 ((adusum e))))
	 (ipolysum (e lo)
	   (ipoly1 ($expand e) lo))
	 (ipoly1 (e lo)
	   (cond ((smono e poly-var)
		  (ipoly2 *a *n lo (asksign (simplify (list '(mplus) *n 1)))))
		 ((mplusp e)
		  (cons '(mplus) (mapcar #'(lambda (x) (ipoly1 x lo)) (cdr e))))
		 (t (adusum e)
		    0)))
	 (ipoly2 (a n lo sign)
	   (cond ((member (asksign lo) '($zero $negative) :test #'eq)
		  (throw 'isumout 'divergent)))
	   (unless (equal lo 1)
	     (let (($simpsum t))
	       (adsum `((%sum)
			((mtimes) ,a -1 ((mexpt) ,poly-var ,n))
			,poly-var 1 ((mplus) -1 ,lo)))))
	   (cond ((eq sign '$negative)
		  (list '(mtimes) a ($zeta (meval (list '(mtimes) -1 n)))))
		 ((throw 'isumout 'divergent))))
	 (isgeo (e lo)
	   (let ((r ($ratsimp (div* (maxima-substitute (list '(mplus) poly-var 1) poly-var e) e))))
	     (and (free r poly-var)
		  (isgeo1 (maxima-substitute lo poly-var e)
			  r (asksign (simplify (list '(mplus) (list '(mabs) r) -1)))))))
	 (isgeo1 (a r sign)
	   (cond ((eq sign '$positive)
		  (throw 'isumout 'divergent))
		 ((eq sign '$zero)
		  (throw 'isumout 'divergent))
		 ((eq sign '$negative)
		  (adsum (list '(mtimes) a
			       (list '(mexpt) (list '(mplus) 1 (list '(mtimes) -1 r)) -1)))))))
      (cond ((isum-giveup e)
	     (setq combin-sum nil combin-usum (list e)))
	    ((eq (catch 'isumout (isum1 e lo)) 'divergent)
	     (merror (intl:gettext "sum: sum is divergent."))))))

  (defun sumsum (e poly-var lo hi *plus *times)
    (setf combin-sum nil)
    (setf combin-usum nil)
    (labels
	((finite-sum (e y lo hi)
	   (cond ((null e))
		 ((free e poly-var)
		  (adsum (m* y e (m+ hi 1 (m- lo)))))
		 ((poly? e poly-var)
		  (adsum (m* y (fpolysum e lo hi poly-var))))
		 ((eq (caar e) '%binomial) (fbino e y lo hi poly-var))
		 ((eq (caar e) 'mplus)
		  (mapc #'(lambda (q) (finite-sum q y lo hi)) (cdr e)))
		 ((and (or (mtimesp e) (mexptp e) (mplusp e))
		       (fsgeo e y lo hi)))
		 (t
		  (adusum e)
		  nil)))
	 (fsgeo (e y lo hi)
	   (let ((r ($ratsimp (div* (maxima-substitute (list '(mplus) poly-var 1) poly-var e) e))))
	     (cond ((equal r 1)
		    (adsum
		     (list '(mtimes)
			   (list '(mplus) 1 hi (list '(mtimes) -1 lo))
			   (maxima-substitute lo poly-var e))))
		   ((free r poly-var)
		    (adsum
		     (list '(mtimes) y
			   (maxima-substitute 0 poly-var e)
			   (list '(mplus)
				 (list '(mexpt) r (list '(mplus) hi 1))
				 (list '(mtimes) -1 (list '(mexpt) r lo)))
			   (list '(mexpt) (list '(mplus) r -1) -1))))))))
      (cond ((eq hi '$inf)
	     (cond (*infsumsimp
		    (isum e lo poly-var))
		   ((setq combin-usum (list e)))))
	    ((finite-sum e 1 lo hi)))
      (cond ((eq combin-sum nil)
	     (return-from sumsum (list '(%sum) e poly-var lo hi))))
      (setq *plus
	    (nconc (mapcar
		    #'(lambda (q) (simptimes (list '(mtimes) *times q) 1 nil))
		    combin-sum)
		   *plus))
      (values (and combin-usum (setq combin-usum (list '(%sum) (simplus (cons '(plus) combin-usum) 1 t) poly-var lo hi)))
	      *plus))))

;; Is this guy actually looking at the value of its middle arg?

(defun simpprod (x y z)
  (let (($ratsimpexpons t))
    (cond ((equal y 1)
	   (setq y (maybe-simplifya (cadr x) z)))
	  ((setq y (simptimes (list '(mexpt) (cadr x) y) 1 z)))))
  (simpprod1 y (caddr x)
	     (maybe-simplifya (cadddr x) z)
	     (maybe-simplifya (cadr (cdddr x)) z)))

