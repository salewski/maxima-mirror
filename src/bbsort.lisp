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

(defun bbsort1 (l)
  (prog (sl sl1 antisym-sign)
     (if (or (null l) (null (cdr l))) (return (values l antisym-sign))
	 (setq sl (list nil (car l))))
     loop (setq l (cdr l))
     (if (null l) (return (values (nreverse (cdr sl)) antisym-sign)))
     (setq sl1 sl)
     loop1(cond ((null (cdr sl1)) (rplacd sl1 (cons (car l) nil)))
		((alike1 (car l) (cadr sl1)) (return (values 0 nil)))
		((great (car l) (cadr sl1)) (rplacd sl1 (cons (car l) (cdr sl1))))
		(t (setq antisym-sign (not antisym-sign) sl1 (cdr sl1)) (go loop1)))
     (go loop)))

