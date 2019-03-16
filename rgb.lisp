;;; -*- mode: lisp; syntax: common-lisp; package: simple-rgb encoding: utf-8 -*-
;;; Author: William S. Annis
;;; $Id$
;;;
;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(in-package :simple-rgb)

(deftype rgb ()
  '(vector (unsigned-byte 8) 3))

(declaim (inline rgb))
(defun rgb (r g b)
  (make-array '(3) :element-type '(unsigned-byte 8)
                   :initial-contents (list r g b)))
(declaim (notinline rgb))

(declaim (inline rgb=))
(defun rgb= (a b)
  (declare (type rgb a b))
  (and (= (aref a 0) (aref b 0))
       (= (aref a 1) (aref b 1))
       (= (aref a 2) (aref b 2))))
(declaim (notinline rgb=))


(deftype hsv ()
  '(vector (float 0.0e0 1.0e0) 3))

(define-condition hsv-type-error (type-error)
  ((bugged-vector :initarg :bugged :accessor bugged-hsv-vector))
  (:report (lambda (condition stream)
             (format stream
                 "all elements of HSV must be floats between 0.0 and 1.0: ~A"
                 (bugged-hsv-vector condition)))))

(declaim (inline hsv))
(defun hsv (h s v)
  ;; The :ELEMENT-TYPE option in MAKE-ARRAY will only raise a condition on
  ;; type oddities if you try to insert a float that wants too much space
  ;; (CLHS 15.1.2.1).
  (unless (and (typep h '(float 0.0e0 1.0e0))
               (typep s '(float 0.0e0 1.0e0))
               (typep v '(float 0.0e0 1.0e0)))
    (error (make-condition 'hsv-type-error :bugged (vector h s v))))
  (make-array '(3) :element-type '(float 0.0e0 1.0e0)
                   :initial-contents (list h s v)))
(declaim (notinline hsv))

(declaim (inline parse))
(defun parse (string)
  (let* ((string (if (eq (aref string 0) #\#) (subseq string 1) string))
         (length (length string))
         (data
          (if (< 3 length)
              (loop for i from 0 to 4 by 2
                 collect (parse-integer string :start i :end (+ 2 i) :radix 16))
              (loop for i from 0 to 2
                 for digit = (parse-integer string :start i :end (+ 1 i) :radix 16)
                 collect (+ (* 16 digit) digit)))))
    (apply #'rgb data)))
(declaim (notinline parse))


(defparameter +rgb-black+ (rgb 0 0 0))
(defparameter +rgb-white+ (rgb 255 255 255))

;;; ALPHA weights the mix, 0.0 favoring the first color, 1.0 the second.
(declaim (inline mix-rgb))
(defun mix-rgb (a b &key (alpha 0.5))
  (declare (type (float 0.0 1.0) alpha)
           (type rgb a b))
  (let ((c (rgb 0 0 0)))
    (dotimes (i 3 c)
      (setf (aref c i)
            (round (+ (aref a i)
                      (* alpha
                         (- (aref b i)
                            (aref a i)))))))))
(declaim (notinline mix-rgb))

;;; This one overwrites the first argument with the mixed color.
(declaim (inline mix-rgb!))
(defun mix-rgb! (a b &key (alpha 0.5))
  (declare (type (float 0.0 1.0) alpha)
           (type rgb a b))
  (dotimes (i 3 a)
    (setf (aref a i)
          (round (+ (aref a i)
                    (* alpha
                       (- (aref b i)
                          (aref a i))))))))
(declaim (notinline mix-rgb!))

;;; http://en.wikipedia.org/wiki/Grayscale
(declaim (inline greyscale-rgb))
(defun greyscale-rgb (a)
  (declare (type rgb a))
  (let ((gs (round (+ (* .3 (aref a 0))
                      (* .59 (aref a 1))
                      (* .11 (aref a 2))))))
    (rgb gs gs gs)))
(declaim (notinline greyscale-rgb))

(declaim (inline lighten-rgb))
(defun lighten-rgb (a &key (alpha 0.5))
  (mix-rgb a +rgb-white+ :alpha alpha))
(declaim (notinline lighten-rgb))

(declaim (inline lighten-rgb!))
(defun lighten-rgb! (a &key (alpha 0.5))
  (mix-rgb! a +rgb-white+ :alpha alpha))
(declaim (notinline lighten-rgb!))

(declaim (inline darken-rgb))
(defun darken-rgb (a &key (alpha 0.5))
  (mix-rgb a +rgb-black+ :alpha alpha))
(declaim (notinline darken-rgb))

(declaim (inline darken-rgb!))
(defun darken-rgb! (a &key (alpha 0.5))
  (mix-rgb! a +rgb-black+ :alpha alpha))
(declaim (notinline darken-rgb!))

(declaim (inline invert-rgb))
(defun invert-rgb (a)
  (rgb (- 255 (aref a 0))
       (- 255 (aref a 1))
       (- 255 (aref a 2))))
(declaim (notinline invert-rgb))

;;; http://livedocs.adobe.com/en_US/Illustrator/13.0/help.html?content=WS714a382cdf7d304e7e07d0100196cbc5f-6288.html
;;; This does nothing interesting to greys.
(declaim (inline complement-rgb))
(defun complement-rgb (a)
  (declare (type rgb a))
  (let* ((r (aref a 0))
         (g (aref a 1))
         (b (aref a 2))
         (min+max (+ (min r g b) (max r g b))))
    (rgb (- min+max r)
         (- min+max g)
         (- min+max b))))
(declaim (notinline complement-rgb))

(declaim (inline contrast-rgb))
(defun contrast-rgb (a &optional (cut 0.5))
  (declare (type rgb a))
  (let ((cutoff (round (* cut 255))))
    (labels ((contrastify (color-component)
               (if (>= cutoff color-component) 0 255)))
      (rgb (contrastify (aref a 0))
           (contrastify (aref a 1))
           (contrastify (aref a 2))))))
(declaim (notinline contrast-rgb))

(declaim (inline xmlify-rgb))
(defun xmlify-rgb (a &optional (stream nil))
  (declare (type rgb a))
  (format stream "#~2,'0X~2,'0X~2,'0X" (aref a 0) (aref a 1) (aref a 2)))
(declaim (notinline xmlify-rgb))


(declaim (inline rgb->hsv))
(defun rgb->hsv (a)
  (declare (type rgb a))
  (let* ((r (/ (aref a 0) 255.0))
         (g (/ (aref a 1) 255.0))
         (b (/ (aref a 2) 255.0))
         (max (max r g b))
         (min (min r g b))
         (v max))
    (if (= max min)
        (hsv 0.0 0.0 v)
        (let ((s (/ (- max min) max))
              (h 0))
          (cond ((= r max)
                 (setf h (- (/ (- max b) (- max min))
                            (/ (- max g) (- max min)))))
                ((= g max)
                 (setf h (+ 2.0 (- (/ (- max r) (- max min))
                                   (/ (- max b) (- max min))))))
                (t (setf h (+ 4.0 (- (/ (- max g) (- max min))
                                     (/ (- max r) (- max min)))))))
          (setf h (mod (/ h 6.0) 1))
          (hsv h s v)))))
(declaim (notinline rgb->hsv))

(declaim (inline hsv->rgb))
(defun hsv->rgb (a)
  (declare (type hsv a))
  (let ((h (aref a 0))
        (s (aref a 1))
        (v (aref a 2)))
    (labels ((rgb-from-floats (r g b)
               (apply #'rgb (mapcar #'(lambda (c)
                                        (coerce (round (* c 255)) 'integer))
                                    (list r g b)))))
      (if (= s 0.0)
          (rgb-from-floats v v v)
          (multiple-value-bind (i f) (truncate (* h 6.0))
            (let* ((p (* v (- 1.0 s)))
                   (q (* v (- 1.0 (* s f))))
                   (tv (* v (- 1.0 (* s (- 1.0 f))))))
              (cond ((= (mod i 6) 0) (rgb-from-floats v tv p))
                    ((= i 1) (rgb-from-floats q v p))
                    ((= i 2) (rgb-from-floats p v tv))
                    ((= i 3) (rgb-from-floats p q v))
                    ((= i 4) (rgb-from-floats tv p v))
                    ((= i 5) (rgb-from-floats v p q)))))))))
(declaim (notinline hsv->rgb))

;;; (hsv->rgb (rotate-hsv (rgb->hsv color) 180)) == (complement-rgb color)
(declaim (inline rotate-hsv))
(defun rotate-hsv (a rotation)
  (declare (type hsv a))
  (let ((h (aref a 0))
        (s (aref a 1))
        (v (aref a 2))
        (scaled-rotation (/ rotation 360.0)))
    (hsv (mod (+ h scaled-rotation) 1.0) s v)))
(declaim (notinline rotate-hsv))

(declaim (inline rotate-rgb))
(defun rotate-rgb (a rotation)
  (hsv->rgb (rotate-hsv (rgb->hsv a) rotation)))
(declaim (notinline rotate-rgb))

;;; rgb.lisp ends here
