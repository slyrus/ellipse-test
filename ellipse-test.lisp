;;;; ellipse-test.lisp

(in-package #:ellipse-test)


(defun square (x)
  (* x x))

(defun vec-len (x1 y1)
  (sqrt (+ (square x1)
           (square y1))))

(defun ellipse-point (center-x center-y
                      radius1-dx radius1-dy radius2-dx radius2-dy
                      theta)
  (values (+ center-x
             (* radius1-dx (sin theta))
             (* radius2-dx (sin theta)))
          (+ center-y
             (* radius1-dy (cos theta))
             (* radius2-dy (cos theta)))))

;; angle = atan2(vector2.y, vector2.x) - atan2(vector1.y, vector1.x);

;; and you may want to normalize it to the range 0 .. 2 * Pi:

;; if (angle < 0) angle += 2 * M_PI;

(defun rad-to-deg (theta)
  (* 180 (/ theta pi)))

(defun dot-prod (x1 y1 x2 y2)
  (+ (* x1 y1) (* x2 y2)))

(defun normalize-angle (theta)
  (if (minusp theta)
      (+ theta (* pi 2))
      theta))

(defun find-angle* (x1 y1 x2 y2)
  (let ((theta (- (phase (complex y2 x2))
                  (phase (complex y1 x1)))))
    (normalize-angle theta)))

(defun find-angle-2 (x1 y1 x2 y2)
  (let ((dp (dot-prod x1 y1 x2 y2))
        (magxy (* (vec-len x1 y1) (vec-len x2 y2))))
    (acos (normalize-angle (/ dp magxy)))))

(defun find-angle (x1 y1)
  (let ((angle (acos (/ x1 (vec-len x1 y1)))))
    (if (minusp y1)
        (- (* 2 pi) angle)
        angle)))

(defun reparameterize-ellipse (radius1-dx radius1-dy radius2-dx radius2-dy)
  (let ((a (vec-len radius1-dx radius1-dy))
        (b (vec-len radius2-dx radius2-dy))) 
    (let ((theta
           (if (> b a)
               (progn
                 (rotatef a b)
                 (find-angle radius2-dx radius2-dy))
               (find-angle radius1-dx radius1-dy))))
      (values a b theta))))

(defun ell (eta center-x center-y a b theta)
  (values (+ center-x
             (* a (cos theta) (cos eta))
             (- (* b (sin theta) (sin eta))))
          (+ center-y
             (* a (sin theta) (cos eta))
             (* b (cos theta) (sin eta)))))

(defun ell* (eta
             center-x center-y
             radius1-dx radius1-dy radius2-dx radius2-dy)
  (multiple-value-bind (a b theta)
      (reparameterize-ellipse radius1-dx radius1-dy radius2-dx radius2-dy)
    (ell eta center-x center-y a b theta)))

(defun ell-prime (eta a b theta)
  (values (+ (- (* a (cos theta) (sin eta)))
             (- (* b (sin theta) (cos eta))))
          (+ (- (* a (sin theta) (sin eta)))
             (* b (cos theta) (cos eta)))))

(defun ell-prime* (eta
                   center-x center-y
                   radius1-dx radius1-dy radius2-dx radius2-dy)
  (declare (ignore center-x center-y))
  (multiple-value-bind (a b theta)
      (reparameterize-ellipse radius1-dx radius1-dy radius2-dx radius2-dy)
    (ell-prime eta a b theta)))

(let ((center-x 200)
      (center-y 200)
      (radius1-dx 100)
      (radius1-dy 200)
      (radius2-dx 100)
      (radius2-dy 100)
      (eta 0))
  (values
   (ell* eta center-x center-y
      radius1-dx radius1-dy radius2-dx radius2-dy)
   (ell-prime* eta center-x center-y
           radius1-dx radius1-dy radius2-dx radius2-dy)))

(defun my-draw-ellipse (stream center-x center-y
                        radius1-dx radius1-dy radius2-dx radius2-dy &key (ink +blue+))
  #+nil
  (draw-circle* stream center-x center-y radius1-dx :ink +blue+ :line-thickness 1)
  (progn

    (draw-ellipse* stream
                   center-x center-y
                   radius1-dx radius1-dy radius2-dx radius2-dy
                   :ink +gray90+ :line-thickness 4 :filled nil)
    (flet ((draw-ellipse-segment (theta1 theta2)
             (multiple-value-bind (p1x p1y)
                 (ell* theta1
                      center-x center-y
                      radius1-dx radius1-dy radius2-dx radius2-dy)
               (multiple-value-bind (p2x p2y)
                   (ell* theta2 center-x center-y
                        radius1-dx radius1-dy radius2-dx radius2-dy)
                 (multiple-value-bind (e1x e1y)
                     (ell-prime* theta1
                                center-x center-y
                                radius1-dx radius1-dy radius2-dx radius2-dy)
                   (multiple-value-bind (e2x e2y)
                       (ell-prime* theta2
                                  center-x center-y
                                  radius1-dx radius1-dy radius2-dx radius2-dy)
                     (let ((alpha (* (sin (- theta2 theta1))
                                     (/ (- (sqrt (+ 4 (* 3 (square (tan (/ (- theta2 theta1) 2)))))) 1)
                                        3))))
                       (draw-polygon* stream
                                      (list p1x p1y
                                            #+nil
                                            (progn (+ p1x (* alpha e1x)) (+ p1y (* alpha e1y))
                                                   (- p2x (* alpha e2x)) (- p2y (* alpha e2y)))
                                            p2x p2y)
                                      :filled nil
                                      :ink ink :line-thickness 2))))))))
      (loop for a from 0 to (* 2 pi) by (/ pi 4)
         for b = (+ a (/ pi 4))
           do
           (draw-ellipse-segment a b)))))

(defun display-ellipse (frame pane)
  (declare (ignore frame))
  (my-draw-ellipse pane 100 200 -60 60 30 60 :ink +blue+)
  (my-draw-ellipse pane 300 200 40 -80 40 -30 :ink +dark-green+))

(defun ellipse-test-pdf (&key (file "/tmp/ellipse-test.pdf") (device-type :a4))
  (with-open-file (file-stream file :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (clim-pdf::with-output-to-pdf-stream
        (stream file-stream
                :header-comments '(:title (name task))
                :scale-to-fit t
                :device-type device-type)
      (draw-ellipse stream))))

(define-application-frame ellipse-test-app ()
  ()
  (:panes
   (app application-pane
        :display-function 'display-ellipse
        :display-time t
        :height 400 :width 600))
  (:layouts
   (default app)))

(defun ellipse-test-main ()
  (let ((frame (make-application-frame 'ellipse-test-app)))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

(ellipse-test-pdf)





