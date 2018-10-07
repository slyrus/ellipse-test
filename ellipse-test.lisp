;;;; ellipse-test.lisp

(in-package #:ellipse-test)

(defun draw-test-ellipses (stream)
  (my-draw-ellipse stream 100 300 70 70 -10 10 :ink +blue+)

  (draw-ellipse* stream 230 80 30 0 0 10 :filled t
                 :start-angle 0 :end-angle (/ (* 6 pi) 4))

  (draw-ellipse* stream 100 200 70 70 -10 10 :ink +blue+ :filled t
                 :start-angle 0 :end-angle 3 :filled t)

  (draw-ellipse* stream 100 230 70 70 -10 10 :ink +blue+ :filled nil
                 :start-angle 0.3 :end-angle pi :filled t)

  (draw-ellipse* stream 200 80 50 0 0 50 :ink +red+ :filled nil
                 :start-angle 0 :end-angle (* 0.3 (/ pi 2)))

  (draw-ellipse* stream 200 100 50 0 0 50 :ink +red+ :filled nil
                 :start-angle (/ pi 2) :end-angle (* 6 (/ pi 8)))

  (draw-ellipse* stream 100 100 25 0 0 25 :ink +red+ :filled nil
                 :start-angle 0 :end-angle pi)

  (draw-ellipse* stream 250 200 10 -40 1 -20 :ink +dark-green+ :filled t)

  (draw-line* stream 230 200 270 200 :ink +red+ :line-thickness 4)

  (draw-ellipse* stream 400 200 -50 50 0 10 :ink +orange+)

  (draw-ellipse* stream 300 300 30 0 0 10 :filled nil
                 :start-angle 0 :end-angle (/ (* 6 pi) 4))

  (draw-ellipse* stream 350 200 10 -40 1 -20 :ink +dark-green+ :filled t))

(defun draw-ellipses (stream)
  (draw-ellipse* stream 300 100 50 -30 20 8 :filled nil :line-thickness 6
                 :start-angle 0 :end-angle pi)

  (draw-ellipse* stream 100 100 50 -30 20 8 :filled nil :line-thickness 6
                 :start-angle 0 :end-angle (* 6 (/ pi 4)))

  (draw-ellipse* stream 200 100 50 -30 20 8 :filled nil :line-thickness 6)

  (draw-ellipse* stream 400 200 -50 50 0 10 :ink +orange+)

  (draw-circle* stream 300 200 -50 :ink +orange+ :start-angle 0 :end-angle (/ pi 2))

  (draw-ellipse* stream 200 200 -50 50 0 10 :ink +orange+ :start-angle 0 :end-angle (/ pi 2))

  (draw-circle* stream 100 200 -50 :ink +orange+ :start-angle 0 :end-angle (/ pi 2)))

(defun display-ellipse (frame pane)
  (declare (ignore frame))
  (draw-ellipses pane))

(defun ellipse-test-pdf (&key (file "/tmp/ellipse-test.pdf") (device-type :a4))
  (with-open-file (file-stream file :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (clim-pdf::with-output-to-pdf-stream
        (stream file-stream
                :header-comments '(:title (name task))
                :scale-to-fit t
                :device-type device-type)
      (draw-ellipses stream))))

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




;;; not used
;;;
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

(defun transformation-angle (tr)
  (let ((x1 0) (y1 1)
        (x2 1) (y2 0))
    (multiple-value-bind (tx1 ty1)
        (transform-position tr x1 y1)
      (multiple-value-bind (tx2 ty2)
          (transform-position tr x2 y2)
        (- (find-angle* tx1 ty1 tx2 ty2) (/ pi 2))))))

(let ((tr (clim:make-transformation 1 0 0 -1 0 0)))
  (transformation-angle tr))
;;;
;;;

