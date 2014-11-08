;;;; lterm.lisp

(in-package #:lterm)
(export 'lterm)
;;; "lterm" goes here. Hacks and glory await!
(defun handle-expose-event (count window gcontext words)
  (when (zerop count)
    (let* ((width (xlib:drawable-width window))
	   (height (xlib:drawable-height window))
	   (right-margin 5)
	   (left-margin 10)
	   (line-spacing (+ 3 (xlib:font-ascent (xlib:gcontext-font gcontext))))
	   (inter-word-space (xlib:text-width gcontext " "))
	   (line 1)
	   (text-cursor left-margin))
      (dolist (word words)
	(let ((word-width (xlib:text-width gcontext word)))
	  (when (> (+ text-cursor word-width right-margin) width)
	    (incf line)
	    (setf text-cursor left-margin))
	  (xlib:draw-glyphs window gcontext
			text-cursor (* line line-spacing)
			word)
	  (incf text-cursor (+ word-width inter-word-space))))))
  nil)
(defun handle-enter-event (exitp)
  (if exitp
      (format t "Mouse left window! ~%" )
      (format t "Mouse entered window! ~%" )))
(defun constituent(c)
  (and (graphic-char-p c)
       (not (char= c #\space))))
(defun white-space-split (string)
  (when (plusp (length string))
    (let ((cut-point (position-if
		      (complement #'constituent)
		      string)))
      (if cut-point
	  (cons (subseq string 0 cut-point)
		(white-space-split
		 (subseq string (1+ cut-point))))
	(list string)))))

(defun lterm (&optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (gcontext (xlib:create-gcontext
		    :drawable root-window
		    :font "-lispm-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*"
		    :foreground white
		    :background black))
	 (window (xlib:create-window
		  :parent root-window
		  :x 0 :y 0
		  :width 500 :height 250
		  :background black
		  :event-mask (xlib:make-event-mask :exposure
						    :button-press
						    :structure-notify)))
	 (width nil)
	 (height nil))
    (describe (xlib:gcontext-font gcontext))
    (xlib:map-window window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:configure-notify (w-w w-h) (setf width w-w
					 height w-h) nil)
      (:exposure (count) (handle-expose-event count window gcontext
					      (white-space-split
					       "Ragged right setting is easier than justified setting.
This is both a strength and a weakness.  Although the
regular word spacing of ragged right setting is easier on
the reader's eye, in craft work there is honour and glory in
doing things the hard way. The reader of justified text
knows of the labour and expense, and is flattered to get
something for nothing, even if it is worth what he paid.")))
      (:button-press () t))
    (xlib:destroy-window window)
    (xlib:close-display display)))
