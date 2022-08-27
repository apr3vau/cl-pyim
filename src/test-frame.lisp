(in-package :cl-pyim)

;;;;;;;;;;;;;;;;

;; Edit here to dict directory

(defvar *dict-directory-path*
  "~/common-lisp/cl-pyim/default-dict/")

;;;;;;;;;;;;;;;;

;; Edit here to your CJK font.

(mcclim-truetype::register-ttf-font
 (find-port)
 #P"/opt/X11/share/fonts/TTF/XiaolaiMonoSC-Regular.ttf" t)

(defvar *chinese-text-style*
  (make-text-style "Xiaolai Mono SC" "Regular" 18))

;;;;;;;;;;;;;;;;

(define-application-frame test-frame ()
  ()
  (:menu-bar t)
  (:panes
   (preview :application :height 200 :width 600
			 :incremental-redisplay t)
   (result :application :height 200)
   (field :text-field))
  (:layouts (default (vertically ()
		       preview result field))))

(defvar *last-list* nil)

(define-presentation-type pinyin ()
  :inherit-from t)

(defun refresh-pinyin (&rest args)
  (declare (ignore args))
  (ignore-errors
   (let* ((frame (find-application-frame 'test-frame
					 :create nil
					 :activate nil))
	  (field (find-pane-named frame 'field))
	  (field-value (gadget-value field))
	  (preview (find-pane-named frame 'preview))
	  (result (find-pane-named frame 'result)))
     (window-clear preview)
     (uiop:if-let ((num (parse-integer
			 (string (aref field-value
				       (1- (length field-value))))
			 :junk-allowed t)))
       (when *last-list*
	 (let ((target (nth (1- num) *last-list*)))
	   (with-room-for-graphics (result)
	     (with-text-style (result *chinese-text-style*)
	       (format result "~a" (if (eql (type-of (car target)) 'word)
				       (word-chinese (car target))
				       (car target)))))
	   (setf (gadget-value field) (cdr target))))
       ;; Copy from demo. Work well amazingly
       (with-output-recording-options (preview :record t :draw t)
	 (with-output-as-presentation (preview 'pinyin 'pinyin)
	   (with-text-style (preview *chinese-text-style*)
	     (format preview "~:{~a.~a ~}"
		     (let ((r (match-candidates field-value)))
		       (setf *last-list* r)
		       (loop for i from 1 to (min 100 (length r))
			     for (j . nil) in r
			     collect (list i (if (eql (type-of j) 'word)
						 (word-chinese j)
						 j)))))))))
     (scroll-extent preview 0 0))))

;;;;;;;;;;;;;;;;

;; Refresh every 0.3s. Match will cost less than 0.1s.

(defparameter *refresh-thread* nil)

(defun run-test-frame ()
  (ignore-errors (dict-init *dict-directory-path*))
  (mapc #'(lambda (thread)
	    (if (string= (bt:thread-name thread)
			 "refresh-thread")
		(bt:destroy-thread thread)))
	(bt:all-threads))
  (setf *refresh-thread*
	(bt:make-thread
	 #'(lambda ()
	     (loop (refresh-pinyin)
		   (sleep 0.3)))
	 :name "refresh-thread"))
  (run-frame-top-level
   (make-application-frame 'test-frame)))
