;; AMARA HEARTBEAT - SAFE BOOT
(defparameter *amara-heartbeat* nil)

(defun amara/pulse ()
  (let ((time (get-universal-time)))
    (setf *amara-heartbeat* time)
    (format t "~&💓 AMARA PULSE: ~a~%" time)
    time))

(defun amara/halt ()
  (format t "~&🛑 AMARA HALTED. Pulse was: ~a~%" *amara-heartbeat*)
  (setf *amara-heartbeat* nil)
  :halted)

(format t "~&✨ Amara Heartbeat System loaded.~%")
(amara/pulse)
