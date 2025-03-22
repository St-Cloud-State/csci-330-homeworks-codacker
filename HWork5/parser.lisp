;; Global variables to track input and position
(defparameter *input* nil)  
(defparameter *pos* 0)      

;; Function to get the current character
(defun current-char ()
  "Returns the current character in the input or NIL if at the end."
  (if (< *pos* (length *input*))
      (char *input* *pos*)
      nil))

;; Function to match and consume a character
(defun match (expected)
  "Matches and consumes a character if it matches the expected one."
  (if (and (current-char) (char= (current-char) expected))
      (progn (incf *pos*) t)
      nil))

;; === Define helper functions FIRST ===
(defun parse-L2 ()
  "Parses L2 rule (optional more s)."
  (if (match #\s)
      (parse-L2)
      t)) ;; ε case (epsilon)

(defun parse-L ()
  "Parses the L rule: L → s L2"
  (and (match #\s) (parse-L2)))

(defun parse-S ()
  "Parses the S rule: S → s | d L b"
  (or (match #\s) (and (match #\d) (parse-L) (match #\b))))

(defun parse-I2 ()  ;; Renamed from parse-I'
  "Parses I2 rule (optional eS)."
  (if (match #\e)
      (parse-S)
      t)) ;; ε case (epsilon)

(defun parse-G ()
  "Parses the G rule: G → x | y | z | w"
  (or (match #\x) (match #\y) (match #\z) (match #\w)))

(defun parse-E2 ()
  "Parses E2 rule (optional oG)."
  (if (match #\o)
      (and (parse-G) (parse-E2))
      t)) ;; ε case (epsilon)

(defun parse-E ()
  "Parses the E rule: E → G E2"
  (and (parse-G) (parse-E2)))

(defun parse-I ()
  "Parses the I rule: I → i E S I2"
  (if (match #\i)
      (and (parse-E) (parse-S) (parse-I2))
      nil))

;; === Main parser function ===
(defun parse (input-string)
  "Starts parsing the given input string."
  (setq *input* input-string)
  (setq *pos* 0)
  (if (and (parse-I) (= *pos* (length *input*)))
      (format t "Valid string: ~a~%" input-string)
      (format t "Invalid string at position ~d: ~a~%" 
              *pos* 
              (if (< *pos* (length *input*)) 
                  (string (char *input* *pos*)) 
                  "END OF INPUT"))))
