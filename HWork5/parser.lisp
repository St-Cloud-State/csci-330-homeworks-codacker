(defun match (input pattern)
  "Checks if the input matches the given pattern."
  (if (equal input pattern)
      t
      nil))

(defun parse-middle (input)
  "Checks if the middle section of the string follows the grammar rules. Returns an error message if invalid."
  (cond
    ((match input "wo") t)
    ((match input "xoy") t)
    ((and (<= 3 (length input))  
          (char= (aref input 0) #\x)
          (char= (aref input 1) #\o)
          (char= (aref input (1- (length input))) #\z)) t)
    (t (format nil "Unexpected symbol at position ~a" (1+ (length input))))))  ;; Fixed: Changed (length input) to (1+ (length input))

(defun parse-s (input)
  "Recursive function to check if input starts with 'i' and ends with 's' with a valid structure in between."
  (if (or (string= input "")
          (not (char= (aref input 0) #\i)))
      (format nil "Unexpected symbol at position 1")  ;; Start position is fixed to 1
      (if (not (char= (aref input (1- (length input))) #\s))
          (format nil "Unexpected symbol at position ~a" (1- (length input)))  ;; Fixed the position here too
          (let ((middle-result (parse-middle (subseq input 1 (1- (length input)))))) 
            (if (stringp middle-result)
                middle-result
                t)))))

(defun parse (input)
  "Main function to validate the string according to the grammar. Returns error messages for invalid cases."
  (let ((result (parse-s input)))
    (if (equal result t)
        "VALID"
        result)))

(defun run-tests ()
  (format t "~%Running Tests...~%")
  
  (let ((test-cases '(("iwos" "VALID")
                      ("ixoys" "VALID")
                      ("ixozs" "VALID")
                      ("ixoozs" "VALID")
                      ("ixoooozs" "VALID")
                      ("ixooooooooozs" "VALID")
                      ("ixooooooooooooozs" "VALID")
                      ("iwoxs" "Unexpected symbol at position 4")
                      ("xoys" "Unexpected symbol at position 0")
                      ("ixoz" "Unexpected symbol at position 3")
                      ("iwzxs" "Unexpected symbol at position 2")
                      ("izoozs" "Unexpected symbol at position 1")
                      ("iwowos" "Unexpected symbol at position 2")
                      ("ixoyz" "Unexpected symbol at position 4"))))
    
    (dolist (case test-cases)
      (let ((input (first case))
            (expected (second case))
            (result (parse (first case))))
        (format t "Test: (parse \"~a\") => ~a (Expected: ~a)~%" input result expected)))))

;; Run tests automatically when the file is loaded
(run-tests)
