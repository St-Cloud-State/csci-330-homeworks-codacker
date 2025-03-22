(defun match (input pattern)
  "Checks if the input matches the given pattern."
  (if (equal input pattern)
      t
      nil))

(defun parse-middle (input)
  "Checks if the middle section of the string follows the grammar rules."
  (or (match input "wo")      ; One valid possibility: "wo"
      (match input "xoy")     ; Another valid possibility: "xoy"
      (and (<= 3 (length input))  ; Checking longer patterns
           (char= (aref input 0) #\x)
           (char= (aref input 1) #\o)
           (char= (aref input (1- (length input))) #\z))))  ; Must end with 'z'

(defun parse-s (input)
  "Recursive function to check if input starts with 'i' and ends with 's' with a valid structure in between."
  (and (not (string= input ""))
       (char= (aref input 0) #\i)  ; Must start with 'i'
       (char= (aref input (1- (length input))) #\s)  ; Must end with 's'
       (parse-middle (subseq input 1 (1- (length input))))))  ; Check middle part

(defun parse (input)
"Main function to validate the string according to the grammar."
  (if (and (not (string= input "")) 
           (char= (aref input 0) #\i)  ; Ensure it starts with 'i'
           (parse-s input))
      "VALID"
      "INVALID"))

(defun run-tests ()
  (format t "~%Running Tests...~%")
  
  (let ((test-cases '(("iwos" "VALID")
                      ("ixoys" "VALID")
                      ("ixozs" "VALID")
                      ("ixoozs" "VALID")
                      ("ixoooozs" "VALID")
                      ("ixooooooooozs" "VALID")
                      ("ixooooooooooooozs" "VALID")
                      ("iwoxs" "INVALID")
                      ("xoys" "INVALID")
                      ("ixoz" "INVALID")
                      ("iwzxs" "INVALID")
                      ("izoozs" "INVALID")
                      ("iwowos" "INVALID")
                      ("ixoyz" "INVALID"))))
    
    (dolist (case test-cases)
      (let ((input (first case))
            (expected (second case))
            (result (parse (first case))))
        (format t "Test: (parse \"~a\") => ~a (Expected: ~a)~%" input result expected)))))

;; Run tests automatically when the file is loaded
(run-tests)