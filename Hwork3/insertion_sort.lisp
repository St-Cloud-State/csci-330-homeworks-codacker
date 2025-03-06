;; Function to insert an element into a sorted list
(defun insert (x lst)
  (cond
    ((null lst) (list x))  ;; If the list is empty, return a list with x
    ((<= x (car lst)) (cons x lst))  ;; If x is smaller, place it at the front
    (t (cons (car lst) (insert x (cdr lst))))))  ;; Otherwise, insert recursively

;; Function to perform insertion sort
(defun insertion-sort (lst)
  (if (null lst)
      nil  ;; Base case: empty list
      (insert (car lst) (insertion-sort (cdr lst)))))  ;; Insert first element into sorted rest

;; Test cases
(format t "Sorted List: ~a~%" (insertion-sort '(4 2 9 1 5 6 8 3 7)))  
;; Expected Output: (1 2 3 4 5 6 7 8 9)

(format t "Sorted List: ~a~%" (insertion-sort '(10 3 15 7 8 23 74 18)))  
;; Expected Output: (3 7 8 10 15 18 23 74)
