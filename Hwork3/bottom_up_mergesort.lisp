;; Function to merge two sorted lists
(defun merge-lists (lst1 lst2)
  (cond
    ((null lst1) lst2)  ;; If lst1 is empty, return lst2
    ((null lst2) lst1)  ;; If lst2 is empty, return lst1
    ((<= (car lst1) (car lst2)) 
     (cons (car lst1) (merge-lists (cdr lst1) lst2)))  ;; Pick smaller element
    (t 
     (cons (car lst2) (merge-lists lst1 (cdr lst2)))))) ;; Pick smaller element

;; Function to partition a list into sorted pairs
(defun make-pairs (lst)
  (cond
    ((null lst) nil)  ;; Base case: empty list
    ((null (cdr lst)) (list lst))  ;; Base case: single element list
    (t (cons (merge-lists (list (car lst)) (list (cadr lst))) 
             (make-pairs (cddr lst))))))  ;; Merge pairs and recurse

;; Function to merge adjacent lists
(defun merge-adjacent (lst)
  (cond
    ((null lst) nil)  ;; Base case: empty list
    ((null (cdr lst)) lst)  ;; Base case: one list left, return it as is
    (t (cons (merge-lists (car lst) (cadr lst)) 
             (merge-adjacent (cddr lst))))))  ;; Merge adjacent pairs

;; Function to iteratively merge lists in bottom-up Mergesort
(defun bottom-up-merge (lst)
  (if (null (cdr lst)) 
      (car lst)  ;; If only one list remains, return it
      (bottom-up-merge (merge-adjacent lst))))  ;; Continue merging

;; Bottom-Up Mergesort function
(defun bottom-up-mergesort (lst)
  (bottom-up-merge (make-pairs lst)))

;; Test cases
(format t "Sorted List: ~a~%" (bottom-up-mergesort '(4 2 9 1 5 6 8 3 7)))  
;; Expected Output: (1 2 3 4 5 6 7 8 9)

(format t "Sorted List: ~a~%" (bottom-up-mergesort '(10 3 15 7 8 23 74 18)))  
;; Expected Output: (3 7 8 10 15 18 23 74)
