;; Function to partition a list into two nearly equal halves
(defun partition (lst)
  (if (or (null lst) (null (cdr lst))) 
      (list lst nil)  ;; If empty or single element, return (lst, nil)
      (let ((result (partition (cddr lst))))  ;; Recursively partition the rest
        (list (cons (car lst) (car result)) (cons (cadr lst) (cadr result))))))

;; Function to merge two sorted lists
(defun merge (lst1 lst2)
  (cond
    ((null lst1) lst2)  ;; If lst1 is empty, return lst2
    ((null lst2) lst1)  ;; If lst2 is empty, return lst1
    ((<= (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2)))  ;; Pick smaller
    (t (cons (car lst2) (merge lst1 (cdr lst2))))))  ;; Pick smaller

;; Mergesort function using partition and merge
(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))  ;; Base case: empty or single-element list
      lst
      (let* ((parts (partition lst))
             (sorted1 (mergesort (car parts)))  ;; Recursively sort first half
             (sorted2 (mergesort (cadr parts))))  ;; Recursively sort second half
        (merge sorted1 sorted2))))  ;; Merge the sorted halves

;; Test cases
(print (mergesort '(4 2 9 1 5 6 8 3 7)))  ;; Expected Output: (1 2 3 4 5 6 7 8 9)
(print (mergesort '(10 3 15 7 8 23 74 18)))  ;; Expected Output: (3 7 8 10 15 18 23 74)
