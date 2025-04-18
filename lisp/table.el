;; hash-table functions
;; map% each% get% fget% set% fset% update% filter% map% merge%

;; list functions
;; map@ each@ filter@ get@ set@ update@ 

;; since '() == nil; do not use empty lists! this can mess up table retrieval

(defalias 'items% 'ht-items)
(defalias 'keys% 'ht-keys)
(defalias 'values% 'ht-values)
(defalias 'first@ 'car)
(defalias 'merge% 'ht-merge)
(defalias 'last@ '-last-item)
(defalias 'first@ '-first-item)

(defun empty? (x &optional len-fn)
  (if len-fn
      (> (funcall len-fn x) 0)
    (cond
     ((ht-p x) (ht-empty? x))
     (t (= (length x) 0)))))

(defun not-empty? (x &optional len-fn)
  (not (funcall 'empty? x len-fn)))

(defalias '!empty? 'not-empty?)
(defalias 'not-empty-p 'not-empty?)
(defalias 'empty-p 'empty?)

(defun -get% (h ks)
  (let* ((k (car ks))
	 (others (cdr ks)))
    (when k
      (let* ((v (ht-get h k)))
	(if (equal others '())
	    v
	  (when (ht-p v)
	    (-get% v others)))))))

(defun -fget% (h ks)
  (let* ((k (car ks))
	 (others (cdr ks)))
    (when k
      (let* ((v (ht-get h k)))
	(if (equal others '())
	    (if (eq v nil)
		(let* ((-v (ht)))
		  (ht-set! h k -v)
		  -v)
	      v)
	  (if (ht-p v)
	      (-fget% v others)
	    (let* ((v (ht)))
	      (progn (ht-set! h k v)
		     (-fget% v others)))))))))


(defun slice@ (lst &optional start end step)
  (if (and (not start) (not end))
      lst
  (let* ((out '())
	 (len (length lst))
	 (start (or start 0))
	 (end (or end (- len 1)))
	 (start (if (>= start len) (- len 1) start))
	 (end (if (>= end len) (- len 1) end))
	 (end (if (< end 0) (+ end len) end))
	 (start (if (< start 0) (+ start len) start))
	 (step (or step 1)))
    (cl-loop for x from start below end
	     when (= 0 (% x step))
	     do (setq out (append@ out (nth x lst))))
    out)))

(defun fget% (h &rest ks)
  (-fget% h ks))

(defun get% (h &rest ks)
  (-get% h ks))

(defun fset% (h &rest ks-and-value)
  (let* ((ks (butlast ks-and-value))
	 (value (-last-item ks-and-value))
	 (found (if (= (length ks) 1)
		    h
		 (-fget% h (butlast ks)))))
    (ht-set! found (-last-item ks) value)
    h))

(defun set% (h &rest ks-and-value)
  (if-let* ((ks (butlast ks-and-value))
	    (value (-last-item ks-and-value))
	    (found (if (= (length ks) 1)
		       h
		    (-get% h (butlast ks))))
	    (is-h (ht-p found)))
      (progn (ht-set! found (-last-item ks) value)
	     h)))

(defun update% (h &rest ks-and-fn)
  (if-let* ((ks (butlast ks-and-fn))
	    (fn (last@ ks-and-fn))
	    (found (if (= (length ks) 1)
		       h
		     (apply 'get% h (butlast ks))))
	    (is-h (ht-p found))
	    (last-key (last@ ks)))
      (progn (ht-set! found
		      last-key
		      (funcall fn last-key (ht-get found last-key)))
	     h)))

(defun filter% (h fn &optional mapfn)
  (let* ((out (ht)))
    (dolist (it (ht-items h))
      (let* ((k (car it))
	     (v (-last-item it)))
	(when (funcall fn k v)
	    (if mapfn
		(ht-set! out k (funcall mapfn k v))
	      (ht-set! out k v)))))
    out))

(defun get@ (lst &rest ks)
  (if-let* ((k (car ks)))
      (progn (let* ((other-ks (cdr ks))
		    (k (if (< k 0)
			   (+ (length lst) k)
			 k))
		    (v (elt lst k)))
	       (if (eq other-ks '())
		   v
		 (if (listp v)
		     (apply #'get@ v other-ks)))))))

(defun update@ (lst &rest ks-and-fn)
  (if-let* ((ks (butlast ks-and-fn))
	    (fn (-last-item ks-and-fn))
	    (found (if (> (length ks) 1) (apply 'get@ (butlast ks)) lst))
	    (valid-found? (listp found))
	    (len (length found))
	    (last-key (last@ ks))
	    (last-key (if (< last-key 0) (+ last-key len) last-key))
	    (valid? (not (or (>= last-key len) (< last-key 0)))))
      (progn (setf (nth last-key found) (funcall fn (nth last-key found)))
	     lst)))

(defun set@ (lst &rest ks-and-value)
  (if-let* ((ks (butlast ks-and-value))
	    (value (-last-item ks-and-value))
	    (found (if (> (length ks) 1) (apply 'get@ (butlast ks)) lst))
	    (valid-found? (listp found))
	    (len (length found))
	    (last-key (last@ ks))
	    (last-key (if (< last-key 0) (+ last-key len) last-key))
	    (valid? (not (or (>= last-key len) (< last-key 0)))))
      (progn (setf (nth last-key found) value)
	     lst)))


(defun rm@ (lst &rest ks)
  (let* ((remove
		  (lambda (-lst &rest -ks)
			(if (= (length -ks) 1)
				(let* ((popped (nth (car -ks) -lst)))
				  (pop (nthcdr (last@ -ks) -lst))
				  popped)
			  (if-let* ((found (apply 'get@ -lst (butlast -ks)))
						(valid? (listp found)))
				  (progn
					(pop (nthcdr (last@ -ks) found))
					found)))))
		 (lc (length lst))
		 (ks (map@ ks (lambda (k)
						(cond
						 ((< k 0) (+ lc k))
						 ((< k lc) k)
						 (t nil))))))
	(map@ ks (partial-apply remove lst))))

(defun rm% (h &rest ks)
  (if (= (length ks) 1)
      (let* ((popped (ht-get h (car ks))))
	(ht-remove! h (last@ ks))
	popped)
    (if-let* ((found (apply 'get% h (butlast ks)))
	      (valid? (ht-p found)))
	(progn (ht-remove! found (last@ ks))
	       found))))

(defun take@ (lst n)
  (-take n lst))

(defun nth@ (lst n)
  (if (< n 0)
      (nth (- (length lst) n) lst)
   (nth n lst)))

(defun enumerate@ (lst &optional map-fn)
  (let* ((process (lambda (x i)
		    `(,@(nth i x)))))
   (cl-loop for x from 0 to (- (length lst) 1)
	    collect (let* ((v (nth@ lst x))
			   (v (if map-fn (funcall map-fn x v) v)))
		      (if map-fn v (list x v))))))

(defun range@ (start end &optional step)
  (let* ((step (or step 1)))
    (cl-loop for x from start below end
	     when (= (% x step) 0)
	     collect x)))

(defun filter% (h fn &optional map-fn)
  (let* ((out (ht)))
    (dolist (k (keys% h))
      (when-let* ((v (get% h k))
		  (ok? (funcall fn k v))
		  (v (if map-fn
			 (funcall map-fn k v)
		       v)))
	(set% out k v)))
    out))

(defun map% (h fn &optional map-keys?)
  (let* ((out (ht)))
    (dolist (k (keys% h))
      (let* ((v (funcall fn k (get% h k))))
	(if map-keys?
	    (set% out (first@ v) (last@ v))
	  (set% out k v))))   
    out))

(defun each% (h fn)
  (let* ((out (ht)))
    (cl-loop for k being the hash-keys of h
	     do (funcall fn k (get% h k)))))


(defun select@ (lst &rest ks)
  (cl-loop for x in ks
	   collect (apply 'get@ lst (if (listp x)
					x
				      (list x)))))
(defun each@ (lst fn)
  (cl-loop for x in lst do (funcall fn x)))

(defun map@ (lst fn)
  (cl-loop for x in lst collect (funcall fn x)))

(defun mapi@ (lst fn)
  (cl-loop for x from 0 below (length lst)
	   collect (funcall fn x (get@ lst x))))

(defun eachi@ (lst fn)
  (cl-loop for x from 0 below (length lst)
	   do (funcall fn x (get@ lst x))))

(defun filter@ (lst fn &optional map-fn)
  (let* ((out '()))
    (dolist (x lst)
      (when (funcall fn x)
	(let* ((v (if map-fn (funcall map-fn x) x)))
	  (setq out (append@ out v)))))
    out))

(defun filteri@ (lst fn &optional map-fn)
  (let* ((i 0)
	 (out '()))
    (dolist (x lst)
      (when (funcall fn i x)
	(let* ((v (if map-fn (funcall map-fn i x) x)))
	  (setq out (append@ out v))))
      (setq i (+ i 1)))
    out))

(defun select@ (lst &rest ks)
  (cl-loop for x in ks
	   collect (apply 'get@ lst (if (listp x)
					x
				      (list x)))))

(defun reject% (h &rest ks)
  (let* ((out (ht))
		 (KS (keys% h))
		 (selected (cl-loop for k in KS
							when (not (index@ ks k))
							collect k)))
	(apply 'select% h selected)))

(defun seq-along (lst)
  (range@ 0 (length lst)))

(defun reject@ (lst &rest ks)
  (let* ((out (ht))
		 (KS (seq-along lst))
		 (selected (cl-loop for k in KS
							when (not (index@ ks k))
							collect k)))
	(apply 'select@ lst selected)))

(defun select% (h &rest ks)
  (let* ((out (ht))
		 (required (map@ ks (lambda (k)
							  (if (listp k)
								  (apply 'get% h k)
								(get% h k))))))
	(each@ (range@ 0 (length required))
		   (lambda (i)
			 (let* ((k (nth@ ks i))
					(v (nth@ required i)))
			   (if (listp k)
				   (apply #'set% out (append@ k v))
				 (set% out k v)))))
	out))

(defun pop@ (lst &optional times)
  (let* ((out (list))
		 (ln (length lst))
		 (times (or times 1)))
	(cl-loop for i from 1 upto times
			 when (not (= ln 0))
			 do (progn
				  (setq out (append out (rm@ lst (1- ln))))
				  (setq ln (- ln 1))))
	(reverse out)))

;; ks: string | list[string]...
(defun pop% (h &rest ks)
  (let* ((out (list))
	 (required (map@ ks (lambda (k)
			      (if (listp k)
				  (apply 'get% h k)
				(get% h k))))))
    (each@ (range@ 0 (length required))
	   (lambda (i)
	     (let* ((k (nth@ ks i))
		    (v (nth@ required i)))
	       (if (listp k)
		   (progn
		     (setq out (append@ out v))
		     (apply #'rm% h k))
		 (progn (setq out (append@ out v))
			(rm% h k))))))
    out))


(defun index@ (lst elem)
  (-find-index (lambda (x) (equal x elem)) lst))

(defun index-by@ (lst pred)
  (-find-index pred lst))

(defun index-all@ (lst elem)
  (cl-loop for x in lst
	   for y from 0 below (length lst)
	   when (equal elem x)
	   collect y))

(defun index-all-by@ (lst pred)
  (cl-loop for x in lst
	   for y from 0 below (length lst)
	   when (funcall pred x)
	   collect y))

(defun contains?@ (lst item &optional pred)
  (let* ((pred (or pred 'equal)))
    (cl-loop for x in lst
	     when (apply pred (list x item))
	     return t)))

(defun contains?% (h item &optional pred)
  (let* ((pred (or pred 'equal))
	 (items (items% h))
	 (matched? (lambda (it)
		     (let* ((k (car it))
			    (v (last@ it)))
		       (when (apply pred (list v item))
			 k)))))
    (cl-loop for it in items
	     when (apply matched? (list it))
	     return t)))

(defun append@ (lst &rest elems)
  (append lst elems))

(defun lappend@ (lst &rest elems)
  (let* ((temp lst))
    (dolist (e (reverse elems))
      (setq temp (push e temp)))
    temp))

(cl-defun ht% (&optional ks &key (default-fn 'make-hash-table))
  (let* ((out (ht)))
    (when ks
      (dolist (k ks)
	(ht-set out k (funcall default-fn))))
    out))

(cl-defun list-ht% (&optional ks &key (default-fn 'list))
  (let* ((out (ht)))
    (when ks
      (dolist (k ks)
	(ht-set out k (funcall default-fn))))
    out))

(defun singleton? (s-or-lst-or-h)
  (if (or (list? s-or-lst-or-h)
	  (string? s-or-lst-or-h))
      (= (length s-or-lst-or-h) 1)
    (= 1 (length (keys% s-or-lst-or-h)))))

(defun set%@ (lst i k v)
  (when-let* ((found (get@ lst i)))
    (set% found k v)))

(defun append% (h &rest ks-and-value)
  (let* ((ks (butlast ks-and-value))
	 (value (last@ ks-and-value)))
    (if (singleton? ks)
	(apply #'fappend% h ks-and-value)
      (when-let* ((found (apply #'get% h ks))
		  (found (append@ found value))
		  (ks-and-value (append@ ks found)))
	(apply 'set% h ks-and-value)
	found))))

(defun fappend% (h &rest ks-and-value)
  (if-let* ((value (last@ ks-and-value))
	    (ks (butlast ks-and-value))
	    (found (apply #'get% h ks))
	    (found (append@ found value))
	    (-ks-and-value (append@ ks found)))
      (apply 'set% h -ks-and-value)
    (apply 'fset% h (append@ (butlast ks-and-value)
			     (list (last@ ks-and-value)))))
  h)

(defun ->plist% (&rest hs)
  (ht->plist (apply #'merge% hs)))

(defun ->alist% (&rest hs)
  (ht->alist (apply #'merge% hs)))

(defun from-plist% (p)
  (ht-from-plist p))

(defun from-alist% (p)
  (ht-from-alist p))

(defun ->list (x &optional force)
  (if force (list x)
    (if (listp x) x (list x))))

(defun ->a-list (lst)
  (cl-loop for x from 0 below (- (length lst) 1)
	   when (= (% x 2) 0)
	   collect (list (nth x lst) (nth (+ x 1) lst))))

(defalias 'as-list '->list)
(defalias 'as-alist '->alist)
(defalias 'as-alist% '->alist%)
(defalias 'as-plist% '->plist%)
