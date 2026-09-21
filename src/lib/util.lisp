(defpackage clover.lib.util
  (:use :cl)
  (:import-from :generators
                :make-generator
                :yield
                :next
                :stop-iteration)
  (:import-from :bordeaux-threads
                :with-timeout)
  (:export
    :measuring-time
    :with-optional-timeout
    :make-deadline
    :remaining-seconds
    :call-with-budget
    :pairwise-collect-if
    :permutation
    :take  
    :remove-duplicates-by-key
    ))
(in-package :clover.lib.util)


(defun make-deadline (timeout-seconds)
  ;; prove 開始時に1回だけ作る締切(internal-time 単位)。nil なら無制限。
  (and timeout-seconds
       (+ (get-internal-real-time)
          (round (* timeout-seconds internal-time-units-per-second)))))

(defun remaining-seconds (deadline)
  ;; 締切までの残り秒数(実数)。無制限なら nil。締切超過は 0。
  (when deadline
    (max 0 (/ (- deadline (get-internal-real-time))
              internal-time-units-per-second))))

(defun call-with-budget (deadline cap thunk)
  ;; thunk を「残り時間と cap の小さい方」を上限に実行する。
  ;; - deadline も cap も nil → 無制限で素通し(通常呼び出しがここ)
  ;; - 予算が尽きていたら実行せず即タイムアウトを通知
  ;; - タイムアウト条件はここでは捕捉せず上へ伝播させ、prove の1箇所で捕捉する
  (let* ((rest (remaining-seconds deadline))
         (budget (cond ((and (null rest) (null cap)) nil)
                       ((null rest) cap)
                       ((null cap) rest)
                       (t (min rest cap)))))
    (cond
      ((null budget)   (funcall thunk))
      ((<= budget 0)   (error 'sb-ext:timeout))
      (t (with-timeout (budget) (funcall thunk))))))


(defmacro with-optional-timeout ((timeout-seconds) &body body)
  (let ((ts (gensym "TIMEOUT"))
        (thunk (gensym "THUNK")))
    `(let ((,ts ,timeout-seconds))
       (flet ((,thunk () ,@body))
         (if ,ts
             (with-timeout (,ts) (,thunk))
             (,thunk))))))

(defmacro measuring-time (&body body)
  (let ((start (gensym "START"))
        (vals  (gensym "VALS")))
    `(let* ((,start (get-internal-real-time))
            (,vals  (multiple-value-list (progn ,@body))))
       (values-list
         (cons (/ (- (get-internal-real-time) ,start)
                  internal-time-units-per-second)
               ,vals)))))

(defun pairwise-collect-if (fn lst)
  (loop :for (a . rest) :on lst
        :nconc (let ((acc nil))
                 (dolist (b rest (nreverse acc))
                   (multiple-value-bind (flag value) (funcall fn a b)
                     (when flag (push value acc)))))))
 

(defun permutation (elements)
  (make-generator ()
    (if (<= (length elements) 1)
        (yield elements)
        (handler-case
            (loop
              :with gen := (permutation (subseq elements 1))
              :for perm := (next gen)
              :do
              (loop
                :for i :from 0 :below (length elements)
                :do
                (yield 
                  (append 
                    (subseq perm 0 i)
                    (subseq elements 0 1)
                    (subseq perm i)))))
          (stop-iteration (c) 
            (declare (ignore c)) nil)))))

(defun take (n gen)
  (let (result)
    (handler-case 
        (dotimes (i n)
          (let ((value (next gen)))
            (when value
              (push value result))))
      (stop-iteration (c)
        (declare (ignore c))))
    result))



(defun remove-duplicates-by-key (items key-fn eq-fn)
  "(remove-duplicates items :test eq-fn) と同一の結果(原順序・後優先)を返す。
   key-fn で各要素をバケツ分けし、同一キーのバケツ内だけ eq-fn で比較する。
   前提: eq-fn が真なら key-fn の結果が equal(=同一バケツ)であること(over-approximation)。
   この前提の下で、結果は従来の remove-duplicates と完全に一致しつつ比較回数を削減する。"
;;;; --- 正準キーによる重複排除の高速化 -------------------------------------
;;;; remove-duplicates + alphabet-equivalent-p は O(m^2) 比較で、alphabet-equivalent-p 内部の
;;;; subsumption が都度 rename を呼ぶため非常に重い(プロファイル上の支配項の一つ)。各要素を
;;;; 「変数リネーム不変」な正準キー文字列(本モジュールの canonical-*-key)でバケツ分けし、
;;;; 同一バケツ内のみ eq-fn で確認することで、比較回数を O(m^2) から O(Σ bucket^2) に削減する。 
  (let* ((keyed (mapcar (lambda (x) (cons (funcall key-fn x) x)) items))
         (buckets (make-hash-table :test #'equal)))
    (loop :for kc :in keyed :for i :from 0
          :do (push (cons i (cdr kc)) (gethash (car kc) buckets)))
    (loop :for kc :in keyed :for i :from 0
          :unless (some (lambda (p)
                          (and (> (car p) i)
                               (funcall eq-fn (cdr kc) (cdr p))))
                        (gethash (car kc) buckets))
          :collect (cdr kc)))) 
