(defpackage clover.unify
  (:use :cl
        :clover.conditions
        :clover.util
        :clover.substitute
        :clover.types)
  (:import-from :clover.parser
                :%intern-symbol-to-specified-package)
  (:import-from :clover.rename
                :rename)
  (:export 
    :subsumption-clause-p
    :alphabet=
    :find-most-general-unifier-set)
  )
(in-package :clover.unify)



(defun %collect-disagreement-set (obj1 obj2)
  (let ((result
          (unifier-set
            (remove-duplicates 
              (%%collect-disagreement-set obj1 obj2)
              :test #'unifier=)))) 
    result))



(defmethod %%collect-disagreement-set ((term1 vterm) (term2 vterm))
  (list 
    (unifier term1 term2)))


(defmethod %%collect-disagreement-set ((term1 vterm) (term2 fterm))
  (when (occurrence-check term1 term2)
    (error 
      (make-condition 'occurrence-check-error
                      :message "occurrence check error while %%collect-disagreement-set"
                      :vterm term1
                      :fterm term2)))
  (list 
    (unifier term1 term2)))


(defmethod %%collect-disagreement-set ((term1 fterm) (term2 vterm))
  (%%collect-disagreement-set term2 term1))


(defmethod %%collect-disagreement-set ((term1 fterm) (term2 fterm))
  (let* ((fsymbol1 (fterm.fsymbol term1))
         (fsymbol2 (fterm.fsymbol term2))
         (args1    (fterm.args term1))
         (args2    (fterm.args term2))
         (length1  (length args1))
         (length2  (length args2)))

    (unless (and (eq fsymbol1 fsymbol2)
                 (=  length1  length2))
      (error (make-condition 'unmatching-fterm-error
                             :message "unmatching fterm error while %%collect-disagreement-set"
                             :fterm1 term1
                             :fterm2 term2)))

    (mapcan #'%%collect-disagreement-set args1 args2)))


(defmethod %%collect-disagreement-set ((literal1 literal) (literal2 literal))
  (let* ((predicate1 (literal.predicate literal1))
         (predicate2 (literal.predicate literal2))
         (args1      (literal.args literal1))
         (args2      (literal.args literal2))
         (length1    (length args1))
         (length2    (length args2)))

    (unless (and (eq predicate1 predicate2)
                 (= length1 length2))
      (error (make-condition 'unmatching-literal-error
                             :message "unmatching literal while %%collect-disagreement-set"
                             :literal1 literal1
                             :literal2 literal2)))

    (mapcan #'%%collect-disagreement-set args1 args2)))



(defmethod %select-one-of-substitutable-unifier ((disagreement-set unifier-set))
  (let ((unifiers 
          (unifier-set.unifiers disagreement-set)))
    (find-if
      (lambda (target)
        (find-if
          (lambda (x)
            (and 
              (not (unifier= target x))
              (or
                (occurrence-check (unifier.src target) (unifier.src x))
                (occurrence-check (unifier.src target) (unifier.dst x)))))
          unifiers))
      unifiers)))


(defmethod %flatten-disagreement-set ((disagreement-set unifier-set) (unifier unifier))
  (unifier-set
    (remove-duplicates
      (mapcar 
        (lambda (target)
          (let* ((target-src (unifier.src target))
                 (target-dst (unifier.dst target))
                 (try1       (handler-case 
                                 (apply-unifier target unifier)
                               (unexpected-unifier-source (e) nil)))
                 (try2       (handler-case 
                                 (when (typep target-dst 'vterm)
                                   (apply-unifier 
                                     (unifier target-dst target-src)
                                     unifier))
                               (unexpected-unifier-source (e) nil)))
                 (result     (or try1 try2)))

            (cond 
              ((unifier= unifier target)
               target)
              ((not result)
               (error
                 (make-condition 'unexpected-unifier-source)))
              ((occurrence-check (unifier.src result) (unifier.dst result))
               (error (make-condition 'occurrence-check-error
                                      :message "occurrence check error while %flatten-disagreement-set"
                                      :vterm (unifier.src result)
                                      :fterm (unifier.dst result))))
              (t result))))
        (unifier-set.unifiers disagreement-set))
      :test #'unifier=)))


(defun %find-most-general-unifier-set (object1 object2)
  (handler-case

      (let* ((result 
               (%collect-disagreement-set object1 object2)))

        (loop :for   unifier := (%select-one-of-substitutable-unifier result)
              :while unifier
              :do    (setf result (%flatten-disagreement-set result unifier)))

        (unless (consistent-unifier-set-p result)
          (error 
            (make-condition 
              'ununifiable-error
              :message "ununifiable error because of unmathing fterm exists"
              :object1 object1
              :object2 object2)))

        result)

    (unmatching-fterm-error (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of unmathing fterm exists"
          :object1 object1
          :object2 object2)))
    (unmatching-literal-error (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of unmathing literals"
          :object1 object1
          :object2 object2)))
    (occurrence-check-error (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of occurrence check error"
          :object1 object1
          :object2 object2)))
    (unexpected-unifier-source (e)
      (error 
        (make-condition 
          'ununifiable-error
          :message "ununifiable error because of unexpected unifier source"
          :object1 object1
          :object2 object2)))))


(defmethod find-most-general-unifier-set ((literal1 literal) (literal2 literal))
  (unless (and (eq (literal.predicate literal1)
                   (literal.predicate literal2))
               (= (length (literal.args literal1))
                  (length (literal.args literal2))))
    (error 
      (make-condition 
        'ununifiable-error
        :message "ununifiable literal"
        :object1 literal1
        :object2 literal2)))
  (%find-most-general-unifier-set literal1 literal2))


(defmethod find-most-general-unifier-set ((fterm1 fterm) (fterm2 fterm))
  (unless (and (eq (fterm.fsymbol fterm1)
                   (fterm.fsymbol fterm2))
               (= (length (fterm.args fterm1))
                  (length (fterm.args fterm2))))
    (error 
      (make-condition 
        'ununifiable-error
        :message "ununifiable fterm"
        :object1 fterm1
        :object2 fterm2)))
  (%find-most-general-unifier-set fterm1 fterm2))

(defmethod find-most-general-unifier-set ((term1 term) (term2 term))
  (%find-most-general-unifier-set term1 term2))


(defun %unifier-consistent-with-p (unifier others)
  ;; unifier が others(unifier のリスト)の全要素と整合するか。
  ;; 整合 = src が一致するなら dst も一致する
  ;; (consistent-unifier-set-p の対ごとの判定条件と同一)。
  (let ((src (unifier.src unifier))
        (dst (unifier.dst unifier)))
    (every
      (lambda (target)
        (or (term/= src (unifier.src target))
            (term= dst (unifier.dst target))))
      others)))

(defun %consistent-extension-p (consistent-acc new-unifiers)
  ;; 整合済みと仮定する consistent-acc(unifier のリスト)に new-unifiers を加えても
  ;; 整合かを増分的に判定する。acc 同士は整合済みなので再検査せず、
  ;; (1) new 同士 と (2) new × acc のみを確認する。
  ;; 結果は consistent-unifier-set-p(acc ∪ new) と同値(整合条件は対称)だが、
  ;; O(|acc∪new|^2) ではなく O(|new| * (|new| + |acc|)) で済む。
  (loop :for (u . rest) :on new-unifiers
        :always (and (%unifier-consistent-with-p u rest)
                     (%unifier-consistent-with-p u consistent-acc))))

(defun %subsumption-clause-p-renamed (renamed-clause1 renamed-clause2)
  ;; subsumption-clause-p の判定本体。呼び出し側で renamed-clause1 と renamed-clause2 が
  ;; 既に standardize-apart 済み(互いに変数素)であることを前提とし、内部でのリネームを行わない。
  ;; これにより %remove-subsumption のような O(n^2) のペア走査で、各節のリネームを
  ;; ループ前に1度だけ行い、ペア毎の二重リネームを省くことができる。
  ;; subsumption の真偽はリネーム不変なので、結果は従来の subsumption-clause-p と同値。
  (when (<= (clause-length renamed-clause1)
            (clause-length renamed-clause2))
    (let ((found-isolated-literal nil)
          (mgu-list-for-each-literal nil))
      (loop
        :named exit
        :for literal1 :in (clause.literals renamed-clause1)
        :for mgu-list := 
          (loop
            :for literal2 :in (clause.literals renamed-clause2)
            :for mgu := (when  (eq (literal.negation literal1)
                                   (literal.negation literal2))
                          (handler-case
                              (find-most-general-unifier-set literal1 literal2)
                            (ununifiable-error (e) nil)))
            :if mgu
            :collect mgu)
        :do
        (cond
          ((null mgu-list)
           (setf found-isolated-literal t)
           (return-from exit) )
          (t
           (push mgu-list mgu-list-for-each-literal))))
      (unless found-isolated-literal
        ;; 各 literal1 に candidate mgu を1つ割り当てる組合せを探索する。
        ;; 全タプルを列挙してから整合性を見る(find-in-cartesian)のではなく、
        ;; 累積 unifier acc を持ち回り、割り当てるたびに部分割り当ての整合性を
        ;; 確認して、矛盾した時点でその枝を枝刈りする。
        ;; （整合的な集合の部分集合は必ず整合的なので、矛盾した部分割り当てを
        ;;   含む完全タプルは必ず矛盾する＝枝刈りで解を取りこぼさない。）
        ;; これにより同一述語の長い節で発生する m^n のデカルト積爆発を抑える。
        ;; acc は常に整合済みなので、整合性確認は新規 unifier を acc に対して照合する
        ;; 増分判定(%consistent-extension-p)で行い、毎回 acc 全体を再検査しない。
        (labels ((search-assignment (remaining-mgu-lists acc)
                   (if (null remaining-mgu-lists)
                       ;; 全 literal1 に割り当て済み。acc は整合的なので、
                       ;; 実体に適用して clause2 の部分集合かを最終検証する。
                       (clause-subset
                         (apply-unifier-set renamed-clause1 (unifier-set acc))
                         renamed-clause2)
                       ;; 次の literal1 の候補 mgu を順に試す。
                       (some
                         (lambda (mgu)
                           (let ((new-unifiers (unifier-set.unifiers mgu)))
                             (and
                               (%consistent-extension-p acc new-unifiers)
                               (search-assignment (cdr remaining-mgu-lists)
                                                  (append acc new-unifiers)))))
                         (car remaining-mgu-lists)))))
          (search-assignment mgu-list-for-each-literal nil))))))

(defmethod subsumption-clause-p ((clause1 clause) (clause2 clause))
  ;; {P(x), Q(x)}
  ;; {P(A), P(B), Q(B)}
  ;; ((unifier unifier) (unifier))
  ;; clause1 と clause2 を standardize-apart(互いに変数素化)してから判定本体へ委譲する。
  (%subsumption-clause-p-renamed (rename clause1) (rename clause2)))


(defmethod alphabet= ((term1 term) (term2 term))
  (let* ((dummy-pred
           (%intern-symbol-to-specified-package
             "DUMMYPRED"))
         (dummy-clause1 
          (clause
            (list (literal nil dummy-pred (list term1)))))
         (dummy-clause2
          (clause
            (list (literal nil dummy-pred (list term2))))))
    (and (subsumption-clause-p dummy-clause2 dummy-clause1)
         (subsumption-clause-p dummy-clause1 dummy-clause2))))

(defmethod alphabet= ((clause1 clause) (clause2 clause))
  (and (subsumption-clause-p clause2 clause1)
       (subsumption-clause-p clause1 clause2)))

(defmethod alphabet= ((clause-set1 clause-set) (clause-set2 clause-set))
  (and 
    (null (set-difference 
            (clause-set.clauses clause-set1)
            (clause-set.clauses clause-set2)
            :test #'alphabet=))
    (null (set-difference 
            (clause-set.clauses clause-set2)
            (clause-set.clauses clause-set1)
            :test #'alphabet=))))

(defun %alphabet=-for-rule-or-eq (rule1-src rule1-dst rule2-src rule2-dst)
  ;; 規則/等式 (src -> dst) のアルファ同値判定。
  ;; src と dst を1つの結合項 §(src, dst) にまとめ、その項レベル alphabet= に帰着する。
  ;; こうすると src と dst をまたぐ変数対応（自由変数・共有変数・同変数/別変数の区別）が、
  ;; 単一の変数全単射として一括かつ正確に扱われる。
  ;; § には衝突を避けるためのダミー関数記号を用い、両辺で同一の記号を使う。
  (let ((pair-symbol
          (%intern-symbol-to-specified-package "DUMMYRULEOREQPAIR")))
    (alphabet=
      (fterm pair-symbol (list rule1-src rule1-dst))
      (fterm pair-symbol (list rule2-src rule2-dst)))))


(defmethod alphabet= ((rewrite-rule1 rewrite-rule) (rewrite-rule2 rewrite-rule))
  (let* ((rule1-src (rewrite-rule.src rewrite-rule1))
         (rule1-dst (rewrite-rule.dst rewrite-rule1))
         (rule2-src (rewrite-rule.src rewrite-rule2))
         (rule2-dst (rewrite-rule.dst rewrite-rule2)))
    (%alphabet=-for-rule-or-eq rule1-src rule1-dst rule2-src rule2-dst)))

(defmethod alphabet= ((equation1 equation) (equation2 equation))
  (let* ((eq1-left (equation.left equation1))
         (eq1-right (equation.right equation1))
         (eq2-left (equation.left equation2))
         (eq2-right (equation.right equation2)))
    (and
      (eq (equation.negation equation1)
          (equation.negation equation2))
      (or
        (%alphabet=-for-rule-or-eq
          eq1-left eq1-right eq2-left eq2-right)
        (%alphabet=-for-rule-or-eq
          eq1-left eq1-right eq2-right eq2-left)))))

(defmethod alphabet= ((equation-set1 equation-set) (equation-set2 equation-set))
  (and 
    (null (set-difference 
            (equation-set.equations equation-set1)
            (equation-set.equations equation-set2)
            :test #'alphabet=))
    (null (set-difference 
            (equation-set.equations equation-set2)
            (equation-set.equations equation-set1)
            :test #'alphabet=))))
