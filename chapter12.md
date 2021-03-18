# Глава 12
## Компиляция Логических Программ

В конце [главы 11](B978008057115750011X.xhtml) было представлено новое, более эффективное представление для логических переменных.
Было бы разумно построить новую версию интерпретатора Пролога, включающую это представление.
Однако [глава 9](B9780080571157500091.xhtml) научила нас, что компиляторы работают быстрее, чем интерпретаторы, и их не так уж сложно построить.
Таким образом, в этой главе будет представлен компилятор Пролога, который преобразует Пролог в Лисп.

Каждый предикат Пролога будет преобразован в функцию Лиспа, и мы примем соглашение, согласно которому предикат, вызываемый с другим числом аргументов, является другим(отличающимся от предыдущего) предикатом.
Если символ `p` может быть вызван с одним или двумя аргументами, нам понадобятся две функции Лиспа для реализации двух предикатов.
Следуя традиции Пролога, они будут называться `p/1` и `p/2`.

Следующий шаг - решить, как должен выглядеть сгенерированный код Lisp.
Он должен объединить/унифицировать заголовок каждого предложения напротив аргументов, и, если объединение/унификация завершится успешно, он должен вызвать предикаты в теле.
Трудность состоит в том, что нужно помнить о точках выбора.
Если вызов предиката в первом предложении завершился неудачно, мы должны иметь возможность вернуться ко второму предложению и повторить попытку.

Это можно сделать, передав *успешное продолжение* в качестве дополнительного аргумента каждому предикату.
Это продолжение представляет собой цели, которые остаются нерешенными, аргумент функции `prove` `other-goals`(другие цели).
Для каждого предложения в предикате, если все цели в предложении выполнены, мы должны вызвать успешное продолжение.
Если цель не удается/не достигается, мы не делаем ничего особенного; мы просто переходим к следующему предложению.
Есть одно осложнение: после неудачи мы должны отменить все привязки, сделанные `unify!`.
Рассмотрим пример.
Предложения

```lisp
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
```

можно скомпилировать в это:

```lisp
(defun likes/2 (?arg1 ?arg2 cont)
  ;; First clause:
```

`  (if (and (unify!
?arg1 'Robin) (unify!
?arg2 'cats))`

```lisp
      (funcall cont))
  (undo-bindings)
  ;; Second clause:
```

`  (if (unify!
?argl 'Sandy)`

```lisp
      (likes/2 ?arg2 'cats cont))
  (undo-bindings)
  ;; Third clause:
```

`  (if (unify!
?argl 'Kim)`

```lisp
      (likes/2 ?arg2 'Lee
          #'(lambda () (likes/2 ?arg2 'Kim cont))))))
```

В первом предложении мы просто проверяем два аргумента и, если унификации успешны, вызываем продолжение напрямую, потому что первое предложение не имеет тела.
Во втором предложении рекурсивно вызывается `likes/2`, чтобы увидеть, нравятся ли `?arg2` кошки(`cats`).
Если это удалось, то исходная цель достигнута, и вызывается продолжение `cont`.
В третьем предложении мы должны снова рекурсивно вызвать `likes/2`, на этот раз попросив проверить, нравится ли `?arg2` `Lee`.
Если эта проверка прошла успешно, будет вызвано продолжение.
В этом случае продолжение включает в себя еще один вызов функции `likes/2`, чтобы проверить, нравится ли `?аrg2` `Ким`.
Если это удастся, то, наконец, будет вызвано исходное продолжение, `cont`.

Напомним, что в интерпретаторе Пролога мы должны были добавить список ожидающих целей, `other-goals`, к целям в теле предложения.
В компиляторе нет необходимости выполнять `append`. Вместо этого продолжение cont представляет `other-goals`(другие цели), а тело предложения представлено явными вызовами функций.

Обратите внимание, что приведенный ранее код для `likes/2` устраняет некоторые ненужные вызовы `unify!`.
Самая очевидная реализация будет иметь один вызов `unify!` для каждого аргумента.
Таким образом, для второго предложения у нас будет код:

`(if (and (unify!
?argl 'Sandy) (unify!
?arg2 ?x))`

```lisp
  (likes/2 ?x 'cats cont))
```

где нам понадобится подходящая привязка let для переменной `?x`.

## 12.1 Компилятор Пролога

В этом разделе представлен компилятор, показанный на [рис. 12.1](#f0010).
На верхнем уровне находится функция `prolog-compile`, которая принимает символ, просматривает предложения, определенные для этого символа, и группирует их по арности(количеству аргументов).
Каждый символ/арность компилируется в отдельную функцию Лиспа с помощью `compile-predicate`.

| []()                                          |
|-----------------------------------------------|
| ![f12-01](images/chapter12/f12-01.jpg)        |
| Рисунок 12.1: Глоссарий для компилятора Prolog |

(ed: this should be a markdown table)

```lisp
(defun prolog-compile (symbol &optional
                       (clauses (get-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate
        symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all the clauses with any other arity
      (prolog-compile
        symbol (clauses-with-arity clauses #'/= arity)))))
```

Сюда включены три служебные функции:

```lisp
(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (find-all arity clauses
            :key #'(lambda (clause)
                     (relation-arity (clause-head clause)))
            :test test))

(defun relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun args (x) "The arguments of a relation" (rest x))
```

Следующим шагом является компиляция предложений для данного предиката с фиксированной арностью в функцию Lisp.
На данный момент это будет сделано путем независимой компиляции каждого предложения и помещения их в лямбда с правильным списком параметров.

```lisp
(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((predicate (make-predicate symbol arity))
        (parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,predicate (,@parameters cont)
            .,(mapcar #'(lambda (clause)
                        (compile-clause parameters clause 'cont))
              clauses))))))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
        collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (symbol symbol '/ arity))
```

Теперь самое сложное: мы должны фактически сгенерировать код для предложения.
Вот снова пример кода, желаемого для одного предложения.
Начнем с установки в качестве цели простого кода:

```lisp
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(defun likes/2 (?arg1 ?arg2 cont)
  ...
```

`  (if (and (unify!
?argl 'Kim) (unify!
?arg2 ?x)`

```lisp
      (likes/2 ?arg2 'Lee
            #'(lambda () (likes/2 ?x 'Kim))))
```

  ...)

но мы также рассмотрим возможность обновления до улучшенного кода:

```lisp
(defun likes/2 (?arg1 ?arg2 cont)
  ...
```

`  (if (unify!
?arg1 'Kim)`

      `(likes/2 ?arg2 'Lee`

            `#'(lambda () (likes/2 ?arg2 'Kim))))`

  ...)

Один из подходов - написать две функции, `compile-head` и `compile-body`, а затем объединить их в код (if *head body*)
Такой подход может легко сгенерировать предыдущий код.
Однако позволим себе немного забежать вперед.
Если мы в конечном итоге захотим сгенерировать улучшенный код, нам понадобится некоторая связь между головой(head) и телом(body).
Нам нужно знать, что голова решила не компилировать объединение/унификацию `?arg2` и `?x`, но из-за этого тело должно будет заменить `?arg2` на `?x`.
Это означает, что функция `compile-head` концептуально возвращает два значения: код для головы(head) и указание замен, которые необходимо выполнить в теле(body).
С этим можно справиться, явно манипулируя мноожественными значениями, но это кажется сложным.

Альтернативный подход - исключить `compile-head` и просто написать `compile-body`.
Это возможно, если мы действительно выполним преобразование исходного кода в предложении.
Вместо того, чтобы трактовать это предложение как:

```lisp
(<- (likes Kim ?x)
  (likes ?x Lee) (likes ?x Kim))
```

преобразуем его в эквивалент:

```lisp
(<- (likes ?arg1 ?arg2)
  (= ?arg1 Kim) (= ?arg2 ?x) (likes ?x Lee) (likes ?x Kim))
```

Теперь аргументы в заголовке(head) предложения соответствуют аргументам в функции `likes/2`, поэтому нет необходимости создавать какой-либо код для заголовка(head).
Это упрощает задачу за счет исключения `compile-head`, и это лучшая декомпозиция по другой причине: вместо добавления оптимизаций в `compile-head` мы добавим их в код в `compile-body`, который обрабатывает знак равно =.
Таким образом, мы можем оптимизировать вызовы, которые пользователь делает к =, в дополнение к вызовам, вводимым преобразованием исходного кода.

Для ознакомления, последовательность вызовов функций окажется следующей:

```lisp
prolog-compile
  compile-predicate
    compile-clause
      compile-body
        compile-call
        compile-arg
        compile-unify
            compile-arg
```

где каждая функция вызывает те, что ниже, с отступом на один уровень.
Мы уже определили первые две функции.
Вот наша первая версия `compile-clause`:

```lisp
(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (compile-body
    (nconc
      (mapcar #'make-= parms (args (clause-head clause)))
      (clause-body clause))
    cont))

(defun make-= (x y) `(= ,x ,y))
```

Основная часть работы выполняется в `compile-body`, который немного сложнее.
Есть три случая.
Если тела нет, мы просто вызываем продолжение.
Если тело начинается с вызова =, мы компилируем вызов `unify!`.
В противном случае мы компилируем вызов функции, передавая соответствующее продолжение.

Однако на этом этапе стоит подумать о будущем.
Если мы хотим трактовать сейчас = специально, мы, вероятно, захотим позже обработать другие цели специально.
Поэтому вместо явной проверки для = мы будем выполнять управляемую данными диспетчеризацию, ища любой предикат, к которому привязано свойство `prolog-compiler-macro`.
Как и макросы компилятора Лиспа, макрос может не справиться с поставленной задачей.
Мы примем соглашение, согласно которому возврат `:pass` означает, что макрос решил не обрабатывать это и следовательно, он должен быть скомпилирован как обычная цель.

```lisp
(defun compile-body (body cont)
  "Compile the body of a clause."
  (if (null body)
      `(funcall ,cont)
      (let* ((goal (first body))
             (macro (prolog-compiler-macro (predicate goal)))
             (macro-val (if macro
                            (funcall macro goal (rest body) cont))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            (compile-call
               (make-predicate (predicate goal)
                               (relation-arity goal))
               (mapcar #'(lambda (arg) (compile-arg arg))
                       (args goal))
               (if (null (rest body))
                   cont
                   `#'(lambda ()
                      ,(compile-body (rest body) cont))))))))

(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         #'(lambda ,arglist .,body)))

(def-prolog-compiler-macro = (goal body cont)
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass
        `(if ,(compile-unify (first args) (second args))
             ,(compile-body body cont)))))

(defun compile-unify (x y)
  "Return code that tests if var and term unify."
  `(unify! ,(compile-arg x) ,(compile-arg y)))
```

Все, что остается, это `compile-arg`, функция для компиляции аргументов в цели для тела предложения.
Следует рассмотреть три случая, как показано ниже при компиляции аргумента `q`:

| []()                         |                              |
|------------------------------|------------------------------|
| `1 (<- (p ?x) (q ?x))`       | `(q/1 ?x cont)`              |
| `2 (<- (p ?x) (q (f a b)))`  | `(q/1 '(f a b) cont)`        |
| `3 (<- (p ?x) (q (f ?x b)))` | `(q/1 (list 'f ?x 'b) cont)` |

В случае 1 аргументом является переменная, и она компилируется как есть.
В случае 2 аргумент - это постоянное выражение (без переменных), которое компилируется в цитируемое(quote) выражение.
В случае 3 аргумент содержит переменную, поэтому мы должны сгенерировать код, который строит выражение.
Случай 3 фактически разделен на две части в списке ниже: одна компилируется в вызов `list`, а другая - в вызов `cons`.
Важно помнить, что цель `(q (f ?x b))` *не* включает вызов функции `f`.
Скорее, она включает термин `(f ?x b)`, который представляет собой просто список из трех элементов.

```lisp
(defun compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((variable-p arg) arg)
        ((not (has-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'compile-arg arg)))
        (t `(cons ,(compile-arg (first arg))
                  ,(compile-arg (rest arg))))))

(defun has-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))
```

Посмотрим, как это работает.
Мы рассмотрим следующие пункты:

```lisp
(<- (likes Robin cats))
(<- (likes Sandy ?x) (likes ?x cats))
(<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))
```

Вот что дает нам `prolog-compile`:

```lisp
(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)
```

`  (IF (UNIFY!
?ARG1 'ROBIN)`

`    (IF (UNIFY!
?ARG2 'CATS)`

```lisp
      (FUNCALL CONT)))
```

`  (IF (UNIFY!
?ARG1 'SANDY)`

`    (IF (UNIFY!
?ARG2 ?X)`

```lisp
      (LIKES/2 ?X 'CATS CONT)))
```

`  (IF (UNIFY!
?ARG1 'KIM)`

`    (IF (UNIFY!
?ARG2 ?X)`

```lisp
      (LIKES/2 ?X 'LEE (LAMBDA ()
            (LIKES/2 ?X 'KIM CONT))))))
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
```

`  (IF (UNIFY!
?ARG1 ?ITEM)`

`    (IF (UNIFY!
?ARG2 (CONS ?ITEM ?REST))`

```lisp
      (FUNCALL CONT)))
```

`  (IF (UNIFY!
?ARG1 ?ITEM)`

`    (IF (UNIFY!
?ARG2 (CONS ?X ?REST))`

```lisp
      (MEMBER/2 ?ITEM ?REST CONT))))
```

## 12.2 Исправление ошибок в компиляторе

В этой версии компилятора есть некоторые проблемы:

*   Мы забыли отменить привязки после каждого вызова `unify!`.

*   Определение `undo-bindings!` определенное ранее требует в качестве аргумента индекса в массиве `*trail*`.
Таким образом, нам нужно будет сохранять текущую вершину trail при входе в каждую функцию.

*   Локальные переменные, такие как `?x`, использовались без введения.
Они должны быть привязаны к новым переменным.

Отменить привязку просто: мы добавляем одну строку в `compile-predicate`, вызов функции `might-add-undo-bindings`. Эта функция вставляет вызов `undo-bindings!` после каждой неудачи(failure).
Если есть только одно предложение, отмены не требуется, потому что предикат, расположенный выше в вызывающей последовательности, сделает это в случае неудачи(failure).
Если есть несколько предложений, функция оборачивает все тело функции в let, которая фиксирует начальное значение указателя заполнения trail, так что привязки могут быть отменены в нужной точке.
Точно так же мы можем решить проблему несвязанных переменных, заключив вызов `bind-unbound-vars` вокруг каждого скомпилированного предложения:

```lisp
(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((predicate (make-predicate symbol arity))
        (parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,predicate (,@parameters cont)
  .,(maybe-add-undo-bindings                  ;***
     (mapcar #'(lambda (clause)
           (compile-clause parameters clause 'cont))
      clauses)))))))

(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars                                   ;***
    parms                                              ;***
    (compile-body
      (nconc
        (mapcar #'make-= parms (args (clause-head clause)))
        (clause-body clause))
      cont)))

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect '(undo-bindings! old-trail)
                  collect exp)))))

(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (set-difference (variables-in exp)
                                  parameters)))
    (if exp-vars
        `(let ,(mapcar #'(lambda (var) `(,var (?)))
                       exp-vars)
           ,exp)
        exp)))
```

С этими улучшениями мы получили код для `likes` и `member`:

```lisp
(DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)
  (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
```

`    (IF (UNIFY!
?ARG1 'ROBIN)`

`      (IF (UNIFY!
?ARG2 'CATS)`

```lisp
            (FUNCALL CONT)))
```

`    (UNDO-BINDINGS!
OLD-TRAIL)`

```lisp
    (LET ((?X (?)))
```

`      (IF (UNIFY!
?ARG1 'SANDY)`

`        (IF (UNIFY!
?ARG2 ?X)`

```lisp
            (LIKES/2 ?X 'CATS CONT))))
```

`    (UNDO-BINDINGS!
OLD-TRAIL)`

```lisp
    (LET ((?X (?)))
```

`      (IF (UNIFY!
?ARG1 'KIM)`

`        (IF (UNIFY!
?ARG2 ?X)`

```lisp
            (LIKES/2 ?X 'LEE (LAMBDA ()
                    (LIKES/2 ?X 'KIM CONT))))))))
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
  (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
    (LET ((?ITEM (?))
            (?REST (?)))
```

`      (IF (UNIFY!
?ARG1 ?ITEM)`

`            (IF (UNIFY!
?ARG2 (CONS ?ITEM ?REST))`

```lisp
                        (FUNCALL CONT))))
```

`    (UNDO-BINDINGS!
OLD-TRAIL)`

```lisp
    (LET ((?X (?))
```

`            (?
ITEM (?))`

```lisp
            (?REST (?)))
```

`    (IF (UNIFY!
?ARG1 ?ITEM)`

`      (IF (UNIFY!
?ARG2 (CONS ?X ?REST))`

```lisp
                        (MEMBER/2 ?ITEM ?REST CONT))))))
```

## 12.3 Улучшение компилятора

Это неплохо, хотя есть еще возможности для улучшения.
Одно небольшое улучшение - устранение ненужных переменных.
Например, `?rest` в первом предложении `member` и `?x` во втором предложении привязаны к новым переменным - результату вызова (?) - и затем используются только один раз.
Сгенерированный код можно было бы немного упростить, просто поместив вызов (?) в нужную позицию(inline), а не связывая его результат с переменной и затем ссылаясь на эту переменную.
Это изменение состоит из двух частей: обновление `compile-arg` для компиляции анонимной переменной встроенной(inline) и изменение макроса <- так, чтобы он преобразовывал все переменные, которые появляются только один раз в предложении, в анонимные переменные:

```lisp
(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(make-anonymous clause)))

(defun compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((variable-p arg) arg)
        ((not (has-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'compile-arg arg)))
        (t `(cons ,(compile-arg (first arg))
                  ,(compile-arg (rest arg))))))

(defun make-anonymous (exp &optional
                       (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '?)
        (t exp)))
```

Найти анонимные переменные непросто.
Следующая функция хранит два списка: переменные, которые были обнаружены один раз, и переменные, которые были обнаружены дважды или более.
Затем для обхода дерева используется локальная функция `walk`, рекурсивно рассматривая компоненты каждой cons-ячейки и обновляя два списка при обнаружении каждой переменной.
Следует помнить об использовании локальных функций, а также об альтернативе, обсуждаемой в [упражнении 12.23](#p4625) на [стр. 428](#p428).

```lisp
(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (let ((seen-once nil)
            (seen-more nil))
    (labels ((walk (x)
            (cond
                ((variable-p x)
                    (cond ((member x seen-once)
                              (setf seen-once (delete x seen-once))
                              (push x seen-more))
                        ((member x seen-more) nil)
                        (t (push x seen-once))))
                ((consp x)
                    (walk (first x))
                    (walk (rest x))))))
      (walk tree)
      seen-once)))
```

Теперь `member` компилируется в это:

```lisp
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
```

  `(LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))`

    `(LET ((?ITEM (?)))`

      `(IF (UNIFY!
?ARG1 ?ITEM)`

        `(IF (UNIFY!
?ARG2 (CONS ?ITEM (?)))`

                `(FUNCALL CONT))))`

    `(UNDO-BINDINGS!
OLD-TRAIL)`

```lisp
    (LET ((?ITEM (?))
        (?REST (?)))
```

`      (IF (UNIFY!
?ARG1 ?ITEM)`

`        (IF (UNIFY!
?ARG2 (CONS (?) ?REST))`

```lisp
            (MEMBER/2 ?ITEM ?REST CONT))))))
```

## 12.4 Улучшение компиляции унификации

Теперь перейдем к усовершенствованию `compile-unify`.
Напомним, что мы хотим исключить определенные вызовы `unify!`, Чтобы, например, первое предложение `member:`

```lisp
(<- (member ?item (?item . ?rest)))
```

компилируется в:

```lisp
(LET ((?ITEM (?)))
```

`  (IF (UNIFY!
?ARG1 ?ITEM)`

`    (IF (UNIFY!
?ARG2 (CONS ?ITEM (?)))`

```lisp
        (FUNCALL CONT))))
```

когда оно могло бы скомпилироваться в более эффективный:

`(IF (UNIFY!
?ARG2 (CONS ?ARG1 (?)))`

```lisp
    (FUNCALL CONT))
```

Устранение объединения/унификации одной цели в дальнейшем отразится и на других целях, поэтому нам нужно будет отслеживать выражения, которые были объединены/унифицированы вместе.
У нас есть выбор дизайна.
Либо `compile-unify` может изменять глобальную переменную состояния, либо возвращать несколько значений.
На основании того, что глобальные переменные беспорядочны, мы делаем второй выбор: `compile-unify` принимает список привязок в качестве дополнительного аргумента и возвращает два значения: фактический код и обновленный список привязок.
Мы ожидаем, что для работы с этими множественными значениями придется изменить другие связанные функции.

Когда в нашем примере сначала вызывается `compile-unify`, её просят объединить/унифицировать `?argl` и `?item`.
Мы хотим, чтобы она возвращала не код (точнее, тривиально верный тест t).
Для второго значения она должна вернуть новый список привязок с `?item`, привязанным к `?arg1`. Эта привязка будет использоваться для замены `?item` на `?arg1` в последующем коде.

Как мы узнаем, что нужно привязать `?item` к `?arg1`, а не наоборот?
Поскольку `?arg1` уже привязан к чему-то - значению, переданному в `member`. Мы не знаем, что это за значение, но мы не можем его игнорировать.
Таким образом, в исходном списке привязки должно быть указано, что параметры к чему-то привязаны.
Простое соглашение - привязать параметры к самим себе.
Таким образом, исходный список привязки будет:

```lisp
((?arg1 .?arg1) (?arg2 . ?arg2))
```

В предыдущей главе ([страница 354](B978008057115750011X.xhtml#p354)) мы видели, что привязка переменной к самой себе может привести к проблемам; нам придется быть осторожными.

Помимо устранения объединения/унификации новых переменных с параметрами, можно сделать еще несколько улучшений.
Например, унификации, включающие только константы, могут быть выполнены во время компиляции.
Вызов `(= (f a) (f a))` всегда успешен, а `(= 3 4)` всегда терпит неудачу.
Кроме того, объединение/унификация двух cons-ячеек может быть разбита на компоненты во время компиляции: `(= (f ?x) (f a))` сокращается до `(= ?x a)` и `(= f f)`, где последний тривиально успешен(истинен).
Мы даже можем выполнить проверку некоторых событий во время компиляции: `(= ?x (f ?x))` должно завершиться неудачей(fail).

В следующей таблице перечислены эти улучшения вместе с разбивкой по случаям объединения/унификации связанной переменной `(?arg1)` или несвязанной переменной `(?x)` с другим выражением.
Первый столбец - это вызов унификации, второй - сгенерированный код, а третий - привязки, которые будут добавлены в результате вызова:

|      | Unification         | Code                    | Bindings            |
|------|---------------------|-------------------------|---------------------|
| 1    | `(= 3 3)`           | `t`                     | `-`                 |
| 2    | `(= 3 4)`           | `nil`                   | `-`                 |
| 3    | `(= (f ?x) (?p 3))` | `t`                     | `(?x . 3) (?p . f)` |
| 4    | `(= ?arg1 ?y)`      | `t`                     | `(?y . ?arg1)`      |
| 5    | `(= ?arg1 ?arg2)`   | `(unify! ?arg1 ?arg2)`  | `(?arg1 . ?arg2)`   |
| 6    | `(= ?arg1 3)`       | `(unify! ?arg1 3)`      | `(?arg1 . 3)`       |
| 7    | `(= ?arg1 (f ? y))` | `(unify! ?arg1 . . . )` | `(?y . ?y)`         |
| 8    | `(= ?x ?y)`         | `t`                     | `(?y . ?y)`         |
| 9    | `(= ?x 3)`          | `t`                     | `(?x . 3)`          |
| 10   | `(= ?x (f ? y))`    | `(unify! ?x . . . )`    | `(?y . ?y)`         |
| 11   | `(= ?x (f ? x))`    | `nil`                   | `-`                 |
| 12   | `(= ?x ?)`          | `t`                     | `-`                 |

Из этой таблицы мы можем создать нашу новую версию `compile-unify`.
Первая часть довольно проста.
Она заботится о первых трех случаях в этой таблице и обеспечивает вызов `compile-unify-variable` с переменной в качестве первого аргумента для остальных случаев.

```lisp
(defun compile-unify (x y bindings)
  "Return 2 values: code to test if x and y unify,
  and a new binding list."
  (cond
    ;; Unify constants and conses:                       ; Case
    ((not (or (has-variable-p x) (has-variable-p y)))    ; 1,2
     (values (equal x y) bindings))
    ((and (consp x) (consp y))                           ; 3
     (multiple-value-bind (code1 bindings1)
         (compile-unify (first x) (first y) bindings)
       (multiple-value-bind (code2 bindings2)
           (compile-unify (rest x) (rest y) bindings1)
         (values (compile-if code1 code2) bindings2))))
    ;; Here x or y is a variable.  Pick the right one:
    ((variable-p x) (compile-unify-variable x y bindings))
    (t              (compile-unify-variable y x bindings))))

(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))
```

Следующая функция `compile-unify-variable` - одна из самых сложных, которые мы видели.
Для каждого аргумента мы смотрим, есть ли у него привязка (локальные переменные `xb` и` yb`), а затем используем привязки, чтобы получить значение каждого аргумента (`x1` и `y1`).
Обратите внимание, что либо для несвязанной переменной, либо для переменной, привязанной к самой себе, `x` будет равняться `x1` (и то же самое для `y` и `y1`).
Если любая из пар значений не эквивалентна, мы должны использовать новые (`x1` или `y1`), и предложение с комментарием deref делает это.
После этого мы просто рассмотрим случаи/варианты по одному.
Оказалось, что было проще немного изменить порядок, чем в предыдущей таблице, но каждое предложение снабжено соответствующим номером:

```lisp
(defun compile-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-unify x1 y1 bindings))
      ((find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(unify! ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(unify! ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings)))))) ; 8,9
```

Найдите время, чтобы понять, как работает эта функция.
Затем переходите к следующим вспомогательным функциям:

```lisp
(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)

(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))
```

Теперь нам нужно интегрировать новый `compile-unify` в остальную часть компилятора.
Проблема в том, что новая версия принимает дополнительный аргумент и возвращает дополнительное значение, поэтому все вызывающие его функции должны быть изменены.
Давайте еще раз посмотрим на последовательность вызовов:

```lisp
prolog-compile
  compile-predicate
    compile-clause
      compile-body
        compile-call
        compile-arg
          compile-unify
            compile-arg
```

Во-первых, спускаясь вниз, мы видим, что `compile-arg` должна принимать список привязок в качестве аргумента, чтобы она могла искать и подставлять соответствующие значения.
Но она не изменяет список привязок, поэтому она все равно возвращает одно значение:

```lisp
(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
             (compile-arg (binding-val binding) bindings)
             arg)))
        ((not (find-if-anywhere #'variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'(lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))
```

Теперь, поднимаясь вверх, `compile-body` надо взять список привязок и передать его различным функциям:

```lisp
(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)                              ;***
     `(progn ,(compile-body (rest body) cont bindings) ;***
             (return-from ,*predicate* nil)))          ;***
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro
                             (funcall macro goal (rest body)
                                      cont bindings))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            `(,(make-predicate (predicate goal)
                               (relation-arity goal))
              ,@(mapcar #'(lambda (arg)
                            (compile-arg arg bindings))
                        (args goal))
              ,(if (null (rest body))
                   cont
                   `#'(lambda ()
                        ,(compile-body
                           (rest body) cont
                           (bind-new-variables bindings goal))))))))))
```

Функция `bind-new-variables` принимает любые переменные, упомянутые в цели, которые еще не были связаны, и связывает эти переменные с собой.
Это потому, что цель, какой бы она ни была, может связывать ее аргументы.

```lisp
(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if #'(lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))

(defun self-cons (x) (cons x x))
```

Одна из функций, которую необходимо изменить, чтобы принять список привязки, - это макрос компилятора для =:

```lisp
(def-prolog-compiler-macro = (goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
            (compile-unify (first args) (second args) bindings)
          (compile-if
            code1
            (compile-body body cont bindings1))))))
```

Последний шаг вверх - изменить `compile-clause` так, чтобы она запускала все, передавая `compile-body` список привязки со всеми параметрами, привязанными к себе:

```lisp
(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars
    parms
    (compile-body
      (nconc
        (mapcar #'make-= parms (args (clause-head clause)))
        (clause-body clause))
      cont
      (mapcar #'self-cons parms))))                    ;***
```

Наконец, мы видим плоды наших усилий:

```lisp
(DEFUN MEMBER/2 (?ARG1 ?ARG2 CONT)
  (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
```

`    (IF (UNIFY!
?ARG2 (CONS ?ARG1 (?)))`

```lisp
            (FUNCALL CONT))
```

`    (UNDO-BINDINGS!
OLD-TRAIL)`

```lisp
    (LET ((?REST (?)))
```

`        (IF (UNIFY!
?ARG2 (CONS (?) ?REST))`

```lisp
                (MEMBER/2 ?ARG1 ?REST CONT)))))
  (DEFUN LIKES/2 (?ARG1 ?ARG2 CONT)
    (LET ((OLD-TRAIL (FILL-POINTER *TRAIL*)))
```

`        (IF (UNIFY!
?ARG1 'ROBIN)`

`                (IF (UNIFY!
?ARG2 'CATS)`

```lisp
                    (FUNCALL CONT)))
```

`        (UNDO-BINDINGS!
OLD-TRAIL)`

`        (IF (UNIFY!
?ARG1 'SANDY)`

```lisp
            (LIKES/2 ?ARG2 'CATS CONT))
```

`        (UNDO-BINDINGS!
OLD-TRAIL)`

`        (IF (UNIFY!
?ARG1 'KIM)`

```lisp
            (LIKES/2 ?ARG2 'LEE (LAMBDA ()
                        (LIKES/2 ?ARG2 'KIM CONT))))))
```

## 12.5 Дальнейшие улучшения унификации

Можно ли еще раз улучшить `compile-unify`?
Если мы будем настаивать на том, чтобы он вызывал `unify!`, кажется, что его нельзя сделать намного лучше.
Однако мы могли бы улучшить его, фактически скомпилировав `unify!`. Это ключевая идея в абстрактной машине Уоррена, или WAM(Warren Abstract Machine), которая является наиболее часто используемой моделью для компиляторов Пролога.

Мы вызываем `unify!` в четырех случаях (5, 6, 7 и 10), и в каждом случае первый аргумент является переменной, и мы кое-что знаем о втором аргументе.
Но первое, что делает `unify!` - это избыточно проверяет, является ли первый аргумент переменной.
Мы могли бы исключить ненужные тесты, вызвав более специализированные функции, а не универсальную функцию `unify!`.
Рассмотрим этот вызов:

`(unify!
?arg2 (cons ?arg1 (?)))`

Если `?arg2` является несвязанной переменной, этот код подходит.
Но если `?arg2` - постоянный атом, мы должны немедленно выйти с ошибкой(fail), не позволяя `cons` и `?` генерировать мусор.
Мы могли бы изменить тест на:

```lisp
(and (consp-or-variable-p ?arg2)
```

`    (unify-first!
?arg2 ?arg1)`

`    (unify-rest!
?arg2 (?)))`

с подходящими определениями упомянутых здесь функций.
Это изменение должно ускорить время выполнения и ограничить количество генерируемого мусора.
Конечно, это удлиняет сгенерированный код, так что это может замедлить работу, если программа в конечном итоге будет тратить слишком много времени на передачу кода процессору.

** Упражнение 12.1 [h] ** Напишите определения для `consp-or-variable-p, unify-first!` и `unify-rest!`, и измените компилятор для генерации кода, подобного описанному ранее.
Вы можете посмотреть функцию `compile-rule` в [раздел 9.6](B9780080571157500091.xhtml#s0035), начиная с [страницы 300] (B9780080571157500091.xhtml#p300).
Эта функция скомпилировала вызов `pat-match` в отдельные тесты; теперь мы хотим сделать то же самое для `unify!`.
Выполните несколько тестов, чтобы сравнить измененный компилятор с исходной версией.

**Exercise  12.2 [h]** We can gain some more efficiency by keeping track of which variables have been dereferenced and calling an appropriate unification function: either one that dereferences the argument or one that assumes the argument has already been dereferenced.
Implement this approach.

**Упражнение 12.3 [m]** Какой код создается для `(= (f (g ?x) ?y) (f ?y (?p a)))?` Какой более эффективный код представляет ту же унификацию?
Насколько легко изменить компилятор, чтобы получить более эффективный результат?

**Упражнение 12.4 [h]** Оглядываясь назад, кажется, что привязка переменных к самим себе, как в `(?argl . ?argl`), была не такой уж хорошей идеей.
Это усложняет значение привязок и запрещает нам использовать существующие инструменты.
Например, для случая 11 мне пришлось использовать `find-anywhere` вместо `occur-check`, потому что `occur-check` ожидает незацикленный список привязок.
Но функция find-anywhere не так хорошо выполняет свою работу, как `occur-check`.
Напишите версию `compile-unify`, которая возвращает три значения: код, незацикленный список привязки и список переменных, которые привязаны к неизвестным значениям.

**Упражнение 12.5 [h]** Альтернативой предыдущему упражнению является отказ от использования списков привязки во всех случаях.
Вместо этого мы могли бы передать список классов эквивалентности, то есть список списков, где каждый подсписок содержит один или несколько элементов, которые были объединены/унифицированы.
При таком подходе начальный список классов эквивалентности будет `((?arg1) (?arg2))`.
После объединения/унификации `?arg1` с `?x`, `?arg2` с `?arg2` и `?x` с 4, список будет ( `(4 ?arg1 ?x) (?arg2 ?y))`.
Это предполагает соглашение о том, что канонический член класса эквивалентности (тот, который будет заменен всеми остальными) идет первым.
Реализуйте этот подход.
Какие у него преимущества и недостатки?

## 12.6 Пользовательский интерфейс для компилятора

Компилятор может преобразовывать Пролог в Лисп, но это не принесет нам пользы, если мы не сможем удобно организовать компиляцию правильных отношений Пролога и вызвать правильные функции Лиспа.
Другими словами, мы должны интегрировать компилятор с макросами `<-` и `?`.
Удивительно, но нам вообще не нужно менять эти макросы.
Вместо этого мы изменим функции, которые вызывают эти макросы.
Когда вводится новое предложение, мы вводим предикат предложения в список `*uncompiled*`.
Это однострочное дополнение к `add-clause:`

```lisp
(defvar *uncompiled* nil
  "Prolog symbols that have not been compiled.")

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)                          ;***
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))
```

Теперь, когда делается запрос, макрос `?-` заменяется вызовом `top-level-proof`. Список целей в запросе вместе с целью `show-prolog-vars` добавляется в качестве единственного предложения. для отношения `top-level-query`. Затем этот запрос, вместе с любыми другими, которые находятся в некомпилированном списке, компилируется.
Наконец, вызывается вновь скомпилированная функция запроса верхнего уровня.

```lisp
(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (show-prolog-vars ,(mapcar #'symbol-name vars)
                                    ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 #'ignore)
  (format t "~&No.")
  (values))

(defun run-prolog (procedure cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (setf (fill-pointer *trail*) 0)
  (setf *var-counter* 0)
  ;; Finally, call the query
  (catch 'top-level-prove
    (funcall procedure cont)))

(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (mapc #'prolog-compile symbols)
  (setf *uncompiled* (set-difference *uncompiled* symbols)))

(defun ignore (&rest args)
  (declare (ignore args))
  nil)
```

Обратите внимание, что на верхнем уровне нам не нужно продолжение, чтобы что-то делать.
Произвольно мы решили передать функцию `ignore`, которая определена так, чтобы игнорировать свои аргументы.
Эта функция полезна во многих местах; некоторые программисты объявляют её встроенной и затем используют вызов `ignore` вместо объявления(декларации) ignore:

```lisp
(defun third-arg (x y z)
  (ignore x y)
  z)
```

Соглашение о вызовах компилятора отличается от интерпретатора, поэтому необходимо переопределить примитивы.
Старое определение примитива `show-prolog-vars` имело три параметра: список аргументов цели, список привязки и список ожидающих целей.
Новое определение `show-prolog-vars/2` также имеет три параметра, но это просто совпадение.
Первые два параметра - это два отдельных аргумента цели: список имен переменных и список значений переменных.
Последний параметр - функция продолжение.
Чтобы продолжить, мы вызываем эту функцию, но в случае неудачи мы переходим к точке перехвата, установленной в `top-level-prove`.

```lisp
(defun show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
            for var in vars do
            (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))

(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))
```

Имея эти определения, мы можем вызвать компилятор автоматически, просто сделав запрос с макросом ?-.

**Упражнение 12.6 [m]** Предположим, вы определяете предикат `p`, который вызывает `q`, а затем определяете `q`.
В некоторых реализациях Lisp, когда вы делаете запрос типа `(?- (p? x)) `, вы можете получить предупреждающее сообщение типа `"function q/1 undefined"` перед получением правильного ответа.
Проблема в том, что каждая функция компилируется отдельно, поэтому предупреждения, обнаруженные во время компиляции `p/1`, будут выводиться сразу, даже если функция `q/1` будет определена позже.
В ANSI Common Lisp есть способ отложить вывод предупреждений до тех пор, пока не будет выполнена серия компиляций: завершите компиляцию макросом `with-compilation-unit`. Даже если ваша реализация не предоставляет этот макрос, он может предоставлять те же функции под другим именем.
Выясните, определен ли уже в вашей реализации `with-compilation-unit` или его можно определить.

## 12.7 Тестирование компилятора

Наш скомпилированный код Prolog запускает головоломку зебра за 17,4 секунды, что в 16 раз больше, чем у интерпретированной версии, со скоростью 740 LIPS.

Другой популярный тест - это обратная функция Lisp, которую мы можем закодировать как отношение rev:

```lisp
(<- (rev () ()))
(<- (rev (?x . ?a) ?b) (rev ?a ?c) (concat ?c (?x) ?b))
(<- (concat () ?1 ?1)
(<- (concat (?x . ?a) ?b (?x . ?c)) (concat ?a ?b ?c))
```

rev использует отношение concat, которое означает конкатенацию, (`concat ?a ?b ?c`) истинно, когда `?a`, объединенное с `?b`, дает `?c`.
Это родственное имя предпочтительнее других процедурных имен, таких как append.
Но `rev` очень похож на следующие определения Лиспа:

```lisp
(defun rev (1)
  (if (null 1)
    nil
    (app (rev (rest 1 ))
        (list (first 1)))))

(defun app (x y)
  (if (null x)
    y
      (cons (first x)
        (app (rest x) y))))
```

Обе версии неэффективны.
Можно написать итеративную версию `reverse`, которая не требует дополнительных затрат и является хвосто-рекурсивной:

```lisp
(<- (irev ?l ?r) (irev3 ?l () ?r))
(<- (irev3 (?x . ?l) ?so-far ?r) (irev3 ?l (?x . ?so-far) ?r))
(<- (irev3 () ?r ?r))
```

Пролог `irev` эквивалентен этой программе на Лиспе:

```lisp
(defun irev (list) (irev2 list nil))

(defun irev2 (list so-far)
  (if (consp list)
      (irev2 (rest list) (cons (first list) so-far))
      so-far))
```

В следующей таблице показано время в секундах для выполнения этих подпрограмм в списках длиной 20 и 100, как для Пролога, так и для Лиспа, как интерпретируемых, так и скомпилированных.
(Только скомпилированный Lisp может выполнить rev для списка из 100 элементов, не исчерпывая пространства стека.) Также включены времена для головоломки "зебра", хотя версии этой программы на Lisp не существует.

| Problem    | Interp. Prolog | Comp. Prolog | Speed-up | Interp. Lisp | Comp. Lisp |
|------------|----------------|--------------|----------|--------------|------------|
| `zebra`    | 278.000        | 17.241       | 16       | -            | -          |
| `rev 20`   | 4.24           | .208         | 20       | .241         | .0023      |
| `rev 100`  | -              | -            | -        | -            | .0614      |
| `irev 20`  | .22            | .010         | 22       | .028         | .0005      |
| `irev 100` | 9.81           | .054         | 181      | .139         | .0014      |

Этот тест слишком мал, чтобы быть окончательным, но в этих примерах компилятор Prolog в 16–181 раз быстрее интерпретатора Prolog, немного быстрее, чем интерпретируемый Lisp, но все же в 17–90 раз медленнее, чем скомпилированный Lisp.
Это говорит о том, что интерпретатор Пролога не может использоваться в качестве практического инструмента программирования, но компилятор Пролога может.

Прежде чем двигаться дальше, интересно отметить, что Пролог автоматически предоставляет необязательные аргументы.
Хотя не существует специального синтаксиса для необязательных аргументов, часто используется соглашение о наличии двух версий отношения: одной с *n* аргументами и одной с *n -* 1.
Единственное предложение для случая *n -* 1 обеспечивает отсутствующий и, следовательно, "необязательный" аргумент.
В следующем примере `irev/2` можно рассматривать как версию `irev/3`, в которой отсутствует необязательный аргумент ().

```lisp
(<- (irev ?l ?r) (irev ?l () ?r))
(<- (irev (?x . ?l ) ?so-far ?r) (irev ?l (?x . ?so-far) ?r))
(<- (irev () ?r ?r))
```

Это примерно эквивалентно следующей версии Лиспа:

```lisp
(defun irev (list &optional (so-far nil))
  (if (consp list)
      (irev (rest list) (cons (first list) so-far))
      so-far))
```

## 12.8 Добавление дополнительных примитивов

Точно так же, как компилятору Lisp нужны машинные инструкции для ввода/вывода, арифметики и т.п., наша система Prolog должна иметь возможность выполнять определенные примитивные действия.
Для интерпретатора Пролога примитивы были реализованы с помощью символов функций.
Когда интерпретатор отправлялся за списком предложений, если вместо этого он получал функцию, он вызывал эту функцию, передавая ей аргументы в текущее отношение, текущие привязки и список неудовлетворенных целей.
Для компилятора Prolog примитивы могут быть установлены просто путем написания функции Lisp, которая соблюдает соглашение о принятии продолжения в качестве последнего аргумента и имеет имя в форме *symbol/arity*(символ/аность) Например, вот простой способ обработки ввода и вывод:

```lisp
(defun read/1 (exp cont)
```

`  (if (unify!
exp (read))`

```lisp
      (funcall cont)))
(defun write/1 (exp cont)
  (write (deref-exp exp) :pretty t)
  (funcall cont))
```

Вызов `(write ?x)` всегда будет успешным, поэтому всегда будет вызываться продолжение.
Точно так же можно использовать `(read ?x)` для чтения значения и объединения/унификации его с `?x`.
Если `?x` не связан, это то же самое, что присвоение значения.
Однако также можно выполнить вызов типа `(read (?x + ?y))`, который будет успешным только в том случае, если вход представляет собой список из трех элементов с + в середине.
Это простое расширение для определения `read/2` и `write/2` как отношений, указывающих, какой поток использовать.
Чтобы сделать это полезным, нужно определить `open/2` как отношение, которое принимает pathname(путь) в качестве одного аргумента и возвращает поток в качестве другого.
При желании могут поддерживаться и другие необязательные аргументы.

Примитив nl выводит новую строку:

```lisp
(defun nl/0 (cont) (terpri) (funcall cont))
```

Мы предоставили специальную поддержку для предиката унификации, =.
Однако мы могли бы значительно упростить компилятор, имея простое определение для `=/2`:

```lisp
(defun =/2 (?arg1 ?arg2 cont)
```

`  (if (unify!
?arg1 ?arg2)`

```lisp
    (funcall cont)))
```

Фактически, если мы дадим нашему компилятору единственное предложение:


(<- (= ?x `?x))`

он производит именно этот код для определения `=/ 2`.
Есть и другие предикаты равенства, о которых стоит побеспокоиться.
Предикат `= =/2` больше похож на equal в Лиспе.
Он не объединяет/унифицирует, а вместо этого проверяет равенство двух структур по своим элементам.
Переменная считается равной(equal) только самой себе.
Вот реализация:

```lisp
(defun =/2 (?arg1 ?arg2 cont)
  "Are the two arguments EQUAL with no unification,
```

`но с разыменованием?
Если так, то получится. "

```lisp
  (if (deref-equal ?arg1 ?arg2)
    (funcall cont)))
(defun deref-equal (x y)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?"
  (or (eql (deref x) (deref y))
    (and (consp x)
      (consp y)
      (deref-equal (first x) (first y))
      (deref-equal (rest x) (rest y)))))
```

Один из самых важных примитивов - это `call`.
Как и `funcall` в Лиспе, `call` позволяет нам создать цель, а затем попытаться ее доказать.

```lisp
(defun call/1 (goal cont)
  "Try to prove goal by calling it."
  (deref goal)
  (apply (make-predicate (first goal)
          (length (args goal)))
      (append (args goal) (list cont))))
```

Эта версия `call` выдаст ошибку времени выполнения, если цель не инстанцируется для списка, первый элемент которого является правильно определенным предикатом; можно проверить это и молча потерпеть неудачу, если нет определенного предиката.
Вот пример call, в котором цель законна:

```lisp
> (?- (= ?p member) (call (?p ?x (a b c))))
?P = MEMBER
?X = A;
?P = MEMBER
?X = B;
?P = MEMBER
?X = C;
No.
```

Теперь, когда у нас есть `call`, можно реализовать много нового.
Вот логические связки and и or:

```lisp
(<- (or ?a ?b) (call ?a))
(<- (or ?a ?b) (call ?b))
(<- (and ?a ?b) (call ?a) (call ?b))
```

Обратите внимание, что это только бинарные связки, а не *n*-арные специальные формы, используемые в Лиспе.
Кроме того, это определение сводит на нет большую часть преимуществ компиляции.
Цели внутри and или or будут интерпретироваться с помощью `call`, а не компилироваться.

Мы также можем определить `not` или, по крайней мере, нормальный Пролог `not`, который сильно отличается от логического `not`.
Фактически, в некоторых диалектах `not` пишется \+, что предполагается как &#x22AC;, то есть "не может быть производным".
Интерпретация состоит в том, что если цель G не может быть доказана, то (`not G`) истинно.
Логически существует разница между истинным и неизвестным (`not G`), но игнорирование этой разницы делает Prolog более практичным языком программирования.
См. [Lloyd 1987](B9780080571157500285.xhtml#bb0745) для получения дополнительной информации о формальной семантике отрицания в Прологе.

Вот реализация `not/1`.
Поскольку он должен управлять trail, и у нас могут быть другие предикаты, которые захотят сделать то же самое, мы упакуем то, что было сделано в `might-add-undo-bindings` в макрос `with-undo-bindings:`

```lisp
(defmacro with-undo-bindings (&body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
    (first body)
    '(let ((old-trail (fill-pointer *trail*)))
      ,(first body)
        ,@(loop for exp in (rest body)
```

`                collect '(undo-bindings!
old-trail)`

```lisp
                collect exp))))
(defun not/1 (relation cont)
```

`"Отрицание неудачей: Если вы не можете доказать, что G.
тогда (not G) верно"`.

```lisp
  ;; Either way, undo the bindings.
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from not/1 nil)))
    (funcall cont)))
```

Вот пример, когда `not` работает нормально:

```lisp
> (?- (member ?x (a b c)) (not (= ?x b)))
?X = A;
?X = C;
No.
```

Теперь посмотрим, что произойдет, если мы просто изменим порядок двух целей:

```lisp
> (?- (not (= ?x b)) (member ?x (a b c)))
No.
```

Первый пример завершается успешно, если только `?x` не привязан к `b`. Во втором примере `?x` не cвязан в начале, поэтому `(= ?x b )` завершается успешно, not терпит неудачу и `member` цель никогда не достигается.
Таким образом, наша реализация `not` имеет последовательную процедурную интерпретацию, но она не эквивалентна декларативной интерпретации, обычно применяемой для логического отрицания.
Обычно можно было бы ожидать, что `a` и `c` будут допустимыми решениями запроса, независимо от порядка целей.

Одно из фундаментальных различий между Prolog и Lisp заключается в том, что Prolog является реляционным: вы можете легко выражать индивидуальные отношения.
Lisp, с другой стороны, хорошо выражает коллекции вещей в виде списков.
Пока у нас нет способа сформировать коллекцию объектов, удовлетворяющих отношению в Прологе.
Мы можем легко перебирать объекты; мы просто не можем собрать их вместе.
Примитивный `bagof` - это один из способов сбора данных.
В общем, `(bagof ?x (p ?x) ?bag)` объединяет `?bag` со списком всех `?x`-ов, которые удовлетворяют `(p ?x)`.
Если таких `?x`-ов нет, то вызов `bagof` не выполняется.
A *bag* - это неупорядоченная коллекция, в которой разрешены дубликаты.
Например, *bag* {*a*, *b, a*} такая же, как *bag* {*a*, *a*, *b*}, но отличается от {*a*, *b*}.
Bags контрастируют с *sets*, которые представляют собой неупорядоченные коллекции без дубликатов.
Набор(set) {*a*, *b*} такой же, как набор(set) {*a*, *b*}.
Вот реализация `bagof:`

```lisp
(defun bagof/3 (exp goal resuit cont)
  "Find all solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
```

`  ;; Ex: Assume (p 1) (p 2) (p 3).
Then:`

```lisp
  ;: (bagof ?x (p ?x) ?1) => ?1 = (1 2 3)
  (let ((answers nil))
  (call/1 goal #'(lambda ()
      (push (deref-copy exp) answers)))
  (if (and (not (null answers))
```

`    (unify!
resuit (nreverse answers)))`

```lisp
  (funcall cont))))
  (defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  (sublis (mapcar #'(lambda (var) (cons (deref var) (?))
    (unique-find-anywhere-if #'var-p exp))
  exp))
```

Ниже мы используем `bagof`, чтобы собрать список всех, кого любит Сэнди(Sandy likes).
Обратите внимание, что в результате получается bag, а не set: Сэнди появляется несколько раз.

```lisp
> (?- (bagof ?who (likes Sandy ?who) ?bag))
?WHO = SANDY
?BAG = (LEE KIM ROBIN SANDY CATS SANDY);
No.
```

В следующем примере мы формируем bag из каждого списка длиной три, в котором в качестве членов входят `A` и `B`:

```lisp
> (?- (bagof ?l (and (length ?l (1  + (1  + (1  + 0))))
      (and (member a ?l) (member b ?l)))
    ?bag))
?L = (?5 ?8 ?11 ?68 ?66)
?BAG = ((A B ?17) (A ?21 B) (B A ?31) (?38 A B) (B ?48 A) (?52 B A))
No.
```

Те, кто разочарован bag, содержащим несколько версий одного и того же ответа, могут предпочесть примитивный `setof`, который выполняет те же вычисления, что и `bagof`, но затем отбрасывает дубликаты.

```lisp
(defun setof/3 (exp goal resuit cont)
  "Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
```

`  ;; Ex: Assume (p 1) (p 2) (p 3).
Then:`

```lisp
  ;;  (setof ?x (p ?x) ?l ) => ?l = (1 2 3)
  (let ((answers nil))
  (call/1 goal #'(lambda ()
      (push (deref-copy exp) answers)))
  (if (and (not (null answers))
```

`    (unify!
resuit (delete-duplicates`

```lisp
        answers
        :test #'deref-equal)))
  (funcall cont))))
```

Пролог поддерживает арифметические операции с оператором `is`.
Например, `(is ?x (+ ?y 1))` объединяет/унифицирует `?x` со значением `?y` плюс один.
Это выражение не работает, если `?y` не привязано, и выдает ошибку времени выполнения, если `?y` не является числом.
В нашей версии Пролога мы можем поддерживать не только арифметику, но и любое выражение Лиспа:

```lisp
(defun is/2 (var exp cont)
  ;; Example: (is ?x (+  3 (* ?y (+ ?z 4))))
  ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))
  (if (and (not (find-if-anywhere #'unbound-var-p exp))
```

`    (unify!
var (eval (deref-exp exp))))`

```lisp
  (funcall cont)))
(defun unbound-var-p (exp)
  "Is EXP an unbound var?"
  (and (var-p exp) (not (bound-p exp))))
```

Кроме того, мы могли бы также предоставить программисту Prolog доступ к функции `unbound-var-p`.
Стандартное имя этого предиката - `var/1`:

```lisp
(defun var/1 (?arg1 cont)
  "Succeeds if ?arg1 is an uninstantiated variable."
  (if (unbound-var-p ?arg1)
  (funcall cont)))
```

Это примитив не работает, если какая-либо часть второго аргумента не связана.
Однако есть выражения с переменными, которые можно решить, хотя и не прямым вызовом `eval`.
Например, следующая цель может быть решена путем привязки `?x` к `2`:

```lisp
(solve (=  12 (* (+ ?x 1) 4)))
```

Мы могли бы захотеть иметь более прямой доступ к Лиспу из Пролога.
Проблема с `is` в том, что он требует проверки на наличие несвязанных переменных и вызывает `eval` для рекурсивного вычисления аргументов.
В некоторых случаях мы просто хотим добраться до Лисп `apply`, не прибегая к страховочной сетке, предоставляемой `is`.
Это делает примитив `lisp`.
Излишне говорить, что `lisp` не является частью стандартного Пролога.

```lisp
(defun lisp/2 (?result exp cont)
  "Apply (first exp) to (rest exp), and return the result."
  (if (and (consp (deref exp))
```

`    (unify!
?result (apply (first exp) (rest exp))))`

```lisp
  (funcall cont)))
```

**Упражнение 12.7 [m]** Определите примитив `solve/1`, который работает аналогично функции `solve`, используемой в student ([страница 225](B9780080571157500078.xhtml#p225)).
Решите, следует использовать в качестве аргумента одно уравнение или список уравнений.

**Упражнение 12.8 [h]** Предположим, у нас есть цель вида `(solve (=  12 (* (+ ?x 1) 4)))`.
Вместо того, чтобы манипулировать уравнением, когда во время выполнения вызывается `solve/1`, мы могли бы предпочесть выполнять часть работы во время компиляции, обрабатывая вызов, как если бы он был `(solve (= ?x 2))`.
Напишите макрос компилятора Пролога для `solve`.
Обратите внимание, что даже если вы определили макрос компилятора, вам все равно понадобится базовый примитив, потому что предикат может быть вызван через `call/1`.
То же самое происходит в Лиспе: даже когда вы предоставляете макрос компилятора, вам все равно нужна фактическая функция в случае `funcall` или `apply`.

**Упражнение 12.9 [h]** Для какого из предикатов `call`, `and`, `or`, `not` или` repeat` можно использовать макросы компилятора?
Напишите макросы компилятора для тех предикатов, которые могут его использовать.

**Упражнение 12.10 [m]** Вы могли заметить, что `call/1` неэффективен по двум важным причинам.
Сначала он вызывает `make-predicate`, который должен построить символ, добавляя строки, а затем искать строку в таблице символов Lisp.
Измените `make-predicate` для сохранения символа предиката при его первом создании, чтобы он мог быстрее выполнять поиск при последующих вызовах.
Вторая неэффективность - это вызов append.
Измените весь компилятор так, чтобы аргумент продолжение шел первым, а не последним, что устраняет необходимость применения  `append` в `call`.

**Упражнение 12.11 [s]** Примитив `true/0` всегда выполняется успешно, а `fail/0` всегда терпит неудачу.
Определите эти примитивы.
Подсказка: первая соответствует функции Common Lisp, а вторая - функции, уже определенной в этой главе.

**Упражнение 12.12 [s]** Можно ли написать `= =/2` как список предложений, а не как примитив?

**Упражнение 12.13 [m]** Напишите версию `deref-copy`, которая обходит выражение аргумента только один раз.

## 12.9 Cut(Обрезка)

В Лиспе можно писать программы с явным откатом(backtrack), хотя это может быть неудобно, если имеется более одной или двух точек возврата.
В Прологе откат является автоматическим и неявным, но мы пока не знаем никакого способа *избежать* его.
Существует две причины, по которым программист на Prolog может захотеть отключить откат.
Во-первых, отслеживание точек отката требует времени и места.
Программист, который знает, что у определенной проблемы есть только одно решение, должен иметь возможность ускорить вычисления, сказав программе не рассматривать другие возможные ответвления.
Во-вторых, иногда простая логическая спецификация проблемы приводит к избыточным решениям или даже к некоторым непредвиденным решениям.
Может случиться так, что простое сокращение пространства поиска, чтобы исключить некоторые отслеживания откатов, даст только желаемые ответы, в то время как реструктуризация программы, чтобы давать все и только правильные ответы, будет сложнее.
Вот пример.
Предположим, мы хотим определить предикат `max/3`, который выполняется, когда третий аргумент является максимумом из первых двух аргументов, где первые два аргумента всегда будут преобразованы в числа.
Прямое определение:

```lisp
(<- (max ?x ?y ?x) (>= ?x ?y))
(<- (max ?x ?y ?y) (< ?x ?y))
```

Декларативно оно правильно, но с процедурной точки зрения вычисление отношения < является пустой тратой времени, если >= успешно: в этом случае < не может быть успешным.
Символ сокращения/обрезки, записываемый !, Можно использовать, чтобы остановить бесполезные вычисления.
Мы могли бы написать:

```lisp
(<- (max ?x ?y ?x) (>= ?x ?y) !)
(<- (max ?x ?y ?y))
```

Обрезка в первом предложении говорит о том, что если первое предложение будет успешным, то никакие другие предложения не будут рассматриваться.
Так что теперь второй пункт нельзя толковать самостоятельно.
Скорее, он интерпретируется как "если первое предложение не выполняется, то второе из двух чисел является максимальным".

Как правило, сокращение/обрезка может происходить в любом месте предложения, а не только в конце.
Хорошей декларативной интерпретации обрезки не существует, но процедурная интерпретация двояка.
Во-первых, когда обрезка "выполняется" как цель, она всегда успешна.
Но в дополнение к успеху она устанавливает барьер, который нельзя преодолеть последующим откатом.
Обрезка служит для отключения отката как от целей справа от обрезки (в том же предложении), так и от предложений ниже обрезки (в том же предикате).
Давайте посмотрим на более абстрактный пример:

```lisp
(<- (p) (q) (r) ! (s) (t))
(<- (p) (s))
```

При обработке первого предложения `p` откат может происходить свободно при попытке решить `q` и `r`.
Как только `r` решен, встречается обрезка.
С этого момента откат может происходить свободно при решении `s` и` t`, но Prolog никогда не будет откатываться за обрез в `r`, и второе предложение не будет рассматриваться.
С другой стороны, если `q` или `r` завершились неудачно (до того, как произойдет обрезка), тогда Пролог перейдет ко второму предложению.

Теперь, когда цель сокращения ясна, давайте подумаем, как его следует реализовать.
Мы рассмотрим немного более сложный предикат с переменными и несколькими разрезами:

```lisp
(<- (p ?x a) ! (q ?x))
(<- (p ?x b) (r ?x) ! (s ?x))
```

Мы должны организовать это так, чтобы, как только мы возвращаемся к сокращению, цели больше не учитывались.
В первом предложении, когда `q / 1` не работает, мы хотим немедленно вернуться из` p / 2`, а не рассматривать второе предложение.
Точно так же в первый раз, когда «s / 1» терпит неудачу, мы хотим вернуться из «p / 2», вместо того, чтобы рассматривать другие решения для «r / 1».
Таким образом, нам нужен код, который выглядит примерно так:

```lisp
(defun p/2 (argl arg2 cont)
  (let ((old-trail (fill-pointer *trail*)))
```

`    (if (unify!
arg2 'a)`

```lisp
      (progn (q/1 argl cont)
          (return-from p/2 nil)))
```

`    (undo-bindings!
old-trail)`

`    (if (unify!
arg2 'b)`

```lisp
      (r/1 argl #'(lambda ()
              (progn (s/1 argl cont)
                (return-from p/2 nil)))))))
```

Мы можем получить этот код, сделав одно изменение в `compile-body:` когда первая цель в теле (или то, что осталось от тела) является отрезанным символом, тогда мы должны сгенерировать `progn`, содержащий код для остальной части тела, за которой следует `return-from` компилируемого предиката.
К сожалению, имя предиката недоступно для `compile-body`. Мы могли бы изменить `compile-clause` и `compile-body`, чтобы использовать имя предиката в качестве дополнительного аргумента, или мы могли бы привязать предикат как специальную переменную в `compile-predicate`.
Я выбираю последнее:

```lisp
(defvar *predicate* nil
  "The Prolog predicate currently being compiled")

(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((*predicate* (make-predicate symbol arity))    ;***
        (parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,*predicate* (,@parameters cont)
  .,(maybe-add-undo-bindings
     (mapcar #'(lambda (clause)
           (compile-clause parameters clause 'cont))
      clauses)))))))

(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)                              ;***
     `(progn ,(compile-body (rest body) cont bindings) ;***
             (return-from ,*predicate* nil)))          ;***
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (predicate goal)))
              (macro-val (if macro
                             (funcall macro goal (rest body)
                                      cont bindings))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            `(,(make-predicate (predicate goal)
                               (relation-arity goal))
              ,@(mapcar #'(lambda (arg)
                            (compile-arg arg bindings))
                        (args goal))
              ,(if (null (rest body))
                   cont
                   `#'(lambda ()
                        ,(compile-body
                           (rest body) cont
                           (bind-new-variables bindings goal))))))))))
```

**Упражнение 12.14 [m]** Учитывая приведенные ниже определения, выясните, что будет делать вызов `test-cut` и что он будет писать:

```lisp
(<- (test-cut) (p a) (p b) ! (p c) (p d))
(<- (test-cut) (p e))
(<- (p ?x) (write (?x 1)))
(<- (p ?x) (write (?x 2)))
```

Другой способ использовать обрезку - это цикл *repeat/fail*(повтор/сбой).
Повторение предиката определяет следующие два предложения:

```lisp
(<- (repeat))
(<- (repeat) (repeat))
```

Альтернативное определение примитива:

```lisp
(defun repeat/0 (cont)
  (loop (funcall cont)))
```

К сожалению, `repeat` - один из предикатов, которым злоупотребляют.
В нескольких книгах по Прологу представлены такие программы:

```lisp
(<- (main)
  (write "Hello.")
  (repeat)
  (write "Command: ")
  (read ?command)
  (process ?command)
  (= ?command exit)
  (write "Good bye."))
```

Смысл состоит в том, что команды читаются по одной, а затем обрабатываются.
Для каждой команды, кроме `exit`, процесс выполняет соответствующее действие и затем терпит неудачу.
Это вызывает откат к цели repeat(повторения), и новая команда считывается и обрабатывается.
Когда команда `exit`, процедура возвращается.

Есть две причины, почему это плохая программа.
Во-первых, это нарушает принцип ссылочной прозрачности.
Вещи, которые выглядят одинаково, должны быть одинаковыми, независимо от контекста, в котором они используются.
Но здесь невозможно сказать, что четыре из шести целей в теле составляют цикл, а другие цели находятся вне цикла.
Во-вторых, это нарушает принцип абстракции.
Предикат должен быть понятен как отдельная единица.
Но здесь процесс предиката можно понять только при рассмотрении контекста, в котором он вызывается: контекста, который требует его сбоя после обработки каждой команды.
Как указывает [Ричард О'Киф 1990] (B9780080571157500285.xhtml#bb0925), правильный способ написания этого предложения выглядит следующим образом:

```lisp
(<- (main)
  (write "Hello.")
  (repeat)
      (write "Command: ")
      (read ?command)
      (process ?command)
      (or (= ?command exit) (fail))
  !
  (write "Good bye."))
```

Отступ четко указывает на пределы повторения цикла.
Цикл завершается явным тестом, за которым следует отрезка, чтобы вызывающая программа не могла случайно вернуться(откатиться) в цикл после выхода.
Лично я предпочитаю такой язык, как Lisp, где круглые скобки делают такие конструкции, как циклы, явными, а отступы могут выполняться автоматически.
Но О'Киф показывает, что хорошо структурированные читаемые программы можно писать и на Прологе.

Конструкции if-then и if-then-else можно легко записать в виде предложений.
Обратите внимание, что if-then-else использует обрезку для фиксации части `then`, если тест удовлетворен.

```lisp
(<- (if ?test ?then) (if ?then ?else (fail)))
(<- (if ?test ?then ?else)
  (call ?test)
  !
  (call ?then))
(<- (if ?test ?then ?else)
  (call ?else))
```

Обрезка может использоваться для реализации нелогического `не`.
Следующие два предложения часто приводятся перед определением `not`.
Наш компилятор успешно превращает эти два предложения в точно такой же код, который был приведен ранее для примитива `not/1`:

```lisp
(<- (not ?p) (call ?p) ! (fail))
(<- (not ?p))
```

## 12.10 "Настоящий" Пролог

Система Prolog-In-Lisp, разработанная в этой главе, использует синтаксис Lisp, потому что она предназначена для встраивания в систему Lisp.
Другие реализации Пролога, использующие синтаксис Лиспа, включают микро-Пролог, Символический Пролог и Пролог LMI.

Однако большинство систем Пролога используют синтаксис, близкий к традиционным математическим обозначениям.
В следующей таблице сравнивается синтаксис "стандартного" Пролога с синтаксисом Пролога-в-Лиспе.
Хотя в настоящее время над стандартизацией Пролога работает международный комитет, окончательный отчет еще не выпущен, поэтому разные диалекты могут иметь немного отличающийся синтаксис.
Тем не менее, в большинстве реализаций используются обозначения, приведенные здесь.
Они являются производными от Пролога, разработанного в Эдинбургском университете для DEC-10 Дэвидом Х.
Д.
Уоррен и его коллеги.
Имена примитивов в последнем разделе также взяты из Эдинбургского Пролога.

|           | Prolog          | Prolog-In-Lisp        |
|-----------|-----------------|-----------------------|
| atom      | `lower`         | `const`               |
| variable  | `Upper`         | `?var`                |
| anonymous | `-`             | `?`                   |
| goal      | `p(Var,const)`  | `(p ?var const)`      |
| rule      | `p(X) :- q(X).` | `(<- (p ?x) (q ?x))`  |
| fact      | `p(a).`         | `(<- (p a))`          |
| query     | `?- p(X).`      | `(?- (p ?x))`         |
| list      | `[a,b,c]`       | `(a b c)`             |
| cons      | `[a| Rest]`     | `(a . ?rest)`         |
| nil       | `[]`            | `()`                  |
| and       | `p(X). q(X)`    | `(and (p ?x) (q ?x)>` |
| or        | `P(X): q(X)`    | `(or (p ?x) (q ?x))`  |
| not       | `\+ p(X)`       | `(not (p ?x))`        |

Мы переняли склонность Лиспа к спискам; термины состоят из атомов, переменных и заключений других терминов.
В реальном Prolog есть cons-ячейки, но термины обычно строятся из *структур*, а не списков.
Термин Пролога `p(a, b)` соответствует вектору Лиспа `#(p/2 a b)`, а не списку `(p a b)`.
Меньшая часть реализаций Prolog использует *совместное использование структур.* В этом подходе каждый неатомарный термин представлен каркасом, который содержит заполнители для переменных и заголовок, который указывает на каркас, а также содержит переменные, которые будут заполнять заполнители .
С разделяемой структуры просто сделать копию: просто скопируйте заголовок, независимо от размера скелета.
Однако манипулирование терминами осложняется необходимостью отслеживать как скелет, так и заголовок.
См. [Boyer and Moore 1972](B9780080571157500285.xhtml#bb0110) для получения дополнительной информации о совместном использовании структуры.

Еще одно важное отличие состоит в том, что настоящий Пролог использует эквивалент продолжений неудач, а не продолжений успехов.
Никакого фактического продолжения в смысле замыкания не создается.
Вместо этого, когда выбор сделан, адрес кода для следующего выбора помещается в стек.
В случае неудачи из стека выскакивает следующий вариант.
Это напоминает подход с откатом с использованием возможности Scheme `call/cc`, описанный на [стр. 772] (B9780080571157500224.xhtml#p772).

**Упражнение 12.15 [m]** Предполагая подход, использующий стек продолжений неудач вместо продолжений успехов, покажите, как будет выглядеть код для `p` и `member`.
Обратите внимание, что вам не нужно передавать продолжения сбоя; вы можете просто поместить их в стек, который вызовет `top-level-prove`.
Как будет реализовано сокращение?
Правильно ли мы сделали выбор, реализовав наш компилятор с продолжениями успехов, или было бы лучше продолжать неудачи?

## 12.11 История и ссылки

As described in [chapter 11](B978008057115750011X.xhtml), the idea of logic programming was fairly well understood by the mid-1970s.
But because the implementations of that time were slow, logic programming did not catch on.
It was the Prolog compiler for the DEC-10 that made logic programming a serious alternative to Lisp and other general-purpose languages.
The compiler was developed in 1977 by David H.
D.
Warren with Fernando Pereira and Luis Pereira.
See the paper by [Warren (1979)](B9780080571157500285.xhtml#bb1325) and by all three (1977).

Unfortunately, David H.
D.
Warren's pioneering work on compiling Prolog has never been published in a widely accessible form.
His main contribution was the description of the Warren Abstract Machine (WAM), an instruction set for compiled Prolog.
Most existing compilers use this instruction set, or a slight modification of it.
This can be done either through byte-code interpretation or through macroexpansion to native machine instructions.
[A&iuml;t-Kaci 1991](B9780080571157500285.xhtml#bb0020) provides a good tutorial on the WAM, much less terse than the original ([Warren 1983](B9780080571157500285.xhtml#bb1330)).
The compiler presented in this chapter does not use the WAM.
Instead, it is modeled after Mark [Stickel's (1988)](B9780080571157500285.xhtml#bb1200) theorem prover.
A similar compiler is briefly sketched by Jacques [Cohen 1985](B9780080571157500285.xhtml#bb0225).

## 12.12 Упражнения

**Exercise  12.16 [m]** Change the Prolog compiler to allow implicit `calls`.
That is, if a goal is not a cons cell headed by a predicate, compile it as if it were a `call`.
The clause:

```lisp
(<- (p ?x ?y) (?x c) ?y)
```

should be compiled as if it were:

```lisp
(<- (p ?x ?y) (call (?x c)) (call ?y))
```

**Exercise  12.17 [h]** Here are some standard Prolog primitives:

*   `get/1` Read a single character and unify it with the argument.

*   `put/1` Print a single character.

*   `nonvar/1, /=, /==` The opposites of `var, = and = =` , respectively.

*   `integer/1` True if the argument is an integer.

*   `atom/1` True if the argument is a symbol (like Lisp's `symbol p`).

*   `atomic/1` True if the argument is a number or symbol (like Lisp's `atom`).

*   <,>,=<,>= Arithmetic comparison; succeeds when the arguments are both instantiated to numbers and the comparison is true.

*   `listing/0` Print out the clauses for all defined predicates.

*   `listing/1` Print out the clauses for the argument predicate.

Implement these predicates.
In each case, decide if the predicate should be implemented as a primitive or a list of clauses, and if it should have a compiler macro.

There are some naming conflicts that need to be resolved.
Terms like `atom` have one meaning in Prolog and another in Lisp.
Also, in Prolog the normal notation is \= and \==, not /= and /==.
For Prolog-In-Lisp, you need to decide which notations to use: Prolog's or Lisp's.

**Exercise  12.18 [s]** In Lisp, we are used to writing n-ary calls like `(<  1 n 10 ) or (= x y z )`.
Write compiler macros that expand n-ary calls into a series of binary calls.
For example, `(<  1 n 10)` should expand into `(and (<  1 n) (< n 10))`.

**Exercise  12.19 [m]** One feature of Lisp that is absent in Prolog is the `quote` mechanism.
Is there a use for `quote?` If so, implement it; if not, explain why it is not needed.

**Exercise  12.20 [h]** Write a tracing mechanism for Prolog.
Add procedures `p-trace` and `p-untrace` to trace and untrace Prolog predicates.
Add code to the compiler to generate calls to a printing procedure for goals that are traced.
In Lisp, we have to trace procedures when they are called and when they return.
In Prolog, there are four cases to consider: the call, successful completion, backtrack into subsequent clauses, and failure with no more clauses.
We will call these four `cases call`, `exit`, `redo,` and `fail`, respectively.
If we traced `member,` we would expect tracing output to look something like this:

```lisp
> (?- (member ?x (a b c d)) (fail))
  CALL MEMBER: ?1 (A B C D)
  EXIT MEMBER: A (A B C D)
  REDO MEMBER: ?1 (A B C D)
    CALL MEMBER: ?1 (B C D)
    EXIT MEMBER: B (B C D)
    REDO MEMBER: ?1 (B C D)
      CALL MEMBER: ?1 (C D)
      EXIT MEMBER: C (C D)
      REDO MEMBER: ?1 (C D)
        CALL MEMBER: ?1 (D)
        EXIT MEMBER: D (D)
        REDO MEMBER: ?1 (D)
          CALL MEMBER: ?1 NIL
          REDO MEMBER: ?1 NIL
          FAIL MEMBER: ?1 NIL
        FAIL MEMBER: ?1 (D)
      FAIL MEMBER: ?1 (C D)
    FAIL MEMBER: ?1 (B C D)
  FAIL MEMBER: ?1 (A B C D)
No.
```

**Exercise  12.21 [m]** Some Lisp systems are very slow at compiling functions.
`KCL` is an example; it compiles by translating to `C` and then calling the `C` compiler and assembler.
In `KCL` it is best to compile only code that is completely debugged, and run interpreted while developing a program.

Alter the Prolog compiler so that calling the Lisp compiler is optional.
In all cases, Prolog functions are translated into Lisp, but they are only compiled to machine language when a variable is set.

**Exercise  12.22 [d]** Some Prolog systems provide the predicate `freeze` to "freeze" a goal until its variables are instantiated.
For example, the goal `(freeze x (> x 0))` is interpreted as follows: if `x` is instantiated, then just evaluate the goal `(> x 0)`, and succeed or fail depending on the result.
However, if `x` is unbound, then succeed and continue the computation, but remember the goal `(> x 0)` and evaluate it as soon as `x` becomes instantiated.
Implement freeze.

**Exercise  12.23 [m]** Write a recursive version of `anonymous-variables-in` that does not use a local function.

## 12.13 Ответы

**Answer 12.6** Here's a version that works for Texas Instruments and Lucid implementations:

```lisp
(defmacro with-compilation-unit (options &body body)
  "Do the body, but delay compiler warnings until the end."
  ;; This is defined in Common Lisp the Language, 2nd ed.
  '(,(read-time-case
    #+TI 'compiler:compiler-warnings-context-bind
    #+Lucid 'with-deferred-warnings
        'progn)
    .,body))

(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (with-compilation-unit ()
  (mapc #'prolog-compile symbols)
  (setf *uncompiled* (set-difference *uncompiled* symbols))))
```

**Answer 12.9** Macros for `and` and `or` are very important, since these are commonly used.
The macro for `and` is trivial:

```lisp
(def-prolog-compiler-macro and (goal body cont bindings)
  (compile-body (append (args goal) body) cont bindings))
```

The macro for or is trickier:

```lisp
(def-prolog-compiler-macro or (goal body cont bindings)
  (let ((disjuncts (args goal)))
    (case (length disjuncts)
      (0 fail)
      (1 (compile-body (cons (first disjuncts) body) cont bindings))
      (t (let ((fn (gensym "F")))
        '(fl&egrave;t ((,fn () ,(compile-body body cont bindings)))
          .,(maybe-add-undo-bindings
            (loop for g in disjuncts collect
              (compile-body (list g) '#',fn
                bindings)))))))))
```

**Answer 12.11**`true/0` is `funcall` : when a goal succeeds, we call the continuation, `fail/0` is `ignore`: when a goal fails, we ignore the continuation.
We could also define compiler macros for these primitives:

```lisp
(def-prolog-compiler-macro true (goal body cont bindings)
  (compile-body body cont bindings))

(def-prolog-compiler-macro fail (goal body cont bindings)
  (declare (ignore goal body cont bindings))
  nil)
```

**Answer 12.13**

```lisp
(defun deref-copy (exp)
  "Build a copy of the expression, which may have variables.
  The part without variables can be returned as is."
  (let ((var-alist nil ))
    (labels
      ((walk (exp)
        (deref exp)
        (cond ((consp exp)
          (reuse-cons (walk (first exp))
              (walk (rest exp))
              exp))
          ((var-p exp)
          (let ((entry (assoc exp var-alist)))
            (if (not (null entry))
            (cdr entry)
            (let ((var-copy (?)))
                (push (cons exp var-copy) var-alist)
                var-copy))))
          (t exp))))
    (walk exp))))
```

**Answer 12.14** In the first clause of `test-cut`, all four calls to `p` will succeed via the first clause of `p`.
Then backtracking will occur over the calls to `(p c)` and `(p d)`.
All four combinations of `1` and `2` succeed.
After that, backtracking would normally go back to the call to `(p b)`.
But the cut prevents this, and the whole `(test-cut)` goal fails, without ever considering the second clause.
Here's the actual output:

```lisp
(?- (test-cut))
(A 1)(B 1)(C 1) (D 1)
Yes;
(D 2)
Yes;
(C 2)(D 1)
Yes;
(D 2)
Yes;
No.
```

**Answer 12.17** For example:

```lisp
(defun >/2 (x y cont)
  (if (and (numberp (deref x)) (numberp (deref y)) (> x y))
    (funcall cont)))
(defun numberp/1 (x cont)
  (if (numberp (deref x))
    (funcall cont)))
```

**Answer 12.19** Lisp uses quote in two ways: to distinguish a symbol from the value of the variable represented by that symbol, and to distinguish a literal list from the value that would be returned by evaluating a function call.
The first distinction Prolog makes by a lexical convention: variables begin with a question mark in our Prolog, and they are capitalized in real Prolog.
The second distinction is not necessary because Prolog is relational rather than functional.
An expression is a goal if it is a member of the body of a clause, and is a literal if it is an argument to a goal.

**Answer 12.20** Hint: Here's how `member` could be augmented with calls to a procedure, `prolog-trace`, which will print information about the four kinds of tracing events:

```lisp
(defun member/2 (?arg1 ?arg2 cont)
  (let ((old-trail (fill-pointer *tra1l*))
      (exit-cont #'(lambda ()
          (prolog-trace 'exit 'member ?arg1 ?arg2 )
          (funcall cont))))
    (prolog-trace 'call 'member ?arg1 ?arg2)
```

`    (if (unify!
?arg2 (cons ?arg1 (?)))`

```lisp
      (funcall exit-cont))
```

`    (undo-bindings!
old-trail)`

```lisp
    (prolog-trace 'redo 'member ?arg1 ?arg2)
    (let ((?rest (?)))
```

`      (if (unify!
?arg2 (cons (?) ?rest))`

```lisp
      (member/2 ?arg1 ?rest exit-cont)))
    (prolog-trace 'fail 'member ?arg1 ?arg2)))
```

The definition of `prolog-trace` is:

```lisp
(defvar *prolog-trace-indent* 0)
(defun prolog-trace (kind predicate &rest args)
  (if (member kind '(call redo))
  (incf *prolog-trace-indent* 3))
  (format t "~&~VT~a ~  a:~{ ~  a  ~}"
      *prolog-trace-indent* kind predicate args)
  (if (member kind '(fail exit))
  (decf *prolog-trace-indent* 3)))
```

**Answer 12.23**

```lisp
(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (values (anon-vars-in tree nil nil)))

(defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variables
  seen once, and a list of variables seen more than once."
  (cond
    ((consp tree)
    (multiple-value-bind (new-seen-once new-seen-more)
      (anon-vars-in (first tree) seen-once seen-more)
      (anon-vars-in (rest tree) new-seen-once new-seen-more)))
    ((not (variable-p tree)) (values seen-once seen-more))
    ((member tree seen-once)
    (values (delete tree seen-once) (cons tree seen-more)))
    ((member tree seen-more)
    (values seen-once seen-more))
    (t (values (cons tree seen-once) seen-more))))
```