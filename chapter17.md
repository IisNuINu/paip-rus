# Глава 17
## Маркировка линейных диаграмм удовлетворением ограничений

> Неверно думать о работе Вальса только как об изложении эпистемологии штриховых рисунков многогранников.
Вместо этого я думаю, что это элегантный пример парадигмы, которую мы можем ожидать увидеть снова и снова.

> -Patrick Winston

> The Psychology of Computer Vision (1975)

Эта книга касается только тех областей ИИ, которые имеют дело с абстрактными рассуждениями.
Есть и другая сторона искусственного интеллекта, область *робототехники*, которая связана с взаимодействием абстрактных рассуждений с реальным миром с помощью датчиков и двигателей.
Робот получает входные данные от камер, микрофонов, сонара и сенсорных устройств и производит "выходной сигнал", перемещая свои придатки или генерируя звуки.
Реальный мир - более беспорядочное место, чем абстрактные миры, которые мы рассматривали.
Робот должен иметь дело с зашумленными данными, неисправными компонентами и другими агентами и событиями в мире, которые могут повлиять на изменения в окружающей среде.

Компьютерное зрение - это подраздел робототехники, который занимается интерпретацией визуальной информации.
Низкоуровневое зрение принимает данные непосредственно с камеры и обнаруживает линии, области и текстуры.
Мы не будем этим заниматься.
Высокоуровневое зрение использует результаты низкоуровневого компонента для построения трехмерной модели объектов, изображенных на сцене.
В этой главе рассматривается один небольшой аспект видения(зрения) высокого уровня.

## 17.1 Проблема маркировки строк

В этой главе мы рассмотрим проблему маркировки линейной диаграммы: имея список линий и вершин, в которых они пересекаются, как мы можем определить, что эти линии представляют?
Например, даны девять линий на [рис. 17.1](#f0010), как мы можем интерпретировать диаграмму как куб?

| []()                                   |
|----------------------------------------|
| ![f17-01](images/chapter17/f17-01.jpg) |
| Рисунок 17.1: Куб                     |

Прежде чем мы сможем пререйти к интерпретации, мы должны договориться о кандидатах.
В конце концов, [рисунок 17.1](#f0010) может быть просто шестиугольником с тремя линиями посередине.
Для целей этой главы мы будем рассматривать только диаграммы, которые изображают один или несколько *многогранников-* трехмерных твердых фигур, поверхности которых являются плоскими гранями, ограниченными прямыми линиями.
Кроме того, мы разрешаем только *трехгранные* вершины.
То есть каждая вершина должна быть образована пересечением трех граней, как в углу куба, где вершина, передняя часть и сторона куба сходятся.
Третье ограничение на диаграммы - недопустимость так называемых "случайных" вершин.
Например, [рисунок 17.1](#f0010) может быть изображением трех разных кубиков, висящих в пространстве, которые случайно выстраиваются так, что край одного выровнен с краем другого с нашей точки зрения.
Предположим, что это не так.

Учитывая диаграмму, которая соответствует этим трем ограничениям, наша цель - идентифицировать каждую линию, помещая ее в один из трех классов:

1.  Выпуклая линия разделяет две видимые грани многогранника так, что прямая от одной грани до другой проходит внутри многогранника.
Он будет отмечен знаком плюс: +.
!!!(p) {:.numlist}

2.  Вогнутая линия разделяет две грани двух многогранников, так что линия между двумя пространствами проходит через пустое пространство.
Он будет отмечен знаком минус: -.
!!!(p) {:.numlist}

3.  Граничная линия обозначает ту же физическую ситуацию, что и выпуклая линия, но диаграмма ориентирована таким образом, что видна только одна из двух граней многогранника.
Таким образом, линия отмечает границу между многогранником и фоном.
Он будет отмечен стрелкой: &rarr ;.
Двигаясь по линии от хвоста до точки стрелки, многогранник находится справа, а фон - слева.
!!!(p) {:.numlist}

[Рисунок 17.2](#f0015) показывает маркировку куба с использованием этих соглашений.
Вершина A - это ближний угол куба, а три выходящие из нее прямые - выпуклые.
Линии GD и DF - вогнутые линии, обозначающие стык между кубом и поверхностью, на которой он лежит.
Остальные линии являются граничными линиями, указывающими на то, что между кубом и фоном нет физической связи, но есть другие стороны куба, которые нельзя увидеть.

| []()                                   |
|----------------------------------------|
| ![f17-02](images/chapter17/f17-02.jpg) |
| Рисунок 17.2: Куб, помеченный линией       |

Техника маркировки линий, разработанная в этой главе, основана на простой идее.
Сначала мы перечисляем все возможные вершины и все возможные обозначения для каждой вершины.
Оказывается, в мире трехгранных многоугольников всего четыре разных типа вершин.
Мы называем их вершинами L, Y, W и T. из-за их формы.
Вершины Y и W также называются вилками и стрелками соответственно.
Вершины перечислены на [рисунок 17.3](#f0020).
Каждая вершина накладывает некоторые ограничения на составляющие ее линии.
Например, в вершине W средняя линия может быть помечена знаком + или -, но не стрелкой.

| []()                                          |
|-----------------------------------------------|
| ![f17-03](images/chapter17/f17-03.jpg)        |
| Рисунок 17.3: Возможные вершины и метки       |

Каждая линия соединяет две вершины, поэтому должна удовлетворять обоим ограничениям.
Это предлагает простой алгоритм для разметки диаграммы, основанный на распространении ограничений: сначала пометьте каждую вершину всеми возможными метками для типа вершины.
L вершина имеет шесть возможностей, Y - пять, T - четыре, а W - три.
Затем выберите вершину V.
Рассмотрим соседнюю вершину N (то есть N и V соединены линией).
N также будет иметь набор возможных меток.
Если N и V согласовывают возможные обозначения линии между ними, то мы ничего не выиграем.
Но если пересечение двух множеств возможностей меньше, чем множество возможностей V, то мы нашли ограничение на диаграмме.
Мы соответствующим образом корректируем возможные маркировки N и V.
Каждый раз, когда мы добавляем ограничение в вершину, мы повторяем весь процесс для всех соседних вершин, чтобы дать ограничению возможность распространиться как можно дальше.
Когда каждая вершина была посещена хотя бы один раз и больше нет ограничений для распространения, тогда мы закончили.

[Рисунок 17.4](#f0025) иллюстрирует этот процесс.
Слева начинаем с куба.
Все вершины имеют все возможные обозначения, за исключением того, что мы знаем, что линия GD вогнута (-), что означает, что куб покоится на поверхности.
Это ограничивает вершину D таким образом, что прямая DA должна быть выпуклой (+).
На среднем рисунке ограничение на вершину D распространилось на вершину A, а на правом рисунке оно распространяется на вершину B.
Скоро весь куб будет промаркирован уникальными метками.

| []()                                   |
|----------------------------------------|
| ![f17-04](images/chapter17/f17-04.jpg) |
| Рисунок 17.4: Распространение ограничений   |

Многие диаграммы будут иметь уникальные(однозначно расставленные) метки в процессе распространения ограничений.
Однако некоторые диаграммы неоднозначны.
У них все еще будет несколько меток после завершения распространения ограничения.
В этом случае мы можем искать решение.
Просто выберите неоднозначную вершину, выберите одну из возможных меток для этой вершины и повторите процесс распространения/поиска ограничения.
Продолжайте, пока диаграмма не станет однозначной или противоречивой.

На этом набросок алгоритма маркировки линий завершен.
Теперь мы готовы к реализации программы маркировки.
Её глоссарий находится на [рис. 17.5](#f0030).

| []()                                                |
|-----------------------------------------------------|
| ![f17-05](images/chapter17/f17-05.jpg)              |
| Рисунок 17.5: Глоссарий программы маркировки линий |

*(ed: should be a markdown table)*

Две основные структуры данных - это диаграмма(diagram) и вершина(vertex).
Можно было бы реализовать тип данных для `lines`(линий), но это не обязательно: линии неявно определяются двумя вершинами в их конечных точках.

Диаграмма полностью определяется списком вершин, поэтому для структы  diagram(диаграммы) нужен только один слот.
vertex(Вершина) же - более сложная структура.
Каждая вершина имеет идентифицирующее имя(name) (обычно из одной буквы), тип вершины(type) (L, Y, W или T), список соседних вершин(neighboring) и список возможных меток/марок(possible labelings).
Маркировка(labeling) - это список меток линии.
Например, вершина Y изначально будет иметь список из пяти возможных меток.
Если обнаруживается, что вершина является внутренней частью вогнутого угла, то она будет иметь единственную маркировку (- - -).
Мы даем информацию о типе в слотах вершины, потому что это сложный тип данных.
Синтаксис defstruct таков, что вы не можете указать: тип(type) без предварительного указания значения по умолчанию.
Мы выбрали L в качестве значения по умолчанию для слота типа случайным образом, но обратите внимание, что было бы ошибкой указать `nil` в качестве значения по умолчанию, потому что `nil` не относится к правильному типу.

```lisp
(defstruct diagram "A diagram is a list of vertexes." vertexes)

(defstruct (vertex (:print-function print-vertex))
  (name      nil :type atom)
  (type      'L  :type (member L Y W T))
  (neighbors nil :type list)  ; of vertex
  (labelings nil :type list)) ; of lists of (member + - L R)))))
```

Неоднозначная вершина будет иметь несколько маркировок, в то время как однозначная вершина имеет ровно одну, а вершина без маркировки указывает на невозможную диаграмму.
Изначально мы не знаем, какие вершины какие, поэтому все они начинаются с нескольких возможных обозначений.
Обратите внимание, что маркировка - это список, а не набор: порядок меток имеет значение и соответствует порядку соседних вершин.
Функция possible-labelings дает список всех возможных маркировок для каждого типа вершины.
Мы используем R и L вместо стрелок в качестве меток, потому что ориентация стрелок имеет значение.
R означает, что когда вы путешествуете от вершины к ее соседу, многогранник находится справа, а фоновый объект - слева.
Таким образом, R эквивалентно стрелке, указывающей от вершины.
Буква L как раз наоборот.

```lisp
(defun ambiguous-vertex-p (vertex)
  "A vertex is ambiguous if it has more than one labeling."
  (> (number-of-labelings vertex) 1))

(defun number-of-labelings (vertex)
  (length (vertex-labelings vertex)))

(defun impossible-vertex-p (vertex)
  "A vertex is impossible if it has no labeling."
  (null (vertex-labelings vertex)))

(defun impossible-diagram-p (diagram)
  "An impossible diagram is one with an impossible vertex."
  (some #'impossible-vertex-p (diagram-vertexes diagram)))

(defun possible-labelings (vertex-type)
  "The list of possible labelings for a given vertex type."
  ;; In these labelings, R means an arrow pointing away from
  ;; the vertex, L means an arrow pointing towards it.
  (case vertex-type
    ((L) '((R L)   (L R)   (+ R)   (L +)   (- L)   (R -)))
    ((Y) '((+ + +) (- - -) (L R -) (- L R) (R - L)))
    ((T) '((R L +) (R L -) (R L L) (R L R)))
    ((W) '((L R +) (- - +) (+ + -)))))
```

## 17.2 Комбинирование ограничений и поиска

Основная функция `print-labelings` принимает диаграмму в качестве входных данных, уменьшает количество меток на каждой вершине путем распространения ограничений, а затем выполняет поиск всех согласованных интерпретаций.
Вывод печатается до и после каждого шага.

```lisp
(defun print-labelings (diagram)
  "Label the diagram by propagating constraints and then
  searching for solutions if necessary.  Print results."
  (show-diagram diagram "~&The initial diagram is:")
  (every #'propagate-constraints (diagram-vertexes diagram))
  (show-diagram diagram
                "~2&After constraint propagation the diagram is:")
  (let* ((solutions (if (impossible-diagram-p diagram)
                        nil
                        (search-solutions diagram)))
         (n (length solutions)))
    (unless (= n 1)
      (format t "~2&There are ~r solution~:p:" n)
      (mapc #'show-diagram solutions)))
  (values))
```

Функция `propagate-constraints`(распространять-ограничения) берет вершину и рассматривает ограничения, налагаемые соседними вершинами, чтобы получить список всех `propagate-constraints`(согласованных маркировок) для вершины.
Если количество согласованных маркировок меньше, чем количество до того, как мы начали, тогда ограничения соседей повлияли на эту вершину, поэтому мы распространяем вновь найденные ограничения на эту вершину обратно каждому соседу.
Функция возвращает nil и, таким образом, немедленно останавливает распространение, если есть невозможная вершина.
В противном случае распространение продолжается до тех пор, пока не останется никаких изменений в маркировке.

Весь алгоритм распространения запускается вызовом `every in print-labelings,` который распространяет ограничения из каждой вершины диаграммы.
Но не очевидно, что это все, что требуется.
Разве после однократного распространения из каждой вершины не может быть другой вершины, которую нужно изменить?
Единственная вершина, которая может нуждаться в перемаркировке, - это та, у которой был изменен сосед с момента ее последнего обновления.
Но любую такую вершину посетило бы `propagate-constraint` (распространение ограничения), поскольку мы распространяемся на всех соседей.
Таким образом, за один проход по вершинам в сочетании с рекурсивными вызовами будут найдены и применены все возможные ограничения.

Следующий вопрос, который стоит задать, - гарантировано ли завершение работы алгоритма.
Ясно, что это так, потому что `propagate-constraints`(распространение ограничения) может производить рекурсивные вызовы только тогда, когда удаляет метку.
Но так как изначально существует конечное число разметок (не более шести на вершину), должно быть конечное количество вызовов для `propagate-constraints.`

```lisp
(defun propagate-constraints (vertex)
  "Reduce the labelings on vertex by considering neighbors.
  If we can reduce, propagate the constraints to each neighbor."
  ;; Return nil only when the constraints lead to an impossibility
  (let ((old-num (number-of-labelings vertex)))
    (setf (vertex-labelings vertex) (consistent-labelings vertex))
    (unless (impossible-vertex-p vertex)
      (when (< (number-of-labelings vertex) old-num)
        (every #'propagate-constraints (vertex-neighbors vertex)))
      t)))
```

Вершина передается функции `consistent-labelings`(согласовать метки).
Она получает все метки для этой вершины от соседних вершин, собирая их в `neighbor-labels`(метки соседей).
Затем она проверяет все метки на текущей вершине, оставляя только те, которые согласуются со всеми ограничениями соседей.
Вспомогательная функция `labels-for` находит метки для конкретного соседа у вершины, и `reverse-label` учитывает тот факт, что метки L и R интерпретируются относительно вершины, на которую они указывают.

```lisp
(defun consistent-labelings (vertex)
  "Return the set of labelings that are consistent with neighbors."
  (let ((neighbor-labels
          (mapcar #'(lambda (neighbor) (labels-for neighbor vertex))
                  (vertex-neighbors vertex))))
    ;; Eliminate labelings that don't have all lines consistent
    ;; with the corresponding line's label from the neighbor.
    ;; Account for the L-R mismatch with reverse-label.
    (find-all-if
      #'(lambda (labeling)
          (every #'member (mapcar #'reverse-label labeling)
                 neighbor-labels))
      (vertex-labelings vertex))))
```

Распространения ограничений часто бывает достаточно, чтобы получить уникальную интерпретацию.
Но иногда диаграмма все же недостаточно ограничена, и нам придется выполнить поиск для решения.
Функция `search-solutions` сначала проверяет, является ли диаграмма неоднозначной, проверяя, есть ли у нее неоднозначная вершина v.
Если диаграмма недвусмысленна(т.е однозначна), то это решение, и мы его возвращаем (в виде списка, поскольку `since search-solutions` предназначен для возврата списка всех решений).
В противном случае для каждой из возможных меток для неоднозначной вершины мы создаем новую копию диаграммы и устанавливаем метку вершины v в копии в одну из возможных меток.
Фактически, мы предполагаем, что маркировка правильная.
Мы вызываем `propagate-constraints;` если она возвращает сбой(fails), значит, мы ошиблись, поэтому нет никаких решений с такой маркировкой(метка установлена не правильно).
Но если она возвращает успех, мы рекурсивно вызываем `search-solutions`, чтобы получить список решений, сгенерированных с этой меткой.

```lisp
(defun search-solutions (diagram)
  "Try all labelings for one ambiguous vertex, and propagate."
  ;; If there is no ambiguous vertex, return the diagram.
  ;; If there is one, make copies of the diagram trying each of
  ;; the possible labelings.  Propagate constraints and append
  ;; all the solutions together.
  (let ((v (find-if #'ambiguous-vertex-p
                    (diagram-vertexes diagram))))
    (if (null v)
        (list diagram)
        (mapcan
          #'(lambda (v-labeling)
              (let* ((diagram2 (make-copy-diagram diagram))
                     (v2 (find-vertex (vertex-name v) diagram2)))
                (setf (vertex-labelings v2) (list v-labeling))
                (if (propagate-constraints v2)
                    (search-solutions diagram2)
                    nil)))
          (vertex-labelings v)))))
```

Вот и все, что касается алгоритма; остались лишь вспомогательные функции.
Вот три из них:

```lisp
(defun labels-for (vertex from)
  "Return all the labels for the line going to vertex."
  (let ((pos (position from (vertex-neighbors vertex))))
    (mapcar #'(lambda (labeling) (nth pos labeling))
            (vertex-labelings vertex))))

(defun reverse-label (label)
  "Account for the fact that one vertex's right is another's left."
  (case label (L 'R) (R 'L) (otherwise label)))

(defun find-vertex (name diagram)
  "Find the vertex in the given diagram with the given name."
  (find name (diagram-vertexes diagram) :key #'vertex-name))
```

Вот функции печати.
`print-vertex` печатает вершину в краткой форме.
Она подчиняется соглашению `print` о возврате первого аргумента.
Функции `show-vertex` и `show-diagram` печатают более подробные формы.
Они подчиняются соглашению о функциях, подобных `describe`, т.е. о том, что они вообще не возвращают никаких значений.

```lisp
(defun print-vertex (vertex stream depth)
  "Print a vertex in the short form."
  (declare (ignore depth))
  (format stream "~a/~d" (vertex-name vertex)
          (number-of-labelings vertex))
  vertex)

(defun show-vertex (vertex &optional (stream t))
  "Print a vertex in a long form, on a new line."
  (format stream "~&   ~a ~d:" vertex (vertex-type vertex))
  (mapc #'(lambda (neighbor labels)
            (format stream " ~a~a=[~{~a~}]" (vertex-name vertex)
                    (vertex-name neighbor) labels))
        (vertex-neighbors vertex)
        (matrix-transpose (vertex-labelings vertex)))
  (values))

(defun show-diagram (diagram &optional (title "~2&Diagram:")
                             (stream t))
  "Print a diagram in a long form.  Include a title."
  (format stream title)
  (mapc #'show-vertex (diagram-vertexes diagram))
  (let ((n (reduce #'* (mapcar #'number-of-labelings
                               (diagram-vertexes diagram)))))
  (when (> n 1)
    (format stream "~&For ~:d interpretation~:p." n))
  (values)))
```

Обратите внимание, что `matrix-transpose` вызывается `show-vertex`, чтобы перевернуть матрицу меток на свою сторону.
Это работает так:

```lisp
(possible-labelings 'Y)
((+ + +)
  (- - -)
  (L R -)
  (- L R)
  (R - L))
(matrix-transpose (possible-labelings 'Y))
((+ - L - R)
  (+ - R L -)
  (+ - - R L))
```

Реализация `matrix-transpose` на удивление лаконична.
Это старый Lisp-трюк, и его стоит понять:

```lisp
(defun matrix-transpose (matrix)
  "Turn a matrix on its side."
  (if matrix (apply #'mapcar #'list matrix)))
```

Оставшийся код связан с созданием диаграмм.
Нам нужен удобный способ задания диаграмм.
Один из способов - программа распознавания линий, работающая на оцифрованном вводе с камеры или растрового изображения.
Другая возможность - интерактивная программа рисования, использующая мышь и отображение растрового изображения.
Но поскольку пока нет стандарта Common Lisp для взаимодействия с такими устройствами, нам придется довольствоваться текстовым описанием.
Макрос `defdiagram` определяет диаграмму и дает ей имя.
За именем следует список описаний вершин.
Каждое описание представляет собой список, состоящий из имени вершины, типа вершины (Y, A, L или T) и имен соседних вершин.
Вот снова описание `defdiagram` для куба, показанного на [рис. 17.6](#f0035).

| []()                                   |
|----------------------------------------|
| ![f17-06](images/chapter17/f17-06.jpg) |
| Рисунок 17.6: Куб                      |

```lisp
(defdiagram cube
  (a Y b c d)
  (b W g e a)
  (c W e f a)
  (d W f g a)
  (e L c b)
  (f L d c)
  (g L b d))
```

Макрос `defdiagram` вызывает `construct-diagram` для выполнения реальной работы.
Было бы возможно преобразовать `defdiagram` в `defvar`, сделав имена специальными переменными.
Но тогда ответственность за создание копий такой переменной перед ее передачей деструктивной функции будет лежать на пользователе.
Вместо этого я использую `put-diagram` и `diagram` для размещения и получения диаграмм в таблице, `diagram` извлекает названную диаграмму и делает ее копию.
Таким образом, пользователь не может повредить исходные диаграммы, хранящиеся в таблице.
Другой возможностью было бы расширение `defdiagram` в определение функции для `name`, возвращающей копию диаграммы.
Я решил хранить пространство имен диаграммы отдельно от пространства имен функций, поскольку имена вроде `cube` имеют смысл в обоих пространствах.

```lisp
(defmacro defdiagram (name &rest vertex-descriptors)
  "Define a diagram.  A copy can be gotten by (diagram name)."
  `(put-diagram ',name (construct-diagram
                         (check-diagram ',vertex-descriptors))))

(let ((diagrams (make-hash-table)))

(defun diagram (name)
  "Get a fresh copy of the diagram with this name."
  (make-copy-diagram (gethash name diagrams)))

(defun put-diagram (name diagram)
  "Store a diagram under a name."
  (setf (gethash name diagrams) diagram)
  name))
```

Функция `construct-diagram` переводит описание каждой вершины, используя `construct-vertex`, а затем заполняет соседей каждой вершины.

```lisp
(defun construct-diagram (vertex-descriptors)
  "Build a new diagram from a set of vertex descriptor."
  (let ((diagram (make-diagram)))
    ;; Put in the vertexes
    (setf (diagram-vertexes diagram)
          (mapcar #'construct-vertex vertex-descriptors))
    ;; Put in the neighbors for each vertex
    (dolist (v-d vertex-descriptors)
      (setf (vertex-neighbors (find-vertex (first v-d) diagram))
            (mapcar #'(lambda (neighbor)
                        (find-vertex neighbor diagram))
                    (v-d-neighbors v-d))))
    diagram))

(defun construct-vertex (vertex-descriptor)
  "Build the vertex corresponding to the descriptor."
  ;; Descriptors are like: (x L y z)
  (make-vertex
    :name (first vertex-descriptor)
    :type (second vertex-descriptor)
    :labelings (possible-labelings (second vertex-descriptor))))

(defun v-d-neighbors (vertex-descriptor)
  "The neighboring vertex names in a vertex descriptor."
  (rest (rest vertex-descriptor)))
```

`defstruct` для `diagram` автоматически создает функцию `copy-diagram`, но она просто копирует каждое поле, не копируя содержимое каждого поля.
Таким образом, нам нужно `make-copy-diagram` для создания копии, не имеющей общей структуры с оригиналом.

```lisp
(defun make-copy-diagram (diagram)
  "Make a copy of a diagram, preserving connectivity."
  (let* ((new (make-diagram
                :vertexes (mapcar #'copy-vertex
                                  (diagram-vertexes diagram)))))
    ;; Put in the neighbors for each vertex
    (dolist (v (diagram-vertexes new))
      (setf (vertex-neighbors v)
            (mapcar #'(lambda (neighbor)
                        (find-vertex (vertex-name neighbor) new))
                    (vertex-neighbors v))))
    new))
```

## 17.3 Маркировка Диаграм

Теперь мы готовы попробовать маркировку диаграмм.
Сначала куб:

```lisp
> (print-labelings (diagram 'cube))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC=[RL+L-R] EB=[LRR+L-]
  F/6 L: FD=[RL+L-R] FC=[LRR+L-]
  G/6 L: GB=[RL+L-R] GD=[LRR+L-]
```

`For 29,160 interpr`e`tations.`

```lisp
After constraint propagation the diagram is:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/2 W: BG=[L-] BE=[R-] BA=[++]
  C/2 W: CE=[L-] CF=[R-] CA=[++]
  D/2 W: DF=[L-] DG=[R-] DA=[++]
  E/3 L: EC=[R-R] EB=[LL-]
  F/3 L: FD=[R-R] FC=[LL-]
  G/3 L: GB=[R-R] GD=[LL-]
```

`For 216 interpr`e`tations.`

```lisp
There are four solutions:
Diagram:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/1 W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[L] CF=[R] CA=[+]
  D/1 W: DF=[L] DG=[R] DA=[+]
  E/l L: EC=[R] EB=[L]
  F/1 L: FD=[R] FC=[L]
  G/1 L: GB=[R] GD=[L]
  Diagram:
  A/1 Y: AD=[+] AC=[+] AD=[+]
  B/1 W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[L] CF=[R] CA=[+]
  D/1 W: DF=[-] DG=[-] DA=[+]
  E/l L: EC=[R] EB=[L]
  F/1 L: FD=[-] FC=[L]
  G/1 L: GB=[R] GD=[-]
Diagram:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/1 W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[-] CF=[-] CA=[+]
  D/1 W: DF=[L] DG=[R] DA=[+]
  E/l L: EC=[-] EB=[L]
  F/1 L: FD=[R] FC=[-]
  G/1 L: GB=[R] GD=[L]
Diagram:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/1 W: BG=[-] BE=[-] BA=[+]
  C/1 W: CE=[L] CF=[R] CA=[+]
  D/1 W: DF=[L] DG=[R] DA=[+]
  E/1 L: EC=[R] EB=[-]
  F/1 L: FD=[R] FC=[L]
  G/1 L: GB=[-] GD=[L]
```

Четыре интерпретации соответствуют, соответственно, случаям, когда куб свободно плавает, прикреплен к полу (GD и DF = -), прикреплен к стене справа (EC и CF = -) или прикреплен к стене на слева (BG и BE = -).
Они показаны на [рисунок 17.7](#f0040).
Было бы неплохо, если бы мы могли предоставить информацию о том, где прикреплен куб, и посмотреть, сможем ли мы получить уникальную интерпретацию.
Функция `ground`(земля) принимает диаграмму и модифицирует ее, делая одну или несколько линий линиями примыкающими к земле - линиями, имеющими вогнутую (-) метку, соответствующую месту соединения с землей.

| []()                                          |
|-----------------------------------------------|
| ![f17-07](images/chapter17/f17-07.jpg)        |
| Рисунок 17.7: Четыре интерпретации куба |

```lisp
(defun ground (diagram vertex-a vertex-b)
  "Attach the line between the two vertexes to the ground.
  That is, label the line with a -"
  (let* ((A (find-vertex vertex-a diagram))
         (B (find-vertex vertex-b diagram))
         (i (position B (vertex-neighbors A))))
    (assert (not (null i)))
    (setf (vertex-labelings A)
          (find-all-if #'(lambda (l) (eq (nth i l) '-))
                     (vertex-labelings A)))
    diagram))
```

Мы можем увидеть, как это работает на кубе:

```lisp
> (print-labelings (ground (diagram 'cube) 'g 'd))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC=[RL+L-R] EB[LRR+L-]
  F/6 L: FD=[RL+L-R] FC=[LRR+L-]
  G/1 L: GB=[R] GD=[-]
```

`For 4,860 interpr`e`tations.`

```lisp
After constraint propagation the diagram is:
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/l W: BG=[L] BE=[R] BA=[+]
  C/l W: CE=[L] CF=[R] CA=[C  +]
  D/l W: DF=[-] DG=[-] DA=[+]
  E/l L: EC=[R] EB=[L]
  F/1 L: FD=[-] FC=[L]
  G/1 L: GB=[R] GD=[-]
```

Обратите внимание, что пользователю нужно было указать только одну из двух линий примыкающих к земле, GD.
Программа обнаружила, что DF тоже заземлен.
Точно так же при программировании `ground-line`(наземной линии) нам нужно было обновить только одну из вершин.
Остальное делается путем распространения ограничений.

Следующий пример дает те же четыре интерпретации в том же порядке (свободно плавающий, прикрепленный снизу, прикрепленный справа и прикрепленный слева) при интерпретации без земли.
Заземленная версия дает уникальное решение, показанное в следующих выходных данных и на [рис. 17.9](#f0050).

| []()                                   |
|----------------------------------------|
| ![f17-08](images/chapter17/f17-08.jpg) |
| Рисунок 17.8: Куб на тарелке           |

| []()                                   |
|----------------------------------------|
| ![f17-09](images/chapter17/f17-09.jpg) |
| Рисунок 17.9: Промаркированный куб на тарелке   |

```lisp
(defdiagram cube-on-plate
  (a Y b c d)
  (b W g e a)
  (c W e f a)
  (d W f g a)
  (e L c b)
  (f Y d c i)
  (g Y b d h)
  (h W l g j)
  (i W f m j)
  (j Y h i k)
  (k W m l j)
  (l L h k)
  (m L k i))
> (print-labelings (ground (diagram 'cube-on-plate) 'k 'm))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC=[RL+L-R] EB=[LRR+L-]
  F/5 Y: FD=C+-L-R] FC=[+-RL-] FI=[+--RL]
  G/5 Y: GB=[+-L-R] GD=[+-RL-] GH=[+--RL]
  H/3 W: HL=[L-+] HG=[R-+] HJ=[++-]
  I/3 W: IF=[L-+] IM=[R-+] IJ=[++-]
  J/5 Y: JH=[+-L-R] JI=[+-RL-] JK=[+--RL]
  K/1 W: KM=[-] KL=[-] KJ=[+]
  L/6 L: LH=[RL+L-R] LK=[LRR+L-]
  M/6 L: MK=[RL+L-R] MI=[LRR+L-]
```

`For 32.805.000 interpr`e`tations.`

```lisp
After constraint propagation the diagram is
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/2 W: BG=[L-] BE=[R-] BA=[++]
  C/2 W: CE=[L-] CF=[R-] CA=[++]
  D/2 W: DF=[L-] DG=[R-] DA=[++]
  E/1 L: EC=[R] EB=[L]
  F/1 Y: FD=[-] FC=[L] FI=[R]
  G/1 Y: GB=[R] GD=[-] GH=[L]
  H/1 W: HL=[L] HG=[R] HJ=[+]
  I/1 W: IF=[L] IM=[R] IJ=[+]
  J/1 Y: JH=[+] JI=[+] JK=[+]
  K/1 W: KM=[-] KL=[-] KJ=[+]
  L/1 L: LH=[R] LK=[-]
  M/1 L: MK=[-] MI=[L]
```

Интересно опробовать алгоритм на "невозможной" диаграмме.
Оказывается, алгоритм не находит никакой интерпретации этой хорошо известной иллюзии:

```lisp
(defdiagram poiuyt
  (a L b g)
  (b L j a)
  (c L d l)
  (d L h c)
  (e L f i)
  (f L k e)
  (g L a l)
  (h L l d)
  (i L e k)
  (j L k b)
  (k W j i f)
  (l W h g c))
> (print-1 abel ings (diagram 'poiuyt))
The initial diagram is:
  A/6 L: AB=[RL+L-R] AG=[LRR+L-]
  B/6 L: BJ=[RL+L-R] BA=[LRR+L-]
  C/6 L: CD=[RL+L-R] CL=[LRR+L-]
  D/6 L: DH=[RL+L-R] DC=[LRR+L-]
  E/6 L: EF=[RL+L-R] EI=[LRR+L-]
  F/6 L: FK=[RL+L-R] FE=[LRR+L-]
  G/6 L: GA=[RL+L-R] GL=[LRR+L-]
  H/6 L: HL=[RL+L-R] HD=[LRR+L-]
  I/6 L: IE=[RL+L-R] IK=[LRR+L-]
  J/6 L: JK=[RL+L-R] JB=[LRR+L-]
  K/3 W: KJ=[L-+] KI=[R-+] KF=[++-]
  L/3 W: LH=[L-+] LG=[R-+] LC=[++-]
```

`For 544,195.584 interpr`e`tations.`

```lisp
After constraint propagation the diagram is:
  A/5 L: AB=[RL+-R] AG=[LRRL-]
  B/5 L: BJ=[RLL-R] BA=[LR+L-]
  C/2 L: CD=[LR] CL=[+-]
  D/3 L: DH=[RL-] DC=[LRL]
  E/3 L: EF=[RLR] EI=[LR-]
  F/2 L: FK=[+-] FE=[RL]
  G/4 L: GA=[RL-R] GL=[L+L-]
  H/4 L: HL=[R+-R] HD=[LRL-]
  I/4 L: IE=[RL-R] IK=[L+L-]
  J/4 L: JK=[R+-R] JB=[LRL-]
  K/3 W: KJ=[L-+] KI=[R-+] KF=[++-]
  L/3 W: LH=[L-+] LG=[R-+] LC=[++-]
```

`For 2,073,600 interpr`e`tations.`

`There are z`e`ro solutions:`

Теперь попробуем более сложную диаграмму(схему):

```lisp
(defdiagram tower
  (a Y b c d)    (n L q o)
  (b W g e a)    (o W y j n)
  (c W e f a)    (P L r i)
  (d W f g a)    (q W n s w)
  (e L c b)      (r W s p x)
  (f Y d c i)    (s L r q)
  (g Y b d h)    (t W w x z)
  (h W l g J)    (u W x y z)
  (i W f m p)    (v W y w z)
  (j Y h o k)    (w Y t v q)
  (k W m l j)    (x Y r u t)
  (l L h k)      (y Y v u o)
  (m L k i)      (z Y t u v))
> (print-labelings (ground (diagram 'tower) 'l 'k))
The initial diagram is:
  A/5 Y: AB=[+-L-R] AC=[+-RL-] AD=[+--RL]
  B/3 W: BG=[L-+] BE=[R-+] BA=[++-]
  C/3 W: CE=[L-+] CF=[R-+] CA=[++-]
  D/3 W: DF=[L-+] DG=[R-+] DA=[++-]
  E/6 L: EC[RL+L-R] EB=[LRR+L-]
  F/5 Y: FD=[+-L-R] FC=[+-RL-] FI=[+--RL]
  G/5 Y: GB=[+-L-R] GD=[+-RL-] GH=[+--RL]
  H/3 W: HL=[L-+] HG=[R-+] HJ=[++-]
  I/3 W: IF=[L-+] IM=[R-+] IP=[++-]
  J/5 Y: JH=[+-L-R] JO=[+-RL-] JK=[+--RL]
  K/3 W: KM=[L-+] KL=[R-+] KJ=[++-]
  L/1 L: LH=[R] LK=[-]
  M/6 L: MK=[RL+L-R] MI=[LRR+L-]
  N/6 L: NQ=[RL+L-R] NO=[LRR+L-]
  O/3 W: OY=[L-+] OJ=[R-+] ON=[++-]
  P/6 L: PR=[RL+L-R] PI=[LRR+L-]
  Q/3 W: QN=[L-+] QS=[R-+] QW=[++-]
  R/3 W: RS=[L-+] RP=[R-+] RX=[++-]
  S/6 L: SR=[RL+L-R] SQ=[LRR+L-]
```

`  T/3 W:` TW=[L-+] `TX=[R-+] TZ=[++-]`

```lisp
  U/3 W: UX=[L-+] UY=[R-+] UZ=[++-]
  V/3 W: VY=[L-+] VW=[R-+] VZ=[++-]
  W/5 Y: WT=[+-L-R] WV=[+-RL-] WQ=[+--RL]
  X/5 Y: XR=[+-L-R] XU=[+-RL-] XT=[+--RL]
  Y/5 Y: YV=[+-L-R] YU=[+-RL-] YO=[+--RL]
  Z/5 Y: ZT=[+-L-R] ZU=[+-RL-] ZV=[+--RL]
For 1,614,252,037,500,000 interpretations.
```

После распространения ограничения диаграмма выглядит так:

```lisp
  A/1 Y: AB=[+] AC=[+] AD=[+]
  B/l W: BG=[L] BE=[R] BA=[+]
  C/1 W: CE=[L] CF=[R] CA=[+]
  D/l W: DF=[-] DG=[-] DA=[+]
  E/1 L: EC=[R] EB=[L]
  F/1 Y: FD=[-] FC=[L] FI=[R]
  G/1 Y: GB=[R] GD=[-]GH=[L]
  H/1 W: HL=[L] HG=[R] HJ=[+]
  I/1 W: IF=[L] IM=[R] IP=[+]
  J/l Y: JH=[+] JO=[+] JK=[+]
  K/l W: KM=[-] KL=[-] KJ=[+]
  L/l L: LH=[R] LK=[-]
  M/1 L: MK=[-] MI=[L]
  N/l L: NQ=[R] NO[-]
  O/l W: OY=[+] OJ=[+] ON=[-]
  P/l L: PR=[L] PI=[+]
  Q/1 W: QN=[L] QS=[R] QW=[+]
  R/1 W: RS=[L] RP=[R] RX=[+]
  S/1 L: SR=[R] SQ=[L]
  T/1 W: TW=[+] TX=[+] TZ=[-]
  U/1 W: UX=[+] UY=[+] UZ=[-]
  V/l W: VY=[+] VW=[+] VZ=[-]
  W/l Y: WT=[+] WV=[+] WQ=[+]
  X/1 Y: XR=[+] XU=[+] XT=[+]
  Y/1 Y: YV=[+] YU=[+] YO=[+]
  Z/l Y: ZT=[-] ZU=[-] ZV=[-]
```

Мы видим, что алгоритм смог прийти к единственной интерпретации.
Более того, даже несмотря на то, что было большое количество возможностей - более квадриллиона, - вычисления выполняются довольно быстро.
Большая часть времени уходит на печать, поэтому для получения точных результатов мы определяем функцию для поиска решений, ничего не печатая:

```lisp
(defun find-labelings (diagram)
  "Return a list of all consistent labelings of the diagram."
  (every #'propagate-constraints (diagram-vertexes diagram))
  (search-solutions diagram))
```

Когда мы измеряем время применения `find-labelings` к расположенной на земле башне и poiuyt, мы обнаруживаем, что расчет башни занимает 0,11 секунды, а poiuyt 21 секунду.
Это более чем в 180 раз дольше, даже несмотря на то, что у poiuyt вдвое меньше вершин и всего около полумиллиона интерпретаций по сравнению с квадриллионом башни.
Обработка poiuyt занимает много времени из-за небольшого количества локальных ограничений, поэтому нарушения обнаруживаются только при одновременном рассмотрении нескольких широко разделенных частей фигуры.
Интересно, что тот же факт, из-за которого обработка poiuyt занимает больше времени, также ответственен за его восприятие как иллюзию.

## 17.4 Проверка диаграмм на наличие ошибок

В этом разделе рассматривается еще один пример и рассматривается, что делать, если во входных данных есть очевидные ошибки.
Пример взят из книги Чарняка и Макдермотта *Introduction to Artificial Intelligence*(Введение в искусственный интеллект), стр. 138, и показан на [рис. 17.12](#f0065).

| []()                                          |
|-----------------------------------------------|
| ![f17-10](images/chapter17/f17-10.jpg)        |
| Рисунок 17.10: Невозможная фигура (Poiuyt) |

| []()                                   |
|----------------------------------------|
| ![f17-11](images/chapter17/f17-11.jpg) |
| Рисунок 17.11: Башня                  |

| []()                                   |
| ---------------------------------------|
| ![f17-12](images/chapter17/f17-12.jpg) |
| Рисунок 17.12: Диаграмма(Схема) арки       |

```lisp
(defdiagram arch
  (a W e b c)    (p L o q)
  (b L d a)      (q T P i r)
  (c Y a d g)    (r T j s q)
  (d Y c b m)    (s L r t)
  (e L a f)      (t W v s k)
  (f T e g n)    (u L t l)
  (g W h f c)    (v L t l)
  (h T g i o)    (w W x l y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T J l t)    (z W 3 x y)
  (l T k m v)    (l T n o w)
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
```

К сожалению, выполнение этого примера не дает согласованных интерпретаций после распространения ограничения.
Это кажется неправильным.
Хуже того, когда мы пытаемся заземлить(приложить к земле) диаграмму по линии XZ и вызывать для неё `print-labelings`, мы получаем следующую ошибку:

```lisp
>>>ERROR: The first argument to NTH was of the wrong type.
```

`The function expected a fixnum >= z`e`ro.`

`While in the function LABELS-FOR`<= `CONSISTENT-LABELINGS`

```lisp
Debugger entered while in the following function:
```

`LABELS-FOR (P.C.
= 23)`

```lisp
  Arg 0 (VERTEX): U/6
  Arg 1 (FROM): 4/4
```

Что пошло не так?
Хорошее предположение - что диаграмма непоследовательна - где-то была допущена ошибка при расшифровке диаграммы.
Может быть, диаграмма на самом деле невозможна, как и poiuyt.
Но это маловероятно, поскольку нам легко дать интуитивную интерпретацию.
Нам нужно отладить диаграмму, и было бы неплохо обработать ошибку более изящно.

Одно свойство диаграммы, которое легко проверить, - это то, что каждая линия должна упоминаться дважды.
Если между вершинами A и B есть линия, в дескрипторах вершин должно быть две записи следующего вида:

```lisp
(A ? ... B ...)
(B ? ... A ...)
```

Здесь символ "?" означает, что нас не беспокоит тип вершин, только наличие линии в двух местах.
Следующий код выполняет эту проверку при определении диаграммы.
Он также проверяет, что каждая вершина относится к одному из четырех допустимых типов и имеет правильное количество соседей.

```lisp
(defmacro defdiagram (name &rest vertex-descriptors)
```

`  "Define a diagram.
A copy can be gotten by (diagram name)."`

```lisp
  '(put-diagram '.name (construct-diagram
                    (check-diagram ',vertex-descriptors))))

(defun check-diagram (vertex-descriptors)
  "Check if the diagram description appears consistent."
  (let ((errors 0))
    (dolist (v-d vertex-descriptors)
      ;; v-d is like: (a Y b c d)
      (let ((A (first v-d))
            (v-type (second v-d)))
        ;; Check that the number of neighbors is right for
        ;; the vertex type (and that the vertex type is legal)
        (when (/= (length (v-d-neighbors v-d))
                  (case v-type ((W Y T) 3) ((L) 2) (t -1)))
          (warn "Illegal type/neighbor combo: ~a" v-d)
          (incf errors))
        ;; Check that each neighbor B is connected to
        ;; this vertex, A, exactly once
        (dolist (B (v-d-neighbors v-d))
          (when (/= 1 (count-if
                        #'(lambda (v-d2)
                            (and (eql (first v-d2) B)
                                 (member A (v-d-neighbors v-d2))))
                        vertex-descriptors))
            (warn "Inconsistent vertex: ~a-~a" A B)
            (incf errors)))))
    (when (> errors 0)
      (error "Inconsistent diagram.  ~d total error~:p."
             errors)))
  vertex-descriptors)
```

Теперь попробуем еще раз арку:

```lisp
(defdiagram arch
  (a W e b c)    (p L o q)
  (b L d a)      (q T p i r)
  (c Y a d g)    (r T j s q)
  (d Y c b m)    (s L r t)
  (e L a f)      (t W v s k)
  (f T e g n)    (u L t l)
  (g W h f c)    (v L 2 4)
  (h T g i o)    (w W x l y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T j l t)    (z W 3 x y)
  (l T k m v)    (1 T n o w)
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
Warning: Inconsistent vertex: T-V
Warning: Inconsistent vertex: U-T
Warning: Inconsistent vertex: U-L
Warning: Inconsistent vertex: L-V
Warning: Inconsistent vertex: 4-U
Warning: Inconsistent vertex: 4-L
```

`>>ERROR: Inconsistent diagram.
6 total errors.`

`defdiagram` выполнял расшифровку из промаркированной вручную диаграммы, и похоже, что эта транскрипция стала жертвой одной из старейших проблем математической записи: путать "u" с "v". Другая проблема заключалась в том, чтобы рассматривать линию U-L как единую линию, когда на самом деле она разбита на два сегмента, U-4 и 4-L.
Исправление этих ошибок дает диаграмму:

```lisp
(defdiagram arch
  (a W e b c)    (P L o q)
  (b L d a)      (q T P i r)
  (c Y a d g)    (r T j s q)
  (d Y c b m)    (s L r t)
  (e L a f)      (t W u s k)        *;t-u not t-v*
  (f T e g n)    (u L t 4)          *;u-4 not u-l*
  (g W h f c)    (v L 2 4)
  (h T g i o)    (w W x l y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T J l t)    (z W 3 x y)
  (l T k m 4)    (1 T n o w)          *;l-4 not l-v*
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
```

На этот раз `check-diagram` ошибок не обнаружила, но запуск `print-labelings` еще раз не дает решения.
`Чтобы` получить больше информации о том, какие ограничения применяются, `я` изменил `progate-constraints`, чтобы распечатать некоторую информацию:

```lisp
(defun propagate-constraints (vertex)
  "Reduce the number of labelings on vertex by considering neighbors.
  If we can reduce, propagate the new constraint to each neighbor."
  :: Return nil only when the constraints lead to an impossibility
  (let ((old-num (number-of-labelings vertex)))
    (setf (vertex-labelings vertex) (consistent-labelings vertex))
    (unless (impossible-vertex-p vertex)
      (when (< (number-of-labelings vertex) old-num)
        (format t "~&; ~a: ~14a ~a" vertex ;***
                (vertex-neighbors vertex) ;***
                (vertex-labelings vertex)) ;***
        (every #'propagate-constraints (vertex-neighbors vertex)))
      vertex)))
```

Повторный запуск проблемы дает следующую трассу:

```lisp
> (print-labelings (ground (diagram 'arch) 'x 'z))
The initial diagram is:
  A/3 W: AE=[L-+] AB-CR-+] AC=[++-]
  P/6 L: P0=[RL+L-R] PQ=[LRR+L-]
  B/6 L: BD=[RL+L-R] BA=[LRR+L-]
  Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+-LR]
  C/5 Y: CA=[+-L-R] CD=[+-RL-] CG=[+--RL]
  R/4 T: RJ=[RRRR] RS=[LLLL] RQ=[+-LR]
  D/5 Y: DC=[+-L-R] DB=[+-RL-] DM=[+--RL]
  S/6 L: SR=[RL+L-R] ST=[LRR+L-]
  S/6 L: EA=[RL+L-R] EF=[LRR+L-]
  T/3 W: TU=[L-+] TS=[R-+] TK=[++-]
  F/4 T: FE=[RRRR] FG=[LLLL] FN=[+-LR]
  U/6 L: UT=[RL+L-R] U4=[LRR+L-]
  G/3 W: GH=[L-+] GF=[R-+] GC=[++-]
  V/6 L: V2=[RL+L-R] V4=[LRR+L-]
  H/4 T: HG=[RRRR] HI=[LLLL] Ho=[+-LR]
  W/3 W: WX=[L-+] W1=[R-+] WY=[++-]
  I/4 T: IH=[RRRR] IJ=[LLLL] IQ=[+-LR]
  X/1 L: XW=[R] XZ=[-]
  J/4 T: JI=[RRRR] JK=[LLLL] JR=[+-LR]
  Y/5 Y: YW=[+-L-R] Y2=[+-RL-] YZ=[+--RL]
  K/4 T: KJ=[RRRR] KL=[LLLL] KT=[+-LR]
  Z/3 W: Z3=[L-+] ZX=[R-+] ZY=[++-]
  L/4 T: LK=[RRRR] LM=[LLLL] L4=[+-LR]
  1/4 T: 1N=[RRRR] 10=[LLLL] 1 W=[+-LR]
  M/6 L: ML=[RL+L-R] MD=[LRR+L-]
  2/3 W: 2 V=[L-+] 23=[R-+] 2Y=[++-]
  N/6 L: NF=[RL+L-R] N1=[LRR+L-]
  3/6 L: 3Z=[RL+L-R] 32=[LRR+L-]
  0/3 W: 0P=[L-+] 01=[R-+] 0H=[++-]
  4/4 T: 4U=[RRRR] 4 L=[LLLL] 4 V=[+-LR]
For 2,888, 816, 545.234, 944,000 interpretations
: P/2: (0/3 Q/4)        ((R L) (- L))
: 0/1: (P/2 1/4 H/4)    ((L R +))
: P/1: (0/1 Q/4)        ((R L))
: 1/3: (N/6 0/1 W/3)    ((R L +) (R L -) (R L L))
: N/2: (F/4 1/3)        ((R L) (- L))
: F/2: (E/6 G/3 N/2)    ((R L -) (R L L))
: E/2: (A/3 F/2)      ((R L) (- L))
: A/2: (E/2 B/6 C/5)    ((L R +) (- - +))
: B/3: (D/5 A/2)      ((R L) (- L) (R -))
: D/3: (C/5 B/3 M/6)    ((- - -) (- L R) (R - L))
: W/1: (X/l 1/3 Y/5)    ((L R +))
: 1/1: (N/2 0/1 W/l)    ((R L L))
: Y/1: (W/l 2/3 Z/3)    ((+ + +))
: 2/2: (V/6 3/6 Y/1)    ((L R +) (- - +))
: V/3: (2/2 4/4)      ((R L) (- L) (R -))
: 4/2: (U/6 L/4 V/3)    ((R L -) (R L R))
: U/2: (T/3 4/2)      ((R L) (- L))
: T/2: (U/2 S/6 K/4)    ((L R +) (- - +))
: S/2: (R/4 T/2)      ((R L) (R -))
: K/1: (J/4 L/4 T/2)    ((R L +))
: J/1: (1/4 K/1 R/4)    ((R L L))
: I/1: (H/4 J/1 Q/4)    ((R L R))
: L/1: (K/l M/6 4/2)    ((R L R))
: M/2: (L/1 D/3)      ((R L) (R -))
: 3/3: (Z/3 2/2)      ((R L) (- L) (R -))
: Z/1 : (3/3 X/1 Y/1)    ((- - +))
: 3/1: (Z/l 2/2)    ((- L))
: 2/1: (V/3 3/1 Y/1)    ((L R +))
: V/2: (2/1 4/2)      ((R L) (R -))
After constraint propagation the diagram is:
  A/0 W:
  P/l L: P0=[R] PQ=CL]
  B/0 L:
  Q/4 T: QP=[RRRR] QI=[LLLL] QR=[+-LR]
  C/0 Y:
  R/4 T: RJ=[RRRR] RS=[LLLL] RQ=[+-LR]
  D/0 Y:
  S/2 L: SR=[RR] ST=[L-]
  E/2 L: EA=[R-] EF=[LL]
  T/2 W: TU=[L-] TS=CR-] TK=[++]
  F/2 T: FE=[RR] FG=[LL] FN=[-  L]
  U/2 L: UT=[R-] U4=[LL]
  G/0 W:
  V/2 L: V2=[RR] V4=[L-]
  H/0 T:
  W/l W: WX=[L] W1=[R] WY=[+]
  I/1 T: IH=[R] IJ=[L] IQ=[R]
  X/1 L: XW=[R] XZ=[-]
  J/1 T: JI=[R] JK=[L] JR=[L]
  Y/1 Y: YW=[+] Y2=[+] YZ=[+]
  K/1 T: KJ=[R] KL=[L] KT=[+]
  Z/1 W: Z3=[-] ZX=[-] ZY=[+]
  L/1 T: LK=[R] LM=[L] L4=[R]
  1/1 T: 1 N=[R] 10=[L] 1 W=[L]
  M/2 L: ML=[RR] MD=[L-]
  2/1 W: 2 V=[L] 23=[R] 2Y=[+]
  N/2 L: NF=[R-] N1=[LL]
  3/1 L: 3Z=[-] 32=[L]
  0/1 W: 0P=[L] 01=[R] 0H=[+]
  4/2 T: 4U=[RR] 4 L=[LL] 4 V=[-  R]
```

На диаграмме после распространения ограничений мы видим, что вершины A, B, C, D, G и H не имеют интерпретаций, поэтому они являются хорошим местом для поиска ошибки в первую очередь.
Из трассы(распечатанного следа), генерируемого `propagate-constraints`(распространением ограничений (линии, начинающиеся с точки с запятой), мы видим, что распространение ограничений началось в точке P и после семи распространений достигло некоторых подозрительных вершин:

```lisp
: A/2: (E/2 B/6 C/5)    ((L R +) (- - + ))
: B/3: (D/5 A/2)        ((R L) (- L) (R -))
: D/3: (C/5 B/3 M/6)    ((- - -) (- L R) (R - L))
```

A и B выглядят приемлемо, но посмотрите на запись для вершины D.
Она показывает три интерпретации и показывает, что соседями являются C, B и M.
Обратите внимание, что линия DC, первая запись в каждой из интерпретаций, должна быть либо -, либо R.
Но это ошибка, потому что "правильная" интерпретация имеет DC как линию +.
При более внимательном рассмотрении мы замечаем, что D на самом деле является вершиной W-типа, а не вершиной Y, как написано в определении.
Мы должны иметь:

```lisp
(defdiagram arch
  (a W e b c)    (p L o q)
  (b L d a)      (q T p i r)
  (c Y a d g)    (r T j s q)
  (d W b m c)    (s L r t)          ;*d is a W, not Y*
  (e L a f)      (t W u s k)
  (f T e g n)    (u L t 4)
  (g W h f c)    (v L 2 4)
  (h T g i o)    (w W x 1 y)
  (i T h j q)    (x L w z)
  (j T i k r)    (y Y w 2 z)
  (k T J l t)    (z W 3 x y)
  (1 T k m 4)    (1 T n o w)
  (m L l d)      (2 W v 3 y)
  (n L f 1)      (3 L z 2)
  (o W P 1 h)    (4 T u l v))
```

Запустив задачу еще раз и проверив вывод трассировки, мы вскоре обнаружим настоящий корень проблемы: наиболее естественная интерпретация диаграммы выходит за рамки программы!
Есть много интерпретаций, в которых блоки летают в воздухе, но если мы заземлим линии OP, TU и XZ, мы столкнемся с проблемами.
Помните, мы говорили, что рассматриваем только трехгранные вершины.
Но вершина 1 будет четырехгранной вершиной, образованной пересечением четырех плоскостей: верхней и задней части основания, а также нижней и левой стороны левой колонны.
Интуитивно правильная маркировка для диаграммы будет иметь O1 вогнутой (-) линией, а Al - закрывающей линией, но наш репертуар разметки для T вершин не позволяет этого.
Следовательно, диаграмма не может быть корректно промаркирована.

Вернемся назад и рассмотрим ошибку, появившуюся в первой версии диаграммы.
Несмотря на то, что ошибка больше не появляется на этой диаграмме, мы хотим убедиться, что она не появится в другом случае.
Вот ошибка:

```lisp
>>>ERROR: The first argument to NTH was of the wrong type.
```

Функция ожидала fixnum >= `zero.`

`Находясь в функции LABELS-FOR` <= `CONSISTENT-LABELINGS`

```lisp
Debugger entered while in the following function:
```

`LABELS-FOR (P.C.
= 23)`

```lisp
      Arg 0 (VERTEX): U/6
      Arg 1 (FROM): 4/4
```

Глядя на определение `labels-for`, мы видим, что он ищет исходную вершину, которая в данном случае равна 4, среди соседей U.
Она не была найдена, поэтому pos стал nil, а функция nth пожаловалась, что ей не было дано целое число в качестве аргумента.
Таким образом, эта ошибка, если бы мы преследовали ее раньше, указала бы на то, что 4 не был указан как сосед U, хотя должен был быть.
Конечно, мы выяснили это другими способами.
В любом случае, здесь нет ошибки, которую нужно исправить - до тех пор, пока диаграмма будет непротиворечивой, ошибка `label-for` больше не появится.

В этом разделе выделены два момента. Во-первых, пишите код, который проверяет ввод как можно тщательнее.
Во-вторых, даже когда проверка ввода выполнена, пользователю по-прежнему остается понять ограничения программы.

## 17.5 История и Ссылки

[Guzman (1968)](B9780080571157500285.xhtml#bb0500) was one of the first to consider the problem of interpreting line diagrams.
He classified vertexes, and defined some heuristics for combining information from adjacent vertexes.
[Huffman (1971)](B9780080571157500285.xhtml#bb0560) and [Clowes (1971)](B9780080571157500285.xhtml#bb0215) independently came up with more formai and complete analyses, and David [Waltz (1975)](B9780080571157500285.xhtml#bb1300) extended the analysis to handle shadows, and introduced the constraint propagation algorithm to eut down on the need for search.
The algorithm is sometimes called "Waltz filtering" in his honor.
With shadows and nontrihedral angles, there are thousands of vertex labelings instead of 18, but there are also more constraints, so the constraint propagation actually does better than it does in our limited world.
Waltz's approach and the Huf f man-Clowes labels are covered in most introductory AI books, including Rich and Knight 1990, [Charniak and McDermott 1985](B9780080571157500285.xhtml#bb0175), and [Winston 1984](B9780080571157500285.xhtml#bb1405).
Waltz's original paper appears in *The Psychology of Computer Vision* ([Winston 1975](B9780080571157500285.xhtml#bb1400)), an influential volume collecting early work done at MIT.
He also contributed a summary article on Waltz filtering ([Waltz 1990](B9780080571157500285.xhtml#bb1305)).

Many introductory AI texts give vision short coverage, but [Charniak and McDermott (1985)](B9780080571157500285.xhtml#bb0175) and [Tanimoto (1990)](B9780080571157500285.xhtml#bb1220) provide good overviews of the field.
[Zucker (1990)](B9780080571157500285.xhtml#bb1450) provides an overview of low-level vision.

[Ramsey and Barrett (1987)](B9780080571157500285.xhtml#bb0975) give an implementation of a line-recognition program.
It would make a good project to connect their program to the one presented in this chapter, and thereby go all the way from pixels to 3-D descriptions.

## 17.6 Упражнения

This chapter has solved the problem of line-labeling for polyhedra made of trihedral vertexes.
The following exercises extend this solution.

**Exercise  17.1 [h]** Use the line-labeling to produce a face labeling.
Write a function that takes a labeled diagram as input and produces a list of the faces (planes) that comprise the diagram.

**Exercise  17.2 [h]** Use the face labeling to produce a polyhedron labeling.
Write a function that takes a list of faces and a diagram and produces a list of polyhedra (blocks) that comprise the diagram.

**Exercise  17.3 [d]** Extend the system to include quad-hedral vertexes and/or shadows.
There is no conceptual difficulty in this, but it is a very demanding task to find all the possible vertex types and labelings for them.
Consult [Waltz 1975](B9780080571157500285.xhtml#bb1300).

**Exercise  17.4 [d]** Implement a program to recognize lines from pixels.

**Exercise  17.5 [d]** If you have access to a workstation with a graphical interface, implement a program to allow a user to draw diagrams with a mouse.
Have the program generate output in the form expected by `construct-diagram`