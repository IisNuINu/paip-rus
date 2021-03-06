# Глава 2
## Простая Лисп Программа

> *Убедитесь, что это произошло.*

> (Человек уверен только в том, что он создает.)

> -Giovanni Battista Vico (1668-1744)

> Italian royal historiographer

Вы никогда не станете хорошо владеть иностранным языком, изучая словари.
Будет лучше, если вы будете слышать и говорить (или читать и писать) на языке, чтобы получить навыки.
То же самое верно и для изучения компьютерных языков.

В этой главе показано, как объединить основные функции и специальные формы Lisp в целостную программу.
Если вы научитесь этому, то освоить оставшийся словарный запас Lisp (как описано в главе 3) будет легко.

## 2.1 Грамматика для подмножества английского языка

Программа, которую мы разработаем в этой главе, генерирует случайные английские предложения.
Вот простая грамматика для крошечной части английского языка:

> *Sentence* => *Noun-Phrase + Verb-Phrase*  

> *Noun-Phrase* => *Article + Noun*  

> *Verb-Phrase* => *Verb + Noun-Phrase*  

> *Article* => *the, a,...*  

> *Noun* => *man, ball, woman, table...*  

> *Verb* => *hit, took, saw, liked...*


Чтобы быть техническим, это описание называется "контекстно-свободной грамматикой структуры фразы", а лежащая в ее основе парадигма называется "генеративным синтаксисом".
Идея заключается в том, что в любом месте, где мы хотим предложение, мы можем создать noun phrase (именная фраза), за которым следует verb phrase(глагольная фраза).
Везде, где была указана noun phrase (именная фраза), мы генерируем вместо нее article (артикль), за которым следует noun (существительное).
Где бы ни была указан article (артикль), мы генерируем либо "the", либо "a", либо какой-то другой артикль.
Формализм является "контекстно-свободным", потому что правила применяются везде, независимо от окружающих слов, а подход является "генеративным", потому что правила в целом определяют полный набор предложений в языке (а также, набор бессмысленностей).
Ниже мы покажем вывод одного предложения с помощью этих правил:

* Чтобы получить предложение - *Sentence*, добавьте *Noun-Phrase* и *Verb-Phrase*
  * Чтобы получить именную фразу *Noun-Phrase*, добавьте артикль *Article* и существительное *Noun*
    * Выбираем *"the"* для артикля *Article*
    * Выбираем *"man"* для существительного *Noun*
  * Результирующая именная фраза *Noun-Phrase* будет *"the man"*
  * Чтобы получить глагольную фразу *Verb-Phrase,* добавьте глагол *Verb* и именную фразу *Noun-Phrase*
    * Выбираем *"hit"* для глагола *Verb*
    * Чтобы получить именную фразу *Noun-Phrase*, добвьте артикль *Article* и существительное *Noun*
      * Выбираем *"the"* для артикля *Article*
      * Выбираем *"ball"* для существительного *Noun*
    * Результирующая именная фраза *Noun-Phrase* будет *"the ball"*
  * Результирующая глагольная фраза *Verb-Phrase* будет *"hit the ball"*
* Результирующее предложение *Sentence* будет *"The man hit the ball"*

## 2.2 Простое Решение

Мы разработаем программу, которая генерирует случайные предложения из грамматики структуры фразы.
Самый простой подход заключается в представлении каждого грамматического правила отдельной функцией Lisp:

```lisp
(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))
```

Каждое из этих определений функций имеет пустой список параметров `()`.
Это означает, что функции не принимают аргументов.
Это необычно, потому что, строго говоря, функция без аргументов всегда будет возвращать одно и то же, поэтому мы можем использовать константу вместо неё.
Однако эти функции используют  функцию `random` (как мы вскоре увидим) и, таким образом, могут возвращать различные результаты даже без аргументов.
Таким образом, они не являются функциями в математическом смысле, но они все еще называются функциями в Lisp, потому что они возвращают значение.

Теперь остается только определить функцию "один из" - `one-of`.
Она принимает список возможных вариантов в качестве аргумента, выбирает один из них наугад и возвращает одноэлементный список выбранного элемента.
Эта последняя часть предназначена для того, чтобы все функции в грамматике возвращали список слов, пусть даже и одноэлементный.
Таким образом, мы можем свободно применять `append` к любой категории.

```lisp
(defun one-of (set)
  "Выбирает один элемент набора и составляет из него список."
  (list (random-elt set)))

(defun random-elt (choices)
  "Выбирает элемент из списка наугад"
  (elt choices (random (length choices))))
```

Здесь используются две новые функции: `elt` и `random`.
`elt` выбирает элемент из списка.
Первый аргумент - это список, а второй - позиция в списке.
Путаница заключается в том, что позиции начинаются с 0, поэтому `(elt choices 0)` является первым элементом списка, а `(elt choices 1)` - вторым.
Считайте, что номера позиций говорят вам, как далеко вы находитесь от начала списка.
Выражение `(random n)` возвращает целое число от 0 до n-1, так что `(random 4)` вернет либо 0, либо 1, либо 2, либо 3.

Теперь мы можем протестировать программу, создав несколько случайных предложений, а также фразу существительного(noun) и глагола(verb):

```lisp
> (sentence) => (THE WOMAN HIT THE BALL)

> (sentence) => (THE WOMAN HIT THE MAN)

> (sentence) =>(THE BALL SAW THE WOMAN)

> (sentence) => (THE BALL SAW THE TABLE)

> (noun-phrase) => (THE MAN)

> (verb-phrase) => (LIKED THE WOMAN)

> (trace sentence noun-phrase verb-phrase article noun verb) =>
(SENTENCE NOUN-PHRASE VERB-PHRASE ARTICLE NOUN VERB)

> (sentence) =>
(1 ENTER SENTENCE)
  (1 ENTER NOUN-PHRASE)
    (1 ENTER ARTICLE)
    (1 EXIT ARTICLE: (THE))
    (1 ENTER NOUN)
    (1 EXIT NOUN: (MAN))
  (1 EXIT NOUN-PHRASE: (THE MAN))
  (1 ENTER VERB-PHRASE)
    (1 ENTER VERB)
    (1 EXIT VERB: (HIT))
    (1 ENTER NOUN-PHRASE)
      (1 ENTER ARTICLE)
      (1 EXIT ARTICLE: (THE))
      (1 ENTER NOUN)
      (1 EXIT NOUN: (BALL))
    (1 EXIT NOUN-PHRASE: (THE BALL))
  (1 EXIT VERB-PHRASE: (HIT THE BALL))
(1 EXIT SENTENCE: (THE MAN HIT THE BALL))
(THE MAN HIT THE BALL)
```

Программа работает нормально, и трассировка выглядит точно так же, как пример деривации выше, но определения Lisp немного сложнее читать, чем исходные грамматические правила.
Эта проблема будет усугубляться по мере рассмотрения более сложных правил.
Предположим, мы хотим, чтобы noun phrases(именые фразы) были изменены неопределенным числом прилагательных(adjectives) и неопределенным числом предложных фраз(prepositional phrases).
В грамматической нотации у нас могут быть следующие правила:

> *Noun-Phrase => Article + Adj\* + Noun + PP\*

> Adj\* => 0&#x0338;, Adj + Adj\* 

> PP\* => 0&#x0338;, PP + PP\*

> PP => Prep + Noun-Phrase  

> Adj => big, little, blue, green, ...  

> Prep => to, in, by, with, ...*


В этой нотации 0&#x0338; указывает на выбор пустоты, запятая указывает на выбор нескольких альтернатив, а звездочка не является чем-то особенным-как и в Lisp, это просто часть имени символа.
Однако здесь используется соглашение, что имена, заканчивающиеся звездочкой, обозначают ноль или более повторений основного имени.
То есть, *PP\** означает ноль или более повторений *PP*.
<a id="tfn02-1"></a>
Это обозначение известно как "звезда Клина" (произносится как "clean-E") в честь математика Стивена Коула Клина.[1](#fn02-1)

Проблема в том, что правила для *Adj\** и *PP\**  содержат варианты выбора, которые мы должны были бы представить как своего рода условия в Lisp.
Например:

```lisp
(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))
```

Я выбрал две различные реализации для `Adj*` и `PP*`; любой подход будет работать в любой функции.
Однако мы должны быть осторожны; вот два подхода, которые не сработают:

```lisp
(defun Adj* ()
  "Warning - incorrect definition of Adjectives."
  (one-of '(nil (append (Adj) (Adj*)))))
(defun Adj* ()
  "Warning - incorrect definition of Adjectives."
  (one-of (list nil (append (Adj) (Adj*)))))
```

Первое определение неверно, потому что оно может возвращать буквальное выражение `((append (Adj) (Adj*)))`, а не список слов, как ожидается.
Второе определение вызвало бы бесконечную рекурсию, потому что вычисление значения `(Adj*)` всегда включает рекурсивный вызов `(Adj*)`.
Дело в том, что то, что начиналось как простые функции, теперь становится довольно сложным.
Чтобы понять их, нам нужно знать многие соглашения Lisp - `defun, (), case, if`, `quote` и правила порядка вычисления-когда в идеале реализация грамматического правила должна использовать только *лингвистические* соглашения.
Если бы мы хотели разработать более широкую грамматику, проблема могла бы стать еще хуже, потому что автор правил мог бы все больше и больше зависеть от Lisp.

## 2.3 Решение, Основанное На Правилах

Альтернативная реализация этой программы сосредоточилась бы на упрощении написания грамматических правил и позже беспокоилась бы о том, как они будут обработаны.
Давайте еще раз посмотрим на исходные грамматические правила:

> *Sentence => Noun-Phrase + Verb-Phrase 
 
> Noun-Phrase => Article + Noun  

> Verb-Phrase => Verb + Noun-Phrase  

> Article => the, a, ...  

> Noun => man, ball, woman, table...  

> Verb => hit, took, saw, liked...*


Каждое правило состоит из Стрелки с символом на левой стороне и еще чего-то на правой стороне.
Сложность заключается в том, что правые стороны могут быть двух видов: объединенный список символов, как в  "Noun-Phrase => Article+Noun", или список альтернативных слов, как в "Noun => man, ball, ..."
Мы можем учесть эти возможности, решив, что каждое правило будет иметь список возможностей в правой части, и что объединенный список, *например*  "Article+Noun", будет представлен как список Lisp, *например*  "(Article Noun)".
Затем список правил можно представить следующим образом:

```lisp
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammars.")
```

Обратите внимание, что Лисп-версия правил близко имитирует исходную версию.
В частности, я включаю символ "->", хотя он не служит никакой реальной цели; он чисто декоративный.

Специальные формы `defvar` и `defparameter` вводят специальные переменные и присваивают им значение; разница заключается в том, что *переменная* такая как `*grammar*`, регулярно изменяется в ходе выполнения программы.
С другой стороны, *параметр*, такой как `*simple-grammar*`, обычно остается постоянным.
Изменение параметра считается *изменением программы*, а не изменением выполняемым *самой программой*.

После того, как список правил был определен, он может использоваться, чтобы найти возможные перезаписи данного символа-category.
Функция `assoc` предназначена именно для такого рода задач.
Она принимает два аргумента, "key"(ключ) и список списков, и возвращает первый элемент списка списков, который начинается с переданного ключа.
Если его нет, она возвращает `nil`.
Вот вам пример:

```lisp
> (assoc 'noun *grammar*) => (NOUN -> MAN BALL WOMAN TABLE)
```

Хотя правила довольно просто реализованы в виде списков, хорошей идеей является наложение уровня абстракции путем определения функций для работы с правилами.
Нам понадобятся три функции: одна для получения правой части правила, одна для левой части и одна для поиска всех возможных переписываний (правых частей) для категории(category).

```lisp
(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Возвращает список возможных перезаписей для данной категории."
  (rule-rhs (assoc category *grammar*)))
```

Определение этих функций облегчит чтение программ, которые их используют, а также облегчит изменение представления правил, если мы когда-либо решим это сделать.

Теперь мы готовы рассмотреть главную проблему: определить функцию, которая будет генерировать предложения (или именные фразы/noun phrases, или любую другую категорию).
Мы будем называть эту функцию `generate`.
Ей придется разбираться с тремя случаями:
(1) в простейшем случае `generate` передается символ, с которым связан набор правил перезаписи.
Мы выбираем одно из них наугад, а затем генерируем из него.(второе условие в программе)
(2) Если символ не имеет возможных правил перезаписи, он должен быть терминальным символом-словом, а не грамматической категорией,-и мы оставляем его в покое.(третье условие в программе - по умолчанию)
На самом деле, мы возвращаем список с этим входным словом, потому что, как и в предыдущей программе, мы хотим, чтобы все результаты были списками слов.
(3) в некоторых случаях, когда символ присутствует в rewrites(т.е. является нетерминальным символом/категорией), мы выберем один из списка символов ему соответствующих и попытаемся сгенерировать уже из этого символа.(второе условие в программе)
Таким образом, `generate` также должна принимать список в качестве входных данных, и в этом случае она должна выполнить генерацию для каждого элемента списка (первое условие в программе), а затем соединить(append) их все вместе.
Итак, первое предложение в `generate` обрабатывает этот случай, в то время как второе предложение обрабатывает (1) и третье предложение - обрабатывает (2).
Обратите внимание, что мы использовали функцию `mappend` из раздела 1.7 (стр. 18).

```lisp
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))
```

Как и многие программы в этой книге, эта функция коротка, но насыщена информацией: Искусство программирования включает в себя знание того, что писать, а также того, чего *не* писать.

Этот стиль программирования называется программирование управляемое данными (*data-driven* programming), потому что данные (список перезаписей, связанных с категорией) управляют тем, что программа делает дальше.
Это естественный и простой в использовании стиль программирования в Lisp, приводящий к сжатым и расширяемым программам, потому что всегда можно добавить новый фрагмент данных с новой ассоциацией без необходимости изменять исходную программу.

Вот несколько примеров использования функции `generate`:

```lisp
> (generate 'sentence) => (THE TABLE SAW THE BALL)

> (generate 'sentence) => (THE WOMAN HIT A TABLE)

> (generate 'noun-phrase) => (THE MAN)

> (generate 'verb-phrase) (TOOK A TABLE)
```

Существует множество возможных способов написать `generate`.
Следующая версия использует `if` вместо ``cond`:

```lisp
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (generate (random-elt choices))))))
```

В этой версии используется специальная форма `let`, которая вводит новую переменную (в данном случае `choices`), а также связывает переменную со значением.
В этом случае введение переменной избавляет нас от вызова функции `rewrites` дважды, как это было сделано в версии `generate` с `cond`.
Общая форма формы `let` такова::

```lisp
    `(let` ((*var value*)...)
        *body-containing-vars*)
```

`let` - это наиболее распространенный способ введения переменных, которые не являются параметрами функций.
Нужно сопротивляться искушению использовать переменную, не вводя ее:

```lisp
(defun generate (phrase)
  (setf choices ...)         ;; wrong!
  ... choices ...)
```
Это неверно, потому что символ `choices` теперь относится к специальной или глобальной переменной, которая может быть разделяться или изменяться другими функциями.
Таким образом, эта версия функции `generate` ненадежна, поскольку нет никакой гарантии, что `choices` сохранит то же самое значение с момента его установки(связывания/присваивания) до момента, когда на него снова ссылаются(когда его будут использовать).
С помощью `let` мы вводим совершенно новую переменную, к которой никто другой не может получить доступ; поэтому она гарантированно сохраняет правильное значение.

&#9635; **Упражнение 2.1 [м]** написать версию `generate`, которая использует `cond` но избегает вызова называть `rewrites` дважды.

&#9635; **Упражнение 2.2 [m]** напишите версию `generate`, которая явно различает терминальные символы (те, которые не имеют правил перезаписи) и нетерминальные символы.

## 2.4 два пути следования

Две версии предыдущей программы представляют собой два альтернативных подхода, которые возникают снова и снова при разработке программ: (1) Использовать наиболее простое отображение описания проблемы непосредственно в код Lisp.
(2) Использовать наиболее естественную нотацию, доступную для решения проблемы, а затем позаботиться о написании интерпретатора для этой нотации.

Подход (2) предполагает дополнительный шаг и, таким образом, больше работы для небольших проблем.
Однако программы, использующие этот подход, часто легче модифицировать и расширять.
Это особенно верно в области, где есть много данных для учета.
Грамматика естественного языка является одной из таких областей-на самом деле, большинство проблем ИИ соответствуют этому описанию.
Идея подхода (2) состоит в том, чтобы работать с проблемой как можно больше в ее собственных терминах и минимизировать ту часть решения, которая написана непосредственно на Лиспе.

К счастью, в Lisp очень легко создавать новые нотации - фактически, новые языки программирования.
Таким образом, Lisp стимулирует создание более надежных программ.
На протяжении всей этой книги мы будем помнить об этих двух подходах.
Читатель может заметить, что в большинстве случаев мы выбираем второй.

## 2.5 изменение грамматики без изменения программы

Мы покажем полезность подхода (2), определив новую грамматику, которая включает прилагательные (adjectives), предложные фразы(prepositional phrases), имена собственные (proper names)  и местоимения (pronouns).
Затем мы можем применить функцию `generate` без изменений к этой новой грамматике.

```lisp
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

> (generate 'sentence)
(A TABLE ON A TABLE IN THE BLUE ADIABATIC MAN SAW ROBIN
 WITH A LITTLE WOMAN)

> (generate 'sentence)
(TERRY SAW A ADIABATIC TABLE ON THE GREEN BALL BY THAT WITH KIM
 IN THESE BY A GREEN WOMAN BY A LITTLE ADIABATIC TABLE IN ROBIN
 ON LEE)

> (generate 'sentence)
(THE GREEN TABLE HIT IT WITH HE)
```

Обратите внимание на проблему с согласованием падежей для местоимений: программа генерирует "с ним/with he,", хотя правильной грамматической формой является "with him" .
Также ясно, что программа не отличает разумный вывод от глупого.

## 2.6 использование одних и тех же данных для нескольких программ

Еще одно преимущество представления информации в декларативной форме - в виде правил или фактов, а не в виде функций Lisp - заключается в том, что ее легче использовать для различных целей.
Предположим, нам нужна функция, которая генерировала бы не только список слов в предложении, но и представление полного синтаксиса предложения.
Например, вместо списка `(a woman took a ball)` мы хотим получить вложенный список:

```lisp
(SENTENCE (NOUN-PHRASE (ARTICLE A) (NOUN WOMAN))
          (VERB-PHRASE (VERB TOOK)
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))
```

Это соответствует дереву, которое лингвисты рисуют, как показано на рис. 2.1.

![Figure 2.1: Sentence Parse Tree](images/chapter2/f02-01.jpg)
**Рис. 2.1: Дерево Синтаксического Анализа Предложений**

Используя подход (1) "простых функций", мы застряли бы; нам пришлось бы переписывать каждую функцию, чтобы создать дополнительную структуру.
С помощью подхода (2) "новая нотация" мы могли бы сохранить грамматику такой, какая она есть, и просто написать одну новую функцию: версию `generate`, которая создает вложенные списки.
Эти два изменения заключаются в том, чтобы `cons`(создать список) из категории в передней части каждой перезаписи, а затем не выполнять `append` для всех результатов, а просто перечислить их с помощью `mapcar`:

```lisp
(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))
```

Вот несколько примеров:

```lisp
> (generate-tree 'Sentence)
(SENTENCE (NOUN-PHRASE (ARTICLE A)
                       (ADJ*)
                       (NOUN WOMAN)
                       (PP*))
      (VERB-PHRASE (VERB HIT)
                       (NOUN-PHRASE (PRONOUN HE))
                       (PP*)))

> (generate-tree 'Sentence)
(SENTENCE (NOUN-PHRASE (ARTICLE A)
                       (NOUN WOMAN))
          (VERB-PHRASE (VERB TOOK)
                       (NOUN-PHRASE (ARTICLE A) (NOUN BALL))))
```

В качестве еще одного примера подхода "одини-данные/несколько-программы" мы можем разработать функцию для генерации всех возможных перезаписей фразы.
Функция `generate-all` возвращает список фраз, а не только одну, и мы определяем вспомогательную функцию `combine-all` для управления комбинацией результатов.
Кроме того, есть четыре случая вместо трех, потому что мы должны явно проверить на nil.
Тем не менее, полная программа довольно проста:

```lisp
(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
  E.g., (combine-all '((a) (b)) '((1) (2)))
  -> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xlist))
           ylist))
```

Теперь мы можем использовать `generate-all` для проверки нашей оригинальной маленькой грамматики.
Обратите внимание, что серьезным недостатком `generate-all` является то, что он не может иметь дело с рекурсивными грамматическими правилами, такими как 'Adj\* => Adj + Adj\*', которые появляются в `*bigger-grammar*`, поскольку они приводят к бесконечному выводу.
Но она прекрасно работает для конечных языков, таких как язык, порожденный `*simple-grammar*`:

```lisp
> (generate-all 'Article)

((THE) (A))

> (generate-all 'Noun)

((MAN) (BALL) (WOMAN) (TABLE))

> (generate-all 'noun-phrase)
((A MAN) (A BALL) (A WOMAN) (A TABLE)
 (THE MAN) (THE BALL) (THE WOMAN) (THE TABLE))

> (length (generate-all 'sentence))
256
```

Есть 256 предложений, потому что каждое предложение в этом языке имеет форму артикль-существительное-глагол-артикль-существительное(Article-Noun-Verb-Article-Noun), и есть два артикля, четыре существительных и четыре глагола (2 х 4 х 4 х 2 х 4 = 256).

## 2.7 Упражнения

&#9635; **Exercise  2.3 [h]** Напишите тривиальную грамматику для какого-нибудь другого языка.
Это может быть естественный язык, отличный от английского, или, возможно, подмножество компьютерного языка.

&#9635; **Exercise  2.4 [m]** Один из способов определения функции `combine-all` заключается в том, что она вычисляет перекрестное произведение функции `append` по списку аргументов.
Напишите функцию более высокого порядка `cross-product` и определите `combine-all` в ее терминах.

Мораль заключается в том, чтобы сделать свой код как можно более общим, потому что вы никогда не знаете, что вы можете сделать с ним дальше.

## 2.8 Ответы

### Answer 2.1

```lisp
  (defun generate (phrase)
  "Generate a random sentence or phrase"
  (let ((choices nil))
    (cond ((listp phrase)
        (mappend #'generate phrase))
       ((setf choices (rewrites phrase))
        (generate (random-elt choices)))
       (t (list phrase)))))
```

### Answer 2.2

```lisp
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((non-terminal-p phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun non-terminal-p (category)
  "True if this is a category in the grammar."
  (not (null (rewrites category))))
```

### Answer 2.4

```lisp
(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))
```

Теперь мы можем использовать `cross-product` и другими способами:

```
> (cross-product #'+ '(1 2 3) '(10 20 30))
(11 12 13
 21 22 23
 31 32 33)

> (cross-product #'list '(a b c d e f g h)
                        '(1 2 3 4 5 6 7 8))
((A 1) (B 1) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)
 (A 2) (B 2) (C 2) (D 2) (E 2) (F 2) (G 2) (H 2)
 (A 3) (B 3) (C 3) (D 3) (E 3) (F 3) (G 3) (H 3)
 (A 4) (B 4) (C 4) (D 4) (E 4) (F 4) (G 4) (H 4)
 (A 5) (B 5) (C 5) (D 5) (E 5) (F 5) (G 5) (H 5)
 (A 6) (B 6) (C 6) (D 6) (E 6) (F 6) (G 6) (H 6)
 (A 7) (B 7) (C 7) (D 7) (E 7) (F 7) (G 7) (H 7)
 (A 8) (B 8) (C 8) (D 8) (E 8) (F 8) (G 8) (H 8))
```

----------------------
<a id="fn02-1"></a>
[1](#tfn02-1) Вскоре мы увидим обозначение "Kleene plus", где *PP+* обозначает одно или несколько повторений *PP*.