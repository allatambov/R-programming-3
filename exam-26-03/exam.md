## Экзамен по курсу "Основы программирования в R"

*Алла Тамбовцева*

**Задание 1**

Дан вектор с названиями регионов, в которых проживают респонденты:

```
regs <- c('Курганская область', 'Владимирская область', 'Челябинская область', 'Ульяновская область', 'Республика Алтай', 'Тюменская область', 'Свердловская область', 'Ленинградская область')
```

*(2 балла)* Выведите на экран число респондентов, которые проживают в [Уральском Федеральном округе](https://ru.wikipedia.org/wiki/%D0%A3%D1%80%D0%B0%D0%BB%D1%8C%D1%81%D0%BA%D0%B8%D0%B9_%D1%84%D0%B5%D0%B4%D0%B5%D1%80%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B9_%D0%BE%D0%BA%D1%80%D1%83%D0%B3).


В заданиях 2-4 Вы будете работать с базой данных, которая посвящена выступлениям TED (сокращенная версия базы с Kaggle). 


*comments* - число комментариев

*description* - описание выступления

*duration* - продолжительность видео в секундах

*event* - название мероприятия

*main speaker* - главный спикер

*title* - название

*occupation* - профессия/статус спикера

**Задание 2**

1. *(1 балл)* Загрузите базу данных *TED.csv*.

2. *(1 балл)* Сколько видеозаписей выступлений в базе? Сколько переменных в базе данных? Приведите код для ответов на вопросы и запишите ответ в виде комментария.

3. *(1 балл)* Сколько в базе строк, содержащих пропущенные значения? Приведите код для ответа на вопрос и запишите ответ в виде комментария.

4. *(1 балл)* Удалите из базы пропущенные значения.

5. *(1 балл)* Выведите описательные статистики для всех переменных в базе.

**Задание 3**

Все необходимые действия должны быть выполнены с помощью библиотеки *dplyr*.

1. *(2 балла)* Сгруппируйте все наблюдения в соответствии со значениями переменной *occupation*. Сколько в базе данных выступающих с различной занятостью? Приведите код.

2. *(2 балла)* Добавьте в базу данных переменную *duration_min* - продолжительность видео в минутах. Добавьте в базу переменную *comm*, которая принимает значение 0, если комментариев к видео меньше 500, и значение 1, если комментариев не менее 500.

3. *(3 балла)* Чего в базе данных больше: видео, у которых комментариев больше 800 или видео, у которых продолжительность менее 100 секунд? Приведите код и ответ на вопрос в виде комментария.

4. *(3 балла)* Чего в базе данных больше: выступлений с более 100 комментариями, которые готовили архитекторы или выступлений с менее 400 комментариями, которые готовили активисты? Приведите код и ответ на вопрос в виде комментария.

5. *(3 балла)* Постройте (необязательно с помощью *ggplot2*) график, который бы показывал распределение показателя *comments*. Поменяйте цвет, подпишите оси и дайте название графику.

**Задание 4 (5 баллов)**

Постройте с помощью библиотеки *ggplot2* диаграмму рассеяния для показателей *число комментариев* (*comments*) и *продолжительность видео в секундах* (*duration*).  Дайте название графику, подпишите оси и поменяйте цвет точек так, чтобы точки, соответствующие видео с более чем 400 комментариями и не менее, чем 400 комментариями, были разного цвета.

**Задание 5 (5 баллов)**

Дан вектор явки (в процентах).


```
turnout <- c(17.5, 32, 64, 78, 69, 82, 90, 42, 146, 123, 98, 16)
```

Напишите код, который делает следующее. До тех пор, пока значение явки не превышает 100 процентов, R выводит на экран значение явки, но если значение больше 100, R сразу останавливается, пишет "Are you sure?" и дальше ничего не делает (то есть перестает выводить значения на экран).

**Задание 6 (6 баллов)**

Напишите функцию *bmi*, которая запрашивает у пользователя с клавиатуры его рост в сантиметрах и вес в килограммах (считайте, что пользователь вводит два числа через пробел) и возвращает индекс массы тела, округленный до целого значения. Индекс массы тела считается так: 

    ```
    BMI = m / h^2, где m - масса тела в килограммах, h - рост в МЕТРАХ
    ```

**Задание 7 (4 балла)**

Напишите функцию *dante*, которая выводит на экран вопрос "В каком круге Ада по Данте находится Платон?", сохраняет ответ пользователя, введенный с клавиатуры, и если ответ верный (принимаются ответы *Лимб*, *первый* и *1*), то выводит на экран сообщение "Верный ответ!", если нет -- выводит сообщение "Неверно. Перечитайте Данте!".