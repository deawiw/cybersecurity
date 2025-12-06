# Практическая работа 3
deawiw@yandex.ru

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Программное обеспечение macOS 14.0
2.  RStudio Desktop
3.  Интерпретатор языка R 4.1

## План

Проанализировать встроенные в пакет nycflights13 наборы данных с помощью
языка R и ответить на вопросы

## Шаги:

1.  Загружаем библиотеки

    ``` r
    library(nycflights13)
    library(dplyr)
    ```


        Attaching package: 'dplyr'

        The following objects are masked from 'package:stats':

            filter, lag

        The following objects are masked from 'package:base':

            intersect, setdiff, setequal, union

2.  Сколько встроенных в пакет nycflights13 датафреймов?

    ``` r
    length(ls("package:nycflights13"))
    ```

        [1] 5

3.  Сколько строк в каждом датафрейме?

    ``` r
    c(nrow(airlines), nrow(airports), nrow(flights), nrow(planes), nrow(weather))
    ```

        [1]     16   1458 336776   3322  26115

4.  Сколько столбцов в каждом датафрейме?

    ``` r
    c(ncol(airlines), ncol(airports), ncol(flights), ncol(planes), ncol(weather))
    ```

        [1]  2  8 19  9 15

5.  Как просмотреть примерный вид датафрейма?

    ``` r
    flights %>% glimpse()
    ```

        Rows: 336,776
        Columns: 19
        $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2…
        $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
        $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
        $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, …
        $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, …
        $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1…
        $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849,…
        $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851,…
        $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -1…
        $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "…
        $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 4…
        $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N394…
        $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA",…
        $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD",…
        $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149, 1…
        $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733, …
        $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6…
        $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0…
        $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 0…

6.  Сколько компаний-перевозчиков (carrier) учитывают эти наборы данных
    (представлено в наборах данных)?

    ``` r
    nrow(airlines)
    ```

        [1] 16

7.  Сколько рейсов принял аэропорт John F Kennedy Intl в мае?

    ``` r
    flights %>% filter(month == 5, dest == "JFK") %>% nrow()
    ```

        [1] 0

8.  Какой самый северный аэропорт?

    ``` r
    airports %>% arrange(desc(lat)) %>% slice(1)
    ```

        # A tibble: 1 × 8
          faa   name                      lat   lon   alt    tz dst   tzone
          <chr> <chr>                   <dbl> <dbl> <dbl> <dbl> <chr> <chr>
        1 EEN   Dillant Hopkins Airport  72.3  42.9   149    -5 A     <NA> 

9.  Какой аэропорт самый высокогорный (находится выше всех над уровнем
    моря)?

    ``` r
    airports %>% arrange(desc(alt)) %>% slice(1)
    ```

        # A tibble: 1 × 8
          faa   name        lat   lon   alt    tz dst   tzone         
          <chr> <chr>     <dbl> <dbl> <dbl> <dbl> <chr> <chr>         
        1 TEX   Telluride  38.0 -108.  9078    -7 A     America/Denver

10. Какие бортовые номера у самых старых самолетов?

    ::: {.cell}

    ``` r
    planes %>% arrange(year) %>% select(tailnum)
    ```

    ::: {.cell-output .cell-output-stdout}

        # A tibble: 3,322 × 1
           tailnum
           <chr>  
         1 N381AA 
         2 N201AA 
         3 N567AA 
         4 N378AA 
         5 N575AA 
         6 N14629 
         7 N615AA 
         8 N425AA 
         9 N383AA 
        10 N364AA 
        # ℹ 3,312 more rows

    ::: :::

11. Какая средняя температура воздуха была в сентябре в аэропорту John F
    Kennedy Intl (в градусах Цельсия)?

    ``` r
    weather %>% filter(month == 9, origin == "JFK") %>%
      summarise(avg_temp_f = mean(temp, na.rm = TRUE),
                avg_temp_c = (mean(temp, na.rm = TRUE) - 32) * 5/9)
    ```

        # A tibble: 1 × 2
          avg_temp_f avg_temp_c
               <dbl>      <dbl>
        1       66.9       19.4

12. Самолеты какой авиакомпании совершили больше всего вылетов в июне?

    ``` r
    flights %>%
      filter(month == 6) %>%
      count(carrier, sort = TRUE) %>%
      left_join(airlines, by = "carrier") %>%
      slice(1) %>% select(name, n)
    ```

        # A tibble: 1 × 2
          name                      n
          <chr>                 <int>
        1 United Air Lines Inc.  4975

13. Самолеты какой авиакомпании задерживались чаще других в 2013 году?

    ``` r
    flights %>%
      filter(!is.na(arr_delay), arr_delay > 0) %>%
      count(carrier, sort = TRUE) %>%
      left_join(airlines, by = "carrier") %>%
      slice(1) %>% select(name, n)
    ```

        # A tibble: 1 × 2
          name                         n
          <chr>                    <int>
        1 ExpressJet Airlines Inc. 24484

## Оценка результата

В результате лабораторной работы мы развили практические навыки
использования языка программирования R для анализа данных и закрепили
знания базовых типов данных языка R

## Вывод

Таким образом, мы развили навыки использования функций обработки данных
пакета dplyr, такие как select(), filter(), mutate(), arrange(),
group_by(), summarise().
