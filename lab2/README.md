# Практическая работа 2
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

Проанализировать встроенный в пакет dplyr набор данных starwars с
помощью языка R и ответить на вопросы

## Шаги:

1.  Загрузка библиотеки

    ``` r
    library(dplyr)
    ```


        Attaching package: 'dplyr'

        The following objects are masked from 'package:stats':

            filter, lag

        The following objects are masked from 'package:base':

            intersect, setdiff, setequal, union

2.  Сколько строк в датафрейме?

    ``` r
    starwars %>% nrow()
    ```

        [1] 87

3.  Сколько столбцов в датафрейме?

    ``` r
    starwars %>% ncol()
    ```

        [1] 14

4.  Как просмотреть примерный вид датафрейма?

    ``` r
    starwars %>% glimpse()
    ```

        Rows: 87
        Columns: 14
        $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
        $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
        $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
        $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
        $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
        $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
        $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
        $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
        $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
        $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
        $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
        $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
        $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
        $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

5.  Сколько уникальных рас персонажей (species) представлено в данных?

    ``` r
    length(unique(starwars$species))
    ```

        [1] 38

6.  Найти самого высокого персонажа

    ``` r
    starwars %>%
        filter(!is.na(height)) %>%
        arrange(desc(height)) %>%
        head(1)
    ```

        # A tibble: 1 × 14
          name      height  mass hair_color skin_color eye_color birth_year sex   gender
          <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
        1 Yarael P…    264    NA none       white      yellow            NA male  mascu…
        # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
        #   vehicles <list>, starships <list>

7.  Найти всех персонажей ниже 170

    ``` r
    starwars[starwars$height < 170, ]
    ```

        # A tibble: 28 × 14
           name     height  mass hair_color skin_color eye_color birth_year sex   gender
           <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
         1 C-3PO       167    75 <NA>       gold       yellow           112 none  mascu…
         2 R2-D2        96    32 <NA>       white, bl… red               33 none  mascu…
         3 Leia Or…    150    49 brown      light      brown             19 fema… femin…
         4 Beru Wh…    165    75 brown      light      blue              47 fema… femin…
         5 R5-D4        97    32 <NA>       white, red red               NA none  mascu…
         6 Yoda         66    17 white      green      brown            896 male  mascu…
         7 Mon Mot…    150    NA auburn     fair       blue              48 fema… femin…
         8 <NA>         NA    NA <NA>       <NA>       <NA>              NA <NA>  <NA>  
         9 Wicket …     88    20 brown      brown      brown              8 male  mascu…
        10 Nien Nu…    160    68 none       grey       black             NA male  mascu…
        # ℹ 18 more rows
        # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
        #   vehicles <list>, starships <list>

8.  Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ
    подсчитать по формуле

    ``` r
    starwars %>% 
      mutate("IMT" = mass/(height*height)) %>% 
      select(name,IMT)
    ```

        # A tibble: 87 × 2
           name                   IMT
           <chr>                <dbl>
         1 Luke Skywalker     0.00260
         2 C-3PO              0.00269
         3 R2-D2              0.00347
         4 Darth Vader        0.00333
         5 Leia Organa        0.00218
         6 Owen Lars          0.00379
         7 Beru Whitesun Lars 0.00275
         8 R5-D4              0.00340
         9 Biggs Darklighter  0.00251
        10 Obi-Wan Kenobi     0.00232
        # ℹ 77 more rows

9.  Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
    отношению массы (mass) к росту (height) персонажей

    ``` r
       starwars %>% 
      mutate("MH" = mass/height) %>% 
      arrange(desc(MH)) %>%
      slice(1:10)
    ```

        # A tibble: 10 × 15
           name     height  mass hair_color skin_color eye_color birth_year sex   gender
           <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
         1 Jabba D…    175  1358 <NA>       green-tan… orange         600   herm… mascu…
         2 Grievous    216   159 none       brown, wh… green, y…       NA   male  mascu…
         3 IG-88       200   140 none       metal      red             15   none  mascu…
         4 Owen La…    178   120 brown, gr… light      blue            52   male  mascu…
         5 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
         6 Jek Ton…    180   110 brown      fair       blue            NA   <NA>  <NA>  
         7 Bossk       190   113 none       green      red             53   male  mascu…
         8 Tarfful     234   136 brown      brown      blue            NA   male  mascu…
         9 Dexter …    198   102 none       brown      yellow          NA   male  mascu…
        10 Chewbac…    228   112 brown      unknown    blue           200   male  mascu…
        # ℹ 6 more variables: homeworld <chr>, species <chr>, films <list>,
        #   vehicles <list>, starships <list>, MH <dbl>

10. Найти средний возраст персонажей каждой раcы вселенной Звездных войн

    ``` r
    starwars %>%
      group_by(species) %>%
      summarise(avg_age = mean(birth_year, na.rm = TRUE)) %>%
      filter(!is.na(avg_age)) %>%
      arrange(desc(avg_age))
    ```

        # A tibble: 15 × 2
           species        avg_age
           <chr>            <dbl>
         1 Yoda's species   896  
         2 Hutt             600  
         3 Wookiee          200  
         4 Cerean            92  
         5 Zabrak            54  
         6 Human             53.7
         7 Droid             53.3
         8 Trandoshan        53  
         9 Gungan            52  
        10 Mirialan          49  
        11 Twi'lek           48  
        12 Rodian            44  
        13 Mon Calamari      41  
        14 Kel Dor           22  
        15 Ewok               8  

11. Найти самый распространенный цвет глаз персонажей вселенной Звездных
    войн

    ``` r
    starwars %>%
      filter(!is.na(eye_color)) %>%
      count(eye_color, sort = TRUE) %>%
      slice(1)
    ```

        # A tibble: 1 × 2
          eye_color     n
          <chr>     <int>
        1 brown        21

12. Подсчитать среднюю длину имени в каждой расе

    ``` r
    starwars %>%
        filter(!is.na(species), !is.na(name)) %>%
        mutate(name_length = nchar(name)) %>%
        group_by(species) %>%
        summarise(avg_name_length = mean(name_length))
    ```

        # A tibble: 37 × 2
           species   avg_name_length
           <chr>               <dbl>
         1 Aleena              12   
         2 Besalisk            15   
         3 Cerean              12   
         4 Chagrian            10   
         5 Clawdite            10   
         6 Droid                4.83
         7 Dug                  7   
         8 Ewok                21   
         9 Geonosian           17   
        10 Gungan              11.7 
        # ℹ 27 more rows

## Оценка результата

В результате работы применены инструменты языка R для анализа данных,
получены практические навыки загрузки, изучения структуры и обработки
данных

## Вывод

Таким образом, мы развили навыки использования функций обработки данных
пакета dplyr, такие как select(), filter(), mutate(), arrange(),
group_by(), summarise().
