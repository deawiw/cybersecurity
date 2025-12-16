# Практическая работа 5
deawiw@yandex.ru

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Программное обеспечение macOS 14.0
2.  RStudio Desktop
3.  Интерпретатор языка R 4.1

## План

1.  Импортируйте данные –
    https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv
    Данные были собраны с помощью анализатора беспроводного трафика
    airodump-ng
2.  Привести датасеты в вид “аккуратных данных”, преобразовать типы
    столбцов в соответствии с типом данных
3.  Просмотрите общую структуру данных с помощью функции glimpse()
4.  Произвести анализ данных.
5.  Определить небезопасные точки доступа (без шифрования – OPN)
6.  Определить производителя для каждого обнаруженного устройства
7.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах
8.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию.
9.  Обнаружить топ-10 самых быстрых точек доступа.
10. Отсортировать точки доступа по частоте отправки запросов (beacons) в
    единицу времени по их убыванию. 11.Определить производителя для
    каждого обнаруженного устройства (пользоваться базой данных
    производителей из состава Wireshark или онлайн сервисами OUI lookup)
11. Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес
12. Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.
13. Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер.

## Шаги:

1.  Импортируйте данные.

``` r
library(readr)
```

    Warning: package 'readr' was built under R version 4.5.2

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(lubridate)
```


    Attaching package: 'lubridate'

    The following objects are masked from 'package:base':

        date, intersect, setdiff, union

``` r
library(stringr)
wifi_raw <- read_csv("https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv")
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 12249 Columns: 15

    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (10): BSSID, Privacy, Cipher, Authentication, Power, # beacons, # IV, L...
    dbl   (2): channel, Speed
    lgl   (1): Key
    dttm  (2): First time seen, Last time seen

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

1.  Привести датасеты в вид “аккуратных данных”, преобразовать типы
    столбцов в соответствии с типом данных

``` r
wifi_clean <- wifi_raw %>%
  mutate(
    `First time seen` = ymd_hms(`First time seen`),
    `Last time seen` = ymd_hms(`Last time seen`),
    channel = as.integer(channel),
    Speed = as.integer(Speed)
  ) %>%
  filter(!is.na(`First time seen`), !is.na(`Last time seen`))
```

1.  Просмотрите общую структуру данных с помощью функции glimpse()

``` r
glimpse(wifi_clean)
```

    Rows: 12,248
    Columns: 15
    $ BSSID             <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:…
    $ `First time seen` <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-2…
    $ `Last time seen`  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-2…
    $ channel           <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6…
    $ Speed             <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 18…
    $ Privacy           <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2…
    $ Cipher            <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", …
    $ Authentication    <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK",…
    $ Power             <chr> "-30", "-30", "-68", "-37", "-57", "-63", "-27", "-3…
    $ `# beacons`       <chr> "846", "750", "694", "510", "647", "251", "1647", "1…
    $ `# IV`            <chr> "504", "116", "26", "21", "6", "3430", "80", "11", "…
    $ `LAN IP`          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "…
    $ `ID-length`       <chr> "12", "4", "2", "14", "25", "13", "12", "13", "24", …
    $ ESSID             <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, …
    $ Key               <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …

1.  Определить небезопасные точки доступа (без шифрования – OPN)

``` r
insecure_ap <- wifi_clean %>%
  filter(Privacy == "OPN")

insecure_ap
```

    # A tibble: 42 × 15
       BSSID    `First time seen`   `Last time seen`    channel Speed Privacy Cipher
       <chr>    <dttm>              <dttm>                <int> <int> <chr>   <chr> 
     1 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
     2 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:12       6   130 OPN     <NA>  
     3 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:11       6   130 OPN     <NA>  
     4 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:10       6    -1 OPN     <NA>  
     5 00:25:0… 2023-07-28 09:13:06 2023-07-28 11:56:21      44    -1 OPN     <NA>  
     6 E8:28:C… 2023-07-28 09:13:09 2023-07-28 11:56:05      11   130 OPN     <NA>  
     7 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:27:06       6   130 OPN     <NA>  
     8 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:39:43       6   130 OPN     <NA>  
     9 E8:28:C… 2023-07-28 09:13:17 2023-07-28 11:52:32       1   130 OPN     <NA>  
    10 E8:28:C… 2023-07-28 09:13:50 2023-07-28 11:43:39      11   130 OPN     <NA>  
    # ℹ 32 more rows
    # ℹ 8 more variables: Authentication <chr>, Power <chr>, `# beacons` <chr>,
    #   `# IV` <chr>, `LAN IP` <chr>, `ID-length` <chr>, ESSID <chr>, Key <lgl>

``` r
nrow(insecure_ap)
```

    [1] 42

1.  Определить производителя для каждого обнаруженного устройства

``` r
get_manufacturer <- function(mac) {
  oui <- str_sub(mac, 1, 8)
  case_when(
    oui == "00:25:00" ~ "Dell",
    oui == "00:14:22" ~ "D-Link",
    oui == "00:23:69" ~ "Apple",
    oui == "00:26:BB" ~ "Cisco",
    oui == "08:00:27" ~ "VirtualBox",
    TRUE ~ "Unknown"
  )
}

wifi_clean <- wifi_clean %>%
  mutate(Manufacturer = get_manufacturer(BSSID))

count(wifi_clean, Manufacturer, sort = TRUE)
```

    # A tibble: 2 × 2
      Manufacturer     n
      <chr>        <int>
    1 Unknown      12247
    2 Dell             1

1.  Выявить устройства, использующие последнюю версию протокола
    шифрования WPA3, и названия точек доступа, реализованных на этих
    устройствах

``` r
wifi_clean %>%
  filter(str_detect(Privacy, "WPA3") | str_detect(Authentication, "WPA3")) %>%
  select(BSSID, ESSID, Privacy, Authentication)
```

    # A tibble: 8 × 4
      BSSID             ESSID                Privacy   Authentication
      <chr>             <chr>                <chr>     <chr>         
    1 26:20:53:0C:98:E8 <NA>                 WPA3 WPA2 SAE PSK       
    2 A2:FE:FF:B8:9B:C9 Christie’s           WPA3 WPA2 SAE PSK       
    3 96:FF:FC:91:EF:64 <NA>                 WPA3 WPA2 SAE PSK       
    4 CE:48:E7:86:4E:33 iPhone (Анастасия)   WPA3 WPA2 SAE PSK       
    5 8E:1F:94:96:DA:FD iPhone (Анастасия)   WPA3 WPA2 SAE PSK       
    6 BE:FD:EF:18:92:44 Димасик              WPA3 WPA2 SAE PSK       
    7 3A:DA:00:F9:0C:02 iPhone XS Max 🦊🐱🦊 WPA3 WPA2 SAE PSK       
    8 76:C5:A0:70:08:96 <NA>                 WPA3 WPA2 SAE PSK       

1.  Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию.

``` r
wifi_clean %>%
  mutate(duration_min = as.numeric(difftime(`Last time seen`, `First time seen`, units = "mins"))) %>%
  arrange(desc(duration_min)) %>%
  select(BSSID, ESSID, duration_min)
```

    # A tibble: 12,248 × 3
       BSSID             ESSID         duration_min
       <chr>             <chr>                <dbl>
     1 00:25:00:FF:94:73 <NA>                  163.
     2 10:51:07:CB:33:BF <NA>                  163.
     3 00:95:69:E7:7C:ED <NA>                  163.
     4 00:95:69:E7:7D:21 <NA>                  163.
     5 10:51:07:CB:33:E7 <NA>                  163.
     6 8C:55:4A:DE:F2:38 <NA>                  163.
     7 E8:28:C1:DD:04:52 MIREA_HOTSPOT         163.
     8 00:95:69:E7:7F:35 <NA>                  163.
     9 E8:28:C1:DC:B2:52 MIREA_HOTSPOT         163.
    10 BC:F1:71:D5:3F:C7 <NA>                  163.
    # ℹ 12,238 more rows

1.  Обнаружить топ-10 самых быстрых точек доступа.

``` r
wifi_clean %>%
  arrange(desc(Speed)) %>%
  select(BSSID, ESSID, Speed) %>%
  head(10)
```

    # A tibble: 10 × 3
       BSSID             ESSID Speed
       <chr>             <chr> <int>
     1 00:95:69:E7:7D:21 <NA>   8171
     2 00:95:69:E7:7C:ED <NA>   4096
     3 00:95:69:E7:7F:35 <NA>   2245
     4 98:F6:21:72:9E:D6 <NA>   2143
     5 F6:4D:98:98:18:C3 <NA>   1062
     6 52:FE:C5:8B:DF:D3 <NA>   1037
     7 C0:E4:34:D8:E7:E5 <NA>    958
     8 74:DF:BF:7B:00:19 <NA>    911
     9 26:20:53:0C:98:E8 <NA>    866
    10 96:FF:FC:91:EF:64 <NA>    866

1.  Отсортировать точки доступа по частоте отправки запросов (beacons) в
    единицу времени по их убыванию

``` r
wifi_clean %>%
  mutate(
    beacons = as.numeric(`# beacons`),
    duration_min = as.numeric(difftime(`Last time seen`, `First time seen`, units = "mins")),
    beacon_rate = beacons / duration_min
  ) %>%
  filter(!is.na(beacon_rate), duration_min > 0) %>%
  arrange(desc(beacon_rate)) %>%
  select(BSSID, ESSID, beacon_rate)
```

    Warning: There was 1 warning in `mutate()`.
    ℹ In argument: `beacons = as.numeric(`# beacons`)`.
    Caused by warning:
    ! NAs introduced by coercion

    # A tibble: 124 × 3
       BSSID             ESSID                    beacon_rate
       <chr>             <chr>                          <dbl>
     1 F2:30:AB:E9:03:ED iPhone (Uliana)                51.4 
     2 B2:CF:C0:00:4A:60 Михаил's Galaxy M32            48   
     3 3A:DA:00:F9:0C:02 iPhone XS Max 🦊🐱🦊           33.3 
     4 02:BC:15:7E:D5:DC MT_FREE                        30   
     5 00:3E:1A:5D:14:45 MT_FREE                        30   
     6 76:C5:A0:70:08:96 <NA>                           30   
     7 D2:25:91:F6:6C:D8 Саня                           23.1 
     8 BE:F1:71:D6:10:D7 C322U21 0566                   10.4 
     9 00:03:7A:1A:03:56 MT_FREE                        10   
    10 38:1A:52:0D:84:D7 EBFCD57F-EE81fI_DL_1AO2T        9.78
    # ℹ 114 more rows

1.  Определить производителя для каждого обнаруженного устройства

``` r
get_manufacturer <- function(mac) {
  oui <- str_sub(mac, 1, 8)
  case_when(
    oui == "00:25:00" ~ "Dell",
    oui == "00:14:22" ~ "D-Link",
    oui == "00:23:69" ~ "Apple",
    oui == "00:26:BB" ~ "Cisco",
    oui == "08:00:27" ~ "VirtualBox",
    TRUE ~ "Unknown"
  )
}

wifi_clean <- wifi_clean %>%
  mutate(Manufacturer = get_manufacturer(BSSID))

count(wifi_clean, Manufacturer, sort = TRUE)
```

    # A tibble: 2 × 2
      Manufacturer     n
      <chr>        <int>
    1 Unknown      12247
    2 Dell             1

1.  Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
is_randomized_mac <- function(mac) {
  first_byte <- str_sub(mac, 1, 2)
  first_byte_dec <- strtoi(first_byte, base = 16)
  bitwAnd(first_byte_dec, 2) == 2
}

wifi_clean <- wifi_clean %>%
  mutate(
    is_randomized = is_randomized_mac(BSSID)
  )

non_random_devices <- wifi_clean %>%
  filter(!is_randomized)

nrow(non_random_devices)
```

    [1] 313

1.  Кластеризовать запросы от устройств к точкам доступа по их именам.
    Определить время появления устройства в зоне радиовидимости и время
    выхода его из нее.

``` r
clusters <- wifi_clean %>%
  filter(!is.na(ESSID)) %>%
  group_by(ESSID) %>%
  summarise(
    first_seen = min(`First time seen`),
    last_seen = max(`Last time seen`),
    duration_min = as.numeric(difftime(last_seen, first_seen, units = "mins")),
    ap_count = n(),
    .groups = "drop"
  )

glimpse(clusters)
```

    Rows: 64
    Columns: 5
    $ ESSID        <chr> "Amuler", "AndroidAP177B", "C239U51 0701", "C239U52 6706"…
    $ first_seen   <dttm> 2023-07-28 10:27:46, 2023-07-28 09:13:03, 2023-07-28 10:…
    $ last_seen    <dttm> 2023-07-28 10:27:46, 2023-07-28 11:36:31, 2023-07-28 11:…
    $ duration_min <dbl> 0.00000, 143.46667, 69.06667, 76.28333, 70.40000, 76.3000…
    $ ap_count     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …

1.  Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер

``` r
wifi_clean <- wifi_clean %>%
  mutate(Power = as.numeric(Power))
```

    Warning: There was 1 warning in `mutate()`.
    ℹ In argument: `Power = as.numeric(Power)`.
    Caused by warning:
    ! NAs introduced by coercion

``` r
signal_stability <- wifi_clean %>%
  filter(!is.na(ESSID), !is.na(Power)) %>%
  group_by(ESSID) %>%
  summarise(
    mean_power = mean(Power),
    sd_power = sd(Power),
    observations = n(),
    .groups = "drop"
  ) %>%
  arrange(sd_power)

most_stable_cluster <- signal_stability %>%
  slice(1)

most_stable_cluster
```

    # A tibble: 1 × 4
      ESSID              mean_power sd_power observations
      <chr>                   <dbl>    <dbl>        <int>
    1 iPhone (Анастасия)        -66     1.41            2

## Оценка результата

В рамках практческой работы была исследована радиоэлектронная обстановка
и составлено представление о механизмах работы Wi-Fi сетей на канальном
и сетевом уровне модели OSI

## Вывод

В практической работе мы использовали навыки написания кода на языке
программирования R для обработки данных и закрепили знания основных
функций обработки данных экосистемы tidyverse языка R.
