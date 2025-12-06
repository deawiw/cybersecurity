# Практическая работа 4
deawiw@yandex.ru

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Программное обеспечение macOS 14.0
2.  RStudio Desktop
3.  Интерпретатор языка R 4.1

## План

Используя программный пакет dplyr, освоить анализ DNS логов с помощью
языка программирования R.

## Шаги:

1.  Импортируйте данные DNS

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(readr)
```

    Warning: package 'readr' was built under R version 4.5.2

``` r
download.file("https://storage.yandexcloud.net/dataset.ctfsec/dns.zip", "dns.zip")
unzip("dns.zip")
dns <- read_tsv("dns.log", comment = "#",col_names = FALSE, show_col_types = FALSE)
```

1.  Добавьте пропущенные данные о структуре данных (назначении столбцов)

``` r
colnames(dns) <- c("ts", "uid", "id.orig_h", "id.orig_p", 
                    "id.resp_h", "id.resp_p", "proto", 
                    "trans_id", "query", "qclass", "qtype", 
                    "rcode", "rcode_name", "AA", "TC", 
                    "RD", "RA", "Z", "answers", "TTLs", 
                    "rejected")
```

1.  Преобразуйте данные в столбцах в нужный формат

``` r
dns <- dns[, !is.na(colnames(dns))]

dns <- dns %>%
  mutate(
    ts = as.POSIXct(ts, origin = "1970-01-01"),
    id.orig_p = as.integer(id.orig_p),
    id.resp_p = as.integer(id.resp_p),
    trans_id = as.integer(trans_id)
  )
```

1.  Просмотрите общую структуру данных с помощью функции glimpse()

``` r
glimpse(dns)
```

    Rows: 427,935
    Columns: 21
    $ ts         <dttm> 2012-03-16 16:30:05, 2012-03-16 16:30:15, 2012-03-16 16:30…
    $ uid        <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bsb…
    $ id.orig_h  <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "192…
    $ id.orig_p  <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 13…
    $ id.resp_h  <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "19…
    $ id.resp_p  <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137,…
    $ proto      <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "ud…
    $ trans_id   <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 621…
    $ query      <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\…
    $ qclass     <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",…
    $ qtype      <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_…
    $ rcode      <chr> "33", "32", "32", "32", "32", "32", "32", "32", "32", "32",…
    $ rcode_name <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB"…
    $ AA         <chr> "0", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",…
    $ TC         <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ RD         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    $ RA         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    $ Z          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE…
    $ answers    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
    $ TTLs       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,…
    $ rejected   <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",…

1.  Сколько участников информационного обмена в сети Доброй Организации?

``` r
internal_ips <- unique(c(dns$id.orig_h, dns$id.resp_h))
length(internal_ips)
```

    [1] 1359

1.  Какое соотношение участников обмена внутри сети и участников
    обращений к внешним ресурсам?

``` r
all_ips <- unique(c(dns$id.orig_h, dns$id.resp_h))
internal_count <- sum(grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1]))", all_ips))
external_count <- length(all_ips) - internal_count
internal_count / external_count
```

    [1] 13.77174

1.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.

``` r
top_requesters <- dns %>%
  count(id.orig_h, sort = TRUE) %>% head(10)
print(top_requesters)
```

    # A tibble: 10 × 2
       id.orig_h           n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

1.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений

``` r
top_domains <- dns %>%
  count(query, sort = TRUE) %>% head(10)
print(top_domains)
```

    # A tibble: 10 × 2
       query                                                                       n
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x… 10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

1.  Опеределите базовые статистические характеристики (функция
    summary()) интервала времени между последовательными обращениями к
    топ-10 доменам

``` r
top_domains <- dns %>%
  count(query, sort = TRUE) %>% head(10)
for(domain in top_domains) {
  cat("\nДомен:", domain, "\n")
  diffs <- dns %>%
    filter(query == domain) %>%
    arrange(ts) %>%
    pull(ts) %>%
    as.numeric() %>%
    diff()
  if(length(diffs) > 0) {
    print(summary(diffs))}}
```


    Домен: teredo.ipv6.microsoft.com tools.google.com www.apple.com time.apple.com safebrowsing.clients.google.com *\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 WPAD 44.206.168.192.in-addr.arpa HPE8AA67 ISATAP 

    Warning: There was 1 warning in `filter()`.
    ℹ In argument: `query == domain`.
    Caused by warning in `query == domain`:
    ! longer object length is not a multiple of shorter object length

         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.330     1.890     8.924     5.450 49697.680 

    Домен: 39273 14057 13390 13109 11658 10401 9134 7248 6929 6569 

    Warning: There was 1 warning in `filter()`.
    ℹ In argument: `query == domain`.
    Caused by warning in `query == domain`:
    ! longer object length is not a multiple of shorter object length

1.  Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?

``` r
top_domains <- dns %>%
  count(query, sort = TRUE) %>% head(10)
suspicious <- dns %>%
  filter(query %in% top_domains) %>% 
  group_by(id.orig_h, query) %>%
  filter(n() >= 5) %>%  
  summarise(
    requests = n(),
    time_diffs = diff(as.numeric(ts)),  
    mean_diff = mean(time_diffs),
    sd_diff = sd(time_diffs),
    .groups = 'drop') %>%
  filter(
    requests >= 5,   
    sd_diff < 60,     
    mean_diff < 300) %>%
  arrange(sd_diff)
```

    Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    dplyr 1.1.0.
    ℹ Please use `reframe()` instead.
    ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
      always returns an ungrouped data frame and adjust accordingly.

``` r
if(nrow(suspicious) > 0) {
  cat("Подозрительные IP:")
  print(suspicious)}
```

1.  Определите местоположение (страну, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние сервисы,
    например http://ip-api.com (API-эндпоинт – http://ip-api.com/json).

``` r
library(jsonlite)

get_ip_info <- function(ip) {
  url <- paste0("http://ip-api.com/json/", ip)
  response <- fromJSON(url)
  if (response$status == "success") {
    return(data.frame(
      ip = ip,
      country = response$country,
      city = response$city,
      isp = response$isp))} 
  else {
    return(data.frame(ip = ip, country = NA, city = NA, isp = NA))}}
top_ips <- dns %>%
  filter(query %in% top_domains$query) %>%
  distinct(id.resp_h) %>%
  pull(id.resp_h) %>% head(10)
results <- lapply(top_ips, get_ip_info)
geo_df <- do.call(rbind, results)

print(geo_df)
```

                    ip country city isp
    1   192.168.27.203      NA   NA  NA
    2  192.168.202.255      NA   NA  NA
    3     10.7.136.159      NA   NA  NA
    4   192.168.27.202      NA   NA  NA
    5     172.19.1.100      NA   NA  NA
    6     10.7.137.108      NA   NA  NA
    7     10.7.136.109      NA   NA  NA
    8    192.168.207.4      NA   NA  NA
    9      10.7.136.63      NA   NA  NA
    10  192.168.206.44      NA   NA  NA

## Оценка результата

В результате практической работы получили опыт анализа DNS-трафика с
помощью языка R

## Вывод

Научились анализировать DNS-логи в R. Нашли самых активных
пользователей, популярные сайты и проверили сеть на подозрительную
активность.
