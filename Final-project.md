---
title: "Final project"
author: "Austin"
date: "11/26/2022"
output: 
  html_document:
    keep_md: TRUE
editor_options: 
  chunk_output_type: console
---



```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
## ✔ tibble  3.1.8     ✔ dplyr   1.0.8
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(downloader)
library(readxl)
library(dplyr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
## 
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(data.table)
```

```
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:lubridate':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
##     yday, year
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
## 
## The following object is masked from 'package:purrr':
## 
##     transpose
```

```r
library(maps)
```

```
## 
## Attaching package: 'maps'
## 
## The following object is masked from 'package:purrr':
## 
##     map
```

```r
library(USAboundaries)
library(ggsflabel)
```

```
## 
## Attaching package: 'ggsflabel'
## 
## The following objects are masked from 'package:ggplot2':
## 
##     geom_sf_label, geom_sf_text, StatSfCoordinates
```

```r
library(sf)
```

```
## Linking to GEOS 3.10.2, GDAL 3.4.2, PROJ 8.2.1; sf_use_s2() is TRUE
```

```r
library(tmap)
library(rnaturalearth)
```


```r
dat <- read_csv("globalterrorismdb_0718dist.csv")
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```
## Rows: 181691 Columns: 135
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (55): approxdate, resolution, country_txt, region_txt, provstate, city, ...
## dbl (75): eventid, iyear, imonth, iday, extended, country, region, latitude,...
## lgl  (5): gsubname3, weaptype4, weaptype4_txt, weapsubtype4, weapsubtype4_txt
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
head(dat)
```

```
## # A tibble: 6 × 135
##        eventid iyear imonth  iday appro…¹ exten…² resol…³ country count…⁴ region
##          <dbl> <dbl>  <dbl> <dbl> <chr>     <dbl> <chr>     <dbl> <chr>    <dbl>
## 1 197000000001  1970      7     2 <NA>          0 <NA>         58 Domini…      2
## 2 197000000002  1970      0     0 <NA>          0 <NA>        130 Mexico       1
## 3 197001000001  1970      1     0 <NA>          0 <NA>        160 Philip…      5
## 4 197001000002  1970      1     0 <NA>          0 <NA>         78 Greece       8
## 5 197001000003  1970      1     0 <NA>          0 <NA>        101 Japan        4
## 6 197001010002  1970      1     1 <NA>          0 <NA>        217 United…      1
## # … with 125 more variables: region_txt <chr>, provstate <chr>, city <chr>,
## #   latitude <dbl>, longitude <dbl>, specificity <dbl>, vicinity <dbl>,
## #   location <chr>, summary <chr>, crit1 <dbl>, crit2 <dbl>, crit3 <dbl>,
## #   doubtterr <dbl>, alternative <dbl>, alternative_txt <chr>, multiple <dbl>,
## #   success <dbl>, suicide <dbl>, attacktype1 <dbl>, attacktype1_txt <chr>,
## #   attacktype2 <dbl>, attacktype2_txt <chr>, attacktype3 <dbl>,
## #   attacktype3_txt <chr>, targtype1 <dbl>, targtype1_txt <chr>, …
```

```r
tail(dat)
```

```
## # A tibble: 6 × 135
##        eventid iyear imonth  iday appro…¹ exten…² resol…³ country count…⁴ region
##          <dbl> <dbl>  <dbl> <dbl> <chr>     <dbl> <chr>     <dbl> <chr>    <dbl>
## 1 201712310020  2017     12    31 <NA>          0 <NA>          4 Afghan…      6
## 2 201712310022  2017     12    31 <NA>          0 <NA>        182 Somalia     11
## 3 201712310029  2017     12    31 <NA>          0 <NA>        200 Syria       10
## 4 201712310030  2017     12    31 <NA>          0 <NA>        160 Philip…      5
## 5 201712310031  2017     12    31 <NA>          0 <NA>         92 India        6
## 6 201712310032  2017     12    31 <NA>          0 <NA>        160 Philip…      5
## # … with 125 more variables: region_txt <chr>, provstate <chr>, city <chr>,
## #   latitude <dbl>, longitude <dbl>, specificity <dbl>, vicinity <dbl>,
## #   location <chr>, summary <chr>, crit1 <dbl>, crit2 <dbl>, crit3 <dbl>,
## #   doubtterr <dbl>, alternative <dbl>, alternative_txt <chr>, multiple <dbl>,
## #   success <dbl>, suicide <dbl>, attacktype1 <dbl>, attacktype1_txt <chr>,
## #   attacktype2 <dbl>, attacktype2_txt <chr>, attacktype3 <dbl>,
## #   attacktype3_txt <chr>, targtype1 <dbl>, targtype1_txt <chr>, …
```
terroist organization

```r
second <- select(dat, gname, nkill)
head(second)
```

```
## # A tibble: 6 × 2
##   gname                              nkill
##   <chr>                              <dbl>
## 1 MANO-D                                 1
## 2 23rd of September Communist League     0
## 3 Unknown                                1
## 4 Unknown                               NA
## 5 Unknown                               NA
## 6 Black Nationalists                     0
```

```r
tail(second)
```

```
## # A tibble: 6 × 2
##   gname                                      nkill
##   <chr>                                      <dbl>
## 1 Unknown                                        0
## 2 Al-Shabaab                                     1
## 3 Muslim extremists                              2
## 4 Bangsamoro Islamic Freedom Movement (BIFM)     0
## 5 Unknown                                        0
## 6 Unknown                                        0
```

```r
third <- second %>%
  select(nkill, gname) %>%
  arrange(gname) %>%
  filter(gname != "Unknown")
head(third)
```

```
## # A tibble: 6 × 2
##   nkill gname
##   <dbl> <chr>
## 1     1 1 May
## 2     1 1 May
## 3     0 1 May
## 4     0 1 May
## 5     0 1 May
## 6     0 1 May
```

```r
tail(third)
```

```
## # A tibble: 6 × 2
##   nkill gname                        
##   <dbl> <chr>                        
## 1     0 Zomi Revolutionary Army (ZRA)
## 2    21 Zulu Militants               
## 3     1 Zulu Miners                  
## 4     0 Zuwar al-Imam Rida           
## 5     0 Zviadists                    
## 6     5 Zwai Tribe
```

```r
no_zero1 <- third %>% 
  filter(nkill != 0)
head(no_zero1)
```

```
## # A tibble: 6 × 2
##   nkill gname                                                
##   <dbl> <chr>                                                
## 1     1 1 May                                                
## 2     1 1 May                                                
## 3     5 14 March Coalition                                   
## 4     1 16 January Organization for the Liberation of Tripoli
## 5     8 1920 Revolution Brigades                             
## 6     7 1920 Revolution Brigades
```

```r
by_cyl <- no_zero1 %>% group_by(gname)
by_cyl
```

```
## # A tibble: 47,178 × 2
## # Groups:   gname [1,759]
##    nkill gname                                                
##    <dbl> <chr>                                                
##  1     1 1 May                                                
##  2     1 1 May                                                
##  3     5 14 March Coalition                                   
##  4     1 16 January Organization for the Liberation of Tripoli
##  5     8 1920 Revolution Brigades                             
##  6     7 1920 Revolution Brigades                             
##  7     1 19th of July Christian Resistance Brigade            
##  8     1 20 December Movement (M-20)                          
##  9     1 23 May Democratic Alliance (Algeria)                 
## 10     1 23rd of September Communist League                   
## # … with 47,168 more rows
```

```r
DT <- data.table(no_zero1)
maybe <- DT[, sum(nkill), by = gname]
order <- maybe %>%
arrange(desc(V1))
order
```

```
##                                             gname    V1
##    1: Islamic State of Iraq and the Levant (ISIL) 38923
##    2:                                     Taliban 29410
##    3:                                  Boko Haram 20328
##    4:                           Shining Path (SL) 11601
##    5:     Liberation Tigers of Tamil Eelam (LTTE) 10989
##   ---                                                  
## 1755:                      Youths of Ali Movement     1
## 1756:                                  Zemun Clan     1
## 1757:                                       Zetas     1
## 1758:                 Zionist Resistance Fighters     1
## 1759:                                 Zulu Miners     1
```

```r
worst25<- head(order, n=25)
#1
graph25 <- ggplot(data = worst25) +
  geom_col(mapping = aes(x = reorder(gname , +V1), y = V1, fill= gname))+
  theme(legend.position = "none") +
  labs(title = "Group death toll") +
  ylab("Deaths") +
  xlab("Group name")
graph25 + coord_flip()
```

![](Final-project_files/figure-html/Groups-1.png)<!-- -->

```r
#I want to add numbers at the end of the col before using it as well take off the index need to add labels

worst10 <- head(order, n=10)
worst10
```

```
##                                                gname    V1
##  1:      Islamic State of Iraq and the Levant (ISIL) 38923
##  2:                                          Taliban 29410
##  3:                                       Boko Haram 20328
##  4:                                Shining Path (SL) 11601
##  5:          Liberation Tigers of Tamil Eelam (LTTE) 10989
##  6:                                       Al-Shabaab  9330
##  7: Farabundo Marti National Liberation Front (FMLN)  8065
##  8:                Nicaraguan Democratic Force (FDN)  6662
##  9:                  Tehrik-i-Taliban Pakistan (TTP)  6042
## 10:    Revolutionary Armed Forces of Colombia (FARC)  5661
```

```r
gggraph <- ggplot(data = worst10) +
  geom_col(mapping = aes(x = reorder(gname, +V1), y = V1))
gggraph+ coord_flip()
```

![](Final-project_files/figure-html/Groups-2.png)<!-- -->
overall deaths

```r
 newdate <- dat %>%
  select(iyear, imonth, iday, nkill, country_txt) %>%
  mutate(date = make_datetime(iyear, imonth, iday))
  head(newdate)
```

```
## # A tibble: 6 × 6
##   iyear imonth  iday nkill country_txt        date               
##   <dbl>  <dbl> <dbl> <dbl> <chr>              <dttm>             
## 1  1970      7     2     1 Dominican Republic 1970-07-02 00:00:00
## 2  1970      0     0     0 Mexico             1969-11-30 00:00:00
## 3  1970      1     0     1 Philippines        1969-12-31 00:00:00
## 4  1970      1     0    NA Greece             1969-12-31 00:00:00
## 5  1970      1     0    NA Japan              1969-12-31 00:00:00
## 6  1970      1     1     0 United States      1970-01-01 00:00:00
```

```r
newdate_ordered<- newdate %>%
  select(date, nkill, country_txt) %>%
  arrange(date)
head(newdate_ordered)
```

```
## # A tibble: 6 × 3
##   date                nkill country_txt  
##   <dttm>              <dbl> <chr>        
## 1 1969-11-30 00:00:00     0 Mexico       
## 2 1969-12-31 00:00:00     1 Philippines  
## 3 1969-12-31 00:00:00    NA Greece       
## 4 1969-12-31 00:00:00    NA Japan        
## 5 1970-01-01 00:00:00     0 United States
## 6 1970-01-01 00:00:00     0 United States
```

```r
no_zero <- newdate_ordered %>% 
  filter(nkill != 0)
head(no_zero)
```

```
## # A tibble: 6 × 3
##   date                nkill country_txt       
##   <dttm>              <dbl> <chr>             
## 1 1969-12-31 00:00:00     1 Philippines       
## 2 1970-01-11 00:00:00     1 Ethiopia          
## 3 1970-01-20 00:00:00     1 Guatemala         
## 4 1970-02-10 00:00:00     1 West Germany (FRG)
## 5 1970-02-13 00:00:00     7 West Germany (FRG)
## 6 1970-02-16 00:00:00     1 United States
```

```r
na.omit(no_zero)
```

```
## # A tibble: 83,229 × 3
##    date                nkill country_txt       
##    <dttm>              <dbl> <chr>             
##  1 1969-12-31 00:00:00     1 Philippines       
##  2 1970-01-11 00:00:00     1 Ethiopia          
##  3 1970-01-20 00:00:00     1 Guatemala         
##  4 1970-02-10 00:00:00     1 West Germany (FRG)
##  5 1970-02-13 00:00:00     7 West Germany (FRG)
##  6 1970-02-16 00:00:00     1 United States     
##  7 1970-02-21 00:00:00    47 Switzerland       
##  8 1970-03-05 00:00:00     2 United States     
##  9 1970-03-09 00:00:00     2 United States     
## 10 1970-03-20 00:00:00     1 United States     
## # … with 83,219 more rows
```

```r
#2
World <- ggplot(no_zero, aes(x = date, y = nkill)) +            # Draw ggplot2 plot
  geom_line()+ 
  labs(title = "Terrorism from 1970-2020") +
  ylab("Deaths") +
  xlab("Date")+
  theme_bw()
World
```

![](Final-project_files/figure-html/Overall terroirsm-1.png)<!-- -->

```r
#this is weird that there is a gap from somewhere in 1994


five_largest_events<- no_zero %>%
  filter(country_txt %in%  c("Iraq", "United States", "Rwanda","Somalia","Iran"))

fivegraph <- ggplot(five_largest_events, aes(x = date, y = nkill, color= country_txt)) +            # Draw ggplot2 plot
  geom_line()+ 
    labs(title = "Terrorism worst events from 1970-2020") +
  ylab("Deaths") +
  xlab("Date")+
  theme_bw()
fivegraph
```

![](Final-project_files/figure-html/Overall terroirsm-2.png)<!-- -->

```r
#Cinema Rex fire Iraq 1978
#Rewanda genocide 1994
#9/11/2000
#Iraq war many diffrent things



Top_5 <- no_zero %>%
  filter(country_txt %in%  c("Afghanistan", "Iraq", "Somalia", "United States", "Syria", "Rwanda"))
Top_5
```

```
## # A tibble: 28,828 × 3
##    date                nkill country_txt  
##    <dttm>              <dbl> <chr>        
##  1 1970-02-16 00:00:00     1 United States
##  2 1970-03-05 00:00:00     2 United States
##  3 1970-03-09 00:00:00     2 United States
##  4 1970-03-20 00:00:00     1 United States
##  5 1970-04-24 00:00:00     1 United States
##  6 1970-05-09 00:00:00     1 United States
##  7 1970-05-15 00:00:00     1 United States
##  8 1970-05-22 00:00:00     1 United States
##  9 1970-05-28 00:00:00     1 United States
## 10 1970-06-19 00:00:00     1 United States
## # … with 28,818 more rows
```

```r
Topgraph <- ggplot(Top_5, aes(x = date, y = nkill, color= country_txt)) +            # Draw ggplot2 plot
  geom_line()+ 
  theme_bw()
Topgraph
```

![](Final-project_files/figure-html/Overall terroirsm-3.png)<!-- -->

```r
Top_10 <- no_zero %>%
   filter(country_txt %in%  c("Afghanistan", "Iraq", "Somalia", "Burkina Faso", "Syria", "Nigeria", "Mali", "Niger", "Myanmar", "Pakistan"))

Top10graph <- ggplot(Top_10, aes(x = date, y = nkill, color= country_txt)) +            # Draw ggplot2 plot
  geom_line()+ 
  theme_bw()
Top10graph
```

![](Final-project_files/figure-html/Overall terroirsm-4.png)<!-- -->

```r
#us
US_only <- no_zero %>%
  filter(country_txt == "United States")
US_only
```

```
## # A tibble: 283 × 3
##    date                nkill country_txt  
##    <dttm>              <dbl> <chr>        
##  1 1970-02-16 00:00:00     1 United States
##  2 1970-03-05 00:00:00     2 United States
##  3 1970-03-09 00:00:00     2 United States
##  4 1970-03-20 00:00:00     1 United States
##  5 1970-04-24 00:00:00     1 United States
##  6 1970-05-09 00:00:00     1 United States
##  7 1970-05-15 00:00:00     1 United States
##  8 1970-05-22 00:00:00     1 United States
##  9 1970-05-28 00:00:00     1 United States
## 10 1970-06-19 00:00:00     1 United States
## # … with 273 more rows
```

```r
USgraph <- ggplot(US_only, aes(x = date, y = nkill)) +            # Draw ggplot2 plot
  geom_line()+ 
  theme_bw()
USgraph
```

![](Final-project_files/figure-html/Overall terroirsm-5.png)<!-- -->
united states

```r
dat2 <-  dat %>%
  select(country_txt, city, nkill, iyear, imonth, iday, longitude, latitude, provstate, region_txt) %>%
  mutate(date = make_datetime(iyear, imonth, iday))
head(dat2)
```

```
## # A tibble: 6 × 11
##   country_txt     city  nkill iyear imonth  iday longi…¹ latit…² provs…³ regio…⁴
##   <chr>           <chr> <dbl> <dbl>  <dbl> <dbl>   <dbl>   <dbl> <chr>   <chr>  
## 1 Dominican Repu… Sant…     1  1970      7     2   -70.0    18.5 <NA>    Centra…
## 2 Mexico          Mexi…     0  1970      0     0   -99.1    19.4 Federal North …
## 3 Philippines     Unkn…     1  1970      1     0   121.     15.5 Tarlac  Southe…
## 4 Greece          Athe…    NA  1970      1     0    23.8    38.0 Attica  Wester…
## 5 Japan           Fuko…    NA  1970      1     0   130.     33.6 Fukouka East A…
## 6 United States   Cairo     0  1970      1     1   -89.2    37.0 Illino… North …
## # … with 1 more variable: date <dttm>, and abbreviated variable names
## #   ¹​longitude, ²​latitude, ³​provstate, ⁴​region_txt
```

```r
citys <- dat2 %>%
  select(country_txt,city, nkill, date, longitude, latitude, provstate, region_txt)
citys
```

```
## # A tibble: 181,691 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 Dominican Re… Sant…     1 1970-07-02 00:00:00   -70.0    18.5 <NA>    Centra…
##  2 Mexico        Mexi…     0 1969-11-30 00:00:00   -99.1    19.4 Federal North …
##  3 Philippines   Unkn…     1 1969-12-31 00:00:00   121.     15.5 Tarlac  Southe…
##  4 Greece        Athe…    NA 1969-12-31 00:00:00    23.8    38.0 Attica  Wester…
##  5 Japan         Fuko…    NA 1969-12-31 00:00:00   130.     33.6 Fukouka East A…
##  6 United States Cairo     0 1970-01-01 00:00:00   -89.2    37.0 Illino… North …
##  7 Uruguay       Mont…     0 1970-01-02 00:00:00   -56.2   -34.9 Montev… South …
##  8 United States Oakl…     0 1970-01-02 00:00:00  -122.     37.8 Califo… North …
##  9 United States Madi…     0 1970-01-02 00:00:00   -89.4    43.1 Wiscon… North …
## 10 United States Madi…     0 1970-01-03 00:00:00   -89.4    43.1 Wiscon… North …
## # … with 181,681 more rows, and abbreviated variable names ¹​longitude,
## #   ²​latitude, ³​provstate, ⁴​region_txt
```

```r
united <- citys %>%
  filter(country_txt == "United States")
united
```

```
## # A tibble: 2,836 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 United States Cairo     0 1970-01-01 00:00:00   -89.2    37.0 Illino… North …
##  2 United States Oakl…     0 1970-01-02 00:00:00  -122.     37.8 Califo… North …
##  3 United States Madi…     0 1970-01-02 00:00:00   -89.4    43.1 Wiscon… North …
##  4 United States Madi…     0 1970-01-03 00:00:00   -89.4    43.1 Wiscon… North …
##  5 United States Bara…     0 1970-01-01 00:00:00   -89.7    43.5 Wiscon… North …
##  6 United States Denv…     0 1970-01-06 00:00:00  -105.     39.8 Colora… North …
##  7 United States Detr…     0 1970-01-09 00:00:00   -83.0    42.3 Michig… North …
##  8 United States Rio …     0 1970-01-09 00:00:00   -66.1    18.4 Puerto… North …
##  9 United States New …     0 1970-01-12 00:00:00   -73.9    40.7 New Yo… North …
## 10 United States Rio …     0 1970-01-12 00:00:00   -65.8    18.4 Puerto… North …
## # … with 2,826 more rows, and abbreviated variable names ¹​longitude, ²​latitude,
## #   ³​provstate, ⁴​region_txt
```

```r
tail(united, n=20)
```

```
## # A tibble: 20 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 United States Bloo…     0 2017-08-05 00:00:00   -93.3    44.9 Minnes… North …
##  2 United States Char…     1 2017-08-12 00:00:00   -78.5    38.0 Virgin… North …
##  3 United States Hous…     0 2017-08-19 00:00:00   -95.4    29.8 Texas   North …
##  4 United States Bato…     0 2017-09-11 00:00:00   -91.2    30.5 Louisi… North …
##  5 United States Bato…     1 2017-09-12 00:00:00   -91.2    30.4 Louisi… North …
##  6 United States Bato…     1 2017-09-14 00:00:00   -91.2    30.4 Louisi… North …
##  7 United States Anti…     1 2017-09-24 00:00:00   -86.6    36.0 Tennes… North …
##  8 United States Las …    59 2017-10-01 00:00:00  -115.     36.1 Nevada  North …
##  9 United States Flet…     0 2017-10-06 00:00:00   -82.5    35.4 North … North …
## 10 United States Spok…     0 2017-10-08 00:00:00  -117.     47.7 Washin… North …
## 11 United States Oxfo…     0 2017-10-22 00:00:00   -99.6    40.3 Nebras… North …
## 12 United States San …     0 2017-10-28 00:00:00   -66.1    18.4 Puerto… North …
## 13 United States New …     8 2017-10-31 00:00:00   -73.9    40.7 New Yo… North …
## 14 United States Rich…     0 2017-10-31 00:00:00   -91.8    32.4 Louisi… North …
## 15 United States Cham…     0 2017-11-07 00:00:00   -88.2    40.1 Illino… North …
## 16 United States Vale      0 2017-11-13 00:00:00   -81.4    35.6 North … North …
## 17 United States Aztec     3 2017-12-07 00:00:00  -108.     36.8 New Me… North …
## 18 United States New …     0 2017-12-11 00:00:00   -73.9    40.7 New Yo… North …
## 19 United States Harr…     0 2017-12-22 00:00:00   -76.9    40.3 Pennsy… North …
## 20 United States Harr…     1 2017-12-22 00:00:00   -76.9    40.3 Pennsy… North …
## # … with abbreviated variable names ¹​longitude, ²​latitude, ³​provstate,
## #   ⁴​region_txt
```

```r
united1 <- united %>%
  filter(city != "Unknown")
united2 <- united1 %>%
  filter(provstate != "Alaska")
united2
```

```
## # A tibble: 2,816 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 United States Cairo     0 1970-01-01 00:00:00   -89.2    37.0 Illino… North …
##  2 United States Oakl…     0 1970-01-02 00:00:00  -122.     37.8 Califo… North …
##  3 United States Madi…     0 1970-01-02 00:00:00   -89.4    43.1 Wiscon… North …
##  4 United States Madi…     0 1970-01-03 00:00:00   -89.4    43.1 Wiscon… North …
##  5 United States Bara…     0 1970-01-01 00:00:00   -89.7    43.5 Wiscon… North …
##  6 United States Denv…     0 1970-01-06 00:00:00  -105.     39.8 Colora… North …
##  7 United States Detr…     0 1970-01-09 00:00:00   -83.0    42.3 Michig… North …
##  8 United States Rio …     0 1970-01-09 00:00:00   -66.1    18.4 Puerto… North …
##  9 United States New …     0 1970-01-12 00:00:00   -73.9    40.7 New Yo… North …
## 10 United States Rio …     0 1970-01-12 00:00:00   -65.8    18.4 Puerto… North …
## # … with 2,806 more rows, and abbreviated variable names ¹​longitude, ²​latitude,
## #   ³​provstate, ⁴​region_txt
```

```r
united3 <- united2 %>%
  filter(provstate != "Hawaii")
united3
```

```
## # A tibble: 2,812 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 United States Cairo     0 1970-01-01 00:00:00   -89.2    37.0 Illino… North …
##  2 United States Oakl…     0 1970-01-02 00:00:00  -122.     37.8 Califo… North …
##  3 United States Madi…     0 1970-01-02 00:00:00   -89.4    43.1 Wiscon… North …
##  4 United States Madi…     0 1970-01-03 00:00:00   -89.4    43.1 Wiscon… North …
##  5 United States Bara…     0 1970-01-01 00:00:00   -89.7    43.5 Wiscon… North …
##  6 United States Denv…     0 1970-01-06 00:00:00  -105.     39.8 Colora… North …
##  7 United States Detr…     0 1970-01-09 00:00:00   -83.0    42.3 Michig… North …
##  8 United States Rio …     0 1970-01-09 00:00:00   -66.1    18.4 Puerto… North …
##  9 United States New …     0 1970-01-12 00:00:00   -73.9    40.7 New Yo… North …
## 10 United States Rio …     0 1970-01-12 00:00:00   -65.8    18.4 Puerto… North …
## # … with 2,802 more rows, and abbreviated variable names ¹​longitude, ²​latitude,
## #   ³​provstate, ⁴​region_txt
```

```r
united4 <- united3 %>%
  filter(provstate != "Puerto Rico")
united4
```

```
## # A tibble: 2,574 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 United States Cairo     0 1970-01-01 00:00:00   -89.2    37.0 Illino… North …
##  2 United States Oakl…     0 1970-01-02 00:00:00  -122.     37.8 Califo… North …
##  3 United States Madi…     0 1970-01-02 00:00:00   -89.4    43.1 Wiscon… North …
##  4 United States Madi…     0 1970-01-03 00:00:00   -89.4    43.1 Wiscon… North …
##  5 United States Bara…     0 1970-01-01 00:00:00   -89.7    43.5 Wiscon… North …
##  6 United States Denv…     0 1970-01-06 00:00:00  -105.     39.8 Colora… North …
##  7 United States Detr…     0 1970-01-09 00:00:00   -83.0    42.3 Michig… North …
##  8 United States New …     0 1970-01-12 00:00:00   -73.9    40.7 New Yo… North …
##  9 United States Seat…     0 1970-01-13 00:00:00  -122.     47.6 Washin… North …
## 10 United States Cham…     0 1970-01-14 00:00:00   -88.2    40.1 Illino… North …
## # … with 2,564 more rows, and abbreviated variable names ¹​longitude, ²​latitude,
## #   ³​provstate, ⁴​region_txt
```

```r
fixed <- united4 %>%
  filter(nkill != 0)
fixed
```

```
## # A tibble: 263 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 United States San …     1 1970-02-16 00:00:00  -122.     37.8 Califo… North …
##  2 United States Bel …     2 1970-03-09 00:00:00   -76.3    39.5 Maryla… North …
##  3 United States Detr…     1 1970-03-20 00:00:00   -83.0    42.3 Michig… North …
##  4 United States Balt…     1 1970-04-24 00:00:00   -76.6    39.3 Maryla… North …
##  5 United States Sacr…     1 1970-05-09 00:00:00  -121.     38.6 Califo… North …
##  6 United States Seat…     1 1970-05-15 00:00:00  -122.     47.6 Washin… North …
##  7 United States St. …     1 1970-05-22 00:00:00   -93.1    44.9 Minnes… North …
##  8 United States Sacr…     1 1970-05-28 00:00:00  -121.     38.6 Califo… North …
##  9 United States Chic…     1 1970-06-19 00:00:00   -87.7    41.8 Illino… North …
## 10 United States San …     1 1970-06-19 00:00:00  -122.     37.8 Califo… North …
## # … with 253 more rows, and abbreviated variable names ¹​longitude, ²​latitude,
## #   ³​provstate, ⁴​region_txt
```

```r
ordered <- fixed %>%
  arrange(desc(nkill))
ordered
```

```
## # A tibble: 263 × 8
##    country_txt   city  nkill date                longi…¹ latit…² provs…³ regio…⁴
##    <chr>         <chr> <dbl> <dttm>                <dbl>   <dbl> <chr>   <chr>  
##  1 United States New …  1384 2001-09-11 00:00:00   -73.9    40.7 New Yo… North …
##  2 United States New …  1383 2001-09-11 00:00:00   -73.9    40.7 New Yo… North …
##  3 United States Arli…   190 2001-09-11 00:00:00   -77.1    38.9 Virgin… North …
##  4 United States Okla…   168 1995-04-19 00:00:00   -97.5    35.5 Oklaho… North …
##  5 United States Las …    59 2017-10-01 00:00:00  -115.     36.1 Nevada  North …
##  6 United States Orla…    50 2016-06-12 00:00:00   -81.4    28.5 Florida North …
##  7 United States Shan…    44 2001-09-11 00:00:00   -78.9    40.0 Pennsy… North …
##  8 United States San …    16 2015-12-02 00:00:00  -117.     34.1 Califo… North …
##  9 United States Litt…    15 1999-04-20 00:00:00  -105.     39.6 Colora… North …
## 10 United States West     15 2013-04-17 00:00:00   -97.1    31.8 Texas   North …
## # … with 253 more rows, and abbreviated variable names ¹​longitude, ²​latitude,
## #   ³​provstate, ⁴​region_txt
```

```r
mapready <- fixed %>%
  select(city, nkill, date, longitude ,latitude)
head(mapready)
```

```
## # A tibble: 6 × 5
##   city          nkill date                longitude latitude
##   <chr>         <dbl> <dttm>                  <dbl>    <dbl>
## 1 San Francisco     1 1970-02-16 00:00:00    -122.      37.8
## 2 Bel Air           2 1970-03-09 00:00:00     -76.3     39.5
## 3 Detroit           1 1970-03-20 00:00:00     -83.0     42.3
## 4 Baltimore         1 1970-04-24 00:00:00     -76.6     39.3
## 5 Sacramento        1 1970-05-09 00:00:00    -121.      38.6
## 6 Seattle           1 1970-05-15 00:00:00    -122.      47.6
```

```r
#need to put onto map
states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))
newcord <- ordered %>% st_as_sf( coords = c("longitude", "latitude"), crs=4326)
new <- st_transform(newcord, crs=2163)
head(newcord)
```

```
## Simple feature collection with 6 features and 6 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: -115.1359 ymin: 28.5196 xmax: -73.93135 ymax: 40.69713
## Geodetic CRS:  WGS 84
## # A tibble: 6 × 7
##   country_txt   city          nkill date                provstate region_txt   
##   <chr>         <chr>         <dbl> <dttm>              <chr>     <chr>        
## 1 United States New York City  1384 2001-09-11 00:00:00 New York  North America
## 2 United States New York City  1383 2001-09-11 00:00:00 New York  North America
## 3 United States Arlington       190 2001-09-11 00:00:00 Virginia  North America
## 4 United States Oklahoma City   168 1995-04-19 00:00:00 Oklahoma  North America
## 5 United States Las Vegas        59 2017-10-01 00:00:00 Nevada    North America
## 6 United States Orlando          50 2016-06-12 00:00:00 Florida   North America
## # … with 1 more variable: geometry <POINT [°]>
```

```r
USgraph <- ggplot() +
  geom_sf(data = states, aes(fill= 'White'))+ 
  geom_sf(data = new,aes(size= nkill))+
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(color = "black", linetype = 2))+
  labs(title = "United States event map") +
  ylab("") +
  xlab("")
USgraph
```

![](Final-project_files/figure-html/United States map-1.png)<!-- -->
kills by weapon type

```r
weapondata <- dat %>%
  select(weaptype1, weaptype1_txt,gname, nkill )
weapondata
```

```
## # A tibble: 181,691 × 4
##    weaptype1 weaptype1_txt gname                              nkill
##        <dbl> <chr>         <chr>                              <dbl>
##  1        13 Unknown       MANO-D                                 1
##  2        13 Unknown       23rd of September Communist League     0
##  3        13 Unknown       Unknown                                1
##  4         6 Explosives    Unknown                               NA
##  5         8 Incendiary    Unknown                               NA
##  6         5 Firearms      Black Nationalists                     0
##  7         5 Firearms      Tupamaros (Uruguay)                    0
##  8         6 Explosives    Unknown                                0
##  9         8 Incendiary    New Year's Gang                        0
## 10         8 Incendiary    New Year's Gang                        0
## # … with 181,681 more rows
```

```r
weaopon <- weapondata %>%
  arrange(weaptype1)
weaopon
```

```
## # A tibble: 181,691 × 4
##    weaptype1 weaptype1_txt gname          nkill
##        <dbl> <chr>         <chr>          <dbl>
##  1         1 Biological    Dark Harvest       0
##  2         1 Biological    Dark Harvest       0
##  3         1 Biological    Rajneeshees        0
##  4         1 Biological    Rajneeshees        0
##  5         1 Biological    Rajneeshees        0
##  6         1 Biological    Rajneeshees        0
##  7         1 Biological    Aum Shinri Kyo     0
##  8         1 Biological    Aum Shinri Kyo     0
##  9         1 Biological    Unknown            2
## 10         1 Biological    Unknown            1
## # … with 181,681 more rows
```

```r
no_unknown <- weaopon %>%
  filter(weaptype1_txt != "Unknown")
no_unknown
```

```
## # A tibble: 166,534 × 4
##    weaptype1 weaptype1_txt gname          nkill
##        <dbl> <chr>         <chr>          <dbl>
##  1         1 Biological    Dark Harvest       0
##  2         1 Biological    Dark Harvest       0
##  3         1 Biological    Rajneeshees        0
##  4         1 Biological    Rajneeshees        0
##  5         1 Biological    Rajneeshees        0
##  6         1 Biological    Rajneeshees        0
##  7         1 Biological    Aum Shinri Kyo     0
##  8         1 Biological    Aum Shinri Kyo     0
##  9         1 Biological    Unknown            2
## 10         1 Biological    Unknown            1
## # … with 166,524 more rows
```

```r
yes <- no_unknown %>%
  filter(nkill != "0")
yes
```

```
## # A tibble: 76,432 × 4
##    weaptype1 weaptype1_txt gname                        nkill
##        <dbl> <chr>         <chr>                        <dbl>
##  1         1 Biological    Unknown                          2
##  2         1 Biological    Unknown                          1
##  3         1 Biological    Unknown                          2
##  4         1 Biological    Unknown                          2
##  5         1 Biological    Unknown                          1
##  6         1 Biological    Unknown                          1
##  7         2 Chemical      Ulster Volunteer Force (UVF)     1
##  8         2 Chemical      Unknown                          1
##  9         2 Chemical      Unknown                          1
## 10         2 Chemical      Unknown                          1
## # … with 76,422 more rows
```

```r
DT <- data.table(yes)
maybe1 <- DT[, sum(nkill), by = weaptype1_txt]
head(maybe1)
```

```
##    weaptype1_txt     V1
## 1:    Biological      9
## 2:      Chemical    624
## 3:  Radiological      2
## 4:      Firearms 174894
## 5:    Explosives 174277
## 6:  Fake Weapons      1
```

```r
order1 <- maybe1 %>%
arrange(desc(V1))
order1
```

```
##                                                                   weaptype1_txt
##  1:                                                                    Firearms
##  2:                                                                  Explosives
##  3:                                                                       Melee
##  4:                                                                  Incendiary
##  5: Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)
##  6:                                                                    Chemical
##  7:                                                                       Other
##  8:                                                          Sabotage Equipment
##  9:                                                                  Biological
## 10:                                                                Radiological
## 11:                                                                Fake Weapons
##         V1
##  1: 174894
##  2: 174277
##  3:  10575
##  4:   5476
##  5:   3184
##  6:    624
##  7:    123
##  8:     83
##  9:      9
## 10:      2
## 11:      1
```

```r
crack <- ggplot(data = order1) +
  geom_col(mapping = aes(x = reorder(weaptype1_txt , +V1), y = V1, fill=weaptype1_txt))+
  theme(legend.position = "none") +
  labs(title = "Most used Weapons") +
  ylab("Deaths") +
  xlab("Weapon")
crack + coord_flip()
```

![](Final-project_files/figure-html/Weapon-1.png)<!-- -->

