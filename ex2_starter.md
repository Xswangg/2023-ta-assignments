Exercise 2-Group 1
================

## Load data

Load the following data: + applications from `app_data_sample.parquet` +
edges from `edges_sample.csv`

``` r
# import data
data_path <- "/Users/jessica.song/Desktop/MG/!671 Talent Analytics/Excercise2/"
applications <- read_feather(paste0(data_path,"app_data_starter.feather"))

applications
```

    ## # A tibble: 2,018,477 × 21
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ℹ 2,018,467 more rows
    ## # ℹ 17 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender <chr>, race <chr>, earliest_date <date>,
    ## #   latest_date <date>, tenure_days <dbl>

## Get gender for examiners

We’ll get gender based on the first name of the examiner, which is
recorded in the field `examiner_name_first`. We’ll use library `gender`
for that, relying on a modified version of their own
[example](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html).

Note that there are over 2 million records in the applications table –
that’s because there are many records for each examiner, as many as the
number of applications that examiner worked on during this time frame.
Our first step therefore is to get all *unique* names in a separate list
`examiner_names`. We will then guess gender for each one and will join
this table back to the original dataset. So, let’s get names without
repetition:

``` r
# install.packages("genderdata")
install.packages("/Users/jessica.song/Downloads/genderdata-master", repos = NULL, type="source")
```

``` r
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it

# get a list of first names without repetitions
examiner_names <- applications %>% 
  distinct(examiner_name_first)

examiner_names
```

    ## # A tibble: 2,595 × 1
    ##    examiner_name_first
    ##    <chr>              
    ##  1 JACQUELINE         
    ##  2 BEKIR              
    ##  3 CYNTHIA            
    ##  4 MARY               
    ##  5 MICHAEL            
    ##  6 LINDA              
    ##  7 KARA               
    ##  8 VANESSA            
    ##  9 TERESA             
    ## 10 SUN                
    ## # ℹ 2,585 more rows

Now let’s use function `gender()` as shown in the example for the
package to attach a gender and probability to each name and put the
results into the table `examiner_names_gender`

``` r
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

examiner_names_gender
```

    ## # A tibble: 1,822 × 3
    ##    examiner_name_first gender proportion_female
    ##    <chr>               <chr>              <dbl>
    ##  1 AARON               male              0.0082
    ##  2 ABDEL               male              0     
    ##  3 ABDOU               male              0     
    ##  4 ABDUL               male              0     
    ##  5 ABDULHAKIM          male              0     
    ##  6 ABDULLAH            male              0     
    ##  7 ABDULLAHI           male              0     
    ##  8 ABIGAIL             female            0.998 
    ##  9 ABIMBOLA            female            0.944 
    ## 10 ABRAHAM             male              0.0031
    ## # ℹ 1,812 more rows

Finally, let’s join that table back to our original applications data
and discard the temporary tables we have just created to reduce clutter
in our environment.

``` r
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) limit (Mb)  max used  (Mb)
    ## Ncells  4481207 239.4    7452397 398.1         NA   4930629 263.4
    ## Vcells 59566401 454.5  119601668 912.5      16384 104011879 793.6

## Guess the examiner’s race

We’ll now use package `wru` to estimate likely race of an examiner. Just
like with gender, we’ll get a list of unique names first, only now we
are using surnames.

``` r
library(wru)

examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_surnames
```

    ## # A tibble: 3,806 × 1
    ##    surname   
    ##    <chr>     
    ##  1 HOWARD    
    ##  2 YILDIRIM  
    ##  3 HAMILTON  
    ##  4 MOSHER    
    ##  5 BARR      
    ##  6 GRAY      
    ##  7 MCMILLIAN 
    ##  8 FORD      
    ##  9 STRZELECKA
    ## 10 KIM       
    ## # ℹ 3,796 more rows

We’ll follow the instructions for the package outlined here
<https://github.com/kosukeimai/wru>.

``` r
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## Warning: Unknown or uninitialised column: `state`.

    ## Proceeding with last name predictions...

    ## ℹ All local files already up-to-date!

    ## 701 (18.4%) individuals' last names were not matched.

``` r
examiner_race
```

    ## # A tibble: 3,806 × 6
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ##  1 HOWARD       0.597   0.295    0.0275   0.00690   0.0741
    ##  2 YILDIRIM     0.807   0.0273   0.0694   0.0165    0.0798
    ##  3 HAMILTON     0.656   0.239    0.0286   0.00750   0.0692
    ##  4 MOSHER       0.915   0.00425  0.0291   0.00917   0.0427
    ##  5 BARR         0.784   0.120    0.0268   0.00830   0.0615
    ##  6 GRAY         0.640   0.252    0.0281   0.00748   0.0724
    ##  7 MCMILLIAN    0.322   0.554    0.0212   0.00340   0.0995
    ##  8 FORD         0.576   0.320    0.0275   0.00621   0.0697
    ##  9 STRZELECKA   0.472   0.171    0.220    0.0825    0.0543
    ## 10 KIM          0.0169  0.00282  0.00546  0.943     0.0319
    ## # ℹ 3,796 more rows

As you can see, we get probabilities across five broad US Census
categories: white, black, Hispanic, Asian and other. (Some of you may
correctly point out that Hispanic is not a race category in the US
Census, but these are the limitations of this package.)

Our final step here is to pick the race category that has the highest
probability for each last name and then join the table back to the main
applications table. See this example for comparing values across
columns: <https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/>.
And this one for `case_when()` function:
<https://dplyr.tidyverse.org/reference/case_when.html>.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race
```

    ## # A tibble: 3,806 × 8
    ##    surname    pred.whi pred.bla pred.his pred.asi pred.oth max_race_p race 
    ##    <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>      <dbl> <chr>
    ##  1 HOWARD       0.597   0.295    0.0275   0.00690   0.0741      0.597 white
    ##  2 YILDIRIM     0.807   0.0273   0.0694   0.0165    0.0798      0.807 white
    ##  3 HAMILTON     0.656   0.239    0.0286   0.00750   0.0692      0.656 white
    ##  4 MOSHER       0.915   0.00425  0.0291   0.00917   0.0427      0.915 white
    ##  5 BARR         0.784   0.120    0.0268   0.00830   0.0615      0.784 white
    ##  6 GRAY         0.640   0.252    0.0281   0.00748   0.0724      0.640 white
    ##  7 MCMILLIAN    0.322   0.554    0.0212   0.00340   0.0995      0.554 black
    ##  8 FORD         0.576   0.320    0.0275   0.00621   0.0697      0.576 white
    ##  9 STRZELECKA   0.472   0.171    0.220    0.0825    0.0543      0.472 white
    ## 10 KIM          0.0169  0.00282  0.00546  0.943     0.0319      0.943 Asian
    ## # ℹ 3,796 more rows

Let’s join the data back to the applications table.

``` r
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)

applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()
```

    ##            used  (Mb) gc trigger  (Mb) limit (Mb)  max used  (Mb)
    ## Ncells  4667118 249.3    7452397 398.1         NA   7309171 390.4
    ## Vcells 61909738 472.4  119601668 912.5      16384 118366405 903.1

## Examiner’s tenure

To figure out the timespan for which we observe each examiner in the
applications data, let’s find the first and the last observed date for
each examiner. We’ll first get examiner IDs and application dates in a
separate table, for ease of manipulation. We’ll keep examiner ID (the
field `examiner_id`), and earliest and latest dates for each application
(`filing_date` and `appl_status_date` respectively). We’ll use functions
in package `lubridate` to work with date and time values.

``` r
library(lubridate) # to work with dates

examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates
```

    ## # A tibble: 2,018,477 × 3
    ##    examiner_id filing_date appl_status_date  
    ##          <dbl> <date>      <chr>             
    ##  1       96082 2000-01-26  30jan2003 00:00:00
    ##  2       87678 2000-10-11  27sep2010 00:00:00
    ##  3       63213 2000-05-17  30mar2009 00:00:00
    ##  4       73788 2001-07-20  07sep2009 00:00:00
    ##  5       77294 2000-04-10  19apr2001 00:00:00
    ##  6       68606 2000-04-28  16jul2001 00:00:00
    ##  7       89557 2004-01-26  15may2017 00:00:00
    ##  8       97543 2000-06-23  03apr2002 00:00:00
    ##  9       98714 2000-02-04  27nov2002 00:00:00
    ## 10       65530 2002-02-20  23mar2009 00:00:00
    ## # ℹ 2,018,467 more rows

The dates look inconsistent in terms of formatting. Let’s make them
consistent. We’ll create new variables `start_date` and `end_date`.

``` r
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
```

Let’s now identify the earliest and the latest date for each examiner
and calculate the difference in days, which is their tenure in the
organization.

``` r
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)

examiner_dates
```

    ## # A tibble: 5,625 × 4
    ##    examiner_id earliest_date latest_date tenure_days
    ##          <dbl> <date>        <date>            <dbl>
    ##  1       59012 2004-07-28    2015-07-24         4013
    ##  2       59025 2009-10-26    2017-05-18         2761
    ##  3       59030 2005-12-12    2017-05-22         4179
    ##  4       59040 2007-09-11    2017-05-23         3542
    ##  5       59052 2001-08-21    2007-02-28         2017
    ##  6       59054 2000-11-10    2016-12-23         5887
    ##  7       59055 2004-11-02    2007-12-26         1149
    ##  8       59056 2000-03-24    2017-05-22         6268
    ##  9       59074 2000-01-31    2017-03-17         6255
    ## 10       59081 2011-04-21    2017-05-19         2220
    ## # ℹ 5,615 more rows

Joining back to the applications data.

``` r
applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()
```

    ##            used  (Mb) gc trigger   (Mb) limit (Mb)  max used  (Mb)
    ## Ncells  4675263 249.7    7452397  398.1         NA   7452397 398.1
    ## Vcells 67984013 518.7  145769555 1112.2      16384 121407963 926.3

Save file as processed variables, to skip these steps in the following
exercises.

``` r
write_feather(applications, paste0(data_path,"app_data_starter_coded.feather"))
```

# Rest of the exercise

``` r
# import data
data_path <- "/Users/jessica.song/Desktop/MG/!671 Talent Analytics/Excercise2/"
applications1 <- read_feather(paste0(data_path,"app_data_starter_coded.feather"))

applications1
```

    ## # A tibble: 2,018,477 × 26
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ℹ 2,018,467 more rows
    ## # ℹ 22 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender.x <chr>, race.x <chr>, earliest_date.x <date>,
    ## #   latest_date.x <date>, tenure_days.x <dbl>, gender.y <chr>, race.y <chr>, …

…

``` r
# Create quarter columns
applications1$filing_quarter <- paste(year(applications1$filing_date), quarter(applications1$filing_date), sep="-Q")
applications1$abandon_quarter <- paste(year(applications1$abandon_date), quarter(applications1$abandon_date), sep="-Q")
applications1$issue_quarter <- paste(year(applications1$patent_issue_date), quarter(applications1$patent_issue_date), sep="-Q")

applications1
```

    ## # A tibble: 2,018,477 × 29
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ℹ 2,018,467 more rows
    ## # ℹ 25 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <chr>,
    ## #   tc <dbl>, gender.x <chr>, race.x <chr>, earliest_date.x <date>,
    ## #   latest_date.x <date>, tenure_days.x <dbl>, gender.y <chr>, race.y <chr>, …

``` r
# Summarize application number by examiner and quarter
# Aggregate data
aggregated_data <- applications1 %>%
  group_by(examiner_id, filing_quarter) %>%
  summarise(
    new_applications = n_distinct(application_number),
    .groups = "drop"
  )

# Aggregate and join for abandoned applications
abandoned_data <- applications1 %>%
  filter(!is.na(abandon_date)) %>%
  group_by(examiner_id, abandon_quarter) %>%
  summarise(
    abandoned_applications = n_distinct(application_number),
    .groups = "drop"
  )
aggregated_data <- left_join(aggregated_data, abandoned_data, by = c("examiner_id", "filing_quarter" = "abandon_quarter"))

# Aggregate and join for allowed applications
allowed_data <- applications1 %>%
  filter(!is.na(patent_issue_date)) %>%
  group_by(examiner_id, issue_quarter) %>%
  summarise(
    allowed_applications = n_distinct(application_number),
    .groups = "drop"
  )
aggregated_data <- left_join(aggregated_data, allowed_data, by = c("examiner_id", "filing_quarter" = "issue_quarter"))

# Calculate in-process applications
applications1$in_process_indicator <- with(applications1, 
  (filing_quarter <= filing_quarter) & 
  (
    (is.na(abandon_date) & is.na(patent_issue_date)) | 
    (!is.na(abandon_date) & abandon_quarter > filing_quarter) |
    (!is.na(patent_issue_date) & issue_quarter > filing_quarter) |
    (!is.na(abandon_date) & !is.na(patent_issue_date) & abandon_quarter > filing_quarter & issue_quarter > filing_quarter)
  )
)


in_process_data <- applications1 %>%
  group_by(examiner_id, filing_quarter) %>%
  summarise(
    in_process_applications = sum(in_process_indicator, na.rm = TRUE),
    .groups = "drop"
  )


aggregated_data <- left_join(aggregated_data, in_process_data, by = c("examiner_id", "filing_quarter"))

# Replace NA with 0 for counts
aggregated_data[is.na(aggregated_data)] <- 0

library(dplyr)

# Rename the column name of "filing_quarter"
aggregated_data <- aggregated_data %>%
  rename(analysis_quarter = filing_quarter)

aggregated_data
```

    ## # A tibble: 190,881 × 6
    ##    examiner_id analysis_quarter new_applications abandoned_applications
    ##          <dbl> <chr>                       <int>                  <int>
    ##  1       59012 2004-Q3                         1                      0
    ##  2       59012 2006-Q1                         1                      0
    ##  3       59012 2006-Q2                         4                      0
    ##  4       59012 2006-Q3                         5                      0
    ##  5       59012 2006-Q4                         9                      0
    ##  6       59012 2007-Q1                         9                      0
    ##  7       59012 2007-Q2                        16                      0
    ##  8       59012 2007-Q3                        11                      0
    ##  9       59012 2007-Q4                        10                      0
    ## 10       59012 2008-Q1                        11                      0
    ## # ℹ 190,871 more rows
    ## # ℹ 2 more variables: allowed_applications <int>, in_process_applications <int>

``` r
# Summarize by art unit
# Convert appl_status_date to Date-Time Type
applications1$appl_status_date <- as.POSIXct(applications1$appl_status_date, format="%d%b%Y %H:%M:%S")

# Find the latest appl_status_date for each examiner_id and extract examiner_art_unit
latest_art_unit <- applications1 %>%
  group_by(examiner_id) %>%
  filter(appl_status_date == max(appl_status_date, na.rm = TRUE)) %>%
  summarise(current_art_unit = first(examiner_art_unit), .groups = "drop")

# Add current_art_unit column to applications1
applications1 <- left_join(applications1, latest_art_unit, by = "examiner_id")

applications1
```

    ## # A tibble: 2,018,477 × 31
    ##    application_number filing_date examiner_name_last examiner_name_first
    ##    <chr>              <date>      <chr>              <chr>              
    ##  1 08284457           2000-01-26  HOWARD             JACQUELINE         
    ##  2 08413193           2000-10-11  YILDIRIM           BEKIR              
    ##  3 08531853           2000-05-17  HAMILTON           CYNTHIA            
    ##  4 08637752           2001-07-20  MOSHER             MARY               
    ##  5 08682726           2000-04-10  BARR               MICHAEL            
    ##  6 08687412           2000-04-28  GRAY               LINDA              
    ##  7 08716371           2004-01-26  MCMILLIAN          KARA               
    ##  8 08765941           2000-06-23  FORD               VANESSA            
    ##  9 08776818           2000-02-04  STRZELECKA         TERESA             
    ## 10 08809677           2002-02-20  KIM                SUN                
    ## # ℹ 2,018,467 more rows
    ## # ℹ 27 more variables: examiner_name_middle <chr>, examiner_id <dbl>,
    ## #   examiner_art_unit <dbl>, uspc_class <chr>, uspc_subclass <chr>,
    ## #   patent_number <chr>, patent_issue_date <date>, abandon_date <date>,
    ## #   disposal_type <chr>, appl_status_code <dbl>, appl_status_date <dttm>,
    ## #   tc <dbl>, gender.x <chr>, race.x <chr>, earliest_date.x <date>,
    ## #   latest_date.x <date>, tenure_days.x <dbl>, gender.y <chr>, race.y <chr>, …

``` r
# 1. Summarize the number of people in each art unit
people_in_art_unit <- applications1 %>%
  group_by(current_art_unit) %>%
  summarise(number_of_people = n_distinct(examiner_id)) %>%
  ungroup()  

# 2. Summarize the number of women in each art unit
women_in_art_unit <- applications1 %>%
  filter(gender.y == "female") %>% 
  group_by(current_art_unit) %>%
  summarise(number_of_women = n_distinct(examiner_id)) %>%
  ungroup()

# 3. Summarize the number of examiners by race in each art unit
examiners_by_race_in_art_unit <- applications1 %>%
  group_by(current_art_unit, race.y) %>%
  summarise(number_of_examiners = n_distinct(examiner_id)) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'current_art_unit'. You can override using
    ## the `.groups` argument.

``` r
# View the results
print(people_in_art_unit)
```

    ## # A tibble: 285 × 2
    ##    current_art_unit number_of_people
    ##               <dbl>            <int>
    ##  1             1600                1
    ##  2             1609                2
    ##  3             1611               21
    ##  4             1612               23
    ##  5             1613               15
    ##  6             1614               17
    ##  7             1615               40
    ##  8             1616               29
    ##  9             1617               32
    ## 10             1618               17
    ## # ℹ 275 more rows

``` r
print(women_in_art_unit)
```

    ## # A tibble: 271 × 2
    ##    current_art_unit number_of_women
    ##               <dbl>           <int>
    ##  1             1600               1
    ##  2             1609               1
    ##  3             1611              12
    ##  4             1612              10
    ##  5             1613               6
    ##  6             1614               6
    ##  7             1615              16
    ##  8             1616              12
    ##  9             1617              12
    ## 10             1618               6
    ## # ℹ 261 more rows

``` r
print(examiners_by_race_in_art_unit)
```

    ## # A tibble: 853 × 3
    ##    current_art_unit race.y   number_of_examiners
    ##               <dbl> <chr>                  <int>
    ##  1             1600 white                      1
    ##  2             1609 white                      2
    ##  3             1611 Asian                      1
    ##  4             1611 Hispanic                   2
    ##  5             1611 white                     18
    ##  6             1612 Asian                      5
    ##  7             1612 black                      1
    ##  8             1612 white                     17
    ##  9             1613 Asian                      5
    ## 10             1613 Hispanic                   1
    ## # ℹ 843 more rows

``` r
# Separation indicator
# Start by assuming all examiners are separated
aggregated_data$separation_indicator <- TRUE

# Create a subset of data for the quarter "2017-Q2"
data_2017_Q2 <- aggregated_data %>%
  filter(analysis_quarter == "2017-Q2") %>%
  group_by(examiner_id) %>%
  summarise(
    total_applications = sum(new_applications + abandoned_applications + allowed_applications + in_process_applications),
    separation_indicator = total_applications == 0, # Determine separation_indicator based on activity in "2017-Q2"
    .groups = "drop"
  )

# Get the examiner_ids that have separation_indicator as FALSE (meaning they had activity in "2017-Q2")
active_examiner_ids <- data_2017_Q2 %>%
  filter(separation_indicator == FALSE) %>%
  pull(examiner_id)

# Update the separation_indicator in the original aggregated_data
# Set separation_indicator to FALSE for examiners who had activity in "2017-Q2"
aggregated_data$separation_indicator[aggregated_data$examiner_id %in% active_examiner_ids] <- FALSE

# View the updated dataset
print(aggregated_data)
```

    ## # A tibble: 190,881 × 7
    ##    examiner_id analysis_quarter new_applications abandoned_applications
    ##          <dbl> <chr>                       <int>                  <int>
    ##  1       59012 2004-Q3                         1                      0
    ##  2       59012 2006-Q1                         1                      0
    ##  3       59012 2006-Q2                         4                      0
    ##  4       59012 2006-Q3                         5                      0
    ##  5       59012 2006-Q4                         9                      0
    ##  6       59012 2007-Q1                         9                      0
    ##  7       59012 2007-Q2                        16                      0
    ##  8       59012 2007-Q3                        11                      0
    ##  9       59012 2007-Q4                        10                      0
    ## 10       59012 2008-Q1                        11                      0
    ## # ℹ 190,871 more rows
    ## # ℹ 3 more variables: allowed_applications <int>,
    ## #   in_process_applications <int>, separation_indicator <lgl>

``` r
# AU move indicator

# Group by examiner_id and check if there is more than one unique examiner_art_unit.x
applications1 <- applications1 %>%
  group_by(examiner_id) %>%
  mutate(
    AU_move_indicator = n_distinct(examiner_art_unit) > 1
  ) %>%
  ungroup()  # Remove the grouping
```

``` r
# Descriptive table
# install.packages(gtsummary)
library(gtsummary)

# For aggregated_data: Create a descriptive table for separation_indicator
table_separation <- aggregated_data %>%
  distinct(examiner_id, .keep_all = TRUE) %>%
  select(separation_indicator) %>%
  tbl_summary(missing = "no") %>%
  add_n() %>%
  modify_header(label = "**Variable**")

# For applications1: Create a descriptive table for AU_move_indicator
table_au_move <- applications1 %>%
  distinct(examiner_id, .keep_all = TRUE) %>%
  select(AU_move_indicator) %>%
  tbl_summary(missing = "no") %>%
  add_n() %>%
  modify_header(label = "**Variable**")

# Estimate Turnover Rate, ensuring each examiner_id is counted only once
turnover_rate <- aggregated_data %>%
  distinct(examiner_id, .keep_all = TRUE) %>%
  summarise(
    total = n(),
    separated = sum(separation_indicator, na.rm = TRUE)
  ) %>%
  mutate(turnover_rate = separated / total)

# Estimate Mobility Rate, ensuring each examiner_id is counted only once
mobility_rate <- applications1 %>%
  distinct(examiner_id, .keep_all = TRUE) %>%
  summarise(
    total = n(),
    moved_au = sum(AU_move_indicator, na.rm = TRUE)
  ) %>%
  mutate(mobility_rate = moved_au / total)

# Display the tables and rates
table_separation
```

<div id="otdujksosa" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#otdujksosa table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#otdujksosa thead, #otdujksosa tbody, #otdujksosa tfoot, #otdujksosa tr, #otdujksosa td, #otdujksosa th {
  border-style: none;
}
&#10;#otdujksosa p {
  margin: 0;
  padding: 0;
}
&#10;#otdujksosa .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#otdujksosa .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#otdujksosa .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#otdujksosa .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#otdujksosa .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#otdujksosa .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#otdujksosa .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#otdujksosa .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#otdujksosa .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#otdujksosa .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#otdujksosa .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#otdujksosa .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#otdujksosa .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#otdujksosa .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#otdujksosa .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#otdujksosa .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#otdujksosa .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#otdujksosa .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#otdujksosa .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#otdujksosa .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#otdujksosa .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#otdujksosa .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#otdujksosa .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#otdujksosa .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#otdujksosa .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#otdujksosa .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#otdujksosa .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#otdujksosa .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#otdujksosa .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#otdujksosa .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#otdujksosa .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#otdujksosa .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#otdujksosa .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#otdujksosa .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#otdujksosa .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#otdujksosa .gt_left {
  text-align: left;
}
&#10;#otdujksosa .gt_center {
  text-align: center;
}
&#10;#otdujksosa .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#otdujksosa .gt_font_normal {
  font-weight: normal;
}
&#10;#otdujksosa .gt_font_bold {
  font-weight: bold;
}
&#10;#otdujksosa .gt_font_italic {
  font-style: italic;
}
&#10;#otdujksosa .gt_super {
  font-size: 65%;
}
&#10;#otdujksosa .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#otdujksosa .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#otdujksosa .gt_indent_1 {
  text-indent: 5px;
}
&#10;#otdujksosa .gt_indent_2 {
  text-indent: 10px;
}
&#10;#otdujksosa .gt_indent_3 {
  text-indent: 15px;
}
&#10;#otdujksosa .gt_indent_4 {
  text-indent: 20px;
}
&#10;#otdujksosa .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Variable&lt;/strong&gt;"><strong>Variable</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N&lt;/strong&gt;"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 5,649&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>N = 5,649</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">separation_indicator</td>
<td headers="n" class="gt_row gt_center">5,649</td>
<td headers="stat_0" class="gt_row gt_center">5,581 (99%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
table_au_move
```

<div id="ateptsxyin" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ateptsxyin table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ateptsxyin thead, #ateptsxyin tbody, #ateptsxyin tfoot, #ateptsxyin tr, #ateptsxyin td, #ateptsxyin th {
  border-style: none;
}
&#10;#ateptsxyin p {
  margin: 0;
  padding: 0;
}
&#10;#ateptsxyin .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ateptsxyin .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ateptsxyin .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ateptsxyin .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ateptsxyin .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ateptsxyin .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ateptsxyin .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ateptsxyin .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ateptsxyin .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ateptsxyin .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ateptsxyin .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ateptsxyin .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ateptsxyin .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ateptsxyin .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ateptsxyin .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ateptsxyin .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ateptsxyin .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ateptsxyin .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ateptsxyin .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ateptsxyin .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ateptsxyin .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ateptsxyin .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ateptsxyin .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ateptsxyin .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ateptsxyin .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ateptsxyin .gt_left {
  text-align: left;
}
&#10;#ateptsxyin .gt_center {
  text-align: center;
}
&#10;#ateptsxyin .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ateptsxyin .gt_font_normal {
  font-weight: normal;
}
&#10;#ateptsxyin .gt_font_bold {
  font-weight: bold;
}
&#10;#ateptsxyin .gt_font_italic {
  font-style: italic;
}
&#10;#ateptsxyin .gt_super {
  font-size: 65%;
}
&#10;#ateptsxyin .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ateptsxyin .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ateptsxyin .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ateptsxyin .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ateptsxyin .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ateptsxyin .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ateptsxyin .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Variable&lt;/strong&gt;"><strong>Variable</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N&lt;/strong&gt;"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 5,649&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>N = 5,649</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">AU_move_indicator</td>
<td headers="n" class="gt_row gt_center">5,649</td>
<td headers="stat_0" class="gt_row gt_center">2,763 (49%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
print(paste("Turnover Rate:", turnover_rate$turnover_rate))
```

    ## [1] "Turnover Rate: 0.987962471233847"

``` r
print(paste("Mobility Rate:", mobility_rate$mobility_rate))
```

    ## [1] "Mobility Rate: 0.489113117365905"
