**NOTE**: It's apparently a well-documented issue that GitHub Markdown (`.md`) files don't render LaTeX equations or math *at all*. 

As such, I've left the LaTeX equations and math from my original R Markdown document as-is, as I have not found a robust way to incorporate LaTeX-style math into GitHub markdown documents, without converting them all into image files, like `.jpg` or `.png`.

I apologize how crude they will appear.

If you wish to view a better version of this document (floating table of contents, properly rendered math), please visit [this](https://nigelmckernan.ca/static/media/ma_paper) page.

MA Paper Code Appendix
================
Nigel McKernan
10/18/2020

  - [1 Introduction](#introduction)
      - [1.1 Base-R/data.table vs
        Tidyverse](#base-rdata.table-vs-tidyverse)
      - [1.2 Overview of Packages Used](#overview-of-packages-used)
  - [2 Exogenous Regressor Data](#exogenous-regressor-data)
      - [2.1 Pulling In The Data](#pulling-in-the-data)
      - [2.2 Removing and Renaming
        Columns](#removing-and-renaming-columns)
      - [2.3 Splitting A Column](#splitting-a-column)
      - [2.4 Filtering Observations](#filtering-observations)
      - [2.5 Pivoting](#pivoting)
      - [2.6 Recoding Observations](#recoding-observations)
      - [2.7 Cleanup](#cleanup)
  - [3 Temporal Disaggregation](#temporal-disaggregation)
      - [3.1 Dissaggregating](#dissaggregating)
      - [3.2 Pivoting Long](#pivoting-long)
  - [4 Instrumental Variables](#instrumental-variables)
      - [4.1 Pulling Climate Data](#pulling-climate-data)
      - [4.2 Temporal Aggregation](#temporal-aggregation)
  - [5 Pollution Data](#pollution-data)
  - [6 House Price Index Data](#house-price-index-data)
  - [7 Initial Regression Models](#initial-regression-models)
      - [7.1 Overview of Model(s)
        Estimated](#overview-of-models-estimated)
      - [7.2 Formulae & OLS](#formulae-ols)
      - [7.3 IV Models](#iv-models)
  - [8 Diagnostics and Further
    Refinements](#diagnostics-and-further-refinements)
      - [8.1 F-Test for Effects](#f-test-for-effects)
      - [8.2 Hausman Test](#hausman-test)
      - [8.3 Breusch-Pagan LM Test](#breusch-pagan-lm-test)
  - [9 Instrumental Variable (IV)
    Diagnostics](#instrumental-variable-iv-diagnostics)
      - [9.1 F-Testing our Instruments](#f-testing-our-instruments)
      - [9.2 Wu-Hausman Test for
        Endogeneity](#wu-hausman-test-for-endogeneity)
      - [9.3 Sargan Test for Over-Identified
        Restrictions](#sargan-test-for-over-identified-restrictions)
      - [9.4 Re-fitting with Single
        Instruments](#re-fitting-with-single-instruments)
  - [10 Model Caveats](#model-caveats)
      - [10.1 Cross-Sectional Dependence](#cross-sectional-dependence)
          - [10.1.1 Breusch-Pagan’s LM Test](#breusch-pagans-lm-test)
          - [10.1.2 Heckman (1976) Correction](#heckman-1976-correction)
      - [10.2 Serial Correlation](#serial-correlation)
      - [10.3 Remedying Cross-Sectional & Serial
        Correlation](#remedying-cross-sectional-serial-correlation)
      - [10.4 Poolability and Variable Coefficients
        Models](#poolability-and-variable-coefficients-models)
  - [11 Summary](#summary)
  - [12 References](#references)

# 1 Introduction

This appendix is a companion piece to my Master’s Research Project that
I completed to fulfill the requirements of my Master’s in Economics from
Concordia University in Montréal, Québec.

You can find that [here](https://github.com/nigeljmckernan/MAPaperAppendix/blob/master/Thesis_Nigel_McKernan.pdf).

I have 3 main goals that I want to achieve with this document:

  - To demonstrate *how* I executed the analysis of my paper via the
    programming language R and its vast collection of packages.

  - To practice writing [R Markdown](https://rmarkdown.rstudio.com)
    documents (what you are reading right now).
    
      - R Markdown documents inserts R code into reports, slides, etc.,
        so as to seamlessly transition between regular text
        documentation, and incorporating the R code written that
        pertains to the report.

  - To demonstrate the following skills to prospective interested
    parties, such as employers, schools, or other organizations:
    
      - Research
      - Data analysis, cleaning, and visualization
      - Communicating my research and analysis in a layman-esque manner
        (as best as I can).
       

## 1.1 Base-R/data.table vs Tidyverse

Apart from the econometrics-focused packages that I’ll be using, I
prefer using packages and functions from the
[Tidyverse](https://tidyverse.org) collection of packages for data
manipulation/munging/cleaning, compared to those found in base-R or in
the `data.table` package.

That’s simply my personal preference with regards to how I best like
cleaning and manipulating dirty or untidy data to a point where it is
conducive to data analysis, regressions, machine learning, etc.

  - I realize `data.table` is faster, though I prefer `dplyr`’s syntax,
    and the seamless transition into functions from the other Tidyverse
    packages, like `tidyr`, is unbelievably convenient.
    
      - If I need the speed that `data.table` is excellent at providing,
        I’ll just use `dtplyr` to speed up my `dplyr` operations.

As an aside, I think people often attribute the `%>%` (pipe) operator
from `magrittr` as something that belongs to the Tidyverse. I personally
don’t see it like that as the pipe works really well with non-Tidyverse
packages (including `data.table`\!\!).

With that out of the way, I’ll be loading up the `tidyverse` package to
load up our essentials.

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
library(dtplyr)
library(foreach, warn.conflicts = FALSE)
library(tempdisagg)
library(plm, warn.conflicts = FALSE)
library(broom)
```

``` r
library(stargazer)
```

## 1.2 Overview of Packages Used

With that previous disclaimer out of the way, the non-Tidyverse packages
I’ll be using are:

  - `plm` - the main workhorse package for conducting panel-data
    regression analyses that most closely resembles `lm()`
    
      - `lme4` & `nlme` won’t be used as my model does not really
        incorporate quadratic regressors (my models fails to reject the
        RESET test) or other non-linear elements and does not take a
        mixed-models approach.

  - `lmtest` - mainly for `waldtest()` for comparing models, and
    `coeftest()` for testing the significance of my regressors while
    incorporating different estimated covariance matrices from `plm`
    
      - `sandwich` is not used as the more useful covariance matrix
        estimators for panel data are already provided in `plm`
        
          - i.e. `vcovSCC()`

  - `foreach` - the main reason why I like the this package so much is
    its `for` loop returns an *object*. Not only that, but via the
    `.combine` argument, you can really customize the output `foreach()`
    returns.
    
      - For this project, I want a *single* data-frame where I iterate
        through various years, months, and station ID’s across Canada
        from the Environment Canada’s website to pull years-worth of
        climate data at the hourly and daily levels.
        
          - To do that, the value for my `.combine` argument will be
            `rbind`, as I want the output from each iteration appended
            the end of the previous iteration, returning a *single*
            data-frame, instead of a nested list that I would have to
            unnest afterwards.
          - By default, `foreach()` will return a nested list where each
            element in the list contains the output of the respective
            iteration.
    
      - I will not be using a parallelized backend such as `doParallel`
        as pulling data via http(s) only works sequentially when I’ve
        tried it, so the `%do%` or `%doSEQ%` methods will work fine.

  - `tempdisagg` - I will not be covering much of the theory of temporal
    disaggregation, but this package will be used to temporally
    disaggregate the socioeconomic data from Statistics Canada that is
    only available annually, down to a monthly frequency, which is the
    frequency needed to match our dependent variable.
    
      - I don’t attempt to use an indicator series as there is not a
        clear consensus, or known recommended indicator data, for
        something like household income, dwelling vacancy rate, or
        homicide rate
        
          - As such, I will be using the Denton-Cholette (`method =
            "denton-cholette"`) method of estimating our AR(1) regressor
            by regressing on a vector of one’s, which does not require
            an indicator series, and produces very smooth results.
        
          - As an aside, I *really* wish there was an attempt to wrap
            `td()` and `ta()` from `tempdisagg` into a “tidy” way of
            doing temporal (dis)aggregation
            
              - I really like what `parsnip` from
                [Tidymodels](https://tidymodels.org) does to
                “front-endify” a lot of machine-learning/regression
                techniques into a coherent and consistent API, and
                really wish there was an attempt to bring `plm` and
                `tempdisagg` into that ecosystem.
            
              - A close alternative is to use the various `map_x()`
                functions from `purrr` to iterate in a “tidy” way across
                a dataframe.

# 2 Exogenous Regressor Data

For my exogenous regressors, they all come from Statistics Canada tables
(hereafter I’ll abbreviate it to StatsCan), and since the process for
how I go about cleaning/pulling/extracting the data for the *exogenous*
variables needed is generally the same, I’ll only demonstrate this for
one variable, for brevity’s sake.

The dataset I’ll be using is from table
[11-10-0135-01](https://doi.org/10.25318/1110013501-eng) from StatsCan,
“Low income statistics by age, sex and economic family type”

The variable I’ll be pulling/cleaning from this table is to extract the
percent of persons under the After-Tax-Low-Income-Cutoff, by city, by
year.

  - In my model, the short-hand for this variable is “LICO”

## 2.1 Pulling In The Data

Base-R’s `read.csv()` function is known to be generally slow, and not
the greatest at parsing columns into their respective data types.

`fread()` from `data.table` is blazing fast, but I generally encounter
more errors with it and the columns that get parsed aren’t always
converted to their appropriate data type.

I do find it useful if the input file is consistent *enough* to not
garble up the data on the way in.

On the other hand, `read_csv()` or `read_tsv()` from `readr` is very
consistent (from my experience) in correctly parsing the data into its
appropriate data types, though it is slower than `fread()`.

As such, `readr::read_csv()` will be used the pull the data from our CSV
file.

``` r
LICO_Data <- read_csv("Z:\\Documents and Misc Stuff\\MA Paper\\11100135-eng\\11100135.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   REF_DATE = col_double(),
    ##   GEO = col_character(),
    ##   DGUID = col_character(),
    ##   `Persons in low income` = col_character(),
    ##   `Low income lines` = col_character(),
    ##   Statistics = col_character(),
    ##   UOM = col_character(),
    ##   UOM_ID = col_double(),
    ##   SCALAR_FACTOR = col_character(),
    ##   SCALAR_ID = col_double(),
    ##   VECTOR = col_character(),
    ##   COORDINATE = col_character(),
    ##   VALUE = col_double(),
    ##   STATUS = col_character(),
    ##   SYMBOL = col_logical(),
    ##   TERMINATED = col_logical(),
    ##   DECIMALS = col_double()
    ## )

``` r
LICO_Data
```

    ## # A tibble: 400,554 x 17
    ##    REF_DATE GEO   DGUID `Persons in low~ `Low income lin~ Statistics UOM  
    ##       <dbl> <chr> <chr> <chr>            <chr>            <chr>      <chr>
    ##  1     1976 Cana~ 2016~ All persons      Low income meas~ Number of~ Numb~
    ##  2     1976 Cana~ 2016~ All persons      Low income meas~ Percentag~ Perc~
    ##  3     1976 Cana~ 2016~ All persons      Low income meas~ Average g~ Perc~
    ##  4     1976 Cana~ 2016~ All persons      Low income cut-~ Number of~ Numb~
    ##  5     1976 Cana~ 2016~ All persons      Low income cut-~ Percentag~ Perc~
    ##  6     1976 Cana~ 2016~ All persons      Low income cut-~ Average g~ Perc~
    ##  7     1976 Cana~ 2016~ All persons      Low income cut-~ Number of~ Numb~
    ##  8     1976 Cana~ 2016~ All persons      Low income cut-~ Percentag~ Perc~
    ##  9     1976 Cana~ 2016~ All persons      Low income cut-~ Average g~ Perc~
    ## 10     1976 Cana~ 2016~ All persons      Market basket m~ Number of~ Numb~
    ## # ... with 400,544 more rows, and 10 more variables: UOM_ID <dbl>,
    ## #   SCALAR_FACTOR <chr>, SCALAR_ID <dbl>, VECTOR <chr>, COORDINATE <chr>,
    ## #   VALUE <dbl>, STATUS <chr>, SYMBOL <lgl>, TERMINATED <lgl>, DECIMALS <dbl>

As you can see, some columns do not have names that are clear what they
represent in the dataset, or are too long and must be cut down in length
for ease of manipulation later.

Let’s proceed with that now.

## 2.2 Removing and Renaming Columns

``` r
LICO_Part_1 <- LICO_Data %>%
  
  select(-DGUID,
           -UOM_ID,
           -SCALAR_ID,
           -VECTOR,
           -COORDINATE,
           -DECIMALS,
           -SYMBOL,
           -STATUS,
           -TERMINATED,
           -SCALAR_FACTOR,
           -UOM)
  
LICO_Part_1 %>% head() %>% kable()
```

| REF\_DATE | GEO    | Persons in low income | Low income lines                         | Statistics                          |  VALUE |
| --------: | :----- | :-------------------- | :--------------------------------------- | :---------------------------------- | -----: |
|      1976 | Canada | All persons           | Low income measure after tax             | Number of persons in low income     | 3008.0 |
|      1976 | Canada | All persons           | Low income measure after tax             | Percentage of persons in low income |   13.0 |
|      1976 | Canada | All persons           | Low income measure after tax             | Average gap ratio                   |   31.6 |
|      1976 | Canada | All persons           | Low income cut-offs after tax, 1992 base | Number of persons in low income     | 2989.0 |
|      1976 | Canada | All persons           | Low income cut-offs after tax, 1992 base | Percentage of persons in low income |   13.0 |
|      1976 | Canada | All persons           | Low income cut-offs after tax, 1992 base | Average gap ratio                   |   33.5 |

We’ve eliminated some columns that were either redundant, or had no
pertinent information.

However columns like “Low income lines” and “Persons in low income” can
be renamed to something more succinct.

``` r
LICO_Part_2 <- LICO_Part_1 %>%
  
  rename(Persons = `Persons in low income`,
           Lines = `Low income lines`,
           Year = REF_DATE,
           Value = VALUE)

LICO_Part_2 %>% head() %>% kable()
```

| Year | GEO    | Persons     | Lines                                    | Statistics                          |  Value |
| ---: | :----- | :---------- | :--------------------------------------- | :---------------------------------- | -----: |
| 1976 | Canada | All persons | Low income measure after tax             | Number of persons in low income     | 3008.0 |
| 1976 | Canada | All persons | Low income measure after tax             | Percentage of persons in low income |   13.0 |
| 1976 | Canada | All persons | Low income measure after tax             | Average gap ratio                   |   31.6 |
| 1976 | Canada | All persons | Low income cut-offs after tax, 1992 base | Number of persons in low income     | 2989.0 |
| 1976 | Canada | All persons | Low income cut-offs after tax, 1992 base | Percentage of persons in low income |   13.0 |
| 1976 | Canada | All persons | Low income cut-offs after tax, 1992 base | Average gap ratio                   |   33.5 |

That looks much better.

## 2.3 Splitting A Column

The “GEO” column is a bit problematic for us as this column contains
observations for Canada in aggregate and the various provinces, but
*also* individual cities that have the province name in the observation
separated by a comma (,).

  - e.g. Winnipeg, Manitoba
    
      - (Go Jets Go\!)

Instead this column should ideally be split into two:

  - One column for “Province/Country”
  - The other for “City”

So how do we split this column into two?

Lucky for us, any time the observation is a city, it’s split with a
comma (,) from its respective province.

The `separate()` function from `tidyr` makes splitting this easy to do:

``` r
LICO_Part_3 <- LICO_Part_2 %>%
  
  separate(GEO, c("City","Province"), ", ") 
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 247962 rows [1,
    ## 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
LICO_Part_3 %>% filter(!Province %>% is.na()) %>% head() %>% kable()
```

| Year | City   | Province | Persons     | Lines                                    | Statistics                          | Value |
| ---: | :----- | :------- | :---------- | :--------------------------------------- | :---------------------------------- | ----: |
| 1976 | Québec | Quebec   | All persons | Low income measure after tax             | Number of persons in low income     |  50.0 |
| 1976 | Québec | Quebec   | All persons | Low income measure after tax             | Percentage of persons in low income |   9.2 |
| 1976 | Québec | Quebec   | All persons | Low income measure after tax             | Average gap ratio                   |  30.3 |
| 1976 | Québec | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Number of persons in low income     |  52.0 |
| 1976 | Québec | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Percentage of persons in low income |   9.6 |
| 1976 | Québec | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Average gap ratio                   |  30.7 |

Since not all observations denote cities, and therefore do not contain a
comma, there’s no splitting to be done, and so the second column will
contain `NA`’s any time that occurs.

This is denoted by the first error box above.

## 2.4 Filtering Observations

Since this dataset contains superfluous or extra data that is not needed
for this analysis, let’s filter those observations out.

First, since this project concerns Canadian Metropolitan Areas (CMA’s)
and *not* provinces or the country in aggregate, we will be filtering
out any observations that are not CMA’s.

Additionally, our dependent variable only has data from 2001 to 2019, so
we need to filter out years outside of that range to match our dependent
variable.

As well, we do not need *all* the various measures of low income from
the “Lines” column. We are only concerned with the after-tax variant,
based off 1992 as a base year to deflate other years.

Finally, we want to capture *all* persons; not those stratified into
different age groups in the “Persons” column.

Those last two requirements can be accomplished by the `stringr`
package, which makes dealing with text data very easy.

So, let’s filter out the observations in accordance with our needs
above:

``` r
LICO_Part_4 <- LICO_Part_3 %>%
  
  filter(!Province %>% is.na(),
           Year >= 2001 & Year < 2020,
           Persons %>% str_starts("All"),
           Lines %>% str_detect("1992 base"),
           Lines %>% str_detect("after tax"))

LICO_Part_4 %>% head() %>% kable()
```

| Year | City     | Province | Persons     | Lines                                    | Statistics                          | Value |
| ---: | :------- | :------- | :---------- | :--------------------------------------- | :---------------------------------- | ----: |
| 2001 | Québec   | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Number of persons in low income     | 119.0 |
| 2001 | Québec   | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Percentage of persons in low income |  16.0 |
| 2001 | Québec   | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Average gap ratio                   |  29.1 |
| 2001 | Montréal | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Number of persons in low income     | 569.0 |
| 2001 | Montréal | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Percentage of persons in low income |  17.6 |
| 2001 | Montréal | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Average gap ratio                   |  30.5 |

Alright, those observations have now been filtered out of our dataset.

Now, we need to *pivot* this dataset.

## 2.5 Pivoting

If you’re familiar with Pivot Tables in Microsoft Excel or something
similar (LibreOffice Calc), then this should be relatively easy to
understand.

If not, pivoting data is hard to explain without demonstration, but
simply it seeks to transpose individual observations into their *own*
columns, or the opposite, to take columns and then *pivot* them into a
single column.

  - Our dataset as it is now is in the *former* situation; we want to
    turn observations into columns;
    
      - I want to make certain observations in one column, into *their
        own* columns:
        
          - i.e. The “Statistics” column has different types of
            measurements, with their corresponding value in the “Value”
            column
            
              - What I want to achieve is to make a column for every
                different measure in the “Statistics” column, with their
                respective value taken from the “Value” column

The end goal of this is to have a unique observation per pairwise
combination of city and year, *per row*.

Hard to understand, right? Let’s just demonstrate it instead:

But before we do, let’s first:

  - See how many **distinct** values the “Statistics” column can be as
    our dataset currently stands
  - Take a look at what our dataframe looks like *before* we pivot it

<!-- end list -->

``` r
# See distinct values for the Statistics column
LICO_Part_4 %>% select(Statistics) %>% distinct() %>% kable()
```

| Statistics                          |
| :---------------------------------- |
| Number of persons in low income     |
| Percentage of persons in low income |
| Average gap ratio                   |

``` r
# Preview refresh of our table
LICO_Part_4 %>% head() %>% kable()
```

| Year | City     | Province | Persons     | Lines                                    | Statistics                          | Value |
| ---: | :------- | :------- | :---------- | :--------------------------------------- | :---------------------------------- | ----: |
| 2001 | Québec   | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Number of persons in low income     | 119.0 |
| 2001 | Québec   | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Percentage of persons in low income |  16.0 |
| 2001 | Québec   | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Average gap ratio                   |  29.1 |
| 2001 | Montréal | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Number of persons in low income     | 569.0 |
| 2001 | Montréal | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Percentage of persons in low income |  17.6 |
| 2001 | Montréal | Quebec   | All persons | Low income cut-offs after tax, 1992 base | Average gap ratio                   |  30.5 |

What I want to achieve is to take those 3 distinct values in the
“Statistics” column, and turn them into their own columns, with their
respective value taken from the “Value” column.

``` r
LICO_Part_5 <- LICO_Part_4 %>%
  
  select(-Province) %>%
    
#Getting rid of the Province column since I really don't need it anymore.
  
    pivot_wider(names_from = Statistics,
                values_from = Value) %>%

#Renaming the resulting columns to something a little more concise, but based off the renaming, you probably get what they represent.
  
rename (No_Persons = `Number of persons in low income`,
            Percentage = `Percentage of persons in low income`,
            Gap = `Average gap ratio`)

LICO_Part_5 %>% head() %>% kable()
```

| Year | City            | Persons     | Lines                                    | No\_Persons | Percentage |  Gap |
| ---: | :-------------- | :---------- | :--------------------------------------- | ----------: | ---------: | ---: |
| 2001 | Québec          | All persons | Low income cut-offs after tax, 1992 base |         119 |       16.0 | 29.1 |
| 2001 | Montréal        | All persons | Low income cut-offs after tax, 1992 base |         569 |       17.6 | 30.5 |
| 2001 | Ottawa-Gatineau | All persons | Low income cut-offs after tax, 1992 base |         117 |       10.6 | 37.7 |
| 2001 | Toronto         | All persons | Low income cut-offs after tax, 1992 base |         481 |       10.6 | 37.2 |
| 2001 | Winnipeg        | All persons | Low income cut-offs after tax, 1992 base |          88 |       13.7 | 30.5 |
| 2001 | Calgary         | All persons | Low income cut-offs after tax, 1992 base |          88 |       10.1 | 35.1 |

I did some renaming of the resulting columns, but they should be a close
enough analogue to what they were named before.

## 2.6 Recoding Observations

Now I’m going to do some alterations to the names of some cities in our
“City” column.

To avoid any potential errors and make sure everything is named
consistently, I’m going to replace any “é” with a regular “e”.

  - This is found in the names of Montréal and Québec.
    
      - I don’t have anything against accent aigu, or Québec, or
        Montréal (I mean I *live* there after all), it’s just to make
        sure any join operations I do with the other datasets always
        correctly match to each other.

To do this, I’m relying again on functions in the `stringr` package,
specifically `str_replace()`

``` r
LICO_Part_5$City <- LICO_Part_5$City %>%
  
  str_replace("é", "e") %>%
  
  str_replace("Ottawa-Gatineau","Ottawa")

LICO_Part_5 %>% head() %>% kable()
```

| Year | City     | Persons     | Lines                                    | No\_Persons | Percentage |  Gap |
| ---: | :------- | :---------- | :--------------------------------------- | ----------: | ---------: | ---: |
| 2001 | Quebec   | All persons | Low income cut-offs after tax, 1992 base |         119 |       16.0 | 29.1 |
| 2001 | Montreal | All persons | Low income cut-offs after tax, 1992 base |         569 |       17.6 | 30.5 |
| 2001 | Ottawa   | All persons | Low income cut-offs after tax, 1992 base |         117 |       10.6 | 37.7 |
| 2001 | Toronto  | All persons | Low income cut-offs after tax, 1992 base |         481 |       10.6 | 37.2 |
| 2001 | Winnipeg | All persons | Low income cut-offs after tax, 1992 base |          88 |       13.7 | 30.5 |
| 2001 | Calgary  | All persons | Low income cut-offs after tax, 1992 base |          88 |       10.1 | 35.1 |

## 2.7 Cleanup

Now that we’ve all the necessary recoding, filtering, and pivoting,
let’s do some final cleanup of removing any unneeded columns.

For my analysis, I’m only keeping the “City”, “Year”, and “Percentage”
columns to keep the pairwise City and Year panel observation and its
corresponding value.

``` r
LICO_Part_6 <- LICO_Part_5 %>%
  
  select(City,
         Year,
         Percentage)

LICO_Part_6 %>% head() %>% kable()
```

| City     | Year | Percentage |
| :------- | ---: | ---------: |
| Quebec   | 2001 |       16.0 |
| Montreal | 2001 |       17.6 |
| Ottawa   | 2001 |       10.6 |
| Toronto  | 2001 |       10.6 |
| Winnipeg | 2001 |       13.7 |
| Calgary  | 2001 |       10.1 |

Now our dataset looks something we can incorporate into a dataframe to
use in `plm()`, assuming the rest of our variables all end up looking
like that too, which they do.

Just for some visualization, as I haven’t really done any until now, I’m
going to plot the percentage over time faceted by each city.

``` r
LICO_Part_6 %>%
  
  ggplot(aes(Year,Percentage, colour = City)) + 
  
  geom_line() + 
  
  xlab("Year") + 
  
  ylab("Percentage of Persons Below After-Tax LICO (%)") + 
  
  ggtitle("LICO Plots by City over Time")
```

![](MA_Paper_GitHub_files/figure-gfm/plotting%20LICO-1.png)<!-- -->

# 3 Temporal Disaggregation

Now that our data is “tidy”, we need to temporally disaggregate each
series to a monthly frequency.

This is done with the `td()` function from the `tempdisagg` package.

Before we move any further, we must do one more pivot so each city will
be its own column, so `td()` can do its job properly.

``` r
LICO_Part_7 <- LICO_Part_6 %>%
  
  pivot_wider(names_from = City,
              values_from = Percentage) %>%
  
  arrange(Year)

LICO_Part_7 %>% kable()
```

| Year | Quebec | Montreal | Ottawa | Toronto | Winnipeg | Calgary | Edmonton | Vancouver |
| ---: | -----: | -------: | -----: | ------: | -------: | ------: | -------: | --------: |
| 2001 |   16.0 |     17.6 |   10.6 |    10.6 |     13.7 |    10.1 |     11.2 |      16.3 |
| 2002 |   12.5 |     15.5 |   12.4 |    13.0 |     15.3 |     9.3 |     11.7 |      18.9 |
| 2003 |   13.2 |     16.1 |   13.2 |    11.1 |     15.7 |    13.6 |     10.3 |      16.7 |
| 2004 |   11.7 |     13.9 |   13.7 |    12.8 |     14.0 |    10.5 |     11.9 |      16.8 |
| 2005 |   10.5 |     14.6 |   10.0 |    13.1 |     14.7 |     8.7 |      9.5 |      15.1 |
| 2006 |    8.4 |     15.9 |    9.8 |    16.4 |     14.4 |     8.8 |      8.3 |      18.5 |
| 2007 |    9.2 |     15.9 |    7.8 |    14.8 |     12.7 |     7.3 |      7.4 |      15.7 |
| 2008 |    6.5 |     15.7 |   12.2 |    12.6 |     11.4 |     7.0 |      8.4 |      14.3 |
| 2009 |    4.5 |     13.9 |    9.2 |    15.0 |     11.8 |     8.7 |      9.4 |      17.3 |
| 2010 |    6.7 |     13.7 |   11.1 |    12.6 |     13.0 |     9.2 |      9.1 |      14.8 |
| 2011 |    6.6 |     14.2 |   10.3 |    12.8 |     11.4 |     8.5 |     10.0 |      15.5 |
| 2012 |   10.6 |     14.6 |   10.1 |    15.5 |     13.3 |     8.3 |      6.6 |      11.8 |
| 2013 |    5.9 |     15.1 |   11.8 |    15.3 |     12.6 |     9.1 |      7.1 |      12.3 |
| 2014 |   10.2 |     10.3 |    9.1 |    13.3 |     11.2 |     8.3 |      9.0 |      10.9 |
| 2015 |    8.5 |     14.1 |   10.1 |    11.8 |     13.3 |     8.3 |      5.4 |      14.0 |
| 2016 |    7.1 |     11.5 |    8.5 |    11.2 |     10.1 |     7.0 |      6.2 |      10.3 |
| 2017 |    5.0 |     12.8 |    8.1 |     9.5 |     10.2 |     6.4 |      6.5 |      10.3 |
| 2018 |    9.6 |     10.3 |    8.8 |     9.5 |     10.6 |     9.3 |      5.9 |       8.4 |

We have 18 years’ worth of data, multiply by 12 months, and our new
dataframe must have 216 rows to conduct this properly.

So let’s create an empty matrix which we’ll populate later, for the
`for()` loop implementation.

I also run the disaggregation using `map_dfc()` from `purrr` to create
the object, which doesn’t need an empty dataframe/matrix beforehand.

``` r
LICO_td <- matrix(nrow = nrow(LICO_Part_7) * 12L, 
                  ncol = ncol(LICO_Part_7)) %>%
  
  as_tibble()
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
colnames(LICO_td) <- colnames(LICO_Part_7)

LICO_td
```

    ## # A tibble: 216 x 9
    ##    Year  Quebec Montreal Ottawa Toronto Winnipeg Calgary Edmonton Vancouver
    ##    <lgl> <lgl>  <lgl>    <lgl>  <lgl>   <lgl>    <lgl>   <lgl>    <lgl>    
    ##  1 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  2 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  3 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  4 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  5 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  6 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  7 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  8 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ##  9 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ## 10 NA    NA     NA       NA     NA      NA       NA      NA       NA       
    ## # ... with 206 more rows

## 3.1 Dissaggregating

The next chunk of code will be using the `tempdisagg` package so we can
do our disaggregations.

The package does not interface in a “tidy” way, so base-R-style `for()`
loops will have to be used to get the desired results.

However, in the chunk following this, I’ll use `map_dfc()` from `purrr`
(also part of the Tidyverse) to iterate over the dataframe in a “tidy”
way, supplanting for loops.

There’s probably a way you can do the following with the `across()` and
`mutate()` functions from `dplyr` but I didn’t experiment with that.

As stated earlier, since I’m not using any indicator series to perform
the disaggregations, our estimated \(AR(1)\) regressor will be regressed
on a vector of ones instead.

This is accomplished by using the “Denton-Cholette” method.

``` r
# A for loop implementation
for(i in seq(ncol(LICO_td) - 1L)) {
  LICO_td[[(i+1L)]] <- td(LICO_Part_7[[(i+1)]] ~ 1L,
     method = "denton-cholette",
     to = 12,
     conversion = "mean")$values
}


# A purrr-style implementation

# Also more "tidy"

LICO_Part_7 %>%
  
  select(-Year) %>%
  
  map_dfc( ~ td(.x ~ 1L,
                method = "denton-cholette",
                to = 12L,
                conversion = "mean")$values) %>%
  
  mutate(Year_Month = seq.Date(from = LICO_Part_7$Year %>% 
                                 min() %>% 
                                 
                                 # For ISO 8601 standard, making it safer to parse
                                 paste("01","01",sep = "-") %>% 
                                 
                                 lubridate::ymd() %>% 
                                 as.Date(),
                               
                               by = "month",
                               length.out = (nrow(LICO_Part_7) * 12L))) %>%
  
  relocate(Year_Month,
           .before = Quebec) -> LICO_td

LICO_td %>% head() %>% kable()
```

| Year\_Month |   Quebec | Montreal |   Ottawa |   Toronto | Winnipeg |  Calgary | Edmonton | Vancouver |
| :---------- | -------: | -------: | -------: | --------: | -------: | -------: | -------: | --------: |
| 2001-01-01  | 17.00080 | 18.24057 | 10.15116 |  9.796798 | 13.33935 | 10.66128 | 10.92921 |  15.44209 |
| 2001-02-01  | 16.95881 | 18.21369 | 10.16999 |  9.830499 | 13.35448 | 10.63773 | 10.94057 |  15.47809 |
| 2001-03-01  | 16.87482 | 18.15994 | 10.20766 |  9.897900 | 13.38475 | 10.59063 | 10.96330 |  15.55008 |
| 2001-04-01  | 16.74885 | 18.07931 | 10.26415 |  9.999003 | 13.43014 | 10.51998 | 10.99738 |  15.65807 |
| 2001-05-01  | 16.58088 | 17.97180 | 10.33948 | 10.133806 | 13.49067 | 10.42578 | 11.04283 |  15.80205 |
| 2001-06-01  | 16.37093 | 17.83741 | 10.43365 | 10.302310 | 13.56633 | 10.30803 | 11.09964 |  15.98203 |

## 3.2 Pivoting Long

Looking at the tail end of our data, we seem to have properly indexed
our disaggregated data, now we have to pivot it back to our “tidy”
requirements.

Once again, we will be using the `tidyr` package, but this time we will
go in the opposite direction from our previous pivoting and convert
columns into *observations*.

``` r
LICO_td_pivot <- LICO_td %>%
  
  pivot_longer(-Year_Month,
               names_to = "City",
               values_to = "LICO") %>%
  
  
  relocate(Year_Month, .before = City) %>%
  
  arrange(City,
          Year_Month)

LICO_td_pivot %>% head(n = 13L) %>% kable()
```

| Year\_Month | City    |      LICO |
| :---------- | :------ | --------: |
| 2001-01-01  | Calgary | 10.661283 |
| 2001-02-01  | Calgary | 10.637733 |
| 2001-03-01  | Calgary | 10.590632 |
| 2001-04-01  | Calgary | 10.519981 |
| 2001-05-01  | Calgary | 10.425780 |
| 2001-06-01  | Calgary | 10.308028 |
| 2001-07-01  | Calgary | 10.166726 |
| 2001-08-01  | Calgary | 10.001874 |
| 2001-09-01  | Calgary |  9.813471 |
| 2001-10-01  | Calgary |  9.601518 |
| 2001-11-01  | Calgary |  9.366015 |
| 2001-12-01  | Calgary |  9.106961 |
| 2002-01-01  | Calgary |  8.824357 |

The column “Year\_Month” I added will be one of the two indices (along
with “City”) to properly indicate our observations in our dataframe for
`plm()` to use.

# 4 Instrumental Variables

So we’ve collected our data on our exogenous regressors, we still need
data for our instrumental variables, which are Wind Direction and Wind
Speed.

Environment Canada has an API set up to pull historical data in bulk,
depending on the city, time, or frequency needed.

## 4.1 Pulling Climate Data

In the following example, I will only pull data for one station ID for
one year, as iterating over multiple station ID’s, years, and months
takes a significant amount of time, though I will provide the
`foreach()` loop I wrote to accomplish this in the next code chunk.

``` r
# First part of the URL needed
URL_first <- "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID="

# Second part of the URL needed
URL_second <- "&Year="

# Third part of the URL needed
URL_third <- "&Month="

# Fourth part of the URL needed for hourly frequency
URL_fourth <- "&timeframe=1&submit= Download+Data"

# Example station ID in Yellowknife, Northwest Territories
station = 1706

# Example year and month, 2006 and January, respectively
year = 2006
month = 1

# readr's read_csv() does not work with the following code for some reason I don't understand
read.csv(paste0(URL_first,
                station,
                URL_second,
                year,
                URL_third,
                month,
                URL_fourth)) %>% 
  
  as_tibble() 
```

    ## # A tibble: 744 x 28
    ##    ï..Longitude..x. Latitude..y. Station.Name Climate.ID Date.Time  Year Month
    ##               <dbl>        <dbl> <chr>             <int> <chr>     <int> <int>
    ##  1            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  2            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  3            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  4            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  5            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  6            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  7            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  8            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ##  9            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ## 10            -114.         62.5 YELLOWKNIFE~    2204100 2006-01-~  2006     1
    ## # ... with 734 more rows, and 21 more variables: Day <int>, Time <chr>,
    ## #   Temp..Â.C. <dbl>, Temp.Flag <lgl>, Dew.Point.Temp..Â.C. <dbl>,
    ## #   Dew.Point.Temp.Flag <chr>, Rel.Hum.... <int>, Rel.Hum.Flag <chr>,
    ## #   Wind.Dir..10s.deg. <int>, Wind.Dir.Flag <lgl>, Wind.Spd..km.h. <int>,
    ## #   Wind.Spd.Flag <lgl>, Visibility..km. <dbl>, Visibility.Flag <lgl>,
    ## #   Stn.Press..kPa. <dbl>, Stn.Press.Flag <lgl>, Hmdx <lgl>, Hmdx.Flag <lgl>,
    ## #   Wind.Chill <int>, Wind.Chill.Flag <lgl>, Weather <chr>

As you can probably see, this API can be easily iterated over multiple
station ID’s, years, and months to collect a very large amount of data.

The following chunk of code you should not run as it takes a very long
time to download, and will eat up multiple gigabytes in size.

It makes use of `foreach()`’s excellent `.combine` argument to determine
what kind of format the iterated output will return.

In this case, I want each iteration to append its rows to the previous,
returning a single dataframe in the end to use.

It uses a dataframe called `stations` that contains all relevant
monitoring station metadata.

It’s based off the Station Inventory CSV file that you can grab
[here](https://drive.google.com/drive/folders/1WJCDEU34c60IfOnG4rv5EPZ4IhhW9vZH).

``` r
data_hourly <- foreach(station=seq_along(stations$station_ID), .combine  = "rbind") %:%
  
foreach(year=seq(from = stations$first[[station]], to = stations$last[[station]]), .combine = "rbind") %:%
  
foreach(month=seq(12), .combine = "rbind") %do% {
  
read.csv(paste0(URL_first,stations[station],URL_second,year,URL_third,month,URL_fourth))
  
}
```

## 4.2 Temporal Aggregation

Since the frequency of this data is too fine (daily/hourly), it must be
aggregated up to a monthly frequency.

Once again, `dplyr` comes to the rescue, with `group_by()` and
`summarise()` making this fairly simple to do.

``` r
summarised <- data_hourly %>% 
  
  # In case you want to speed up dplyr's operations
  lazy_dt() %>%
  
  # We need to get the city names from our stations dataframe so we can group the summarised data by city
  inner_join(stations, 
             by = c("Climate_ID" = "Climate ID")) %>%

  
  
  filter(!Direction %>% is.na(), # Filtering out any observations where Wind Direction is null
         Year >= 1991 # None of the other variables I'll use have data before 1991
         ) %>%

  
  group_by(City, Year, Month) %>% 
  
  summarise(Dir = mean(Direction), 
            Spd = mean(Speed)) %>%
  
  mutate(Year_Month = Year %>% 
           paste(Month,"1",sep = "-") %>%
           lubridate:: ymd() %>%
           as.Date()) %>%
  
  ungroup() %>%
  
  select(-Year,
         -Month) %>%
    
    # If you've used dbplyr before, then just like dbplyr, dtplyr needs collect() to be put at the end of your call, or the data will not be pulled.
    collect() %>%
  
    relocate(Year_Month,
           .before = City)
  
 summarised %>% head(n = 12L) %>% kable()
```

| Year\_Month | City    |      Dir |      Spd |
| :---------- | :------ | -------: | -------: |
| 1991-01-01  | Calgary | 24.86103 | 16.47278 |
| 1991-02-01  | Calgary | 22.29495 | 16.37382 |
| 1991-03-01  | Calgary | 19.31728 | 13.46034 |
| 1991-04-01  | Calgary | 24.87537 | 19.00297 |
| 1991-05-01  | Calgary | 20.76791 | 14.47564 |
| 1991-06-01  | Calgary | 22.38529 | 14.46471 |
| 1991-07-01  | Calgary | 21.62084 | 12.71056 |
| 1991-08-01  | Calgary | 22.14222 | 11.36000 |
| 1991-09-01  | Calgary | 22.99235 | 12.09327 |
| 1991-10-01  | Calgary | 23.03752 | 16.18759 |
| 1991-11-01  | Calgary | 22.96210 | 13.28426 |
| 1991-12-01  | Calgary | 23.22600 | 14.54357 |

# 5 Pollution Data

One of the last pieces of data needed to be obtained is data on fine
particulate matter at 2.5 microns (PM2.5).

This variable will be our main endogenous regressor that we need to
supplant with our previously prepared wind data.

This data is available from the National Air Pollution Surveillance
(NAPS) program.

The various years’ worth of data is available
[here](http://data.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/).

The data available at the NAPS website are (in my opinion) pretty poorly
catalogued;

  - Being behind .ZIP files

  - The format of the underlying CSV files changing sporadically
    
      - Depending on which year you’re look at

As such, I’m not going to go through the process I went through of
tidying and cleaning it up.

I mainly:

  - Brought each year’s data into Excel and did some manual cleaning
  - Consolidated all years’ data into one file
  - Brought that one file into R

To compensate for the lack of code, here is a visualization of pollution
levels over time faceted by City.

``` r
pollution %>%
  
  ggplot(aes(Date, Pollution)) + 
  
  geom_line() +
  
  facet_wrap( ~ City) + 
  
  ylab(expression(paste("PM2.5 (",mu,"g/m"^3,")"))) + 
  
  ggtitle("PM2.5 Concentrations over Time by City")
```

![](MA_Paper_GitHub_files/figure-gfm/poll%20viz-1.png)<!-- -->

# 6 House Price Index Data

Finally, our dependent variable is the Teranet/National Bank of Canada
House Price Index (THPI).

This data needs the least amount of preparation done as it’s already
provided in a “tidy” format and in the frequency needed (monthly).

The only barrier to obtaining the data is submitting an email address,
first name, and last name to receive the data.

The data can be found [here](https://housepriceindex.ca/index-history/).

``` r
pollution %>%
  
  ggplot(aes(Date,Index)) + 
  
  geom_line() + 
  
  facet_wrap(~City) + 
  
  ylab("THPI Index") + 
  
  ggtitle("THPI Index Over Time by City")
```

![](MA_Paper_GitHub_files/figure-gfm/price%20viz-1.png)<!-- -->

# 7 Initial Regression Models

Behind the scenes, I’ve pulled the rest of my data, and gone through
cleaning, joining, (dis)aggregating certain series.

Now we are at a point where we have a single `pdata.frame` containing:

  - Our *Exogenous* variables
    
      - Mainly socioeconomic data from StatsCan

  - Our *Endogenous* variable
    
      - Air Pollution (PM2.5)
        
          - Pulled from NAPS

  - Our *Instrumental Variables*
    
      - Wind Direction and Wind Speed
        
          - Pulled from Environment Canada

  - Our *Dependent* variable
    
      - The Teranet/National Bank of Canada House Price Index (THPI)

  - Our *Panel Indices*
    
      - City and Year\_Month

The following code chunk will contain some formulae that we’ll recycle
over different models, which saves me from typing/copy-pasting the
different formulae over and over again.

After, we will run the following regression models with `plm::plm()`:

  - Pooled
    
      - Basically pretending the panel is a single cross-section, just
        like “regular” OLS

  - Fixed-Effects (also called a “Within” model)
    
      - All observations are *completely* temporally de-meaned.
    
      - Does *not* assume orthogonality between explanatory variables
        and the unobserved fixed effect - Therefore no constant
        
          - Time-invariant variables are eliminated, and therefore
            redundant/useless
        
          - The error term **only** includes time-varying/idiosyncratic
            residuals

  - Random-Effects
    
      - All observations are *partially* temporally de-meaned
    
      - *Does* assume orthogonality between explanatory variables and
        the unobserved time-invariant effect
        
          - The constant is retained
        
          - Time-invariant variables are *not* eliminated
        
          - The error term includes **both** time-invariant, *and*
            time-varying/idiosyncratic residuals

With that out of the way, I’ll briefly present my model and its
components:

## 7.1 Overview of Model(s) Estimated

Our main model of interest is as follows:

\(THPI = \beta_0 + \theta \widehat{Pollution_{it}} + \beta X_{it} + \eta_i + \epsilon_{it}\)

Where:

  - \(\beta_0\) is our time and panel-invariant that will be included
    *only* in our Pooled-OLS and Random-Effects models.
  - \(\beta\) is a \(1 \times K\) vector containing the coefficients
    corresponding to our *exogenous* regressors
  - \(X_{it}\) is a \(N \times K\) matrix containing the data on our
    exogenous regressors
  - \(\theta\) is our coefficient for the fitted-values of Pollution
  - Fitted values are obtained from the first-stage regression, shown
    below
  - \(\widehat{Pollution_{it}}\) is the fitted-values of Pollution on
    our exogenous and instrumental regressors from the first-stage
    regression
  - \(\eta_i\) is our time-invariant error that will theoretically be
    eliminated in our Fixed-Effects/Within model
  - \(\epsilon_{it}\) is our time-varying/idiosyncratic error

The first-stage regression is as follows:

\(Pollution = \alpha_0 + \alpha Z_{it} + \upsilon_i + \omega_{it}\)

Where:

  - \(Z_{it}\) is a \(N \times L\) (where \(L \geq K\)) matrix
    containing data on our exogenous regressors *and* our instruments
  - \(\alpha\) is a \(1 \times L\) vector containing the coefficients
    for our instruments and exogenous regressors
  - \(\alpha_0\) is our time and panel-invariant constant which is
    included *only* in our Pooled-OLS and Random-Effects models
  - \(\upsilon_i\) is our time-invariant error that will theoretically
    be eliminated in our Fixed-Effects/Within model
  - \(\omega_{it}\) is our time-varying/idiosyncratic error

## 7.2 Formulae & OLS

``` r
# Some formulae that we'll come back to as we explore some initial models for fit

# Regular OLS
form_ols <- Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop

# IV formula with both Direction and Speed
form_iv <- Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop | Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + Direction + Speed

# IV formula with just direction
form_iv_dir <- Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop | Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + Direction

# IV formula with just speed
form_iv_spd <- Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop | Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + Speed

pooled_ols <- plm(form_ols,
                  model = "pooling",
                  effect =  "individual",
                  data = pollution)

random_ols <- plm(form_ols,
                 model = "random",
                 effect = "individual",
                 random.method = "amemiya",
                 data = pollution)

fixed_ols <- plm(form_ols,
                 model = "within",
                 effect = "individual",
                 data = pollution)

stargazer(pooled_ols, fixed_ols, random_ols, type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

Index

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Pollution

</td>

<td>

1.480<sup>\*\*\*</sup>

</td>

<td>

0.051

</td>

<td>

0.046

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.244)

</td>

<td>

(0.127)

</td>

<td>

(0.127)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Homicide

</td>

<td>

4.938<sup>\*\*\*</sup>

</td>

<td>

\-0.059

</td>

<td>

\-0.080

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.665)

</td>

<td>

(0.562)

</td>

<td>

(0.562)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-3.232<sup>\*\*\*</sup>

</td>

<td>

\-0.990<sup>\*\*\*</sup>

</td>

<td>

\-0.998<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.140)

</td>

<td>

(0.106)

</td>

<td>

(0.106)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

LowInc

</td>

<td>

\-6.859<sup>\*\*\*</sup>

</td>

<td>

\-2.036<sup>\*\*\*</sup>

</td>

<td>

\-2.039<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.317)

</td>

<td>

(0.188)

</td>

<td>

(0.188)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Rent

</td>

<td>

0.208<sup>\*\*\*</sup>

</td>

<td>

0.261<sup>\*\*\*</sup>

</td>

<td>

0.262<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

<td>

(0.005)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Unemp

</td>

<td>

1.134<sup>\*</sup>

</td>

<td>

\-6.302<sup>\*\*\*</sup>

</td>

<td>

\-6.295<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.643)

</td>

<td>

(0.334)

</td>

<td>

(0.334)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Vacancy

</td>

<td>

1.700<sup>\*\*\*</sup>

</td>

<td>

2.310<sup>\*\*\*</sup>

</td>

<td>

2.299<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.500)

</td>

<td>

(0.259)

</td>

<td>

(0.259)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

\-0.007<sup>\*\*\*</sup>

</td>

<td>

0.028<sup>\*\*\*</sup>

</td>

<td>

0.028<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

<td>

(0.002)

</td>

<td>

(0.002)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

209.617<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

\-29.945

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(8.915)

</td>

<td>

</td>

<td>

(26.598)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,507

</td>

<td>

1,507

</td>

<td>

1,507

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.643

</td>

<td>

0.912

</td>

<td>

0.912

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.642

</td>

<td>

0.911

</td>

<td>

0.911

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

337.904<sup>\*\*\*</sup> (df = 8; 1498)

</td>

<td>

1,937.245<sup>\*\*\*</sup> (df = 8; 1491)

</td>

<td>

15,492.520<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

From left to right our models are as follows:

  - Pooled OLS
  - Fixed-Effects
  - Random-Effects

So running our regressions *without* any instrumental variables leads to
a Pollution regressor that is not only the wrong sign desired
(positive), but also not very significant in either magnitude (very
close to zero), nor statistical significance in our latter two models.

This improves only marginally as we introduce fixed and random effects
into our models.

Our adjusted-\(R^2\) improves significantly, however.

Let’s see if introducing our two instruments improves this situation at
all.

## 7.3 IV Models

``` r
pooled_iv <- plm(form_iv,
                 model = "pooling",
                 effect = "individual",
                 data = pollution)

random_iv <- plm(form_iv,
                model = "random",
                effect = "individual",
                random.method = "amemiya",
                data = pollution)

fixed_iv <- plm(form_iv,
                 model = "within",
                 effect = "individual",
                 data = pollution)


stargazer(pooled_iv, fixed_iv, random_iv, type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="3">

Index

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

<td>

(3)

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Pollution

</td>

<td>

\-5.081<sup>\*\*</sup>

</td>

<td>

\-8.452<sup>\*\*\*</sup>

</td>

<td>

\-8.104<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.289)

</td>

<td>

(1.600)

</td>

<td>

(1.574)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Homicide

</td>

<td>

2.486<sup>\*\*</sup>

</td>

<td>

\-3.517<sup>\*\*\*</sup>

</td>

<td>

\-3.444<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.173)

</td>

<td>

(1.296)

</td>

<td>

(1.253)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-2.998<sup>\*\*\*</sup>

</td>

<td>

\-1.203<sup>\*\*\*</sup>

</td>

<td>

\-1.212<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.189)

</td>

<td>

(0.216)

</td>

<td>

(0.208)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

LowInc

</td>

<td>

\-8.197<sup>\*\*\*</sup>

</td>

<td>

\-1.328<sup>\*\*\*</sup>

</td>

<td>

\-1.389<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.603)

</td>

<td>

(0.398)

</td>

<td>

(0.386)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Rent

</td>

<td>

0.180<sup>\*\*\*</sup>

</td>

<td>

0.318<sup>\*\*\*</sup>

</td>

<td>

0.317<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.011)

</td>

<td>

(0.015)

</td>

<td>

(0.014)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Unemp

</td>

<td>

2.623<sup>\*\*\*</sup>

</td>

<td>

\-5.109<sup>\*\*\*</sup>

</td>

<td>

\-5.122<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.938)

</td>

<td>

(0.705)

</td>

<td>

(0.684)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Vacancy

</td>

<td>

2.632<sup>\*\*\*</sup>

</td>

<td>

1.700<sup>\*\*\*</sup>

</td>

<td>

1.708<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.689)

</td>

<td>

(0.532)

</td>

<td>

(0.513)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

\-0.004<sup>\*\*\*</sup>

</td>

<td>

0.003

</td>

<td>

0.003

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

<td>

(0.007)

</td>

<td>

(0.006)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

269.516<sup>\*\*\*</sup>

</td>

<td>

</td>

<td>

41.769

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(23.395)

</td>

<td>

</td>

<td>

(25.617)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,507

</td>

<td>

1,507

</td>

<td>

1,507

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.495

</td>

<td>

0.707

</td>

<td>

0.718

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.492

</td>

<td>

0.704

</td>

<td>

0.716

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

1,801.790<sup>\*\*\*</sup>

</td>

<td>

3,887.335<sup>\*\*\*</sup>

</td>

<td>

4,118.936<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td colspan="4" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="3" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

The ordering of models is the same as previous.

What a stark contrast to before:

  - In all three models, the Pollution regressor is the sign we want
    (negative), and significant in magnitude *and* statistical
    significance.
    
      - Especially after introducing Fixed/Random effects -Our Homicide
        Rate and Unemployment regressors are the correct sign in the
        latter two models

What’s disappointing is our Vacancy regressor is supposed to be
negative, and our Income regressor should be positive.

  - This may be due to some caveats with regards to the data *and* the
    models, as I’ll discuss later.

Let’s move on to some diagnostic tests and further discussion/selection
of our models.

# 8 Diagnostics and Further Refinements

This section will contain various tests about our models to see which
one has superior properties or explanatory power.

## 8.1 F-Test for Effects

The first test we’ll conduct is that of testing for individual effects
in our models.

Compared to fixed-effects, our Pooled-OLS model has no effects that our
different Cities might introduce into the model.

This first F-test examines whether introducing City effects into our
model changes our model enough to warrant including them.

``` r
pFtest(fixed_iv, pooled_iv)
```

    ## 
    ##  F test for individual effects
    ## 
    ## data:  form_iv
    ## F = 131.8, df1 = 7, df2 = 1491, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

We easily reject the null hypothesis of no individual effects in our
data.

Therefore, we are justified in including City-level effects, and
abandoning our Pooled-OLS model.

## 8.2 Hausman Test

The next test we’ll conduct is that of the Durbin-Wu-Hausman test.

This tests whether both of our Fixed-Effects and Random-Effects models
are consistent or not.

The null hypothesis is that both models are consistent, but
Random-Effects is more efficient.

The alternative hypothesis is that only our Fixed-Effects model is
consistent.

``` r
phtest(fixed_iv, random_iv)
```

    ## 
    ##  Hausman Test
    ## 
    ## data:  form_iv
    ## chisq = 1.9713, df = 8, p-value = 0.9819
    ## alternative hypothesis: one model is inconsistent

We fail to reject the null, and therefore conclude that while both Fixed
and Random-Effects are consistent, our Random-Effects model is the more
efficient.

So we seemed to have rejected both our Pooled OLS model (due to the
significant effects Cities have on the model), and our Fixed-Effects
model (due to Random-Effects being more efficient).

Let’s double-check with one final test to see if Random-Effects is
superior to our Pooled model.

## 8.3 Breusch-Pagan LM Test

This final test is the Breusch-Pagan Lagrange Multiplier test.

This test is similar to the the F test we conducted earlier, however
that test is a comparison between our Pooled-OLS model and our Within
model, *not* between our Pooled-OLS model and our Random-Effects model.

The null hypothesis states that there are no panel effects in our model.

``` r
plmtest(pooled_iv, type="bp")
```

    ## 
    ##  Lagrange Multiplier Test - (Breusch-Pagan) for unbalanced panels
    ## 
    ## data:  form_iv
    ## chisq = 7950.6, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: significant effects

Like before, we reject the null and find that there are significant
individual effects in our model to warrant including them.

We’ll continue on with our Random-Effects-IV model as the model to beat.

# 9 Instrumental Variable (IV) Diagnostics

In order for our IV models to be consistent, we need to examine the
extent to which:

  - Our instruments are relevant *and* strong enough
  - Our suspected endogenous variable is **actually** endogenous
  - We are over-identifying in restrictions by having more instruments
    than endogenous variables

A lot of this testing is fairly easy in a single-cross-section context
with `AER::summary.ivreg(diagnostics=TRUE)`, however, `AER`’s `ivreg()`
cannot incorporate fixed or random effects into the model automatically.

A lot of the following code is replicating the code for the
`diagnostics=TRUE` argument found in `AER::summary.ivreg()` to test the
“IV” aspects of our Random-Effects-Instrumental-Variable (REIV) model.

Someone could probably make a wrapper to provide
`AER::summary.ivreg(diagnostics=TRUE)` functionality for panel data
models constructed in `plm::plm()`

Let’s proceed.

## 9.1 F-Testing our Instruments

The first step in this process is to conduct an F-test for potentially
weak/irrelevant instruments.

``` r
# Wald test on our first stage regression for instrument validity

# Formula for our first-stage regression WITH out instruments
form_first <- Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + Direction + Speed

# Formula for our first-stage regression WITHOUT our instruments
form_first_no_inst <- Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop

# Formula for our first-stage regression with only the Wind Direction instrument
form_first_dir <- Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + Direction

# Formula for our first-stage regression with only the Wind Speed instrument
form_first_spd <- Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + Speed

first_stage <- plm(form_first,
                   model = "random",
                   random.method = "amemiya",
                   data = pollution)

first_stage_no_inst <- plm(form_first_no_inst,
                           model = "random",
                           random.method = "amemiya",
                           data = pollution)

first_stage_dir <- plm(form_first_dir,
                           model = "random",
                           random.method = "amemiya",
                           data = pollution)

first_stage_spd <- plm(form_first_spd,
                           model = "random",
                           random.method = "amemiya",
                           data = pollution)


first_stage %>% stargazer(type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Pollution

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Homicide

</td>

<td>

\-0.346<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.113)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-0.034

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.021)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

LowInc

</td>

<td>

0.065<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Rent

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Unemp

</td>

<td>

0.152<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.067)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Vacancy

</td>

<td>

\-0.067

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

\-0.003<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.0005)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Direction

</td>

<td>

0.066<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.021)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Speed

</td>

<td>

\-0.053<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.010)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

9.097<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.701)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,507

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.072

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.067

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

117.028<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

Both of our instruments are statistically significant in our first-stage
regression, indicating that they are strong instruments for our
analysis.

``` r
lmtest::waldtest(first_stage_no_inst,first_stage)
```

    ## Wald test
    ## 
    ## Model 1: Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + 
    ##     Pop
    ## Model 2: Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + 
    ##     Pop + Direction + Speed
    ##   Res.Df Df  Chisq Pr(>Chisq)    
    ## 1   1499                         
    ## 2   1497  2 36.782   1.03e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We soundly reject the null of no joint significance, and conclude that
our two instruments are relevant and strong for this model.

In summary, we find that our two instrumental variables, Wind Direction
and Wind Speed, are strong *and* relevant instruments for our REIV
model.

## 9.2 Wu-Hausman Test for Endogeneity

The following test will determine whether our suspected endogenous
variable “Pollution” is *actually* endogenous based off incorporating
the fitted values of our first-stage regression (basically our
Pollution-“hat” values) as an additional exogenous regressor.

``` r
first_stage_fit <- fitted(first_stage)


pollution$fitted_first <- first_stage_fit


lmtest::waldtest(Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + fitted_first,
                 . ~ . -fitted_first,
                 data = pollution)
```

    ## Warning: Using formula(x) is deprecated when x is a character vector of length > 1.
    ##   Consider formula(paste(x, collapse = " ")) instead.

    ## Wald test
    ## 
    ## Model 1: Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp + 
    ##     Vacancy + Pop + fitted_first
    ## Model 2: Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp + 
    ##     Vacancy + Pop
    ##   Res.Df Df      F   Pr(>F)   
    ## 1   1497                      
    ## 2   1498 -1 10.574 0.001172 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

We reject the null hypothesis of no joint significance, and conclude
that OLS is inconsistent due to the presence of endogeneity with
Pollution, therefore only our IV method is consistent, so far.

## 9.3 Sargan Test for Over-Identified Restrictions

We have one more test to run as we have more instrumental variables than
endogenous variables (2 vs 1, respectively), which leaves us in a case
of over-identifying restrictions.

We need to conduct the Sargan test to see if we’re warranted in having
more instruments than endogenous variables in our REIV model.

``` r
random_res <- random_iv$residuals

pollution$res <- random_res


auxs <- plm(res ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + Pop + Direction + Speed,
            model = "random",
            random.method = "amemiya",
            data = pollution)


# Sum of Squared Residuals of our Auxiliary regression of our instruments on our model residuals
rss <- auxs$residuals^2 %>% sum()

rss
```

    ## [1] 702479.5

``` r
rssr <- (random_res - mean(random_res))^2 %>% sum()

rssr
```

    ## [1] 711228.3

``` r
# Test Statistic
Q <- length(random_res) * (1 - rss/rssr)

Q
```

    ## [1] 18.53745

``` r
# Chi-squared Test 
pchisq(Q, 1L, lower.tail = FALSE)
```

    ## [1] 1.665982e-05

As we can see, our p-value is very much under all common critical
values, rejecting the null that our over-identified restrictions are
valid.

We must now determine which, between Wind Direction or Wind Speed, is
the better instrumental variable to use.

## 9.4 Re-fitting with Single Instruments

Let’s re-conduct our Wald test for weak instruments, and then re-fit our
models to see which instrument fares better.

``` r
first_stage_dir %>% stargazer(type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Pollution

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Homicide

</td>

<td>

\-0.394<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.114)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-0.020

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.022)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

LowInc

</td>

<td>

0.074<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Rent

</td>

<td>

0.006<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Unemp

</td>

<td>

0.143<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.068)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Vacancy

</td>

<td>

\-0.065

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.053)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

\-0.003<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.0004)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Direction

</td>

<td>

0.057<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.021)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

6.603<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.368)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,507

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.053

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.048

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

84.207<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

``` r
lmtest::waldtest(first_stage_dir, first_stage_no_inst)
```

    ## Wald test
    ## 
    ## Model 1: Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + 
    ##     Pop + Direction
    ## Model 2: Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + 
    ##     Pop
    ##   Res.Df Df  Chisq Pr(>Chisq)   
    ## 1   1498                        
    ## 2   1499 -1 7.2641   0.007035 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Wind Direction seems to be a relevant and strong instrument on its own.

Let’s examine Wind Speed to see if it fares any better.

``` r
first_stage_spd %>% stargazer(type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

Pollution

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Homicide

</td>

<td>

\-0.353<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.113)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-0.035

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.022)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

LowInc

</td>

<td>

0.074<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.038)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Rent

</td>

<td>

0.008<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.001)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Unemp

</td>

<td>

0.147<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.067)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Vacancy

</td>

<td>

\-0.067

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.052)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

\-0.003<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.0005)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Speed

</td>

<td>

\-0.050<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.010)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

10.450<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.678)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,507

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.066

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.061

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

106.678<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

``` r
lmtest::waldtest(first_stage_spd, first_stage_no_inst)
```

    ## Wald test
    ## 
    ## Model 1: Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + 
    ##     Pop + Speed
    ## Model 2: Pollution ~ Homicide + Income + LowInc + Rent + Unemp + Vacancy + 
    ##     Pop
    ##   Res.Df Df  Chisq Pr(>Chisq)    
    ## 1   1498                         
    ## 2   1499 -1 26.879  2.166e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Wind Speed also seems like a relevant and strong instrument, though
compared to Wind Direction, it’s statistical significance seems to be
stronger.

Since they’re both appropriate instruments for Pollution, let’s examine
them in our 2SLS model context and compare.

``` r
# REIV direction-only model
random_iv_dir <- plm(form_iv_dir,
                     model = "random",
                     random.method = "amemiya",
                     data = pollution)

# REIV speed-only model
random_iv_spd <- plm(form_iv_spd,
                     model = "random",
                     random.method = "amemiya",
                     data = pollution)


stargazer(random_iv_dir,random_iv_spd, type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="2">

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td colspan="2">

Index

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1)

</td>

<td>

(2)

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Pollution

</td>

<td>

4.308<sup>\*</sup>

</td>

<td>

\-11.494<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.408)

</td>

<td>

(2.432)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Homicide

</td>

<td>

1.645

</td>

<td>

\-4.825<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.225)

</td>

<td>

(1.721)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-0.895<sup>\*\*\*</sup>

</td>

<td>

\-1.286<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.152)

</td>

<td>

(0.274)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

LowInc

</td>

<td>

\-2.394<sup>\*\*\*</sup>

</td>

<td>

\-1.133<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.319)

</td>

<td>

(0.519)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Rent

</td>

<td>

0.235<sup>\*\*\*</sup>

</td>

<td>

0.337<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.017)

</td>

<td>

(0.019)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Unemp

</td>

<td>

\-6.891<sup>\*\*\*</sup>

</td>

<td>

\-4.627<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.556)

</td>

<td>

(0.918)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Vacancy

</td>

<td>

2.598<sup>\*\*\*</sup>

</td>

<td>

1.492<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.383)

</td>

<td>

(0.679)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

0.040<sup>\*\*\*</sup>

</td>

<td>

\-0.006

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

<td>

(0.008)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

\-65.571<sup>\*</sup>

</td>

<td>

69.419<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(38.352)

</td>

<td>

(31.227)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

<td>

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Observations

</td>

<td>

1,507

</td>

<td>

1,507

</td>

</tr>

<tr>

<td style="text-align:left">

R<sup>2</sup>

</td>

<td>

0.850

</td>

<td>

0.591

</td>

</tr>

<tr>

<td style="text-align:left">

Adjusted R<sup>2</sup>

</td>

<td>

0.849

</td>

<td>

0.589

</td>

</tr>

<tr>

<td style="text-align:left">

F Statistic

</td>

<td>

8,825.667<sup>\*\*\*</sup>

</td>

<td>

2,372.361<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td colspan="3" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td colspan="2" style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

The model only incorporating Wind Speed as a regressor leaves the most
desirable signs for our coefficients, albeit with a lower
adjusted-\(R^2\).

As it fits our model and tests better, I’ll omit the Wind Direction
model going forward.

# 10 Model Caveats

This model is not perfect:

  - There are areas where I lack the expertise/confidence to rectify
    certain shortcomings
    
      - Exploring the possibility of a **Dynamic** Panel Data model, a
        là Arellano & Bond (1991)
        
          - This *can* be done in `plm` with the `pgmm()` function.
            
              - I only have **8** cross-sections/samples in this
                dataset, and I have **8** non-constant coefficients
                
                  - I’m in a case of exactly-identified restrictions
                    (\(K = N\)), alas if I wanted to include lagged
                    dependent, independent, or IV variables, I would
                    need more cross-sections, or sacrifice some of my
                    current \(T = t\) coefficients.
                  - The ideal conditions for a Dynamic Panel Data model
                    is a “large-\(N\), small-\(T\)” dataset, leaving
                    headroom for lots of potential regressors.

  - Issues where I do not *know* how to rectify, or I’m not confident
    *can* be rectified, based off what I know (I’m not an expert, I’m
    currently trying to read through literature to help educate myself
    on this):
    
      - Cross-Sectional Dependence & Serial Correlation (explored in the
        proceeding section)
        
          - Heckman Correction can help rectify the former
          - Robust covariance matrix estimation can help with the latter

  - Potential lack of Poolability of our model
    
      - Might indicate that a Variable Coefficients Model might be more
        appropriate
        
          - Also explored further in a proceeding sub-section

Let’s address some of those concerns.

## 10.1 Cross-Sectional Dependence

Briefly, here are the consequences of each phenomenon:

  - Cross-Sectional Dependence
    
      - Indicates that our samples are *not* randomly selected, and some
        unobserved factor(s) common to all observations within the
        cross-section is not captured in the model
        
          - This *will* bias our estimators
          - This is a violation of one of the Gauss-Markov assumptions
            of independently & identically drawn samples

  - Serial Correlation in the Errors
    
      - Indicates the errors follow an auto-correlative pattern
        
          - Errors at a particular point in time are related in some way
            to past errors
          - This will *not* bias our estimators, but it will raise their
            *variances*, reducing their efficiency

### 10.1.1 Breusch-Pagan’s LM Test

``` r
pcdtest(random_iv_spd,
        test = "lm")
```

    ## 
    ##  Breusch-Pagan LM test for cross-sectional dependence in panels
    ## 
    ## data:  Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp +     Vacancy + Pop | Homicide + Income + LowInc + Rent + Unemp +     Vacancy + Pop + Speed
    ## chisq = 577.15, df = 28, p-value < 2.2e-16
    ## alternative hypothesis: cross-sectional dependence

``` r
pcdtest(random_iv_spd,
        test = "sclm")
```

    ## 
    ##  Scaled LM test for cross-sectional dependence in panels
    ## 
    ## data:  Index ~ Pollution + Homicide + Income + LowInc + Rent + Unemp +     Vacancy + Pop | Homicide + Income + LowInc + Rent + Unemp +     Vacancy + Pop + Speed
    ## z = 73.383, p-value < 2.2e-16
    ## alternative hypothesis: cross-sectional dependence

Running the standard variant of the Breusch-Pagan LM test and its Scaled
variant both indicate that we can reject the null hypothesis of no
cross-sectional dependence.

How would we rectify this?

### 10.1.2 Heckman (1976) Correction

Well, the various variables collected for this dataset, are primarily
available for a certain amount of Census Metropolitan Areas, which are
all urban areas.

Alas, not all urban areas have complete data availability (let alone
rural areas) on our dependent variable at all.

So we are in a situation where our dataset is clearly non-random.

Without getting too technical, Heckman (1976) proposed a sort of
“correction” for truncated datasets, where dependent variable data is
not available for all observations.

The so-called Heckman correction essentially helps “correct” for the
bias present in that hypothetical sample that does not have data
available for the dependent variable for all observations, by utilizing
the potentially available data on the *independent* variables by,
according to Wikipedia, “explicitly modeling the sampling probability of
each expectation along with the conditional expectation of the dependent
variable”.

The `sampleSelection` package apparently can implement the Heckman
regression, though I have not tried it yet, and have no idea if it can
be implemented in a panel-data and/or instrumental-variable context.

I will mention this as a potential follow-up project for me to attempt
later.

## 10.2 Serial Correlation

``` r
pdwtest(random_iv_spd)
```

    ## 
    ##  Durbin-Watson test for serial correlation in panel models
    ## 
    ## data:  form_iv_spd
    ## DW = 0.026519, p-value < 2.2e-16
    ## alternative hypothesis: serial correlation in idiosyncratic errors

Like the cross-sectional dependence tests, we reject the null of no
serial correlation in the errors.

## 10.3 Remedying Cross-Sectional & Serial Correlation

Though our estimators might be inconsistent due to the presence of
cross-sectional dependence, the biased variances due to serial
correlation in the errors can at least be rectified via a robust
covariance matrix estimation.

The variant I’ll be using is that of Driscoll & Kraay (1998), where the
estimation is robust against both cross-sectional dependence, *and*
serial correlation.

What are the requirements of using this estimation properly?

Per the `plm` package, this estimation is “consistent in a
\(T\)-asymptotic setting, regardless of the \(N\) dimension”.

This describes our dataset rather aptly, as we have \(T = 1507\) months
(unevenly) spread across \(N = 8\) cross-sections.

We can say that we’re in a “large-\(T\), small-\(N\)” dataset.

This is implemented in the `plm` package by using `vcovSCC()` as the
`vcov.` argument in `lmtest`’s `coeftest()` function.

``` r
lmtest::coeftest(random_iv_spd,
         vcov. = vcovSCC) %>%
  
  stargazer(type = "html")
```

<table style="text-align:center">

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

<em>Dependent variable:</em>

</td>

</tr>

<tr>

<td>

</td>

<td colspan="1" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

Pollution

</td>

<td>

\-11.494<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.109)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Homicide

</td>

<td>

\-4.825<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(2.053)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Income

</td>

<td>

\-1.286<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.262)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

LowInc

</td>

<td>

\-1.133

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.747)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Rent

</td>

<td>

0.337<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.019)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Unemp

</td>

<td>

\-4.627<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.568)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Vacancy

</td>

<td>

1.492

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(1.360)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Pop

</td>

<td>

\-0.006

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(0.008)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td style="text-align:left">

Constant

</td>

<td>

69.419<sup>\*</sup>

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

(41.345)

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

<td>

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td colspan="2" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

<em>Note:</em>

</td>

<td style="text-align:right">

<sup>*</sup>p\<0.1; <sup>**</sup>p\<0.05; <sup>***</sup>p\<0.01

</td>

</tr>

</table>

We lose statistical significance in several of our regressors.

What is key however in our takeaway, is our Pollution regressor is still
statistically significant at the 1% threshold.

## 10.4 Poolability and Variable Coefficients Models

I’m rather un-exposed to literature and research conducted using
Variable Coefficients models, but as far as I understand it, instead of
fixed coefficients across all cross-sections (common to Pooled, Fixed,
and Random effects models), each cross-section gets its **own**
coefficient for a particular variable.

I’m even less exposed to Variable-Coefficients models that use IV in
their estimation (I would appreciate some recommendations for reading
material).

So for example, there will be **eight** different coefficients for my
Pollution variable, along with 8 coefficients for every other regressor.

Let’s use the `pvcm()` function from `plm` to estimate a
Variable-Coefficients model.

``` r
# Random-effects-IV models are not supported for some reasons that I don't understand
variable_fixed <- pvcm(form_iv_spd,
                       model = "within",
                       data = pollution)
  
  variable_fixed %>% summary()
```

    ## Oneway (individual) effect No-pooling model
    ## 
    ## Call:
    ## pvcm(formula = form_iv_spd, data = pollution, model = "within")
    ## 
    ## Unbalanced Panel: n = 8, T = 178-193, N = 1507
    ## 
    ## Residuals:
    ##         Min.      1st Qu.       Median      3rd Qu.         Max. 
    ## -17.83995544  -2.62213392   0.02363387   2.44565741  19.23562367 
    ## 
    ## Coefficients:
    ##   (Intercept)        Pollution            Homicide           Income       
    ##  Min.   :-554.40   Min.   :-0.165111   Min.   :-8.3430   Min.   :-2.7769  
    ##  1st Qu.:-409.31   1st Qu.:-0.038954   1st Qu.:-6.5090   1st Qu.:-0.7924  
    ##  Median : -75.19   Median : 0.003365   Median :-0.7361   Median :-0.3788  
    ##  Mean   : -72.19   Mean   : 0.057449   Mean   : 0.7826   Mean   :-0.2718  
    ##  3rd Qu.: 224.81   3rd Qu.: 0.103750   3rd Qu.: 4.8100   3rd Qu.: 0.5823  
    ##  Max.   : 453.14   Max.   : 0.413544   Max.   :18.5158   Max.   : 1.4942  
    ##      LowInc             Rent             Unemp            Vacancy        
    ##  Min.   :-5.4671   Min.   :-0.3877   Min.   :-7.8909   Min.   :-10.2137  
    ##  1st Qu.:-2.0381   1st Qu.: 0.2340   1st Qu.:-5.5223   1st Qu.: -7.4157  
    ##  Median :-0.4860   Median : 0.3799   Median :-2.0395   Median : -1.7460  
    ##  Mean   :-1.2484   Mean   : 0.3081   Mean   :-2.9910   Mean   : -2.9359  
    ##  3rd Qu.: 0.4386   3rd Qu.: 0.4556   3rd Qu.:-0.7561   3rd Qu.:  0.8673  
    ##  Max.   : 1.2306   Max.   : 0.6310   Max.   : 1.6617   Max.   :  3.6491  
    ##       Pop          
    ##  Min.   :-0.88145  
    ##  1st Qu.:-0.28199  
    ##  Median : 0.02745  
    ##  Mean   :-0.01351  
    ##  3rd Qu.: 0.17864  
    ##  Max.   : 0.67237  
    ## 
    ## Total Sum of Squares: 1.0344e+10
    ## Residual Sum of Squares: 36569
    ## Multiple R-Squared: 1

It seems that when running a Variable-Coefficients model, our Pollution
seems to be distributed around an average of
\(\bar{\beta_{Pollution}} \approx 0\), which does not bode well for all
of the preceeding analysis.

Let’s conduct a poolability test (basically an \(F\)-test on the
coefficients to check for stability) to see if our coefficients stay
relatively stable across the multiple cross-sections.

``` r
pooltest(random_iv_spd,
         variable_fixed)
```

    ## 
    ##  F statistic
    ## 
    ## data:  form_iv_spd
    ## F = 747.27, df1 = 63, df2 = 1435, p-value < 2.2e-16
    ## alternative hypothesis: unstability

As we can see, our coefficients are not stable across cross-sections,
leading to the possibility of a Variable-Coefficients model giving
better results.

However as I previously stated, I’m not experienced with the
theory/literature of VE models, so I will leave this as a follow-up
project to conduct.

# 11 Summary

In summary:

  - We imported, cleaned, pivoted, and summarised various aspects of our
    data
    
      - Mainly accomplished with the [Tidyverse](https://tidyverse.org)
        collection of packages

  - Performed temporal disaggregation on annual data to arrive at a
    monthly frequency
    
      - Accomplished with the `tempdisagg` package

  - Estimated various panel-data OLS and Instrumental Variable (IV)
    models
    
      - Accomplished with the `plm` package

  - Performed various tests of fit, consistency, etc., to determine the
    best of our models
    
      - Mainly accomplished with `plm` and `lmtest` packages
      - Arrived at a Random-Effects-IV model for best fit

  - Performed robust covariance matrix estimation to account for
    Cross-Sectional Dependence & Serial Correlation in the errors
    
      - Accomplished with `plm::vcovSCC()` function, though the
        `sandwich` package can probably be used in a panel-data context,
        I’m not sure

Though, we ran into some caveats that require follow-up investigation
and research that I may undertake, time-willing.

Some of these are:

  - Cross-Sectional Dependence
    
      - Our data is *not* a random sample
      - Biases our coefficients
      - Partially remedied through `plm::vcovSCC()`
      - Heckman Correction (1976) may be warranted

  - Serial Correlation in the errors
    
      - Biases the variances of our estimators
      - Also partially remedied through `plm::vcovSCC()`

  - Over-Identification of Restrictions with our Instrumental Variables
    
      - Arrived at by conducting the Sargan test
        
          - Opted to use a single instrument
            
              - Wind Speed

  - Lack of Poolability of Coefficients
    
      - Indicating a Variable-Coefficients model may provide a better
        fit

Thank you for taking the time to read this. I’m continually learning
about econometrics and data science, as I find the subjects interesting
and seek to continually improve my understanding and my coding
competencies with them.

If you have any suggestions, questions, or other comments, please feel
free to contact me.

# 12 References

<div id="refs" class="references hanging-indent">

<div id="ref-cepa2007priority">

Act, Canadian Environmental Protection. 2007. “Priority Substances List
Assessment Report for Respirable Particulate Matter.” *Environment
Canada & Health Canada*.
<https://www.canada.ca/en/health-canada/services/environmental-workplace-health/reports-publications/environmental-contaminants/canadian-environmental-protection-act-1999-priority-substances-list-assessment-report-respirable-particulate-matter.html>.

</div>

<div id="ref-anderson2015asthewind">

Anderson, M. 2015. *As the Wind Blows: The Effects of Long-Term Exposure
to Air Pollution on Mortality*. National Bureau of Economic Research.

</div>

<div id="ref-arellano1991some">

Arellano, M., and S. Bond. 1991. “Some Tests of Specification for Panel
Data: Monte Carlo Evidence and an Application to Employment Equations.”
*Review of Economic Studies* 58 (2): 277–97.

</div>

<div id="ref-bajari2007estimating">

Bajari, P., and M. Khan. 2007. “Estimating Hedonic Models of Consumer
Demand with an Application to Urban Sprawl.” In *A*, 129–55. New York:
Springer.

</div>

<div id="ref-bondy2018april">

Bondy, M., S. Roth, and L. Sager. 2018. *Crime Is in the Air: The
Contemporaneous Relationship Between Air Pollution and Crime*. *IZA
Institute of Labor Economics*. <http://ftp.iza.org/dp11492.pdf>.

</div>

<div id="ref-statscan2015low">

Canada, Statistics. 2015. *Low Income Cut-Offs*. Statistics Canada.
<https://www150.statcan.gc.ca/n1/pub/75f0002m/2012002/lico-sfr-eng.htm>.

</div>

<div id="ref-stats2020inc">

———. n.d.a. “Table 11-10-0190-01 Market Income, Government Transfers,
Total Income, Income Tax and After-Tax Income by Economic Family Type.”
<https://doi.org/10.25318/1110019001-eng>.

</div>

<div id="ref-stats2020unemp">

———. n.d.b. “Table 14-10-0096-01 Labour Force Characteristics by Census
Metropolitan Area, Annual.” <https://doi.org/10.25318/1410009601-eng>.

</div>

<div id="ref-stats2020vac">

———. n.d.c. “Table 34-10-0127-01 Canada Mortgage and Housing
Corporation, Vacancy Rates, Apartment Structures of Six Units and over,
Privately Initiated in Census Metropolitan Areas.”
<https://doi.org/10.25318/3410012701-eng>.

</div>

<div id="ref-stats2020rent">

———. n.d.d. “Table 34-10-0133-01 Canada Mortgage and Housing
Corporation, Average Rents for Areas with a Population of 10,000 and
over.” <https://doi.org/10.25318/3410013301-eng>.

</div>

<div id="ref-stats2020hom">

———. n.d.e. “Table 35-10-0071-01 Number and Rate of Homicide Victims, by
Census Metropolitan Areas.” <https://doi.org/10.25318/3510007101-eng>.

</div>

<div id="ref-goc2019national">

Canada, Government of. 2019. “National Air Pollution Surveillance
Program.” *Environment and Climate Change Canada*.
<https://www.canada.ca/en/environment-climate-change/services/air-pollution/monitoring-networks-data/national-air-pollution-program.html>.

</div>

<div id="ref-goc2020historical">

———. 2020. “Historical Climate Data.” *Environment and Climate Change
Canada*. <https://climate.weather.gc.ca/>.

</div>

<div id="ref-chan2014july">

Chan, W. M. 2014. “Comparison of Spatial Hedonic House Price Models:
Application to Real Estate Transactions in Vancouver West.” *Simon
Fraser University*.
<http://summit.sfu.ca/system/files/iritems1/14416/FINAL\%20PROJECT\%20Wai\%20Man\%20Chan.pdf>.

</div>

<div id="ref-chay2005does">

Chay, K., and M. Greenstone. 2005. “Does Air Quality Matter? Evidence
from the Housing Market.” *Journal of Political Economy* 113 (2).

</div>

<div id="ref-cholette2006benchmarking">

Cholette, P. A., and E. B. Dagum. 2006. *Benchmarking, Temporal
Distribution, and Reconciliation Methods for Time Series*. New York:
Springer-Verlag.

</div>

<div id="ref-chow1971best">

Chow, G., and A. Lin. 1971. “Best Linear Unbiased Interpretation,
Distribution, and Extrapolation of Time Series by Related Series.” *The
Review of Economics and Statistics* 53 (4): 372–75.

</div>

<div id="ref-coulson2013what">

Coulson, N. E., and J. E. Zabel. 2013. “What Can We Learn from Hedonic
Models When Housing Markets Are Dominated by Foreclosures?” *Annual
Review of Resource Economics* 5 (1): 261–79.

</div>

<div id="ref-croissant2008panel">

Croissant, Y., and G. Millo. 2008. “Panel Data Econometrics with R: The
Plm Package.” *Journal of Statistical Software* 27 (2): 1–43.
<https://doi.org/10.18637/jss.v027.i02>.

</div>

<div id="ref-csavina2014july">

Csavina, J., J. Field, O. Felix, A. Y. Corral-Avita, A. E. Saez, and E.
A. Betterton. 2014. “Effect of Wind Speed and Relative Humidity on
Atmospheric Dust Concentrations in Semi-Arid Climates.” *Science of the
Total Environment* 487: 82–90.
<https://doi.org/10.1016/j.scitotenv.2014.03.138>.

</div>

<div id="ref-denton1971adjustment">

Denton, F. T. 1971. “Adjustment of Monthly or Quarterly Series to Annual
Totals: An Approach Based on Quadratic Minimization.” *Journal of the
American Statistical Association* 66 (333): 99–102.

</div>

<div id="ref-deryugina2016mortality">

Deryugina, T., G. Heutel, N. Miller, and J. Reif. 2016. *The Mortality
and Medical Costs of Air Pollution: Evidence from Changes in Wind
Direction*. National Bureau of Economic Research.

</div>

<div id="ref-driscoll1998consistent">

Driscoll, J., and A. Kraay. 1998. “Consistent Covariance Matrix
Estimation with Spatially Dependent Panel Data.” *The Review of
Economics and Statistics* 80 (4): 549–60.

</div>

<div id="ref-garrett2002aggregated">

Garrett, T. 2002. “Aggregated Vs Disaggregated Data in Regression
Analysis: Implications for Inference.” *Federal Reserve Bank of St.
Louis* 2002 (024).

</div>

<div id="ref-heckman1976common">

Heckman, J. 1976. “The Common Structure of Statistical Models of
Truncation, Sample Selection and Limited Dependent Variables and a
Simple Estimator for Such Models.” *Annals of Economic and Social
Measurement* 5 (4): 475–92.

</div>

<div id="ref-hla2018star">

Hlavac, Marek. 2018. “Stargazer: Well-Formatted Regression and Summary
Statistics Tables. R Package Version 5.2.2.”
<https://CRAN.R-project.org/package=stargazer>.

</div>

<div id="ref-hong2015august">

Hong, S. 2015. “Does Air Pollution Feature Lower Housing Price in
Canada?” Edited by Y. Aydede and A. Akbari. *St. Mary’s University*.
<http://library2.smu.ca/bitstream/handle/01/26426/Hong\_Sheng\_MRP\_2015.pdf?sequence=1&isAllowed=y>.

</div>

<div id="ref-teranbc2020our">

Inc, Teranet, and National Bank of Canada. 2020. “Our Methodology.”
*Teranet Inc. & National Bank of Canada*.
<https://housepriceindex.ca/about/our-methodology/>.

</div>

<div id="ref-jacobs1994dividing">

Jacobs, J. 1994. “’Dividing by 4’: A Feasible Quarterly Forecasting
Method?” University of Groningen, Department of Economics.

</div>

<div id="ref-kleiber2008aer">

Kleiber, Christian, and Achim Zeileis. 2008. *Applied Econometrics with
R*. New York: Springer-Verlag. <https://CRAN.R-project.org/package=AER>.

</div>

<div id="ref-leonard2016july">

Leonard, T., T. M. Powell-Wiley, C. Ayers, J. C. Murdoch, W. Yin, and S.
L. Pruitt. 2016. “Property Values as a Measure of Neighbourhoods: An
Application of Hedonic Price Theory.” *Epidemiology* 27 (4): 518–24.

</div>

<div id="ref-li2006studies">

Li, W., M. Prud’homme, and K. Yu. 2006. *Studies in Hedonic Resale
Housing Price Indexes*. *Canadian Economic Association 40th Annual
Meetings*. Montreal: Concordia University.

</div>

<div id="ref-litterman1983random">

Litterman, R. 1983. “A Random Walk Markov Model for the Distribution of
Time Series.” *The Review of Economics and Statistics* 1 (2): 471–78.

</div>

<div id="ref-mullerkademann2015internal">

Muller-Kademann, C. 2015. “Internal Validation of Temporal
Disaggregation: A Cloud Chamber Approach.” *Journal of Economic and
Statistics* 235 (3): 298–319.

</div>

<div id="ref-rosen1974jan">

Rosen, S. 1974. “Hedonic Prices and Implicit Markets: Product
Differentiation in Pure Competition.” *The Journal of Political Economy*
82 (1): 34–55.

</div>

<div id="ref-sax2013december">

Sax, C., and P. Steiner. 2013. “Temporal Disaggregation of Time Series.”
*The R Journal* 5 (2): 80–87.

</div>

<div id="ref-small1975air">

Small, K. 1975. “Air Pollution and Property Values: Further Comment.”
*Review of Economics and Statistics* 57: 105–7.

</div>

<div id="ref-troy2008property">

Troy, A., and J. M. Grove. 2008. “Property Values, Parks, and Crime: A
Hedonic Analysis in Baltimore, Md.” *Landscape and Urban Planning* 87
(3): 233–45.

</div>

<div id="ref-wang2015august">

Wang, J., and S. Ogawa. 2015. “Effects of Meteorological Conditions on
Pm 2.5 Concentrations in Nagasaki, Japan.” *International Journal of
Environmental Research and Public Health* 12 (8): 9089–9101.
<https://doi.org/10.3390/ijerph120809089>.

</div>

<div id="ref-wei1990disaggregation">

Wei, W. W., and D. O. Stram. 1990. “Disaggregation of Time Series
Models.” *Journal of the Royal Statistical Society* 52 (3): 453–67.

</div>

</div>
