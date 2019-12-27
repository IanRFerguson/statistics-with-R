---
title: "Peer Assessment II"
output:
  html_document:
          keep_md: true
pandoc_args: [
 "--number-sections",
]
---
Exploratory Data Analysis conducted by Ian Richard Ferguson (<a href = "mailto:irf229@nyu.edu">Email </a>)
<br>

# Background

<i>As a statistical consultant working for a real estate investment firm, your task is to develop a model to predict the selling price of a given home in Ames, Iowa. Your employer hopes to use this information to help assess whether the asking price of a house is higher or lower than the true value of the house. If the home is undervalued, it may be a good investment for the firm.</i>

# Training Data and relevant packages

<i>In order to better assess the quality of the model you will produce, the data have been randomly divided into three separate pieces: a training data set, a testing data set, and a validation data set. For now we will load the training data set, the others will be loaded and used later.</i>


```r
load("ames_train.Rdata")
```

<i>Use the code block below to load any necessary packages</i>


```r
library(statsr)
library(tidyverse)
library(BAS)
library(MASS)
library(knitr)
library(kableExtra)
library(e1071)

options(width = 200)
```

## Part 1 - Exploratory Data Analysis (EDA)
* * *

<b><u>Quantifying Neighborhood Value</u></b>
<br> <br>
What's the old adage in real estate? <i>Location, location, location.</i> The first component of the present exploratory data analysis will investigate the degree to which different neighborhoods influence real estate prices. First, we'll stratify the data by neighborhood and calculate the average prices of homes in each geographic region. This process will also offer insight into the <i>spread</i>, or range, of home prices in each neighborhood.
<br>


```r
neighborhood.data <- ames_train %>% 
                        group_by(Neighborhood) %>% 
                        summarise(homes.sold = n(),
                                  avg.price = mean(price, na.rm = T),
                                med.price = median(price, na.rm = T),
                                iqr.price = IQR(price, na.rm = T),
                                avg.area = mean(area, na.rm = T),
                                med.area = median(area, na.rm = T),
                                iqr.area = IQR(area, na.rm = T)) %>% 
                        arrange(desc(med.price))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Neighborhood </th>
   <th style="text-align:right;"> homes.sold </th>
   <th style="text-align:right;"> avg.price </th>
   <th style="text-align:right;"> med.price </th>
   <th style="text-align:right;"> iqr.price </th>
   <th style="text-align:right;"> avg.area </th>
   <th style="text-align:right;"> med.area </th>
   <th style="text-align:right;"> iqr.area </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> StoneBr </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 339316.05 </td>
   <td style="text-align:right;"> 340691.5 </td>
   <td style="text-align:right;"> 151358.00 </td>
   <td style="text-align:right;"> 1950.300 </td>
   <td style="text-align:right;"> 1870.0 </td>
   <td style="text-align:right;"> 848.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NridgHt </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 333646.74 </td>
   <td style="text-align:right;"> 336860.0 </td>
   <td style="text-align:right;"> 148800.00 </td>
   <td style="text-align:right;"> 2017.158 </td>
   <td style="text-align:right;"> 1980.0 </td>
   <td style="text-align:right;"> 759.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NoRidge </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 295844.64 </td>
   <td style="text-align:right;"> 290000.0 </td>
   <td style="text-align:right;"> 50312.50 </td>
   <td style="text-align:right;"> 2290.000 </td>
   <td style="text-align:right;"> 2282.5 </td>
   <td style="text-align:right;"> 444.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GrnHill </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 280000.00 </td>
   <td style="text-align:right;"> 280000.0 </td>
   <td style="text-align:right;"> 50000.00 </td>
   <td style="text-align:right;"> 1398.500 </td>
   <td style="text-align:right;"> 1398.5 </td>
   <td style="text-align:right;"> 103.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Timber </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 265192.16 </td>
   <td style="text-align:right;"> 232500.0 </td>
   <td style="text-align:right;"> 151200.00 </td>
   <td style="text-align:right;"> 1685.684 </td>
   <td style="text-align:right;"> 1717.0 </td>
   <td style="text-align:right;"> 578.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somerst </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 234595.88 </td>
   <td style="text-align:right;"> 221650.0 </td>
   <td style="text-align:right;"> 72684.25 </td>
   <td style="text-align:right;"> 1602.973 </td>
   <td style="text-align:right;"> 1569.0 </td>
   <td style="text-align:right;"> 412.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Greens </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 198562.50 </td>
   <td style="text-align:right;"> 212625.0 </td>
   <td style="text-align:right;"> 16437.50 </td>
   <td style="text-align:right;"> 1136.000 </td>
   <td style="text-align:right;"> 1230.5 </td>
   <td style="text-align:right;"> 133.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veenker </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 233650.00 </td>
   <td style="text-align:right;"> 205750.0 </td>
   <td style="text-align:right;"> 68125.00 </td>
   <td style="text-align:right;"> 1765.700 </td>
   <td style="text-align:right;"> 1575.5 </td>
   <td style="text-align:right;"> 528.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Crawfor </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 204196.55 </td>
   <td style="text-align:right;"> 205000.0 </td>
   <td style="text-align:right;"> 80100.00 </td>
   <td style="text-align:right;"> 1719.724 </td>
   <td style="text-align:right;"> 1646.0 </td>
   <td style="text-align:right;"> 646.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CollgCr </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 196950.86 </td>
   <td style="text-align:right;"> 195800.0 </td>
   <td style="text-align:right;"> 58836.00 </td>
   <td style="text-align:right;"> 1478.835 </td>
   <td style="text-align:right;"> 1541.0 </td>
   <td style="text-align:right;"> 590.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blmngtn </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 198634.55 </td>
   <td style="text-align:right;"> 191000.0 </td>
   <td style="text-align:right;"> 22887.50 </td>
   <td style="text-align:right;"> 1403.091 </td>
   <td style="text-align:right;"> 1455.0 </td>
   <td style="text-align:right;"> 234.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ClearCr </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 193153.85 </td>
   <td style="text-align:right;"> 185000.0 </td>
   <td style="text-align:right;"> 76000.00 </td>
   <td style="text-align:right;"> 1681.538 </td>
   <td style="text-align:right;"> 1721.0 </td>
   <td style="text-align:right;"> 863.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NWAmes </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 194093.90 </td>
   <td style="text-align:right;"> 185000.0 </td>
   <td style="text-align:right;"> 59100.00 </td>
   <td style="text-align:right;"> 1811.829 </td>
   <td style="text-align:right;"> 1740.0 </td>
   <td style="text-align:right;"> 592.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gilbert </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 193328.02 </td>
   <td style="text-align:right;"> 183500.0 </td>
   <td style="text-align:right;"> 28000.00 </td>
   <td style="text-align:right;"> 1617.837 </td>
   <td style="text-align:right;"> 1594.0 </td>
   <td style="text-align:right;"> 272.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SawyerW </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 183101.00 </td>
   <td style="text-align:right;"> 182500.0 </td>
   <td style="text-align:right;"> 67000.00 </td>
   <td style="text-align:right;"> 1662.870 </td>
   <td style="text-align:right;"> 1651.5 </td>
   <td style="text-align:right;"> 634.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mitchel </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 165013.64 </td>
   <td style="text-align:right;"> 156500.0 </td>
   <td style="text-align:right;"> 32875.00 </td>
   <td style="text-align:right;"> 1279.477 </td>
   <td style="text-align:right;"> 1213.0 </td>
   <td style="text-align:right;"> 447.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NPkVill </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 139275.00 </td>
   <td style="text-align:right;"> 142100.0 </td>
   <td style="text-align:right;"> 13025.00 </td>
   <td style="text-align:right;"> 1259.000 </td>
   <td style="text-align:right;"> 1262.5 </td>
   <td style="text-align:right;"> 390.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NAmes </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:right;"> 141355.97 </td>
   <td style="text-align:right;"> 139900.0 </td>
   <td style="text-align:right;"> 27050.00 </td>
   <td style="text-align:right;"> 1228.123 </td>
   <td style="text-align:right;"> 1144.0 </td>
   <td style="text-align:right;"> 404.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sawyer </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 139312.70 </td>
   <td style="text-align:right;"> 136000.0 </td>
   <td style="text-align:right;"> 20350.00 </td>
   <td style="text-align:right;"> 1188.295 </td>
   <td style="text-align:right;"> 1073.0 </td>
   <td style="text-align:right;"> 463.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SWISU </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 130619.50 </td>
   <td style="text-align:right;"> 134000.0 </td>
   <td style="text-align:right;"> 29250.00 </td>
   <td style="text-align:right;"> 1521.333 </td>
   <td style="text-align:right;"> 1515.0 </td>
   <td style="text-align:right;"> 256.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Edwards </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 136322.02 </td>
   <td style="text-align:right;"> 127250.0 </td>
   <td style="text-align:right;"> 35750.00 </td>
   <td style="text-align:right;"> 1317.100 </td>
   <td style="text-align:right;"> 1152.0 </td>
   <td style="text-align:right;"> 417.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BrkSide </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 122472.56 </td>
   <td style="text-align:right;"> 124000.0 </td>
   <td style="text-align:right;"> 42000.00 </td>
   <td style="text-align:right;"> 1169.927 </td>
   <td style="text-align:right;"> 1196.0 </td>
   <td style="text-align:right;"> 492.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blueste </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 125800.00 </td>
   <td style="text-align:right;"> 123900.0 </td>
   <td style="text-align:right;"> 10250.00 </td>
   <td style="text-align:right;"> 1102.000 </td>
   <td style="text-align:right;"> 1045.0 </td>
   <td style="text-align:right;"> 98.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OldTown </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 120225.61 </td>
   <td style="text-align:right;"> 120000.0 </td>
   <td style="text-align:right;"> 29550.00 </td>
   <td style="text-align:right;"> 1404.507 </td>
   <td style="text-align:right;"> 1328.0 </td>
   <td style="text-align:right;"> 779.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IDOTRR </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 97620.69 </td>
   <td style="text-align:right;"> 99500.0 </td>
   <td style="text-align:right;"> 48900.00 </td>
   <td style="text-align:right;"> 1141.086 </td>
   <td style="text-align:right;"> 1128.0 </td>
   <td style="text-align:right;"> 568.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BrDale </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 98930.00 </td>
   <td style="text-align:right;"> 98750.0 </td>
   <td style="text-align:right;"> 16725.00 </td>
   <td style="text-align:right;"> 1073.400 </td>
   <td style="text-align:right;"> 987.0 </td>
   <td style="text-align:right;"> 105.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MeadowV </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 92946.88 </td>
   <td style="text-align:right;"> 85750.0 </td>
   <td style="text-align:right;"> 20150.00 </td>
   <td style="text-align:right;"> 1051.375 </td>
   <td style="text-align:right;"> 1092.0 </td>
   <td style="text-align:right;"> 222.50 </td>
  </tr>
</tbody>
</table>
<br>
![](Final_peer_files/figure-html/plotting neighborhoods-1.png)<!-- -->

<br> <br>
We observe enormous variance in both the average home price and the range of home prices as a result of the neighborhood they were built in. This finding suggests that the `Neighborhood` variable may be useful in eventually predicting home prices - which we'll explore further in the next section of this EDA.
<br> <br>
To follow-up on these preliminary results, we'll explore the age of homes in each neighborhood. It's possible that a home's relative age is a moderating factor in the results we observed above (for example, if a given neighborhood consists mostly of new homes <i>and</i> averages higher-priced homes). First, let's spot check the population distribution of home age using the `Year.Built` variable.
<br> <br>
![](Final_peer_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
<br>
This distribution shows a clear trend, such that a surge of new homes were built following World War II (between 1945 and 1955). A second wave of new homes were built in the late 1980's - perhaps at the time that Baby Boomers' children were coming of age and starting families of their own. Next, we'll stratify the data by neighborhood to test our hypothesis that the average age of homes moderates home prices by neighborhood.
<br>

```r
home.age <- ames_train %>% 
        group_by(Neighborhood) %>% 
        summarise(homes.sold = n(),
                  oldest = min(Year.Built),
                  median.age = round(median(Year.Built)),
                  newest = max(Year.Built)) %>% 
        arrange(desc(median.age))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Neighborhood </th>
   <th style="text-align:right;"> homes.sold </th>
   <th style="text-align:right;"> oldest </th>
   <th style="text-align:right;"> median.age </th>
   <th style="text-align:right;"> newest </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Somerst </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 1999 </td>
   <td style="text-align:right;"> 2006 </td>
   <td style="text-align:right;"> 2009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blmngtn </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:right;"> 2007 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NridgHt </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:right;"> 2009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> StoneBr </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 1984 </td>
   <td style="text-align:right;"> 2005 </td>
   <td style="text-align:right;"> 2010 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Timber </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 1958 </td>
   <td style="text-align:right;"> 2002 </td>
   <td style="text-align:right;"> 2008 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CollgCr </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 1972 </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:right;"> 2009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gilbert </td>
   <td style="text-align:right;"> 49 </td>
   <td style="text-align:right;"> 1950 </td>
   <td style="text-align:right;"> 2000 </td>
   <td style="text-align:right;"> 2007 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NoRidge </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 1990 </td>
   <td style="text-align:right;"> 1994 </td>
   <td style="text-align:right;"> 2000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SawyerW </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 1880 </td>
   <td style="text-align:right;"> 1993 </td>
   <td style="text-align:right;"> 2008 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GrnHill </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1986 </td>
   <td style="text-align:right;"> 1992 </td>
   <td style="text-align:right;"> 1998 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blueste </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:right;"> 1980 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Greens </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1978 </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:right;"> 1981 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mitchel </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 1940 </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:right;"> 2007 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Veenker </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 1974 </td>
   <td style="text-align:right;"> 1978 </td>
   <td style="text-align:right;"> 1996 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NWAmes </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 1967 </td>
   <td style="text-align:right;"> 1977 </td>
   <td style="text-align:right;"> 2001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NPkVill </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1975 </td>
   <td style="text-align:right;"> 1975 </td>
   <td style="text-align:right;"> 1977 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BrDale </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 1971 </td>
   <td style="text-align:right;"> 1972 </td>
   <td style="text-align:right;"> 1973 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MeadowV </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 1970 </td>
   <td style="text-align:right;"> 1971 </td>
   <td style="text-align:right;"> 1977 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ClearCr </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 1908 </td>
   <td style="text-align:right;"> 1966 </td>
   <td style="text-align:right;"> 1992 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sawyer </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 1918 </td>
   <td style="text-align:right;"> 1966 </td>
   <td style="text-align:right;"> 1982 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NAmes </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:right;"> 1920 </td>
   <td style="text-align:right;"> 1960 </td>
   <td style="text-align:right;"> 2002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Edwards </td>
   <td style="text-align:right;"> 60 </td>
   <td style="text-align:right;"> 1914 </td>
   <td style="text-align:right;"> 1956 </td>
   <td style="text-align:right;"> 2009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Crawfor </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 1921 </td>
   <td style="text-align:right;"> 1941 </td>
   <td style="text-align:right;"> 2008 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SWISU </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 1915 </td>
   <td style="text-align:right;"> 1929 </td>
   <td style="text-align:right;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BrkSide </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 1900 </td>
   <td style="text-align:right;"> 1928 </td>
   <td style="text-align:right;"> 1958 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IDOTRR </td>
   <td style="text-align:right;"> 35 </td>
   <td style="text-align:right;"> 1900 </td>
   <td style="text-align:right;"> 1926 </td>
   <td style="text-align:right;"> 1967 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OldTown </td>
   <td style="text-align:right;"> 71 </td>
   <td style="text-align:right;"> 1872 </td>
   <td style="text-align:right;"> 1920 </td>
   <td style="text-align:right;"> 1962 </td>
  </tr>
</tbody>
</table>

![](Final_peer_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
<br> <br>
Certainly, this plot confirms that not all neighborhoods in the present sample consist of modern homes. Old Town lives up to its name, as it holds the oldest median age of any home in the sample. It stands to reason, therefore, that `Neighborhood` and `Year.Built` may covary. Lastly, we'll spot check the top- and bottom-three neighborhoods for median prices, which may gleam insight into this interaction.
<br>

```r
spot.check <- ames_train %>% 
                group_by(Neighborhood) %>% 
                filter(Neighborhood %in% c("OldTown","IDOTRR","BrkSide","Somerst","Blmngtn","NridgHt")) %>% 
                summarise(median.price = median(price, na.rm = T)) %>% 
                arrange(desc(median.price))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Neighborhood </th>
   <th style="text-align:right;"> median.price </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> NridgHt </td>
   <td style="text-align:right;"> 336860 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Somerst </td>
   <td style="text-align:right;"> 221650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blmngtn </td>
   <td style="text-align:right;"> 191000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BrkSide </td>
   <td style="text-align:right;"> 124000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OldTown </td>
   <td style="text-align:right;"> 120000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IDOTRR </td>
   <td style="text-align:right;"> 99500 </td>
  </tr>
</tbody>
</table>


<br>
While this relationship doesn't appear to be perfectly linear, it does seem to be at least moderately robust. We'll keep this in mind as we move forward with the analysis.
<br> <br> <br>

<b><u>Added Value Via Renovation</u></b>
<br> <br>
It stands to reason that homes that have been renovated may command a higher market value - fewer structural problems, updated appliances and amenities, etc. To explore whether or not home renovation plays a role in mediating home values, we'll create a new variable called `reno` that will identify homes with a remodel year matching the build year.
<br>


```r
ames_train <- ames_train %>% 
        mutate(reno = ifelse(Year.Built != Year.Remod.Add, "Renovated", "Not Renovated"))

ames_train$reno = as.factor(ames_train$reno)

reno.view <- ames_train %>%
                group_by(reno) %>% 
                summarise(homes.sold = n(),
                          median.price = median(price, na.rm = T),
                          iqr.price = IQR(price, na.rm = T))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> reno </th>
   <th style="text-align:right;"> homes.sold </th>
   <th style="text-align:right;"> median.price </th>
   <th style="text-align:right;"> iqr.price </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Not Renovated </td>
   <td style="text-align:right;"> 546 </td>
   <td style="text-align:right;"> 166000 </td>
   <td style="text-align:right;"> 79000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Renovated </td>
   <td style="text-align:right;"> 454 </td>
   <td style="text-align:right;"> 151250 </td>
   <td style="text-align:right;"> 86500 </td>
  </tr>
</tbody>
</table>

![](Final_peer_files/figure-html/plot renovations-1.png)<!-- -->
<br>
It appears as though we may be able to rule out home renovation as a critical factor in the present data set. Perhaps Ames, Iowa is inhabited by DIY-ers, who would rather conduct renovations on their own. At any rate, the median values for renovated and non-renovated homes is nearly identical, which suggests that this factor may not hold very much predictive power.
<br>
<br>


<b><u>Prime Selling Season</u></b>
<br> <br>
Lastly, we'll seek to identify whether certain times of year are more popular for home sales than others. We'll first group the data set by `Mo.Sold` before calculating:
<br> <br>
(1) the number of homes sold that month and <br>
(2) the median sale price for homes that month
<br>


```r
ames_train$Mo.Sold <- as.factor(ames_train$Mo.Sold)

ames_train$Mo.Sold <- recode(.x = ames_train$Mo.Sold, '1' = "January", '2' = "February",
                             '3' = "March", '4' = "April", '5' = "May", '6' = "June",
                             '7' = "July", '8' = "August", '9' = "September", '10' = "October",
                             '11' = "November", '12' = "December")

monthly <- ames_train %>% 
        group_by(Mo.Sold) %>% 
        summarise(number.sold = n(),
                  median.price = median(price, na.rm = T),
                  iqr.price = IQR(price, na.rm = T)) %>% 
        arrange(desc(number.sold))
```


<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Mo.Sold </th>
   <th style="text-align:right;"> number.sold </th>
   <th style="text-align:right;"> median.price </th>
   <th style="text-align:right;"> iqr.price </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> June </td>
   <td style="text-align:right;"> 180 </td>
   <td style="text-align:right;"> 160000 </td>
   <td style="text-align:right;"> 81999.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> July </td>
   <td style="text-align:right;"> 141 </td>
   <td style="text-align:right;"> 159000 </td>
   <td style="text-align:right;"> 66000.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> May </td>
   <td style="text-align:right;"> 132 </td>
   <td style="text-align:right;"> 152000 </td>
   <td style="text-align:right;"> 64562.50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> August </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 175000 </td>
   <td style="text-align:right;"> 107775.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> April </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 149900 </td>
   <td style="text-align:right;"> 77000.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> March </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 150000 </td>
   <td style="text-align:right;"> 64000.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> October </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 178000 </td>
   <td style="text-align:right;"> 83000.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> September </td>
   <td style="text-align:right;"> 56 </td>
   <td style="text-align:right;"> 181000 </td>
   <td style="text-align:right;"> 125125.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> November </td>
   <td style="text-align:right;"> 50 </td>
   <td style="text-align:right;"> 165575 </td>
   <td style="text-align:right;"> 93025.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> February </td>
   <td style="text-align:right;"> 46 </td>
   <td style="text-align:right;"> 157250 </td>
   <td style="text-align:right;"> 68475.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> January </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 178000 </td>
   <td style="text-align:right;"> 106500.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> December </td>
   <td style="text-align:right;"> 39 </td>
   <td style="text-align:right;"> 167000 </td>
   <td style="text-align:right;"> 103875.00 </td>
  </tr>
</tbody>
</table>

![](Final_peer_files/figure-html/unnamed-chunk-8-1.png)<!-- -->![](Final_peer_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

<br>
We observe that summer months appear to be a significantly more popular time of year to buy homes. However, it does not appear that homes sold during this time command a higher market price. In real estate vernacular, it seems that the summer may be considered a <b>buyer's market</b>, while the winter months may be more advantagous to the seller.
<br>

* * *

## Part 2 - Development and assessment of an initial model, following a semi-guided process of analysis

### Section 2.1 An Initial Model
<i>In building a model, it is often useful to start by creating a simple, intuitive initial model based on the results of the exploratory data analysis. (Note: The goal at this stage is **not** to identify the "best" possible model but rather to choose a reasonable and understandable starting point. Later you will expand and revise this model to create your final model.

Based on your EDA, select *at most* 10 predictor variables from “ames_train” and create a linear model for `price` (or a transformed version of price) using those variables. Provide the *R code* and the *summary output table* for your model, a *brief justification* for the variables you have chosen, and a *brief discussion* of the model results in context (focused on the variables that appear to be important predictors and how they relate to sales price).</i>

* * *

The principal goal of this analysis is to construct a linear model to predict home prices in Ames, Iowa. Since we'll be regressing on `price`, let's start by assessing the normality of price distribution across all homes.
<br>
![](Final_peer_files/figure-html/unnamed-chunk-9-1.png)<!-- --><table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> mean.price </th>
   <th style="text-align:right;"> median.price </th>
   <th style="text-align:right;"> skewness </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 181190.1 </td>
   <td style="text-align:right;"> 159467 </td>
   <td style="text-align:right;"> 1.626277 </td>
  </tr>
</tbody>
</table>

<br>
We observe a moderately right-skewed distribution for price values, such that the median price (159467) falls well below the mean (181190). An elegant solution to this - and one that will lend us more predictive power in the model - is to `log-transform` the `price` variable, which will approximate normality in the distribution, as well as making it less sensitive to outliers.
<br> <br>
![](Final_peer_files/figure-html/unnamed-chunk-10-1.png)<!-- --><table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> mean.price </th>
   <th style="text-align:right;"> median.price </th>
   <th style="text-align:right;"> skewness </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 12.01847 </td>
   <td style="text-align:right;"> 11.97959 </td>
   <td style="text-align:right;"> -0.1024074 </td>
  </tr>
</tbody>
</table>

<br>
This transformation successfully normalizes the data, as the median and mean values have converged. The log-transformed price values appear to be normally distributed, which will make `log(price)` a much more reliable dependent variable in our linear model.
<br> <br>
For our initial model, we'll pick <b>10 variables</b> that we suspect may play a role in predicting sale prices. Some of these variables - `Neighborhood`, `Year.Built`, and `reno` - have been elaborated on in the previous EDA. Others are more intuitive, such as `Overall.Qual`.
<br>


```r
basic.model <- lm(log(price) ~ area + Neighborhood + Lot.Config + Overall.Qual + Overall.Cond + Year.Built + reno + TotRms.AbvGrd + Full.Bath + Half.Bath, 
                  data = ames_train)

summary(basic.model)
```

```
## 
## Call:
## lm(formula = log(price) ~ area + Neighborhood + Lot.Config + 
##     Overall.Qual + Overall.Cond + Year.Built + reno + TotRms.AbvGrd + 
##     Full.Bath + Half.Bath, data = ames_train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.43568 -0.07136  0.00297  0.08379  0.48508 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          1.931e+00  8.551e-01   2.259 0.024129 *  
## area                 3.876e-04  2.419e-05  16.022  < 2e-16 ***
## NeighborhoodBlueste -2.375e-01  1.051e-01  -2.260 0.024034 *  
## NeighborhoodBrDale  -3.033e-01  7.285e-02  -4.164 3.41e-05 ***
## NeighborhoodBrkSide -4.631e-02  6.177e-02  -0.750 0.453576    
## NeighborhoodClearCr  1.372e-01  6.857e-02   2.001 0.045671 *  
## NeighborhoodCollgCr  1.160e-02  5.163e-02   0.225 0.822347    
## NeighborhoodCrawfor  1.373e-01  6.124e-02   2.242 0.025196 *  
## NeighborhoodEdwards -7.506e-02  5.568e-02  -1.348 0.177909    
## NeighborhoodGilbert  1.310e-02  5.441e-02   0.241 0.809829    
## NeighborhoodGreens   1.061e-02  9.619e-02   0.110 0.912216    
## NeighborhoodGrnHill  4.020e-01  1.229e-01   3.270 0.001112 ** 
## NeighborhoodIDOTRR  -1.664e-01  6.299e-02  -2.642 0.008388 ** 
## NeighborhoodMeadowV -2.875e-01  6.593e-02  -4.362 1.43e-05 ***
## NeighborhoodMitchel  3.921e-02  5.562e-02   0.705 0.480963    
## NeighborhoodNAmes    5.224e-03  5.413e-02   0.097 0.923132    
## NeighborhoodNoRidge  7.579e-02  5.850e-02   1.296 0.195395    
## NeighborhoodNPkVill -6.178e-02  9.544e-02  -0.647 0.517619    
## NeighborhoodNridgHt  1.726e-01  5.336e-02   3.236 0.001255 ** 
## NeighborhoodNWAmes  -3.019e-02  5.634e-02  -0.536 0.592170    
## NeighborhoodOldTown -9.063e-02  6.157e-02  -1.472 0.141370    
## NeighborhoodSawyer  -3.926e-03  5.600e-02  -0.070 0.944119    
## NeighborhoodSawyerW -5.382e-02  5.471e-02  -0.984 0.325517    
## NeighborhoodSomerst  6.257e-02  5.192e-02   1.205 0.228446    
## NeighborhoodStoneBr  1.880e-01  6.089e-02   3.088 0.002070 ** 
## NeighborhoodSWISU   -4.359e-02  7.283e-02  -0.599 0.549622    
## NeighborhoodTimber   1.377e-01  6.053e-02   2.275 0.023110 *  
## NeighborhoodVeenker  6.405e-02  7.239e-02   0.885 0.376528    
## Lot.ConfigCulDSac    3.771e-02  2.299e-02   1.640 0.101325    
## Lot.ConfigFR2       -3.645e-02  3.024e-02  -1.205 0.228445    
## Lot.ConfigFR3       -1.019e-01  7.336e-02  -1.388 0.165334    
## Lot.ConfigInside    -1.458e-02  1.381e-02  -1.056 0.291333    
## Overall.Qual         9.466e-02  6.575e-03  14.398  < 2e-16 ***
## Overall.Cond         6.378e-02  5.287e-03  12.065  < 2e-16 ***
## Year.Built           4.430e-03  4.292e-04  10.322  < 2e-16 ***
## renoRenovated       -8.200e-03  1.207e-02  -0.680 0.496918    
## TotRms.AbvGrd       -5.415e-03  5.826e-03  -0.929 0.352909    
## Full.Bath           -5.627e-02  1.566e-02  -3.593 0.000343 ***
## Half.Bath           -5.636e-02  1.317e-02  -4.280 2.06e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1591 on 961 degrees of freedom
## Multiple R-squared:  0.8624,	Adjusted R-squared:  0.857 
## F-statistic: 158.5 on 38 and 961 DF,  p-value: < 2.2e-16
```
<br>
As far as naive approaches go, this is an excellent baseline model. We observe an adjusted `R^2` value of <b>0.857</b> in this model, which suggests that nearly 86% of the variance in home prices can be explained by the variables chosen in this model. 
<br> <br>
While we observe a number of highly significant predictor variables (area, overall quality, and overall condition to name a few), there are several variables that do not appear to be significantly predictive. To account for those - and potentially increase the predictive power of our model - we will move on to the model selection stage.
<br>

* * *

### Section 2.2 Model Selection

* * *

Our next step will be to trim down variables that limit the predictive efficacy of our model. We'll contrast two different approaches, starting with 
<br> <br>
<b>(1) BIC backward-selection</b>, followed by <br>
<b>(2) AIC stepwise regression</b>
<br> <br>

* * *

#### Model One: BIC Backward-Selection

* * *

Our first attempt at model selection will utilize BIC backward-selection. Starting with the full array of regressors, we'll remove regressors at each step until the BIC for the model is at its maximum. This method utilizes `k = log(n)` to penalize free parameters; thus, we'll start by calculating `k` before including it in the model.
<br>


```r
k <- log(nrow(ames_train))

basic.bic <- stepAIC(object = basic.model, 
                     k = k, 
                     direction = "backward",
                     trace = F)
```



```r
summary(basic.bic)
```

```
## 
## Call:
## lm(formula = log(price) ~ area + Neighborhood + Overall.Qual + 
##     Overall.Cond + Year.Built + Full.Bath + Half.Bath, data = ames_train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.44074 -0.07099  0.00197  0.08951  0.46522 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          1.709e+00  8.342e-01   2.048 0.040799 *  
## area                 3.755e-04  1.858e-05  20.213  < 2e-16 ***
## NeighborhoodBlueste -2.169e-01  1.051e-01  -2.063 0.039334 *  
## NeighborhoodBrDale  -2.990e-01  7.272e-02  -4.112 4.26e-05 ***
## NeighborhoodBrkSide -4.625e-02  6.188e-02  -0.748 0.454942    
## NeighborhoodClearCr  1.601e-01  6.824e-02   2.345 0.019206 *  
## NeighborhoodCollgCr  2.067e-02  5.161e-02   0.400 0.688901    
## NeighborhoodCrawfor  1.435e-01  6.127e-02   2.342 0.019387 *  
## NeighborhoodEdwards -6.593e-02  5.570e-02  -1.184 0.236838    
## NeighborhoodGilbert  1.680e-02  5.438e-02   0.309 0.757453    
## NeighborhoodGreens   5.912e-02  9.478e-02   0.624 0.532951    
## NeighborhoodGrnHill  4.106e-01  1.230e-01   3.339 0.000873 ***
## NeighborhoodIDOTRR  -1.606e-01  6.308e-02  -2.546 0.011039 *  
## NeighborhoodMeadowV -2.805e-01  6.588e-02  -4.258 2.27e-05 ***
## NeighborhoodMitchel  4.587e-02  5.549e-02   0.827 0.408706    
## NeighborhoodNAmes    1.121e-02  5.405e-02   0.207 0.835674    
## NeighborhoodNoRidge  8.322e-02  5.843e-02   1.424 0.154672    
## NeighborhoodNPkVill -6.201e-02  9.436e-02  -0.657 0.511215    
## NeighborhoodNridgHt  1.705e-01  5.336e-02   3.194 0.001447 ** 
## NeighborhoodNWAmes  -2.296e-02  5.612e-02  -0.409 0.682552    
## NeighborhoodOldTown -8.422e-02  6.165e-02  -1.366 0.172252    
## NeighborhoodSawyer   8.685e-03  5.579e-02   0.156 0.876317    
## NeighborhoodSawyerW -4.799e-02  5.470e-02  -0.877 0.380499    
## NeighborhoodSomerst  6.839e-02  5.191e-02   1.318 0.187972    
## NeighborhoodStoneBr  2.048e-01  6.066e-02   3.376 0.000765 ***
## NeighborhoodSWISU   -3.832e-02  7.294e-02  -0.525 0.599469    
## NeighborhoodTimber   1.443e-01  6.064e-02   2.380 0.017518 *  
## NeighborhoodVeenker  8.076e-02  7.132e-02   1.132 0.257754    
## Overall.Qual         9.542e-02  6.567e-03  14.529  < 2e-16 ***
## Overall.Cond         6.339e-02  5.245e-03  12.086  < 2e-16 ***
## Year.Built           4.525e-03  4.187e-04  10.807  < 2e-16 ***
## Full.Bath           -5.925e-02  1.559e-02  -3.801 0.000153 ***
## Half.Bath           -5.624e-02  1.316e-02  -4.272 2.13e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1595 on 967 degrees of freedom
## Multiple R-squared:  0.8608,	Adjusted R-squared:  0.8562 
## F-statistic: 186.9 on 32 and 967 DF,  p-value: < 2.2e-16
```

In this first instance, we see that three non-significant variables were removed from the full model. Unfortunately, this backward-selection does not seem to have improved our adjusted `R^2` value <i>or</i> our residual standard error. This suggests that the present model does not hold any more predcitive power than the model in full.
<br> <br>

#### Model Two: AIC Stepwise-Regression

* * * 

Our second approach will utilize AIC stepwise regression. In this method, we'll consider <i>adding</i> or <i>subtracting</i> regressors at each step - this will allow us to consider all variables at each step, which gives us confidence that the most robust model will be selected. Unlike BIC regression, our AIC approach will utilize `k = 2` to penalize free parameters; therefore, no need to calculate `k` prior to selecting our regressors.
<br>


```r
basic.aic <- stepAIC(basic.model, 
                     k = 2,
                     direction = "both",
                     trace = F)
```


```r
summary(basic.aic)
```

```
## 
## Call:
## lm(formula = log(price) ~ area + Neighborhood + Lot.Config + 
##     Overall.Qual + Overall.Cond + Year.Built + Full.Bath + Half.Bath, 
##     data = ames_train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.43919 -0.07162  0.00355  0.08644  0.46712 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          1.740e+00  8.338e-01   2.087 0.037124 *  
## area                 3.724e-04  1.857e-05  20.049  < 2e-16 ***
## NeighborhoodBlueste -2.340e-01  1.050e-01  -2.229 0.026048 *  
## NeighborhoodBrDale  -2.995e-01  7.250e-02  -4.131 3.93e-05 ***
## NeighborhoodBrkSide -4.608e-02  6.173e-02  -0.746 0.455564    
## NeighborhoodClearCr  1.408e-01  6.842e-02   2.058 0.039874 *  
## NeighborhoodCollgCr  1.275e-02  5.158e-02   0.247 0.804784    
## NeighborhoodCrawfor  1.398e-01  6.116e-02   2.286 0.022487 *  
## NeighborhoodEdwards -7.155e-02  5.557e-02  -1.288 0.198203    
## NeighborhoodGilbert  1.204e-02  5.436e-02   0.222 0.824749    
## NeighborhoodGreens   2.133e-02  9.568e-02   0.223 0.823619    
## NeighborhoodGrnHill  4.112e-01  1.226e-01   3.355 0.000826 ***
## NeighborhoodIDOTRR  -1.646e-01  6.292e-02  -2.617 0.009016 ** 
## NeighborhoodMeadowV -2.817e-01  6.569e-02  -4.289 1.98e-05 ***
## NeighborhoodMitchel  4.056e-02  5.556e-02   0.730 0.465523    
## NeighborhoodNAmes    7.355e-03  5.396e-02   0.136 0.891609    
## NeighborhoodNoRidge  7.839e-02  5.843e-02   1.342 0.180045    
## NeighborhoodNPkVill -5.286e-02  9.505e-02  -0.556 0.578260    
## NeighborhoodNridgHt  1.722e-01  5.334e-02   3.229 0.001286 ** 
## NeighborhoodNWAmes  -2.792e-02  5.607e-02  -0.498 0.618591    
## NeighborhoodOldTown -8.927e-02  6.154e-02  -1.451 0.147210    
## NeighborhoodSawyer  -2.624e-03  5.584e-02  -0.047 0.962529    
## NeighborhoodSawyerW -5.316e-02  5.466e-02  -0.973 0.330957    
## NeighborhoodSomerst  6.457e-02  5.186e-02   1.245 0.213414    
## NeighborhoodStoneBr  1.898e-01  6.077e-02   3.123 0.001845 ** 
## NeighborhoodSWISU   -4.201e-02  7.275e-02  -0.577 0.563754    
## NeighborhoodTimber   1.390e-01  6.049e-02   2.298 0.021798 *  
## NeighborhoodVeenker  7.176e-02  7.205e-02   0.996 0.319503    
## Lot.ConfigCulDSac    3.701e-02  2.297e-02   1.611 0.107407    
## Lot.ConfigFR2       -3.609e-02  3.021e-02  -1.195 0.232510    
## Lot.ConfigFR3       -1.026e-01  7.317e-02  -1.402 0.161275    
## Lot.ConfigInside    -1.533e-02  1.379e-02  -1.111 0.266713    
## Overall.Qual         9.489e-02  6.559e-03  14.467  < 2e-16 ***
## Overall.Cond         6.322e-02  5.231e-03  12.085  < 2e-16 ***
## Year.Built           4.520e-03  4.189e-04  10.789  < 2e-16 ***
## Full.Bath           -5.728e-02  1.562e-02  -3.667 0.000259 ***
## Half.Bath           -5.592e-02  1.315e-02  -4.252 2.33e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.159 on 963 degrees of freedom
## Multiple R-squared:  0.8622,	Adjusted R-squared:  0.8571 
## F-statistic: 167.4 on 36 and 963 DF,  p-value: < 2.2e-16
```

The remaining coefficients in this latter model are nearly identical to those retained in the BIC model, except for its inclusion of `Lot.Config` as a predictive variable. We see a slight improvement in adjusted `R^2` <i>and</i> residual standard error relative to the full model. Given our three options - the full model, BIC model, and AIC model - the latter approach grants us the most robust predictive power.
<br> <br>
<mark>We'll proceed by using our <b>AIC Stepwise Regression model</b>.

* * *

### Section 2.3 Initial Model Residuals

* * * 

Next, we'll plot the residuals for the selected model. This will give us some indication of its predictive accuracy; if we observe a non-normative distribution of residual values, it may suggest that the model does a poor job predicting price values.
<br> <br>

* * *

![](Final_peer_files/figure-html/model_resid-1.png)<!-- -->![](Final_peer_files/figure-html/model_resid-2.png)<!-- -->

<br>
As we can see, the residuals in this plot:
<br>
<br>
<b>(1) Appear relatively symmetrical and are centered near the middle of the plot<br>
(2) Are clustered near 0 on the y-axis, and<br>
(3) Do not display a clear pattern or shape </b><br>
<br>
<br>
Between the robust `R^2` value and the normal distribution of residual values, we can move forward with confidence in this model's predictive efficacy.
<br>

* * *

### Section 2.4 Initial Model RMSE

* * *

Next, we'll check our chosen model's RMSE. This will give us a basic estimate of how our model will perform when applied to data outside of the training set. Since we're regressing on `log(price)`, we'll first need to exponentiate the data. Our RMSE will be interpreted in dollar value, given the context of the present analysis.
<br> <br>

* * *


```r
basic.predictions <- exp(predict(basic.aic, ames_train))

basic.resids <- ames_train$price - basic.predictions

basic.rmse <- sqrt(mean(basic.resids^2))

paste("Basic Model RMSE: $", round(basic.rmse,2), sep = "")
```

```
## [1] "Basic Model RMSE: $34384.39"
```

The RMSE for the present model translates to $34,384.39. This value may be interpreted as the standard deviation of the unexplained variance (or prediction errors) in the present model. Considering the range of home prices in the current data set - 12,789 dollars to 615,000 dollars - this RMSE value is <b><i>acceptable</i></b>, as it is relatively low in the context of the grand data set. 

* * *

### Section 2.5 Overfitting 

<i>The process of building a model generally involves starting with an initial model (as you have done above), identifying its shortcomings, and adapting the model accordingly. This process may be repeated several times until the model fits the data reasonably well. However, the model may do well on training data but perform poorly out-of-sample (meaning, on a dataset other than the original training data) because the model is overly-tuned to specifically fit the training data. This is called “overfitting.” To determine whether overfitting is occurring on a model, compare the performance of a model on both in-sample and out-of-sample data sets. To look at performance of your initial model on out-of-sample data, you will use the data set `ames_test`.</i>


```r
load("ames_test.Rdata")
```

<i>Use your model from above to generate predictions for the housing prices in the test data set.  Are the predictions significantly more accurate (compared to the actual sales prices) for the training data than the test data?  Why or why not? Briefly explain how you determined that (what steps or processes did you use)?</i>

* * *

Crucially, our selected model has been constructed to match a very specific geographic area. We <b><u>cannot</u></b> use this model to predict house prices for neighborhoods outside of those included in the training data set. While other approaches to solving this problem are viable - calculating effect size of `Neighborhood`, for example - we will simply omit those data points for the sake of parsimony.
<br> <br>
Since the `basic.aic` model does not include the Landmark neighborhood as a factor level, we must first clean the set so the data and model are homogenous. We'll then use the model to predict `log(price)` values in the test data set, before adding the predicted values to the original dataframe for comparison.


```r
ames_test <- ames_test %>% 
        filter(Neighborhood != "Landmrk")

test.values <- exp(predict(basic.aic, ames_test))

ames_test['predicted'] = test.values
```

![](Final_peer_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

<br>
Pretty as a picture. Applying the selected model to the test data set provides evidence for its effectiveness. We see a tight cluster of data around the line of best fit, with very few data points straying too far from the line - this can be interpreted as a success of the model selection process.
<br>

* * *

## Part 3 Development of a Final Model

<i>Now that you have developed an initial model to use as a baseline, create a final model with *at most* 20 variables to predict housing prices in Ames, IA, selecting from the full array of variables in the dataset and using any of the tools that we introduced in this specialization.  

Carefully document the process that you used to come up with your final model, so that you can answer the questions below.</i>

* * *

### Building the Model

* * * 

The present data set consists of 82 variables, some of them more valuable than others. To begin building our final model, we'll need to first identify and remove variables that will not significantly contribute to predicting home prices.
<br>


```r
# Identify variables with lots of missing data
which(sapply(ames_train, function(x) (sum(is.na(x)) >= 50)))
```

```
## Lot.Frontage        Alley Fireplace.Qu      Pool.QC        Fence Misc.Feature 
##            6            9           59           74           75           76
```

```r
# Identify factors with < 2 unique levels ... add these to running list
which(sapply(ames_train, function(x) (is.factor(x)) & length(unique(x))<2)) 
```

```
## Utilities 
##        12
```

```r
# Assign poorly represented variables to string vector + create new dataset without these variables included
bad.variables <- c("Lot.Frontage", "Alley", "Fireplace.Qu", "Pool.QC", "Fence", "Misc.Feature", "Utilities")

clean.data <- ames_train[, !(names(ames_train) %in% bad.variables)]

# Remove incomplete cases from new data set
clean.data <- na.omit(clean.data)
```

<br>
This process leaves us with only 767 observations from the original dataset, but by trimming out poorly represented variables and incomplete cases we may afford ourselves a slight boost in predictive power moving forward.
<br> <br>



```r
# Base model including all variables (besides instance identifier, which is meaningless at a predictive level)
final.model <- lm(log(price) ~ . -PID, 
                  data = clean.data)

# Replace skewed variables with natural log-transformed versions
final.model <- update(final.model, . ~ . -area -Lot.Area -X1st.Flr.SF
                      +log(area) +log(Lot.Area) +log(X1st.Flr.SF))

# Remove extraneous variables via stepwise regression
final.step <- stepAIC(final.model,
                      direction = "both", 
                      k = 2,
                      trace = F)

initial.call <- final.step$call

# Print adjusted R^2 value for reduced model
initial.r2 <- summary(final.step)$adj.r.squared

paste(round(initial.r2 * 100, 2), "% Explained Variance", sep = "")
```

```
## [1] "93.42% Explained Variance"
```

<br> <br>
This intial model is extremely robust, and broadly speaking this would be more than sufficient in a real-world context. However, given the maximum number of coefficients allowed in this assignment (20), we will work to cut down on extraneous coefficients while maximizing the model's adjusted `R^2` value. Calling the `summary` function on the present model, we'll identify less-significant regressors and will remove them from the final model.
<br ><br>


```r
summary(final.step)
```

```
## 
## Call:
## lm(formula = log(price) ~ MS.Zoning + Lot.Shape + Lot.Config + 
##     Neighborhood + Condition.1 + Condition.2 + Bldg.Type + Overall.Qual + 
##     Overall.Cond + Year.Built + Year.Remod.Add + Exterior.2nd + 
##     Exter.Qual + Exter.Cond + Bsmt.Qual + Bsmt.Exposure + BsmtFin.SF.1 + 
##     BsmtFin.SF.2 + Bsmt.Unf.SF + Heating + Heating.QC + Central.Air + 
##     X2nd.Flr.SF + Bsmt.Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
##     Kitchen.Qual + Functional + Fireplaces + Garage.Cars + Garage.Qual + 
##     Garage.Cond + Wood.Deck.SF + Enclosed.Porch + X3Ssn.Porch + 
##     Screen.Porch + Sale.Type + Sale.Condition + log(area) + log(Lot.Area) + 
##     log(X1st.Flr.SF), data = clean.data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.92136 -0.04797  0.00000  0.04964  0.61578 
## 
## Coefficients: (1 not defined because of singularities)
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           -6.370e-02  1.083e+00  -0.059 0.953127    
## MS.ZoningFV            3.834e-01  6.673e-02   5.746 1.30e-08 ***
## MS.ZoningI (all)       5.273e-01  1.480e-01   3.564 0.000388 ***
## MS.ZoningRH            4.294e-01  7.661e-02   5.605 2.88e-08 ***
## MS.ZoningRL            4.407e-01  5.850e-02   7.534 1.35e-13 ***
## MS.ZoningRM            3.575e-01  5.445e-02   6.566 9.38e-11 ***
## Lot.ShapeIR2           2.919e-02  2.332e-02   1.252 0.210949    
## Lot.ShapeIR3           1.921e-01  7.112e-02   2.702 0.007045 ** 
## Lot.ShapeReg           1.992e-03  8.950e-03   0.223 0.823905    
## Lot.ConfigCulDSac     -1.279e-02  1.698e-02  -0.753 0.451598    
## Lot.ConfigFR2         -4.870e-02  2.300e-02  -2.118 0.034500 *  
## Lot.ConfigFR3         -9.966e-02  5.543e-02  -1.798 0.072571 .  
## Lot.ConfigInside      -2.345e-02  1.013e-02  -2.315 0.020873 *  
## NeighborhoodBlueste    6.134e-02  7.794e-02   0.787 0.431521    
## NeighborhoodBrDale     5.644e-02  6.154e-02   0.917 0.359333    
## NeighborhoodBrkSide    1.003e-01  5.089e-02   1.971 0.049103 *  
## NeighborhoodClearCr    2.858e-02  5.216e-02   0.548 0.583846    
## NeighborhoodCollgCr   -2.052e-02  3.881e-02  -0.529 0.597118    
## NeighborhoodCrawfor    1.300e-01  4.632e-02   2.807 0.005128 ** 
## NeighborhoodEdwards   -3.775e-02  4.186e-02  -0.902 0.367416    
## NeighborhoodGilbert   -1.309e-02  4.143e-02  -0.316 0.752120    
## NeighborhoodGreens     7.759e-02  6.642e-02   1.168 0.243098    
## NeighborhoodGrnHill    5.497e-01  1.145e-01   4.801 1.89e-06 ***
## NeighborhoodIDOTRR     5.996e-02  5.780e-02   1.037 0.299903    
## NeighborhoodMeadowV   -1.945e-02  6.127e-02  -0.318 0.750932    
## NeighborhoodMitchel   -3.965e-03  4.240e-02  -0.094 0.925508    
## NeighborhoodNAmes     -2.515e-02  4.126e-02  -0.610 0.542278    
## NeighborhoodNoRidge    5.841e-02  4.364e-02   1.338 0.181164    
## NeighborhoodNPkVill    5.983e-02  1.120e-01   0.534 0.593302    
## NeighborhoodNridgHt    6.675e-02  3.904e-02   1.710 0.087726 .  
## NeighborhoodNWAmes    -3.898e-02  4.351e-02  -0.896 0.370546    
## NeighborhoodOldTown    1.814e-02  5.194e-02   0.349 0.727054    
## NeighborhoodSawyer     1.484e-02  4.297e-02   0.345 0.730028    
## NeighborhoodSawyerW   -3.735e-02  4.116e-02  -0.907 0.364509    
## NeighborhoodSomerst    1.208e-01  4.625e-02   2.612 0.009181 ** 
## NeighborhoodStoneBr    1.201e-01  4.377e-02   2.744 0.006203 ** 
## NeighborhoodSWISU      2.729e-02  5.461e-02   0.500 0.617447    
## NeighborhoodTimber    -2.175e-03  4.522e-02  -0.048 0.961654    
## NeighborhoodVeenker   -1.592e-02  5.283e-02  -0.301 0.763195    
## Condition.1Feedr       2.454e-02  3.613e-02   0.679 0.497281    
## Condition.1Norm        8.541e-02  3.068e-02   2.784 0.005506 ** 
## Condition.1PosA        1.250e-01  5.231e-02   2.390 0.017070 *  
## Condition.1PosN        1.205e-01  4.869e-02   2.474 0.013576 *  
## Condition.1RRAe        6.466e-02  4.963e-02   1.303 0.192971    
## Condition.1RRAn        3.651e-02  4.842e-02   0.754 0.451007    
## Condition.1RRNe       -2.304e-02  8.549e-02  -0.269 0.787639    
## Condition.1RRNn       -1.560e-02  6.900e-02  -0.226 0.821220    
## Condition.2Feedr       2.747e-01  1.397e-01   1.966 0.049692 *  
## Condition.2Norm        3.798e-01  1.315e-01   2.888 0.003989 ** 
## Condition.2PosA        3.449e-01  1.777e-01   1.940 0.052700 .  
## Condition.2PosN       -4.040e-01  1.623e-01  -2.489 0.013004 *  
## Condition.2RRNn        4.954e-01  1.770e-01   2.799 0.005247 ** 
## Bldg.Type2fmCon       -1.418e-02  3.775e-02  -0.376 0.707272    
## Bldg.TypeDuplex       -1.692e-01  4.238e-02  -3.991 7.18e-05 ***
## Bldg.TypeTwnhs        -9.932e-02  3.161e-02  -3.142 0.001743 ** 
## Bldg.TypeTwnhsE       -5.647e-02  2.081e-02  -2.713 0.006807 ** 
## Overall.Qual           4.027e-02  5.713e-03   7.050 3.91e-12 ***
## Overall.Cond           4.697e-02  4.950e-03   9.488  < 2e-16 ***
## Year.Built             2.801e-03  4.317e-04   6.488 1.54e-10 ***
## Year.Remod.Add         5.651e-04  3.229e-04   1.750 0.080542 .  
## Exterior.2ndBrk Cmn    1.983e-01  1.332e-01   1.489 0.136840    
## Exterior.2ndBrkFace    2.996e-01  4.684e-02   6.396 2.73e-10 ***
## Exterior.2ndCmentBd    2.284e-01  4.835e-02   4.725 2.73e-06 ***
## Exterior.2ndHdBoard    2.169e-01  4.182e-02   5.188 2.71e-07 ***
## Exterior.2ndImStucc    2.023e-01  5.670e-02   3.568 0.000381 ***
## Exterior.2ndMetalSd    2.650e-01  4.083e-02   6.491 1.51e-10 ***
## Exterior.2ndPlywood    2.178e-01  4.275e-02   5.095 4.38e-07 ***
## Exterior.2ndStucco     2.792e-01  5.289e-02   5.279 1.68e-07 ***
## Exterior.2ndVinylSd    2.404e-01  4.146e-02   5.799 9.64e-09 ***
## Exterior.2ndWd Sdng    2.475e-01  4.143e-02   5.975 3.48e-09 ***
## Exterior.2ndWd Shng    2.406e-01  4.545e-02   5.295 1.55e-07 ***
## Exter.QualFa           3.248e-02  5.558e-02   0.584 0.559094    
## Exter.QualGd          -4.851e-02  2.695e-02  -1.800 0.072224 .  
## Exter.QualTA          -6.747e-02  3.091e-02  -2.183 0.029336 *  
## Exter.CondFa          -9.998e-02  7.445e-02  -1.343 0.179674    
## Exter.CondGd           1.924e-02  6.392e-02   0.301 0.763545    
## Exter.CondTA           4.853e-02  6.326e-02   0.767 0.443166    
## Bsmt.QualFa           -1.381e-01  3.582e-02  -3.856 0.000125 ***
## Bsmt.QualGd           -4.760e-02  1.794e-02  -2.653 0.008146 ** 
## Bsmt.QualPo            9.447e-01  1.784e-01   5.294 1.55e-07 ***
## Bsmt.QualTA           -5.063e-02  2.286e-02  -2.215 0.027075 *  
## Bsmt.ExposureAv        7.887e-02  1.053e-01   0.749 0.453972    
## Bsmt.ExposureGd        1.251e-01  1.059e-01   1.181 0.238081    
## Bsmt.ExposureMn        5.036e-02  1.057e-01   0.477 0.633806    
## Bsmt.ExposureNo        5.030e-02  1.052e-01   0.478 0.632700    
## BsmtFin.SF.1           1.367e-04  2.373e-05   5.760 1.20e-08 ***
## BsmtFin.SF.2           9.434e-05  3.032e-05   3.111 0.001931 ** 
## Bsmt.Unf.SF            6.954e-05  2.306e-05   3.016 0.002646 ** 
## HeatingGasW            1.140e-01  5.117e-02   2.228 0.026181 *  
## HeatingGrav            3.561e-01  1.546e-01   2.304 0.021464 *  
## Heating.QCFa          -7.544e-02  3.293e-02  -2.291 0.022245 *  
## Heating.QCGd          -2.640e-03  1.183e-02  -0.223 0.823500    
## Heating.QCPo          -1.073e-01  1.209e-01  -0.888 0.375064    
## Heating.QCTA          -2.641e-02  1.148e-02  -2.299 0.021749 *  
## Central.AirY           1.326e-01  3.001e-02   4.418 1.14e-05 ***
## X2nd.Flr.SF            8.293e-05  3.356e-05   2.471 0.013694 *  
## Bsmt.Full.Bath         3.239e-02  9.853e-03   3.287 0.001057 ** 
## Bedroom.AbvGr         -1.326e-02  7.432e-03  -1.785 0.074721 .  
## Kitchen.AbvGr          1.112e-01  4.260e-02   2.611 0.009208 ** 
## Kitchen.QualFa        -1.227e-01  3.973e-02  -3.089 0.002081 ** 
## Kitchen.QualGd        -5.197e-02  2.030e-02  -2.560 0.010662 *  
## Kitchen.QualPo         2.950e-01  1.304e-01   2.262 0.023989 *  
## Kitchen.QualTA        -7.426e-02  2.286e-02  -3.248 0.001210 ** 
## FunctionalMin1         2.780e-02  7.579e-02   0.367 0.713828    
## FunctionalMin2         3.371e-02  7.491e-02   0.450 0.652814    
## FunctionalMod         -2.320e-02  7.642e-02  -0.304 0.761496    
## FunctionalSal         -1.397e-01  1.514e-01  -0.923 0.356319    
## FunctionalTyp          8.395e-02  7.049e-02   1.191 0.234041    
## Fireplaces             1.676e-02  7.393e-03   2.267 0.023647 *  
## Garage.Cars            2.676e-02  8.556e-03   3.128 0.001826 ** 
## Garage.QualFa         -1.306e-01  1.112e-01  -1.174 0.240563    
## Garage.QualGd         -5.936e-02  1.214e-01  -0.489 0.625022    
## Garage.QualPo         -6.699e-01  1.700e-01  -3.942 8.81e-05 ***
## Garage.QualTA         -1.430e-01  1.095e-01  -1.306 0.192074    
## Garage.CondFa         -1.211e-01  3.486e-02  -3.473 0.000542 ***
## Garage.CondGd         -1.142e-02  5.229e-02  -0.218 0.827103    
## Garage.CondPo          2.386e-01  6.489e-02   3.677 0.000252 ***
## Garage.CondTA                 NA         NA      NA       NA    
## Wood.Deck.SF           5.812e-05  3.386e-05   1.716 0.086474 .  
## Enclosed.Porch         1.396e-04  7.503e-05   1.860 0.063281 .  
## X3Ssn.Porch            2.112e-04  1.216e-04   1.736 0.082880 .  
## Screen.Porch           1.439e-04  6.853e-05   2.100 0.036030 *  
## Sale.TypeCon           5.216e-02  5.621e-02   0.928 0.353725    
## Sale.TypeConLD         1.239e-01  6.187e-02   2.003 0.045556 *  
## Sale.TypeConLI        -5.131e-02  6.165e-02  -0.832 0.405541    
## Sale.TypeConLw         4.436e-02  5.138e-02   0.863 0.388264    
## Sale.TypeCWD           7.768e-02  6.679e-02   1.163 0.245185    
## Sale.TypeNew          -5.007e-02  7.399e-02  -0.677 0.498846    
## Sale.TypeOth           1.981e-01  8.154e-02   2.430 0.015318 *  
## Sale.TypeVWD          -2.669e-02  1.130e-01  -0.236 0.813306    
## Sale.TypeWD            2.165e-03  2.382e-02   0.091 0.927588    
## Sale.ConditionAlloca   9.401e-02  1.145e-01   0.821 0.412041    
## Sale.ConditionFamily  -2.872e-02  3.182e-02  -0.903 0.367029    
## Sale.ConditionNormal   7.716e-02  1.673e-02   4.613 4.64e-06 ***
## Sale.ConditionPartial  1.522e-01  7.084e-02   2.148 0.032031 *  
## log(area)              3.192e-01  5.192e-02   6.147 1.25e-09 ***
## log(Lot.Area)          2.993e-02  1.507e-02   1.986 0.047374 *  
## log(X1st.Flr.SF)       1.260e-01  4.634e-02   2.718 0.006708 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1037 on 790 degrees of freedom
## Multiple R-squared:  0.9438,	Adjusted R-squared:  0.9342 
## F-statistic:  97.6 on 136 and 790 DF,  p-value: < 2.2e-16
```
<br> <br>
Specifically, we'll anchor on the <b>most-significant</b> level per factor. As long as one level of the factor is highly robust, we'll consider it important to the efficacy of the model (at least for the time being). We'll remove non-significant regressors from the model, using `p < 0.05` as a bare minimum significance level.
<br> <br>


```r
final.step <- update(final.step, . ~ . -Lot.Shape -Lot.Config -Condition.1 -Condition.2 -Year.Remod.Add
                     -Exter.Qual -Exter.Cond -BsmtExposure -Heating -X2nd.Flr.SF -Bedroom.AbvGr -Functional
                     -Fireplaces -Garage.Cars -Wood.Deck.SF -Enclosed.Porch -X3Ssn.Porch -Sale.Type)

x <- (summary(final.step)$adj.r.squared) * 100

paste(round(x, 2), "% Explained Variance", sep = "")
```

```
## [1] "91.71% Explained Variance"
```

<br>
We'll repeat this process, until we arrive at a model with 20 coefficients.
<br>


```r
final.step <- update(final.step, . ~ . -Bsmt.Exposure -BsmtFin.SF.2 -Bsmt.Unf.SF -Heating.QC)

x <- (summary(final.step)$adj.r.squared) * 100

paste(round(x, 2), "% Explained Variance", sep = "")
```

```
## [1] "91.38% Explained Variance"
```

<br>
<mark>Our final model explains <b>91.38%</b> of the variance in house prices in Ames, IA. </mark>
<br>

### Section 3.1 Final Model

<i>Provide the summary table for your model.</i>

* * *


```r
summary(final.step)
```

```
## 
## Call:
## lm(formula = log(price) ~ MS.Zoning + Neighborhood + Bldg.Type + 
##     Overall.Qual + Overall.Cond + Year.Built + Exterior.2nd + 
##     Bsmt.Qual + BsmtFin.SF.1 + Central.Air + Bsmt.Full.Bath + 
##     Kitchen.AbvGr + Kitchen.Qual + Garage.Qual + Garage.Cond + 
##     Screen.Porch + Sale.Condition + log(area) + log(Lot.Area) + 
##     log(X1st.Flr.SF), data = clean.data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.17701 -0.05668  0.00087  0.05754  0.47916 
## 
## Coefficients: (1 not defined because of singularities)
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           -7.304e-01  9.333e-01  -0.783 0.434058    
## MS.ZoningFV            2.976e-01  7.108e-02   4.187 3.11e-05 ***
## MS.ZoningI (all)       3.604e-01  1.374e-01   2.624 0.008857 ** 
## MS.ZoningRH            2.428e-01  8.252e-02   2.943 0.003338 ** 
## MS.ZoningRL            3.019e-01  6.246e-02   4.833 1.60e-06 ***
## MS.ZoningRM            2.390e-01  5.773e-02   4.140 3.82e-05 ***
## NeighborhoodBlueste    8.662e-03  8.547e-02   0.101 0.919298    
## NeighborhoodBrDale    -1.440e-02  6.711e-02  -0.215 0.830131    
## NeighborhoodBrkSide   -2.574e-04  5.516e-02  -0.005 0.996277    
## NeighborhoodClearCr   -8.438e-03  5.779e-02  -0.146 0.883955    
## NeighborhoodCollgCr   -8.076e-02  4.292e-02  -1.882 0.060225 .  
## NeighborhoodCrawfor    5.865e-02  4.986e-02   1.176 0.239874    
## NeighborhoodEdwards   -1.249e-01  4.583e-02  -2.726 0.006552 ** 
## NeighborhoodGilbert   -7.591e-02  4.539e-02  -1.672 0.094796 .  
## NeighborhoodGreens     1.135e-01  7.283e-02   1.558 0.119492    
## NeighborhoodGrnHill    4.216e-01  1.282e-01   3.287 0.001053 ** 
## NeighborhoodIDOTRR    -5.156e-02  6.217e-02  -0.829 0.407065    
## NeighborhoodMeadowV   -7.454e-02  6.750e-02  -1.104 0.269809    
## NeighborhoodMitchel   -7.324e-02  4.680e-02  -1.565 0.117982    
## NeighborhoodNAmes     -1.048e-01  4.517e-02  -2.320 0.020601 *  
## NeighborhoodNoRidge    2.229e-02  4.788e-02   0.465 0.641699    
## NeighborhoodNPkVill    3.256e-02  1.269e-01   0.257 0.797560    
## NeighborhoodNridgHt    4.751e-02  4.323e-02   1.099 0.272099    
## NeighborhoodNWAmes    -1.338e-01  4.691e-02  -2.852 0.004455 ** 
## NeighborhoodOldTown   -3.607e-02  5.660e-02  -0.637 0.524177    
## NeighborhoodSawyer    -8.456e-02  4.707e-02  -1.796 0.072780 .  
## NeighborhoodSawyerW   -1.103e-01  4.517e-02  -2.442 0.014793 *  
## NeighborhoodSomerst    8.415e-03  5.007e-02   0.168 0.866580    
## NeighborhoodStoneBr    8.853e-02  4.789e-02   1.849 0.064870 .  
## NeighborhoodSWISU     -4.553e-02  6.010e-02  -0.757 0.448966    
## NeighborhoodTimber    -4.827e-02  5.009e-02  -0.964 0.335429    
## NeighborhoodVeenker   -8.718e-02  5.743e-02  -1.518 0.129360    
## Bldg.Type2fmCon       -2.094e-02  4.022e-02  -0.521 0.602733    
## Bldg.TypeDuplex       -1.625e-01  4.556e-02  -3.566 0.000383 ***
## Bldg.TypeTwnhs        -8.679e-02  3.380e-02  -2.567 0.010414 *  
## Bldg.TypeTwnhsE       -5.062e-02  2.173e-02  -2.329 0.020076 *  
## Overall.Qual           5.437e-02  5.894e-03   9.225  < 2e-16 ***
## Overall.Cond           5.202e-02  4.840e-03  10.747  < 2e-16 ***
## Year.Built             3.729e-03  4.365e-04   8.544  < 2e-16 ***
## Exterior.2ndBrk Cmn    1.480e-01  1.487e-01   0.995 0.320054    
## Exterior.2ndBrkFace    2.794e-01  5.146e-02   5.429 7.39e-08 ***
## Exterior.2ndCmentBd    1.977e-01  5.290e-02   3.736 0.000199 ***
## Exterior.2ndHdBoard    1.872e-01  4.549e-02   4.116 4.24e-05 ***
## Exterior.2ndImStucc    2.229e-01  6.283e-02   3.548 0.000410 ***
## Exterior.2ndMetalSd    2.408e-01  4.444e-02   5.419 7.81e-08 ***
## Exterior.2ndPlywood    1.874e-01  4.635e-02   4.042 5.78e-05 ***
## Exterior.2ndStucco     3.349e-01  5.680e-02   5.896 5.37e-09 ***
## Exterior.2ndVinylSd    2.132e-01  4.489e-02   4.749 2.40e-06 ***
## Exterior.2ndWd Sdng    2.250e-01  4.518e-02   4.980 7.70e-07 ***
## Exterior.2ndWd Shng    2.276e-01  4.964e-02   4.584 5.24e-06 ***
## Bsmt.QualFa           -1.531e-01  3.830e-02  -3.998 6.94e-05 ***
## Bsmt.QualGd           -5.589e-02  1.931e-02  -2.895 0.003894 ** 
## Bsmt.QualPo            7.185e-01  1.789e-01   4.016 6.45e-05 ***
## Bsmt.QualTA           -7.262e-02  2.474e-02  -2.936 0.003417 ** 
## BsmtFin.SF.1           7.463e-05  1.327e-05   5.624 2.53e-08 ***
## Central.AirY           1.225e-01  2.841e-02   4.312 1.81e-05 ***
## Bsmt.Full.Bath         4.577e-02  1.039e-02   4.406 1.18e-05 ***
## Kitchen.AbvGr          9.899e-02  4.548e-02   2.177 0.029770 *  
## Kitchen.QualFa        -1.100e-01  4.250e-02  -2.588 0.009830 ** 
## Kitchen.QualGd        -5.073e-02  2.077e-02  -2.442 0.014812 *  
## Kitchen.QualPo         2.151e-01  1.344e-01   1.600 0.109890    
## Kitchen.QualTA        -8.599e-02  2.343e-02  -3.670 0.000258 ***
## Garage.QualFa         -1.772e-01  1.253e-01  -1.414 0.157637    
## Garage.QualGd         -1.730e-01  1.340e-01  -1.292 0.196850    
## Garage.QualPo         -5.559e-01  1.671e-01  -3.326 0.000919 ***
## Garage.QualTA         -2.094e-01  1.232e-01  -1.700 0.089503 .  
## Garage.CondFa         -1.822e-01  3.535e-02  -5.154 3.18e-07 ***
## Garage.CondGd          1.555e-02  5.684e-02   0.273 0.784553    
## Garage.CondPo          1.942e-01  6.756e-02   2.874 0.004157 ** 
## Garage.CondTA                 NA         NA      NA       NA    
## Screen.Porch           1.683e-04  7.474e-05   2.251 0.024617 *  
## Sale.ConditionAlloca   6.260e-02  1.230e-01   0.509 0.611028    
## Sale.ConditionFamily  -2.067e-02  3.466e-02  -0.596 0.551024    
## Sale.ConditionNormal   8.038e-02  1.779e-02   4.518 7.12e-06 ***
## Sale.ConditionPartial  1.106e-01  2.417e-02   4.577 5.42e-06 ***
## log(area)              3.963e-01  2.023e-02  19.586  < 2e-16 ***
## log(Lot.Area)          7.469e-02  1.425e-02   5.241 2.01e-07 ***
## log(X1st.Flr.SF)       1.109e-01  2.067e-02   5.367 1.03e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.1187 on 850 degrees of freedom
## Multiple R-squared:  0.9208,	Adjusted R-squared:  0.9138 
## F-statistic: 130.1 on 76 and 850 DF,  p-value: < 2.2e-16
```

* * *

### Section 3.2 Transformation

<i>Did you decide to transform any variables?  Why or why not? Explain in a few sentences.</i>

* * *

This analysis includes several transformations. Intuitively, the area and lot area of a home should be somewhat predictive of its eventual sales price - larger homes ought to command higher prices. This relationship is moderately robust:
<br>


```r
paste("Correlation between Price and Area:", round(cor(log(ames_train$price), ames_train$area), 2))
```

```
## [1] "Correlation between Price and Area: 0.71"
```

<br>
However, the initial models constructed for this assignment <b><i>did not</i></b> indicate that area was a significant predictor of a homes price when included in a linear model. For that reason, we <b>log-transformed</b> the area variable to approximate normality.
<br>


```r
area.values <- ames_train %>% 
                summarise(normal.area.median = median(area),
                  normal.area.mean = mean(area),
                  log.area.median = median(log(area)),
                  log.area.mean = mean(log(area)))
```


<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> normal.area.median </th>
   <th style="text-align:right;"> normal.area.mean </th>
   <th style="text-align:right;"> log.area.median </th>
   <th style="text-align:right;"> log.area.mean </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1411 </td>
   <td style="text-align:right;"> 1476.615 </td>
   <td style="text-align:right;"> 7.252054 </td>
   <td style="text-align:right;"> 7.241232 </td>
  </tr>
</tbody>
</table>

![](Final_peer_files/figure-html/unnamed-chunk-17-1.png)<!-- -->![](Final_peer_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

<br>
As we can see, the distribution of `area` values is right-skewed, such that the majority of data points falls below the mean value. Log-transforming the `area` variable accounts for this skewness, which makes it a more effective regressor in the context of our model.
<br> <br>
We repeated this process for the `Lot.Area` and `X1st.Floor.SF` variables for the same reason, as these variables logically factor in to a home's sale price. All three of these variables retained significance throughout the stepwise regression process, and were included in the final model.
<br>

* * *

### Section 3.3 Variable Interaction

<i>Did you decide to include any variable interactions? Why or why not? Explain in a few sentences.</i>

* * *

<br>
The present model <i>does not</i> include any variable interactions. This is mainly for the sake of parsimony - we achieved a highly robust `R^2` value without including any interactions, and ultimately the simpler and more streamlined a model the better. While it's theoretically possible that an interaction would marginally improve the efficacy of the model, this parsimonious approach has facilitated a robust model as is.
<br>

* * *

### Section 3.4 Variable Selection

<i>What method did you use to select the variables you included? Why did you select the method you used? Explain in a few sentences.</i>

* * *

We arrived at the final model using AIC stepwise regression, in which each regressor is considered at every step - this method is more effective than backward-only-selection, as it does not permanently remove a regressor from consideration. Using the `basic.aic` model developed in section two as a baseline, the `final.step` model starts with the majority of exploratory variables considered, and adds and removes variables stepwise until the most robust model possible remains. The addition of these addition variables makes a quantitative difference in the efficacy of the model - see the difference between the basic and final models below:
<br>


```r
basic <- round(summary(basic.aic)$adj.r.squared * 100, 2)
final <- round(summary(final.step)$adj.r.squared * 100, 2)
improved <- final - basic

paste("Basic Model Explained Variance: ", basic, "%", sep = "")
```

```
## [1] "Basic Model Explained Variance: 85.71%"
```

```r
paste("Final Model Explained Variance: ", final, "%", sep = "")
```

```
## [1] "Final Model Explained Variance: 91.38%"
```

```r
paste("Improvement: ", improved, "%", sep = "")
```

```
## [1] "Improvement: 5.67%"
```
<br>
A 5.67% increase in explained variance is huge, and certainly provides evidence to support our methodological approach. 
<br> <br>
Notably, the stepwise regression model initially yielded a model with even more predictive power than our final model. Eliminating regressors to comply with the requirements of this assignment deteriorated the adjusted `R^2` value of the final model, albeit slightly.
<br>

```r
massive <- round(initial.r2 * 100, 2)
paste("Final Model Without Variable Subtraction: ", massive, "% Explained Variance", sep = "")
```

```
## [1] "Final Model Without Variable Subtraction: 93.42% Explained Variance"
```

* * *

### Section 3.5 Model Testing

<i>How did testing the model on out-of-sample data affect whether or how you changed your model? Explain in a few sentences.</i>

* * *

Our basic model was relatively successful in predicting real estate prices in Ames. However, we yielded a higher-than-desired RSME values. $34,384 is certainly not an insignificant average squared residual - this suggests that the average prediction error is higher than we would like. All things being equal, we'd hope that adding more significant regressors would afford us a boost in `R^2`, which would reduce our average residuals. <br>


```r
ames_test <- ames_test %>% 
        filter(Exterior.2nd != "AsphShn" & Exterior.2nd != "Stone")

ames_test['final.predictions'] <- exp(predict(final.step, ames_test))
```

![](Final_peer_files/figure-html/unnamed-chunk-19-1.png)<!-- -->![](Final_peer_files/figure-html/unnamed-chunk-19-2.png)<!-- -->
<br>
The slight curvature of the regression light indicates that the model is following the trend (as opposed to being strictly linear). This suggests that our model has been improved from its first iteration - we'll confirm this by calculating the updated RSME in section 4.2.

* * *

## Part 4 Final Model Assessment

### Section 4.1 Final Model Residual

<i>For your final model, create and briefly interpret an informative plot of the residuals.</i>

* * *

Next, we'll plot the distribution of residual values in the final model.

![](Final_peer_files/figure-html/unnamed-chunk-20-1.png)<!-- -->![](Final_peer_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

<br>
Much like our training model, the residuals in the final model are clearly normally distributed. Plotted against the fitted values, the residuals in this model show no defined pattern, are centered around the y-intercept line, and appear to be fairly symmetrical. This suggests that the final model's predictive power does not have a "blind spot", so to speak.
<br>

* * *

### Section 4.2 Final Model RMSE

<i>For your final model, calculate and briefly comment on the RMSE.</i>

* * *

Using predicted values from the test data, we'll calculate the RMSE for our final model.
<br>


```r
final.predictions <- exp(predict(final.step, ames_test))

final.resids <- ames_test$price - final.predictions

final.rmse <- sqrt(mean(final.resids^2, na.rm = T))

paste("Final Model RMSE in Test Data: $", round(final.rmse, 2), sep = "")
```

```
## [1] "Final Model RMSE in Test Data: $21485.24"
```

<br>
Compared to the RMSE in our test model, this value is markedly improved. We observe a mean standard error of $21,485.24 in predicting price. This is not an insignificant level of error, but in the context of the data it is certainly acceptable.
<br>


```r
paste("Basic Model RMSE in Test Data: $", round(basic.rmse, 2), sep = "")
```

```
## [1] "Basic Model RMSE in Test Data: $34384.39"
```

```r
paste("Improvment in RMSE: $", round(basic.rmse - final.rmse, 2), sep = "")
```

```
## [1] "Improvment in RMSE: $12899.15"
```

<br>
In the context of our data, our final model predicts home prices $12,899.15 more accurately.
<br>

* * *

### Section 4.3 Final Model Evaluation

<i>What are some strengths and weaknesses of your model?</i>

* * *

<b><u>Strengths</u></b> <br>
A major strength of this model is its <i>variety</i> of regressors. We've included variables ranging from garage quality to neighborhood to year of construction:


```r
final.step$call
```

```
## lm(formula = log(price) ~ MS.Zoning + Neighborhood + Bldg.Type + 
##     Overall.Qual + Overall.Cond + Year.Built + Exterior.2nd + 
##     Bsmt.Qual + BsmtFin.SF.1 + Central.Air + Bsmt.Full.Bath + 
##     Kitchen.AbvGr + Kitchen.Qual + Garage.Qual + Garage.Cond + 
##     Screen.Porch + Sale.Condition + log(area) + log(Lot.Area) + 
##     log(X1st.Flr.SF), data = clean.data)
```
<br>
The inclusion of this range of variables boosts the model's ecological validity, as it paints a much broader image of the property in question (as opposed to only factoring in one component of the home; e.g., only factoring in the exterior qualities of a home).
<br>
<br>

<b><u>Weaknesses</u></b> <br>
Notably, we observed the most robust adjusted `R^2` value following the initial stepwise regression. For the sake of this assignment, we trimmed the number of regressors down to 20, despite the loss in predictive power afforded to this massive model. 


```r
initial.call
```

```
## lm(formula = log(price) ~ MS.Zoning + Lot.Shape + Lot.Config + 
##     Neighborhood + Condition.1 + Condition.2 + Bldg.Type + Overall.Qual + 
##     Overall.Cond + Year.Built + Year.Remod.Add + Exterior.2nd + 
##     Exter.Qual + Exter.Cond + Bsmt.Qual + Bsmt.Exposure + BsmtFin.SF.1 + 
##     BsmtFin.SF.2 + Bsmt.Unf.SF + Heating + Heating.QC + Central.Air + 
##     X2nd.Flr.SF + Bsmt.Full.Bath + Bedroom.AbvGr + Kitchen.AbvGr + 
##     Kitchen.Qual + Functional + Fireplaces + Garage.Cars + Garage.Qual + 
##     Garage.Cond + Wood.Deck.SF + Enclosed.Porch + X3Ssn.Porch + 
##     Screen.Porch + Sale.Type + Sale.Condition + log(area) + log(Lot.Area) + 
##     log(X1st.Flr.SF), data = clean.data)
```

```r
paste(round(initial.r2 * 100, 2), "% Explained Variance", sep = "")
```

```
## [1] "93.42% Explained Variance"
```

<br>
In a real-world context, we may decide to retain these extra variables rather than remove them. Regardless, our final model retained a majority of its predictive efficacy, so this weakness is only marginally significant.
<br> <br>
Another weakness of this model is the exclusion of several factor levels. This is ultimately necessary, as the factor levels in question were not represented in the training data. However, this lack of representation somewhat limits the applicability of the model, such that it becomes more narrow.
<br>

* * *

### Section 4.4 Final Model Validation

<i>Testing your final model on a separate, validation data set is a great way to determine how your model will perform in real-life practice. 

You will use the “ames_validation” dataset to do some additional assessment of your final model. Discuss your findings, be sure to mention:
* What is the RMSE of your final model when applied to the validation data?  
* How does this value compare to that of the training data and/or testing data?
* What percentage of the 95% predictive confidence (or credible) intervals contain the true price of the house in the validation data set?  
* From this result, does your final model properly reflect uncertainty?</i>


```r
load("ames_validation.Rdata")
```

* * *

The final step in this EDA will be to validate the selected model on an indpendent data set to confirm its predictive efficacy. Given the high `R^2` value and relatively low RMSE value, we should expect the model to perform quite well.
<br> <br>
First, we'll check the number of observations in the validation data set:
<br>

```r
paste(nrow(ames_validation), "observations")
```

```
## [1] "763 observations"
```

<br>
This data set is slightly smaller than our training set, though this shouldn't affect our ability to predict real estate prices. Like we did with the test data earlier, we'll need to match the model to the data - again, <b><u>we cannot</u></b> bootstrap a regression model to fit factors it does not have.
<br>


```r
ames_validation <- ames_validation %>% 
        filter(MS.Zoning != "A (agr)") %>% 
        filter(Exterior.2nd != "CBlock" & Exterior.2nd != "PreCast" & Exterior.2nd != "Stone")

paste(nrow(ames_validation), "observations")
```

```
## [1] "757 observations"
```

<br>
6 observations in the validation data set will be excluded to create harmony between the data and the model.
<br>

* * *

#### Fitting the Model

* * * 

We'll follow the same process as we have previously to exponentiate the predicted sale prices in the validation data set.


```r
ames_validation['predicted.values'] <- exp(predict(final.step, ames_validation))
```

```
## Warning in predict.lm(final.step, ames_validation): prediction from a rank-deficient fit may be misleading
```

![](Final_peer_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

<br>
As we anticipated, the model provides an excellent fit to this data set. We observe a tight cluster of data around the regression line, with few data points landing far away from the group. Additionally, the slight curve of the regression line near the maximum limit of the x-axis suggests that the model accurately follows the trend of the data - in other words, it is not strictly linear and instead "bends" to fit the data.
<br>

* * *

#### RMSE

* * *

As we have observed in the test data set, the final model yields a lower RMSE value than the test model - this suggests that the final model will perform better when applied to novel data. We'll calculate this value to confirm.
<br>

```r
# Exponentiate the data
valid.predictions <- exp(predict(final.step, ames_validation))

valid.resids <- ames_validation$price - valid.predictions

valid.squares <- valid.resids^2

valid.rmse <- sqrt(mean(valid.squares, na.rm = T))

paste("Validation RMSE: $", round(valid.rmse, 2), sep = "")
```

```
## [1] "Validation RMSE: $18915.64"
```
<br>
Indeed, relative to the test model, the final model yields a much lower RMSE value. We can interpret this as less anticipated error in fitting our model. $18915.64 is a very tolerable value in this case, especially given the variance in home prices in the validation data.
<br>

```r
val.priceRange <- range(ames_validation$price)

paste("Price Range in Validation Data Set: $", val.priceRange[2] - val.priceRange[1], sep = "")
```

```
## [1] "Price Range in Validation Data Set: $549500"
```

<br>
Broadly speaking, the final model is a better, more accurate predictor of real estate prices than the test model.
<br>

* * *

#### Confidence Intervals

* * *

Lastly, we'll calculate 95% confidence intervals for each observation in the data set - 757 intervals, in other words. We'll then calculate the coverage probability of the chosen model - this value reflects the probability that predicted values derived from this model are within the observed interval.
<br>

```r
# Calculate 95% confidence interval for each observation
predicted.values.ci <- exp(predict(final.step, ames_validation, interval = "prediction", level = 0.95))

# Calculate proportion of homes with prices within CI
coverage.probability <- mean(ames_validation$price > predicted.values.ci[, "lwr"] &
                                     ames_validation$price < predicted.values.ci[, "upr"], na.rm = T) * 100

paste(round(coverage.probability, 2), "% Coverage Probability", sep = "")
```

```
## [1] "96.28% Coverage Probability"
```
<br>
Like the rest of this model, this value is robust. We observe that there is <b>less than a 4% chance</b> that predicted values derived from this model fall outside of a 95% confidence interval.
<br>

* * *

## Part 5 Conclusion

<i>Provide a brief summary of your results, and a brief discussion of what you have learned about the data and your model.</i>

* * *

The final model in the present analysis offers a highly accurate means of predicting real estate prices in Ames, IA. Using a seperate validation data set, we observe a coverage probability of 96.28%, while only 8.62% of the variance in real estate prices remains unexplained. Ames is observed to be a relatively diverse real estate market, marked by ample variability in price, square footage, and age, to name a few factors. Given this variability, the predictive efficacy of our model is that much more important. 

* * *
