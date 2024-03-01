# Enhanced-Campaign-ROI
Business Problem In the ever-competitive grocery market, ensuring that
marketing campaigns and promotions not only reach the target audience
but also compel them to make purchases is crucial. Regork, a national
grocery chain, employs various marketing campaigns and promotions to
enhance its market reach and elevate sales. However, the effectiveness
of these campaigns and their genuine impact on purchasing behaviors
among diverse customer segments remain intricate puzzles.

Objective The objective of our analysis is to unravel the nuances of
marketing and campaign effectiveness by exploring the relationship
between promotional activities, customer demographics, and purchasing
behaviors. We intend to delve into questions such as:

Which campaigns have demonstrated substantial influence on purchasing?
Are there specific customer demographics that are more responsive to
particular campaigns or promotions? What products are most often
purchased under promotional campaigns? Is there a noticeable uplift in
sales during and after campaigns? Significance Understanding the
dynamics of campaign effectiveness allows Regork to:

Allocate marketing budget more judiciously, maximizing ROI. Tailor
future campaigns to be more resonant with specific customer segments.
Identify and optimize the timing, duration, and featured products of
campaigns. Enhance customer engagement and satisfaction by offering
relevant promotions. Methodology To navigate through this, we will:

Merge transactional data with campaign and product information, creating
a consolidated view of customer purchases in relation to promotional
campaigns. Perform exploratory data analysis to identify patterns,
anomalies, or trends in purchasing behavior during campaigns. Explore
customer demographics to understand who is most influenced by campaigns.
Evaluate the impact of promotions on sales volumes during and
post-campaign periods. Implications Our analysis will pave the way to:

Forge data-driven marketing strategies that amplify sales and customer
engagement. Identify opportunities to enhance campaign effectiveness,
targeting, and timing. Recognize potential areas for growth and further
investment in specific product categories or customer segments.

Load Packages

``` r
library(completejourney) 
```

    ## Welcome to the completejourney package! Learn more about these data
    ## sets at http://bit.ly/completejourney.

``` r
library(tidyverse) 
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)  
library(scales)     
```

    ## 
    ## Attaching package: 'scales'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(ggplot2) 
library(data.table)
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

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
#options(warn=-1)
```

Data Import and Cleaning

``` r
transactions <- get_transactions()
clean_data <- function(data, required_cols){
  data %>%
    select(all_of(required_cols)) %>%
    filter(across(all_of(required_cols), ~!is.na(.))) %>%
    distinct()
}

selected_transactions <- clean_data(transactions, c("household_id", "product_id", "store_id", "quantity", "sales_value", "week", "retail_disc", "coupon_disc", "coupon_match_disc"))
```

    ## Warning: Using `across()` in `filter()` was deprecated in dplyr 1.0.8.
    ## ℹ Please use `if_any()` or `if_all()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
selected_products <- clean_data(products, c("manufacturer_id", "product_id", "department", "product_category", "product_type"))
```

    ## Warning: Using `across()` in `filter()` was deprecated in dplyr 1.0.8.
    ## ℹ Please use `if_any()` or `if_all()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
selected_campaigns <- clean_data(campaigns, c("campaign_id", "household_id"))
```

    ## Warning: Using `across()` in `filter()` was deprecated in dplyr 1.0.8.
    ## ℹ Please use `if_any()` or `if_all()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
selected_coupons <- clean_data(coupons, c("campaign_id", "product_id", "coupon_upc"))
```

    ## Warning: Using `across()` in `filter()` was deprecated in dplyr 1.0.8.
    ## ℹ Please use `if_any()` or `if_all()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
selected_coupon_redemptions <- clean_data(coupon_redemptions, c("campaign_id", "household_id", "coupon_upc","redemption_date"))
```

    ## Warning: Using `across()` in `filter()` was deprecated in dplyr 1.0.8.
    ## ℹ Please use `if_any()` or `if_all()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
selected_demographics <- clean_data(demographics, c("age", "household_id", "income","marital_status","household_size"))
```

    ## Warning: Using `across()` in `filter()` was deprecated in dplyr 1.0.8.
    ## ℹ Please use `if_any()` or `if_all()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
dt_selected_transactions <- as.data.table(selected_transactions)
dt_selected_transactions[, campaign_cost := abs(retail_disc) + abs(coupon_disc) + abs(coupon_match_disc)]
dt_selected_transactions[, net_profit := sales_value - 0.5 * sales_value - campaign_cost]
dt_selected_coupons <- as.data.table(selected_coupons)
dt_selected_campaigns <- as.data.table(selected_campaigns)
dt_selected_coupon_redemptions <- as.data.table(selected_coupon_redemptions)
dt_selected_demographics <- as.data.table(selected_demographics)
dt_selected_campaign_descriptions <- as.data.table(campaign_descriptions)
```

1.  Which campaigns have demonstrated substantial influence on
    purchasing? Top/Bottom Performing Campains With Coupons

``` r
setDTthreads(threads = getOption("datatable.threads"))
options(datatable.allow.cartesian=TRUE)


analysis_data_with_coupons <- dt_selected_transactions[
  dt_selected_coupon_redemptions, 
  on = "household_id",
  nomatch = 0L
]


campaign_impact <- analysis_data_with_coupons[, .(
    total_sales = sum(sales_value, na.rm = TRUE),
    total_transactions = .N,
    avg_sales_per_transaction = mean(sales_value, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE)
  ), by = campaign_id
][order(-total_sales)]

campaign_impact <- campaign_impact[
  dt_selected_campaign_descriptions, 
  on = "campaign_id"
]


top_campaigns <- head(campaign_impact, 5)


bottom_campaigns <- tail(campaign_impact, 5)
top_campaigns
```

    ##    campaign_id total_sales total_transactions avg_sales_per_transaction
    ## 1:           1     1714.35                618                  2.774029
    ## 2:           2    15562.71               6348                  2.451593
    ## 3:           3     8083.16               2766                  2.922328
    ## 4:           4    34284.35               9792                  3.501261
    ## 5:           5    82600.86              21321                  3.874155
    ##    total_quantity campaign_type start_date   end_date
    ## 1:            741        Type B 2017-03-03 2017-04-09
    ## 2:         825016        Type B 2017-03-08 2017-04-09
    ## 3:         510149        Type C 2017-03-13 2017-05-08
    ## 4:        1873063        Type B 2017-03-29 2017-04-30
    ## 5:        5522517        Type B 2017-04-03 2017-05-07

``` r
bottom_campaigns
```

    ##    campaign_id total_sales total_transactions avg_sales_per_transaction
    ## 1:          23    27173.03               9168                  2.963899
    ## 2:          24          NA                 NA                        NA
    ## 3:          25     4188.81               1301                  3.219685
    ## 4:          26    85901.15              26388                  3.255311
    ## 5:          27   238047.27              85025                  2.799733
    ##    total_quantity campaign_type start_date   end_date
    ## 1:        1465286        Type B 2017-12-28 2018-02-04
    ## 2:             NA        Type C 2016-11-14 2017-01-16
    ## 3:          14720        Type B 2016-12-06 2017-02-05
    ## 4:        5342286        Type B 2016-12-28 2017-02-19
    ## 5:        9282453        Type A 2017-02-08 2017-03-26

``` r
combined_campaigns <- rbind(top_campaigns, bottom_campaigns)

top_campaigns
```

    ##    campaign_id total_sales total_transactions avg_sales_per_transaction
    ## 1:           1     1714.35                618                  2.774029
    ## 2:           2    15562.71               6348                  2.451593
    ## 3:           3     8083.16               2766                  2.922328
    ## 4:           4    34284.35               9792                  3.501261
    ## 5:           5    82600.86              21321                  3.874155
    ##    total_quantity campaign_type start_date   end_date
    ## 1:            741        Type B 2017-03-03 2017-04-09
    ## 2:         825016        Type B 2017-03-08 2017-04-09
    ## 3:         510149        Type C 2017-03-13 2017-05-08
    ## 4:        1873063        Type B 2017-03-29 2017-04-30
    ## 5:        5522517        Type B 2017-04-03 2017-05-07

``` r
bottom_campaigns
```

    ##    campaign_id total_sales total_transactions avg_sales_per_transaction
    ## 1:          23    27173.03               9168                  2.963899
    ## 2:          24          NA                 NA                        NA
    ## 3:          25     4188.81               1301                  3.219685
    ## 4:          26    85901.15              26388                  3.255311
    ## 5:          27   238047.27              85025                  2.799733
    ##    total_quantity campaign_type start_date   end_date
    ## 1:        1465286        Type B 2017-12-28 2018-02-04
    ## 2:             NA        Type C 2016-11-14 2017-01-16
    ## 3:          14720        Type B 2016-12-06 2017-02-05
    ## 4:        5342286        Type B 2016-12-28 2017-02-19
    ## 5:        9282453        Type A 2017-02-08 2017-03-26

``` r
top_transactions_plot <- ggplot(top_campaigns, aes(x = reorder(campaign_type, -total_transactions), y = total_transactions, fill = as.factor(campaign_type))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "performance Campaigns with Coupons",
       x = "Campaign Type", 
       y = "Total Transactions") +
  theme_minimal() +
  geom_text(aes(label = scales::dollar(total_transactions)),
            vjust = -0.5,
            col = "black",
            size = 3.5) +
  geom_point(aes(x = reorder(campaign_type, -total_transactions), y = total_transactions), 
             color = "black", size = 3, show.legend = FALSE) +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  ) +
  scale_y_continuous(labels = scales::dollar) 

bottom_transactions_plot <- ggplot(bottom_campaigns, aes(x = reorder(campaign_type, -total_transactions), y = total_transactions, fill = as.factor(campaign_type))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "performance of Campaigns with Coupons",
       x = "Campaign Type", 
       y = "Total Transactions") +
  theme_minimal() +
  geom_text(aes(label = scales::dollar(total_transactions)),
            vjust = -0.5,
            col = "black",
            size = 3.5) +
  geom_point(aes(x = reorder(campaign_type, -total_transactions), y = total_transactions), 
             color = "black", size = 3, show.legend = FALSE)  +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  )  +
  scale_y_continuous(labels = scales::dollar) 


gridExtra::grid.arrange(top_transactions_plot, bottom_transactions_plot, ncol = 1)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_bar()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CampaignTypes_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Top/Bottom Performing Campains Without Coupons

``` r
setDTthreads(threads = getOption("datatable.threads"))
options(datatable.allow.cartesian=TRUE)


analysis_data_without_coupons <- dt_selected_transactions[
  !dt_selected_coupon_redemptions, 
  on = "household_id"
][
  dt_selected_demographics, 
  on = "household_id",
  nomatch = 0L  
][
  dt_selected_campaigns,
  on = .(household_id = household_id),
  nomatch = 0L 
]


campaign_impact <- analysis_data_without_coupons[, .(
    total_sales = sum(sales_value, na.rm = TRUE),
    total_transactions = .N,
    avg_sales_per_transaction = mean(sales_value, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE)
  ), by = campaign_id
][order(-total_sales)]

campaign_impact <- campaign_impact[
  dt_selected_campaign_descriptions, 
  on = "campaign_id"
]


top_campaigns <- head(campaign_impact, 5)


bottom_campaigns <- tail(campaign_impact, 5)


combined_campaigns <- rbind(top_campaigns, bottom_campaigns)

top_campaigns
```

    ##    campaign_id total_sales total_transactions avg_sales_per_transaction
    ## 1:           1     2087.39                597                  3.496466
    ## 2:           2    21259.55               6816                  3.119065
    ## 3:           3     1719.62                916                  1.877314
    ## 4:           4    79261.78              24326                  3.258315
    ## 5:           5   138418.59              41712                  3.318436
    ##    total_quantity campaign_type start_date   end_date
    ## 1:          31061        Type B 2017-03-03 2017-04-09
    ## 2:         652110        Type B 2017-03-08 2017-04-09
    ## 3:           1225        Type C 2017-03-13 2017-05-08
    ## 4:        3633183        Type B 2017-03-29 2017-04-30
    ## 5:        6749876        Type B 2017-04-03 2017-05-07

``` r
top_transactions_plot <- ggplot(top_campaigns, aes(x = reorder(campaign_type, -total_transactions), y = total_transactions, fill = as.factor(campaign_type))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "performance of Campaigns without Coupons",
       x = "Campaign Type", 
       y = "Total Transactions") +
  theme_minimal() +
  geom_text(aes(label = scales::comma(total_transactions)),
            vjust = -0.5,
            col = "black",
            size = 3.5) +
  geom_point(aes(x = reorder(campaign_type, -total_transactions), y = total_transactions + 200), 
             color = "black", size = 3, show.legend = FALSE)  +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  )  +
  scale_y_continuous(labels = scales::dollar) 


bottom_transactions_plot <- ggplot(bottom_campaigns, aes(x = reorder(campaign_type, -total_transactions), y = total_transactions, fill = as.factor(campaign_type))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "performance of Campaigns without Coupons",
       x = "Campaign Type", 
       y = "Total Transactions") +
  theme_minimal() +
  geom_text(aes(label = scales::comma(total_transactions)),
            vjust = -0.5,
            col = "black",
            size = 3.5) +
  geom_point(aes(x = reorder(campaign_type, -total_transactions), y = total_transactions + 200), 
             color = "black", size = 3, show.legend = FALSE)  +
  theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  )  +
  scale_y_continuous(labels = scales::dollar)


gridExtra::grid.arrange(top_transactions_plot, bottom_transactions_plot, ncol = 1)
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_bar()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_text()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CampaignTypes_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

2.  Are there specific customer demographics that are more responsive to
    particular campaigns or promotions? Transactions with coupons with
    different age groups and income

``` r
setDTthreads(threads = getOption("datatable.threads"))
options(datatable.allow.cartesian=TRUE)


analysis_data_with_coupons <- dt_selected_transactions[
  dt_selected_coupon_redemptions, 
  on = "household_id",
  nomatch = 0L
]


campaign_impact <- analysis_data_with_coupons[, .(
    total_sales = sum(sales_value, na.rm = TRUE),
    total_transactions = .N,
    avg_sales_per_transaction = mean(sales_value, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE)
  ), by = .(campaign_id,household_id)
][order(-total_sales)]

campaign_impact <- campaign_impact[
  dt_selected_campaign_descriptions, 
  on = "campaign_id"
]


analysis_data <- campaign_impact[
  dt_selected_demographics, 
  on = "household_id",
  nomatch = 0L
]


demographic_campaign_impact <- analysis_data[, .(
    total_sales = sum(total_sales, na.rm = TRUE),
    total_transactions = .N,
    avg_sales_per_transaction = mean(total_sales, na.rm = TRUE),
    total_quantity = sum(total_quantity, na.rm = TRUE)
  ), by = .(age, income, campaign_type) 
]

demographic_campaign_impact
```

    ##       age    income campaign_type total_sales total_transactions
    ##  1:   65+    35-49K        Type A    69232.09                 12
    ##  2: 45-54 Under 15K        Type A   200771.12                 13
    ##  3: 35-44    35-49K        Type A   377051.13                 27
    ##  4: 45-54    50-74K        Type A   650131.92                 55
    ##  5: 45-54    25-34K        Type A   110958.09                 14
    ##  6: 25-34 Under 15K        Type A    51470.47                  6
    ##  7: 35-44    35-49K        Type B    56872.28                  7
    ##  8: 25-34  100-124K        Type B     3139.93                  1
    ##  9: 45-54    35-49K        Type A    92805.18                 12
    ## 10: 55-64    75-99K        Type A   124219.94                  6
    ## 11: 45-54    50-74K        Type B    73096.22                 11
    ## 12: 35-44  125-149K        Type B     8622.14                  1
    ## 13: 45-54    50-74K        Type C    48848.21                  5
    ## 14: 45-54  125-149K        Type A   409141.60                 12
    ## 15: 45-54  125-149K        Type B    55854.90                  4
    ## 16:   65+    35-49K        Type C     1461.25                  1
    ## 17: 25-34    50-74K        Type A   425047.99                 26
    ## 18: 35-44  150-174K        Type A   267745.45                  7
    ## 19: 45-54  100-124K        Type A   165451.06                 13
    ## 20: 55-64  150-174K        Type B    46557.10                  2
    ## 21: 55-64  150-174K        Type A    68334.44                  4
    ## 22: 45-54    25-34K        Type C     3662.69                  1
    ## 23: 25-34    25-34K        Type A   142914.68                  6
    ## 24: 19-24 Under 15K        Type A    12939.36                  2
    ## 25:   65+    50-74K        Type A    85610.15                  9
    ## 26: 45-54 Under 15K        Type B    57864.23                  5
    ## 27: 55-64    25-34K        Type A     6778.29                  3
    ## 28:   65+  100-124K        Type A     6147.01                  2
    ## 29: 55-64    50-74K        Type A   185098.77                 11
    ## 30: 55-64    50-74K        Type C    10433.44                  2
    ## 31: 35-44    75-99K        Type A   157791.42                 15
    ## 32: 35-44  150-174K        Type B    52383.34                  5
    ## 33:   65+  125-149K        Type A     3354.09                  1
    ## 34: 35-44    35-49K        Type C    14570.32                  1
    ## 35: 35-44    50-74K        Type A   542327.32                 33
    ## 36: 45-54    15-24K        Type A     4130.79                  2
    ## 37: 45-54    35-49K        Type B    21104.72                  4
    ## 38: 25-34    15-24K        Type A     4752.20                  1
    ## 39: 25-34    15-24K        Type B    13099.50                  2
    ## 40: 25-34    50-74K        Type B    80964.85                  9
    ## 41:   65+    25-34K        Type A     3478.19                  2
    ## 42: 45-54    25-34K        Type B    31230.35                  5
    ## 43: 35-44  125-149K        Type A    33565.38                  4
    ## 44: 25-34    75-99K        Type A    83320.90                  7
    ## 45: 35-44    50-74K        Type B    22462.71                  3
    ## 46: 55-64    35-49K        Type A    18383.55                  7
    ## 47: 25-34    25-34K        Type B    64092.78                  2
    ## 48: 35-44    75-99K        Type B    48518.44                  4
    ## 49: 25-34    35-49K        Type A    96608.60                  9
    ## 50: 35-44    15-24K        Type A   118764.79                  8
    ## 51: 55-64    35-49K        Type B     1591.52                  1
    ## 52: 35-44 Under 15K        Type A    12970.84                  2
    ## 53: 45-54    75-99K        Type A   505041.42                 24
    ## 54: 35-44    25-34K        Type B   122975.19                  4
    ## 55: 55-64    75-99K        Type B    12931.01                  2
    ## 56: 45-54    75-99K        Type C    52673.16                  1
    ## 57:   65+ Under 15K        Type A    27527.90                  2
    ## 58: 45-54  150-174K        Type C     3680.44                  1
    ## 59:   65+    75-99K        Type B     4155.06                  1
    ## 60:   65+    75-99K        Type A     4155.06                  1
    ## 61: 19-24    35-49K        Type A     7783.16                  2
    ## 62:   65+    15-24K        Type C     3138.44                  1
    ## 63:   65+    15-24K        Type A    32501.40                  3
    ## 64: 19-24    35-49K        Type B    20888.28                  2
    ## 65:   65+    15-24K        Type B     3634.56                  1
    ## 66: 25-34  125-149K        Type B    47354.23                  3
    ## 67: 19-24    50-74K        Type A    16831.25                  2
    ## 68: 55-64  125-149K        Type A     4131.20                  2
    ## 69: 55-64 Under 15K        Type A     1762.53                  1
    ## 70: 45-54    75-99K        Type B    34507.90                  3
    ## 71: 35-44  200-249K        Type A     5296.11                  1
    ## 72: 35-44  200-249K        Type B     1765.37                  1
    ## 73: 25-34  150-174K        Type A    61445.84                  4
    ## 74: 25-34  150-174K        Type B    31726.50                  2
    ## 75: 45-54     250K+        Type A    30044.01                  3
    ## 76: 55-64  175-199K        Type A      580.16                  1
    ## 77: 45-54  175-199K        Type A   119352.26                  4
    ## 78: 55-64  100-124K        Type A    14827.56                  2
    ## 79: 35-44    25-34K        Type A   105204.66                  1
    ## 80: 45-54  150-174K        Type A    55168.69                  9
    ## 81: 45-54  150-174K        Type B     9415.23                  2
    ## 82: 35-44    50-74K        Type C    34252.51                  3
    ## 83: 45-54    35-49K        Type C     3776.05                  1
    ## 84: 45-54  100-124K        Type B     6537.40                  1
    ## 85: 55-64    50-74K        Type B    19984.65                  2
    ## 86:   65+    50-74K        Type B    16402.18                  3
    ## 87:   65+    50-74K        Type C     4100.58                  2
    ## 88: 25-34    75-99K        Type B    17030.20                  1
    ## 89: 55-64    15-24K        Type A     2089.69                  1
    ## 90: 35-44  100-124K        Type A     2348.42                  1
    ## 91: 25-34  125-149K        Type A     5383.68                  2
    ##       age    income campaign_type total_sales total_transactions
    ##     avg_sales_per_transaction total_quantity
    ##  1:                  5769.341        1146937
    ##  2:                 15443.932       10547712
    ##  3:                 13964.857       16881756
    ##  4:                 11820.580       26183118
    ##  5:                  7925.578        4102905
    ##  6:                  8578.412        1623757
    ##  7:                  8124.611        1821264
    ##  8:                  3139.930           1111
    ##  9:                  7733.765        2473976
    ## 10:                 20703.323        8937714
    ## 11:                  6645.111        1951254
    ## 12:                  8622.140         594251
    ## 13:                  9769.642        3315266
    ## 14:                 34095.133       21890118
    ## 15:                 13963.725        3198549
    ## 16:                  1461.250          11509
    ## 17:                 16348.000       25087344
    ## 18:                 38249.350       14185532
    ## 19:                 12727.005       10912380
    ## 20:                 23278.550        3714611
    ## 21:                 17083.610        6209538
    ## 22:                  3662.690         245213
    ## 23:                 23819.113        5478878
    ## 24:                  6469.680         538306
    ## 25:                  9512.239        1269067
    ## 26:                 11572.846        3118632
    ## 27:                  2259.430         370128
    ## 28:                  3073.505         507089
    ## 29:                 16827.161        7638564
    ## 30:                  5216.720         795451
    ## 31:                 10519.428        3831758
    ## 32:                 10476.668        1396400
    ## 33:                  3354.090          55628
    ## 34:                 14570.320         256052
    ## 35:                 16434.161       44186414
    ## 36:                  2065.395         119522
    ## 37:                  5276.180         862320
    ## 38:                  4752.200           2630
    ## 39:                  6549.750           7307
    ## 40:                  8996.094        7698211
    ## 41:                  1739.095          20053
    ## 42:                  6246.070        1771751
    ## 43:                  8391.345        3026122
    ## 44:                 11902.986        4929567
    ## 45:                  7487.570         788620
    ## 46:                  2626.221         270092
    ## 47:                 32046.390        1884432
    ## 48:                 12129.610        1748257
    ## 49:                 10734.289        9533741
    ## 50:                 14845.599        4483308
    ## 51:                  1591.520         136647
    ## 52:                  6485.420         727081
    ## 53:                 21043.392       27435106
    ## 54:                 30743.798        8684577
    ## 55:                  6465.505        1027972
    ## 56:                 52673.160        2372022
    ## 57:                 13763.950         544686
    ## 58:                  3680.440          10990
    ## 59:                  4155.060         458876
    ## 60:                  4155.060         458876
    ## 61:                  3891.580         102744
    ## 62:                  3138.440           1651
    ## 63:                 10833.800         556893
    ## 64:                 10444.140        2866094
    ## 65:                  3634.560         170078
    ## 66:                 15784.743        2015441
    ## 67:                  8415.625         273506
    ## 68:                  2065.600         209022
    ## 69:                  1762.530            823
    ## 70:                 11502.633        2516810
    ## 71:                  5296.110          42624
    ## 72:                  1765.370          14208
    ## 73:                 15361.460        4819236
    ## 74:                 15863.250        2680038
    ## 75:                 10014.670         196349
    ## 76:                   580.160            312
    ## 77:                 29838.065        5361713
    ## 78:                  7413.780        1122668
    ## 79:                105204.660        7324306
    ## 80:                  6129.854         866542
    ## 81:                  4707.615           3934
    ## 82:                 11417.503        2247215
    ## 83:                  3776.050         159990
    ## 84:                  6537.400           2452
    ## 85:                  9992.325          10375
    ## 86:                  5467.393         328572
    ## 87:                  2050.290         183242
    ## 88:                 17030.200        1498956
    ## 89:                  2089.690          18485
    ## 90:                  2348.420           1167
    ## 91:                  2691.840          54500
    ##     avg_sales_per_transaction total_quantity

``` r
# Visualization (Impact of campaign type on different age groups)
ggplot(demographic_campaign_impact, aes(x = campaign_type, y = total_sales, fill = age)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  labs(title = "Impact of Campaign Type(without coupons) \non Different Age Groups",
       x = "Campaign Type", 
       y = "Total Sales") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
    theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  )  +
  scale_y_continuous(labels = scales::dollar) 
```

![](CampaignTypes_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Visualization (Impact of campaign type on income range)
ggplot(demographic_campaign_impact, aes(x = campaign_type, y = total_sales, fill = income)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  labs(title = "Impact of Campaign Type(without coupons) \non Different Income Range",
       x = "Campaign Type", 
       y = "Total Sales") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
    theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  )  +
  scale_y_continuous(labels = scales::dollar)
```

    ## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Pastel1 is 9
    ## Returning the palette you asked for with that many colors

![](CampaignTypes_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Transactions without coupons with different age groups and income

``` r
setDTthreads(threads = getOption("datatable.threads"))
options(datatable.allow.cartesian=TRUE)


analysis_data_without_coupons <- dt_selected_transactions[
  !dt_selected_coupon_redemptions, 
  on = "household_id"
][
  dt_selected_demographics, 
  on = "household_id",
  nomatch = 0L  
][
  dt_selected_campaigns,
  on = .(household_id = household_id),
  nomatch = 0L  
]


campaign_impact <- analysis_data_without_coupons[, .(
    total_sales = sum(sales_value, na.rm = TRUE),
    total_transactions = .N,
    avg_sales_per_transaction = mean(sales_value, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE)
  ), by = .(campaign_id,household_id)
][order(-total_sales)]

campaign_impact <- campaign_impact[
  dt_selected_campaign_descriptions, 
  on = "campaign_id"
]


analysis_data <- campaign_impact[
  dt_selected_demographics, 
  on = "household_id",
  nomatch = 0L
]


demographic_campaign_impact <- analysis_data[, .(
    total_sales = sum(total_sales, na.rm = TRUE),
    total_transactions = .N,
    avg_sales_per_transaction = mean(total_sales, na.rm = TRUE),
    total_quantity = sum(total_quantity, na.rm = TRUE)
  ), by = .(age, income, campaign_type) 
]

demographic_campaign_impact
```

    ##        age   income campaign_type total_sales total_transactions
    ##   1: 45-54   50-74K        Type B   150138.18                 50
    ##   2: 45-54   50-74K        Type A   236075.87                 86
    ##   3: 35-44   25-34K        Type A    62274.28                 25
    ##   4: 25-34   15-24K        Type B    17223.07                  7
    ##   5: 25-34   15-24K        Type A    44540.68                 18
    ##  ---                                                            
    ## 140: 55-64 125-149K        Type A    14539.38                  3
    ## 141: 19-24 200-249K        Type B     2408.50                  2
    ## 142: 19-24 200-249K        Type A     3612.75                  3
    ## 143: 25-34 200-249K        Type A     8133.50                  2
    ## 144: 25-34 200-249K        Type B     4066.75                  1
    ##      avg_sales_per_transaction total_quantity
    ##   1:                  3002.764        5017498
    ##   2:                  2745.068        8117134
    ##   3:                  2490.971        1585576
    ##   4:                  2460.439         858806
    ##   5:                  2474.482        1735250
    ##  ---                                         
    ## 140:                  4846.460        1182237
    ## 141:                  1204.250         139534
    ## 142:                  1204.250         209301
    ## 143:                  4066.750           3064
    ## 144:                  4066.750           1532

``` r
# Visualization 
ggplot(demographic_campaign_impact, aes(x = campaign_type, y = total_sales, fill = age)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  labs(title = "Impact of Campaign Type(with coupons) \non Different Age Groups",
       x = "Campaign Type", 
       y = "Total Sales") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
    theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  )  +
  scale_y_continuous(labels = scales::dollar)
```

![](CampaignTypes_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Visualization 
ggplot(demographic_campaign_impact, aes(x = campaign_type, y = total_sales, fill = income)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  labs(title = "Impact of Campaign Type(with coupons) \non Different Income Range",
       x = "Campaign Type", 
       y = "Total Sales") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
    theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  )  +
  scale_y_continuous(labels = scales::dollar)
```

    ## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Pastel1 is 9
    ## Returning the palette you asked for with that many colors

![](CampaignTypes_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

ROI Analysis with coupons

``` r
setDTthreads(threads = getOption("datatable.threads"))
options(datatable.allow.cartesian=TRUE)

analysis_data_with_coupons <- dt_selected_transactions[
  dt_selected_coupon_redemptions, 
  on = "household_id",
  nomatch = 0L
]

campaign_impact <- analysis_data_with_coupons[, .(
    total_sales = sum(sales_value, na.rm = TRUE),
    total_transactions = .N,
    avg_sales_per_transaction = mean(sales_value, na.rm = TRUE),
    total_quantity = sum(quantity, na.rm = TRUE),
    total_campaign_cost = sum(campaign_cost, na.rm = TRUE),
    net_profit = sum(net_profit, na.rm = TRUE),
    ROI = (sum(net_profit, na.rm = TRUE) / sum(campaign_cost, na.rm = TRUE)) * 100
  ), by = campaign_id
][order(-total_sales)]


campaign_impact <- campaign_impact[
  dt_selected_campaign_descriptions, 
  on = "campaign_id"
]


print(head(campaign_impact))
```

    ##    campaign_id total_sales total_transactions avg_sales_per_transaction
    ## 1:           1     1714.35                618                  2.774029
    ## 2:           2    15562.71               6348                  2.451593
    ## 3:           3     8083.16               2766                  2.922328
    ## 4:           4    34284.35               9792                  3.501261
    ## 5:           5    82600.86              21321                  3.874155
    ## 6:           6     3138.44               1245                  2.520835
    ##    total_quantity total_campaign_cost net_profit       ROI campaign_type
    ## 1:            741              350.98    506.195 144.22332        Type B
    ## 2:         825016             4281.28   3500.075  81.75300        Type B
    ## 3:         510149             2232.02   1809.560  81.07275        Type C
    ## 4:        1873063             7383.03   9759.145 132.18347        Type B
    ## 5:        5522517            13404.28  27896.150 208.11375        Type B
    ## 6:           1651              548.14   1021.080 186.28088        Type C
    ##    start_date   end_date
    ## 1: 2017-03-03 2017-04-09
    ## 2: 2017-03-08 2017-04-09
    ## 3: 2017-03-13 2017-05-08
    ## 4: 2017-03-29 2017-04-30
    ## 5: 2017-04-03 2017-05-07
    ## 6: 2017-04-19 2017-05-21

``` r
ggplot(campaign_impact, aes(x = campaign_type, y = ROI, fill = campaign_type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "ROI for Different Campaigns",
       x = "Campaign Type", 
       y = "ROI (%)") +
  theme_minimal()  +
    theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](CampaignTypes_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
ggplot(campaign_impact, aes(x = total_campaign_cost, y = total_sales, color = campaign_type)) +
  geom_point(size = 3) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Total Sales vs. Total Campaign Cost",
       x = "Total Campaign Cost", 
       y = "Total Sales") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),  
    axis.title.x = element_text(face = "bold", size = 12),  
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  facet_wrap(~campaign_type) +
  scale_y_continuous(labels = scales::dollar, trans = "log10") +  # Logarithmic scale on y-axis
  scale_x_continuous(labels = scales::dollar, trans = "log10") +  # Logarithmic scale on x-axis
  guides(color = guide_legend(title = "Campaign Type"))
```

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](CampaignTypes_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Conversion Rate

``` r
targeted_households <- dt_selected_campaigns[, .N, by = campaign_id]

converting_households <- dt_selected_transactions[dt_selected_campaigns, on = "household_id", nomatch = 0L
                                                  ][, .N, by = campaign_id]

conversion_rate_data <- targeted_households[converting_households, on = "campaign_id"
                                            ][, conversion_rate := (N / i.N) * 100]


conversion_rate_data <- conversion_rate_data[dt_selected_campaign_descriptions, on = "campaign_id"]

conversion_rate_data
```

    ##     campaign_id    N     i.N conversion_rate campaign_type start_date
    ##  1:           1   13   11887      0.10936317        Type B 2017-03-03
    ##  2:           2   48   37017      0.12967015        Type B 2017-03-08
    ##  3:           3   12   17657      0.06796171        Type C 2017-03-13
    ##  4:           4   81   87804      0.09225092        Type B 2017-03-29
    ##  5:           5  166  214201      0.07749730        Type B 2017-04-03
    ##  6:           6   65  100710      0.06454175        Type C 2017-04-19
    ##  7:           7  198  225118      0.08795387        Type B 2017-04-24
    ##  8:           8 1076  982730      0.10949091        Type A 2017-05-08
    ##  9:           9  176  166277      0.10584747        Type B 2017-05-31
    ## 10:          10  123  163270      0.07533533        Type B 2017-06-28
    ## 11:          11  214  280825      0.07620404        Type B 2017-07-12
    ## 12:          12  170  223674      0.07600347        Type B 2017-07-12
    ## 13:          13 1077 1042566      0.10330281        Type A 2017-08-08
    ## 14:          14  224  322977      0.06935478        Type C 2017-09-04
    ## 15:          15   17   17796      0.09552708        Type C 2017-09-20
    ## 16:          16  188  263252      0.07141446        Type B 2017-10-04
    ## 17:          17  202  280209      0.07208905        Type B 2017-10-18
    ## 18:          18 1133 1071945      0.10569572        Type A 2017-10-30
    ## 19:          19  130  152434      0.08528281        Type B 2017-11-15
    ## 20:          20  244  356127      0.06851488        Type C 2017-11-27
    ## 21:          21   65   85719      0.07582916        Type B 2017-12-06
    ## 22:          22  276  369761      0.07464281        Type B 2017-12-06
    ## 23:          23  183  275321      0.06646787        Type B 2017-12-28
    ## 24:          24   12   15437      0.07773531        Type C 2016-11-14
    ## 25:          25   17   21155      0.08035925        Type B 2016-12-06
    ## 26:          26  118  102402      0.11523212        Type B 2016-12-28
    ## 27:          27  361  302724      0.11925054        Type A 2017-02-08
    ##     campaign_id    N     i.N conversion_rate campaign_type start_date
    ##       end_date
    ##  1: 2017-04-09
    ##  2: 2017-04-09
    ##  3: 2017-05-08
    ##  4: 2017-04-30
    ##  5: 2017-05-07
    ##  6: 2017-05-21
    ##  7: 2017-05-28
    ##  8: 2017-06-25
    ##  9: 2017-07-02
    ## 10: 2017-07-30
    ## 11: 2017-08-27
    ## 12: 2017-08-13
    ## 13: 2017-09-24
    ## 14: 2017-11-08
    ## 15: 2018-02-28
    ## 16: 2017-11-05
    ## 17: 2017-11-19
    ## 18: 2017-12-24
    ## 19: 2017-12-17
    ## 20: 2018-02-05
    ## 21: 2018-01-07
    ## 22: 2018-01-07
    ## 23: 2018-02-04
    ## 24: 2017-01-16
    ## 25: 2017-02-05
    ## 26: 2017-02-19
    ## 27: 2017-03-26
    ##       end_date

``` r
# Visualization of conversion rates per campaign without numbers on top
conversion_rate_data <- targeted_households[converting_households, on = "campaign_id"
                                            ][, conversion_rate := (N / i.N) * 100]

# Merge with campaign descriptions for better readability in plots
conversion_rate_data <- conversion_rate_data[dt_selected_campaign_descriptions, on = "campaign_id"]

# Visualization of conversion rates per campaign without numbers on top
ggplot(conversion_rate_data, aes(x = reorder(campaign_type, -conversion_rate), y = conversion_rate, fill = campaign_type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Conversion Rate for Different Campaigns",
       x = "Campaign Type", 
       y = "Conversion Rate (%)") +
  theme_minimal() +
    theme(
    plot.title = element_text(face = "bold", size = 16),  # Title settings
    axis.title.x = element_text(face = "bold", size = 12),  # X-axis label settings
    axis.title.y = element_text(face = "bold", size = 12)  # Y-axis label settings
  ) +
  scale_y_continuous(labels = scales::percent_format(scale=1))
```

![](CampaignTypes_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
