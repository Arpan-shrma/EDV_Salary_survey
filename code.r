---
title: "Salary Data Analysis"
author: "DATA*6200"
date: "October 11, 2024"
format: 
  revealjs:
    echo: false
    code-line-numbers: false
    slide-number: true
---

## Aim of analysis

-   Which industry or industries have the highest/lowest salaries?
-   Which industries have the highest salary variability?
-   How do salaries vary over time and geography?

## Libraries used

-   Tidyverse
-   Dplyr
-   readxl
-   lubridate
-   patchwork

```{r, include = FALSE}
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(patchwork)
```

# Let's Begin

```{r, echo=FALSE, results='hide'}
#To load Dataset, copy the path of locally saved dataset and paste at place marked
#loading dataset and saving a original copy of the dataframe
df_path <- ("/Users/arpansharma/Desktop/MDS/Data*6200/Assignment_1/ask_a_manager.xlsx") # <------Paste the local dataset path here
original_salary_df <- read_excel(df_path)
```

```{r, echo=FALSE, results='hide'}
# Its giving warning that R is coercing data type of 26564th cell in 6th column. lets inspect about the 6th column.

print(colnames(original_salary_df)[6])
```

```{r, echo=FALSE, results='hide'}
# So, Its the salary column so it should be numeric. Hence, we can ignore the warning.

#making a copy data frame to work on
salary_df <- original_salary_df
salary_df %>% 
  mutate(across(where(is.character), str_trim))
```

```{r, echo=FALSE, results='hide'}
## Dataset First View

salary_df |> head()
```

```{r, echo=FALSE, results='hide'}
#Dimensions of data (Rows, Columns)
dim(salary_df)

#  Number of Rows: 28080 and Number of Columns: 18
```

```{r, echo=FALSE, results='hide'}
# Extracting date from Timestamp. I have extracted date from the timestamp as it included time of response recorded upto seconds. In my view, possible duplicated entries could have different time in seconds or may be in minutes. So, by extracting date will avoid this.

# Replace the 'timestamp' column with the date
salary_df$Timestamp <- as_date(ymd_hms(salary_df$Timestamp))

# View the updated data
print(salary_df)
```

```{r, echo=FALSE, results='hide'}
colnames(salary_df)
```

```{r, echo=FALSE, results='hide'}
# Renaming the columns for easier data exploration using vector of new column names
colnames(salary_df) <- c("date","age_group", "industry", "job_title", "job_title_context", "annual_salary", "bonus_pay", "currency", "specify_other_currency","income_context", "work_country", "us_state", "city", "overall_work_experience", "field_work_experience", "highest_education", "gender", "race"  )
colnames(salary_df)
```

```{r, echo=FALSE, results='hide'}
# Formatting data type of variables like bonus_pay from char to num.
#checking for data type of different variables
str(salary_df)
```

```{r, echo=FALSE, results='hide'}
#   Data type of bonus_pay column should be number, lets change that.

salary_df <- salary_df %>%
  mutate(
    bonus_pay = as.numeric(bonus_pay)
  )
str(salary_df)
```

```{r, echo=FALSE, results='hide'}
# - while Handling Duplicate Rows, There are total 147 duplicate rows found in dataframe.

# Count the total number of rows
total_rows <- nrow(salary_df)

# Count the number of unique rows
unique_rows <- nrow(salary_df %>% distinct())

# Calculate the number of duplicate rows
duplicate_rows <- total_rows - unique_rows

# Print the number of duplicate rows
print(paste("Number of duplicate rows:", duplicate_rows))

#Remove duplicates
salary_df <- unique(salary_df)
str(salary_df)
```

```{r, echo=FALSE, results='hide'}
# Lets conver the columns with character data type to lower case for data cleaning
salary_df <- salary_df %>%
  mutate_at(vars(industry, job_title, work_country,us_state, city, currency, specify_other_currency, highest_education, gender, race), tolower)
```

```{r, echo=FALSE, results='hide'}
### Handling NA values and cleaning columns

# Define a custom function to detect NA-like values
is_na_like <- function(x) {
  str_trim(x) == "" | x %in% c("NA", "N.A", "---", "--", "-", "n/a", "none", "null", "nan") | is.na(x)
}

# Count NA-like values and number of unique values column-wise
na_counts_and_unique <- salary_df %>%
  summarise(across(everything(), list(
    na_count = ~sum(is_na_like(.), na.rm = TRUE),
    unique_count = ~n_distinct(.)
  )))

# View the result
print(na_counts_and_unique)
```

```{r, echo=FALSE, results='hide'}
# Get the data types of each column
sapply(salary_df, class) 
```

```{r, echo=FALSE, results='hide'}
#-   date has no NA values.
#-   age_group has no NA values. I converted the variable into factor.

#checking unique value in age_group
unique(salary_df$age_group)
# Define the custom order for age_group
age_group_level <- c(
  "under 18", "18-24", "25-34", 
  "35-44", "45-54", "55-64", "65 or over"
  )
salary_df <- salary_df %>%
  mutate(age_group = parse_factor(age_group, 
                            levels = age_group_level,
                            ordered = TRUE))
# Rechecking the order of age_group
levels(salary_df$age_group)
```

```{r, echo=FALSE, results='hide'}

#-   Industry column has 1132 different types lets clean the column by making a new column and club similar industries to 15 broader groups.

# Count the top 20 unique values in the industry column
top_20_industries <- salary_df %>%
  count(industry, sort = TRUE) %>%
  head(20)

# Print the result
print(top_20_industries)
```

```{r, echo=FALSE, results='hide'}
# Handle NA values in 'industry' column
salary_df <- salary_df %>%
  mutate(industry = ifelse(is.na(industry), "other", industry))

# create the 'industry_group' column based on the cleaned 'industry' column
salary_df <- salary_df %>%
  mutate(industry_group = case_when(
    str_detect(industry, regex("health|medical|pharma|biotech|life science|veterinar|childcare|clinical", ignore_case = TRUE)) ~ "Healthcare and Life Sciences",
    str_detect(industry, regex("education|university|librar|research|learning|educat", ignore_case = TRUE)) ~ "Education and Research",
    str_detect(industry, regex("computer|tech|software|saas|telecommunication", ignore_case = TRUE)) ~ "Technology and IT",
    str_detect(industry, regex("nonprofits|social work|charit", ignore_case = TRUE)) ~ "Social work and Nonprofits",
    str_detect(industry, regex("accounting|finance|banking|insurance|investment", ignore_case = TRUE)) ~ "Banking,Finance and Insurance",
    str_detect(industry, regex("government|public|policy|contractor|municipal|lobbying|politic", ignore_case = TRUE)) ~ "Government and Public Services",
    str_detect(industry, regex("consulting|business|.*hr.*|recruitment", ignore_case = TRUE)) ~ "Business and Administrations",
    str_detect(industry, regex(".*pr.*|marketing|advertis|sale", ignore_case = TRUE)) ~ "Marketing and Sales",
    str_detect(industry, regex("legal|law", ignore_case = TRUE)) ~ "Legal",
    str_detect(industry, regex("art|design|media|publishing|performing|film|music|entertainment", ignore_case = TRUE)) ~ "Creative Arts and Design",
    str_detect(industry, regex("manufacturing|aerospace|automotive|production|engineering", ignore_case = TRUE)) ~ "Manufacturing and Engineering",
    str_detect(industry, regex("interior|architec|real estate|construction", ignore_case = TRUE)) ~ "Real estate",
    str_detect(industry, regex("agriculture|environmental|energy|oil|gas|sustainability", ignore_case = TRUE)) ~ "Agriculture and Environmental",
    str_detect(industry, regex("retail|food|beverage|hospitality|restaurant|logistics|transport|consumer|customer|service", ignore_case = TRUE)) ~ "Retail, Hospitality, and Consumer Services",
    TRUE ~ "Other"  # Catch-all for uncategorized industries
  ))
#Here the case_when works

# View the industry_group categories
print(salary_df %>% count(industry_group,sort = TRUE))
# Check the result
unique(salary_df$industry_group)
```

```{r, echo=FALSE, results='hide'}
#-   As there are only 2 NA values, I am replacing them with keyword "Unknown".

# Replace NA values in the 'job_title' column with "Unknown"
salary_df <- salary_df %>%
  mutate(job_title = ifelse(is_na_like(job_title), "Unknown", job_title))
```

```{r, echo=FALSE, results='hide'}
#-   I am not changing the NA values of job_title_context and droping the column as it has around 75% Null values.
salary_df <- salary_df %>%
  select(-job_title_context)
```

```{r, echo=FALSE, results='hide'}
#  There are no NA in annual_salary, but there are 7260 in bonus_pay column. As bonus_pay feature is a variable component of income so it can be 0. So, I am going to replace the null values in this column with 0. Also, replace the two with a new column "gross_income" by adding bonus component to the annual_salary.
```

```{r, echo=FALSE, results='hide'}
    # Replace NA values with 0 and then calculate 'gross_income'
    salary_df <- salary_df %>%
      mutate(gross_income = annual_salary + coalesce(bonus_pay, 0)) %>%
      select(-annual_salary, -bonus_pay)

```

<!-- -   currency column have no null values on the other hand specify_other_currency colummn has only 208 entries so I am going to replace "other" in currency column with values from currency column. -->

```{r, echo=FALSE, results='hide'}
unique(salary_df$currency)
```

```{r, echo= FALSE, results= FALSE}
unique(salary_df$specify_other_currency)
```

```{r, echo=FALSE, results='hide'}
# Step 1: Clean the 'specify_other_currency' column using str_detect
salary_df <- salary_df %>%
  mutate(
    specify_other_currency = case_when(
      str_detect(specify_other_currency, regex("rupees|indian rupees|\\brs\\b|\\binr\\b", ignore_case = TRUE)) ~ "INR",
      str_detect(specify_other_currency, regex("american dollars|us dollars|usd", ignore_case = TRUE)) ~ "USD",
      str_detect(specify_other_currency, regex("\\beur\\b|\\beuros\\b", ignore_case = TRUE)) ~ "EUR",
      str_detect(specify_other_currency, regex("\\bgbp\\b|\\bbritish pounds\\b|\\bpounds\\b", ignore_case = TRUE)) ~ "GBP",
      str_detect(specify_other_currency, regex("\\bcad\\b|\\bcanadian\\b", ignore_case = TRUE)) ~ "CAD",
      # Handle "AUD and NZD" separately to avoid replacing it with just "AUD"
      str_detect(specify_other_currency, regex("aud.*nzd|nzd.*aud", ignore_case = TRUE)) ~ NA_character_,
      str_detect(specify_other_currency, regex("aud|australian dollars", ignore_case = TRUE)) ~ "AUD",
      str_detect(specify_other_currency, regex("nzd", ignore_case = TRUE)) ~ "NZD",
      str_detect(specify_other_currency, regex("php|philippine peso", ignore_case = TRUE)) ~ "PHP",
      str_detect(specify_other_currency, regex("brl|brazilian real|br\\$|brl \\(r\\$\\)", ignore_case = TRUE)) ~ "BRL",
      str_detect(specify_other_currency, regex("cny|rmb|chinese yuan", ignore_case = TRUE)) ~ "CNY",
      str_detect(specify_other_currency, regex("chf|swiss franc", ignore_case = TRUE)) ~ "CHF",
      str_detect(specify_other_currency, regex("jpy|japanese yen", ignore_case = TRUE)) ~ "JPY",
      str_detect(specify_other_currency, regex("zar|south african rand", ignore_case = TRUE)) ~ "ZAR",
      str_detect(specify_other_currency, regex("nok|norwegian kroner", ignore_case = TRUE)) ~ "NOK",
      str_detect(specify_other_currency, regex("myr", ignore_case = TRUE)) ~ "MYR",
      str_detect(specify_other_currency, regex("pln|polish|pln \\(polish zloty\\)", ignore_case = TRUE)) ~ "PLN",
      str_detect(specify_other_currency, regex("mxn|mexican pesos", ignore_case = TRUE)) ~ "MXN",
      str_detect(specify_other_currency, regex("krw|korean won", ignore_case = TRUE)) ~ "KRW",
      str_detect(specify_other_currency, regex("sgd|singapore dollars|singapore dollara", ignore_case = TRUE)) ~ "SGD",
      str_detect(specify_other_currency, regex("sar|saudi riyal", ignore_case = TRUE)) ~ "SAR",
      str_detect(specify_other_currency, regex("ttd", ignore_case = TRUE)) ~ "TTD",
      str_detect(specify_other_currency, regex("czk|czech crowns", ignore_case = TRUE)) ~ "CZK",
      str_detect(specify_other_currency, regex("kwd", ignore_case = TRUE)) ~ "KWD",
      str_detect(specify_other_currency, regex("cop", ignore_case = TRUE)) ~ "COP",
      str_detect(specify_other_currency, regex("br$", ignore_case = TRUE)) ~ "BRL",
      str_detect(specify_other_currency, regex("ils|nis|shekel", ignore_case = TRUE)) ~ "ILS",
      str_detect(specify_other_currency, regex("dkk|danish kroner", ignore_case = TRUE)) ~ "DKK",
      str_detect(specify_other_currency, regex("bdt", ignore_case = TRUE)) ~ "BDT",
      str_detect(specify_other_currency, regex("ars|argentinian peso|argentine peso|peso argentino", ignore_case = TRUE)) ~ "ARS",
      str_detect(specify_other_currency, regex("idr", ignore_case = TRUE)) ~ "IDR",
      str_detect(specify_other_currency, regex("lkr", ignore_case = TRUE)) ~ "LKR",
      str_detect(specify_other_currency, regex("rm|ringgit", ignore_case = TRUE)) ~ "MYR",
      str_detect(specify_other_currency, regex("ntd|new taiwan dollar", ignore_case = TRUE)) ~ "NTD",
      str_detect(specify_other_currency, regex("ngn|nigerian naira", ignore_case = TRUE)) ~ "NGN",
      str_detect(specify_other_currency, regex("pkr|pakistani rupees", ignore_case = TRUE)) ~ "PKR",
      str_detect(specify_other_currency, regex("thb|thai baht", ignore_case = TRUE)) ~ "THB",
      TRUE ~ NA_character_  # Set to NA if no match is found
    )
  )
# Step 2: Replace "Other" in the 'currency' column with the cleaned value from 'specify_other_currency'
salary_df <- salary_df %>%
  mutate(currency = ifelse(currency == "other", specify_other_currency, currency)) %>% 
  select(-specify_other_currency) # Drop the specify_other_currency column
```

```{r, echo=FALSE, results='hide'}
unique(salary_df$currency)
na_rows_in_currency <- salary_df %>%
  filter(is.na(currency))
print(na_rows_in_currency)
```

<!-- -   only 13 rows with not specified currency. I will drop these from the dataset. -->

```{r, echo=FALSE, results='hide'}
salary_df <- salary_df %>%
  filter(!is.na(currency)) %>% 
  mutate(currency = toupper(currency))
```

<!-- -   Now, to keep all the salaries at one scale, I have multiplied the conversion rate of each unique currency to USD. -->

```{r, echo=FALSE, results='hide'}
# Step 1: Define conversion rates to USD. These rates may vary and are upto date till the day of analysis
conversion_rates <- list(
  GBP = 1.22,  # 1 GBP ≈ 1.22 USD
  CAD = 0.74,  # 1 CAD ≈ 0.74 USD
  EUR = 1.05,  # 1 EUR ≈ 1.05 USD
  AUD = 0.64,  # 1 AUD ≈ 0.64 USD
  NZD = 0.59,  # 1 NZD ≈ 0.59 USD
  INR = 0.012, # 1 INR ≈ 0.012 USD
  ARS = 0.003, # 1 ARS ≈ 0.003 USD
  CHF = 1.09,  # 1 CHF ≈ 1.09 USD
  MYR = 0.21,  # 1 MYR ≈ 0.21 USD
  ZAR = 0.054, # 1 ZAR ≈ 0.054 USD
  SEK = 0.09,  # 1 SEK ≈ 0.09 USD
  HKD = 0.13,  # 1 HKD ≈ 0.13 USD
  NOK = 0.093, # 1 NOK ≈ 0.093 USD
  BRL = 0.20,  # 1 BRL ≈ 0.20 USD
  DKK = 0.14,  # 1 DKK ≈ 0.14 USD
  TTD = 0.15,  # 1 TTD ≈ 0.15 USD
  MXN = 0.057, # 1 MXN ≈ 0.057 USD
  CZK = 0.044, # 1 CZK ≈ 0.044 USD
  BDT = 0.009, # 1 BDT ≈ 0.009 USD
  PHP = 0.018, # 1 PHP ≈ 0.018 USD
  PLN = 0.24,  # 1 PLN ≈ 0.24 USD
  CNY = 0.14,  # 1 CNY ≈ 0.14 USD
  ILS = 0.27,  # 1 ILS ≈ 0.27 USD
  JPY = 0.0067, # 1 JPY ≈ 0.0067 USD
  SGD = 0.73,  # 1 SGD ≈ 0.73 USD
  KRW = 0.00075, # 1 KRW ≈ 0.00075 USD
  THB = 0.028, # 1 THB ≈ 0.028 USD
  IDR = 0.000065, # 1 IDR ≈ 0.000065 USD
  LKR = 0.003,  # 1 LKR ≈ 0.003 USD
  SAR = 0.27,  # 1 SAR ≈ 0.27 USD
  NTD = 0.031, # 1 NTD ≈ 0.031 USD
  NGN = 0.0013, # 1 NGN ≈ 0.0013 USD
  COP = 0.00024 # 1 COP ≈ 0.00024 USD
)

# Step 2: Apply conversion to the 'gross_income' column based on the 'currency' column
salary_df <- salary_df %>%
  mutate(
    gross_income_usd = case_when(
      currency == "GBP" ~ gross_income * conversion_rates$GBP,
      currency == "CAD" ~ gross_income * conversion_rates$CAD,
      currency == "EUR" ~ gross_income * conversion_rates$EUR,
      currency == "AUD/NZD" & str_detect(work_country, regex("australia", ignore_case = TRUE)) ~ gross_income * conversion_rates$AUD,
      currency == "AUD/NZD" & str_detect(work_country, regex("new zealand", ignore_case = TRUE)) ~ gross_income * conversion_rates$NZD,
      currency == "INR" ~ gross_income * conversion_rates$INR,
      currency == "ARS" ~ gross_income * conversion_rates$ARS,
      currency == "CHF" ~ gross_income * conversion_rates$CHF,
      currency == "MYR" ~ gross_income * conversion_rates$MYR,
      currency == "ZAR" ~ gross_income * conversion_rates$ZAR,
      currency == "SEK" ~ gross_income * conversion_rates$SEK,
      currency == "HKD" ~ gross_income * conversion_rates$HKD,
      currency == "NOK" ~ gross_income * conversion_rates$NOK,
      currency == "BRL" ~ gross_income * conversion_rates$BRL,
      currency == "DKK" ~ gross_income * conversion_rates$DKK,
      currency == "TTD" ~ gross_income * conversion_rates$TTD,
      currency == "MXN" ~ gross_income * conversion_rates$MXN,
      currency == "CZK" ~ gross_income * conversion_rates$CZK,
      currency == "BDT" ~ gross_income * conversion_rates$BDT,
      currency == "PHP" ~ gross_income * conversion_rates$PHP,
      currency == "PLN" ~ gross_income * conversion_rates$PLN,
      currency == "CNY" ~ gross_income * conversion_rates$CNY,
      currency == "ILS" ~ gross_income * conversion_rates$ILS,
      currency == "JPY" ~ gross_income * conversion_rates$JPY,
      currency == "SGD" ~ gross_income * conversion_rates$SGD,
      currency == "KRW" ~ gross_income * conversion_rates$KRW,
      currency == "THB" ~ gross_income * conversion_rates$THB,
      currency == "IDR" ~ gross_income * conversion_rates$IDR,
      currency == "LKR" ~ gross_income * conversion_rates$LKR,
      currency == "SAR" ~ gross_income * conversion_rates$SAR,
      currency == "NTD" ~ gross_income * conversion_rates$NTD,
      currency == "NGN" ~ gross_income * conversion_rates$NGN,
      currency == "COP" ~ gross_income * conversion_rates$COP,
      TRUE ~ gross_income  # No conversion for missing currencies or those already in USD
    )
  )
```

-   The income_context column have 90% null values. So, I will drop the column.

```{r, echo=FALSE, results='hide'}
salary_df <- salary_df %>%
  select(-income_context)
```

<!-- -   The work_country column has spelling issues. So, I am cleaning possible different spellings of countries. -->

```{r, echo=FALSE, results='hide'}
# Clean and standardize 'work_country' column
salary_df <- salary_df %>%
  mutate(work_country = case_when(
    # United States variations
    str_detect(work_country, regex("u.*n.*i.*t.*e.*d.*s.*a.*t.*e|united states|usa|u.s.|us of a|america|u s a|united sta|uniited states|united stattes|unted states|uniyed states|uniyes states|u.s.|uniter statez|united  states|united statss|untied states|the us|🇺🇸|california|new york|los angeles|chicago|texas|hartford|san francisco|virginia|florida|\\bus\\b|unitedstates|u. s.|u.s|isa|u. s|unitef stated|united y|state|usd", ignore_case = TRUE)) ~ "usa",
    
    # United Kingdom variations
    str_detect(work_country, regex("united kingdom|englan|scotland|britain|u.k.|uk|u.k|london|wales|northern ireland|united kindom|united statues|united sttes|unites kingdom", ignore_case = TRUE)) ~ "uk",
    
    # Canada variations
    str_detect(work_country, regex("canada|csnada|c.*a.*n.*a.*d.*|canda|can", ignore_case = TRUE)) ~ "canada",
    
    # India variations
    str_detect(work_country, regex("india|ind|i.*d.*i.*a.*", ignore_case = TRUE)) ~ "india",
    
    # Australia variations
    str_detect(work_country, regex("australia|australi|sydney|melbourne", ignore_case = TRUE)) ~ "australia",
    
    # Mexico variations
    str_detect(work_country, regex("mexico|méxico|mexico city", ignore_case = TRUE)) ~ "mexico",
    
    # Brazil variations
    str_detect(work_country, regex("brazil|brasil", ignore_case = TRUE)) ~ "brazil",
    
    # Germany variations
    str_detect(work_country, regex("germany|berlin|munich", ignore_case = TRUE)) ~ "germany",
    
    # France variations
    str_detect(work_country, regex("france|paris", ignore_case = TRUE)) ~ "france",
    
    # Netherlands variations
    str_detect(work_country, regex("netherlands|holland|amsterdam|nederland|nl", ignore_case = TRUE)) ~ "netherlands",
    
    # Luxembourg variations
    str_detect(work_country, regex("luxembourg|luxemburg", ignore_case = TRUE)) ~ "luxembourg",

    # Hong Kong variations
    str_detect(work_country, regex("hong kong|hong konh", ignore_case = TRUE)) ~ "hong kong",
    
    
    # Spain variations
    str_detect(work_country, regex("spain|catalonia|madrid|barcelona", ignore_case = TRUE)) ~ "spain",
    
    # Switzerland variations
    str_detect(work_country, regex("switzerland|zurich", ignore_case = TRUE)) ~ "switzerland",
    
    # Sweden variations
    str_detect(work_country, regex("sweden|stockholm", ignore_case = TRUE)) ~ "sweden",
    
    # Denmark variations
    str_detect(work_country, regex("denmark|danmark|copenhagen", ignore_case = TRUE)) ~ "denmark",
    
    # Belgium variations
    str_detect(work_country, regex("belgium|brussels", ignore_case = TRUE)) ~ "belgium",
    
    # Norway variations
    str_detect(work_country, regex("norway|oslo", ignore_case = TRUE)) ~ "norway",
    
    # Finland variations
    str_detect(work_country, regex("finland|helsinki", ignore_case = TRUE)) ~ "ninland",
    
    # New Zealand variations
    str_detect(work_country, regex("new zealand|aotearoa|nz", ignore_case = TRUE)) ~ "new zealand",
    
    # South Africa variations
    str_detect(work_country, regex("south africa|cape town|johannesburg", ignore_case = TRUE)) ~ "south africa",
    
    # Ireland variations
    str_detect(work_country, regex("ireland|dublin", ignore_case = TRUE)) ~ "ireland",
    
    # Puerto Rico variations
    str_detect(work_country, regex("puerto rico", ignore_case = TRUE)) ~ "puerto rico",
    
    # Other countries/regions
    str_detect(work_country, regex("czech republic|czechia", ignore_case = TRUE)) ~ "czech republic",
    str_detect(work_country, regex("thailand", ignore_case = TRUE)) ~ "thailand",
    str_detect(work_country, regex("italy", ignore_case = TRUE)) ~ "italy",
    
    # Catch-all for unknown or unmatched locations
    TRUE ~ work_country  # Keep the original value if no match
  ))
```

<!-- -   I manually identified non-country and then made a vector of the values. Then, I removed them using filter command. -->

```{r, echo=FALSE, results='hide'}
# List of non-country values (manually identified)
non_countries <- c("contracts", "global", "currently finance", "uxz", 
                   "$2,175.84/year is deducted for benefits", "remote", 
                   "remote (philippines)", "international", "n/a (remote from wherever i want)", 
                   "y", "na", "policy", "ss", "dbfemf", "loutreland", "ff","i.s.","is","ua", "u.a.", "bonus based on meeting yearly goals set w/ my supervisor")

# Filter out the rows where 'work_country' contains non-country values
salary_df <- salary_df %>%
  filter(!(work_country %in% non_countries))
```

```{r, echo=FALSE, results='hide'}
unique(salary_df$work_country)
```

<!-- -  I am clubbing various countries to their continents -->

```{r, echo=FALSE, results='hide'}
# Step 1: Create a mapping of countries to continents
country_to_continent <- c(
  # North America
  "usa" = "North America",
  "canada" = "North America",
  "mexico" = "North America",
  "puerto rico" = "North America",
  "bermuda" = "North America",
  "the bahamas" = "North America",
  "trinidad and tobago" = "North America",
  
  # Europe
  "uk" = "Europe",
  "netherlands" = "Europe",
  "spain" = "Europe",
  "france" = "Europe",
  "germany" = "Europe",
  "ireland" = "Europe",
  "denmark" = "Europe",
  "switzerland" = "Europe",
  "belgium" = "Europe",
  "sweden" = "Europe",
  "norway" = "Europe",
  "greece" = "Europe",
  "austria" = "Europe",
  "hungary" = "Europe",
  "luxembourg" = "Europe",
  "latvia" = "Europe",
  "czech republic" = "Europe",
  "poland" = "Europe",
  "italy" = "Europe",
  "romania" = "Europe",
  "serbia" = "Europe",
  "slovenia" = "Europe",
  "slovakia" = "Europe",
  "portugal" = "Europe",
  "malta" = "Europe",
  "bulgaria" = "Europe",
  "estonia" = "Europe",
  "croatia" = "Europe",
  "liechtenstein" = "Europe",
  "isle of man" = "Europe",
  "europe" = "Europe", # General mapping for Europe
  
  # Oceania
  "australia" = "Oceania",
  "new zealand" = "Oceania",

  # South America
  "argentina" = "South America",
  "brazil" = "South America",
  "colombia" = "South America",
  "chile" = "South America",
  "uruguay" = "South America",
  "ecuador" = "South America",
  "panamá" = "South America",
  "costa rica" = "South America",
  
  # Asia
  "hong kong" = "Asia",
  "kuwait" = "Asia",
  "japan" = "Asia",
  "sri lanka" = "Asia",
  "china" = "Asia",
  "israel" = "Asia",
  "taiwan" = "Asia",
  "cambodia" = "Asia",
  "vietnam" = "Asia",
  "singapore" = "Asia",
  "south korea" = "Asia",
  "thailand" = "Asia",
  "myanmar" = "Asia",
  "burma" = "Asia",
  "pakistan" = "Asia",
  "bangladesh" = "Asia",
  "india" = "Asia",
  "afghanistan" = "Asia",
  "jordan" = "Asia",
  "uae" = "Asia",
  "qatar" = "Asia",
  "saudi arabia" = "Asia",
  "cyprus" = "Asia",
  
  # Africa
  "south africa" = "Africa",
  "nigeria" = "Africa",
  "rwanda" = "Africa",
  "morocco" = "Africa",
  "ghana" = "Africa",
  "eritrea" = "Africa",
  "somalia" = "Africa",
  "cote d'ivoire" = "Africa",
  "sierra leone" = "Africa",
  "zimbabwe" = "Africa",
  "uganda" = "Africa",
  "kenya" = "Africa",
  "congo" = "Africa",
  "africa" = "Africa", # General Africa mapping
  
  # Miscellaneous
  "jamaica" = "North America",
  "usd" = "Unclassified", 
  "bermuda" = "North America",
  "malaysia" = "Asia",
  "philippines" = "Asia",
  "lithuania" = "Europe",
  "eritrea" = "Africa",
  "cuba" = "North America",
  "isle of man" = "Europe",
  "somalia" = "Africa",
  "rwanda" = "Africa",
  "burma" = "Asia",
  "sierra leone" = "Africa",
  "zimbabwe" = "Africa",
  "ghana" = "Africa",
  "qatar" = "Asia",
  "kenya" = "Africa",
  "jordan" = "Asia",
  "cyprus" = "Asia",
  "liechtenstein" = "Europe",
  "myanmar" = "Asia"
)
salary_df <- salary_df %>%
  mutate(continent = country_to_continent[tolower(work_country)]) 
unique(salary_df$continent)
```

<!-- -   us_state column appears to be clean with few NA values. I will clean NA value of this column in case i decide to plot geographical distribution of salary in us later in my analysis. -->

## Continued

-   I am not using city column in my analysis so I am dropping the column.

```{r, echo=FALSE, results='hide'}
# Drop the 'city' column from salary_df
salary_df <- salary_df %>%
  select(-city)
```

<!-- -   overall_work_experience and field_work_experience has no NA values. Lets convert it into ordered factor. -->

```{r, echo=FALSE, results='hide'}
unique(salary_df$overall_work_experience)
unique(salary_df$field_work_experience)
```

```{r, echo=FALSE, results='hide'}
# Define the levels in a logical order for 'overall_work_experience' and 'field_work_experience'
experience_levels <- c(
  "1 year or less", 
  "2 - 4 years", 
  "5-7 years", 
  "8 - 10 years", 
  "11 - 20 years", 
  "21 - 30 years", 
  "31 - 40 years", 
  "41 years or more"
)

# Clean and convert both 'overall_work_experience' and 'field_work_experience' to ordered factors
salary_df <- salary_df %>%
  mutate(
    overall_work_experience = str_trim(overall_work_experience),  # Remove extra spaces
    overall_work_experience = parse_factor(overall_work_experience, 
                                           levels = experience_levels, 
                                           ordered = TRUE),
    field_work_experience = str_trim(field_work_experience),  # Remove extra spaces
    field_work_experience = parse_factor(field_work_experience, 
                                         levels = experience_levels, 
                                         ordered = TRUE)
  )
```

```{r, echo=FALSE, results='hide'}
# Lets check if there are any cases where the overall work experience is less than field work experience.

# Filter the rows where overall work experience is less than field work experience
invalid_experience_rows <- salary_df %>%
  filter(overall_work_experience < field_work_experience)

# Display the rows
print(invalid_experience_rows)
```

<!-- -   In real world this is not possible so I am removing these 257 rows. -->

```{r, echo=FALSE, results='hide'}
# Remove the invalid rows from the dataset
salary_df <- salary_df %>%
  filter(!(overall_work_experience < field_work_experience))
```

<!-- -   In the highest_education column there are levels of education with 215 null values.  -->

```{r, echo=FALSE, results='hide'}

# As our focus of analysis is on salary variation in industry, time and geography. lets not drop the columns and just name NA as unknown and in the level of education I have kept Unknown at lowest level.

unique(salary_df$highest_education)
na_rows_in_higher_education <- salary_df %>%
  filter(is.na(highest_education))
print(na_rows_in_higher_education)
```

```{r, echo=FALSE, results='hide'}
# Define the cleaning and grouping for the 'higher_education' column
salary_df <- salary_df %>%
  mutate(highest_education = case_when(
    str_detect(highest_education, regex("phd|doctorate", ignore_case = TRUE)) ~ "PhD",
    str_detect(highest_education, regex("professional degree|md|jd", ignore_case = TRUE)) ~ "Professional Degree",
    str_detect(highest_education, regex("master", ignore_case = TRUE)) ~ "Master's Degree",
    str_detect(highest_education, regex("college degree", ignore_case = TRUE)) ~ "College Degree",
    str_detect(highest_education, regex("some college", ignore_case = TRUE)) ~ "Some College",
    str_detect(highest_education, regex("high school", ignore_case = TRUE)) ~ "High School",
    is.na(highest_education) ~ "Unknown",  # Handling missing values
    TRUE ~ highest_education  # For other or unrecognized values
  ))

# Optionally, convert to an ordered factor
education_levels <- c("Unknown","High School", "Some College", "College Degree", "Master's Degree", "Professional Degree", "PhD")
salary_df <- salary_df %>%
  mutate(highest_education = factor(highest_education, levels = education_levels, ordered = TRUE))

# View the cleaned unique values
unique(salary_df$highest_education)
```

```{r, echo=FALSE, results='hide'}
# Cleaning gender column where i clubbed null values into prefer not to answer along with other gender.
unique(salary_df$gender)
```

```{r, echo=FALSE, results='hide'}
salary_df <- salary_df %>%
  mutate(gender = case_when(
    str_detect(gender, regex("woman", ignore_case = TRUE)) ~ "Woman",
    str_detect(gender, regex("man", ignore_case = TRUE)) ~ "Man",
    str_detect(gender, regex("non-binary", ignore_case = TRUE)) ~ "Non-binary",
    str_detect(gender, regex("prefer not to answer|other", ignore_case = TRUE)) ~ "Prefer not to answer",
    is.na(gender) ~ "Prefer not to answer",  # Replace NA with "Prefer not to answer"
    TRUE ~ gender  # Keep any other values as they are
  ))
```

```{r, echo=FALSE, results='hide'}
# Cleaning Race column, as its not focus of our analysis, So, I am introducing "mixed race" in place of people belonging multiple race. Also, null values to prefer not to answer.

unique(salary_df$race)
# Count the occurrences of each unique value in the race column
race_count <- salary_df %>%
  count(race)

# View the result
print(race_count)
```

```{r, echo=FALSE, results='hide'}
# Define race categories and patterns
race_patterns <- list(
  "White" = "white",
  "Asian or Asian American" = "asian|asian american",
  "Black or African American" = "black|african american",
  "Native American or Alaska Native" = "native american|alaska native",
  "Middle Eastern or Northern African" = "middle eastern|northern african",
  "Hispanic, Latino, or Spanish Origin" = "hispanic|latino|spanish origin",
  "Prefer not to answer" = "another option not listed here|prefer not to answer"
)

# Helper function to classify race
classify_race <- function(race_string) {
  if (is.na(race_string)) {
    return("Prefer not to answer")
  }
  
  race_matches <- sapply(race_patterns, function(pattern) str_detect(race_string, regex(pattern, ignore_case = TRUE)))
  matched_races <- names(race_matches[race_matches == TRUE])
  
  # Mixed Race if more than one race is detected
  if (length(matched_races) > 1) {
    return("Mixed Race")
  } else if (length(matched_races) == 1) {
    return(matched_races)
  } else {
    return("Prefer not to answer")  # If no match is found
  }
}

# Apply classification to the race column
salary_df <- salary_df %>%
  mutate(race = sapply(race, classify_race))
```

```{r, echo=FALSE, results='hide'}

##Univariate analysis for gross_salary_usd variable

###Initial summary and Outliers in Currency variables
summary(salary_df$gross_income_usd)
```

<!-- ## Outliers -->

```{r}
# Create a box plot to check for outliers in 'gross_income_usd'
ggplot(salary_df, aes(x = "", y = gross_income_usd)) +
  geom_boxplot(fill = "lightblue", notch = TRUE, outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Highlight outliers
  labs(title = "Box Plot Distribution of Gross Salary (USD)", y = "Gross Salary (USD)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centering the title
```

<!-- There is clearly some strong outlier first lets check for top 20 values these. -->

```{r, echo=FALSE, results='hide'}
# Finding the top 20 gross salaries in the dataset
top_20_gross_salaries <- salary_df %>%
  arrange(desc(gross_income_usd)) %>%
  head(20)

# Display the result
print(top_20_gross_salaries)
```

```{r, echo=FALSE, results='hide'}
# -   From these its clear, that the top most salary 4.4 billion usd that too with 1 year of field experience. But when we see in the gross_income variable the person made a typo as they must wanted to put 60000 to 70000 canadian dollars.
# -   In the second row as well the person may have written wrong currency as he works in Columbia.
# -   In the 3rd row the age_group is 18-24 where as the overall_work_experience is 41 years or more. Also Job title is Bum which clearly indicate garbage value. Lets drop this row.
# -   In 5 ths row the person is student. lets handle these one by one.

```

```{r, echo=FALSE, results='hide'}

# using conversion factor from cad to usd replacing the the value with 70000cad in top most value
salary_df <- salary_df %>%
  mutate(gross_income_usd = ifelse(gross_income_usd == 4440051800, 70000 * 0.75, gross_income_usd))
# in this person belongs to columbia so using conversion factor to replace pesos to usd
salary_df <- salary_df %>%
  mutate(gross_income_usd = ifelse(gross_income_usd == 102000000, gross_income_usd * 0.00025, gross_income_usd))
# dropping the garbage value
salary_df <- salary_df %>%
  filter(gross_income_usd != 10000299)
# dropping the row as student of 18 earning 5 million dollar
salary_df <- salary_df %>%
  filter(gross_income_usd != 5000040)
```

```{r, echo=FALSE, results='hide'}
# Lets check for least values too

# Finding the least 20 gross salaries in the dataset
least_20_gross_salaries <- salary_df %>%
  arrange(gross_income_usd) %>%
  head(40)

# Display the result
print(least_20_gross_salaries)
```

```{r, echo=FALSE, results='hide'}
# -   some of them realistic as people are unemployed, stay at home, or student but some of the people are well experienced still earning like 40, 50 usd per anum. I will assume that they are reflecting there salary as 40k or 50k. To work around this fact i will multiply values below 150 with 1000 while checking if they are student, or stay at home or unemployed or working for non profits.
```

```{r, echo=FALSE, results='hide'}
# Adjust salaries below 150 to assume they represent thousands for specific conditions
salary_df <- salary_df %>%
  mutate(
    gross_income_usd = case_when(
      # Check if gross_salary_usd is less than 150 and does not meet conditions (student, stay-at-home, etc.)
      gross_income_usd < 150 & 
      !str_detect(job_title, regex("student|stay[- ]at[- ]home|unemployed", ignore_case = TRUE)) &
      !str_detect(industry, regex("non[- ]profit|nonprofits", ignore_case = TRUE)) ~ gross_income_usd * 1000,
      
      # Keep the original salary if conditions don't match
      TRUE ~ gross_income_usd
    )
  )
```

```{r, echo=FALSE, results='hide'}
#   Now using Interquartile Range and checking for few keywords in job_title and judging the work experience. I am removing around 825 outliers outliers
```

```{r, echo=FALSE, results='hide'}
# Step 1: Calculate the IQR and the bounds for detecting outliers
iqr_values <- IQR(salary_df$gross_income_usd, na.rm = TRUE)
q1 <- quantile(salary_df$gross_income_usd, 0.25, na.rm = TRUE)  # First quartile (Q1)
q3 <- quantile(salary_df$gross_income_usd, 0.75, na.rm = TRUE)  # Third quartile (Q3)
lower_bound <- q1 - 1.5 * iqr_values
upper_bound <- q3 + 1.5 * iqr_values

# Print the IQR, Q1, Q3, lower and upper bounds
print(paste("Q1 (25th percentile):", q1))
print(paste("Q3 (75th percentile):", q3))
print(paste("IQR:", iqr_values))
print(paste("Lower bound:", lower_bound))
print(paste("Upper bound:", upper_bound))

# Step 2: Define experience levels
low_experience_terms <- c("1 year or less", "2 - 4 years")
high_experience_terms <- c("21 - 30 years", "31 - 40 years", "41 years or more")

# Step 3: Define additional keywords for lower bound exceptions
low_bound_keywords <- c("student", "intern", "stay at home", "unemployed", "part-time", "volunteer")

# Step 4: Identify rows with outliers based on the bounds
outliers <- salary_df %>%
  filter(gross_income_usd < lower_bound | gross_income_usd > upper_bound)

# Step 5: Check for keywords in 'job_title' and 'industry', and experience level
# Keep outliers if they are executives or have extensive experience
key_executive_terms <- c("ceo", "owner", "founder", "executive", "director", "president", "partner", "head", "lead")

# Step 6: Identify valid outliers
valid_outliers <- outliers %>%
  filter(
    str_detect(tolower(job_title), paste(key_executive_terms, collapse = "|")) |  # Executives
    (overall_work_experience %in% high_experience_terms & gross_income_usd > upper_bound) |  # Allow high experience for upper bound
    (overall_work_experience %in% low_experience_terms & gross_income_usd < lower_bound) |  # Allow low experience for lower bound
    str_detect(tolower(job_title), paste(low_bound_keywords, collapse = "|"))  # Allow keywords for low bound
  )

# Step 7: Identify outliers to be removed (those that are not valid outliers)
outliers_to_remove <- outliers %>%
  filter(!(
    str_detect(tolower(job_title), paste(key_executive_terms, collapse = "|")) |
    (overall_work_experience %in% high_experience_terms & gross_income_usd > upper_bound) |
    (overall_work_experience %in% low_experience_terms & gross_income_usd < lower_bound) |
    str_detect(tolower(job_title), paste(low_bound_keywords, collapse = "|"))
  ))

# Print the number of outliers that will be removed
print(paste("Number of outliers to be removed:", nrow(outliers_to_remove)))

# Step 8: Remove the outliers that are not valid
salary_df <- salary_df %>%
  filter(
    (gross_income_usd >= lower_bound & gross_income_usd <= upper_bound) |  # Keep non-outliers
    str_detect(tolower(job_title), paste(key_executive_terms, collapse = "|")) |  # Keep valid outliers (executives)
    (overall_work_experience %in% high_experience_terms & gross_income_usd > upper_bound) |  # Keep valid outliers (high experience)
    (overall_work_experience %in% low_experience_terms & gross_income_usd < lower_bound) |  # Keep valid outliers (low experience)
    str_detect(tolower(job_title), paste(low_bound_keywords, collapse = "|"))  # Keep valid low-bound outliers (students, unemployed, etc.)
  )
```

̛

```{r, echo=FALSE, results='hide'}
str(salary_df)
```

## Which industry or industries have the highest/lowest salaries?

```{r,fig.height=8, fig.width=12}
# Grouping data by industry and calculating the mean salary
salary_by_industry <- salary_df %>%
  group_by(industry_group) %>%
  summarize(Avg_salary = mean(gross_income_usd, na.rm = TRUE)) %>%
  arrange(desc(Avg_salary))

# Use a color palette for bars
industry_colors <- c("Education and Research" = "#1f77b4",
                     "Technology and IT" = "#ff7f0e",
                     "Healthcare and Life Sciences" = "#2ca02c",
                     "Banking,Finance and Insurance" = "#d62728",
                     "Government and Public Services" = "#9467bd",
                     "Marketing and Sales" = "#8c564b",
                     "Legal" = "#e377c2",
                     "Creative Arts and Design" = "#7f7f7f",
                     "Business and Administrations" = "#bcbd22",
                     "Nonprofits" = "#17becf",
                     "Manufacturing and Engineering" = "#aec7e8",
                     "Retail, Hospitality, and Consumer Services" = "#ffbb78",
                     "Environmental and Sustainability" = "#98df8a",
                     "Other" = "#c7c7c7")

# Plot Industry vs Average Salary with colorful bars and labels centered inside
ggplot(salary_by_industry, aes(x = reorder(industry_group, Avg_salary), y = Avg_salary, fill = industry_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Avg_salary, 0)), 
            color = "black", 
            size = 3, 
            vjust = 0.5,  # Center the text in the middle of the bar
            hjust = 1) +  # Horizontally center the text
  scale_fill_manual(values = industry_colors) +  # Apply the color palette
  coord_flip() +
  labs(title = "Insights on Salaries among various industry groups", 
       subtitle = "Technology and legal industries offer the highest average salaries, while Nonprofits and Education lags behind", 
       x = "Industry", 
       y = "Gross Salary (USD)",
       caption = "Data sourced from Ask_a_manager dataset") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "none"  # Remove legend
  )
```

## Insights Graph 1
-   Top Salary Industries:
    -   Technology and IT leads with the highest average salary of approximately USD 120,000. This indicates that professionals in technology-related roles are among the highest earners across industries.
    -   Legal industry also has a high average salary of \$106,000, reflecting the lucrative nature of law-related professions.
    -   Manufacturing and Engineering and Business and Administrations follow closely, with average salaries around USD 96,000.

## Insights Graph 1
-   Mid-range Salary Industries:
    -   Banking, Finance, and Insurance (USD 94,500), Healthcare and Life Sciences (USD 91,499), and Marketing and Sales (USD 87,600) have fairly competitive salaries, representing sectors with substantial compensation for experienced professionals.
    -   These industries exhibit stable demand for skilled workers, contributing to the higher salary ranges.

## Insights Graph 1
-   Lower Salary Industries:
    -   Industries such as Retail, Hospitality, and Consumer Services (USD 69,585), Social Work and Nonprofits (USD 69,046), and Education and Research (USD 65,075) are on the lower end of the salary spectrum.
    -   These sectors are often service-oriented or mission-driven, where salaries tend to be lower compared to industries like tech, law, or finance.

## Which industries have the highest salary variability?

```{r,fig.height=8, fig.width=12}
# Violin plot to show salary distribution by industry
ggplot(salary_df, aes(x = reorder(industry_group, -gross_income_usd, FUN = median), y = gross_income_usd)) +
  geom_violin(fill = "lightblue") +
  coord_flip() +
  labs(title = "Distribution of Income in various Industry Groups", 
       subtitle = "Technology and IT industry has the widest Industries like Social Work and Nonprofits show much narrower distributions ",
       x = "Industry", 
       y = "Gross Salary (USD)",
       caption = "Data sourced from Ask_a_manager dataset") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.caption = element_text(hjust = 1, vjust = -0.5, size = 10, face = "italic")
  )
```

## Insight Graph 2
-   Industry with Highest Salary Variability:

    -   The Technology and IT industry has the widest distribution with a very long upper tail, indicating some very high earners in this field (potentially including tech executives or specialists).
    -   Real Estate shows a more constrained salary distribution with fewer people earning high salaries.
    -   Banking, Finance, and Insurance also has a wide distribution, indicating high variability in salaries.

## Insight Graph 2
-   Industries with Lower Salary Variability:

    -   Industries like Social Work and Nonprofits, Retail, Hospitality, and Consumer Services, and Education and Research show much narrower distributions, suggesting less variability in salaries within these sectors. This likely indicates that salaries are more uniform, with fewer outliers.

-   Healthcare and Life Sciences, Technology and IT, and Banking, Finance, and Insurance have wide central bulges that indicate higher median salaries compared to industries like Creative Arts and Design or Social Work and Nonprofits.

## How do salaries vary over time and geography?

```{r,fig.height=8, fig.width=12}
# Dot plot with jitter, gradient colors, and hidden x-axis labels
ggplot(salary_df, aes(x = reorder(continent, -gross_income_usd, FUN = median), y = gross_income_usd, color = continent)) +
  geom_jitter(size = 3, alpha = 0.6, width = 0.3, height = 0) +  # Add jitter with width
  coord_flip() +
  labs(title = "Salary Variation by different geographical locations",
       subtitle = "Spread of salary is clearly visible in North America and Europe",
       x = "Continents", 
       y = "Gross Salary (USD)",
       caption = "Data sourced from Ask_a_manager dataset") +
  scale_color_manual(values = c("Europe" = "blue", "North America" = "green", "Asia" = "red", "Africa" = "purple", "Oceania" = "orange", "South America" = "cyan")) +
  scale_y_continuous() +  # Customize y-axis if needed
  scale_x_discrete(labels = NULL) +  # Remove x-axis labels (continent names will still appear in the plot itself)
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Rotate y-axis labels (since axes are flipped)
    axis.text.y = element_text(size = 10),  # y-axis labels (x-axis labels after flip) will remain hidden
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
   plot.caption = element_text(hjust = 1, vjust = -0.5, size = 10, face = "italic")
  )
```

## Insight Graph 3
-   North America exhibits the widest salary distribution, with both high and low salaries, while Europe shows a more concentrated salary distribution.
-   The clustering of points around lower salary ranges in regions like Africa and South America might reflect general economic conditions or lower wage levels in those regions compared to North America or Europe.

## Variations of Salary with experience
```{r,fig.height=8, fig.width=12}
# Calculate the average salary by overall work experience
salary_by_overall_experience <- salary_df %>%
  group_by(overall_work_experience) %>%
  summarize(Avg_salary_overall = mean(gross_income_usd, na.rm = TRUE))

# Calculate the average salary by field work experience
salary_by_field_experience <- salary_df %>%
  group_by(field_work_experience) %>%
  summarize(Avg_salary_field = mean(gross_income_usd, na.rm = TRUE))

# Plot 1: Salary trends based on overall work experience (p1)
p1 <- ggplot(salary_by_overall_experience, aes(x = overall_work_experience, y = Avg_salary_overall)) +
  geom_line(size = 1, color = "darkblue") +
  geom_point(size = 2, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "red", size = 1) +  # Add trend line
  labs(title = "Average income trend with number of years of Overall Work Experience",
       x = "Overall Work Experience",
       y = "Average Gross Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Plot 2: Salary trends based on field work experience (p2)
p2 <- ggplot(salary_by_field_experience, aes(x = field_work_experience, y = Avg_salary_field)) +
  geom_line(size = 1, color = "red") +
  geom_point(size = 2, color = "red") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "red", size = 1) +  # Add trend line
  labs(title = "Average income trend with number of years of Field Work Experience",
       caption = "These plots show that with years of experience there is direct correlation of income increase with dip after 21 - 30 years of experience in both groups", hjust = -1,
       x = "Field Work Experience",
       y = "Average Gross Salary (USD)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
    
  )

p1+p2
```
## Insight Graph 4
-   Both overall and field work experience show consistent salary increases up to 20 years, with significant jumps between 5-10 and 10-20 years of experience.
-   Salaries tend to dip after 21-30 years of experience, possibly due to transitions into advisory or less hands-on roles.
-   Salary growth is slightly higher for overall experience compared to field-specific experience, indicating broader career experience may be more valued than deep specialization in certain cases.
