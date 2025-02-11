# 📊 Salary Data Analysis Project

## 🎯 Project Overview
This project analyzes salary distributions across different industries, investigating salary variability and geographical patterns. The analysis focuses on understanding compensation trends and factors that influence salary levels on Ask_a_manager dataset for salary survey.

## 🔍 Key Research Questions
- Which industries have the highest/lowest salaries?
- Which industries show the highest salary variability?
- How do salaries vary across different geographical locations?
- What is the relationship between experience and salary?

## 📦 Dependencies
- tidyverse
- dplyr
- readxl
- lubridate
- patchwork

## 🔧 Data Processing Steps
1. Data Cleaning
   - Standardized currency conversions to USD
   - Handled missing values
   - Removed duplicate entries
   - Normalized country names and grouped into continents
   - Created industry groupings from detailed categories

2. Feature Engineering
   - Combined annual salary and bonus into gross income
   - Standardized experience levels
   - Grouped industries into broader categories
   - Normalized geographical data

## 📈 Key Findings

### Industry Salary Analysis
- **Highest Paying Industries:**
  - Technology & IT ($120,000 avg)
  - Legal ($106,000)
  - Manufacturing & Engineering ($96,000)
  
- **Lower Paying Industries:**
  - Retail & Hospitality ($69,585)
  - Social Work & Nonprofits ($69,046)
  - Education & Research ($65,075)

### Salary Variability
- Technology and IT shows the widest salary distribution
- Banking and Finance displays high variability
- Nonprofit and Education sectors show more uniform distributions

### Geographical Patterns
- North America shows the widest salary range
- Europe displays more concentrated salary distributions
- Lower salary clusters in Africa and South America
- Significant regional economic disparities reflected in salary ranges

### Experience Impact
- Consistent salary increases up to 20 years experience
- Peak earnings typically between 10-20 years
- Slight decline after 21-30 years experience
- General experience valued more than specialized experience

## 📊 Visualizations
The project includes four key visualizations:
1. Industry Average Salaries (Bar Chart)
   ![image](https://github.com/user-attachments/assets/f0db5583-c1a5-4b60-af80-efaa194ac847)
3. Salary Distribution by Industry (Violin Plot)
   ![image](https://github.com/user-attachments/assets/ba71cb26-8c2f-4dc6-9884-787766f552e3)
5. Geographical Salary Distribution (Dot Plot)
   ![image](https://github.com/user-attachments/assets/36d479a5-0cd0-4718-9016-0f7708de3952)
7. Experience vs. Salary Trends (Line Plots)
   ![image](https://github.com/user-attachments/assets/512d2769-f198-48b3-a957-33017c594579)


## 💡 Insights & Implications
- Technology sector leads in both average salary and variability
- Clear geographical salary disparities exist globally
- Experience shows strong correlation with salary growth
- Industry choice significantly impacts earning potential
- Salary structures vary significantly between public and private sectors

## 🚀 Usage
```R
# Install required packages
install.packages(c("tidyverse", "dplyr", "readxl", "lubridate", "patchwork"))

# Load libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(patchwork)
```

## 📄 License
This project is licensed under the MIT License - see the LICENSE file for details.

## 👥 Author
Arpan Sharma

## 🙏 Acknowledgments
This dataset is provided by Prof. Justin Slater from from [here](https://www.askamanager.org/) as part of coursework for Data*6200- Data Manipulation and Data Visualization.
