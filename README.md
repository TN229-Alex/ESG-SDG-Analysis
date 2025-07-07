# ESG-SDG-Analysis
This project analyzes sustainability reports from global companies to identify sentiment and alignment with the UN Sustainable Development Goals (SDGs). Using a custom SDG keyword dictionary and sentiment analysis methods, the goal was to explore how companies disclose their Environmental, Social, and Governance (ESG) commitments and whether their language is actionable or vague.

> Note: Raw input data is not included due to size and source limitations.

## Dataset
- **Total reports:** 5,790
- **Filtered for analysis:** 1,653 English-language reports and then to 217 reports to specific industry
- **Years covered:** 2001–2018
- **Industries & regions:** Extractives and Minerals Processing sector

## Tools & Libraies
This analysis was developed in **R** using the following key packages:
- `tidyverse` – Core data manipulation and visualization (includes `dplyr`, `ggplot2`)
- `data.table` – Fast handling of large text data
- `quanteda` – Tokenization, DFM creation, and advanced text analysis
- `parallel` + `doParallel` – For scalable processing of 1700+ files
- `openxlsx2` – For reading and writing Excel files
- `reshape2` – Reshaping data frames for aggregation and plotting

## Method
1. **Text Loading & Cleaning**
   - Reports were read and preprocessed to remove punctuation, numbers, and stopwords for consistency.

2. **Keyword Dictionary Creation**
   - A custom SDG dictionary was developed using official UN and GRI terminology, assisted by ChatGPT.
   
3. **Keyword Matching & Normalization**
   - Keyword frequencies were counted and normalized across reports of varying lengths to ensure fair weighting.

4. **TF-IDF Scoring**
   - Term Frequency–Inverse Document Frequency was applied to identify impactful terms across reports.
   - This highlighted SDGs that are commonly prioritized or emphasized in disclosures.

5. **Correlation Analysis**
   - A correlation matrix of TF-IDF scores was built to examine the relationship between ESG dimensions and specific SDGs.
   - This allowed identification of SDGs that are closely linked to GRI reporting themes.

6. **Output Generation**
   - Summarized Excel output files were created to show:
     - Keyword counts by SDG and region
     - Sentiment scores by ESG dimension
     - Actionability flags (vague vs. concrete language)

## Note
This project is for educational/demo purposes. Data has been anonymized.
