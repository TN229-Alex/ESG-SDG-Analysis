# ESG-SDG-Analysis
This project analyzes ESG &amp; SDG in 217 sustainability reports to extract SDG-aligned themes and sentiment insights.

> Note: Raw input data is not included due to size and source limitations.

## Tools & Libraies
This analysis was developed in **R** using the following key packages:
- `tidyverse` – Core data manipulation and visualization (includes `dplyr`, `ggplot2`)
- `data.table` – Fast handling of large text data
- `quanteda` – Tokenization, DFM creation, and advanced text analysis
- `parallel` + `doParallel` – For scalable processing of 1700+ files
- `openxlsx2` – For reading and writing Excel files
- `reshape2` – Reshaping data frames for aggregation and plotting

## Outputs
SDG/ESG coverage in excel data.

## Note
This project is for educational/demo purposes. Data has been anonymized.
