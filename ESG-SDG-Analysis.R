###############################################################################
#           Analysis of CSR Reports for SDG Priorities & ESG Alignments
#                 Research questions & text analytics in R                 
# --------------------------------------------------------------------------- #
# - Research Question 1:
#   How do different sustainability goals differ across regions and sectors?
# - Research Question 2:
#   Which SDGs are closer to GRI disclosures under Environment, Social, and Governance Reporting Framework?
###############################################################################

###############################################################################
# STEP 1: PROJECT & ENVIRONMENT SETUP
# - 1.1 Load/Install Packages
# - 1.2 Set Working Directory & Project Paths
# - 1.3 Load & Inspect Data
# - 1.4 Subset to Matched "English" Reports
###############################################################################

## 1.1 Load/Install Packages ----
required_packages <- c(
  "tidyverse",   # Core data handling & visualization (includes ggplot2, dplyr)
  "data.table",  # Big data processing
  "parallel",    # Parallel processing
  "doParallel",  # Parallel backend
  "quanteda",    # Advanced text analysis (superior to basic tm)
  "openxlsx2",   # Excel file reading
  "reshape2"     # Data reshaping
)

install_missing <- required_packages[!required_packages %in% installed.packages()]
if(length(install_missing)) install.packages(install_missing)

# Load the libraries
library(tidyverse)
library(data.table)
library(parallel)
library(doParallel)
library(quanteda)
library(openxlsx2)
library(reshape2)

## 1.2 Set Working Directory & Paths ----
# (Adjust paths as appropriate for your local environment)
text_files_path <- "~/Project/txt_converted"

## 1.3 Load & Inspect Data ----
sustainability_df <- readRDS("DAV_assignment.rds")

# Quick checks
head(sustainability_df)
summary(sustainability_df)
str(sustainability_df)

## 1.4 Subset to Matched "English" Reports ----
# Filter only those that are labeled as "english"
english_reports <- sustainability_df %>%
  filter(english_non_english == "english")

# List text files actually present on disk
text_files <- list.files(text_files_path, pattern = "*.txt", full.names = FALSE)
text_files_without_ext <- tools::file_path_sans_ext(text_files)

# Create a small dataframe for file matching
text_files_df <- data.frame(
  text_file       = text_files, 
  text_file_base  = text_files_without_ext
)

# Final "matched_reports": English-only records whose file names match the .txt files
matched_reports <- english_reports %>%
  filter(file %in% text_files_df$text_file_base)

# Confirm match
dim(matched_reports)
head(matched_reports)

# Quick summary by region
matched_reports %>%
  group_by(Region) %>%
  summarise(Count = n())

###############################################################################

###############################################################################
# STEP 2: EXPLORATORY ANALYSIS (EDA)
# - 2.1 Summaries & Visualizations with "matched_reports"
# - 2.2 Focus on "Extractives & Minerals Processing" Sector
###############################################################################

## 2.1 Summaries & Bar Plot by Region ----

# Count how many matched reports per region
region_counts <- matched_reports %>%
  group_by(Region) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Basic bar chart: number of matched reports by Region
ggplot(region_counts, aes(
  x    = fct_reorder(Region, Count, .desc = TRUE),
  y    = Count,
  fill = Region
)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, color = "black") +
  geom_text(aes(label = Count), vjust = -0.5, size = 2.5) +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
    plot.title       = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.title     = element_text(face = "bold"),
    legend.position  = "right"
  ) +
  labs(
    title = "Count of Matched 'English Reports' by Region",
    x     = "Region", 
    y     = "Count"
  ) +
  guides(fill = guide_legend(reverse = FALSE, title = "Region")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))

## 2.2 Focus on "Extractives & Minerals Processing" Sector ----
# Filter to see how many matched reports fall into the "Extractives and Minerals Processing" category
energy_sector_reports <- matched_reports %>%
  filter(Sec_SASB == "Extractives and Minerals Processing") %>%
  group_by(Region, Sector) %>%
  mutate(Reports = n()) %>%
  ungroup()

head(energy_sector_reports, 10)
dim(energy_sector_reports)

# Bar chart: how many matched reports in "Extractives and Minerals Processing," by region
ggplot(energy_sector_reports, aes(x = Sector, y = Reports, fill = Region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8, color = "black") +
  geom_text(aes(label = Reports),
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 2.5) +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal() +
  theme(
    axis.ticks       = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.position  = "right",
    axis.title.x     = element_text(size = 10, face = "bold"),
    axis.title.y     = element_text(size = 10, face = "bold"),
    axis.text.x      = element_text(size = 10),
    axis.text.y      = element_text(size = 8)
  ) +
  labs(
    title = "English Reports for 'Extractives and Minerals Processing' Sector Across Regions",
    x     = "Sector",
    y     = "Count"
  ) +
  guides(fill = guide_legend(reverse = FALSE, title = "Region")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))

###############################################################################

###############################################################################
# STEP 3: FILE ORGANIZATION & BASIC DATA PREPROCESSING
# - 3.1 Folder Creation for Text Files by Region & Sector
# - 3.2 Move Relevant Files into Subfolders
# - 3.3 Verify the Files Were Successfully Moved
###############################################################################

## 3.1 Folder Creation for Text Files ----

# Create local folder "text_files" (suppresses warnings if it already exists)
dir.create("text_files", showWarnings = FALSE)

# Focus on "Extractives and Minerals Processing" 
energy_sector_reports <- matched_reports %>%
  filter(Sec_SASB == "Extractives and Minerals Processing") %>%
  group_by(Region, Sector) %>%
  mutate(Reports = n()) %>%
  ungroup()

# Extract unique regions and sectors from that subset
regions <- unique(energy_sector_reports$Region)
energy_sectors <- unique(energy_sector_reports$Sector)

# Loop through each Region+Sector pair to create subfolders within "text_files"
for (region in regions) {
  for (sector in energy_sectors) {
    safe_sector_name <- gsub("[[:punct:]]", "", gsub(" ", "_", sector))
    safe_region_name <- gsub("[[:punct:]]", "", gsub(" ", "_", region))
    folder_path      <- file.path("text_files", safe_region_name, safe_sector_name)
    dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
  }
}

## 3.2 Move Relevant Files into Subfolders ----
paths <- c()  # Storing the subfolder paths to reference later

move_files_to_folders <- function(df) {
  for (i in seq_len(nrow(df))) {
    region    <- df$Region[i]
    sector    <- df$Sector[i]
    file_name <- paste0(df$file[i], ".txt") 
    
    # Clean up folder names (remove punctuation, replace spaces)
    safe_sector_name <- gsub("[[:punct:]]", "", gsub(" ", "_", sector))
    safe_region_name <- gsub("[[:punct:]]", "", gsub(" ", "_", region))
    
    # Construct the path: text_files/<region>/<sector>
    folder_path      <- file.path("text_files", safe_region_name, safe_sector_name)
    paths <<- unique(c(paths, folder_path))
    
    source_file_path <- file.path(text_files_path, file_name)
    dest_file_path   <- file.path(folder_path, file_name)
    
    # Copy each file if it doesn't already exist in the destination
    if (!file.exists(dest_file_path)) {
      tryCatch({
        file.copy(source_file_path, dest_file_path)
        message(paste("Successfully copied", file_name, "to", folder_path))
      }, error = function(e) {
        message(paste("Failed to copy", source_file_path, "Error:", e$message))
      })
    } else {
      message(paste("File already exists:", dest_file_path, "- Skipping copy."))
    }
  }
}

move_files_to_folders(energy_sector_reports)

## 3.3 Verify the Files Were Successfully Moved ----

count_files_in_folder <- function(df) {
  count <- 0
  for (i in seq_len(nrow(df))) {
    region    <- df$Region[i]
    sector    <- df$Sector[i]
    file_name <- paste0(df$file[i], ".txt")
    
    safe_sector <- gsub("[[:punct:]]", "", gsub(" ", "_", sector))
    safe_region <- gsub("[[:punct:]]", "", gsub(" ", "_", region))
    
    folder_path     <- file.path("text_files", safe_region, safe_sector)
    dest_file_path  <- file.path(folder_path, file_name)
    
    if (file.exists(dest_file_path)) {
      count <- count + 1
    }
  }
  return(count)
}

# Confirm how many .txt files ended up in their respective subfolders
moved_count <- count_files_in_folder(energy_sector_reports)
cat("Number of text files successfully moved:", moved_count, "\n")

###############################################################################

###############################################################################
# STEP 4: TEXT MINING & TF-IDF ANALYSIS
###############################################################################

paths <- unique(paths)

# Define keywords
sdg_keywords <- list(
  "SDG 1" = c(
    # Core poverty terms
    "poverty", "extreme poverty", "poverty eradication", "poverty reduction", "poverty alleviation", 
    "absolute poverty", "relative poverty", "income poverty", "multidimensional poverty",
    # Economic aspects
    "economic empowerment", "financial inclusion", "microfinance", "microcredit", "pro-poor growth",
    "poverty line", "below poverty line", "living wage", "minimum wage", "economic vulnerability",
    # Social protection
    "social protection", "social safety net", "cash transfer", "conditional cash transfer", 
    "unemployment benefit", "welfare program", "poverty trap", "social security", "pension system",
    # Vulnerable groups
    "rural poverty", "urban poverty", "child poverty", "feminization of poverty",
    "intergenerational poverty", "homelessness", "slum", "informal settlement",
    # Basic needs
    "basic needs", "food insecurity", "housing insecurity", "poverty assessment",
    "pro-poor policy", "subsistence living", "economic inclusion", "financial literacy",
    # Measurement & targeting
    "poverty measurement", "poverty index", "poverty mapping", "targeted intervention",
    "ultra poor", "income disparity", "wealth distribution", "economic inequality"
  ),
  
  "SDG 2" = c(
    # Core hunger/food terms
    "hunger", "zero hunger", "food security", "food insecurity", "food sovereignty", 
    "food access", "food availability", "food stability", "food utilization",
    # Nutrition
    "nutrition", "malnutrition", "undernourishment", "undernutrition", "hidden hunger",
    "stunting", "wasting", "micronutrient deficiency", "food fortification", "dietary diversity",
    # Agriculture
    "sustainable agriculture", "agricultural productivity", "agricultural resilience", 
    "climate-smart agriculture", "agroecology", "agroforestry", "aquaculture", "regenerative agriculture",
    "agricultural biodiversity", "crop diversification", "crop rotation", "intercropping",
    # Farming systems
    "small-scale farming", "smallholder farmer", "family farming", "subsistence farming", 
    "organic farming", "urban farming", "precision agriculture", "vertical farming",
    # Food systems
    "food system", "food supply chain", "food distribution", "food storage", "food processing",
    "post-harvest loss", "food waste", "food loss", "value chain development",
    # Agricultural inputs
    "seed security", "seed bank", "agricultural input", "fertilizer", "irrigation",
    "agricultural extension", "agricultural research", "agrarian reform", "land rights",
    # Food policies
    "food policy", "food price", "food aid", "food reserve", "school feeding program"
  ),
  
  "SDG 3" = c(
    # General health
    "health", "healthcare", "public health", "preventive healthcare", "curative care", 
    "healthcare access", "healthcare affordability", "healthcare quality", "primary healthcare",
    # Health systems
    "universal health coverage", "universal healthcare", "health insurance", "health equity", 
    "health financing", "health workforce", "health infrastructure", "health technology",
    # Disease-specific
    "communicable disease", "non-communicable disease", "infectious disease", "epidemic", 
    "pandemic", "disease prevention", "disease control", "antimicrobial resistance",
    "HIV/AIDS", "tuberculosis", "malaria", "neglected tropical diseases", "hepatitis",
    "cardiovascular disease", "cancer", "diabetes", "respiratory disease", 
    # Maternal & child health
    "maternal health", "maternal mortality", "child health", "child mortality", 
    "neonatal health", "infant mortality", "under-five mortality", "obstetric care",
    "skilled birth attendance", "prenatal care", "postnatal care", "breastfeeding",
    # Vaccines & medicines
    "vaccination", "immunization", "vaccine coverage", "essential medicines", 
    "drug access", "pharmaceutical", "medical supplies", "diagnostics",
    # Mental health
    "mental health", "mental illness", "psychological wellbeing", "psychiatric care", 
    "substance abuse", "addiction", "alcohol abuse", "drug abuse", 
    # Other health areas
    "sexual health", "reproductive health", "family planning", "adolescent health",
    "occupational health", "environmental health", "road safety", "tobacco control",
    "health education", "health literacy", "health promotion", "dietary habit"
  ),
  
  "SDG 4" = c(
    # Core education terms
    "education", "quality education", "educational access", "educational equity", 
    "educational opportunity", "educational attainment", "educational achievement",
    # Education levels
    "primary education", "secondary education", "tertiary education", "higher education", 
    "vocational education", "technical education", "professional education",
    "early childhood education", "preschool education", "elementary education", 
    "middle school education", "high school education", "college education", "university education",
    # Educational approaches
    "inclusive education", "special education", "lifelong learning", "distance learning", 
    "e-learning", "online education", "digital learning", "blended learning",
    "formal education", "non-formal education", "informal education", "self-directed learning",
    # Learning outcomes
    "literacy", "numeracy", "digital literacy", "financial literacy", "scientific literacy", 
    "reading proficiency", "mathematics proficiency", "language proficiency",
    "critical thinking", "problem-solving skills", "soft skills", "life skills",
    # Education system
    "educational policy", "curriculum development", "educational assessment", "educational standard", 
    "teacher training", "teacher qualification", "classroom management", "student-teacher ratio",
    "educational infrastructure", "school facility", "learning material", "textbook",
    # STEM & specialized education
    "STEM education", "science education", "technology education", "engineering education", 
    "mathematics education", "arts education", "humanities education", "social science education",
    # Equity in education
    "educational disparity", "gender parity", "girls' education", "boys' education", 
    "rural education", "education for refugees", "education for disabled", "marginalized learners",
    # Educational support
    "scholarship", "student loan", "financial aid", "educational grant", "school feeding program"
  ),
  
  "SDG 5" = c(
    # Core gender terms
    "gender equality", "gender equity", "gender parity", "gender mainstreaming", 
    "gender-responsive", "gender-sensitive", "gender transformative", "gender analysis",
    # Women's rights
    "women's rights", "women's empowerment", "gender discrimination", "gender bias", 
    "gender stereotypes", "patriarchy", "feminism", "women's movement",
    # Economic aspects
    "gender pay gap", "equal pay", "women's economic empowerment", "female entrepreneurship", 
    "women in business", "women in finance", "financial inclusion for women",
    "women's access to credit", "women's property rights", "women's inheritance rights",
    # Leadership & participation
    "women's leadership", "women in politics", "women in governance", "political participation", 
    "decision-making power", "gender quota", "women's representation", "women on boards",
    "women in STEM", "women in science", "women in technology", "women in academia",
    # Violence & harmful practices
    "gender-based violence", "domestic violence", "intimate partner violence", "sexual harassment", 
    "sexual violence", "human trafficking", "female genital mutilation", "child marriage",
    "forced marriage", "honor killing", "dowry-related violence", "cyber violence",
    # Reproductive rights
    "reproductive rights", "reproductive health", "family planning", "maternal health", 
    "sexual health", "contraception access", "safe abortion", "menstrual health",
    # Work & care
    "work-life balance", "unpaid care work", "domestic work", "childcare", 
    "parental leave", "maternity protection", "paternity leave", "flexible working",
    # LGBTQ+ inclusion
    "LGBTQ+ rights", "sexual orientation", "gender identity", "transgender rights", 
    "non-binary inclusion", "gender diverse", "sexual and gender minorities"
  ),
  
  "SDG 6" = c(
    # Water access & quality
    "clean water", "safe drinking water", "water access", "water quality", "water safety", 
    "water testing", "water treatment", "water purification", "water contamination",
    "water pollution", "groundwater", "surface water", "freshwater", "potable water",
    # Sanitation
    "sanitation", "improved sanitation", "toilet access", "toilet facility", "latrine", 
    "open defecation", "fecal sludge management", "sewage", "sewerage system",
    "wastewater", "wastewater treatment", "blackwater", "greywater", "effluent treatment",
    # Hygiene
    "hygiene", "handwashing", "hand hygiene", "personal hygiene", "menstrual hygiene", 
    "hygiene promotion", "hygiene behavior", "hygiene education", "WASH program",
    # Water management
    "water management", "water resource management", "integrated water resource management", 
    "watershed management", "river basin management", "water governance",
    "water efficiency", "water conservation", "water harvesting", "water recycling", 
    "water reuse", "water footprint", "virtual water", "blue water", "green water",
    # Water infrastructure
    "water infrastructure", "water supply system", "water utility", "water distribution", 
    "water storage", "water treatment plant", "desalination", "borehole", "tube well",
    # Water-related ecosystems
    "wetland", "aquifer", "water ecosystem", "riparian zone", "catchment area", 
    "water scarcity", "water stress", "drought", "arid region", "water resilience",
    # Water-related health
    "waterborne disease", "water-related illness", "cholera", "typhoid", "diarrheal disease"
  ),
  
  "SDG 7" = c(
    # Core energy terms
    "energy access", "energy security", "energy poverty", "energy transition", 
    "clean energy", "sustainable energy", "modern energy", "energy service",
    # Renewable energy types
    "renewable energy", "solar energy", "solar power", "solar PV", "solar thermal", 
    "wind energy", "wind power", "onshore wind", "offshore wind", "hydropower",
    "geothermal energy", "biomass energy", "biofuel", "biogas", "tidal energy", 
    "wave energy", "ocean energy", "green hydrogen", "clean hydrogen",
    # Energy efficiency
    "energy efficiency", "energy conservation", "energy saving", "energy performance", 
    "energy intensity", "efficient lighting", "efficient appliance", "building efficiency",
    "industrial efficiency", "transport efficiency", "energy management system",
    # Energy systems
    "energy system", "power generation", "electricity generation", "power transmission", 
    "power distribution", "grid infrastructure", "electricity grid", "smart grid",
    "mini-grid", "microgrid", "off-grid solution", "decentralized energy", "distributed generation",
    # Energy markets & policy
    "energy market", "energy pricing", "energy subsidy", "energy investment", 
    "energy financing", "energy regulation", "energy policy", "renewable portfolio standard",
    "feed-in tariff", "net metering", "carbon pricing", "emissions trading",
    # Energy technologies
    "energy storage", "battery storage", "battery technology", "pumped hydro storage", 
    "thermal storage", "power-to-X", "clean cooking", "improved cookstove",
    # Energy transition
    "energy transition", "fossil fuel phaseout", "coal phaseout", "just transition", 
    "low-carbon economy", "carbon neutrality", "net-zero energy", "zero-emission energy"
  ),
  
  "SDG 8" = c(
    # Economic growth
    "economic growth", "GDP growth", "per capita growth", "sustainable growth", 
    "inclusive growth", "economic development", "economic productivity", "economic diversification",
    "industrial growth", "market development", "value addition", "economic resilience",
    # Employment
    "employment", "job creation", "job quality", "full employment", "productive employment", 
    "decent work", "fair work", "labor market", "workforce development", "human capital",
    "formal employment", "informal employment", "self-employment", "entrepreneurship", 
    "micro-enterprise", "small enterprise", "medium enterprise", "business development",
    # Labor rights & conditions
    "labor rights", "worker rights", "collective bargaining", "freedom of association", 
    "labor standard", "workplace safety", "occupational safety", "occupational health",
    "working conditions", "working hours", "rest periods", "paid leave", "maternity protection", 
    "living wage", "minimum wage", "wage policy", "income security", "social dialogue",
    # Specific employment issues
    "youth employment", "youth unemployment", "women's employment", "gender wage gap", 
    "child labor", "forced labor", "human trafficking", "modern slavery", "labor exploitation",
    "migrant worker", "domestic worker", "gig economy", "platform work", "precarious work", 
    "zero-hour contract", "job security", "job formalization", "labor informality",
    # Economic sectors & innovation
    "tourism sector", "sustainable tourism", "tourism development", "tourism jobs", 
    "financial sector", "financial access", "financial inclusion", "banking service",
    "trade policy", "trade facilitation", "export promotion", "market access", 
    "technological upgrading", "industrial upgrading", "productive capacity", "firm competitiveness"
  ),
  
  "SDG 9" = c(
    # Infrastructure
    "infrastructure", "resilient infrastructure", "sustainable infrastructure", "infrastructure gap", 
    "infrastructure financing", "infrastructure maintenance", "infrastructure development",
    "critical infrastructure", "economic infrastructure", "social infrastructure", "green infrastructure",
    # Transport
    "transport infrastructure", "road infrastructure", "railway", "port infrastructure", "airport", 
    "transport connectivity", "transport access", "sustainable transport", "low-carbon transport",
    "multimodal transport", "transit system", "public transport", "freight transport", "logistics",
    # Digital/ICT infrastructure
    "digital infrastructure", "telecommunications", "broadband access", "internet access", 
    "mobile coverage", "digital connectivity", "digital divide", "ICT infrastructure",
    "data center", "cloud computing", "digital backbone", "fiber optic", "5G network", "satellite connectivity",
    # Industry
    "industrialization", "industrial development", "industrial policy", "industrial diversification", 
    "manufacturing", "value addition", "productive capacity", "industrial upgrading",
    "industrial competitiveness", "industrial productivity", "industrial innovation", "industrial efficiency",
    "resource efficiency", "green manufacturing", "circular production", "eco-industrial park",
    # Small-scale industry
    "small-scale industry", "cottage industry", "micro industry", "small and medium enterprise", 
    "SME development", "SME finance", "SME support", "business incubation", "industrial cluster",
    # Innovation
    "innovation", "research and development", "R&D investment", "R&D intensity", "technological capability", 
    "innovation ecosystem", "innovation policy", "innovation financing", "technology transfer",
    "indigenous technology", "appropriate technology", "technological learning", "technological absorption", 
    "technological dissemination", "innovation capacity", "patent", "intellectual property"
  ),
  
  "SDG 10" = c(
    # Core inequality terms
    "inequality", "income inequality", "wealth inequality", "economic inequality", "social inequality", 
    "inequality reduction", "distributional impact", "progressive policy", "regressive policy",
    "income disparity", "wealth disparity", "wealth concentration", "income concentration", "income gap", 
    "wealth gap", "economic divide", "social divide", "top income", "bottom income",
    # Economic inclusion
    "social inclusion", "economic inclusion", "financial inclusion", "inclusive growth", 
    "inclusive economy", "inclusive society", "inclusive institution", "inclusive service",
    "equal opportunity", "economic opportunity", "opportunity gap", "intergenerational mobility", 
    "social mobility", "income mobility", "socioeconomic status", "social stratification",
    # Discrimination & marginalization
    "discrimination", "social discrimination", "economic discrimination", "institutional discrimination", 
    "structural discrimination", "discriminatory law", "discriminatory policy", "discriminatory practice",
    "marginalized group", "marginalized community", "vulnerable group", "minority group", 
    "disadvantaged group", "social exclusion", "alienation", "disenfranchisement",
    # Specific inequality dimensions
    "racial inequality", "racial disparity", "ethnic inequality", "gender inequality", 
    "disability inequality", "age inequality", "geographic inequality", "spatial inequality",
    "rural-urban divide", "urban-rural gap", "regional disparity", "territorial inequality", 
    # Policies & measures
    "progressive taxation", "wealth tax", "redistributive policy", "social transfer", 
    "cash transfer", "universal basic income", "minimum income", "social protection floor",
    "equality impact assessment", "inequality monitoring", "distributive analysis", 
    "fiscal incidence", "pro-poor policy", "affirmative action", "positive discrimination",
    # Global inequality
    "global inequality", "international inequality", "north-south divide", "developing country voice", 
    "global governance reform", "international financial reform", "migration policy", "remittance cost"
  ),
  
  "SDG 11" = c(
    # Sustainable cities
    "sustainable city", "sustainable urban development", "urban sustainability", "sustainable urbanization", 
    "urban planning", "city planning", "smart city", "resilient city", "green city", "eco-city",
    "compact city", "livable city", "15-minute city", "urban design", "urban form", "urban regeneration", 
    "urban renewal", "urban renovation", "urban retrofit", "brownfield redevelopment",
    # Housing
    "housing", "affordable housing", "housing affordability", "social housing", "public housing", 
    "housing policy", "housing finance", "housing subsidy", "housing cooperative", "community land trust",
    "rental housing", "housing quality", "housing standard", "adequate housing", "right to housing", 
    "homelessness", "informal settlement", "slum", "slum upgrading", "urban poverty",
    # Urban services & infrastructure
    "urban service", "urban infrastructure", "urban amenity", "basic service access", 
    "water supply", "sanitation service", "waste collection", "waste management", "solid waste",
    "urban energy", "district energy", "district heating", "district cooling", "urban electricity", 
    "street lighting", "urban road", "drainage system", "stormwater management", "flood control",
    # Urban mobility & transport
    "urban mobility", "urban transport", "public transportation", "mass transit", "rapid transit", 
    "bus rapid transit", "light rail", "metro system", "transit-oriented development",
    "active mobility", "walking", "cycling", "bike lane", "pedestrian infrastructure", "car-free zone", 
    "low emission zone", "congestion pricing", "traffic management", "parking policy",
    # Urban environment
    "urban environment", "urban green space", "urban park", "urban forest", "urban biodiversity", 
    "urban ecology", "urban heat island", "urban microclimate", "urban cooling",
    "air quality", "air pollution", "noise pollution", "light pollution", "urban health", 
    # Urban safety & resilience
    "urban safety", "urban security", "safe public space", "crime prevention", "urban violence reduction", 
    "urban resilience", "disaster risk reduction", "climate adaptation", "flood resilience",
    "earthquake resilience", "urban emergency response", "urban crisis management"
  ),
  
  "SDG 12" = c(
    # Sustainable consumption
    "sustainable consumption", "responsible consumption", "consumer behavior", "sustainable lifestyle", 
    "ethical consumption", "green consumption", "consumption pattern", "overconsumption",
    "conscious consumption", "mindful consumption", "consumer awareness", "consumer education", 
    "eco-label", "product certification", "green label", "sustainable purchasing", "green procurement",
    # Sustainable production
    "sustainable production", "cleaner production", "clean manufacturing", "green manufacturing", 
    "eco-design", "design for environment", "life cycle design", "cradle-to-cradle",
    "resource efficiency", "material efficiency", "energy efficiency", "water efficiency", 
    "industrial ecology", "industrial symbiosis", "by-product synergy", "zero emission",
    # Waste management
    "waste reduction", "waste prevention", "waste minimization", "zero waste", "waste hierarchy", 
    "waste diversion", "waste management", "waste treatment", "waste disposal",
    "recycling", "upcycling", "downcycling", "material recovery", "resource recovery", 
    "waste-to-energy", "waste-to-resource", "organic waste", "compost", "anaerobic digestion",
    # Specific waste streams
    "food waste", "food loss", "e-waste", "electronic waste", "plastic waste", "plastic pollution", 
    "single-use plastic", "packaging waste", "construction waste", "demolition waste",
    "hazardous waste", "chemical waste", "biomedical waste", "nuclear waste", "textile waste", 
    # Circular economy
    "circular economy", "circularity", "circular business model", "circular supply chain", 
    "closed-loop system", "material loop", "product as service", "product-service system",
    "sharing economy", "collaborative consumption", "peer-to-peer sharing", "reuse", "refurbishment", 
    "remanufacturing", "repair", "extended producer responsibility", "take-back system",
    # Sustainable business practices
    "sustainable business", "corporate sustainability", "sustainable supply chain", "sustainable procurement", 
    "sustainability reporting", "environmental accounting", "natural capital accounting",
    "life cycle assessment", "carbon footprint", "water footprint", "ecological footprint", 
    "environmental impact assessment", "social impact assessment"
  ),
  
  "SDG 13" = c(
    # Core climate terms
    "climate change", "global warming", "greenhouse effect", "climate emergency", "climate crisis", 
    "climate action", "climate response", "climate solution", "climate policy", "climate governance",
    "climate science", "climate model", "climate projection", "climate scenario", "climate sensitivity", 
    "global temperature", "temperature rise", "warming limit", "1.5 degrees", "2 degrees",
    # Emissions & mitigation
    "carbon emission", "greenhouse gas emission", "GHG emission", "CO2 emission", "methane emission", 
    "emission reduction", "emission target", "emission pathway", "emission scenario", "emission budget",
    "carbon budget", "carbon management", "carbon accounting", "carbon footprint", "carbon inventory", 
    "carbon intensity", "decarbonization", "deep decarbonization", "low-carbon development",
    "carbon neutral", "carbon neutrality", "net zero", "net zero emission", "climate neutral", 
    "climate neutrality", "zero emission", "zero carbon", "negative emission", "carbon negative",
    # Climate solutions
    "carbon price", "carbon tax", "carbon market", "emissions trading", "cap and trade", 
    "carbon offset", "carbon credit", "carbon compensation", "voluntary carbon market",
    "renewable energy transition", "energy transition", "clean energy shift", "fossil fuel phaseout", 
    "coal phaseout", "just transition", "green recovery", "green stimulus", "build back better",
    # Climate adaptation
    "climate adaptation", "adaptation measure", "adaptation strategy", "adaptation planning", 
    "adaptation finance", "adaptation funding", "adaptation cost", "adaptation gap",
    "climate resilience", "climate-resilient", "climate-proofing", "climate risk", "climate vulnerability", 
    "climate hazard", "climate impact", "climate disaster", "climate shock", "slow-onset event",
    # Climate impacts
    "sea level rise", "coastal erosion", "coastal flooding", "ocean acidification", "marine heatwave", 
    "coral bleaching", "extreme weather", "heat wave", "drought", "flood", "hurricane", "cyclone",
    "climate migration", "climate displacement", "climate refugee", "climate conflict", 
    "food security impact", "water security impact", "biodiversity impact", "climate health impact",
    # Climate policy & governance
    "Paris Agreement", "nationally determined contribution", "NDC", "climate negotiation", 
    "UNFCCC", "COP conference", "Kyoto Protocol", "global climate regime", "climate compliance",
    "climate finance", "green climate fund", "climate investment", "carbon disclosure", "TCFD", 
    "climate justice", "common but differentiated responsibility", "intergenerational equity"
  ),
  
  "SDG 14" = c(
    # Marine environments
    "ocean", "sea", "marine", "marine ecosystem", "marine environment", "coastal ecosystem", 
    "coastal zone", "shoreline", "intertidal zone", "estuary", "mangrove", "salt marsh",
    "coral reef", "coral bleaching", "seagrass meadow", "kelp forest", "blue carbon", 
    "deep sea", "abyssal plain", "seamount", "hydrothermal vent", "continental shelf",
    # Marine conservation
    "marine conservation", "ocean conservation", "marine protected area", "MPA", "marine reserve", 
    "ocean sanctuary", "blue park", "no-take zone", "marine spatial planning", "ecosystem-based management",
    "sustainable coastal management", "integrated coastal management", "coastal resilience", 
    "blue economy", "sustainable blue economy", "blue growth", "blue finance", "blue bond",
    # Marine biodiversity
    "marine biodiversity", "marine species", "marine habitat", "marine ecology", "marine ecosystem service", 
    "marine organism", "marine wildlife", "marine endangered species", "marine endemic species",
    "marine mammal", "cetacean", "whale", "dolphin", "shark", "ray", "sea turtle", 
    "seabird", "fish stock", "marine flora", "seaweed", "plankton", "coral", "benthic organism",
    # Ocean health issues
    "ocean health", "marine pollution", "ocean pollution", "ship pollution", "oil spill", 
    "marine debris", "marine litter", "plastic pollution", "microplastic", "ghost fishing gear",
    "ocean acidification", "ocean warming", "marine heatwave", "sea level rise", "coastal erosion", 
    "coastal flooding", "hypoxia", "dead zone", "harmful algal bloom", "ocean deoxygenation",
    # Fisheries & aquaculture
    "fishery", "sustainable fishery", "fishery management", "fish stock", "fish population", 
    "overfishing", "illegal fishing", "unreported fishing", "unregulated fishing", "IUU fishing",
    "bycatch", "fish discard", "fishing quota", "fishing subsidy", "small-scale fishery", 
    "artisanal fishing", "traditional fishing", "fishing community", "fishery collapse",
    "aquaculture", "mariculture", "fish farming", "seaweed farming", "sustainable aquaculture", 
    "integrated multi-trophic aquaculture", "recirculating aquaculture", "aquaponic system",
    # Ocean governance
    "ocean governance", "maritime law", "law of the sea", "UNCLOS", "exclusive economic zone", "EEZ", 
    "high seas", "areas beyond national jurisdiction", "BBNJ", "regional fisheries management"
  ),
  
  "SDG 15" = c(
    # Terrestrial ecosystems
    "terrestrial ecosystem", "land ecosystem", "ecosystem service", "ecosystem function", 
    "ecosystem integrity", "ecosystem health", "ecosystem restoration", "ecosystem degradation",
    "habitat", "habitat conservation", "habitat restoration", "habitat fragmentation", "habitat loss", 
    "ecological connectivity", "wildlife corridor", "ecological network", "protected area",
    # Forests
    "forest", "forestry", "woodland", "tree cover", "primary forest", "old-growth forest", 
    "secondary forest", "temperate forest", "boreal forest", "tropical forest", "rainforest",
    "deforestation", "forest degradation", "forest conservation", "forest management", 
    "sustainable forest management", "forest restoration", "afforestation", "reforestation",
    "forest carbon", "forest carbon sink", "REDD+", "forest certification", "timber legality", 
    "forest stewardship", "forest product", "non-timber forest product", "agroforestry",
    # Biodiversity
    "biodiversity", "biological diversity", "species diversity", "genetic diversity", "ecosystem diversity", 
    "biodiversity hotspot", "biodiversity conservation", "biodiversity loss", "biodiversity monitoring",
    "endangered species", "threatened species", "vulnerable species", "extinct species", "endemic species", 
    "keystone species", "flagship species", "invasive species", "alien species", "indicator species",
    "wildlife", "wildlife conservation", "wildlife management", "wildlife trade", "wildlife trafficking", 
    "poaching", "bush meat", "human-wildlife conflict", "wildlife sanctuary", "game reserve",
    # Land degradation
    "land degradation", "soil degradation", "soil erosion", "soil fertility", "soil health", 
    "soil carbon", "soil biodiversity", "land rehabilitation", "land reclamation", "land restoration",
    "desertification", "drought", "arid land", "semi-arid land", "dryland management", 
    "sustainable land management", "integrated landscape management", "landscape approach",
    # Mountains & specific ecosystems
    "mountain", "mountain ecosystem", "alpine", "highland", "watershed", "water tower", 
    "montane forest", "mountain biodiversity", "mountain community", "mountain livelihood",
    "grassland", "rangeland", "prairie", "savanna", "steppe", "wetland", "peatland", "bog", 
    "marsh", "fen", "floodplain", "riparian zone", "freshwater ecosystem", "inland water",
    # Conservation approaches
    "in-situ conservation", "ex-situ conservation", "seed bank", "gene bank", "botanical garden", 
    "zoological garden", "wildlife sanctuary", "biosphere reserve", "world heritage site",
    "community conservation", "indigenous conservation", "traditional knowledge", "biocultural heritage"
  ),
  
  "SDG 16" = c(
    # Peace & conflict
    "peace", "peacebuilding", "peacekeeping", "peacemaking", "conflict prevention", 
    "conflict resolution", "conflict management", "post-conflict", "reconciliation",
    "violence reduction", "non-violence", "arms control", "disarmament", "demobilization", 
    "reintegration", "small arms", "light weapons", "arms trade", "arms trafficking",
    "war", "armed conflict", "civil conflict", "interstate conflict", "terrorism", "extremism", 
    "radicalization", "counterterrorism", "political violence", "state fragility", "fragile state",
    # Justice & rule of law
    "justice", "access to justice", "legal aid", "legal assistance", "legal empowerment", 
    "legal identity", "birth registration", "civil registration", "vital statistics",
    "rule of law", "judicial independence", "judiciary", "legal system", "legal framework", 
    "constitutional reform", "law enforcement", "police reform", "criminal justice reform",
    "due process", "fair trial", "legal protection", "rights protection", "detention condition", 
    "prison reform", "alternative sentencing", "restorative justice", "transitional justice",
    # Human rights
    "human rights", "civil rights", "political rights", "economic rights", "social rights", 
    "cultural rights", "right to life", "freedom from torture", "freedom of movement",
    "freedom of expression", "freedom of assembly", "freedom of association", "freedom of religion", 
    "right to privacy", "digital rights", "internet freedom", "data protection",
    "minority rights", "indigenous rights", "women's rights", "children's rights", "disability rights", 
    "LGBTQ+ rights", "refugee rights", "migrant rights", "prisoner rights", "victim rights",
    # Governance & institutions
    "governance", "good governance", "institutional capacity", "institutional reform", 
    "public administration", "public service", "civil service", "public sector efficiency",
    "transparency", "government transparency", "information access", "right to information", 
    "open government", "open data", "freedom of information", "whistleblower protection",
    "accountability", "democratic accountability", "horizontal accountability", "vertical accountability", 
    "social accountability", "public oversight", "checks and balances", "separation of powers",
    # Anti-corruption
    "anti-corruption", "corruption prevention", "anti-bribery", "integrity", "public integrity", 
    "corporate integrity", "business integrity", "ethical standard", "code of conduct",
    "corruption", "bribery", "corruption", "bribery", "embezzlement", "fraud", "money laundering", "illicit finance",
    "tax evasion", "tax avoidance", "beneficial ownership", "asset recovery", "stolen asset recovery",
    "conflict of interest", "revolving door", "nepotism", "cronyism", "clientelism", "state capture",
    
    # Democracy & participation
    "democracy", "democratic governance", "democratic institution", "democratic transition",
    "democratic consolidation", "democratic backsliding", "authoritarianism", "autocracy",
    "election", "electoral system", "electoral integrity", "free and fair election",
    "voter registration", "electoral commission", "election observation", "election monitoring",
    "political participation", "civic participation", "civic space", "civic engagement",
    "civil society", "CSO", "NGO", "citizen action", "public consultation", "participatory governance",
    
    # Inclusive institutions
    "inclusive institution", "inclusive governance", "political inclusion", "social inclusion",
    "representative institution", "participatory decision-making", "local governance", "decentralization",
    "federalism", "subsidiarity", "local authority", "municipal government", "community governance"),
  
  "SDG 17" = c(
    # Partnerships
    "partnership", "multi-stakeholder partnership", "public-private partnership", "cross-sector partnership",
    "global partnership", "partnership platform", "partnership broker", "collaborative initiative",
    "international cooperation", "development cooperation", "south-south cooperation",
    "triangular cooperation", "north-south cooperation", "regional cooperation", "bilateral cooperation",
    "multilateral cooperation", "multi-stakeholder dialogue", "stakeholder engagement", "coalition building",
    
    # Finance
    "development finance", "international finance", "development assistance", "official development assistance",
    "ODA", "aid effectiveness", "aid allocation", "aid transparency", "concessional finance",
    "domestic resource mobilization", "tax revenue", "public finance", "national budget", "fiscal space",
    "sovereign debt", "debt sustainability", "debt service", "debt relief", "debt restructuring",
    "external debt", "foreign direct investment", "FDI", "private capital flow", "remittance",
    "blended finance", "impact investment", "innovative finance", "development bank", "multilateral bank",
    
    # Technology
    "technology transfer", "technology diffusion", "technology access", "technology sharing",
    "technology bank", "technology facilitation", "science cooperation", "research collaboration",
    "digital cooperation", "digital divide", "digital bridge", "ICT access", "internet connectivity",
    "open-source technology", "appropriate technology", "clean technology", "environmentally sound technology",
    
    # Capacity building
    "capacity building", "capacity development", "skills transfer", "knowledge transfer",
    "technical assistance", "technical cooperation", "institution strengthening", "capability enhancement",
    "human resource development", "training program", "knowledge sharing", "peer learning",
    "best practice exchange", "south-south learning", "community of practice", "center of excellence",
    
    # Trade
    "international trade", "trade system", "multilateral trading system", "WTO", "trade facilitation",
    "trade barrier", "tariff", "non-tariff barrier", "trade restriction", "market access",
    "preferential treatment", "special and differential treatment", "rules of origin", "trade agreement",
    "regional trade", "free trade", "fair trade", "inclusive trade", "sustainable trade",
    "trade for development", "aid for trade", "trade capacity", "export promotion", "export diversification",
    
    # Systemic issues
    "policy coherence", "development coherence", "institutional coherence", "global governance",
    "international architecture", "multilateral system", "UN system", "Bretton Woods institution",
    "international monetary system", "global financial architecture", "global economic governance",
    "macroeconomic stability", "global economic coordination", "financial regulation",
    
    # Data & monitoring
    "data collection", "data disaggregation", "statistical capacity", "statistical system",
    "national statistics", "development indicator", "SDG indicator", "SDG monitoring",
    "SDG implementation", "SDG localization", "SDG mainstreaming", "SDG financing",
    "SDG review", "voluntary national review", "VNR", "high-level political forum", "HLPF"
  ))

gri_keywords <- list(
  # General keywords
  "environmental_keywords" = c(
    "environment", "climate change", "sustainability", "carbon emissions", "carbon footprint",
    "greenhouse gases", "water consumption", "energy efficiency", "waste management", "pollution",
    "air quality", "land use", "biodiversity", "deforestation", "soil degradation", "water quality",
    "sustainable agriculture", "recycling", "energy transition", "renewable energy", "solar power",
    "wind energy", "hydropower", "carbon offset", "climate adaptation", "climate resilience",
    "green building", "environmental impact", "sustainable practices", "environmental protection",
    "eco-friendly", "clean energy", "net zero", "emissions reduction", "reforestation", "marine conservation",
    "ocean pollution", "plastic waste", "circular economy", "waste reduction", "sustainable transport",
    "eco-tourism", "climate policy", "clean air", "eco-innovation", "climate action", "energy conservation",
    "water conservation", "sustainable supply chain", "green procurement", "environmental footprint",
    "electrical vehicles", "greenwashing",
    
    # Energy-specific additions
    "energy access", "energy security", "energy poverty", "energy service", "energy systems",
    "solar energy", "solar PV", "solar thermal", "onshore wind", "offshore wind", 
    "geothermal energy", "biomass energy", "biofuel", "biogas", "tidal energy", 
    "wave energy", "ocean energy", "green hydrogen", "clean hydrogen",
    "energy saving", "energy performance", "energy intensity", "efficient lighting", 
    "efficient appliance", "building efficiency", "industrial efficiency", "transport efficiency", 
    "energy management system", "power generation", "electricity generation", "power transmission", 
    "power distribution", "grid infrastructure", "electricity grid", "smart grid",
    "mini-grid", "microgrid", "off-grid solution", "decentralized energy", "distributed generation",
    "energy market", "energy pricing", "energy subsidy", "energy investment", 
    "energy financing", "energy regulation", "energy policy", "renewable portfolio standard",
    "feed-in tariff", "net metering", "energy storage", "battery storage", "battery technology", 
    "pumped hydro storage", "thermal storage", "power-to-X", "fossil fuel phaseout", 
    "coal phaseout", "just transition", "low-carbon economy", "carbon neutrality", 
    "net-zero energy", "zero-emission energy",
    
    # Climate-specific additions
    "IPCC", "global warming", "climate mitigation", "climate impacts", "temperature rise",
    "Paris Agreement", "carbon budget", "carbon sequestration", "carbon sink", "carbon capture",
    "CCUS", "carbon taxation", "cap and trade", "climate finance", "climate justice",
    "climate vulnerability", "climate risk assessment", "carbon accounting", "science-based targets",
    "methane emissions", "nitrous oxide", "ozone depletion", "climate modeling", "radiative forcing",
    "carbon intensity", "carbon trading", "carbon markets", "carbon disclosure", "carbon-neutral",
    "climate emergency", "climate crisis",
    
    # Biodiversity additions
    "ecosystem services", "habitat conservation", "species protection", "endangered species",
    "protected areas", "conservation biology", "restoration ecology", "invasive species",
    "wildlife corridors", "genetic diversity", "sustainable forestry", "natural capital",
    "ecological footprint", "habitat loss", "ecosystem degradation", "species extinction",
    "rewilding", "biosphere", "conservation effort", "ecological restoration", "biodiversity loss",
    "biodiversity conservation", "IPBES", "ecosystem health", "nature-based solutions",
    "flora and fauna", "biodiversity hotspot", "keystone species", "sustainable fisheries",
    
    # Water-specific additions
    "watershed management", "groundwater", "freshwater ecosystems", "water stress",
    "water scarcity", "water footprint", "water reuse", "water recycling", "water treatment",
    "water purification", "blue economy", "ocean acidification", "marine protection",
    "coastal management", "coral reef conservation", "water efficiency", "water risk",
    "water security", "watershed protection", "water stewardship", "hydrological cycle",
    "water harvesting", "aquifer depletion", "blue carbon", "aquatic ecosystems",
    
    # Waste and circular economy additions
    "zero waste", "waste-to-energy", "landfill diversion", "compostable", "biodegradable",
    "e-waste", "hazardous waste", "industrial waste", "resource recovery", "materials recovery",
    "upcycling", "downstream waste", "waste valorization", "product lifecycle", "life cycle assessment",
    "cradle-to-cradle", "extended producer responsibility", "remanufacturing", "refurbishment",
    "product stewardship", "end-of-life", "design for environment", "resource efficiency",
    "dematerialization", "leasing economy", "sharing economy", "repair", "reuse"),
  
  "social_keywords" = c(
    # Original keywords
    "community", "social responsibility", "stakeholder engagement", "human rights", "diversity",
    "inclusion", "gender equality", "equity", "employee welfare", "education", "healthcare",
    "mental health", "labor rights", "fair wages", "decent work", "child labor", "forced labor",
    "freedom of association", "workplace safety", "living wage", "employee benefits", "work-life balance",
    "gender pay gap", "workplace harassment", "employee training", "inclusive policies", "social impact",
    "volunteering", "local development", "philanthropy", "charitable giving", "social justice",
    "affordable housing", "access to education", "health and safety", "public health", "hunger",
    "access to clean water", "discrimination", "marginalized groups", "ethnic minorities", "LGBTQ rights",
    "mental wellness", "employment opportunities", "youth development", "indigenous rights",
    "elderly care", "community outreach", "corporate philanthropy", "community engagement",
    "social innovation", "fair trade", "consumer protection", "public accountability",
    
    # Human rights additions
    "human trafficking", "UN Global Compact", "UDHR", "civil liberties", "migrant rights",
    "refugee rights", "privacy rights", "digital rights", "right to food", "right to shelter",
    "land rights", "water rights", "women's rights", "children's rights", "labor standards",
    "ILO conventions", "modern slavery", "human dignity", "social justice", "human rights due diligence",
    "remedy", "grievance mechanisms", "right to education", "right to health", "cultural rights",
    "political rights", "social security", "collective bargaining", "right to organize",
    
    # Labor and employment additions
    "employee engagement", "talent development", "talent retention", "occupational health",
    "labor conditions", "labor standards", "temporary workers", "precarious work", "gig economy",
    "trade unions", "collective agreements", "staff turnover", "worker representation",
    "labor legislation", "employment equity", "employee voice", "parental leave", "childcare",
    "eldercare", "flexible working", "remote work", "workplace culture", "workplace wellbeing",
    "absenteeism", "presenteeism", "employee satisfaction", "workforce development",
    "career progression", "skills training", "apprenticeships", "working conditions",
    
    # Community and society additions
    "social cohesion", "social capital", "social mobility", "civic engagement", "poverty reduction",
    "inequality", "wealth gap", "social welfare", "charity", "NGO partnerships", "vulnerable populations",
    "social inclusion", "digital inclusion", "accessibility", "disability rights", "social enterprise",
    "community investment", "responsible marketing", "ethical advertising", "community needs assessment",
    "social license to operate", "social value", "social return on investment", "peace building",
    "conflict minerals", "responsible sourcing", "ethical supply chain", "benefit corporation",
    "social performance", "socioeconomic impact", "racial equality", "social displacement",
    
    # Health and wellbeing additions
    "wellness programs", "healthcare access", "disease prevention", "epidemic preparedness",
    "mental health support", "burnout prevention", "stress management", "addiction support",
    "food security", "malnutrition", "maternal health", "child mortality", "life expectancy",
    "telemedicine", "health literacy", "health disparities", "healthcare affordability",
    "vaccine equity", "public sanitation", "pandemic response", "neglected diseases",
    "one health", "preventive care", "health promotion", "employee assistance programs",
    "unhealthy products", "smoking cessation", "alcohol harm reduction", "obesity prevention"
  ),
  
  "governance_keywords" = c(
    # Original keywords
    "corporate governance", "board of directors", "executive compensation", "compliance", "transparency",
    "accountability", "anti-corruption", "ethics", "internal controls", "corporate responsibility",
    "anti-bribery", "risk management", "audit", "shareholder rights", "stakeholder interests",
    "financial reporting", "business ethics", "conflict of interest", "board diversity", "integrity",
    "whistleblower policies", "governance structure", "independent directors", "regulatory compliance",
    "financial transparency", "external audit", "audit committee", "corporate culture", "ethical behavior",
    "market manipulation", "tax strategy", "executive remuneration", "cybersecurity", "data privacy",
    "supply chain ethics", "legal compliance", "anti-money laundering", "anti-tax evasion", "insider trading",
    "intellectual property", "corporate risk", "corporate social responsibility", "political donations",
    "sustainable finance", "public-private partnerships", "executive accountability", "corporate sustainability",
    "corporate reputation", "compliance monitoring", "stakeholder engagement", "board leadership",
    
    # Board and leadership additions
    "board effectiveness", "board evaluation", "succession planning", "CEO succession",
    "board composition", "board independence", "board committees", "nomination committee",
    "remuneration committee", "governance committee", "non-executive directors", "supervisory board",
    "lead independent director", "board tenure", "board skills matrix", "director qualifications",
    "executive performance", "CEO performance", "leadership diversity", "board gender balance",
    "C-suite diversity", "board ethnic diversity", "governance code", "corporate bylaws",
    "board charter", "fiduciary duty", "duty of care", "duty of loyalty", "strategic oversight",
    
    # Risk governance additions
    "enterprise risk management", "risk appetite", "risk culture", "risk committee",
    "risk assessment", "emerging risks", "risk disclosure", "systemic risk", "operational risk",
    "financial risk", "market risk", "credit risk", "geopolitical risk", "reputational risk",
    "compliance risk", "strategic risk", "ESG risk", "climate risk governance", "risk mitigation",
    "risk controls", "third-party risk", "supply chain risk", "business continuity", "crisis management",
    "resilience planning", "stress testing", "scenario analysis", "risk monitoring", "risk reporting",
    
    # Ethics and compliance additions
    "code of conduct", "code of ethics", "compliance program", "corporate values", "ethical leadership",
    "compliance training", "ethics hotline", "speak-up culture", "confidential reporting",
    "non-retaliation policy", "ethics committee", "ethical standards", "compliance audit",
    "anti-corruption program", "FCPA compliance", "UK Bribery Act", "sanctions compliance",
    "trade controls", "ethics officer", "chief compliance officer", "compliance framework",
    "compliance monitoring", "ethical decision-making", "integrity due diligence", "compliance certification",
    "ethics risk assessment", "third-party due diligence", "facilitation payments", "conflicts register",
    
    # Shareholder and stakeholder additions
    "shareholder activism", "proxy voting", "say on pay", "shareholder engagement",
    "majority voting", "cumulative voting", "shareholder proposals", "institutional investors",
    "responsible investment", "stewardship code", "dual-class shares", "one-share-one-vote",
    "shareholder resolution", "annual general meeting", "multi-stakeholder initiative",
    "stakeholder capitalism", "stakeholder theory", "stakeholder mapping", "double materiality",
    "stakeholder dialogue", "stakeholder consultation", "materiality assessment", "integrated reporting",
    "stakeholder advisory board", "multi-stakeholder governance", "stakeholder accountability",
    
    # Disclosure and reporting additions
    "non-financial reporting", "ESG disclosure", "sustainability reporting", "corporate reporting",
    "GRI Standards", "SASB Standards", "TCFD", "CDP", "integrated reporting", "IIRC framework",
    "UN Global Compact", "SDG reporting", "EU NFRD", "EU SFDR", "CSRD", "ESG ratings",
    "proxy advisory", "assurance", "third-party verification", "disclosure policy",
    "mandatory disclosure", "voluntary disclosure", "reporting standards", "reporting framework",
    "narrative reporting", "management commentary", "disclosure controls", "reporting controls",
    "performance indicators", "KPIs", "materiality", "double materiality"
  ))

# Combine SDG and GRI keywords
all_keywords <- c(sdg_keywords, gri_keywords)

extract_and_count_keywords <- function(path, all_keywords) {
  print("Starting processing...")
  
  print("Listing TXT files in directory...")
  txt_files <- list.files(path, pattern = "\\.txt$", full.names = TRUE)
  print(paste("Found", length(txt_files), "TXT files."))
  
  count_total_files <<- count_total_files + length(txt_files)
  
  print("Initializing data container...")
  text_dt <- data.table(
    article_name = character(length(txt_files)),
    cleaned_text = character(length(txt_files)),
    count_numbers = integer(length(txt_files))
  )
  
  print("Setting up parallel workers...")
  num_cores <- detectCores() - 1
  
  # Do not use more cores than the number of files
  if (length(txt_files) < num_cores) {
    num_cores <- length(txt_files)
  }
  cl <- makeCluster(num_cores, outfile = "")
  clusterExport(cl, c("all_keywords", "stopwords"), envir = environment())
  
  # Load necessary packages in each worker
  clusterEvalQ(cl, {
    library(data.table)
    library(stringr)
    library(tm)
  })
  
  print(paste(
    "Processing", length(txt_files), "files using", num_cores, "cores..."
  ))
  
  process_chunk <- function(file_path) {
    tryCatch({
      print(paste("Processing file:", file_path))
      combined_text <- paste(fread(file_path, header = FALSE, sep = "\n")[[1]], collapse = " ")
      lowercased_text <- tolower(combined_text)
      print("  Reading text file...")
      
      count_numbers <- str_count(lowercased_text, "\\b\\d+\\b")
      print("  Counting numbers...")
      
      cleaned_text <- lowercased_text %>%
        removePunctuation() %>%
        removeNumbers() %>%
        removeWords(stopwords("en")) %>%
        stripWhitespace()
      print("  Cleaning text...")
      
      keyword_counts <- lapply(all_keywords, function(keywords) {
        sapply(keywords, function(k) str_count(cleaned_text, paste0("\\b", k, "\\b")))
      })
      print("  Performing keyword count...")
      
      # Calculate total words for normalization
      total_words <- length(unlist(strsplit(cleaned_text, "\\s+")))
      print(paste("  Total words in text:", total_words))
      
      article_name <- sub("\\.txt$", "", basename(file_path))
      print(paste("  File", file_path, "processed successfully."))
      
      list(
        article_name = article_name,
        cleaned_text = cleaned_text,
        count_numbers = count_numbers,
        total_words = total_words,
        keyword_counts = keyword_counts
      )
    }, error = function(e) {
      message("Error in ", file_path, ": ", e$message)
      NULL
    })
  }
  
  results <- parLapplyLB(cl, txt_files, function(file) {
    result <- process_chunk(file)
    result
  })
  
  stopCluster(cl)
  
  print("Combining results...")
  valid_results <- results[!sapply(results, is.null)]
  
  print("Building final dataframe...")
  for (i in seq_along(valid_results)) {
    set(
      text_dt,
      i = i,
      j = c(
        "article_name",
        "cleaned_text",
        "count_numbers",
        "total_words"
      ),
      value = list(
        valid_results[[i]]$article_name,
        valid_results[[i]]$cleaned_text,
        valid_results[[i]]$count_numbers,
        valid_results[[i]]$total_words
      )
    )
    
    for (k in names(all_keywords)) {
      raw_count <- sum(unlist(valid_results[[i]]$keyword_counts[k]), na.rm = TRUE)
      
      set(
        text_dt,
        i = i,
        j = paste0("count_", gsub(" ", "_", k)),
        value = raw_count
      )
      
      # Compute normalized mentions per 10,000 words
      tw <- valid_results[[i]]$total_words
      # Avoid division by zero:
      norm_val <- ifelse(tw > 0, (raw_count / tw) * 10000, 0)
      
      set(
        text_dt,
        i = i,
        j = paste0("norm_", gsub(" ", "_", k)),
        value = norm_val
      )
    }
  }
  
  print("Adding region/sector information...")
  path_parts <- strsplit(path, "[/\\\\]")[[1]]
  text_dt[, `:=`(Region = tail(path_parts, 2)[1],
                 Sector = tail(path_parts, 2)[2])]
  
  return(text_dt)
}

count_total_files <- 0
df_list <- list()
start_time <- Sys.time()
for (path in paths) {
  if (length(list.files(path, pattern = "\\.txt$", full.names = TRUE)) == 0) {
    cat("No TXT files found in directory:", path, "\n")
    next
  }
  df <- extract_and_count_keywords(path, all_keywords)
  df_list[[path]] <- df
  cat("\n")
}
end_time <- Sys.time()
elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
print(paste(
  "Total time taken to process ",
  count_total_files, " files", round(elapsed_time, 2), "seconds."
))

# Combine all dataframes into one
final_df <- do.call(rbind, df_list)

subset_df <- as.data.frame(final_df)

names(sustainability_df)[names(sustainability_df) == "file"] <- "article_name"

subset_df$Sector <- NULL
subset_df$Region <- NULL
subset_df$cleaned_text <- NULL
subset_df <- merge(subset_df,
                   sustainability_df[, c("article_name", "Name", "Region", "Sector", "Country", "Year")],
                   by = "article_name",
                   all.x = TRUE)

# Print column names
print(names(subset_df))

cleaned_subset_df <- na.omit(subset_df)

write_xlsx(subset_df, file = "cleaned_subset_data.xlsx")

# ===================== TF-IDF =====================
# 1. Identify all 'count_*' columns
count_cols <- grep("^count_", names(subset_df), value = TRUE)  

# 2. Prepare for IDF calculation
N <- nrow(subset_df)  # total number of documents

# Store Document Frequency in a named vector: df_map[column_name] = doc_freq
df_map <- numeric(length(count_cols))
names(df_map) <- count_cols

# 3. Compute Document Frequency for each count_* column
for(col in count_cols) {
  df_map[col] <- sum(subset_df[[col]] > 0, na.rm = TRUE) 
}

# 4. Compute IDF for each term: idf = log10(N / df)
idf_map <- log10(N / df_map)  
# some columns might have df=0 if no doc used the term - handle or ignore them:
idf_map[!is.finite(idf_map)] <- 0  # set IDF=0 if df=0 or anything infinite

# 5. Create new columns tfidf_* in subset_df
for(col in count_cols) {
  # Name it "tfidf_" + the rest
  tfidf_col <- sub("^count_", "tfidf_", col)
  
  # compute TF = count / total_words
  TF <- ifelse(subset_df$total_words > 0,
               subset_df[[col]] / subset_df$total_words,
               0)
  
  # TF-IDF
  subset_df[[tfidf_col]] <- TF * idf_map[col]
  # Rescaling by 100000 for better understanding
  subset_df[[tfidf_col]] <- subset_df[[tfidf_col]] * 100000
}

# 6. Confirm new tfidf_* columns
head(subset_df[, grepl("^tfidf_", names(subset_df))])

# ===================== SDG & ESG Mapping =====================
# Create SDG mapping
sdg_mapping_tfidf <- tibble(
  SDG = paste0("tfidf_SDG_", 1:17),
  SDG_name = c("No Poverty", "Zero Hunger", "Good Health & Well-being", 
               "Quality Education", "Gender Equality", "Clean Water & Sanitation",
               "Affordable & Clean Energy", "Decent Work & Economic Growth",
               "Industry, Innovation & Infrastructure", "Reduced Inequalities",
               "Sustainable Cities & Communities", "Responsible Consumption & Production",
               "Climate Action", "Life Below Water", "Life on Land",
               "Peace, Justice & Strong Institutions", "Partnerships for the Goals")
)

# Create ESG mapping
esg_mapping_tfidf <- tibble(
  GRI_Category = c("tfidf_environmental_keywords", "tfidf_social_keywords", "tfidf_governance_keywords"),
  ESG_name = c("Environmental", "Social", "Governance")
)

# ===================== SDG Visualizations =====================

# 1. SDG Mentions - Horizontal Bar Chart (Sorted)
normalized_tfidf_df <- subset_df %>%
  pivot_longer(
    cols = starts_with("tfidf_SDG"), 
    names_to = "SDG", 
    values_to = "TFIDF_Value"
  ) %>%
  group_by(SDG) %>%
  summarise(Avg_TFIDF = mean(TFIDF_Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(sdg_mapping_tfidf, by = "SDG") %>%
  filter(!is.na(SDG_name)) %>%
  arrange(desc(Avg_TFIDF))

sdg_overall_tfidf_plot <- ggplot(normalized_tfidf_df,aes(x = reorder(SDG_name, Avg_TFIDF), y = Avg_TFIDF, fill = Avg_TFIDF)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "plasma", name = "Avg TF-IDF") +
  labs(title = "SDG Term Significance in Sustainability Reports",
       x = "Sustainable Development Goals (SDGs)", y = "Average TF-IDF Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +
  coord_flip()

sdg_overall_tfidf_plot

# 3. SDG Mentions Across Sectors - Improved
sdg_sector_tfidf_heatmap <- subset_df %>%
  pivot_longer(
    cols = starts_with("tfidf_SDG"), 
    names_to = "SDG", 
    values_to = "TFIDF_Value"
  ) %>%
  group_by(Sector, SDG) %>%
  summarise(Avg_TFIDF = mean(TFIDF_Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(sdg_mapping_tfidf, by = "SDG") %>%
  # Filter out SDGs with low total TF-IDF
  group_by(SDG_name) %>%
  filter(!is.na(SDG_name) & sum(Avg_TFIDF) > 0) %>%
  ungroup() %>%
  ggplot(aes(x = Sector, y = SDG_name, fill = Avg_TFIDF)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", name = "Avg TF-IDF") +  # Using plasma for better contrast
  labs(
    title = "Intensity of SDG Mentions Across Sectors",
    x = "Sector",
    y = "SDG Categories"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right"
  )
sdg_sector_tfidf_heatmap

sdg_region_tfidf <- subset_df %>%
  pivot_longer(
    cols = starts_with("tfidf_SDG"),
    names_to = "SDG",
    values_to = "TFIDF_Value"
  ) %>%
  group_by(Region, SDG) %>%
  summarise(Avg_TFIDF = mean(TFIDF_Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(sdg_mapping_tfidf, by = "SDG") %>%
  # Filter out SDGs with low total TF-IDF
  group_by(SDG_name) %>%
  filter(!is.na(SDG_name) & sum(Avg_TFIDF) > 0) %>%
  ungroup()

ggplot(sdg_region_tfidf, aes(x = Region, y = SDG_name, fill = Avg_TFIDF)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", name = "Avg TF-IDF") +
  labs(
    title = "Intensity of SDG Mentions Across Region",
    x = "Sector",
    y = "SDG Categories"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "right" 
  )

# ===================== GRI ESG Visualizations =====================

# 3. ESG Mentions by Sector - Stacked Bar with Percentage
esg_sector_pct_tfidf_plot <- subset_df %>%
  pivot_longer(
    cols = c("tfidf_environmental_keywords", 
             "tfidf_social_keywords", 
             "tfidf_governance_keywords"),
    names_to = "GRI_Category",
    values_to = "TFIDF_Value"
  ) %>%
  group_by(Sector, GRI_Category) %>%
  summarise(Avg_TFIDF = mean(TFIDF_Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(esg_mapping_tfidf, by = "GRI_Category") %>%
  filter(!is.na(ESG_name)) %>%
  group_by(Sector) %>%
  mutate(Percentage = Avg_TFIDF / sum(Avg_TFIDF) * 100) %>%
  ggplot(aes(x = Sector, y = Percentage, fill = ESG_name)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), color = "white", size = 3) +
  labs(
    title = "Relative ESG by Sector",
    x = "Sector", 
    y = "Percentage of ESG (TF-IDF)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

esg_sector_pct_tfidf_plot

####################### Research Question 2 #######################################

# Analysis of SDG-GRI Alignment 
# Analyze which SDGs are more closely aligned with GRI disclosures

# 1. Extract the TFIDF columns for analysis
tfidf_sdg_cols <- paste0("tfidf_SDG_", 1:17)
tfidf_gri_cols <- c("tfidf_environmental_keywords", "tfidf_social_keywords", "tfidf_governance_keywords")

# 2. Calculate correlation matrix between SDGs and GRI categories
sdg_gri_cor <- cor(subset_df[, tfidf_sdg_cols], subset_df[, tfidf_gri_cols], use = "pairwise.complete.obs")

# 3. Create a heatmap of correlations
# Format data for plotting
sdg_gri_cor_melted <- melt(sdg_gri_cor)
colnames(sdg_gri_cor_melted) <- c("SDG", "GRI_Category", "Correlation")

# Fix the SDG column to match with sdg_mapping_tfidf
sdg_gri_cor_melted$SDG_num <- as.numeric(gsub("tfidf_SDG_", "", sdg_gri_cor_melted$SDG))
sdg_gri_cor_melted$SDG_name <- sdg_mapping_tfidf$SDG_name[match(sdg_gri_cor_melted$SDG_num, 
                                                                as.numeric(gsub("tfidf_SDG_", "", sdg_mapping_tfidf$SDG)))]
# Add ESG names
sdg_gri_cor_melted$ESG_name <- esg_mapping_tfidf$ESG_name[match(sdg_gri_cor_melted$GRI_Category, 
                                                                esg_mapping_tfidf$GRI_Category)]
head(sdg_gri_cor_melted)

# Plot correlation heatmap
ggplot(sdg_gri_cor_melted, aes(x = ESG_name, y = SDG_name, fill = Correlation)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", name = "Correlation") +
  theme_minimal() +
  labs(
    title = "Correlation between SDGs and GRI Categories",
    x = "GRI Categories (ESG)",
    y = "Sustainable Development Goals"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, size = 8),
    axis.text.y = element_text(size = 8)
  )

# Identify which SDGs align most with each GRI category
# Function to find top SDGs for each GRI category
get_top_sdgs <- function(sdg_gri_cor, top_n = 5) {
  result <- list()
  
  for (col in 1:ncol(sdg_gri_cor)) {
    col_name <- colnames(sdg_gri_cor)[col]
    cor_values <- sdg_gri_cor[, col]
    top_indices <- order(cor_values, decreasing = TRUE)[1:top_n]
    
    top_sdgs <- rownames(sdg_gri_cor)[top_indices]
    top_cors <- cor_values[top_indices]
    
    # Extract SDG numbers for matching
    sdg_numbers <- as.numeric(gsub("tfidf_SDG_", "", top_sdgs))
    
    # Match with SDG names
    sdg_names <- sapply(sdg_numbers, function(num) {
      idx <- which(as.numeric(gsub("tfidf_SDG_", "", sdg_mapping_tfidf$SDG)) == num)
      if(length(idx) > 0) {
        return(sdg_mapping_tfidf$SDG_name[idx])
      } else {
        return(NA)
      }
    })
    
    result[[col_name]] <- data.frame(
      SDG = top_sdgs,
      SDG_num = sdg_numbers,
      Correlation = top_cors,
      SDG_name = sdg_names
    )
  }
  
  return(result)
}

top_sdgs_by_gri <- get_top_sdgs(sdg_gri_cor, top_n = 3)

for (gri_cat in names(top_sdgs_by_gri)) {
  cat("\nTop SDGs aligned with", sub("tfidf_", "", gri_cat), ":\n")
  print(top_sdgs_by_gri[[gri_cat]])
}

# Convert list to data frame
top_sdgs_df <- do.call(rbind, lapply(names(top_sdgs_by_gri), function(gri_cat) {
  df <- top_sdgs_by_gri[[gri_cat]]
  df$ESG_Category <- case_when(
    gri_cat == "tfidf_environmental_keywords" ~ "Environmental",
    gri_cat == "tfidf_social_keywords" ~ "Social",
    gri_cat == "tfidf_governance_keywords" ~ "Governance"
  )
  return(df)
}))

# Ensure proper column names
colnames(top_sdgs_df) <- c("SDG", "SDG_num", "Correlation", "SDG_name", "ESG_Category")

top_sdgs_df$SDG_name_wrapped <- str_wrap(top_sdgs_df$SDG_name, width = 10)

ggplot(top_sdgs_df, aes(x = reorder(SDG_name_wrapped, Correlation), y = Correlation, fill = ESG_Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~ESG_Category, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c("Environmental" = "#2ca25f", "Governance" = "#8856a7", "Social" = "#3182bd")) +
  theme_minimal() +
  labs(
    title = "Top SDG Alignments by ESG Category",
    x = "Sustainable Development Goals",
    y = "Correlation",
    fill = "ESG Category"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 1, size = 8),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )

###############################################################################
# STEP 5: REPORTING
# - 5.1 Summaries for Stakeholders
# - 5.2 Final Data Export / Dashboard Prep
###############################################################################

## 5.1 Summaries for Stakeholders ----
# 1) Summarize the average mention of each SDG again, but in a quick table form
sdg_summary_table <- subset_df %>%
  select(starts_with("tfidf_SDG_")) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "SDG_Column", values_to = "Avg_TFIDF") %>%
  arrange(desc(Avg_TFIDF))

print(sdg_summary_table)

# 2) Summarize the average mention of ESG categories
esg_summary_table <- subset_df %>%
  select(tfidf_environmental_keywords, tfidf_social_keywords, tfidf_governance_keywords) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "ESG_Column", values_to = "Avg_TFIDF") %>%
  arrange(desc(Avg_TFIDF))

print(esg_summary_table)

## 5.2 Final Data Export or Dashboard Prep ----
write.csv(subset_df, "final_sustainability_data.csv", row.names = FALSE)
