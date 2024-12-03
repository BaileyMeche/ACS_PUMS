# DEPENDENCIES
install.packages("rlang")       # Install updated 'rlang' first
install.packages("jsonlite")
install.packages("httr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("Matrix")
install.packages("curl", type = "win.binary")
install.packages("pillar", type = "win.binary")
install.packages("languageserver")
install.packages("tidyr")
install.packages("Hmisc") # For wtd.mean
install.packages("ineq") # For theil.wtd

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(Matrix)
library(tidyr)
library(Hmisc) # For wtd.mean
library(ineq) # For theil.wtd

# API call to Census Data
api_key <- ""  # Add API key here

year = 2019
state_code = '17'
ACS_variables_2012 <- c("RACAIAN", "RACASN",'RACBLK','RACNH','RACPI','RACSOR','RACWHT','HISP', "SCHL", "PWGTP", "PUMA", "state") # YEARS 2012 TO PRESENT 
ACS_variables_2011 <- c("RAC1P",'HISP', "SCHL", "PWGTP", "PUMA", "state") # YEARS 2011 AND BEFORE 

variables_list = ACS_variables_2012 
if (year > 2021){
  variables_list[11] <- "PUMA20"
} else if (year < 2012) { variables_list = ACS_variables_2011 } 

url_1 <- paste0("https://api.census.gov/data/", as.character(year), "/acs/acs1/pums?get=", 
              paste(variables_list[-length(variables_list)], collapse = ","),
              "&for=state:", state_code, "&key=", api_key)
full_data <- NULL 

### API URL
print(paste("Requesting data from:", url_1))
response_1 <- GET(url_1)
if (status_code(response_1) == 200) {
  data_1 <- fromJSON(content(response_1, "text"))
  
  # Convert JSON data to a data frame
  headers_1 <- data_1[1, ]
  rows_1 <- data_1[-1, ]
  full_data <- as.data.frame(rows_1, stringsAsFactors = FALSE)
  names(full_data) <- headers_1
  
  # Convert necessary columns to numeric
  full_data <- full_data %>%
    mutate(across(all_of(variables_list), as.numeric))
  
  # Debug: Show head of data
  print("Data successfully loaded. Showing head of data:")
  print(head(full_data))
  
  # Debug: Show data structure
  print("Data structure:")
  str(full_data)
  
  # Debug: Summary of numeric columns
  print("Summary of data:")
  summary(full_data)
  
  # Check for missing values
  missing_data <- sapply(full_data, function(x) sum(is.na(x)))
  print("Missing data by column:")
  print(missing_data)
  
  # Filter out rows with any NA values and report rows deleted
  original_row_count <- nrow(full_data)
  full_data <- full_data %>% drop_na()
  rows_deleted <- original_row_count - nrow(full_data)
  print(paste("Rows deleted due to NA values:", rows_deleted))
  
} else {
  print(paste("Failed to fetch data from URL 1:", status_code(response_1)))
}

##BUILD CUSTOM VARIABLES 
full_data <- full_data %>%
  mutate(SCHL = as.numeric(SCHL)) %>%
  mutate(
    educ = case_when(
      SCHL <= 14 ~ 0,             # < HS
      SCHL >= 15 & SCHL <= 17 ~ 1,  # HS (no diploma + diploma + GED)
      SCHL == 18 | SCHL == 19 ~ 2,   # Some college
      SCHL == 20 ~ 3,         # Associate's degree
      SCHL == 21 ~ 4,         # Bachelor's degree
      SCHL >= 22 ~ 5          # Post-college
    )
  )
if (year > 2011) {
  # c("RACAIAN", "RACASN",'RACBLK','RACNH','RACPI','RACSOR','RACWHT','HISP', "SCHL", "PWGTP", "PUMA", "state")
  # Construct the race variable
  full_data <- full_data %>%
    mutate(
      race = case_when(
        RACWHT == 1 & RACAIAN == 0 & RACASN == 0 & RACBLK == 0 & 
          RACNH == 0 & RACPI == 0 & RACSOR == 0 & HISP == 01 ~ "WHITE",
        RACBLK == 1 ~ "BLACK",
        RACASN == 1 ~ "ASIAN",
        HISP != 01 ~ "HISPANIC",
        TRUE ~ "OTHER"  # Any other combination falls into "Other"
      )
    )
} else {
  # ("RAC1P",'HISP', "SCHL", "PWGTP", "PUMA", "state")
  # Construct the race variable
  full_data <- full_data %>%
    mutate(
      race = case_when(
        RAC1P == 1 & HISP == 01 ~ "WHITE",
        RAC1P == 2 ~ "BLACK",
        RAC1P == 6 ~ "ASIAN",
        HISP != 01 ~ "HISPANIC",
        TRUE ~ "OTHER"  # Any other combination falls into "Other"
      )
    )
}
custom_data <- full_data %>%
  select(race, educ, PWGTP, PUMA, state)
str(custom_data)


# Histograms for 'race', 'educ', 'PWGTP', 'PUMA'
# ggplot(custom_data) +
#   geom_bar(aes(x = race), fill = "red", color = "black") +
#   labs(title = "Histogram of Race Variable", x = "Race", y = "Count") +
#   theme_minimal()
# 
# ggplot(custom_data) +
#   geom_histogram(aes(x = educ), bins = 20, fill = "blue", color = "black") +
#   labs(title = "Histogram of educ", x = "educ", y = "Frequency") +
#   theme_minimal()
# 
# ggplot(custom_data) +
#   geom_histogram(aes(x = PWGTP), bins = 20, fill = "green", color = "black") +
#   labs(title = "Histogram of PWGTP", x = "PWGTP", y = "Frequency") +
#   theme_minimal()
# 
# puma_call <- tail(variables_list,2)[1]
# ggplot(custom_data) +
#   geom_histogram(aes(x = puma_call), binwidth = 1, fill = "grey", alpha = 0.6, color = "black") +
#   labs(title = "Histogram of PUMA", x = as.character(puma_call), y = "Frequency") +
#   theme_minimal()


###FUNCTIONS

# Weighted Contingency Table function
build_weighted_contingency_table <- function(df, race_col, edu_col, weight_col) {
  races <- df[[race_col]]
  educations <- df[[edu_col]]
  weights <- df[[weight_col]]
  
  # Ensure both race and educ contain valid categories
  unique_races <- sort(unique(races))
  unique_educations <- sort(unique(educations))
  
  # Map each race and educ value to an index in the unique values
  race_idx <- match(races, unique_races)
  edu_idx <- match(educations, unique_educations)
  
  # Debug: Check if there are any NA indices
  if (any(is.na(race_idx))) {
    warning("Unmatched values in race detected in the rows:")
    print(df[is.na(race_idx), ])
  }
  if (any(is.na(edu_idx))) {
    warning("Unmatched values in educ detected in the rows:")
    print(df[is.na(edu_idx), ])
  }
  
  # Create sparse matrix (ensuring no NA indices)
  if (all(!is.na(race_idx)) && all(!is.na(edu_idx))) {
    contingency_matrix <- sparseMatrix(i = race_idx, j = edu_idx, x = weights,
                                       dims = c(length(unique_races), length(unique_educations)))
    return(list(matrix = contingency_matrix, num_races = length(unique_races), num_educations = length(unique_educations)))
  } else {
    stop("Error: Some values in race or educ do not match expected categories.")
  }
}

### CRAMER'S V CONSOLIDATION 
# Cramér's V calculation function
calculate_cramers_v <- function(contingency_matrix, n, num_races, num_educations) {
  observed <- as.matrix(contingency_matrix)
  row_sums <- rowSums(observed)
  col_sums <- colSums(observed)
  
  expected <- outer(row_sums, col_sums) / n
  chi2_stat <- sum((observed - expected)^2 / expected, na.rm = TRUE)
  
  denom <- n * min(num_races - 1, num_educations - 1)
  if (denom <= 0 || chi2_stat < 0) return(NA)
  sqrt(chi2_stat / denom)
}

# Cramer's V Consolidation calculation by PUMA
compute_CVC_consolidation_by_puma <- function(df, puma_col = "PUMA") {
  results <- data.frame()
  
  grouped_data <- df %>%
    group_by(across(all_of(c(puma_col, "state")))) %>%
    group_split()
  
  for (group in grouped_data) {
    n <- sum(group$PWGTP)
    contingency_result <- build_weighted_contingency_table(group, "race", "educ", "PWGTP")
    num_races <- contingency_result$num_races
    num_educations <- contingency_result$num_educations
    
    if (num_races > 1 && num_educations > 1) {
      consolidation <- calculate_cramers_v(contingency_result$matrix, n, num_races, num_educations)
    } else {
      consolidation <- NA
    }
    
    results <- rbind(results, data.frame(PUMA = group$PUMA[1], State = group$state[1], Consolidation = consolidation))
  }
  
  results
}

compute_overall_CVC_consolidation <- function(df) {
  # Total weight (population size)
  n <- sum(df$PWGTP)
  
  # Build the weighted contingency table for the entire dataset
  contingency_result <- build_weighted_contingency_table(df, "race", "educ", "PWGTP")
  num_races <- contingency_result$num_races
  num_educations <- contingency_result$num_educations
  
  # Calculate Cramér's V only if there is more than one category in both race and educ
  if (num_races > 1 && num_educations > 1) {
    consolidation <- calculate_cramers_v(contingency_result$matrix, n, num_races, num_educations)
  } else {
    consolidation <- NA
  }
  
  # Return the consolidation score for the entire dataset
  return(consolidation)
}

### KRUSKAL WALLIS CONSOLIDATION by_puma
compute_KW_consolidation_by_puma <- function(df, puma_col = "PUMA", race_col = "race", educ_col = "educ") {
  results <- df %>%
    group_by(across(all_of(puma_col))) %>%  # Only group by PUMA, no state grouping
    summarise(
      Consolidation = tryCatch(
        kruskal.test(as.formula(paste(educ_col, "~", race_col)), data = cur_data())$statistic,
        error = function(e) NA
      ),
      .groups = "drop"
    )
  return(results)
}

### BETWEEN-GROUP THEIL INDEX CONSOLIDATION 
compute_between_group_theil_by_puma <- function(df, puma_col = "PUMA", race_col = "race", educ_col = "educ", weight_col = "weight") {
  # Define unique races
  unique_races <- c("WHITE", "BLACK", "ASIAN", "HISPANIC", "OTHER")
  
  # Initialize results dataframe
  results <- data.frame()
  
  # Group data by PUMA
  grouped_data <- df %>%
    group_by(across(all_of(puma_col))) %>%
    group_split()
  
  for (puma_data in grouped_data) {
    puma <- unique(puma_data[[puma_col]])  # Extract PUMA ID
    
    # Initialize storage for weights
    race_weights <- numeric(length(unique_races))
    
    # Calculate weights for each race
    for (z in seq_along(unique_races)) {
      race <- unique_races[z]
      
      # Index individuals of this race
      race_index <- which(puma_data[[race_col]] == race)
      
      if (length(race_index) > 0) {
        # Calculate weight for this race
        specific_race_weight <- wtd.mean(
          puma_data[[race_col]] == race, 
          weights = puma_data[[weight_col]], 
          na.rm = TRUE
        )
        # Store weight
        race_weights[z] <- specific_race_weight
      } else {
        race_weights[z] <- 0
      }
    }
    
    # Construct row for this PUMA
    puma_result <- data.frame(
      PUMA = puma,
      t(setNames(race_weights, unique_races))
    )
    
    # Append to results
    results <- rbind(results, puma_result)
  }
  
  return(results)
}

# Supporting Theil Function
theil_wtd <- function(values, weights) {
  # Compute Theil index with weights
  weighted_mean <- wtd.mean(values, weights, na.rm = TRUE)
  if (weighted_mean <= 0) return(NA)  # Avoid log issues
  theil <- sum(weights * (values / weighted_mean) * log(values / weighted_mean), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  return(theil)
}

### EXECUTE 

# Running CVC Consolidation Calculations
puma_consolidation <- compute_CVC_consolidation_by_puma(custom_data)

#overall CVC consolidation value
overall_CVC_consolidation <- compute_overall_CVC_consolidation(custom_data)
print(overall_CVC_consolidation)

# Plot CVC Consolidation Levels by PUMA
ggplot(puma_consolidation, aes(x = .data[[puma_call]], y = Consolidation)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  labs(title = "Cramer's V Consolidation Levels for Each Unique PUMA", y = "Consolidation Level") +
  theme_minimal()

# Compute KW consolidation by PUMA
puma_consolidation_KW <- compute_KW_consolidation_by_puma(custom_data)
print(puma_consolidation_KW)

# Plot KW Consolidation Levels by PUMA
ggplot(puma_consolidation_KW, aes(x = .data[[puma_call]], y = Consolidation)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  labs(title = "KrusKal Wallis Consolidation Levels for Each Unique PUMA", y = "Consolidation Level") +
  theme_minimal()

# Compute Between-group Theil Index by PUMA
theil_results <- compute_between_group_theil_by_puma(custom_data)
print(theil_results)

# Plot Between-group Theil Consolidation Levels by PUMA
theil_results_long <- theil_results %>%
  pivot_longer(cols = c(WHITE, BLACK, ASIAN, HISPANIC, OTHER), 
               names_to = "Race", 
               values_to = "Consolidation")

ggplot(theil_results_long, aes(x = factor(PUMA), y = Consolidation, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Between-group Theil Consolidation Levels by Race for Each PUMA",
    x = "PUMA",
    y = "Consolidation Level"
  ) +
  theme_minimal()