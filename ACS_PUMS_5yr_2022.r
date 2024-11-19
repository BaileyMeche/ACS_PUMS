# DEPENDENCIES
install.packages("rlang")    
install.packages("jsonlite")
install.packages("httr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("Matrix")
install.packages("curl", type = "win.binary")
install.packages("pillar", type = "win.binary")
install.packages("languageserver")
install.packages("tidyr")

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(Matrix)
library(tidyr)

# API call to Census Data
api_key <- ""  # Add API key here
url_1 <- paste0("https://api.census.gov/data/2022/acs/acs5/pums?get=RACAIAN,RACASN,RACBLK,RACNH,RACPI,RACSOR,RACWHT,HISP,SCHL,PWGTP,PUMA20&for=state:04&key=", api_key)

### API URL
print(paste("Requesting data from:", url_1))

response_1 <- GET(url_1)

### Check if response is successful
if (status_code(response_1) == 200) {
  data_1 <- fromJSON(content(response_1, "text"))
  
  # Convert JSON data to a data frame
  headers_1 <- data_1[1, ]
  rows_1 <- data_1[-1, ]
  full_data <- as.data.frame(rows_1, stringsAsFactors = FALSE)
  names(full_data) <- headers_1
  
  # Convert necessary columns to numeric
  full_data <- full_data %>%
    mutate(across(c("RACAIAN", "RACASN",'RACBLK','RACNH','RACPI','RACSOR','RACWHT','HISP', "SCHL", "PWGTP", "PUMA20", "state"), as.numeric))

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
# Assuming full_data contains the original columns as described

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

# Construct educ variable from SCHL "Educational attainment" variable 
full_data <- full_data %>%
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

# Select only the necessary columns and rename the dataset as required
custom_data <- full_data %>%
  select(race, educ, PWGTP, PUMA20, state)
str(custom_data)


# Histograms for 'race', 'educ', 'PWGTP', 'PUMA20'
ggplot(custom_data) +
  geom_bar(aes(x = race), fill = "red", color = "black") +
  labs(title = "Histogram of Race Variable", x = "Race", y = "Count") +
  theme_minimal()

ggplot(custom_data) +
  geom_histogram(aes(x = educ), bins = 20, fill = "blue", color = "black") +
  labs(title = "Histogram of educ", x = "educ", y = "Frequency") +
  theme_minimal()

ggplot(custom_data) +
  geom_histogram(aes(x = PWGTP), bins = 20, fill = "green", color = "black") +
  labs(title = "Histogram of PWGTP", x = "PWGTP", y = "Frequency") +
  theme_minimal()

ggplot(custom_data) +
  geom_histogram(aes(x = PUMA20), bins = 20, fill = "grey", color = "black") +
  labs(title = "Histogram of PUMA20", x = "PUMA20", y = "Frequency") +
  theme_minimal()


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



# Consolidation calculation by PUMA
compute_consolidation_by_puma <- function(df, puma_col = "PUMA20") {
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
    
    results <- rbind(results, data.frame(PUMA20 = group$PUMA20[1], State = group$state[1], Consolidation = consolidation))
  }
  
  results
}

compute_overall_consolidation <- function(df) {
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

### EXECUTE 

# Running Consolidation Calculations
test_df <- custom_data  # Adjust sample size as needed
puma_consolidation <- compute_consolidation_by_puma(test_df)

# Plot Consolidation Levels by PUMA
ggplot(puma_consolidation, aes(x = factor(PUMA20), y = Consolidation)) +
  geom_bar(stat = "identity", fill = "darkgreen", color = "black") +
  labs(title = "Consolidation Levels for Each Unique PUMA20", x = "PUMA20", y = "Consolidation Level") +
  theme_minimal()

#state consolidation value
overall_consolidation <- compute_overall_consolidation(test_df)
print(overall_consolidation)
