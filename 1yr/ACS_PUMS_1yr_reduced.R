# state_consolidation handles API call 

##BUILD CUSTOM VARIABLES 
custom_variable_build <- function(full_data, year) {
  # Construct educ variable from SCHL "Educational attainment" variable 
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
  }
  else {
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
  return(custom_data)
}


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

state_level_consolidation <- function(df, year) {
  custom_df <- custom_variable_build(df, year)
  final_value <- compute_overall_consolidation(custom_df)
  return(final_value)
}