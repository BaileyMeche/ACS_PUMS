# dependencies
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
install.packages("plotly")
install.packages("usmap")

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(Matrix)
library(tidyr)
library(plotly)
library(usmap)
library(purrr)

# Define the API key (replace with your actual API key)
api_key <- ""

# List of state codes to process (replace as needed)
states_list <- list(
  "01" = "Alabama/AL",
  "49" = "Utah/UT",
  "21" = "Kentucky/KY",
  "26" = "Michigan/MI",
  "29" = "Missouri/MO",
  "32" = "Nevada/NV",
  "34" = "New Jersey/NJ",
  "08" = "Colorado/CO",
  "51" = "Virginia/VA",
  "39" = "Ohio/OH",
  "02" = "Alaska/AK",
  "46" = "South Dakota/SD",
  "04" = "Arizona/AZ",
  "06" = "California/CA",
  "55" = "Wisconsin/WI",
  "15" = "Hawaii/HI",
  "22" = "Louisiana/LA",
  "30" = "Montana/MT",
  "47" = "Tennessee/TN",
  "48" = "Texas/TX",
  "09" = "Connecticut/CT",
  "50" = "Vermont/VT",
  "53" = "Washington/WA",
  "17" = "Illinois/IL",
  "20" = "Kansas/KS",
  "35" = "New Mexico/NM",
  "36" = "New York/NY",
  "10" = "Delaware/DE",
  "11" = "District of Columbia/DC",
  "12" = "Florida/FL",
  "56" = "Wyoming/WY",
  "16" = "Idaho/ID",
  "25" = "Massachusetts/MA",
  "27" = "Minnesota/MN",
  "42" = "Pennsylvania/PA",
  "45" = "South Carolina/SC",
  "13" = "Georgia/GA",
  "23" = "Maine/ME",
  "24" = "Maryland/MD",
  "28" = "Mississippi/MS",
  "37" = "North Carolina/NC",
  "41" = "Oregon/OR",
  "05" = "Arkansas/AR",
  "19" = "Iowa/IA",
  "31" = "Nebraska/NE",
  "33" = "New Hampshire/NH",
  "44" = "Rhode Island/RI",
  "54" = "West Virginia/WV",
  "18" = "Indiana/IN",
  "38" = "North Dakota/ND",
  "40" = "Oklahoma/OK"
)
state_codes <- names(states_list)
state_consolidation_results <- numeric(length(state_codes))

source("ACS_PUMS_1yr_reduced_private.R")  # Load the custom function from the consolidation script

years = c(2005, 2010, 2015, 2021)
ACS_variables_2012 <- c("RACAIAN", "RACASN",'RACBLK','RACNH','RACPI','RACSOR','RACWHT','HISP', "SCHL", "PWGTP", "PUMA", "state") # YEARS 2012 TO PRESENT 
ACS_variables_2011 <- c("RAC1P",'HISP', "SCHL", "PWGTP", "PUMA", "state") # YEARS 2011 AND BEFORE 

### FUNCTIONS 
# Function to fetch data and compute consolidation for a specific state code
fetch_and_compute_consolidation <- function(state_code, year) {
  variables_list = ACS_variables_2011 
  if (year > 2011) { variables_list = ACS_variables_2012  }
  url <- paste0("https://api.census.gov/data/", as.character(year), "/acs/acs1/pums?get=", 
                paste(variables_list[-length(variables_list)], collapse = ","),
                "&for=state:", state_code, "&key=", api_key)
  full_data <- NULL 
  
  # Fetch the data
  response <- GET(url)
  
  if (status_code(response) == 200) {
    data_1 <- fromJSON(content(response, "text"))

    ### DATA CLEANING 
    # Convert JSON data to a data frame
    headers_1 <- data_1[1, ]
    rows_1 <- data_1[-1, ]
    full_data <- as.data.frame(rows_1, stringsAsFactors = FALSE)
    names(full_data) <- headers_1
    
    # Convert necessary columns to numeric
    full_data <- full_data %>%
      mutate(across(all_of(variables_list), as.numeric))
    
    # Check for missing data
    missing_data <- sapply(full_data, function(x) sum(is.na(x)))
    original_row_count <- nrow(full_data)
    full_data <- full_data %>% drop_na()
    rows_deleted <- original_row_count - nrow(full_data)

    # Calculate the consolidation score for the state using the function from consolidation_script.R
    consolidation_score <- state_level_consolidation(full_data, year)
    return(consolidation_score)
  } else {
    warning(paste("Failed to fetch data for state:", state_code))
    return(NA)
  }
}

# Initialize results dataframe
state_consolidation_results_df <- data.frame(
  State = state_codes)

# Loop through each state code, fetch data, compute consolidation, and store results
for (year in years) {
  state_consolidation_results <- numeric(length(state_codes))
  
  for (i in seq_along(state_codes)) {
    state_code <- state_codes[i]
    cat("Processing state:", state_code, "\n")
    state_consolidation_results[i] <- fetch_and_compute_consolidation(state_code, year)
  }
  
  state_consolidation_results_df[[as.character(year)]] <- state_consolidation_results
}

# Display all state consolidation results
#print(state_consolidation_results_df)

#### MAPS 
### state figure for 2021 consolidation values 
# Join state names
state_names_df <- data.frame(State = names(states_list), Name = unlist(states_list))
map_data_2021 <- state_consolidation_results_df %>%
  left_join(state_names_df, by = "State") %>%
  mutate(Abbr = sub(".*/", "", Name)) %>%
  select(Abbr, '2021')

# Create a mapping of state abbreviations to full state names
abbr_to_state <- data.frame(
  Abbr = state.abb,
  state = tolower(state.name)  # Convert to lowercase for compatibility with usmap
)

# Add Washington, DC manually as it isn't in state.abb/state.name
abbr_to_state <- abbr_to_state %>%
  add_row(Abbr = "DC", state = "district of columbia")

# Merge state names with the data
map_data_2021 <- map_data_2021 %>%
  left_join(abbr_to_state, by = "Abbr")

# Plot US map with 2021 values
plot_usmap(data = map_data_2021, values = "2021", regions = "state") +
  scale_fill_continuous(name = "2021 Values", low = "red", high = "white") +
  theme(legend.position = "right") +
  labs(title = "US Map with 2021 Values")


### US Map for percent change  
state_consolidation_results_df <- state_consolidation_results_df %>%
  mutate(State = as.character(State))

abbr_to_state <- data.frame(
  State = as.character(names(states_list)),  # Ensure the State codes are character
  state_name = tolower(sapply(states_list, function(x) strsplit(x, "/")[[1]][1]))  # Extract and lowercase state names
)

percent_changes_df <- state_consolidation_results_df %>%
  mutate(
    Percent_Change_2005_2010 = (`2010` - `2005`) / `2005` * 100,
    Percent_Change_2010_2015 = (`2015` - `2010`) / `2010` * 100,
    Percent_Change_2015_2021 = (`2021` - `2015`) / `2015` * 100
  ) %>%
  select(State, Percent_Change_2005_2010, Percent_Change_2010_2015, Percent_Change_2015_2021) %>%
  left_join(abbr_to_state, by = "State") %>%
  select(state_name, Percent_Change_2005_2010, Percent_Change_2010_2015, Percent_Change_2015_2021)

map_data <- percent_changes_df %>%
  select(state_name, Percent_Change_2015_2021) %>%
  rename(state = state_name)  # Rename for compatibility with usmap
map_data$state <- tolower(map_data$state)  # Ensure lowercase state names

plot_usmap(data = map_data, values = "Percent_Change_2015_2021", regions = "states") +
  scale_fill_continuous(name = "Percent Change (2015-2021)", low = "red", high = "black") +
  theme(legend.position = "right") +
  labs(title = "US Map of Percent Change (2015-2021)")