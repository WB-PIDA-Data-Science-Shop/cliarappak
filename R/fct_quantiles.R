# Function: def_quantiles
#
# This function calculates quantiles for a given set of variables, filters the dataset
# to include relevant countries and variables, and categorizes the variables based on
# percentile ranks. The function also handles missing data by identifying and excluding
# variables with missing values for the base country. It computes the quantile thresholds 
# based on the specified `threshold` (Default or Terciles) and then classifies the values
# as 'Weak', 'Emerging', or 'Strong' based on their percentile ranks.

def_quantiles <- function(data, base_country, country_list, comparison_countries, vars, variable_names,threshold) {
  
# List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

# List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names 

# List final relevant variables: those selected, minus those missing
# 
if(length(na_indicators) > 0){
  variables <-
    setdiff(vars, na_indicators)
  
  variables <-
    intersect(variables, names(data))
}else{
  variables <- vars
}


# This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% c(base_country, comparison_list$country_name)
    ) %>%
    select(
      country_name,
      any_of(variables)
    )

# Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = any_of(variables),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

if (threshold=="Default"){
    cutoff<-c(25,50)
}else if (threshold=="Terciles")
{
  cutoff<-c(33,66)
}
  
# Calculate quantiles
  quantiles <-
    quantiles %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q_lv_25 = quantile(value,c(0.25)),
      q_lv_75 = quantile(value,c(0.75)),
      q_cutoff1 = quantile(value, c(cutoff[1]/100)),
      q_cutoff2 = quantile(value, c(cutoff[2]/100)),
      status = case_when(
        dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
        dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
        dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",100-cutoff[2],"%)")
      ),
      nrank = min_rank(-value)
    ) %>%
    ungroup %>%
    rename(dtf = value)

  # Remove indicators where there is too little variance
  low_variance_indicators <-
    quantiles %>%
    filter(country_name == base_country & q_lv_25==q_lv_75) %>%
    select(variable) %>%
    unlist
  
  low_variance_indicators <- low_variance_indicators[!grepl("_avg", low_variance_indicators)]
  

  quantiles <-
    quantiles %>%
    filter(!(variable %in% low_variance_indicators))

}

# Function: def_quantiles_dyn
#
# This function calculates quantiles with dynamic adjustments for missing data.
# It handles missing variables by identifying those with 100% missing data for
# the base country and excluding them from the analysis. Similar to `def_quantiles`, 
# it computes the quantile thresholds based on the specified `threshold` (Default or Terciles).
# The function also filters the dataset to include only relevant variables and categorizes 
# the values as 'Weak', 'Emerging', or 'Strong' based on their percentile ranks.


def_quantiles_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names,threshold) {
  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)
  
  
  # List all variables that are missing for the base country -- these will be removed from the data
  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) 
  
  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])
  
  na_indicators <- na_indicators[!grepl("_avg", na_indicators)]
  
  
  # List final relevant variables: those selected, minus those missing
  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }

  
  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% c(base_country, comparison_list$country_name)
    ) %>%
    select(
      country_name,
      year,
      any_of(variables)
    )
  
  quant_vars <- names(quantiles)[names(quantiles) %in% variables]
  
  # Merge with variable dictionary
  quantiles <-
    quantiles %>%
    
    # Make long per indicator
    pivot_longer(
      cols = any_of(quant_vars),
      names_to = "variable"
    ) %>%
    
    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )
  
  if (threshold=="Default"){
    cutoff<-c(25,50)
  }else if (threshold=="Terciles")
  {
    cutoff<-c(33,66)
  }
  
  # Calculate quantiles
  quantiles <-
    quantiles %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Calculate relevant indicators
    group_by(variable, var_name, year) %>%
    mutate(
      dtt = percent_rank(value),
      q_lv_25 = quantile(value, c(0.25)),
      q_lv_75 = quantile(value, c(0.75)),
      q_cutoff1 = quantile(value, c(cutoff[1]/100)),
      q_cutoff2 = quantile(value, c(cutoff[2]/100)),
      status = case_when(
        dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
        dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
        dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",100-cutoff[2],"%)")
      ),
      nrank = min_rank(-value)
    ) %>%
    ungroup %>%
    rename(dtf = value) %>% 
    # Remove indicators where there is too little variance
    mutate(todrop = ifelse(country_name == base_country & q_lv_25==q_lv_75, 1, 0)) %>% 
    filter(todrop != 1) %>% 
    select(-todrop)
  

}


# Functions: low_variance and low_variance_dyn
# ---------------------------------------------------------
# These functions are designed to calculate quantiles for a selected base country
# and a comparison group of countries. The quantiles are calculated for a set of 
# variables, and the functions categorize the variables into "Weak", "Emerging", 
# or "Strong" based on their percentile rank. Missing data is handled dynamically 
# to ensure that only relevant variables are considered in the quantile calculation.
# 
# Parameters:
# - data: A dataset containing country-specific data for analysis.
# - base_country: The country of interest for which quantiles are calculated.
# - country_list: A dataset with country details, used to filter comparison countries.
# - comparison_countries: A vector of country names to compare against the base country.
# - vars: A vector of variable names to evaluate.
# - variable_names: A dataframe mapping variable codes to human-readable names for better interpretability.
# 
# Outputs:
# - low_variance: Returns a list of variables from the base country that have 
#                 identical 25th and 75th percentiles, indicating low variance.
# - low_variance_dyn: Similar to low_variance but handles missing variables 
#                     dynamically and ensures variables with complete data are selected.
# ---------------------------------------------------------


low_variance <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

  # List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names 

  # List final relevant variables: those selected, minus those missing
  variables <-
    setdiff(vars, na_indicators)

  variables <-
    intersect(variables, names(data))

  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%

    # Keep only the base and comparison countries
    filter(
      (country_name %in% comparison_list$country_name) | (country_name == base_country)
    ) %>%

    # Keep only selected, non-missing indicators
    select(
      country_name,
      all_of(variables)
    )

  # Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

  # Calculate quantiles
  quantiles <-
    quantiles %>%

    # Remove missing values
    filter(!is.na(value)) %>%

    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(0.25)),
      q75 = quantile(value, c(0.75)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      )
    ) %>%
    ungroup %>%
    rename(dtf = value) %>%
    filter(country_name == base_country & q25==q75) %>%
    select(variable) %>%
    unlist

  return(quantiles)

}

low_variance_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {
  
  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)
  
  
  # List all variables that are missing for the base country -- these will be removed from the data
  # na_indicators <-
  #   data %>%
  #   ungroup() %>%
  #   filter(country_name == base_country) %>%
  #   select(where(is.na)) %>%
  #   names
  
  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) 
  
  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])
  
  
  # List final relevant variables: those selected, minus those missing
  # variables <-
  #   setdiff(vars, na_indicators)
  # 
  # variables <-
  #   intersect(variables, names(data))
  
  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }
  
  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%
    
    # Keep only the base and comparison countries
    filter(
      (country_name %in% comparison_list$country_name) | (country_name == base_country)
    ) %>%
    
    # Keep only selected, non-missing indicators
    select(
      country_name,
      all_of(variables)
    )
  
  # Merge with variable dictionary
  quantiles <-
    quantiles %>%
    
    # Make long per indicator
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable"
    ) %>%
    
    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )
  
  # Calculate quantiles
  quantiles <-
    quantiles %>%
    
    # Remove missing values
    filter(!is.na(value)) %>%
    
    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(0.25)),
      q75 = quantile(value, c(0.75)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      )
    ) %>%
    ungroup %>%
    rename(dtf = value) %>%
    filter(country_name == base_country & q25==q75) %>%
    distinct(variable) %>%
    unlist
  
  return(quantiles)
  
}



# ---------------------------------------------------------
# Functions: missing_var and missing_var_dyn
# ---------------------------------------------------------
# These functions identify and handle variables with missing 
# data for a specified base country within a dataset. They 
# are designed to filter relevant variables and list those 
# with complete data for comparative analysis.
# 
# Parameters:
# - data: A dataset containing country-specific data for analysis.
# - base_country: The country of interest for which missing variables 
#                 are identified.
# - country_list: A dataset with country details, used to filter 
#                 comparison countries.
# - comparison_countries: A vector of country names to compare 
#                         against the base country.
# - vars: A vector of variable names to evaluate.
# - variable_names: A dataframe mapping variable codes to human-readable 
#                   names for better interpretability.
# 
# Outputs:
# - missing_var: Returns a list of relevant variables after excluding 
#                those with missing values for the base country.
# - missing_var_dyn: Extends the functionality of missing_var by 
#                    calculating the proportion of missing values 
#                    and handling cases dynamically.

missing_var <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

  # List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names 

  # List final relevant variables: those selected, minus those missing
  variables <-
    setdiff(vars, na_indicators)

  variables <-
    intersect(variables, names(data))

  # List specific family variables missing
  missing_variables <-
    vars[vars %in% na_indicators] %>%
    data.frame() %>%
    rename("variable"=".") %>%
    left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
    .$var_name

}


missing_var_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {
  
  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)
  
  # List all variables that are missing for the base country -- these will be removed from the data
  # na_indicators <-
  #   data %>%
  #   ungroup() %>%
  #   filter(country_name == base_country) %>%
  #   select(where(is.na)) %>%
  #   names
  
  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country) 
  
  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])
  
  # List final relevant variables: those selected, minus those missing
  # variables <-
  #   setdiff(vars, na_indicators)
  # 
  # variables <-
  #   intersect(variables, names(data))
  
  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }
  
  # List specific family variables missing
  missing_variables <-
    vars[vars %in% na_indicators] %>%
    data.frame() %>%
    rename("variable"=".") %>%
    left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
    .$var_name
  
  missing_variables <- unique(missing_variables)
  
}