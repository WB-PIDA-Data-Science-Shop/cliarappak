
make_colnames_unique <- function(data) {
  #unique columns on original data
  CN<- make.unique(names(data))
  
  #Dimensionality Check
  if (length(CN) == ncol(data)) {
    names(data) <- CN
  } else {
    stop("ERROR: Dimensionality Mismatch in initial column uniqueness check. Check Original Dataset.")
  }
  
  return(data)
}
#=================Function that preps dta
dta_prep <- function(data, des_names=FALSE, db_variables) {
# data.table::setnames() below mutates by reference, even on a plain
# data.frame -- without this copy, calling *_prep() would silently corrupt
# the shared `pre_download_data()` reactive's cached value for the next
# format a user downloads in the same session (e.g. rds then csv then dta).
data <- data.table::copy(data)
#Handles descriptive data names switch
if (des_names==TRUE){
  db_data <- data %>%
    # Rename columns to ones in db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = substr(as.character(db_variables$var_name), 1, 32),
      skip_absent = TRUE
    )}
else{db_data<-data}
  
#Duplication in original data check
 if (any(duplicated(names(db_data)))){
   prepared_dta_data<- make_colnames_unique(db_data)
 }
  else{prepared_dta_data <- db_data}
  
  #Old data names before preparation
  column_names <- names(prepared_dta_data)

  #We are shortening to 30 characters because of the added characters in uniqueness code below
  truncated_names <- substr(column_names, 1, 30)
  
  # Make sure names are unique (This function uses illegal "." characters)
  unique_names <- make.unique(truncated_names)
  
  # Replace invalid characters for .dta files
  cleaned_dta_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
  
  
  # Rename columns in the final data
  setnames(prepared_dta_data, old = column_names, new = cleaned_dta_names)

  return(prepared_dta_data)
}


#=========================== RDS PREP
rds_prep <- function(data, des_names, db_variables) {
  # See dta_prep() for why this copy is required -- setnames() mutates by reference.
  data <- data.table::copy(data)
  #Handles descriptive data names switch
  if (des_names==TRUE){
    Rdb_data <- data %>%
      # Rename columns to ones in db_variables
      setnames(
        old = as.character(db_variables$variable),
        new = substr(as.character(db_variables$var_name), 1, 32),
        skip_absent = TRUE
      )}
  else{Rdb_data<-data}
  
  #Duplication in original data check
  if (any(duplicated(names(Rdb_data)))){
    prepared_rds_data<- make_colnames_unique(Rdb_data)
  }
  else{prepared_rds_data <- Rdb_data}
  
  # Set Column Names for cleaning
  column_names <- names(prepared_rds_data)
  
  # Make sure names are unique
  unique_names <- make.unique(column_names)
  
  # Replace invalid characters
  cleaned_rds_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
  
  
  # Rename columns in the data
  setnames(prepared_rds_data, old = column_names, new = cleaned_rds_names)
  
  # Return the prepared data
  
  return(prepared_rds_data)
}
#================ CSV Prep
csv_prep<- function(data,des_names, db_variables){
  # See dta_prep() for why this copy is required -- setnames() mutates by reference.
  data <- data.table::copy(data)
  if (des_names==TRUE){
    XL_data <- data %>%
    # Rename columns to ones in db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = as.character(db_variables$var_name),
      skip_absent = TRUE)}
  else{
    XL_data<-data}
    # Set Column Names for cleaning
  column_names <- names(XL_data)
    
    # Make sure names are unique
  unique_names <- make.unique(column_names)
    
    # Replace invalid characters
  cleaned_XL_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")
    
    
    # Rename columns in the data
  setnames(XL_data, old = column_names, new = cleaned_XL_names)
    
    # Return the prepared data
    
  return(XL_data)
  }
  
check_data <-function(data,country,indicator_1, indicator_2=NULL, db_variables){

  #One variable scenario (used in bar and time trend)
  if (is.null(indicator_2)){
    #Establishes the variable (column) searched for
    var <-
      db_variables %>%
      filter(var_name == indicator_1) %>%
      pull(variable)

    #Establishes the base country (row) that is in use
    indicator_val <-
      data %>%
      filter(country_name == country) %>%
      pull(var)

    #Returns a boolean on whether or not the column is null for the selected indicator1
    return(is.na(indicator_val))}

  #Two indicator scenario used in bivariate correlation
  else{

    # Extracts the columns for the given indicators
    vars <- db_variables %>%
      filter(var_name %in% c(indicator_1, indicator_2)) %>%
      pull(variable)

    # Checks that both indicators are in data
    if (all(vars %in% names(data))) {
      has_na <- data %>%
        filter(country_name == country) %>%
        select(all_of(vars)) %>%
        summarise(across(everything(), ~ any(is.na(.)), .names = "has_na_{col}")) %>%
        summarise(across(starts_with("has_na_"), any)) %>%
        unlist() %>%
        any()

      return(has_na)
    } else {
      # If any of the variables do not exist in dv_variables, this indicates missing data
      return(TRUE)
  }
}}
#============================== Raw Data Check data function (used in time trends plot)
trends_check_data <- function(start_year, end_year, country, var, raw_data) {

  # Filter the data down to start and end years and current comparison country
    comp_data <- raw_data %>%
    select(country_name, Year, !!sym(var)) %>%  # Select only necessary columns
    filter(Year >= start_year & Year <= end_year, country_name == country)
    
  # Check for nulls in the input variable column
  any_nulls <- comp_data %>%
    summarise(has_nulls = any(is.na(!!sym(var)))) %>%
    pull(has_nulls)
  
  return(any_nulls)  # Return TRUE if any nulls are found, FALSE otherwise
}

#====================
#This Function checks data for the map section

check_spatial_data <-function(data,indicator,db_variables){

  var <-
    db_variables %>%
    filter(var_name == indicator) %>%
    pull(variable)

  indicator_val <-
    data %>%
    pull(paste0("value_",var))

  return(all(is.na(indicator_val)))
}