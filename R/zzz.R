# These are all dplyr/tidyr non-standard-evaluation column references (data
# frame column names, not real objects), which R CMD check's static analysis
# can't distinguish from genuinely undefined globals. Declaring them here is
# the standard pattern for this -- same approach cliaretl's own R/*.R files
# already use.
utils::globalVariables(c(
  ".", ":=", "Benchmark_dynamic_family_aggregate", "Category", "Countries",
  "Country", "Description", "Family",
  "Grp", "Indicator", "Source", "Year", "benchmarked_ctf", "color",
  "counter", "country_code", "country_group", "country_name", "delta",
  "description", "description_short", "dtf", "dtt", "earliest_value_ctf",
  "earliest_value_rank", "family_name", "family_order", "family_var",
  "group", "group_category", "group_code", "group_name", "has_nulls",
  "income_group", "label", "latest_value_ctf", "latest_value_rank", "lot",
  "n_countries_max", "n_countries_min", "new_labels", "note", "nrank",
  "q25", "q75", "q_cutoff1", "q_cutoff2", "q_lv_25", "q_lv_75", "rank_id",
  "region", "status", "text", "title", "todrop", "type", "value", "var",
  "var_level", "var_name", "var_name2", "variable", "variable_names",
  "wdi_nygdppcapppkd", "year"
))
