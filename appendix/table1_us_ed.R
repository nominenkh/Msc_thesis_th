
# NOTE: This script is provided for methodological transparency. 
# It processes raw firm-level microdata from LSEG DataStream which cannot be 
# shared publicly in this repository due to commercial licensing restrictions. 
# It outputs the aggregated variables found in the public Master_data.xlsx file.

input_file_path  <- "../data/EX_DEP_US.xlsx"
concordance_file <- "../data/Concordance_NAICS_ISIC.xlsx"

capex_sheet_name <- "CAPEX"
ncfo_sheet_name  <- "NCFO"
wc_sheet_name    <- "WC"
ppe_sheet_name   <- "NPPE"

ID_COL           <- "Identifier"
INDUSTRY_COL     <- "NAICS_Code"
IPO_YEAR_COL     <- "IPO_Year"
MIN_FIRM_COUNT   <- 8
AGE_CUTOFF_YEARS <- 10

decade_periods <- list(
  "1990-1999" = as.character(1990:1999),
  "2000-2009" = as.character(2000:2009),
  "2010-2019" = as.character(2010:2019),
  "2020-2024" = as.character(2020:2024)
)


if(!file.exists(input_file_path)) stop("CRITICAL ERROR: Proprietary input file not found.")

capex_df <- read_excel(input_file_path, sheet = capex_sheet_name)
ncfo_df  <- read_excel(input_file_path, sheet = ncfo_sheet_name)
wc_df    <- read_excel(input_file_path, sheet = wc_sheet_name)
ppe_df   <- read_excel(input_file_path, sheet = ppe_sheet_name)

# 1. CORE CALCULATION (NAICS)


aggregate_flow <- function(df, flow_name, analysis_years) {
  df %>%
    select(all_of(ID_COL), all_of(INDUSTRY_COL), all_of(IPO_YEAR_COL), any_of(analysis_years)) %>%
    pivot_longer(
      cols      = any_of(analysis_years),
      names_to  = "Year",
      values_to = flow_name
    ) %>%
    mutate(
      Year_Numeric     = as.numeric(Year),
      IPO_Year_Numeric = as.numeric(.data[[IPO_YEAR_COL]]),
      Firm_Age         = Year_Numeric - IPO_Year_Numeric,
      Firm_Type        = ifelse(Firm_Age <= AGE_CUTOFF_YEARS, "Young", "Mature")
    ) %>%
    group_by(across(c(ID_COL, INDUSTRY_COL, "Firm_Type"))) %>%
    summarise(
      Total_Flow = sum(.data[[flow_name]], na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    rename(!!paste0("Total_", flow_name) := Total_Flow)
}

calculate_dependence_for_period <- function(capex_data, ncfo_data, wc_data, ppe_data,
                                            analysis_years, period_label) {
  Total_CAPEX_df <- aggregate_flow(capex_data, "CAPEX", analysis_years)
  Total_NCFO_df  <- aggregate_flow(ncfo_data,  "NCFO",  analysis_years)
  Total_WC_df    <- aggregate_flow(wc_data,    "WC",    analysis_years)
  Total_PPE_df   <- aggregate_flow(ppe_data,   "PPE",   analysis_years)
  
  firm_agg_df <- Total_CAPEX_df %>%
    inner_join(Total_NCFO_df, by = c(ID_COL, INDUSTRY_COL, "Firm_Type")) %>%
    inner_join(Total_WC_df,   by = c(ID_COL, INDUSTRY_COL, "Firm_Type")) %>%
    inner_join(Total_PPE_df,  by = c(ID_COL, INDUSTRY_COL, "Firm_Type"))
  
  firm_dependence_df <- firm_agg_df %>%
    mutate(
      Total_Broad_CFO            = Total_NCFO + Total_WC,
      External_Dependence_Ratio  = (Total_CAPEX - Total_Broad_CFO) / Total_CAPEX,
      Investment_Intensity_Ratio = Total_CAPEX / Total_PPE,
      Period = period_label
    ) %>%
    filter(
      !is.na(Total_CAPEX) & Total_CAPEX != 0 &
        !is.na(Total_Broad_CFO) &
        !is.na(Total_PPE) & Total_PPE != 0
    )
  
  industry_dependence <- firm_dependence_df %>%
    group_by(across(c(INDUSTRY_COL, "Firm_Type", "Period"))) %>%
    summarise(
      External_Dependence = median(External_Dependence_Ratio, na.rm = TRUE),
      Capital_Intensity   = median(Investment_Intensity_Ratio, na.rm = TRUE),
      Number_of_Firms     = n(),
      .groups             = "drop"
    ) %>%
    filter(Number_of_Firms >= MIN_FIRM_COUNT)
  
  industry_dependence
}

calculate_and_combine_types <- function(firm_type) {
  type_results_list <- list()
  for (period_name in names(decade_periods)) {
    current_years <- decade_periods[[period_name]]
    decade_result <- calculate_dependence_for_period(
      capex_data = capex_df,
      ncfo_data  = ncfo_df,
      wc_data    = wc_df,
      ppe_data   = ppe_df,
      analysis_years = current_years,
      period_label   = period_name
    )
    if (firm_type != "All") {
      decade_result <- decade_result %>% filter(Firm_Type == firm_type)
    }
    type_results_list[[period_name]] <- decade_result
  }
  bind_rows(type_results_list) %>% mutate(Firm_Type = firm_type)
}

all_companies_dependence    <- calculate_and_combine_types("All")
young_companies_dependence  <- calculate_and_combine_types("Young")
mature_companies_dependence <- calculate_and_combine_types("Mature")

final_heterogeneity_dependence <- bind_rows(
  all_companies_dependence,
  young_companies_dependence,
  mature_companies_dependence
)

# Collapse duplicates within NAICS–Firm_Type–Period
final_heterogeneity_dependence <- final_heterogeneity_dependence %>%
  group_by(NAICS_Code, Firm_Type, Period) %>%
  summarise(
    External_Dependence = mean(External_Dependence, na.rm = TRUE),
    Capital_Intensity   = mean(Capital_Intensity,   na.rm = TRUE),
    Number_of_Firms     = sum(Number_of_Firms,      na.rm = TRUE),
    .groups = "drop"
  )

# 2. PRINT NAICS-LEVEL RESULT

excel_sheets <- list(All_Data_Long = final_heterogeneity_dependence)

for (current_period in unique(final_heterogeneity_dependence$Period)) {
  period_data_long <- final_heterogeneity_dependence %>%
    filter(Period == current_period) %>%
    select(NAICS_Code, Firm_Type, External_Dependence, Capital_Intensity, Number_of_Firms)
  
  if (nrow(period_data_long) == 0) next
  
  ed_wide <- period_data_long %>%
    select(NAICS_Code, Firm_Type, External_Dependence) %>%
    pivot_wider(names_from = Firm_Type, values_from = External_Dependence, names_prefix = "ED_")
  
  ci_wide <- period_data_long %>%
    select(NAICS_Code, Firm_Type, Capital_Intensity) %>%
    pivot_wider(names_from = Firm_Type, values_from = Capital_Intensity, names_prefix = "CI_")
  
  n_wide <- period_data_long %>%
    select(NAICS_Code, Firm_Type, Number_of_Firms) %>%
    pivot_wider(id_cols = NAICS_Code, names_from = Firm_Type,
                values_from = Number_of_Firms, names_prefix = "N_")
  
  final_rz_table <- ed_wide %>%
    left_join(ci_wide, by = "NAICS_Code") %>%
    left_join(n_wide, by = "NAICS_Code") %>%
    select(
      NAICS_Code,
      ED_All, ED_Mature, ED_Young,
      CI_All, CI_Mature, CI_Young,
      N_All
    ) %>%
    arrange(ED_All)
  
  sheet_name <- gsub("-", "_", current_period)
  excel_sheets[[sheet_name]] <- final_rz_table
}


cat(">>> NAICS-LEVEL EXTERNAL DEPENDENCE RESULTS <<<\n")
for (sheet in names(excel_sheets)) {
  if (sheet != "All_Data_Long") {
    cat("\n--- Period:", sheet, "---\n")
    print(excel_sheets[[sheet]], n = Inf)
  }
}


# 3. CONVERT TO ISIC (WEIGHTED)


ed_data <- final_heterogeneity_dependence   

if(!file.exists(concordance_file)) stop("CRITICAL ERROR: Concordance file not found.")
concordance <- read_excel(concordance_file, sheet = "Concordance")

naics_col <- names(concordance)[grepl("naics", names(concordance), ignore.case = TRUE)][1]
isic_col  <- names(concordance)[grepl("isic",  names(concordance), ignore.case = TRUE)][1]

concordance_clean <- concordance %>%
  select(NAICS = all_of(naics_col),
         ISIC_REV3 = all_of(isic_col),
         Industry_Name = 4) %>%
  mutate(
    NAICS      = as.numeric(as.character(NAICS)),
    ISIC_REV3  = as.numeric(as.character(ISIC_REV3)),
    Industry_Name = as.character(Industry_Name)
  ) %>%
  filter(!is.na(NAICS) & !is.na(ISIC_REV3)) %>%
  distinct()

ed_with_isic <- ed_data %>%
  left_join(concordance_clean, by = c("NAICS_Code" = "NAICS")) %>%
  filter(!is.na(ISIC_REV3))

ed_isic_weighted <- ed_with_isic %>%
  group_by(ISIC_REV3, Period, Firm_Type) %>%
  summarise(
    Industry_Name = first(Industry_Name),
    External_Dependence = weighted.mean(External_Dependence,
                                        w = Number_of_Firms, na.rm = TRUE),
    Capital_Intensity = weighted.mean(Capital_Intensity,
                                      w = Number_of_Firms, na.rm = TRUE),
    Number_of_Firms = sum(Number_of_Firms, na.rm = TRUE),
    n_naics_sources = n_distinct(NAICS_Code),
    .groups = "drop"
  ) %>%
  select(ISIC_REV3, Industry_Name, Period, Firm_Type,
         External_Dependence, Capital_Intensity, Number_of_Firms, n_naics_sources)


# 4. PRINT ISIC-LEVEL RESULT


create_rz_table <- function(data, period_name) {
  period_data <- data %>% filter(Period == period_name)
  if (nrow(period_data) == 0) return(NULL)
  
  industry_names <- period_data %>%
    filter(Firm_Type == "All") %>%
    select(ISIC_REV3, Industry_Name) %>%
    distinct()
  
  ed_wide <- period_data %>%
    select(ISIC_REV3, Firm_Type, External_Dependence) %>%
    pivot_wider(names_from = Firm_Type, values_from = External_Dependence, names_prefix = "ED_")
  
  ci_wide <- period_data %>%
    select(ISIC_REV3, Firm_Type, Capital_Intensity) %>%
    pivot_wider(names_from = Firm_Type, values_from = Capital_Intensity, names_prefix = "CI_")
  
  n_wide <- period_data %>%
    filter(Firm_Type == "All") %>%
    select(ISIC_REV3, Number_of_Firms, n_naics_sources) %>%
    rename(N_All = Number_of_Firms, N_NAICS_Sources = n_naics_sources)
  
  ed_wide %>%
    left_join(industry_names, by = "ISIC_REV3") %>%
    left_join(ci_wide, by = "ISIC_REV3") %>%
    left_join(n_wide, by = "ISIC_REV3") %>%
    arrange(desc(ED_All)) %>%
    select(ISIC_REV3, Industry_Name, ED_All, ED_Mature, ED_Young,
           CI_All, CI_Mature, CI_Young, N_All, N_NAICS_Sources)
}

excel_sheets_isic <- list(All_Data_Long_ISIC = ed_isic_weighted)

for (p in sort(unique(ed_isic_weighted$Period))) {
  tab <- create_rz_table(ed_isic_weighted, p)
  if (!is.null(tab) && nrow(tab) > 0) {
    sheet_name <- gsub("-", "_", p)
    excel_sheets_isic[[sheet_name]] <- tab
  }
}


cat(">>> ISIC-LEVEL EXTERNAL DEPENDENCE (WEIGHTED) <<<\n")

for (sheet in names(excel_sheets_isic)) {
  if (sheet != "All_Data_Long_ISIC") {
    cat("\n--- Period:", sheet, "---\n")
    print(excel_sheets_isic[[sheet]], n = Inf)
  }
}
