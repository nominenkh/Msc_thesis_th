
# TABLE 8: Average annual growth in manufacturing value added in India by external dependence

# 1. DIRECT LINK TO YOUR FILE

file_path <- "../data/Master_data.xlsx"

growth  <- read_excel(file_path, sheet = "growth")
ex_dep  <- read_excel(file_path, sheet = "ex_dep")
fin_dev <- read_excel(file_path, sheet = "fin_dev")

# 2. CLEAN NAMES & TYPES
names(sheet_growth)  <- tolower(names(sheet_growth))
names(sheet_ex_dep)  <- tolower(names(sheet_ex_dep))
names(sheet_fin_dev) <- tolower(names(sheet_fin_dev))

sheet_growth$isic_code <- as.character(sheet_growth$isic_code)
sheet_ex_dep$isic_code <- as.character(sheet_ex_dep$isic_code)


sheet_growth <- sheet_growth %>%
  mutate(period = case_when(
    year >= 1990 & year <= 1999 ~ "1990-1999",
    year >= 2000 & year <= 2009 ~ "2000-2009",
    year >= 2010 & year <= 2019 ~ "2010-2019",
    year >= 2020 & year <= 2024 ~ "2020-2024",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

# Filter Industry Duplicates
if("firm_type" %in% names(sheet_ex_dep)){
  sheet_ex_dep <- sheet_ex_dep %>% filter(firm_type == "All")
}
sheet_ex_dep <- sheet_ex_dep %>% distinct(isic_code, period, .keep_all = TRUE)

full_data <- sheet_growth %>%
  left_join(sheet_ex_dep, by = c("isic_code", "period")) %>%
  left_join(sheet_fin_dev, by = c("country_code", "period"))

if ("IND" %in% unique(full_data$country_code)) {
  india_data <- full_data %>% filter(country_code == "IND")
} else {
  india_data <- full_data %>% filter(country_code == "India")
}

median_cutoff <- median(unique(full_data$external_dependence), na.rm = TRUE)

table_india <- india_data %>%
  mutate(
    Type = ifelse(external_dependence >= median_cutoff, "High Dependence", "Low Dependence")
  ) %>%
 
  filter(!is.na(Type)) %>%  
  group_by(period, Type) %>%
  summarise(
    Avg_Growth = mean(growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Type, values_from = Avg_Growth) %>%
  mutate(
    Difference = `High Dependence` - `Low Dependence`
  )

print(table_india)
