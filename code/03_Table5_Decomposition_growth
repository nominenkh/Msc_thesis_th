
# TABLE 6: Decomposition of Growth


# 1. DATA IMPORT

data_path <- "../data/Master_data.xlsx"
estab_path <- "../data/UNIDO_Establishments.xlsx"

growth <- read_excel(data_path, sheet = "growth")
ex_dep <- read_excel(data_path, sheet = "ex_dep") %>% filter(firm_type == "All")
fin_dev <- read_excel(data_path, sheet = "fin_dev")
estab   <- read_excel(estab_path)

# 2. CALCULATE EXTENSIVE MARGIN GROWTH
dec_bands <- list(
  "1990-1999" = c(1990, 1999), 
  "2000-2009" = c(2000, 2009), 
  "2010-2019" = c(2010, 2019), 
  "2020-2024" = c(2020, 2023)
)

estab_dec <- lapply(names(dec_bands), function(p) {
  yrs <- dec_bands[[p]]
  estab %>% 
    rename(country_code = Country_Code, isic_code = ISIC, N = Establishments) %>%
    mutate(N = as.numeric(N)) %>%
    filter(Year >= yrs[1], Year <= yrs[2]) %>%
    group_by(country_code, isic_code) %>%
    summarise(N0 = N[Year == min(Year)][1], N1 = N[Year == max(Year)][1], .groups = "drop") %>%
    mutate(ext_growth = ifelse(N0 > 0 & N1 > 0, (log(N1) - log(N0)) / (yrs[2] - yrs[1]), NA), period = p)
}) %>% bind_rows()

# 3. MERGE AND DEFINE INTERACTION
df_final <- growth %>%
  left_join(ex_dep, by = c("isic_code", "period")) %>%
  left_join(fin_dev, by = c("country_code", "period")) %>%
  left_join(estab_dec, by = c("country_code", "isic_code", "period")) %>%
  mutate(
    interaction = as.numeric(external_dependence) * as.numeric(totcap_gdp),
    int_growth = growth - ext_growth
  ) %>%
  # ADDED FIX: Force a consistent sample by dropping any NAs across all 3 growth metrics
  filter(!is.na(interaction), !is.na(industry_share), 
         !is.na(growth), !is.na(ext_growth), !is.na(int_growth))

# 4. REGRESSION FUNCTION 
run_model <- function(data, y_var, p_filter) {
  d <- data %>% filter(period == p_filter, !is.na(.data[[y_var]]))
  m <- plm(as.formula(paste(y_var, "~ industry_share + interaction")),
           data = pdata.frame(d, index = "country_code"), model = "within")
  se <- sqrt(diag(vcovHC(m, type = "HC1", cluster = "group")))
  return(list(model = m, se = se))
}

# 5. RUN ALL MODELS
periods <- names(dec_bands)
margins <- list(total = "growth", extensive = "ext_growth", intensive = "int_growth")
results <- list()

for(m_name in names(margins)) {
  for(p in periods) {
    results[[paste(m_name, p, sep="_")]] <- run_model(df_final, margins[[m_name]], p)
  }
}

# 6. OUTPUT PANELS
print_panel <- function(m_type, panel_label, is_last = FALSE) {
  mods <- lapply(periods, function(p) results[[paste(m_type, p, sep="_")]]$model)
  ses  <- lapply(periods, function(p) results[[paste(m_type, p, sep="_")]]$se)
  
  stargazer(
    mods, se = ses, type = "text",
    column.labels = periods,
    dep.var.labels.include = FALSE, model.numbers = FALSE,
    covariate.labels = c("Industry's initial share", "Interaction (ED x total capitalization)"),
    omit.stat = c("f", "ser"), # Keep Adjusted R2
    keep.stat = c("n", "rsq", "adj.rsq"),
    add.lines = list(c(paste0(panel_label), rep("", length(periods)))),
    digits = 3,
    star.cutoffs = c(0.1, 0.05, 0.01),
    notes = if(is_last) "Standard errors are clustered at the country level and reported in parentheses." else NULL,
    notes.append = FALSE
  )
}

# Print the three separate blocks
print_panel("total", "Panel A: Total Growth")
print_panel("extensive", "Panel B: Extensive Margin")
print_panel("intensive", "Panel C: Intensive Margin", is_last = TRUE)
