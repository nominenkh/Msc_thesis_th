
# TABLE 5: Industry Growth and Financial Development using Alternative Benchmark

source("00_setup.R")

# 1. DATA IMPORT

file_path <- "../data/Master_data.xlsx"

growth <- read_excel(data_path, sheet = "growth")
fin_dev <- read_excel(data_path, sheet = "fin_dev")
ex_dep_us <- read_excel(data_path, sheet = "ex_dep")
ex_dep_eu <- read_excel(data_path, sheet = "ex_dep_eu")

# 2. REGRESSION FUNCTION
run_stargazer_model <- function(growth_data, ed_data, fd_data, period_filter, firm_type_filter) {
  
  ed_filtered <- ed_data %>% 
    filter(period == period_filter, firm_type == firm_type_filter) %>%
    select(isic_code, period, external_dependence)
  
  df <- growth_data %>%
    filter(period == period_filter) %>%
    left_join(ed_filtered, by = c("isic_code", "period")) %>%
    left_join(fd_data %>% select(country_code, period, rule_law), by = c("country_code", "period")) %>%
    mutate(interaction = external_dependence * rule_law) %>%
    filter(!is.na(growth), !is.na(interaction), !is.na(industry_share))
  
  model <- plm(growth ~ industry_share + interaction,
               data = pdata.frame(df, index = "country_code"),
               model = "within")
  
  # clustered standard errors
  rob_se <- sqrt(diag(vcovHC(model, type = "HC1", cluster = "group")))
  
  # Growth Differential rounded to 3 decimal points
  coef_int <- coef(model)["interaction"]
  diff_val <- coef_int * (quantile(df$external_dependence, 0.75, na.rm=T) - 
                            quantile(df$external_dependence, 0.25, na.rm=T)) * (quantile(df$rule_law, 0.75, na.rm=T) - 
                                                                                  quantile(df$rule_law, 0.25, na.rm=T))
  
  return(list(model = model, se = rob_se, diff = format(round(diff_val, 3), nsmall = 3)))
}

# 3. EXECUTION & REPORTING

periods <- c("1990-1999", "2000-2009", "2010-2019", "2020-2024")
benchmarks <- list(
  list(name = "US All", data = ex_dep_us, type = "All"),
  list(name = "EU All", data = ex_dep_eu, type = "All"),
  list(name = "EU Young", data = ex_dep_eu, type = "Young")
)

models_list <- list(); ses_list <- list(); diff_list <- list()

for (bench in benchmarks) {
  for (per in periods) {
    res <- run_stargazer_model(growth, bench$data, fin_dev, per, bench$type)
    tag <- paste(bench$name, per, sep = "_")
    models_list[[tag]] <- res$model
    ses_list[[tag]] <- res$se
    diff_list[[tag]] <- res$diff
  }
}


generate_table <- function(idx_set, period_labels, title_text) {
  stargazer(
    models_list[idx_set], 
    se = ses_list[idx_set],
    type = "text", 
    digits = 3,  # Ensures coefficients are 3 decimals
    title = title_text,
    column.labels = c("US All", "EU All", "EU Young"),
    column.separate = c(2, 2, 2),
    covariate.labels = c("Industry's share", "ED x rule of law"),
    dep.var.labels.include = FALSE,
    model.numbers = FALSE,
    omit.stat = c("f", "ser", "adj.rsq"),
    keep.stat = c("n", "rsq"),
    add.lines = list(
      c("Variable", period_labels),
      c("Differential in real growth rate", unlist(diff_list[idx_set]))
    ),
    star.cutoffs = c(0.1, 0.05, 0.01),
    notes = "Standard errors are clustered at the country level and reported in parentheses. Significance at the 1%, 5%, and 10% levels is indicated by ***, **, and *, respectively.",
    notes.append = FALSE
  )
}

# Block 1 (1990-2009)
idx1 <- c("US All_1990-1999", "US All_2000-2009", "EU All_1990-1999", 
          "EU All_2000-2009", "EU Young_1990-1999", "EU Young_2000-2009")
labs1 <- c("1990-1999", "2000-2009", "1990-1999", "2000-2009", "1990-1999", "2000-2009")
generate_table(idx1, labs1, "Table 5: Industry Growth and Financial Development (1990-2009)")

# Block 2 (2010-2024)
idx2 <- c("US All_2010-2019", "US All_2020-2024", "EU All_2010-2019", 
          "EU All_2020-2024", "EU Young_2010-2019", "EU Young_2020-2024")
labs2 <- c("2010-2019", "2020-2024", "2010-2019", "2020-2024", "2010-2019", "2020-2024")
generate_table(idx2, labs2, "Table 5: Industry Growth and Financial Development (2010-2024)")
