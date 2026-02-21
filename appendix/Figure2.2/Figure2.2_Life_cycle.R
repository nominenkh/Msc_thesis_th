
file_path <- "../data/EXDEP_ISIC_Weighted_8_2.xlsx"

sheets <- excel_sheets(file_path)

raw_data <- sheets %>%
  set_names() %>%
  map_df(~ read_excel(file_path, sheet = .x, col_types = "text"), .id = "Period") %>%

  mutate(Period = str_replace(Period, "_", "-")) %>%
  mutate(across(c(ED_Young, ED_Mature), as.numeric))

# Sort by 2020-2024 Young firms
top_10_ordered <- raw_data %>%
  filter(Period == "2020-2024") %>%
  slice_max(order_by = ED_Young, n = 10) %>%
  arrange(ED_Young) %>% 
  pull(Industry_Name)

plot_data <- raw_data %>%
  filter(Period %in% c("1990-1999", "2020-2024")) %>%
  filter(Industry_Name %in% top_10_ordered) %>%
  select(Period, Industry_Name, ED_Young, ED_Mature) %>% 
  pivot_longer(cols = starts_with("ED_"), names_to = "Firm_Type", values_to = "ED_Value") %>%
  mutate(
    Firm_Type = str_remove(Firm_Type, "ED_"),
    Industry_Name = factor(Industry_Name, levels = top_10_ordered)
  )

theme_final <- theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", face = "plain"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(10, 5, 10, 5), 
    legend.position = "none"
  )

#  LEFT PLOT (1990 - 1999)
p_left <- ggplot(filter(plot_data, Period == "1990-1999"), 
                  aes(x = Industry_Name, y = ED_Value, fill = Firm_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", ED_Value)), 
            position = position_dodge(0.8), hjust = 1.1, size = 3, family = "Times New Roman") +
  # limits and expand ensure bars/text aren't chopped
  scale_y_reverse(limits = c(25, -5), expand = expansion(mult = c(0.1, 0.1))) + 
  scale_fill_manual(values = c("Young" = "#CD5C5C", "Mature" = "#4682B4")) +
  coord_flip(clip = "off") + 
  theme_final +
  labs(subtitle = "1990 - 1999") +
  theme(plot.subtitle = element_text(hjust = 0.5))

#  RIGHT PLOT (2020 - 2024)
p_right <- ggplot(filter(plot_data, Period == "2020-2024"), 
                   aes(x = Industry_Name, y = ED_Value, fill = Firm_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", ED_Value)), 
            position = position_dodge(0.8), hjust = -0.1, size = 3, family = "Times New Roman") +
  scale_y_continuous(limits = c(-5, 25), expand = expansion(mult = c(0.1, 0.1))) + 
  scale_fill_manual(values = c("Young" = "#CD5C5C", "Mature" = "#4682B4")) +
  coord_flip(clip = "off") + 
  theme_final +
  labs(subtitle = "2020 - 2024") +
  theme(plot.subtitle = element_text(hjust = 0.5))

# CENTER SPINE
p_center <- ggplot(filter(plot_data, Period == "2020-2024" & Firm_Type == "Young"), 
                    aes(x = Industry_Name, y = 0)) +
  geom_text(aes(label = str_wrap(Industry_Name, 25)), 
            family = "Times New Roman", size = 3.2, lineheight = 0.9) +
  coord_flip() + 
  theme_void()


final_plot <- (p_left | p_center | p_right) + 
  plot_layout(widths = c(1, 0.6, 1), guides = "collect") +
  plot_annotation(
    title = "Figure 2.2: Comparison of Financial Dependence by Firm Age",
    subtitle = "Top 10 Industries by 2020-2024 Young Firm Dependence",
    theme = theme(plot.title = element_text(family = "Times New Roman", face = "bold", hjust = 0.5))
  ) & 
  theme(legend.position = "bottom")


print(final_plot)
