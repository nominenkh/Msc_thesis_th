
#  Figure 2.1: Evolution of External Dependence


path <- "../data/EXDEP_ISIC_Weighted_8_2.xlsx"

s1 <- read_excel(path, sheet = "1990_1999") %>% 
  mutate(Period = "1990-1999", ISIC_Code = as.character(ISIC_Code))
s2 <- read_excel(path, sheet = "2000_2009") %>% 
  mutate(Period = "2000-2009", ISIC_Code = as.character(ISIC_Code))
s3 <- read_excel(path, sheet = "2010_2019") %>% 
  mutate(Period = "2010-2019", ISIC_Code = as.character(ISIC_Code))
s4 <- read_excel(path, sheet = "2020_2024") %>% 
  mutate(Period = "2020-2024", ISIC_Code = as.character(ISIC_Code))


all_data_long <- bind_rows(s1, s2, s3, s4) %>%
  rename(ISIC_REV3 = ISIC_Code)

all_data_long <- all_data_long %>%
  mutate(Industry_Name = gsub("Plactic", "Plastic", Industry_Name))

# High Dependence at the TOP of the plot. 

master_order <- all_data_long %>%
  filter(Period == "2020-2024") %>%
  arrange(ED_All) %>%       
  pull(Industry_Name)

all_data_long$Industry_Name <- factor(all_data_long$Industry_Name, levels = master_order)

# Define Cohesive Thesis Theme (Times New Roman)
theme_thesis_heatmap <- theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.ticks = element_blank(),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 10, face = "bold", color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
    plot.caption = element_text(hjust = 0, size = 9, face = "italic", margin = margin(t = 10)),
    legend.title = element_text(size = 10, face = "bold"),
    legend.position = "right"
  )

# Generate the Heatmap
heatmap_plot <- ggplot(all_data_long, aes(x = Period, y = Industry_Name, fill = ED_All)) +
  # Create the tiles
  geom_tile(color = "white", lwd = 0.5) +  
  
  # Add the ED numeric labels inside the tiles
  geom_text(aes(label = sprintf("%.2f", ED_All)), 
            family = "Times New Roman", size = 2.8, color = "black") +
  
  # Diverging palette (Blue = Net Savers, Red = Net Borrowers)
  scale_fill_gradient2(low = "#0571b0", mid = "white", high = "#ca0020", 
                       midpoint = 0, name = "ED Value", na.value = "grey90") +
  
  theme_thesis_heatmap +
  
  # Labels and Captions
  labs(title = "Figure 2.1: Evolution of External Dependence (1990-2024)",
       subtitle = "Industries sorted by 2020-2024 dependence (High at Top)",
       caption = "Note: Red shades indicate high dependence (net borrowers), blue shades indicate low dependence (net savers).\nSource: Author’s calculations based on data from LSEG DataStream.",
       x = NULL, 
       y = NULL)


# Save as high-resolution PNG for document inclusion
ggsave(final_output_path, 
       plot = heatmap_plot, 
       width = 10, 
       height = 12, 
       dpi = 300, 
       bg = "white") 

# Print to console/plots pane
print(heatmap_plot)
