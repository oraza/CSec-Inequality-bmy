library(tidyverse)   # For data manipulation and plotting
library(ggsci)       # For scientific journal-themed colors (optional)
library(ggrepel)     # For avoiding overlapping labels (optional)
library(patchwork)



cs.dat <- read.csv("//Users//sulailfatima//Desktop//GoogleDrive//BMYHealth //BMYHealth - Shafi group//heatdatafig3.csv")

View(cs.dat)
cs.dat <- cs.dat %>%
  group_by(setting) %>%
  mutate(min_estimate = min(estimate, na.rm = TRUE)) %>%
  ungroup()

grph3 <- ggplot(cs.dat, 
                aes(x = estimate,
                    y = fct_reorder(setting, estimate))) +
  geom_segment(aes(x = min_estimate, 
                   xend = estimate, 
                   y = setting, 
                   yend = setting),
               color = "grey20") +
  geom_point(color = "#4682B4", 
             size = 4, 
             alpha = 0.5) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)  # ticks every 10 units
  ) +
  scale_y_discrete(labels = c(
    "Pakistan" = "Pakistan (ICEH - DHS 2017)",
    "India" = "India (ICEH - DHS 2015)",
    "Nepal" = "Nepal (ICEH - DHS 2016)",
    "Bangladesh" = "Bangladesh (ICEH - DHS 2017)",
    "Bhutan" = "Bhutan (ICEH - MICS 2010)"
  )) +
  labs(
    #title = "Birth by CS (%) in the two or three years preceeding the survey",
    x = "Prevalence",
    y = NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 15),
    panel.grid.minor.x = element_blank()
  )

ggsave("graph3.jpg", grph3, dpi = 1200 )
ggsave("graph3.png", grph3, dpi = 1200 )


csfig1.dat <- read.csv("//Users//sulailfatima//Desktop//GoogleDrive//BMYHealth //BMYHealth - Shafi group//heatdata.csv")
View(csfig1.dat)
colnames(csfig1.dat)[1] <- "Group"

csfig1.dat <- csfig1.dat %>%
  rename(
    estimate_2006 = 2,
    ci_2006 = 3,
    estimate_2012 = 4,
    ci_2012 = 5,
    estimate_2017 = 6,
    ci_2017 = 7
  )

df_long <- csfig1.dat %>%
  pivot_longer(
    cols = starts_with("estimate"),
    names_to = "year",
    names_prefix = "estimate_",
    values_to = "estimate"
  ) %>%
  left_join(
    csfig1.dat %>%
      pivot_longer(
        cols = starts_with("ci_"),
        names_to = "year",
        names_prefix = "ci_",
        values_to = "ci"
      ),
    by = c("Group", "year")
  )

df_long <- df_long %>%
  mutate(
    lower = as.numeric(str_extract(ci, "^[0-9\\.]+")),
    upper = as.numeric(str_extract(ci, "[0-9\\.]+$")),
    year = as.numeric(year),
    Dimension = case_when(
      Group %in% c("Poorest", "Poorer", "Middle", "Richer", "Richest") ~ "Wealth",
      Group %in% c("No Education", "Primary Education", "Secondary Education", "Higher Education") ~ "Education",
      Group %in% c("Rural", "Urban") ~ "Place of Residence",
      Group %in% c("Punjab", "Sindh", "KPK", "Balochistan") ~ "Region",
      Group %in% c("Pakistan (Overall)") ~ "Pakistan (Overall)",
      TRUE ~ NA_character_  # section headers or unknowns
    )
  ) %>%
  filter(!is.na(Dimension))  # remove non-informative rows


library(ggplot2)
library(dplyr)
library(patchwork)

# Define a minimal theme with individual legends
custom_theme <- theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 15),
        panel.grid.minor.x = element_blank())


# Wealth
plot_wealth <- df_long %>%
  filter(Dimension == "Wealth") %>%
  ggplot(aes(x = year, y = estimate, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_x_continuous(breaks = c(2006, 2012, 2017)) +
  labs(title = "Wealth", 
       x = "Survey Year", 
       y = "Prevalence of CS (%)",
       tag = "A") +
  custom_theme

# Education
plot_edu <- df_long %>%
  filter(Dimension == "Education") %>%
  ggplot(aes(x = year, y = estimate, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_x_continuous(breaks = c(2006, 2012, 2017)) +
  labs(title = "Education", 
       x = "Survey Year", 
       y = "Prevalence of CS (%)",
       tag = "B") +
  custom_theme

# Place of Residence
plot_res <- df_long %>%
  filter(Dimension == "Place of Residence") %>%
  ggplot(aes(x = year, y = estimate, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_x_continuous(breaks = c(2006, 2012, 2017)) +
  labs(title = "Place of Residence", 
       x = "Survey Year", 
       y = "Prevalence of CS (%)",
       tag = "C") +
  custom_theme

# Region
plot_reg <- df_long %>%
  filter(Dimension == "Region") %>%
  ggplot(aes(x = year, y = estimate, color = Group)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_x_continuous(breaks = c(2006, 2012, 2017)) +
  labs(title = "Region", 
       x = "Survey Year", 
       y = "Prevalence of CS (%)",
       tag = "D") +
  custom_theme

# Overall
plot_overall <- df_long %>%
  filter(Dimension == "Pakistan (Overall)") %>%
  ggplot(aes(x = year, y = estimate, color = Group)) +
  geom_line() +
  geom_point() +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_x_continuous(breaks = c(2006, 2012, 2017)) +
  labs(title = "Pakistan (Overall)", 
       x = "Survey Year", 
       y = "Prevalence of CS (%)",
       tag = "E") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 15),
        panel.grid.minor.x = element_blank())



# Wrap plots so legends don't conflict
library(patchwork)
library(patchwork) # safe re-import

final_plot <- (wrap_elements(plot_edu) | wrap_elements(plot_res)) /
  (wrap_elements(plot_reg) | wrap_elements(plot_wealth)) / 
  (wrap_elements() | wrap_elements(plot_overall) | wrap_elements() )

ggsave("graph1.png", final_plot, dpi = 1600 )











