
## Defining Data
# Socioeconomic Status
df_q1 <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(1.8, 5.2, 10))
df_q2 <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(2.3, 8.9, 12.1))
df_q3 <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(6.6, 12.7, 21.5))
df_q4 <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(13, 22.7, 35.7))
df_q5 <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(23.4, 36.2, 46.1))

# Educational Status
df_noEd  <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(4.1, 7.7, 12.5))
df_priEd <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(9.8, 17.6, 21.6))
df_secEd <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(19.3, 26.9, 36.5))
df_hiEd  <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(30.5, 40, 51.1))

# Place of residence
df_rural <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(5.9, 11.8, 19.4))
df_urban <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(15.3, 25.8, 34.9))

# Region
df_balo  <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(1.6, 1.3, 4.2))
df_kpk   <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(4.1, 4.7, 8))
df_sindh <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(8.1, 16.9, 25.8))
df_punj  <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(10.7, 19.5, 31.8))

# Overall
df_overall <- tibble::tibble(year = c(2006, 2012, 2017), prevalence = c(8.7, 15.9, 24.4))

## Define a function to calculate EAPC and its CI
calculate_eapc <- function(data, group_name, category) {
  fit <- glm(log(prevalence) ~ year, family = gaussian, data = data)
  
  coef <- coefficients(fit)[2]
  conf_int <- confint(fit)[2, ]
  
  eapc_mean <- (exp(coef) - 1) * 100
  eapc_lower <- (exp(conf_int[1]) - 1) * 100
  eapc_upper <- (exp(conf_int[2]) - 1) * 100
  
  tibble::tibble(
    group = group_name,
    Category = category,
    EAPC = round(eapc_mean, 2),
    LL = round(eapc_lower, 2),
    UL = round(eapc_upper, 2)
  )
}

## Feeding datasets through the function
library(tibble)
library(dplyr)

results <- bind_rows(
  calculate_eapc(df_q1, "Poorest", "Socioeconomic Status"),
  calculate_eapc(df_q2, "Poorer", "Socioeconomic Status"),
  calculate_eapc(df_q3, "Middle", "Socioeconomic Status"),
  calculate_eapc(df_q4, "Richer", "Socioeconomic Status"),
  calculate_eapc(df_q5, "Richest", "Socioeconomic Status"),
  
  calculate_eapc(df_noEd, "No Education", "Educational Status"),
  calculate_eapc(df_priEd, "Primary Education", "Educational Status"),
  calculate_eapc(df_secEd, "Secondary Education", "Educational Status"),
  calculate_eapc(df_hiEd, "Higher Education", "Educational Status"),
  
  calculate_eapc(df_rural, "Rural", "Place of residence"),
  calculate_eapc(df_urban, "Urban", "Place of residence"),
  
  calculate_eapc(df_balo, "Balochistan", "Region"),
  calculate_eapc(df_kpk, "KPK", "Region"),
  calculate_eapc(df_sindh, "Sindh", "Region"),
  calculate_eapc(df_punj, "Punjab", "Region"),
  
  calculate_eapc(df_overall, "National", "Overall")
)

## Creating the forest plot
library(ggplot2)
library(ggh4x)
library(forcats)

results <- results %>%
  mutate(
    group = fct_reorder(group, EAPC),
    Category = factor(
      Category,
      levels = c("Educational Status", "Socioeconomic Status", 
                 "Place of residence", "Region", "Overall")
    )
  )

eapc_plot <- ggplot(results, aes(x = EAPC, y = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  ggh4x::facet_wrap2(
    ~ Category,
    scales = "free_y",
    ncol = 1,
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(
        fill = c(rep("white", 4), "#cc555c")
      )
    )
  ) +
  labs(y = NULL, x = "Estimated Average Percentage Change (EAPC)") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

ggsave("eapcPlot.png", eapc_plot, dpi = 1600)

