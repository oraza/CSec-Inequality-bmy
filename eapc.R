
## Data by group
df_q1 <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(1.8,	5.2	, 10)
)

fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_q1)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_q2 <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(2.3,	8.9	, 12.1)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_q2)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_q3 <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(6.6,	12.7,	21.5)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_q3)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_q4 <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(13,	22.7,	35.7)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_q4)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_q5 <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(23.4, 36.2,	46.1)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_q5)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_noEd <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(4.1,	7.7,	12.5)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_noEd)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_priEd <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(9.8,	17.6,	21.6)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_priEd)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_secEd <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(19.3,	26.9,	36.5)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_secEd)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_hiEd <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(30.5, 40, 	51.1)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_hiEd)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_rural <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(5.9,	11.8,	19.4)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_rural)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_urban <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(15.3,	25.8,	34.9)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_urban)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_balo <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(1.6,	1.3,	4.2)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_balo)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_kpk <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(4.1,	4.7,	8)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_kpk)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_sindh <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(8.1,	16.9,	25.8)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_sindh)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_punj <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(10.7,	19.5,	31.8)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_punj)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

df_overall <- data.frame(
  year = c(2006, 2012, 2017),
  prevalence = c(8.7,	15.9,	24.4)
)
fit_model <- glm(log(prevalence) ~ year, family = gaussian, data = df_overall)

coef <- coefficients(fit_model)[2]
# Compute confidence intervals for the coefficient
conf_int <- confint(fit_model)[2,]
# Compute EAPC and 95% CI
eapc_mean <- (exp(coef) - 1) * 100
eapc_lower <- (exp(conf_int[1]) - 1) * 100
eapc_upper <- (exp(conf_int[2]) - 1) * 100
EAPC <- paste0("EAPC: ", round(eapc_mean, 2), " (", round(eapc_lower, 2), " to ", round(eapc_upper, 2), ")")
print(EAPC)

library(ggplot2)
library(readr)
library(dplyr)

# 1. Read your CSV file
df_eapc <- read_csv("sheet3.csv")

# 2. Create forest plot
# Optional: reorder group within each category
df_eapc <- df_eapc %>%
  mutate(group = fct_reorder(group, EAPC))

df_eapc <- df_eapc %>%
  mutate(Category = factor(Category, levels = c("Educational Status",  "Socioeconomical Status", "Place of residence", "Region", "Overall")))


# Plot

library(ggh4x)

# Plot
eapcPlot <- ggplot(df_eapc, aes(x = EAPC, y = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  ggh4x::facet_wrap2(
    ~ Category,
    scales = "free_y",
    ncol = 1,
    strip = ggh4x::strip_themed(
      background_x = ggh4x::elem_list_rect(
        fill = c(rep("white", 4), "#22bbbb")  # 4 white, last (Overall) is blue
      )
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

# saving high resolution 
ggsave("eapcPlot.png", eapcPlot, dpi = 1600 )




