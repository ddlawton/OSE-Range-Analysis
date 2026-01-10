library(here)
library(tidyverse)
library(glmmTMB)
library(emmeans)

source(here::here("R", "_common.R"))

data <- load_ose_data(cache_processed = TRUE)

data <- data |>
    mutate(ose_damage_percent = ose_damage_percent/100)



data <- data |>
    select(year,region,farmer,farmer_gender,fertilizer_treatment,mission_number,ose_count,percent_ground_cover)

data |> 
  glimpse()




mod <- glmmTMB(
  ose_count ~ fertilizer_treatment * region + (1|farmer) + percent_ground_cover,
  data = data,
  family=tweedie()
)

mod2 <- glmmTMB(
  ose_count ~ fertilizer_treatment * region + (1|farmer),
  data = data,
  family=tweedie()
)


mod3 <- glmmTMB(
  ose_count ~ fertilizer_treatment * percent_ground_cover + (1|farmer),
  data = data,
  family=tweedie()
)

summary(mod3)

mod_emms <- emmeans(mod, ~ fertilizer_treatment * region, type = "response")
mod2_emms <- emmeans(mod2, ~ fertilizer_treatment * region, type = "response")
mod3_emms <- emmeans(mod3, ~ fertilizer_treatment * percent_ground_cover, type = "response")


mod_emms
mod2_emms


library(ggplot2)
library(ggeffects)

pred <- ggpredict(mod3, terms = c("percent_ground_cover [all]", "fertilizer_treatment"))

ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color=NA) +
  labs(x = "Percent Ground Cover", y = "Predicted OSE Count", color = "Fertilizer", fill = "Fertilizer") +
  theme_minimal()

emmeans(mod, ~ fertilizer_treatment | region, type = "response")


emmeans(mod, ~ fertilizer_treatment * region, at = list(percent_ground_cover = c(0.5)), type = "response")


library(mgcv)
data_filtered <- data |>
  filter(mission_number != 1) |>
  mutate(farmer = factor(farmer),
         fertilizer_treatment = factor(fertilizer_treatment),
         mission_number = factor(mission_number)
         )

mod_gam <- bam(
  ose_count ~ 
    te(percent_ground_cover, by = fertilizer_treatment, k = 30) +
    fertilizer_treatment * region +
    s(farmer, bs = "re"),
  data = data_filtered,
  family = tw(),
  select = TRUE
)
summary(mod_gam)
gratia::draw(mod_gam)

#check concurvity
concurvity(mod_gam, full = FALSE)
