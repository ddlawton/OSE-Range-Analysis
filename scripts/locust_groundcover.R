library(here)
library(tidyverse)
library(mgcv)

source(here::here("R", "_common.R"))

data <- load_ose_data(cache_processed = TRUE)

data <- data |>
    mutate(ose_damage_percent = ose_damage_percent/100)



data <- data |>
    select(year,region,farmer,farmer_gender,fertilizer_treatment,mission_number,ose_count,percent_ground_cover) |>
    mutate(fertilizer_treatment = factor(fertilizer_treatment),
           region = factor(region),
           farmer = factor(farmer),
           mission_number = factor(mission_number),
           region_treat = factor(paste0(region, '_', fertilizer_treatment))
           )

data |> 
  glimpse()


mod <- bam(
  ose_count ~ 
    s(percent_ground_cover) + 
    s(region, bs="re") +
    s(fertilizer_treatment, bs="re") +
    s(mission_number, bs="re") +
    s(farmer, bs="re"),
  data = data,
  family = tw()
)

summary(mod)
gratia::appraise(mod)

gratia::draw(mod)


library(gratia)

est <- smooth_estimates(mod, select = "percent_ground_cover",partial_match=TRUE)


ggplot(est, aes(x = percent_ground_cover, y = .estimate)) +
  geom_ribbon(aes(ymin = .estimate - .se, ymax = .estimate + .se), alpha = 0.2) +
  geom_line() +
  labs(
    x = "Percent Ground Cover",
    y = "Estimated OSE Count"
  ) +
  theme_minimal()
