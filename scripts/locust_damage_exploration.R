library(here)
library(tidyverse)

source(here::here("R", "_common.R"))

data <- load_ose_data(cache_processed = TRUE)

data <- data |>
    mutate(ose_damage_percent = ose_damage_percent/100)



data <- data |>
    select(year,region,farmer,farmer_gender,fertilizer_treatment,mission_number,ose_damage_percent)

data |> 
  glimpse()



# plot denisty line of damage
ggplot(data, aes(x=ose_damage_percent)) +
  geom_density(fill="blue", alpha=0.5) +
  labs(title="Density Plot of OSE Damage Percent",
       x="OSE Damage Percent",
       y="Density") +
  theme_minimal(base_size=30)


# Summary statistics of damage percent
damage_summary <- data %>%
  summarise(
    count = n(),
    mean_damage = mean(ose_damage_percent, na.rm = TRUE),
    median_damage = median(ose_damage_percent, na.rm = TRUE),
    sd_damage = sd(ose_damage_percent, na.rm = TRUE),
    min_damage = min(ose_damage_percent, na.rm = TRUE))

damage_summary


# plot damage by region and mission number
ggplot(data, aes(x=region, y=ose_damage_percent, color=fertilizer_treatment)) +
  geom_jitter(position=position_jitterdodge(jitter.width=0.2, jitter.height=0), pch=21, alpha=0.5) +
  labs(title="OSE Damage Percent by Region and Fertilizer Treatment",
       x="Region",
       y="OSE Damage Percent") +
  theme_minimal(base_size=20) +
  facet_wrap(~mission_number,ncol=2,scales='free')


# plot damage by farmer gender
ggplot(data, aes(x=farmer_gender, y=ose_damage_percent, color=fertilizer_treatment)) +
  geom_jitter(position=position_jitterdodge(jitter.width=0.2, jitter.height=0), pch=21, alpha=0.5) +
  labs(title="OSE Damage Percent by Farmer Gender and Fertilizer Treatment",
       x="Farmer Gender",
       y="OSE Damage Percent") +
  theme_minimal(base_size=20) +
  facet_wrap(~mission_number,ncol=2,scales='free')
