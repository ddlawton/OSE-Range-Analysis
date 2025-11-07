# ===========================
# Creating the plots 
# showing grasshopper density
# by region and treatment
# ===========================

library(tidyverse)
library(janitor)
library(ggpubr)
library(patchwork)

# Read in data

dat <- readRDS('data/processed/raw_data_long.rds')

density_dat <- dat |>
    select(year,region,farmer,fertilizer_treatment,mission_number,ose_count)

# Ensure your mission_date variable is a factor with correct levels
density_dat <- density_dat %>%
    mutate(region = factor(region, levels = c('Saint Louis','Thies','Fatick','Kaffrine'))) %>%
    mutate(mission_date = 
        case_when(
            mission_number == 1 ~ 'Mission 1 (July)',
            mission_number == 2 ~ 'Mission 2 (September)',
            mission_number == 3 ~ 'Mission 3 (October)'
        ))

# Create a plotting function for a given mission_date
plot_mission <- function(md) {
    ggplot(
        filter(density_dat, mission_date == md), 
        aes(x=region, y=ose_count, color=fertilizer_treatment)
    ) +
    geom_jitter(position = position_jitterdodge(jitter.width=0.2, jitter.height=0)) +
    scale_color_manual(values = c('black','dark green')) +
    ylab('OSE/100m') +
    labs(title = md) +
    theme_pubr(legend = 'bottom') +
    theme(legend.title = element_blank())
}

# List your mission dates in order
mission_levels <- c('Mission 1 (July)', 'Mission 2 (September)', 'Mission 3 (October)')

# Generate individual plots
plots <- lapply(mission_levels, plot_mission)

# Combine using patchwork (`ncol=2` for two columns)
combined_plot <- wrap_plots(plots, ncol=2) + 
    plot_layout(guides = "collect") & theme(legend.position = 'bottom')

 
ggsave(combined_plot,file='output/figures/locust_density_by_region_treatment.png',width=7,height=6)
