# _targets.R - Targets Pipeline for OSE-Range-Analysis
#
# ARCHITECTURE:
# 1. Data targets: Load and preprocess raw data into clean analysis-ready format
# 2. Model targets: Fit all statistical models (GLMMs, GAMs) and save as RDS
# 3. Summary targets: Generate emmeans, pairwise comparisons, diagnostics as CSV/PNG
# 4. Quarto documents: Import target outputs, create visualizations, format tables
#
# This pipeline handles ALL computational work. Quarto docs only import and display.
# Run with: targets::tar_make()

library(targets)
library(here)

# === TARGETS CONFIGURATION ===
tar_option_set(
  packages = c(
    "here",
    "readr",
    "dplyr",
    "glmmTMB",
    "mgcv",
    "emmeans",
    "ggplot2",
    "tibble",
    "purrr",
    "broom",
    "rlang"
  ),
  format = "rds"
)

# --- Project constants (used by targets and analysis documents)
# Define region and plotting constants here so they're available during `targets::tar_make()`.
STUDY_REGIONS <- c('Saint-Louis', 'Thiès', 'Fatick', 'Kaffrine')
ALT_STUDY_REGIONS <- c('Saint Louis', 'Thies', 'Fatick', 'Kaffrine')
MISSION_LABELS <- c('Mission 1 (July)', 'Mission 2 (September)', 'Mission 3 (October)')
DEFAULT_PALETTE <- "Degas"
FERTILIZER_COLORS <- c('control' = 'black', 'fertilized' = 'dark green')
DEFAULT_POINT_SIZE <- 5
DEFAULT_EMMEAN_POINT_SIZE <- 8
DEFAULT_FIGURE_DPI <- 2


# === FUNCTION LOADING ===
# Source all function files for use in targets
function_files <- list.files(here::here("R", "functions"), pattern = "\\.R$", full.names = TRUE)
for (f in function_files) source(f, local = TRUE)

# Utility function to standardize region factor levels
standardize_regions <- function(data, region_col = "region", alt_names = FALSE) {
  regions <- if (alt_names) ALT_STUDY_REGIONS else STUDY_REGIONS

  normalize_region <- function(x) {
    x_ascii <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
    x_ascii <- gsub("-", " ", x_ascii)
    x_ascii <- gsub("\\s+", " ", x_ascii)
    trimws(x_ascii)
  }

  canonical_map <- c(
    "Saint Louis" = "Saint-Louis",
    "Thies" = "Thiès",
    "Fatick" = "Fatick",
    "Kaffrine" = "Kaffrine"
  )

  region_norm <- normalize_region(data[[region_col]])
  region_canonical <- unname(canonical_map[region_norm])

  unresolved <- unique(region_norm[is.na(region_canonical) & !is.na(region_norm)])
  if (length(unresolved) > 0) {
    warning(
      "Unrecognized region values after normalization: ",
      paste(unresolved, collapse = ", ")
    )
  }

  if (alt_names) {
    region_out <- normalize_region(region_canonical)
  } else {
    region_out <- region_canonical
  }

  data[[region_col]] <- factor(region_out, levels = regions)
  return(data)
}

# Note: directory and save helpers moved to `R/functions/figure_export.R`.
# `_targets.R` uses `ensure_dir()` and `save_plot_file()` from that module.

save_plot_safe <- function(plot_obj, out, placeholder = "plot unavailable", width = 8, height = 6, dpi = 200) {
  ensure_dir(dirname(out))
  if (exists("save_plot_file", mode = "function")) {
    tryCatch(
      save_plot_file(plot_obj, out, width = width, height = height, dpi = dpi, placeholder = placeholder),
      error = function(e) {
        if (!is.null(plot_obj)) {
          ggplot2::ggsave(out, plot = plot_obj, width = width, height = height, dpi = dpi)
        } else {
          writeLines(placeholder, out)
        }
      }
    )
  } else {
    if (!is.null(plot_obj)) {
      ggplot2::ggsave(out, plot = plot_obj, width = width, height = height, dpi = dpi)
    } else {
      writeLines(placeholder, out)
    }
  }
  out
}

# Targets ---------------------------------------------------------------
list(
  # 1) Raw/processed data (declare processed CSV file as a file target)
  tar_target(
    processed_ose_data_file,
    here::here("data", "processed", "ose_data_processed.csv"),
    format = "file"
  ),

  # Convenience target: read processed data once for downstream targets
  tar_target(
    processed_ose_data,
    {
      readr::read_csv(processed_ose_data_file)
    }
  ),

  # Preprocess and standardize the processed dataset for all models
  tar_target(
    processed_ose_data_clean,
    {
      df <- processed_ose_data

      # Create a standardized yield column if present under various names
      if ("rendement_en_kg_ha" %in% names(df)) {
        df <- df |> dplyr::mutate(yield_kg_ha = dplyr::coalesce(rendement_en_kg_ha, dplyr::na_if(NA_real_, NA_real_)))
        df <- df |> dplyr::mutate(yield = yield_kg_ha)
      } else if ("yield_kg_ha" %in% names(df)) {
        df <- df |> dplyr::mutate(yield = yield_kg_ha)
      } else if ("yield" %in% names(df)) {
        df <- df |> dplyr::mutate(yield = yield)
      }

      # Standardize region factor levels and common columns
      if (exists("standardize_regions", mode = "function")) {
        df <- standardize_regions(df)
      } else {
        if ("region" %in% names(df)) df$region <- factor(df$region)
      }

      # Convert common columns to appropriate types
      df <- df |> dplyr::mutate(
        farmer = as.factor(.data$farmer),
        fertilizer_treatment = as.factor(.data$fertilizer_treatment),
        mission_number = as.integer(.data$mission_number)
      )

      df
    }
  ),

  # 2) Locust density models per mission
  tar_target(
    locust_density_model_m1_rds,
    {
      df_m1 <- dplyr::filter(processed_ose_data_clean, mission_number == 1)
      model_obj <- create_count_glmm_model(df_m1)
      out <- here::here("data", "model_objects", "locust_density_mission_1.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_density_model_m2_rds,
    {
      df_m2 <- dplyr::filter(processed_ose_data_clean, mission_number == 2)
      model_obj <- create_count_glmm_model(df_m2)
      out <- here::here("data", "model_objects", "locust_density_mission_2.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_density_model_m3_rds,
    {
      df_m3 <- dplyr::filter(processed_ose_data_clean, mission_number == 3)
      model_obj <- create_count_glmm_model(df_m3)
      out <- here::here("data", "model_objects", "locust_density_mission_3.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  # 3) Locust damage models per mission (use existing wrapper if present)
  tar_target(
    locust_damage_model_m1_rds,
    {
      df_m1 <- dplyr::filter(processed_ose_data_clean, mission_number == 1)
      model_obj <- create_damage_glmm_model(df_m1)
      out <- here::here("data", "model_objects", "locust_damage_mission_1.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_model_m2_rds,
    {
      df_m2 <- dplyr::filter(processed_ose_data_clean, mission_number == 2)
      model_obj <- create_damage_glmm_model(df_m2)
      out <- here::here("data", "model_objects", "locust_damage_mission_2.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_model_m3_rds,
    {
      df_m3 <- dplyr::filter(processed_ose_data_clean, mission_number == 3)
      model_obj <- create_damage_glmm_model(df_m3)
      out <- here::here("data", "model_objects", "locust_damage_mission_3.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  # 4) Temperature GAM (example)
  tar_target(
    locust_temperature_gam_rds,
    {
      # Mirror the preprocessing in R/analysis/locust_density_temperature.qmd
      temp_df <- processed_ose_data_clean %>%
        dplyr::filter(fertilizer_treatment == 'control') %>%
        dplyr::select(year, region, farmer, mission_number, ose_count, temperature) %>%
        dplyr::mutate(
          region = factor(region),
          farmer = factor(farmer),
          mission_number = factor(mission_number)
        ) %>%
        dplyr::filter(!is.na(temperature))

      # Use the same formula used in analysis documents (factor-smooth by region)
      gam_formula <- ose_count ~ s(temperature, region, bs = 'fs', k = 10) + s(farmer, bs = 're')
      # Fit GAM using mgcv::gam directly (mirrors analysis documents)
      gam_obj <- tryCatch(
        mgcv::gam(
          formula = gam_formula,
          data = temp_df,
          family = mgcv::tw(),
          select = TRUE
        ),
        error = function(e) {
          # Create diagnostics directory and save helpful debugging info
          diag_dir <- here::here("outputs", "logs")
          ensure_dir(diag_dir)
          diag_file <- file.path(diag_dir, "locust_temperature_gam_error.txt")
          cat("Error fitting locust_temperature_gam:\n", file = diag_file)
          cat(conditionMessage(e), "\n\n", file = diag_file, append = TRUE)
          cat("\n-- Column names (processed data) --\n", file = diag_file, append = TRUE)
          cat(paste(colnames(temp_df), collapse = ", "), "\n\n", file = diag_file, append = TRUE)
          cat("\n-- Data sample (first 6 rows) --\n", file = diag_file, append = TRUE)
          tryCatch({
            utils::capture.output(print(utils::head(temp_df, 6)), file = diag_file, append = TRUE)
          }, error = function(e2) {
            cat("Could not print sample data: ", conditionMessage(e2), "\n", file = diag_file, append = TRUE)
          })
          stop("Failed to fit locust_temperature_gam. Diagnostics written to: ", diag_file, "\nOriginal error: ", conditionMessage(e))
        }
      )
      out <- here::here("data", "model_objects", "locust_temperature_gam.rds")
      ensure_dir(dirname(out))
      saveRDS(gam_obj, out)
      out
    },
    format = "file"
  ),

  # 5) Ground-cover mediation model - Unified GAM with by-variable smooth
  tar_target(
    ground_cover_mediation_rds,
    {
      df_gc <- processed_ose_data_clean |>
        dplyr::filter(!is.na(percent_ground_cover)) |>
        dplyr::mutate(
          farmer = factor(farmer),
          mission_number = factor(mission_number),
          fertilizer_treatment = factor(fertilizer_treatment),
          region = factor(region)
        )
      
      # Unified GAM with by-variable smooth for ground cover mediation analysis
      model_obj <- mgcv::gam(
        ose_count ~ 
          fertilizer_treatment * mission_number + 
          region + 
          s(percent_ground_cover, by = fertilizer_treatment) + 
          s(farmer, bs = 're'),
        data = df_gc,
        family = mgcv::tw(),
        select = TRUE
      )
      
      out <- here::here("data", "model_objects", "ground_cover_mediation.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  # 6) Yield - locust effect model (example)
  tar_target(
    yield_locust_model_rds,
    {
      # Use the standardized `yield` column created in `processed_ose_data_clean`
      if (!"yield" %in% colnames(processed_ose_data_clean)) {
        stop("No `yield` column found in cleaned processed data. Ensure `processed_ose_data_clean` creates a `yield` column.")
      }
      df_yield <- dplyr::filter(processed_ose_data_clean, !is.na(yield))

      # Use a wrapper if available; otherwise attempt a generic model
      if (exists("create_yield_locust_model", mode = "function")) {
        model_obj <- create_yield_locust_model(df_yield)
      } else if (exists("create_count_glmm_model", mode = "function")) {
        model_obj <- create_count_glmm_model(df_yield)
      } else {
        stop("No yield model wrapper found; please implement create_yield_locust_model in R/functions/")
      }
      out <- here::here("data", "model_objects", "yield_locust_model.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  # 7) Derived tables: emmeans and tidy summaries for several models
  tar_target(
    locust_m1_emmeans_csv,
    {
      model_path <- locust_density_model_m1_rds
      model_obj <- readRDS(model_path)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment)
      em_df <- as.data.frame(em)
      out <- here::here("outputs", "tables", "locust_density", "mission_1_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_m2_emmeans_csv,
    {
      model_path <- locust_density_model_m2_rds
      model_obj <- readRDS(model_path)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment)
      em_df <- as.data.frame(em)
      out <- here::here("outputs", "tables", "locust_density", "mission_2_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_m3_emmeans_csv,
    {
      model_path <- locust_density_model_m3_rds
      model_obj <- readRDS(model_path)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment)
      em_df <- as.data.frame(em)
      out <- here::here("outputs", "tables", "locust_density", "mission_3_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    damage_m1_emmeans_csv,
    {
      model_path <- locust_damage_model_m1_rds
      model_obj <- readRDS(model_path)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment)
      em_df <- as.data.frame(em)
      out <- here::here("outputs", "tables", "locust_damage", "mission_1_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_df, out)
      out
    },
    format = "file"
  ),

  # 8) Ground-cover mediation summary
  tar_target(
    ground_cover_summary_csv,
    {
      model_path <- ground_cover_mediation_rds
      model_obj <- readRDS(model_path)
      # Try to use save_model_summary if available
      outdir <- here::here("outputs", "tables", "ground_cover")
      ensure_dir(outdir)
      out <- file.path(outdir, "ground_cover_model_summary.csv")

      # Ensure analysis context if helper expects it
      if (exists("set_current_analysis", mode = "function")) {
        tryCatch(set_current_analysis("ground_cover"), error = function(e) NULL)
      }

      if (exists("save_model_summary", mode = "function")) {
        # Attempt to use project helper to save a nicely formatted summary.
        # The helper expects a table name (not a full path), so pass just the name.
        table_name <- "ground_cover_model_summary"
        tryCatch({
          save_model_summary(model_obj, table_name)
        }, error = function(e) {
          warning("save_model_summary() raised an error: ", conditionMessage(e))
        })

        # If helper didn't create the file, fall back to broom::tidy()
        if (!file.exists(out)) {
          tidy_df <- tryCatch(broom::tidy(model_obj), error = function(e) NULL)
          if (!is.null(tidy_df)) {
            readr::write_csv(tidy_df, out)
          } else {
            stop("save_model_summary did not create expected file and broom::tidy failed to produce a summary")
          }
        }
      } else {
        tidy_df <- broom::tidy(model_obj)
        readr::write_csv(tidy_df, out)
      }

      out
    },
    format = "file"
  ),

  # 9) Yield model summary
  tar_target(
    yield_locust_summary_csv,
    {
      model_path <- yield_locust_model_rds
      model_obj <- readRDS(model_path)
      out <- here::here("outputs", "tables", "yield", "yield_locust_model_summary.csv")
      ensure_dir(dirname(out))
      if (exists("save_model_summary", mode = "function")) {
        # Ensure analysis context for helper
        if (exists("set_current_analysis", mode = "function")) {
          tryCatch(set_current_analysis("yield"), error = function(e) NULL)
        }

        table_name <- "yield_locust_model_summary"
        tryCatch({
          save_model_summary(model_obj, table_name)
        }, error = function(e) {
          warning("save_model_summary() raised an error: ", conditionMessage(e))
        })

        if (!file.exists(out)) {
          readr::write_csv(broom::tidy(model_obj), out)
        }
      } else {
        readr::write_csv(broom::tidy(model_obj), out)
      }
      out
    },
    format = "file"
  ),

  # --- Additional targets to cover remaining Quarto pages ---
  # Yield by treatment x region (yield_treatment_region.qmd)
  tar_target(
    yield_treatment_region_model_rds,
    {
      df <- processed_ose_data_clean |>
        dplyr::select(year, region, farmer, fertilizer_treatment, rendement_en_kg_ha, yield) |>
        dplyr::rename(yield_raw = rendement_en_kg_ha) |>
        dplyr::filter(!is.na(yield_raw) | !is.na(yield))

      # prefer standardized `yield` where available
      if ("yield" %in% names(df) && sum(!is.na(df$yield)) > 0) {
        df$y <- df$yield
      } else {
        df$y <- df$yield_raw
      }

      # Fit GLMM similar to the qmd
      model_obj <- glmmTMB::glmmTMB(
        y ~ fertilizer_treatment * region + (1 | farmer),
        data = df,
        family = tweedie()
      )

      out <- here::here("data", "model_objects", "yield_treatment_region.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  tar_target(
    yield_treatment_region_emmeans_csv,
    {
      model_path <- yield_treatment_region_model_rds
      model_obj <- readRDS(model_path)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      em_df <- as.data.frame(em)
      out <- here::here("outputs", "tables", "yield_treatment_region", "emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    yield_treatment_region_pairwise_csv,
    {
      model_path <- yield_treatment_region_model_rds
      model_obj <- readRDS(model_path)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      pairs_df <- as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey"))
      out <- here::here("outputs", "tables", "yield_treatment_region", "pairwise.csv")
      ensure_dir(dirname(out))
      readr::write_csv(pairs_df, out)
      out
    },
    format = "file"
  ),

  # Yield environment page (yield_environment.qmd) - small summary and plot
  tar_target(
    yield_environment_summary_csv,
    {
      df <- processed_ose_data_clean |>
        dplyr::select(year, region, farmer, temperature, fertilizer_treatment, rendement_en_kg_ha, yield)

      summary_df <- df |>
        dplyr::group_by(region, farmer) |>
        dplyr::summarise(
          mean_temp = mean(temperature, na.rm = TRUE),
          sd_temp = stats::sd(temperature, na.rm = TRUE),
          rendement_en_kg_ha = dplyr::first(na.omit(rendement_en_kg_ha)),
          .groups = "drop"
        )

      out <- here::here("outputs", "tables", "yield_environment", "mean_temp_vs_yield.csv")
      ensure_dir(dirname(out))
      readr::write_csv(summary_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    yield_environment_plot_png,
    {
      df <- readr::read_csv(yield_environment_summary_csv)

      # Filter out non-finite / missing values to avoid plotting warnings
      df <- df |> dplyr::filter(is.finite(.data$mean_temp) & is.finite(.data$rendement_en_kg_ha))

      p <- ggplot2::ggplot(df, ggplot2::aes(x = mean_temp, y = rendement_en_kg_ha)) +
        ggplot2::geom_point(na.rm = TRUE) +
        ggplot2::geom_smooth(method = "gam", se = FALSE, na.rm = TRUE) +
        ggplot2::theme_minimal()
      out <- here::here("outputs", "figures", "yield_environment", "yield_temp_scatter.png")
      ensure_dir(dirname(out))
      save_plot_safe(p, out, "yield environment plot unavailable", width = 7, height = 5)
      out
    },
    format = "file"
  ),

  # Basic stats dataset summary (basic_stats.qmd)
  tar_target(
    dataset_summary_csv,
    {
      data <- processed_ose_data_clean
      sum_tbl <- dplyr::tibble(
        n_farmers = dplyr::n_distinct(data$farmer),
        n_regions = dplyr::n_distinct(data$region),
        n_missions = dplyr::n_distinct(data$mission_number),
        total_observations = nrow(data),
        mean_ose_count = mean(data$ose_count, na.rm = TRUE),
        mean_temperature = mean(data$temperature, na.rm = TRUE),
        mean_ground_cover = mean(data$percent_ground_cover, na.rm = TRUE),
        mean_yield = mean(data$yield, na.rm = TRUE)
      )
      out <- here::here("outputs", "tables", "basic_stats", "dataset_summary.csv")
      ensure_dir(dirname(out))
      readr::write_csv(sum_tbl, out)
      out
    },
    format = "file"
  ),

  # Farmer gender analysis models (farmer_gender_analysis.qmd)
  tar_target(
    farmer_gender_density_model_rds,
    {
      df <- processed_ose_data_clean |>
        dplyr::mutate(ose_damage_percent = .data$ose_damage_percent) |> # ensure column exists
        dplyr::filter(!is.na(farmer_gender))
      model_obj <- glmmTMB::glmmTMB(ose_count ~ farmer_gender + (1 | region) + (1 | mission_number), data = df, family = tweedie())
      out <- here::here("data", "model_objects", "farmer_gender_density.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  tar_target(
    farmer_gender_damage_model_rds,
    {
      df <- processed_ose_data_clean |>
        dplyr::mutate(ose_damage_percent = .data$ose_damage_percent) |> dplyr::filter(!is.na(farmer_gender))
      model_obj <- glmmTMB::glmmTMB(ose_damage_percent ~ farmer_gender + (1 | region) + (1 | mission_number), data = df, family = tweedie())
      out <- here::here("data", "model_objects", "farmer_gender_damage.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  tar_target(
    farmer_gender_yield_model_rds,
    {
      df <- processed_ose_data_clean |>
        dplyr::filter(!is.na(yield))
      model_obj <- glmmTMB::glmmTMB(yield ~ farmer_gender + (1 | region) + (1 | mission_number), data = df, family = tweedie())
      out <- here::here("data", "model_objects", "farmer_gender_yield.rds")
      ensure_dir(dirname(out))
      saveRDS(model_obj, out)
      out
    },
    format = "file"
  ),

  # Mission pairwise comparisons for density and damage (create CSVs)
  tar_target(
    locust_density_m1_pairwise_csv,
    {
      model_obj <- readRDS(locust_density_model_m1_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      pairs_df <- as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey"))
      out <- here::here("outputs", "tables", "locust_density", "mission_1_pairwise.csv")
      ensure_dir(dirname(out))
      readr::write_csv(pairs_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_density_m2_pairwise_csv,
    {
      model_obj <- readRDS(locust_density_model_m2_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      pairs_df <- as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey"))
      out <- here::here("outputs", "tables", "locust_density", "mission_2_pairwise.csv")
      ensure_dir(dirname(out))
      readr::write_csv(pairs_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_density_m3_pairwise_csv,
    {
      model_obj <- readRDS(locust_density_model_m3_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      pairs_df <- as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey"))
      out <- here::here("outputs", "tables", "locust_density", "mission_3_pairwise.csv")
      ensure_dir(dirname(out))
      readr::write_csv(pairs_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_m1_pairwise_csv,
    {
      model_obj <- readRDS(locust_damage_model_m1_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      pairs_df <- as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey"))
      out <- here::here("outputs", "tables", "locust_damage", "mission_1_pairwise.csv")
      ensure_dir(dirname(out))
      readr::write_csv(pairs_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_m2_pairwise_csv,
    {
      model_obj <- readRDS(locust_damage_model_m2_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      pairs_df <- as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey"))
      out <- here::here("outputs", "tables", "locust_damage", "mission_2_pairwise.csv")
      ensure_dir(dirname(out))
      readr::write_csv(pairs_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_m3_pairwise_csv,
    {
      model_obj <- readRDS(locust_damage_model_m3_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment * region, type = "response")
      pairs_df <- as.data.frame(emmeans::contrast(em, method = "pairwise", adjust = "tukey"))
      out <- here::here("outputs", "tables", "locust_damage", "mission_3_pairwise.csv")
      ensure_dir(dirname(out))
      readr::write_csv(pairs_df, out)
      out
    },
    format = "file"
  ),

  # Combined emmeans across missions (density)
  tar_target(
    locust_density_combined_emmeans_csv,
    {
      em1 <- readr::read_csv(locust_m1_emmeans_csv)
      em2 <- readr::read_csv(locust_m2_emmeans_csv)
      em3 <- readr::read_csv(locust_m3_emmeans_csv)
      em_all <- dplyr::bind_rows(
        em1 |> dplyr::mutate(mission = 1),
        em2 |> dplyr::mutate(mission = 2),
        em3 |> dplyr::mutate(mission = 3)
      )
      out <- here::here("outputs", "tables", "locust_density", "combined_mission_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_all, out)
      out
    },
    format = "file"
  ),

  # Combined emmeans across missions (damage) - create per-mission emmeans if missing
  tar_target(
    damage_m2_emmeans_csv,
    {
      model_obj <- readRDS(locust_damage_model_m2_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment)
      em_df <- as.data.frame(em)
      out <- here::here("outputs", "tables", "locust_damage", "mission_2_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    damage_m3_emmeans_csv,
    {
      model_obj <- readRDS(locust_damage_model_m3_rds)
      em <- emmeans::emmeans(model_obj, ~ fertilizer_treatment)
      em_df <- as.data.frame(em)
      out <- here::here("outputs", "tables", "locust_damage", "mission_3_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_df, out)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_combined_emmeans_csv,
    {
      em1 <- readr::read_csv(damage_m1_emmeans_csv)
      em2 <- readr::read_csv(damage_m2_emmeans_csv)
      em3 <- readr::read_csv(damage_m3_emmeans_csv)
      em_all <- dplyr::bind_rows(
        em1 |> dplyr::mutate(mission = 1),
        em2 |> dplyr::mutate(mission = 2),
        em3 |> dplyr::mutate(mission = 3)
      )
      out <- here::here("outputs", "tables", "locust_damage", "combined_mission_emmeans.csv")
      ensure_dir(dirname(out))
      readr::write_csv(em_all, out)
      out
    },
    format = "file"
  ),

  # Diagnostic plots for locust density (per mission)
  tar_target(
    locust_density_m1_diag_png,
    {
      model_obj <- readRDS(locust_density_model_m1_rds)
      p <- tryCatch(plot_model_diagnostics(model_obj, plot_title = "Mission 1"), error = function(e) NULL)
      outdir <- here::here("outputs", "figures", "locust_density")
      ensure_dir(outdir)
      out <- file.path(outdir, "mission_1_diagnostics.png")
      save_plot_safe(p, out, "diagnostic plot unavailable", width = 8, height = 6)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_density_m2_diag_png,
    {
      model_obj <- readRDS(locust_density_model_m2_rds)
      p <- tryCatch(plot_model_diagnostics(model_obj, plot_title = "Mission 2"), error = function(e) NULL)
      outdir <- here::here("outputs", "figures", "locust_density")
      ensure_dir(outdir)
      out <- file.path(outdir, "mission_2_diagnostics.png")
      save_plot_safe(p, out, "diagnostic plot unavailable", width = 8, height = 6)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_density_m3_diag_png,
    {
      model_obj <- readRDS(locust_density_model_m3_rds)
      p <- tryCatch(plot_model_diagnostics(model_obj, plot_title = "Mission 3"), error = function(e) NULL)
      outdir <- here::here("outputs", "figures", "locust_density")
      ensure_dir(outdir)
      out <- file.path(outdir, "mission_3_diagnostics.png")
      save_plot_safe(p, out, "diagnostic plot unavailable", width = 8, height = 6)
      out
    },
    format = "file"
  ),

  # Diagnostic plots for locust damage (per mission)
  tar_target(
    locust_damage_m1_diag_png,
    {
      model_obj <- readRDS(locust_damage_model_m1_rds)
      p <- tryCatch(plot_model_diagnostics(model_obj, plot_title = "Mission 1"), error = function(e) NULL)
      outdir <- here::here("outputs", "figures", "locust_damage")
      ensure_dir(outdir)
      out <- file.path(outdir, "mission_1_diagnostics.png")
      save_plot_safe(p, out, "diagnostic plot unavailable", width = 8, height = 6)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_m2_diag_png,
    {
      model_obj <- readRDS(locust_damage_model_m2_rds)
      p <- tryCatch(plot_model_diagnostics(model_obj, plot_title = "Mission 2"), error = function(e) NULL)
      outdir <- here::here("outputs", "figures", "locust_damage")
      ensure_dir(outdir)
      out <- file.path(outdir, "mission_2_diagnostics.png")
      save_plot_safe(p, out, "diagnostic plot unavailable", width = 8, height = 6)
      out
    },
    format = "file"
  ),

  tar_target(
    locust_damage_m3_diag_png,
    {
      model_obj <- readRDS(locust_damage_model_m3_rds)
      p <- tryCatch(plot_model_diagnostics(model_obj, plot_title = "Mission 3"), error = function(e) NULL)
      outdir <- here::here("outputs", "figures", "locust_damage")
      ensure_dir(outdir)
      out <- file.path(outdir, "mission_3_diagnostics.png")
      save_plot_safe(p, out, "diagnostic plot unavailable", width = 8, height = 6)
      out
    },
    format = "file"
  ),

  # Temperature GAM smooth plot (try project helper, else attempt plot function, else write placeholder)
  tar_target(
    locust_temperature_smooth_png,
    {
      model_path <- locust_temperature_gam_rds
      model_obj <- readRDS(model_path)
      outdir <- here::here("outputs", "figures", "locust_temperature")
      ensure_dir(outdir)
      out <- file.path(outdir, "temperature_gam_smooths.png")

      if (exists("plot_temperature_smooth", mode = "function")) {
        p <- tryCatch(plot_temperature_smooth(model_obj,
                                              smooth = "s(temperature,region)",
                                              xvar = "temperature",
                                              group = "region",
                                              palette = "Degas",
                                              ribbon_fill = "grey70",
                                              ribbon_alpha = 0.3), error = function(e) NULL)
        save_plot_safe(p, out, "temperature smooth unavailable", width = 8, height = 6)
      } else if (exists("save_temperature_smooth", mode = "function")) {
        tryCatch({ save_temperature_smooth(model_obj, smooth = "s(temperature,region)", xvar = "temperature", group = "region", filename = "temperature_gam_smooths") ; out }, error = function(e) writeLines("temperature smooth helper failed", out))
      } else {
        writeLines("temperature smooth helper not found", out)
      }
      out
    },
    format = "file"
  ),

  # Ground-cover GAM smooth plot (try project helper, else write placeholder)
  tar_target(
    ground_cover_smooth_png,
    {
      model_path <- ground_cover_mediation_rds
      model_obj <- readRDS(model_path)
      outdir <- here::here("outputs", "figures", "ground_cover")
      ensure_dir(outdir)
      out <- file.path(outdir, "ground_cover_gam_smooths.png")

      if (exists("plot_gam_smooths_gratia", mode = "function")) {
        p <- tryCatch(plot_gam_smooths_gratia(model_obj), error = function(e) NULL)
        save_plot_safe(p, out, "ground cover smooth unavailable", width = 8, height = 6)
      } else {
        writeLines("ground cover smooth helper not found", out)
      }
      out
    },
    format = "file"
  ),

  # Alias: create a file matching the cache_model name used in some .qmds
  tar_target(
    yield_locust_density_model_rds,
    {
      src <- yield_locust_model_rds
      out <- here::here("data", "model_objects", "yield_locust_density.rds")
      ensure_dir(dirname(out))
      file.copy(src, out, overwrite = TRUE)
      out
    },
    format = "file"
  ),

  # Manuscript-level model summary: scan model_objects and report class and nobs
  tar_target(
    manuscript_model_summary_csv,
    {
      files <- list.files(here::here("data", "model_objects"), pattern = "\\.rds$", full.names = TRUE)
      rows <- lapply(files, function(fp) {
        m <- tryCatch(readRDS(fp), error = function(e) NULL)
        cls <- if (!is.null(m)) paste(class(m)[1], collapse = ", ") else NA_character_
        n <- tryCatch(as.integer(stats::nobs(m)), error = function(e) NA_integer_)
        tibble::tibble(file = fp, model_class = cls, nobs = n)
      })
      out_df <- dplyr::bind_rows(rows)
      out <- here::here("outputs", "tables", "manuscript_methods_results", "model_summary_all_models.csv")
      ensure_dir(dirname(out))
      readr::write_csv(out_df, out)
      out
    },
    format = "file"
  ),

  # 10) Small convenience target: list of all saved model files (for debugging)
  tar_target(
    model_object_files,
    list.files(here::here("data", "model_objects"), pattern = "\\.rds$", full.names = TRUE),
    cue = tar_cue(mode = "always")
  )
)

# End of _targets.R
