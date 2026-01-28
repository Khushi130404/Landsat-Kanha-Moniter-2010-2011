# ===============================
# Landsat 4/5 → Landsat 7 NDVI Harmonization
# ===============================

library(dplyr)
library(lubridate)
library(readr)
library(purrr)
library(ggplot2)
library(tools)

# -------- PATHS --------
l7_dir  <- "D:/Landsat_Kanha_Moniter_2010_2011/Data_Table/Landsat_7/data_sg_smoothed"
l45_dir <- "D:/Landsat_Kanha_Moniter_2010_2011/Data_Table/Landsat_4_5/data_sg_smoothed"

out_data_dir <- "D:/Landsat_Kanha_Moniter_2010_2011/Data_Table/Landsat_4_5/data_sg_scaled_to_L7"

plot_3curve_dir <- "D:/Landsat_Kanha_Moniter_2010_2011/image/Comparison/plot_harmonization"

summary_csv <- "D:/Landsat_Kanha_Moniter_2010_2011/landsat45_to_7_harmonization_summary.csv"

dir.create(out_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plot_3curve_dir, recursive = TRUE, showWarnings = FALSE)

# -------- PARAMETERS --------
max_day_diff <- 5

# -------- FILE LIST --------
l7_files  <- list.files(l7_dir,  pattern = "\\.csv$", full.names = TRUE)
l45_files <- list.files(l45_dir, pattern = "\\.csv$", full.names = TRUE)

common_names <- intersect(basename(l7_files), basename(l45_files))

# -------- NEAREST MATCH FUNCTION --------
nearest_match <- function(d1, d2) {
  if (length(d2) == 0 || all(is.na(d2))) return(NA_integer_)
  which.min(abs(as.numeric(difftime(d2, d1, units = "days"))))
}

# -------- PROCESS FILES --------
results <- map(common_names, function(fname) {

  cat("Processing:", fname, "\n")

  region_name <- paste(strsplit(fname, "_")[[1]][1:2], collapse = "_")

  # ---------- READ DATA ----------
  l7  <- read_csv(file.path(l7_dir, fname), show_col_types = FALSE)
  l45 <- read_csv(file.path(l45_dir, fname), show_col_types = FALSE)

  if (nrow(l7) == 0 || nrow(l45) == 0) return(NULL)

  l7$date  <- as.Date(l7$date)
  l45$date <- as.Date(l45$date)

  l7  <- l7  %>% select(date, ndvi_sg)
  l45 <- l45 %>% select(date, ndvi_sg)

  # ---------- DATE MATCHING ----------
  matched <- l7 %>%
    rowwise() %>%
    mutate(
      idx_l45  = nearest_match(date, l45$date),
      date_l45 = if (!is.na(idx_l45)) l45$date[idx_l45] else NA_Date_,
      ndvi_l45 = if (!is.na(idx_l45)) l45$ndvi_sg[idx_l45] else NA_real_
    ) %>%
    ungroup() %>%
    mutate(
      day_diff = abs(as.numeric(difftime(date, date_l45, units = "days")))
    )

  # ---- NOW filter (day_diff definitely exists) ----
  matched <- matched %>%
    dplyr::filter(
      !is.na(ndvi_l45),
      day_diff <= max_day_diff
    ) %>%
    rename(ndvi_l7 = ndvi_sg)

  # ---------- OLS REGRESSION ----------
  model <- lm(ndvi_l7 ~ ndvi_l45, data = matched)

  # ---------- APPLY MODEL ----------
  l45_scaled <- l45 %>%
    mutate(
      ndvi_sg_scaled = coef(model)[1] + coef(model)[2] * ndvi_sg
    )

  write_csv(l45_scaled, file.path(out_data_dir, fname))

  # ============================================================
  # 3-CURVE PLOT (L7 + L4/5 RAW + HARMONIZED)
  # ============================================================
  plot_3curve <- bind_rows(
    l7 %>% mutate(sensor = "Landsat 7 (ETM+)", ndvi = ndvi_sg),
    l45 %>% mutate(sensor = "Landsat 4/5 (TM) raw", ndvi = ndvi_sg),
    l45_scaled %>% mutate(sensor = "Landsat 4/5 (TM) harmonized", ndvi = ndvi_sg_scaled)
  )

  p <- ggplot(plot_3curve, aes(date, ndvi, color = sensor)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.2) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b (%Y)") +
    labs(
      title = "NDVI Time Series (L4/5 → L7 Harmonization)",
      subtitle = file_path_sans_ext(fname),
      x = "Time",
      y = "NDVI",
      color = "Sensor"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    filename = paste0(file_path_sans_ext(fname), "_3curve.png"),
    plot = p,
    path = plot_3curve_dir,
    width = 9,
    height = 4.8,
    dpi = 300
  )

  tibble(
    region = region_name,
    intercept = coef(model)[1],
    slope = coef(model)[2],
    r2 = summary(model)$r.squared,
    n_pairs = nrow(matched)
  )
})

# -------- SAVE SUMMARY --------
reg_summary <- bind_rows(results)
write_csv(reg_summary, summary_csv)

cat("ALL REGIONS DONE ✅\n")
