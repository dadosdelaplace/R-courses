# ----- packages -----
library(fda)
library(tidyverse)
library(showtext)
library(sysfonts)

source("./aux_fun.R")

# ----- dataviz setting -----
font_add_google("Space Grotesk", "space")
showtext_auto()
theme_own <- 
  theme_minimal(base_family = "space") + 
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16.5),
        plot.subtitle =
          element_text(face = "bold", size = 12.5, color = "grey30"))

# ----- load and dataviz raw data -----

# Temperature
temp_tb <- 
  # Temperature matrix (365 x 35)
  CanadianWeather$dailyAv[, , 1] |> 
  as_tibble() |>
  mutate("day_month" = row.names(CanadianWeather$dailyAv[, , 1]),
         "day" = 1:365,
         "t" = seq(0, 1, l = 365), .before = everything()) |>
  pivot_longer(cols = -c(day_month, day, t), 
               names_to = "station", 
               values_to = "temp")

# Log-prec
prec_tb <- 
  # Log-precip matrix (365 x 35)
  CanadianWeather$dailyAv[, , 3] |> 
  as_tibble() |>
  mutate("day_month" = row.names(CanadianWeather$dailyAv[, , 1]),
         "day" = 1:365,
         "t" = seq(0, 1, l = 365), .before = everything()) |>
  pivot_longer(cols = -c(day_month, day, t), 
               names_to = "station", 
               values_to = "log_precip")

# Join them
data_tb <-
  temp_tb |> 
  left_join(prec_tb, by = c("day_month", "day", "t", "station"))



# ----- preprocess smooth data -----
data_smooth <-
  data_tb |> 
  mutate("temp_smooth" = smooth.spline(1:365, temp, spar = 0.9)$y,
         "log_precip_smooth" = smooth.spline(1:365, log_precip, spar = 0.9)$y,
         .by = c(station))

# ----- check depth of raw/smooth data -----
med_data <-
  compute_MED(data_tb, name_t = "day", variables = c("temp", "log_precip"),
              id_row = "station")
med_smooth_data <-
  compute_MED(data_smooth, name_t = "day",
              variables = c("temp_smooth", "log_precip_smooth"),
              id_row = "station")

# univariate
library(fdaoutlier)

# Temperature
ed_temp <-
  extremal_depth(dts =
                   data_smooth |> select(day, station, temp_smooth) |>
                   pivot_wider(names_from = "day", values_from = temp_smooth) |> 
                   select(-station))
# Precipitation
ed_prec <-
  extremal_depth(dts =
                   data_smooth |> select(day, station, log_precip_smooth) |>
                   pivot_wider(names_from = "day", values_from = log_precip_smooth) |> 
                   select(-station))
ed_data <-
  tibble("station" = unique(data_smooth$station),
         "ed_temp" = ed_temp, "ed_precip" = ed_prec) |>
  pivot_longer(cols = c(ed_temp, ed_precip),
               names_to = "variable", values_to = "ed_values") |> 
  mutate("variable" =
           if_else(str_detect(variable, "temp"),
                   "temp_smooth", "log_precip_smooth")) |>
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)", "Log-precip (log10(mm))"))) |> 
  mutate("ranking" = rank(ed_values), .by = c(variable))

# check differences before/after smoothing
med_data |> 
  left_join(med_smooth_data, by = c("id_row"),
            suffix = c("_raw", "_smooth")) |> 
  mutate("diff_ranking" = ranking_smooth != ranking_raw)
# 10 stations have +-1 ranking difference

# ----- dataviz -----
# bootstrap uncertainty
n_boot <- 499
stations <- unique(data_smooth$station)
days <- unique(data_smooth$day)
n_stations <- length(stations)
boot_means_temp <- boot_means_precip <- matrix(NA, nrow = n_boot, ncol = length(days))
for (b in 1:n_boot) {
  idx <- sample(1:n_stations, n_stations, replace = TRUE)
  boot_means_temp[b, ] <-
    data_smooth |>
    select(day, station, temp_smooth) |>
    pivot_wider(names_from = day, values_from = temp_smooth) |> 
    slice(idx) |> 
    summarise(across(-station, mean)) |> 
    as.matrix()
  
  boot_means_precip[b, ] <-
    data_smooth |>
    select(day, station, log_precip_smooth) |>
    pivot_wider(names_from = day, values_from = log_precip_smooth) |> 
    slice(idx) |> 
    summarise(across(-station, mean)) |> 
    as.matrix()
}

band_boot <-
  tibble("day" = sort(unique(data_smooth$day)),
         "fun_mean" =
           data_smooth |>
           summarise("fmean" = mean(temp_smooth), .by = day) |>
           pull(fmean),
         "sup_lim" = apply(boot_means_temp, 2, quantile, 0.975),
         "inf_lim" = apply(boot_means_temp, 2, quantile, 0.025),
         "variable" = "temp_smooth") |> 
  bind_rows(tibble("day" = sort(unique(data_smooth$day)),
                   "fun_mean" =
                     data_smooth |>
                     summarise("fmean" = mean(log_precip_smooth), .by = day) |>
                     pull(fmean),
                   "sup_lim" = apply(boot_means_precip, 2, quantile, 0.975),
                   "inf_lim" = apply(boot_means_precip, 2, quantile, 0.025),
                   "variable" = "log_precip_smooth")) |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))")))

gg1 <-
  data_tb |>
  pivot_longer(cols = c(temp, log_precip), names_to = "variable", 
               values_to = "values") |> 
  mutate("variable" =
           factor(variable, levels = c("temp", "log_precip"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))"))) |>
  mutate("fun_mean" = mean(values, na.rm = TRUE),
         .by = c(day, variable)) |> 
  ggplot(aes(x = day, y = values, group = station)) +
  geom_line(alpha = 0.4, color = "steelblue", linewidth = 0.3) +
  geom_line(aes(x = day, y = fun_mean), color = "darksalmon",
            linewidth = 1.1) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  labs(title = "Raw data", x = "Month", y = "Values")

gg2 <-
  data_smooth |>
  pivot_longer(cols = c(temp_smooth, log_precip_smooth), names_to = "variable", 
               values_to = "values") |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))"))) |>
  mutate("fun_mean" = mean(values, na.rm = TRUE),
         .by = c(day, variable)) |>
  left_join(med_smooth_data, by = c("station" = "id_row")) |> 
  ggplot() +
  geom_line(aes(x = day, y = values, group = station, color = med_values),
            alpha = 0.7, linewidth = 0.8) +
  # geom_ribbon(data = band_boot,
  #             aes(x = day, ymin = inf_lim, ymax = sup_lim),
  #             alpha = 0.25, fill = "darkgreen") +
  geom_line(aes(x = day, y = fun_mean), color = "grey20",
            linewidth = 0.7) +
  scale_color_distiller(palette  = "RdYlBu",
                        direction = 1) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  labs(title = "Smooth data (smooth parameter = 0.9). Depth measure: MED",
       color = "Multivariate extremal depth (MED)",
       x = "Month", y = "Values")

gg2b <-
  data_smooth |>
  pivot_longer(cols = c(temp_smooth, log_precip_smooth), names_to = "variable", 
               values_to = "values") |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))"))) |>
  mutate("fun_mean" = mean(values, na.rm = TRUE),
         .by = c(day, variable)) |>
  left_join(med_smooth_data, by = c("station" = "id_row")) |> 
  ggplot() +
  geom_tile(aes(x = day, y = station, fill = med_values),
            alpha = 0.5) +
  scale_fill_distiller(palette  = "RdYlBu", direction = 1) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  theme(legend.position = "right") +
  labs(subtitle = "Smooth data (smooth parameter = 0.9). Depth measure: MED",
       fill = "Multivariate extremal depth (MED)",
       x = "Month", y = "Station")

gg3 <-
  data_smooth |>
  pivot_longer(cols = c(temp_smooth, log_precip_smooth), names_to = "variable", 
               values_to = "values") |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))"))) |>
  mutate("fun_mean" = mean(values, na.rm = TRUE),
         .by = c(day, variable)) |>
  left_join(ed_data, by = c("station", "variable")) |> 
  ggplot() +
  geom_line(aes(x = day, y = values, group = station, color = ed_values),
            alpha = 0.7, linewidth = 0.8) +
  geom_line(aes(x = day, y = fun_mean), color = "grey20",
            linewidth = 0.7) +
  scale_color_distiller(palette  = "RdYlBu",
                        direction = 1) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  labs(title = "Smooth data (smooth parameter = 0.9) Depth measure: ED",
       color = "Univariate extremal depth (ED)",
       x = "Month", y = "Values")

gg3b <-
  data_smooth |>
  pivot_longer(cols = c(temp_smooth, log_precip_smooth), names_to = "variable", 
               values_to = "values") |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))"))) |>
  mutate("fun_mean" = mean(values, na.rm = TRUE),
         .by = c(day, variable)) |>
  left_join(ed_data, by = c("station", "variable")) |> 
  ggplot() +
  geom_tile(aes(x = day, y = station, fill = ed_values),
            alpha = 0.5) +
  scale_fill_distiller(palette  = "RdYlBu", direction = 1) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  theme(legend.position = "right") +
  labs(subtitle = "Smooth data (smooth parameter = 0.9) Depth measure: ED",
       fill = "Univariate extremal depth (ED)",
       x = "Month", y = "Station")
gg4 <-
  data_smooth |>
  pivot_longer(cols = c(temp_smooth, log_precip_smooth), names_to = "variable", 
               values_to = "values") |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))"))) |>
  mutate("fun_mean" = mean(values, na.rm = TRUE),
         .by = c(day, variable)) |>
  left_join(ed_data, by = c("station", "variable")) |> 
  left_join(med_smooth_data, by = c("station" = "id_row"),
            suffix = c("_ed", "_med")) |> 
  mutate("diff_ranking" = ranking_med - ranking_ed) |> 
  ggplot() +
  geom_line(aes(x = day, y = values, group = station, color = diff_ranking),
            alpha = 0.9, linewidth = 0.8) +
  scale_color_distiller(palette  = "RdYlBu",
                        direction = 1) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  labs(title = "Smooth data (smooth parameter = 0.9)",
       subtitle = "Blue (Δ > 0): more central in MED; Red (Δ < 0): outlier hidden by univariate ED",
       color = "Diff ranking (med - ed)",
       x = "Month", y = "Values")

gg5 <-
  ed_data |> 
  left_join(med_smooth_data, by = c("station" = "id_row"),
            suffix = c("_ed", "_med")) |> 
  mutate("diff_ranking" = ranking_med - ranking_ed) |> 
  ggplot(aes(x = med_values, y = ed_values)) +
  geom_point(aes(color = ranking_med),
             alpha = 0.8, size = 3.5) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30",
              linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = station), size = 3.1) +
  scale_color_distiller(palette  = "RdYlBu", direction = 1) +
  facet_wrap(~variable, scales = "free") +
  theme_own +
  labs(title = "Smooth data (smooth parameter = 0.9)",
       color = "Ranking (MED)",
       x = "MED values", y = "ED values")

gg6 <-
  ed_data |> 
  left_join(med_smooth_data, by = c("station" = "id_row"),
            suffix = c("_ed", "_med")) |> 
  mutate("diff_ranking" = ranking_med - ranking_ed) |>
  select(station, variable, ed_values, ranking_med) |> 
  pivot_wider(names_from = variable, values_from = ed_values) |> 
  ggplot(aes(x = `Temperature (°C)`, y = `Log-precip (log10(mm))`)) +
  geom_point(aes(color = ranking_med), alpha = 0.8, size = 3.5) +
  geom_abline(color = "grey30", linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = station), size = 2.7) +
  scale_color_distiller(palette  = "RdYlBu", direction = 1) +
  theme_own +
  labs(color = "Ranking (MED)",
       x = "ED values (temperature)", y = "ED values (log10(precipitation))")


library(patchwork)
gg1 / gg2
gg2 / gg3
gg2b / gg3b
gg4
(gg5 + gg6) + plot_layout(widths = c(1.2, 0.5))
# MED adds value mainly for stations like Calgary or Quebec:
# climatically unusual due to their joint temp-precipitation
# pattern throughout the year, rather than being extreme in
# any single variable. This is exactly the type of outlier
# that only a multivariate measure can detect



# ----- fpca: fda.usc version -----
library(fda.usc)

# pivot_wider
temp_smooth_wider <- 
  data_smooth |>
  select(day, temp_smooth, station) |>
  pivot_wider(names_from = day, values_from = temp_smooth)
log_precip_smooth_wider <- 
  data_smooth |>
  select(day, log_precip_smooth, station) |>
  pivot_wider(names_from = day, values_from = log_precip_smooth)
temp_fdata <- fdata(temp_smooth_wider[, -1] |> as.matrix(), argvals = 1:365)
prec_fdata <- fdata(log_precip_smooth_wider[, -1] |> as.matrix(), argvals = 1:365)

# FPCA
fpca_temp <- fdata2pc(temp_fdata, ncomp = 15)
fpca_prec <- fdata2pc(prec_fdata, ncomp = 15)

# Explained variance
cumvar_temp <- cumsum(fpca_temp$d^2) / sum(fpca_temp$d^2)
cumvar_prec <- cumsum(fpca_prec$d^2) / sum(fpca_prec$d^2)

# How many PC to retain 99%?
K_temp <- which(cumvar_temp >= 0.99)[1]
K_prec <- which(cumvar_prec >= 0.99)[1]

# outputs
fpca_temp$d # eigenautovalues λ_k
fpca_temp$basis # eigenfunctions ψ_k (fd object k x p) ($rotation same)
fpca_temp$coefs # scores ξ_ik (matrix 35 x p)
fpca_temp$coefs[, 1:K_temp] # selected scores ξ_ik (matrix 35 x K)
fpca_prec$coefs[, 1:K_prec] # selected scores ξ_ik (matrix 35 x K)
fpca_temp$mean # functional mean μ(t)

# Reconstruction: mean + scores %*% eigenfunctions
temp_approx_fda <-
  # (n x k) x (k x p) = n x p 
  t(matrix(rep(fpca_temp$mean$data, nrow(fpca_temp$coefs)),
           nrow = ncol(fpca_temp$basis$data))) +
  fpca_temp$coefs[, 1:K_temp] %*% fpca_temp$basis$data[1:K_temp, ]
temp_fdata_fpca <-
  fdata(temp_approx_fda, argvals = temp_fdata$argvals)

prec_approx_fda <-
  # (n x k) x (k x p) = n x p 
  t(matrix(rep(fpca_prec$mean$data, nrow(fpca_prec$coefs)),
           nrow = ncol(fpca_prec$basis$data))) +
  fpca_prec$coefs[, 1:K_temp] %*% fpca_prec$basis$data[1:K_temp, ]
prec_fdata_fpca <-
  fdata(prec_approx_fda, argvals = prec_fdata$argvals)

data_smooth_fpca <-
  tibble("station" = unique(data_smooth$station),
         as_tibble(prec_fdata_fpca$data)) |>
  pivot_longer(cols = -station, names_to = "day",
                        values_to = "log_precip_smooth_fpca") |>
  mutate("day" = as.numeric(day)) |>
  left_join(tibble("station" = unique(data_smooth$station),
                   as_tibble(temp_fdata_fpca$data)) |>
              pivot_longer(cols = -station, names_to = "day",
                           values_to = "temp_smooth_fpca") |>
              mutate("day" = as.numeric(day)), by = c("station", "day")) |> 
  left_join(med_smooth_data, by = c("station" = "id_row"))

gg7a <-
  data_smooth_fpca |>
  pivot_longer(cols = c(temp_smooth_fpca, log_precip_smooth_fpca),
               names_to = "variable", values_to = "values") |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth_fpca", "log_precip_smooth_fpca"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))"))) |>  
  ggplot() +
  geom_line(aes(x = day, y = values, group = station, color = med_values),
            alpha = 0.7, linewidth = 0.8) +
  scale_color_distiller(palette  = "RdYlBu", direction = 1) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  labs(title = "FPCA-based approximation",
       subtitle = "FPCA applied to smooth data (smooth parameter = 0.9)",
       color = "Multivariate extremal depth (MED)",
       x = "Month", y = "Values")

fpc_smooth <-
  t(fpca_temp$basis$data[1:K_temp, ]) |> as_tibble() |> 
  rename(PC1 = V1, PC2 = V2, PC3 = V3) |> 
  mutate("day" = 1:365) |> 
  pivot_longer(cols = -day, names_to = "PC",
               values_to = "values") |> 
  mutate("variable" = "temp_smooth") |> 
  bind_rows(t(fpca_prec$basis$data[1:K_prec, ]) |> as_tibble() |> 
              rename(PC1 = V1, PC2 = V2, PC3 = V3) |> 
              mutate("day" = 1:365) |> 
              pivot_longer(cols = -day, names_to = "PC",
                           values_to = "values") |> 
              mutate("variable" = "log_precip_smooth")) |> 
  mutate("variable" =
           factor(variable, levels = c("temp_smooth", "log_precip_smooth"),
                  labels = c("Temperature (°C)",
                             "Log-precip (log10(mm))")))

gg7b <-
  fpc_smooth |> 
  ggplot() +
  geom_line(aes(x = day, y = values, group = PC, color = PC),
            linewidth = 1.5) +
  scale_color_manual(values = c("#f4a582", "#4dac26", "#92c5de")) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  theme_own +
  labs(title = "FPCA-based approximation",
       color = "FPC",
       x = "Month", y = "Values")

scores_data <-
  tibble("station" = unique(data_smooth$station),
         as_tibble(fpca_temp$coefs[, 1:2]),
         "variable" = "Temperature (°C)") |> 
  bind_rows(tibble("station" = unique(data_smooth$station),
                   as_tibble(fpca_prec$coefs[, 1:2]),
                   "variable" = "Log-precip (log10(mm))")) |> 
  left_join(med_smooth_data, by = c("station" = "id_row"))

# Scatter PC1 vs PC2
gg7c <- 
  scores_data |> 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = med_values), size = 4, alpha = 0.8) +
  geom_vline(aes(xintercept = 0), color = "grey30", linetype = "dashed") +
  geom_hline(aes(yintercept = 0), color = "grey30", linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = station), size = 4) +
  scale_color_distiller(palette  = "RdYlBu", direction = 1) +
  facet_wrap(~variable, scales = "free") +
  theme_own +
  labs(title = "FPCA decomposition",
       subtitle = glue::glue("Var explained in temp = {round(cumvar_temp[2], 2)}, Var explained in precip = {round(cumvar_prec[2], 2)}"),
       color = "Multivariate extremal depth (MED)",
       x = "PC1", y = "PC2")
gg7a / gg7b
gg7c
# Temp PC1 shows enormous variability: temperature dominates the total
# functional variance. FPCA "sees" temperature much more than precipitation.
# Furthermore, PC1 captures the mean temperature level or total
# precipitation volume (left: colder and wetter regions,
# right: drier and warmer ones), while PC2 captures more the 
# "climate type" (seasonal variability)."

# fpca version with bspline basis and/or Fourier basis?

# ----- export -----
write_csv(data_tb, file = "./data/data_tb.csv")
write_csv(data_smooth, file = "./data/data_smooth.csv")
save(fpca_temp, file = "./data/fpca_temp.RData")
save(fpca_prec, file = "./data/fpca_prec.RData")
