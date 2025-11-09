# ================= Packages =================
required_pkgs <- c(
  "readxl", "dplyr", "stringr", "forcats", "ggplot2",
  "cowplot", "gridExtra", "tidyr", "tibble"
)
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# ================= Helper Functions =================
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || is.na(lhs)) rhs else lhs
}

safe_format <- function(x) {
  format(round(x, 2), nsmall = 2, trim = TRUE)
}

p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p <= 0.001) return("***")
  if (p <= 0.01)  return("**")
  if (p <= 0.05)  return("*")
  ""
}

get_bar_x <- function(df_metric, mode_value, diff_value) {
  row <- df_metric %>% dplyr::filter(mode == !!mode_value, difficulty == !!diff_value)
  if (nrow(row) == 0) return(NA_real_)
  row$x[1]
}

compute_band_layout <- function(y_limits) {
  rng <- diff(range(y_limits, na.rm = TRUE))
  if (!is.finite(rng) || rng <= 0) rng <- 1
  list(
    bottom_base = y_limits[1] + 0.15 * rng,
    middle_base = y_limits[1] + 0.45 * rng,
    top_base    = y_limits[2] - 0.12 * rng,
    step_dy     = 0.06 * rng,
    tick_h      = 0.02 * rng,
    label_offset = 0.035 * rng
  )
}

resolve_sig_colour <- function(band, diff_value, difficulty_colours) {
  if (!is.na(band) && tolower(band) == "within") return("#000000")
  diff_key <- tolower(diff_value)
  if (diff_key %in% names(difficulty_colours)) {
    return(difficulty_colours[[diff_key]])
  }
  "#000000"
}

build_sig_geoms <- function(df_metric, sig_rows, difficulty_colours, y_limits) {
  if (is.null(sig_rows) || nrow(sig_rows) == 0) {
    return(list(segments = tibble::tibble(), texts = tibble::tibble()))
  }

  layout <- compute_band_layout(y_limits)
  seg_list <- list()
  text_list <- list()

  for (i in seq_len(nrow(sig_rows))) {
    row <- sig_rows[i, ]
    x1 <- get_bar_x(df_metric, row$left_mode, row$left_diff)
    x2 <- get_bar_x(df_metric, row$right_mode, row$right_diff)
    if (is.na(x1) || is.na(x2) || identical(x1, x2)) next

    band_key <- tolower(row$band %||% "across")
    step_val <- row$step %||% 0
    base_y <- switch(
      band_key,
      bench   = layout$bottom_base,
      within  = layout$middle_base,
      across  = layout$top_base,
      layout$top_base
    )
    y0 <- base_y + step_val * layout$step_dy

    x_left <- min(x1, x2)
    x_right <- max(x1, x2)

    label_txt <- row$label
    if (is.null(label_txt) || !nzchar(label_txt)) {
      label_txt <- p_to_stars(row$p)
    }
    if (!nzchar(label_txt)) next

    diff_colour <- resolve_sig_colour(row$band, row$right_diff, difficulty_colours)

    seg_df <- tibble::tibble(
      x = c(x_left, x_left, x_right),
      xend = c(x_left, x_right, x_right),
      y = c(y0 - layout$tick_h, y0, y0 - layout$tick_h),
      yend = c(y0, y0, y0),
      col = diff_colour
    )
    seg_list[[length(seg_list) + 1]] <- seg_df

    text_df <- tibble::tibble(
      x = (x_left + x_right) / 2,
      y = y0 + layout$label_offset,
      lab = label_txt,
      col = diff_colour
    )
    text_list[[length(text_list) + 1]] <- text_df
  }

  list(
    segments = if (length(seg_list)) dplyr::bind_rows(seg_list) else tibble::tibble(),
    texts    = if (length(text_list)) dplyr::bind_rows(text_list) else tibble::tibble()
  )
}

make_plot_for_metric <- function(df_metric, metric_name, output_base, sig_specs = NULL) {
  mode_levels <- c("baselineV", "baselineR", "controller", "eye", "head", "body", "face")
  mode_labels <- c(
    baselineV = "BenchmarkV",
    baselineR = "BenchmarkR",
    controller = "Controller",
    eye        = "Gaze",
    head       = "Head",
    body       = "Body",
    face       = "Face"
  )
  difficulty_levels <- c("control", "baseline", "easy", "medium", "hard")
  difficulty_labels <- c(
    control = "Benchmark",
    baseline = "Baseline",
    easy = "Easy",
    medium = "Medium",
    hard = "Hard"
  )
  slot_map <- c(control = "C", baseline = "B", easy = "L", medium = "M", hard = "R")
  series_map <- c(control = "Benchmark", baseline = "Benchmark", easy = "Low", medium = "Mid", hard = "High")
  colour_map <- c(Benchmark = "#B9BCDB", Low = "#8CEAB4", Mid = "#F6D365", High = "#F1917E")
  difficulty_colour_map <- c(
    control = colour_map["Benchmark"],
    baseline = colour_map["Benchmark"],
    easy = colour_map["Low"],
    medium = colour_map["Mid"],
    hard = colour_map["High"]
  )

  df_metric <- df_metric %>%
    mutate(
      mode = factor(mode, levels = mode_levels),
      difficulty = factor(difficulty, levels = difficulty_levels)
    ) %>%
    filter(!is.na(mode), !is.na(difficulty)) %>%
    arrange(mode, difficulty)

  if (nrow(df_metric) == 0) {
    warning(sprintf("No data available for metric '%s'", metric_name))
    return(invisible(NULL))
  }

  df_metric <- df_metric %>%
    mutate(
      Category_raw = as.character(mode),
      Category = mode_labels[Category_raw],
      Category = factor(Category, levels = mode_labels[mode_levels], ordered = TRUE),
      DifficultyLabel = difficulty_labels[as.character(difficulty)],
      series = factor(series_map[as.character(difficulty)], levels = c("Benchmark", "Low", "Mid", "High")),
      slot = factor(slot_map[as.character(difficulty)], levels = c("L", "M", "C", "R", "B"))
    )

  group_w <- 0.8
  bar_w <- group_w / 2
  delta <- group_w / 4
  gap <- 0.03
  offset_map <- c(L = -delta - gap/2, M = 0, C = 0, R = delta + gap/2, B = 0)
  x_base <- as.numeric(df_metric$Category)
  df_metric <- df_metric %>% mutate(x = x_base + unname(offset_map[as.character(slot)]))

  y_lower <- min(df_metric$mean - df_metric$se, na.rm = TRUE)
  y_upper <- max(df_metric$mean + df_metric$se, na.rm = TRUE)
  rng <- y_upper - y_lower
  if (!is.finite(rng) || rng == 0) {
    span <- max(abs(c(y_lower, y_upper)), na.rm = TRUE)
    if (!is.finite(span) || span == 0) span <- 1
    y_lower <- -span
    y_upper <- span
  }
  padding <- max(0.08 * max(abs(y_lower), abs(y_upper)), 0.5)
  y_limits <- c(y_lower - padding, y_upper + padding)
  y_breaks <- pretty(y_limits, n = 6)

  x_breaks <- seq_along(levels(df_metric$Category))
  x_labels <- levels(df_metric$Category)

  theme_base <- theme_minimal(base_size = 13) +
    theme(
      legend.position    = "none",
      panel.background   = element_rect(fill = "#FFFFFF", color = NA),
      plot.background    = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.ticks.y       = element_blank(),
      axis.line.x        = element_blank(),
      axis.line.y        = element_blank(),
      axis.text.x        = element_text(size = 12, face = "bold", margin = margin(t = 10, b = 10)),
      axis.text.y        = element_text(size = 11),
      axis.title         = element_blank()
    )

  p_bars <- ggplot(df_metric, aes(x = x, y = mean, fill = series)) +
    geom_hline(yintercept = y_breaks, color = "#E5E5E5", linewidth = 0.5) +
    geom_col(width = bar_w, position = "identity", colour = NA) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = bar_w * 0.35,
                  colour = "#484D5F", linewidth = 0.6) +
    scale_fill_manual(values = colour_map) +
    scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
    coord_cartesian(ylim = y_limits, expand = 0) +
    theme_base +
    ggtitle(metric_name)

  if (!is.null(sig_specs) && nrow(sig_specs) > 0) {
    sig_rows <- sig_specs %>% dplyr::filter(metric == !!metric_name)
    if (nrow(sig_rows) > 0) {
      sig_geoms <- build_sig_geoms(df_metric, sig_rows, difficulty_colour_map, y_limits)
      if (nrow(sig_geoms$segments) > 0) {
        p_bars <- p_bars +
          geom_segment(
            data = sig_geoms$segments,
            aes(x = x, xend = xend, y = y, yend = yend, colour = col),
            inherit.aes = FALSE,
            linewidth = 0.6,
            linetype = "dashed"
          )
      }
      if (nrow(sig_geoms$texts) > 0) {
        p_bars <- p_bars +
          geom_text(
            data = sig_geoms$texts,
            aes(x = x, y = y, label = lab, colour = col),
            inherit.aes = FALSE,
            size = 4,
            fontface = "bold"
          )
      }
      if (nrow(sig_geoms$segments) > 0 || nrow(sig_geoms$texts) > 0) {
        p_bars <- p_bars + scale_colour_identity(guide = "none")
      }
    }
  }

  lab_df <- df_metric %>% mutate(label = sprintf("%.2f [%.2f, %.2f]", mean, mean - se, mean + se))
  p_bars_labels <- p_bars +
    geom_text(data = lab_df, aes(label = label), vjust = -0.6, size = 3.4)

  table_df <- df_metric %>%
    mutate(
      DifficultyDisplay = factor(DifficultyLabel, levels = difficulty_labels[difficulty_levels], ordered = TRUE),
      Category = factor(Category, levels = x_labels, ordered = TRUE)
    ) %>%
    select(DifficultyDisplay, Category, P = P) %>%
    tidyr::pivot_wider(names_from = Category, values_from = P, values_fill = "-") %>%
    arrange(DifficultyDisplay) %>%
    rename(Difficulty = DifficultyDisplay) %>%
    mutate(Difficulty = as.character(Difficulty)) %>%
    as.data.frame()

  table_theme <- gridExtra::ttheme_minimal(
    core = list(fg_params = list(fontsize = 11, fontfamily = "Helvetica"), padding = grid::unit(c(6, 4), "pt")),
    colhead = list(fg_params = list(fontsize = 12, fontface = "bold", fontfamily = "Helvetica"), padding = grid::unit(c(6, 4), "pt")),
    rowhead = list(fg_params = list(fontsize = 12, fontface = "bold", fontfamily = "Helvetica"), padding = grid::unit(c(6, 4), "pt"))
  )
  table_grob <- gridExtra::tableGrob(table_df, rows = NULL, theme = table_theme)

  combined <- cowplot::plot_grid(p_bars_labels, table_grob, ncol = 1, rel_heights = c(3.2, 1.6))

  metric_safe <- stringr::str_replace_all(metric_name, "[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("^_+|_+$", "")

  plots_only_dir <- file.path(output_base, "plots_only")
  plots_with_table_dir <- file.path(output_base, "plots_with_table")
  if (!dir.exists(plots_only_dir)) dir.create(plots_only_dir, recursive = TRUE)
  if (!dir.exists(plots_with_table_dir)) dir.create(plots_with_table_dir, recursive = TRUE)

  ggsave(file.path(plots_only_dir, paste0(metric_safe, "_bars.png")), p_bars, width = 10, height = 4.5, dpi = 300, bg = "white")
  ggsave(file.path(plots_with_table_dir, paste0(metric_safe, "_bars_table.png")), combined, width = 10, height = 6.5, dpi = 300, bg = "white")

  invisible(list(bars = p_bars, combined = combined))
}

# ================= Data Preparation =================
input_path <- file.path("descriptive", "PERFORMANCE_descriptive.xlsx")
if (!file.exists(input_path)) {
  stop(sprintf("输入文件 '%s' 不存在", input_path))
}

mode_difficulty <- readxl::read_excel(input_path, sheet = "mode_difficulty") %>%
  mutate(
    metric = as.character(metric),
    mode = as.character(mode),
    difficulty = as.character(difficulty),
    mean = as.numeric(mean),
    sd = as.numeric(sd),
    se = as.numeric(se)
  ) %>%
  mutate(P = paste0(safe_format(mean), "±", safe_format(sd)))

sig_path <- file.path("Factorial", "PERFORMANCE_FACTORIAL.xlsx")
sig_specs <- tryCatch({
  readxl::read_excel(sig_path, sheet = "sig") %>%
    mutate(
      metric = as.character(metric),
      band = as.character(band),
      left_mode = as.character(left_mode),
      left_diff = as.character(left_diff),
      right_mode = as.character(right_mode),
      right_diff = as.character(right_diff),
      p = as.numeric(p),
      label = as.character(label),
      step = dplyr::coalesce(as.integer(step), 0L)
    )
}, error = function(e) {
  message("未找到或无法读取 sig 工作表：", e$message)
  tibble::tibble()
})

output_base <- file.path("descriptive", "mode_difficulty_output")
if (!dir.exists(output_base)) dir.create(output_base, recursive = TRUE)

metrics <- unique(mode_difficulty$metric)
message("检测到 ", length(metrics), " 个 metric：", paste(metrics, collapse = ", "))

for (metric_name in metrics) {
  df_metric <- mode_difficulty %>% filter(metric == !!metric_name)
  make_plot_for_metric(df_metric, metric_name, output_base, sig_specs)
}

message("全部图表已生成，保存于 ", output_base)
