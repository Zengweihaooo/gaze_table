# ================= Packages =================
required_pkgs <- c("readxl", "dplyr", "stringr", "forcats", "ggplot2", "cowplot", "gridExtra", "tidyr")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# ================= Helper Functions =================
safe_format <- function(x) {
  format(round(x, 2), nsmall = 2, trim = TRUE)
}

coalesce_lookup <- function(keys, map, default = NA_character_) {
  res <- unname(map[keys])
  res[is.na(res)] <- default
  res
}

make_plot_for_metric <- function(df_metric, metric_name, output_base) {
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
      Category = coalesce_lookup(Category_raw, mode_labels, stringr::str_to_title(Category_raw)),
      Category = factor(Category, levels = unique(mode_labels[mode_levels]), ordered = TRUE),
      DifficultyLabel = coalesce_lookup(as.character(difficulty), difficulty_labels, stringr::str_to_title(as.character(difficulty))),
      series = coalesce_lookup(as.character(difficulty), series_map, "Low"),
      series = factor(series, levels = c("Benchmark", "Low", "Mid", "High")),
      slot = coalesce_lookup(as.character(difficulty), slot_map, "L"),
      slot = factor(slot, levels = c("L", "M", "C", "R", "B"))
    )

  # 计算绘图位置
  group_w <- 0.8
  bar_w <- group_w / 2
  delta <- group_w / 4
  gap <- 0.03
  offset_map <- c(L = -delta - gap/2, M = 0, C = 0, R = delta + gap/2, B = 0)
  x_base <- as.numeric(df_metric$Category)
  df_metric <- df_metric %>% mutate(x = x_base + unname(offset_map[as.character(slot)]))

  # 轴设定
  y_lower <- min(df_metric$mean - df_metric$se, na.rm = TRUE)
  y_upper <- max(df_metric$mean + df_metric$se, na.rm = TRUE)
  y_range <- y_upper - y_lower
  if (is.na(y_range) || y_range == 0) {
    span <- max(abs(c(y_lower, y_upper)), na.rm = TRUE)
    if (is.na(span) || span == 0) span <- 1
    y_lower <- -span
    y_upper <- span
  }
  padding <- max(0.05 * max(abs(y_lower), abs(y_upper)), 1e-6)
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

  # 柱状图
  p_bars <- ggplot(df_metric, aes(x = x, y = mean, fill = series)) +
    geom_hline(yintercept = y_breaks, color = "#E5E5E5", linewidth = 0.5) +
    geom_col(width = bar_w, position = "identity", colour = NA) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = bar_w * 0.35, colour = "#484D5F", linewidth = 0.6) +
    scale_fill_manual(values = colour_map) +
    scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
    coord_cartesian(ylim = y_limits, expand = 0) +
    theme_base +
    ggtitle(metric_name)

  # 数据标签
  lab_df <- df_metric %>% mutate(label = sprintf("%.2f [%.2f, %.2f]", mean, mean - se, mean + se))
  p_bars_labels <- p_bars +
    geom_text(data = lab_df, aes(label = label), vjust = -0.6, size = 3.4)

  # 表格数据（行：难度，列：模式）
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

  metric_safe <- stringr::str_replace_all(metric_name, "[^A-Za-z0-9]+", "_") %>% stringr::str_replace_all("^_+|_+$", "")

  # 输出目录
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

output_base <- file.path("descriptive", "mode_difficulty_output")
if (!dir.exists(output_base)) dir.create(output_base, recursive = TRUE)

metrics <- unique(mode_difficulty$metric)
message("检测到 ", length(metrics), " 个 metric：", paste(metrics, collapse = ", "))

for (metric_name in metrics) {
  df_metric <- mode_difficulty %>% filter(metric == !!metric_name)
  make_plot_for_metric(df_metric, metric_name, output_base)
}

message("全部图表已生成，保存于 ", output_base)
