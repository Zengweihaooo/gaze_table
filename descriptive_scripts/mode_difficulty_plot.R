# ================= Packages =================
required_pkgs <- c("readxl", "dplyr", "stringr", "forcats", "ggplot2", "cowplot", "gridExtra", "tidyr", "tibble")
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

normalize_mode_name <- function(x) {
  if (length(x) == 0) return(character())
  cleaned <- stringr::str_replace_all(stringr::str_to_lower(trimws(x)), "[^a-z0-9]", "")
  lookup <- c(
    baselinev = "baselineV",
    benchmarkv = "baselineV",
    baseliner = "baselineR",
    benchmarkr = "baselineR",
    controller = "controller",
    eye = "eye",
    gaze = "eye",
    head = "head",
    body = "body",
    face = "face"
  )
  res <- unname(lookup[cleaned])
  res
}

normalize_diff_name <- function(x) {
  if (length(x) == 0) return(character())
  cleaned <- stringr::str_to_lower(trimws(x))
  lookup <- c(control = "control", baseline = "baseline", easy = "easy", hard = "hard")
  res <- unname(lookup[cleaned])
  res
}

get_band_value <- function(map, key, fallback = NULL) {
  val <- map[[key]]
  if (!is.null(val)) return(val)
  if (!is.null(fallback)) {
    val <- map[[fallback]]
    if (!is.null(val)) return(val)
  }
  map[[1]]
}

load_sig_specs <- function(factorial_path) {
  if (!file.exists(factorial_path)) {
    warning(sprintf("显著性文件 '%s' 不存在，跳过显著性标注", factorial_path))
    return(tibble::tibble())
  }

  safe_read <- function(sheet) {
    tryCatch(readxl::read_excel(factorial_path, sheet = sheet), error = function(e) NULL)
  }

  bench_step_map <- c(
    controller = 0L,
    eye = 1L,
    head = 2L,
    body = 3L,
    face = 4L,
    baselineR = 5L
  )

  bench_specs <- {
    raw <- safe_read("posthoc_vs_baselineV")
    if (is.null(raw)) {
      tibble::tibble()
    } else {
      parts <- stringr::str_split_fixed(raw$contrast, "-", 2)
      target_mode <- normalize_mode_name(stringr::str_trim(parts[, 1]))
      reference_mode <- normalize_mode_name(stringr::str_trim(parts[, 2]))
      difficulty_clean <- normalize_diff_name(raw$difficulty)
      tibble::tibble(
        metric = as.character(raw$METRIC),
        band = "bench",
        left_mode = "baselineV",
        left_diff = "control",
        right_mode = target_mode,
        right_diff = difficulty_clean,
        p = dplyr::coalesce(as.numeric(raw[["p.value"]]), as.numeric(raw[["p_holm"]])),
        label = raw$sig_holm,
        step = bench_step_map[target_mode],
        reference_mode = reference_mode
      ) %>%
        dplyr::filter(
          !is.na(right_mode),
          reference_mode == "baselineV",
          !is.na(right_diff)
        ) %>%
        dplyr::select(-reference_mode)
    }
  }

  mode_diff_raw <- safe_read("posthoc_mode_difficulty")
  within_specs <- tibble::tibble()
  across_specs <- tibble::tibble()

  if (!is.null(mode_diff_raw)) {
    within_specs <- mode_diff_raw %>%
      dplyr::filter(contrast_type == "simple:difficulty|mode") %>%
      dplyr::mutate(
        mode_token = stringr::str_match(contrast, "difficulty_in_([^,:]+)")[, 2],
        mode_clean = normalize_mode_name(mode_token)
      ) %>%
      dplyr::filter(!is.na(mode_clean)) %>%
      dplyr::transmute(
        metric = as.character(METRIC),
        band = "within",
        left_mode = mode_clean,
        left_diff = "easy",
        right_mode = mode_clean,
        right_diff = "hard",
        p = dplyr::coalesce(as.numeric(`p.value`), as.numeric(p_holm)),
        label = sig_holm,
        step = 0L
      )

    across_specs <- mode_diff_raw %>%
      dplyr::filter(contrast_type == "simple:mode|difficulty") %>%
      dplyr::mutate(
        contrast_body = stringr::str_remove(contrast, "^[^:]+:"),
        parts = stringr::str_split_fixed(contrast_body, "-", 2),
        left_token = stringr::str_trim(parts[, 1]),
        right_token = stringr::str_trim(parts[, 2]),
        left_mode_raw = stringr::str_split_fixed(left_token, ",", 2)[, 1],
        right_mode_raw = stringr::str_split_fixed(right_token, ",", 2)[, 1],
        diff_clean = normalize_diff_name(by_level),
        left_mode_clean = normalize_mode_name(left_mode_raw),
        right_mode_clean = normalize_mode_name(right_mode_raw)
      ) %>%
      dplyr::filter(!is.na(diff_clean), !is.na(left_mode_clean), !is.na(right_mode_clean)) %>%
      dplyr::transmute(
        metric = as.character(METRIC),
        band = "across",
        left_mode = left_mode_clean,
        left_diff = diff_clean,
        right_mode = right_mode_clean,
        right_diff = diff_clean,
        p = dplyr::coalesce(as.numeric(`p.value`), as.numeric(p_holm)),
        label = sig_holm,
        step = NA_integer_
      )
  }

  sig_specs <- dplyr::bind_rows(bench_specs, within_specs, across_specs) %>%
    dplyr::filter(
      !is.na(metric),
      !is.na(left_mode),
      !is.na(right_mode),
      !is.na(left_diff),
      !is.na(right_diff)
    ) %>%
    dplyr::mutate(
      show_flag = (!is.na(label) & nzchar(label)) | (!is.na(p) & p <= 0.05)
    ) %>%
    dplyr::filter(show_flag) %>%
    dplyr::select(-show_flag)

  if (nrow(sig_specs) == 0) {
    return(sig_specs)
  }

  sig_specs %>%
    dplyr::mutate(
      auto_group = dplyr::case_when(
        band == "bench" ~ right_mode,
        band == "across" ~ paste0("across_", left_diff),
        TRUE ~ "within"
      )
    ) %>%
    dplyr::group_by(metric, band, auto_group) %>%
    dplyr::mutate(
      step = dplyr::coalesce(as.integer(step), as.integer(dplyr::row_number() - 1L))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-auto_group)
}

p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p <= 0.001) return("***")
  if (p <= 0.01)  return("**")
  if (p <= 0.05)  return("*")
  ""
}

resolve_sig_label <- function(label, p) {
  if (!is.na(label) && nzchar(label)) return(label)
  p_to_stars(p)
}

get_bar_x <- function(df_metric, mode_value, difficulty_value) {
  row <- df_metric %>%
    dplyr::filter(mode == !!mode_value, difficulty == !!difficulty_value)
  if (nrow(row) == 0) return(NA_real_)
  row$x[1]
}

compute_band_layout <- function(y_limits) {
  rng <- diff(range(y_limits))
  if (!is.finite(rng) || rng <= 0) rng <- 1
  bases <- list(
    bench = y_limits[1] + 0.08 * rng,
    within = y_limits[1] + 0.50 * rng,
    across = y_limits[2] - 0.12 * rng
  )
  steps <- list(
    bench = 0.05 * rng,
    within = 0.04 * rng,
    across = 0.08 * rng
  )
  list(
    base = bases,
    step = steps,
    tick_height = 0.02 * rng,
    label_offset = 0.03 * rng
  )
}

resolve_sig_colour <- function(band, left_diff, right_diff, difficulty_colours) {
  band_key <- tolower(ifelse(is.na(band), "across", band))
  if (band_key == "within") return("#000000")
  diffs <- c(right_diff, left_diff)
  for (diff_value in diffs) {
    if (!is.na(diff_value) && diff_value %in% names(difficulty_colours)) {
      col <- difficulty_colours[[diff_value]]
      if (!is.na(col)) return(col)
    }
  }
  "#000000"
}

calc_sig_span <- function(sig_rows, layout) {
  if (is.null(sig_rows) || nrow(sig_rows) == 0) {
    return(list(min = Inf, max = -Inf))
  }
  y_min <- Inf
  y_max <- -Inf
  for (i in seq_len(nrow(sig_rows))) {
    row <- sig_rows[i, ]
    label_text <- resolve_sig_label(row$label, row$p)
    if (!nzchar(label_text)) next
    step_value <- ifelse(is.na(row$step), 0, row$step)
    band_key <- tolower(ifelse(is.na(row$band), "across", row$band))
    base_y <- get_band_value(layout$base, band_key, "across")
    step_size <- get_band_value(layout$step, band_key, "across")
    y0 <- base_y + step_value * step_size
    y_min <- min(y_min, y0 - layout$tick_height)
    y_max <- max(y_max, y0 + layout$label_offset)
  }
  list(min = y_min, max = y_max)
}

adjust_y_limits_for_sig <- function(y_limits, sig_rows) {
  layout <- compute_band_layout(y_limits)
  if (is.null(sig_rows) || nrow(sig_rows) == 0) {
    return(list(y_limits = y_limits, layout = layout))
  }

  for (iter in seq_len(6)) {
    layout <- compute_band_layout(y_limits)
    span <- calc_sig_span(sig_rows, layout)
    needs_top <- is.finite(span$max) && span$max > y_limits[2]
    needs_bottom <- is.finite(span$min) && span$min < y_limits[1]
    if (!needs_top && !needs_bottom) break
    rng <- diff(range(y_limits))
    if (!is.finite(rng) || rng <= 0) rng <- 1
    if (needs_top) {
      grow <- max(span$max - y_limits[2], 1e-6)
      y_limits[2] <- y_limits[2] + grow + rng * 0.02
    }
    if (needs_bottom) {
      grow <- max(y_limits[1] - span$min, 1e-6)
      y_limits[1] <- y_limits[1] - grow - rng * 0.02
    }
  }

  list(y_limits = y_limits, layout = compute_band_layout(y_limits))
}

build_sig_geoms <- function(df_metric, sig_rows, difficulty_colours, y_limits, layout = NULL) {
  if (is.null(sig_rows) || nrow(sig_rows) == 0) {
    return(list(
      segments = tibble::tibble(),
      texts = tibble::tibble()
    ))
  }

  if (is.null(layout)) {
    layout <- compute_band_layout(y_limits)
  }

  segments_list <- list()
  texts_list <- list()

  for (i in seq_len(nrow(sig_rows))) {
    row <- sig_rows[i, ]
    x1 <- get_bar_x(df_metric, row$left_mode, row$left_diff)
    x2 <- get_bar_x(df_metric, row$right_mode, row$right_diff)
    if (is.na(x1) || is.na(x2) || identical(x1, x2)) next

    step_value <- row$step
    if (is.na(step_value)) step_value <- 0

    band_key <- tolower(ifelse(is.na(row$band), "across", row$band))
    base_y <- get_band_value(layout$base, band_key, "across")
    step_size <- get_band_value(layout$step, band_key, "across")
    y0 <- base_y + step_value * step_size

    x_left <- min(x1, x2)
    x_right <- max(x1, x2)

    label_text <- resolve_sig_label(row$label, row$p)
    if (!nzchar(label_text)) next

    col_value <- resolve_sig_colour(row$band, row$left_diff, row$right_diff, difficulty_colours)

    segments_list[[length(segments_list) + 1]] <- tibble::tibble(
      x = c(x_left, x_left, x_right),
      xend = c(x_left, x_right, x_right),
      y = c(y0 - layout$tick_height, y0, y0 - layout$tick_height),
      yend = c(y0, y0, y0),
      col = col_value
    )

    texts_list[[length(texts_list) + 1]] <- tibble::tibble(
      x = (x_left + x_right) / 2,
      y = y0 + layout$label_offset,
      lab = label_text,
      col = col_value
    )
  }

  list(
    segments = if (length(segments_list)) dplyr::bind_rows(segments_list) else tibble::tibble(),
    texts = if (length(texts_list)) dplyr::bind_rows(texts_list) else tibble::tibble()
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
    control = unname(colour_map["Benchmark"]),
    baseline = unname(colour_map["Benchmark"]),
    easy = unname(colour_map["Low"]),
    medium = unname(colour_map["Mid"]),
    hard = unname(colour_map["High"])
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

  sig_rows_metric <- tibble::tibble()
  if (!is.null(sig_specs) && nrow(sig_specs) > 0) {
    sig_rows_metric <- sig_specs %>% dplyr::filter(metric == !!metric_name)
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
    y_range <- y_upper - y_lower
  }
  extra_pad <- max(y_range * 0.35, 0.5)
  y_limits <- c(y_lower - extra_pad, y_upper + extra_pad)

  adjusted <- adjust_y_limits_for_sig(y_limits, sig_rows_metric)
  y_limits <- adjusted$y_limits
  sig_layout <- adjusted$layout

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

  # 显著性标注
  if (nrow(sig_rows_metric) > 0) {
    geoms <- build_sig_geoms(df_metric, sig_rows_metric, difficulty_colour_map, y_limits, layout = sig_layout)
    if (nrow(geoms$segments) > 0) {
      p_bars <- p_bars +
        geom_segment(
          data = geoms$segments,
          aes(x = x, xend = xend, y = y, yend = yend),
          inherit.aes = FALSE,
          linewidth = 0.6,
          linetype = "dashed",
          colour = "#000000"
        )
    }
    if (nrow(geoms$texts) > 0) {
      text_outline_size <- 4.6
      text_fill_size <- 4
      p_bars <- p_bars +
        geom_text(
          data = geoms$texts,
          aes(x = x, y = y, label = lab),
          inherit.aes = FALSE,
          size = text_outline_size,
          fontface = "bold",
          colour = "#000000"
        ) +
        geom_text(
          data = geoms$texts,
          aes(x = x, y = y, label = lab, colour = col),
          inherit.aes = FALSE,
          size = text_fill_size,
          fontface = "bold"
        ) +
        scale_colour_identity(guide = "none")
    }
  }

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

  ggsave(
    file.path(plots_only_dir, paste0(metric_safe, "_bars.png")),
    p_bars,
    width = 10,
    height = 4.5,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
  ggsave(
    file.path(plots_with_table_dir, paste0(metric_safe, "_bars_table.png")),
    combined,
    width = 10,
    height = 6.5,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )

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

sig_input_path <- file.path("Factorial", "PERFORMANCE_FACTORIAL.xlsx")
sig_specs <- load_sig_specs(sig_input_path)
if (nrow(sig_specs) == 0) {
  message("未从 ", sig_input_path, " 解析到显著性比较，图中将不显示显著性标注")
}

output_base <- file.path("descriptive", "mode_difficulty_output")
if (!dir.exists(output_base)) dir.create(output_base, recursive = TRUE)

metrics <- unique(mode_difficulty$metric)
message("检测到 ", length(metrics), " 个 metric：", paste(metrics, collapse = ", "))

for (metric_name in metrics) {
  df_metric <- mode_difficulty %>% filter(metric == !!metric_name)
  make_plot_for_metric(df_metric, metric_name, output_base, sig_specs)
}

message("全部图表已生成，保存于 ", output_base)
