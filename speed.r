# -------- Packages --------
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# -------- Data --------
bench_mean <- 32.19
bench_sd   <- 1.37

dat <- data.frame(
  Category = c(
    "BenchmarkV",
    rep("Controller", 2), rep("Gaza", 2),
    rep("Head", 2),       rep("Body", 2),
    rep("Face", 2)
  ),
  Condition = c(
    "BenchmarkV",         # 单柱
    "Easy","Hard", "Easy","Hard",
    "Easy","Hard", "Easy","Hard",
    "Easy","Hard"
  ),
  mean = c(
    bench_mean,
    27.41, 25.72, 28.10, 23.97,
    30.72, 26.65, 30.24, 25.33,
    27.60, 22.66
  ),
  sd = c(
    bench_sd,
    3.15, 3.67, 3.11, 3.10,
    4.21, 3.49, 3.09, 4.11,
    6.18, 5.90
  ),
  stringsAsFactors = FALSE
)

# 横轴顺序 & 分组内次序（左 Easy 右 Hard；BenchmarkV 保持单柱）
dat$Category  <- factor(dat$Category,
  levels = c("BenchmarkV","Controller","Gaza","Head","Body","Face"))
dat$Condition <- factor(dat$Condition, levels = c("Easy","Hard","BenchmarkV"))

# 颜色
col_bench <- "#B9BCDB"
col_easy  <- "#84D6CA"
col_hard  <- "#FB7F72"
err_col   <- "#484D5F"

fill_vals <- c("Easy" = col_easy, "Hard" = col_hard, "BenchmarkV" = col_bench)

# 公共主题（灰背景+白网格）
base_theme <- theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.background  = element_rect(fill = "#EFEFEF", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11)
  )

# ——关键：让单柱组等宽&误差棒居中——
pd <- position_dodge2(width = 0.7, preserve = "single", padding = 0)

# y 轴更紧凑 + 主刻度=5、无次刻度
y_raw <- max(dat$mean + dat$sd, na.rm = TRUE) * 1.03
y_top <- ceiling(y_raw / 5) * 5
y_breaks <- seq(0, y_top, by = 5)

# -------- 图1：不带标注 --------
p1 <- ggplot(dat, aes(x = Category, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = pd, width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = pd, width = 0.28, color = err_col, linewidth = 0.6) +
  scale_fill_manual(values = fill_vals) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, y_top), expand = 0) +
  base_theme

print(p1)
ggsave("bars_compact_no_labels.png", p1, width = 8, height = 4.2, dpi = 300)

# -------- 图2：带“均值(±SD)”标注 --------
lab_df <- transform(dat, label = sprintf("%.2f (±%.2f)", mean, sd))

p2 <- ggplot(lab_df, aes(x = Category, y = mean, fill = Condition)) +
  geom_bar(stat = "identity", position = pd, width = 0.7) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                position = pd, width = 0.28, color = err_col, linewidth = 0.6) +
  geom_text(aes(label = label),
            position = pd, vjust = -0.5, size = 3.4) +
  scale_fill_manual(values = fill_vals) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, y_top * 1.02), expand = 0) +
  base_theme

print(p2)
ggsave("bars_compact_with_labels.png", p2, width = 8, height = 4.6, dpi = 300)
