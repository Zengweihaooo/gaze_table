# ================= Packages =================
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ================= Data =================
bench_mean <- 32.19; bench_sd <- 1.37

dat <- data.frame(
  Category = c("BenchmarkV",
               "Controller","Controller",
               "Gaza","Gaza",
               "Head","Head",
               "Body","Body",
               "Face","Face"),
  slot     = c("C",
               "L","R",
               "L","R",
               "L","R",
               "L","R",
               "L","R"),
  series   = c("BenchmarkV",
               "Easy","Hard",
               "Easy","Hard",
               "Easy","Hard",
               "Easy","Hard",
               "Easy","Hard"),
  mean     = c(bench_mean,
               27.41, 25.72,
               28.10, 23.97,
               30.72, 26.65,
               30.24, 25.33,
               27.60, 22.66),
  sd       = c(bench_sd,
               3.15, 3.67,
               3.11, 3.10,
               4.21, 3.49,
               3.09, 4.11,
               6.18, 5.90),
  stringsAsFactors = FALSE
)

# 固定横轴顺序
dat$Category <- factor(dat$Category,
  levels = c("BenchmarkV","Controller","Gaza","Head","Body","Face"))
dat$slot   <- factor(dat$slot, levels = c("L","C","R"))
dat$series <- factor(dat$series, levels = c("Easy","BenchmarkV","Hard"))

# ================= 手动定位（确保居中+无缝贴合） =================
# 以每个类目的中心为整数(1,2,3,...)，在左右各偏移 delta；两柱宽度为 bar_w，
# 令 bar_w = 2*delta -> L 与 R 正好在中间“无缝贴合”
group_w <- 0.8               # 每个类目总体占宽（<1 避免挤到相邻类目）
delta   <- group_w / 4       # 0.2
bar_w   <- group_w / 2       # 0.4  => 与上式使两柱贴合

offset_map <- c(L = -delta, C = 0, R = +delta)

x_base <- as.numeric(dat$Category)
dat$x  <- x_base + unname(offset_map[as.character(dat$slot)])

# ================= 样式 =================
col_easy  <- "#84D6CA"
col_hard  <- "#FB7F72"
col_bench <- "#B9BCDB"
err_col   <- "#484D5F"

theme_base <- theme_minimal(base_size = 13) +
  theme(
    legend.position    = "none",
    panel.background   = element_rect(fill = "#EFEFEF", color = NA),
    plot.background    = element_rect(fill = "#EFEFEF", color = NA),
    panel.grid.major   = element_line(color = "white"),
    panel.grid.minor   = element_line(color = "white"),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 12, face = "bold"),
    axis.text.y        = element_text(size = 11),
    axis.title         = element_blank()
  )

# y 轴主刻度=5，无次刻度
y_top    <- ceiling(max(dat$mean + dat$sd) / 5) * 5
y_breaks <- seq(0, y_top, by = 5)

# x 轴：用数值坐标，刻度放在每个类目中心
x_breaks <- seq_along(levels(dat$Category))
x_labels <- levels(dat$Category)

# ================= 图1：不带标注 =================
p1 <- ggplot(dat, aes(x = x, y = mean, fill = series)) +
  geom_col(width = bar_w, position = "identity", colour = NA) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = bar_w * 0.35, color = err_col, linewidth = 0.6,
                position = "identity") +
  scale_fill_manual(values = c(Easy = col_easy, BenchmarkV = col_bench, Hard = col_hard)) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, y_top), expand = 0) +
  theme_base

print(p1)

# ================= 图2：带“均值(±SD)”标注 =================
lab_df <- transform(dat, label = sprintf("%.2f (±%.2f)", mean, sd))

p2 <- ggplot(dat, aes(x = x, y = mean, fill = series)) +
  geom_col(width = bar_w, position = "identity", colour = NA) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = bar_w * 0.35, color = err_col, linewidth = 0.6,
                position = "identity") +
  geom_text(data = lab_df, aes(label = label),
            vjust = -0.5, size = 3.4, position = "identity") +
  scale_fill_manual(values = c(Easy = col_easy, BenchmarkV = col_bench, Hard = col_hard)) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, y_top * 1.02), expand = 0) +
  theme_base

print(p2)

# ================= （可选）固定像素导出 =================
save_exact_px <- function(plot, file, width_px, height_px, bg = "#EFEFEF") {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename = file, width = width_px, height = height_px,
                  units = "px", background = bg, res = 72)
    print(plot); dev.off()
  } else {
    png(filename = file, width = width_px, height = height_px, units = "px",
        bg = bg, type = "cairo")
    print(plot); dev.off()
  }
}
# Save files
ggsave("bars_aligned_no_labels.png", p1, width = 8, height = 4.2, dpi = 300, bg = "#EFEFEF")
ggsave("bars_aligned_with_labels.png", p2, width = 8, height = 4.6, dpi = 300, bg = "#EFEFEF")
