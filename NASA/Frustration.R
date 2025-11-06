# ================= Packages =================
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ================= Data =================
# BenchmarkV: 23.89 (±22.18)
# BenchmarkR: Low 5.56 (±10.11), High 15.56 (±19.04)
# Controller: Low 34.44 (±25.50), High 40.00 (±22.99)
# Gaze: Low 28.89 (±20.03), High 42.22 (±27.24)
# Head: Low 9.44 (±11.32), High 26.11 (±17.33)
# Body: Low 15.56 (±16.91), High 35.56 (±23.46)
# Face: Low 21.11 (±22.29), High 38.33 (±22.38)

bench_mean <- 23.89
bench_sd <- 22.18

dat <- data.frame(
  Category = c("BenchmarkV",
               "BenchmarkR","BenchmarkR",
               "Controller","Controller",
               "Gaze","Gaze",
               "Head","Head",
               "Body","Body",
               "Face","Face"),
  slot     = c("C",
               "L","R",
               "L","R",
               "L","R",
               "L","R",
               "L","R",
               "L","R"),
  series   = c("BenchmarkV",
               "Low","High",
               "Low","High",
               "Low","High",
               "Low","High",
               "Low","High",
               "Low","High"),
  mean     = c(bench_mean,
               5.56, 15.56,      # BenchmarkR Low, High
               34.44, 40.00,     # Controller Low, High
               28.89, 42.22,     # Gaze Low, High
               9.44, 26.11,      # Head Low, High
               15.56, 35.56,     # Body Low, High
               21.11, 38.33),    # Face Low, High
  sd       = c(bench_sd,
               10.11, 19.04,     # BenchmarkR Low, High
               25.50, 22.99,     # Controller Low, High
               20.03, 27.24,     # Gaze Low, High
               11.32, 17.33,     # Head Low, High
               16.91, 23.46,     # Body Low, High
               22.29, 22.38),    # Face Low, High
  stringsAsFactors = FALSE
)

# 固定横轴顺序
dat$Category <- factor(dat$Category,
  levels = c("BenchmarkV","BenchmarkR","Controller","Gaze","Head","Body","Face"))
dat$slot   <- factor(dat$slot, levels = c("L","C","R"))
dat$series <- factor(dat$series, levels = c("Low","BenchmarkV","High"))

# ================= 手动定位（确保居中+相邻柱体有间距） =================
# 以每个类目的中心为整数(1,2,3,...)，在左右各偏移 delta；两柱宽度为 bar_w
group_w <- 0.8               # 每个类目总体占宽（<1 避免挤到相邻类目）
bar_w   <- group_w / 2       # 0.4  柱体宽度保持不变
delta   <- group_w / 4       # 0.2  基础偏移
gap     <- 0.03              # 相邻柱体间距（几个像素）

# 对于非BenchmarkV类别，L和R之间增加间距
offset_map <- c(L = -delta - gap/2, C = 0, R = +delta + gap/2)

x_base <- as.numeric(dat$Category)
dat$x  <- x_base + unname(offset_map[as.character(dat$slot)])

# ================= 配色方案 =================
col_low   <- "#8CEAB4"      # Low
col_high  <- "#F1917E"      # High
col_bench <- "#B9BCDB"      # BenchmarkV

err_col   <- "#484D5F"

theme_base <- theme_minimal(base_size = 13) +
  theme(
    legend.position    = "none",
    panel.background   = element_rect(fill = "#FFFFFF", color = NA),  # 白底
    plot.background    = element_rect(fill = "#FFFFFF", color = NA),  # 白底
    panel.grid.major   = element_blank(),  # 去除所有网格线
    panel.grid.minor   = element_blank(),  # 去除所有网格线
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x       = element_blank(),  # 去除x轴刻度线（底部灰色竖线）
    axis.ticks.y       = element_blank(),  # 去除y轴刻度线（左侧竖着的刻度线）
    axis.line.x        = element_blank(),  # 去除x轴线（底部灰色竖线）
    axis.line.y        = element_blank(),  # 去除y轴线（左侧竖线）
    axis.text.x        = element_text(size = 12, face = "bold", 
                                       margin = margin(t = 10, b = 10)),  # 底部文字距离坐标轴更远
    axis.text.y        = element_text(size = 11),
    axis.title         = element_blank()
  )

# y 轴：从0开始，0-60，组间距10
y_top    <- 60
y_bottom <- 0
y_breaks <- seq(0, 60, by = 10)

# x 轴：用数值坐标，刻度放在每个类目中心
x_breaks <- seq_along(levels(dat$Category))
x_labels <- levels(dat$Category)

# ================= 95% 置信区间计算 =================
n <- 30  # 样本量
df <- n - 1  # 自由度 = 29
t_critical <- qt(0.975, df)  # t分布临界值（95% CI，双尾）
# 计算95% CI的误差范围
dat$ci_margin <- t_critical * (dat$sd / sqrt(n))

# ================= 图1：不带标注 =================
p1 <- ggplot(dat, aes(x = x, y = mean, fill = series)) +
  # 手动添加水平坐标线
  geom_hline(yintercept = y_breaks, color = "#E5E5E5", linewidth = 0.5) +
  geom_col(width = bar_w, position = "identity", colour = NA) +
  geom_errorbar(aes(ymin = mean - ci_margin, ymax = mean + ci_margin),
                width = bar_w * 0.35, color = err_col, linewidth = 0.6,
                position = "identity") +
  scale_fill_manual(values = c(Low = col_low, BenchmarkV = col_bench, High = col_high)) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  coord_cartesian(ylim = c(y_bottom, y_top), expand = 0) +
  theme_base

# ================= 图2：带"均值 (95% CI)"标注 =================
lab_df <- transform(dat, label = sprintf("%.2f [%.2f, %.2f]", mean, 
                                          mean - ci_margin, mean + ci_margin))

p2 <- ggplot(dat, aes(x = x, y = mean, fill = series)) +
  # 手动添加水平坐标线
  geom_hline(yintercept = y_breaks, color = "#E5E5E5", linewidth = 0.5) +
  geom_col(width = bar_w, position = "identity", colour = NA) +
  geom_errorbar(aes(ymin = mean - ci_margin, ymax = mean + ci_margin),
                width = bar_w * 0.35, color = err_col, linewidth = 0.6,
                position = "identity") +
  geom_text(data = lab_df, aes(label = label),
            vjust = -0.5, size = 3.4, position = "identity") +
  scale_fill_manual(values = c(Low = col_low, BenchmarkV = col_bench, High = col_high)) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  coord_cartesian(ylim = c(y_bottom, y_top * 1.02), expand = 0) +
  theme_base

# 保存文件（确保目录存在）
if (!dir.exists("no_labels")) dir.create("no_labels", recursive = TRUE)
if (!dir.exists("with_labels")) dir.create("with_labels", recursive = TRUE)
ggsave("no_labels/Frustration_no_labels.png", p1, width = 8, height = 3.57, dpi = 300, bg = "white")
ggsave("with_labels/Frustration_with_labels.png", p2, width = 8, height = 3.91, dpi = 300, bg = "white")

