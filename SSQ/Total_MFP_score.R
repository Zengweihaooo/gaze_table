# ================= Packages =================
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ================= Data =================
# BenchmarkV: 1.10 (±2.06)
# BenchmarkR: Low 2.55 (±3.77), High 1.60 (±3.32)
# Controller: Low 1.90 (±4.52), High 1.47 (±4.10)
# Gaze: Low 1.57 (±3.28), High 1.03 (±4.09)
# Head: Low 2.60 (±5.10), High 2.57 (±4.45)
# Body: Low 2.70 (±4.92), High 2.50 (±5.12)
# Face: Low 1.57 (±2.43), High 1.83 (±3.31)

bench_mean <- 1.10
bench_sd <- 2.06

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
               2.55, 1.60,       # BenchmarkR Low, High
               1.90, 1.47,       # Controller Low, High
               1.57, 1.03,       # Gaze Low, High
               2.60, 2.57,       # Head Low, High
               2.70, 2.50,       # Body Low, High
               1.57, 1.83),      # Face Low, High
  sd       = c(bench_sd,
               3.77, 3.32,       # BenchmarkR Low, High
               4.52, 4.10,       # Controller Low, High
               3.28, 4.09,       # Gaze Low, High
               5.10, 4.45,       # Head Low, High
               4.92, 5.12,       # Body Low, High
               2.43, 3.31),      # Face Low, High
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

# ================= 95% 置信区间计算 =================
n <- 30  # 样本量
df <- n - 1  # 自由度 = 29
t_critical <- qt(0.975, df)  # t分布临界值（95% CI，双尾）
# 计算95% CI的误差范围
dat$ci_margin <- t_critical * (dat$sd / sqrt(n))

# y 轴：-1到5，组间距1
y_top    <- 5
y_bottom <- -1
y_breaks <- seq(-1, 5, by = 1)

# x 轴：用数值坐标，刻度放在每个类目中心
x_breaks <- seq_along(levels(dat$Category))
x_labels <- levels(dat$Category)

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
            vjust = ifelse(lab_df$mean >= 0, -0.5, 1.5), size = 3.4, position = "identity") +
  scale_fill_manual(values = c(Low = col_low, BenchmarkV = col_bench, High = col_high)) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  coord_cartesian(ylim = c(y_bottom, y_top * 1.02), expand = 0) +
  theme_base

# 保存文件（确保目录存在）
if (!dir.exists("no_labels")) dir.create("no_labels", recursive = TRUE)
if (!dir.exists("with_labels")) dir.create("with_labels", recursive = TRUE)
ggsave("no_labels/Total_MFP_score_no_labels.png", p1, width = 8, height = 3.57, dpi = 300, bg = "white")
ggsave("with_labels/Total_MFP_score_with_labels.png", p2, width = 8, height = 3.91, dpi = 300, bg = "white")

