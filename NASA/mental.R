# ================= Packages =================
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ================= Data =================
# BenchmarkV: 31.67 (±21.60)
# BenchmarkR: Low 8.33 (±14.35), High 33.33 (±23.97)
# Controller: Low 47.22 (±24.79), High 63.89 (±23.60)
# Gaze: Low 50.00 (±28.03), High 69.44 (±22.78)
# Head: Low 35.00 (±19.25), High 53.33 (±27.82)
# Body: Low 35.56 (±23.05), High 57.78 (±24.66)
# Face: Low 37.22 (±23.03), High 57.22 (±24.24)

bench_mean <- 31.67
bench_sd <- 21.60

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
               8.33, 33.33,      # BenchmarkR Low, High
               47.22, 63.89,     # Controller Low, High
               50.00, 69.44,     # Gaze Low, High
               35.00, 53.33,     # Head Low, High
               35.56, 57.78,     # Body Low, High
               37.22, 57.22),    # Face Low, High
  sd       = c(bench_sd,
               14.35, 23.97,     # BenchmarkR Low, High
               24.79, 23.60,     # Controller Low, High
               28.03, 22.78,     # Gaze Low, High
               19.25, 27.82,     # Head Low, High
               23.05, 24.66,     # Body Low, High
               23.03, 24.24),    # Face Low, High
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

# y 轴：从0开始，0-80，组间距10
y_top    <- 80
y_bottom <- 0
y_breaks <- seq(0, 80, by = 10)

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
ggsave("no_labels/mental_no_labels.png", p1, width = 8, height = 3.57, dpi = 300, bg = "white")
ggsave("with_labels/mental_with_labels.png", p2, width = 8, height = 3.91, dpi = 300, bg = "white")
