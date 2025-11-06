# ================= Packages =================
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ================= Data =================
# BenchmarkR: Easy 61.15 (±8.95), Hard 38.93 (±6.58)
# Controller: Easy 17.77 (±6.34), Hard 11.97 (±6.33)
# Gaza: Easy 31.98 (±8.56), Hard 12.30 (±4.64)
# Head: Easy 38.57 (±8.06), Hard 20.12 (±6.12)
# Body: Easy 36.88 (±7.22), Hard 18.17 (±6.49)
# Face: Easy 35.20 (±8.14), Hard 17.45 (±5.56)

dat <- data.frame(
  Category = c("BenchmarkR","BenchmarkR",
               "Controller","Controller",
               "Gaza","Gaza",
               "Head","Head",
               "Body","Body",
               "Face","Face"),
  slot     = c("L","R",
               "L","R",
               "L","R",
               "L","R",
               "L","R",
               "L","R"),
  series   = c("Easy","Hard",
               "Easy","Hard",
               "Easy","Hard",
               "Easy","Hard",
               "Easy","Hard",
               "Easy","Hard"),
  mean     = c(61.15, 38.93,    # BenchmarkR Easy, Hard
               17.77, 11.97,    # Controller Easy, Hard
               31.98, 12.30,    # Gaza Easy, Hard
               38.57, 20.12,    # Head Easy, Hard
               36.88, 18.17,    # Body Easy, Hard
               35.20, 17.45),   # Face Easy, Hard
  sd       = c(8.95, 6.58,      # BenchmarkR Easy, Hard
               6.34, 6.33,      # Controller Easy, Hard
               8.56, 4.64,      # Gaza Easy, Hard
               8.06, 6.12,      # Head Easy, Hard
               7.22, 6.49,      # Body Easy, Hard
               8.14, 5.56),     # Face Easy, Hard
  stringsAsFactors = FALSE
)

# 固定横轴顺序
dat$Category <- factor(dat$Category,
  levels = c("BenchmarkR","Controller","Gaza","Head","Body","Face"))
dat$slot   <- factor(dat$slot, levels = c("L","R"))
dat$series <- factor(dat$series, levels = c("Easy","Hard"))

# ================= 手动定位（确保居中+相邻柱体有间距） =================
# 以每个类目的中心为整数(1,2,3,...)，在左右各偏移 delta；两柱宽度为 bar_w
group_w <- 0.8               # 每个类目总体占宽（<1 避免挤到相邻类目）
bar_w   <- group_w / 2       # 0.4  柱体宽度保持不变
delta   <- group_w / 4       # 0.2  基础偏移
gap     <- 0.03              # 相邻柱体间距（几个像素）

# 对于所有类别，L和R之间增加间距
offset_map <- c(L = -delta - gap/2, R = +delta + gap/2)

x_base <- as.numeric(dat$Category)
dat$x  <- x_base + unname(offset_map[as.character(dat$slot)])

# ================= 配色方案 =================
col_easy  <- "#8CEAB4"      # Easy
col_hard  <- "#F1917E"      # Hard

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

# y 轴：根据数据范围确定，最大值约 61.15，设置为 0-70
y_top    <- 70
y_breaks <- seq(0, 70, by = 10)

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
  scale_fill_manual(values = c(Easy = col_easy, Hard = col_hard)) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, y_top), expand = 0) +
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
  scale_fill_manual(values = c(Easy = col_easy, Hard = col_hard)) +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, y_top * 1.02), expand = 0) +
  theme_base

# 保存文件（确保目录存在）
if (!dir.exists("no_labels")) dir.create("no_labels", recursive = TRUE)
if (!dir.exists("with_labels")) dir.create("with_labels", recursive = TRUE)
ggsave("no_labels/RW_no_labels.png", p1, width = 8, height = 3.57, dpi = 300, bg = "white")
ggsave("with_labels/RW_with_labels.png", p2, width = 8, height = 3.92, dpi = 300, bg = "white")

