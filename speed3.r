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

# ================= 配色方案选择 =================
# 方案0：你指定的配色
palette_0 <- list(
  Easy = "#F1D77E",      # 淡黄色
  Hard = "#9DC3E7",      # 淡蓝色
  BenchmarkV = "#63E398" # 淡绿色
)

# 方案1：马卡龙粉色系（甜美粉嫩）
palette_1 <- list(
  Easy = "#FFB3D9",      # 粉红色
  Hard = "#B3E5FC",      # 天蓝色
  BenchmarkV = "#C8E6C9" # 粉绿色
)

# 方案2：马卡龙紫色系（优雅淡雅）
palette_2 <- list(
  Easy = "#E1BEE7",      # 淡紫色
  Hard = "#BBDEFB",      # 淡蓝色
  BenchmarkV = "#DCEDC8" # 淡黄绿色
)

# 方案3：马卡龙橙色系（温暖清新）
palette_3 <- list(
  Easy = "#FFCCBC",      # 淡橙色
  Hard = "#B3E5FC",      # 淡蓝色
  BenchmarkV = "#FFF9C4" # 淡黄色
)

# 方案4：海蓝/青绿+深海蓝+珊瑚橙
palette_4 <- list(
  Easy = "#2AA9A1",      # 海蓝/青绿色
  Hard = "#F28C6B",      # 珊瑚橙
  BenchmarkV = "#1D4E89" # 深海蓝
)

# 方案5：暗灰蓝+石灰绿+柔粉红
palette_5 <- list(
  Easy = "#455A80",      # 暗灰蓝
  Hard = "#E8B7C7",      # 柔粉红
  BenchmarkV = "#8FCB9B" # 石灰绿
)

# 方案6：金黄色+暗紫+极淡灰
palette_6 <- list(
  Easy = "#D9A441",      # 金黄色
  Hard = "#E8E8E8",      # 极淡灰
  BenchmarkV = "#3C2F51" # 暗紫
)

# 方案7：赭石/陶土橙+鼠尾草绿+清浅天蓝
palette_7 <- list(
  Easy = "#D17C6B",      # 赭石/陶土橙
  Hard = "#6EB8E6",      # 清浅天蓝
  BenchmarkV = "#9EBF9D" # 鼠尾草绿
)

# 方案8：赛博青+深石墨灰+桃紫/品红
palette_8 <- list(
  Easy = "#00A9A5",      # 赛博青
  Hard = "#D46BBB",      # 桃紫/品红
  BenchmarkV = "#3D3D3D" # 深石墨灰
)

# 配色方案列表
palettes <- list(palette_0, palette_1, palette_2, palette_3, 
                 palette_4, palette_5, palette_6, palette_7, palette_8)
palette_names <- c("custom", "pink", "purple", "orange",
                    "teal_blue", "sage_green", "gold_purple", "terracotta", "cyber_gray")

err_col   <- "#484D5F"

theme_base <- theme_minimal(base_size = 13) +
  theme(
    legend.position    = "none",
    panel.background   = element_rect(fill = "#FFFFFF", color = NA),  # 白底
    plot.background    = element_rect(fill = "#FFFFFF", color = NA),  # 白底
    panel.grid.major   = element_line(color = "#E5E5E5"),  # 浅灰色网格线
    panel.grid.minor   = element_line(color = "#E5E5E5"),  # 浅灰色网格线
    panel.grid.major.x = element_blank(),  # 竖线不用
    axis.text.x        = element_text(size = 12, face = "bold", 
                                       margin = margin(t = 10, b = 10)),  # 底部文字距离坐标轴更远
    axis.text.y        = element_text(size = 11),
    axis.title         = element_blank()
  )

# y 轴主刻度=5，无次刻度
y_top    <- ceiling(max(dat$mean + dat$sd) / 5) * 5
y_breaks <- seq(0, y_top, by = 5)

# x 轴：用数值坐标，刻度放在每个类目中心
x_breaks <- seq_along(levels(dat$Category))
x_labels <- levels(dat$Category)

# ================= 循环生成所有配色方案 =================
for (i in 1:length(palettes)) {
  current_pal <- palettes[[i]]
  pal_name <- palette_names[i]
  
  col_easy  <- current_pal$Easy
  col_hard  <- current_pal$Hard
  col_bench <- current_pal$BenchmarkV
  
  # 图1：不带标注
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
  
  # 图2：带"均值(±SD)"标注
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
  
  # 保存文件
  ggsave(sprintf("bars_%s_no_labels.png", pal_name), p1, width = 8, height = 4.2, dpi = 300, bg = "white")
  ggsave(sprintf("bars_%s_with_labels.png", pal_name), p2, width = 8, height = 4.6, dpi = 300, bg = "white")
  
  cat(sprintf("配色方案 %d (%s) 已生成\n", i-1, pal_name))
}
