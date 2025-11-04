# -------- Packages --------
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# -------- Data (same numbers) --------
bench_mean <- 32.19; bench_sd <- 1.37

raw <- data.frame(
  Category  = c("Controller","Gaza","Head","Body","Face"),
  Easy_mean = c(27.41, 28.10, 30.72, 30.24, 27.60),
  Easy_sd   = c( 3.15,  3.11,  4.21,  3.09,  6.18),
  Hard_mean = c(25.72, 23.97, 26.65, 25.33, 22.66),
  Hard_sd   = c( 3.67,  3.10,  3.49,  4.11,  5.90)
)

# --- Build 3 slots per category: L(Easy), C(BenchmarkV), R(Hard) ---
# For non-BenchmarkV categories, C 是不可见占位；BenchmarkV 只有 C 槽位
mk_rows <- function(cat, em, es, hm, hs){
  rbind(
    data.frame(Category=cat, slot="L", series="Easy",       mean=em, sd=es, spacer=FALSE),
    data.frame(Category=cat, slot="C", series="Spacer",     mean=0,  sd=0,  spacer=TRUE ),
    data.frame(Category=cat, slot="R", series="Hard",       mean=hm, sd=hs, spacer=FALSE)
  )
}
dat <- do.call(rbind,
               Map(mk_rows, raw$Category, raw$Easy_mean, raw$Easy_sd,
                              raw$Hard_mean, raw$Hard_sd))

# BenchmarkV 行：中间真实，左右为不可见占位
bench <- rbind(
  data.frame(Category="BenchmarkV", slot="L", series="Spacer",     mean=0,          sd=0,          spacer=TRUE),
  data.frame(Category="BenchmarkV", slot="C", series="BenchmarkV", mean=bench_mean, sd=bench_sd,   spacer=FALSE),
  data.frame(Category="BenchmarkV", slot="R", series="Spacer",     mean=0,          sd=0,          spacer=TRUE)
)
dat <- rbind(bench, dat)

# Factor orders: x顺序固定，槽位固定：左(Easy)-中(Bench)-右(Hard)
dat$Category <- factor(dat$Category, levels=c("BenchmarkV","Controller","Gaza","Head","Body","Face"))
dat$slot     <- factor(dat$slot,     levels=c("L","C","R"))
dat$series   <- factor(dat$series,   levels=c("Easy","BenchmarkV","Hard","Spacer"))

# -------- Style --------
col_bench <- "#B9BCDB"; col_easy <- "#84D6CA"; col_hard <- "#FB7F72"; err_col <- "#484D5F"
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

# 单一的 position 对象，供柱/误差棒/文字共用
pd <- position_dodge(width = 0.8)

# y 轴主刻度=5，无次刻度，上限对齐到5的倍数
y_top <- ceiling(max(dat$mean + dat$sd) / 5) * 5
y_breaks <- seq(0, y_top, by = 5)

# -------- Plot 1: no labels --------
p1 <- ggplot(dat, aes(x = Category, y = mean, group = slot, fill = series)) +
  geom_col(width = 0.7, position = pd, colour = NA,
           aes(alpha = ifelse(series=="Spacer", 0, 1))) +
  geom_errorbar(
    data = subset(dat, series != "Spacer"),
    aes(ymin = mean - sd, ymax = mean + sd, group = slot),
    position = pd, width = 0.3, color = err_col, linewidth = 0.6
  ) +
  scale_fill_manual(values = c(Easy=col_easy, BenchmarkV=col_bench, Hard=col_hard, Spacer="#000000")) +
  scale_alpha_continuous(range = c(0,1), guide = "none") +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, y_top), expand = 0) +
  base_theme
print(p1)

# -------- Plot 2: with value labels --------
lab_df <- subset(dat, series != "Spacer")
lab_df$label <- sprintf("%.2f (±%.2f)", lab_df$mean, lab_df$sd)
p2 <- ggplot(dat, aes(x = Category, y = mean, group = slot, fill = series)) +
  geom_col(width = 0.7, position = pd, colour = NA,
           aes(alpha = ifelse(series=="Spacer", 0, 1))) +
  geom_errorbar(
    data = lab_df,
    aes(ymin = mean - sd, ymax = mean + sd, group = slot),
    position = pd, width = 0.3, color = err_col, linewidth = 0.6
  ) +
  geom_text(
    data = lab_df,
    aes(label = label, group = slot),
    position = pd, vjust = -0.5, size = 3.4
  ) +
  scale_fill_manual(values = c(Easy=col_easy, BenchmarkV=col_bench, Hard=col_hard, Spacer="#000000")) +
  scale_alpha_continuous(range = c(0,1), guide = "none") +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, y_top*1.02), expand = 0) +
  base_theme
print(p2)

# -------- Save PNG files --------
ggsave("bars_centered_no_labels.png", p1, width = 8, height = 4.2, dpi = 300, bg = "#EFEFEF")
ggsave("bars_centered_with_labels.png", p2, width = 8, height = 4.6, dpi = 300, bg = "#EFEFEF")
