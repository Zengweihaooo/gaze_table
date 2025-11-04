# ================= Packages =================
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ================= Data =================
bench_mean <- 32.19; bench_sd <- 1.37

raw <- data.frame(
  Category  = c("Controller","Gaza","Head","Body","Face"),
  Easy_mean = c(27.41, 28.10, 30.72, 30.24, 27.60),
  Easy_sd   = c( 3.15,  3.11,  4.21,  3.09,  6.18),
  Hard_mean = c(25.72, 23.97, 26.65, 25.33, 22.66),
  Hard_sd   = c( 3.67,  3.10,  3.49,  4.11,  5.90)
)

# --- Build 3 slots per category: L(Easy), C(BenchmarkV), R(Hard) ---
mk_rows <- function(cat, em, es, hm, hs){
  rbind(
    data.frame(Category=cat, slot="L", series="Easy",   mean=em, sd=es, spacer=FALSE),
    data.frame(Category=cat, slot="C", series="Spacer", mean=0,  sd=0,  spacer=TRUE ),
    data.frame(Category=cat, slot="R", series="Hard",   mean=hm, sd=hs, spacer=FALSE)
  )
}
dat <- do.call(rbind, Map(mk_rows, raw$Category, raw$Easy_mean, raw$Easy_sd,
                                       raw$Hard_mean, raw$Hard_sd))

# BenchmarkV：中间真实值，左右透明占位，确保等宽且居中
bench <- rbind(
  data.frame(Category="BenchmarkV", slot="L", series="Spacer",     mean=0, sd=0, spacer=TRUE),
  data.frame(Category="BenchmarkV", slot="C", series="BenchmarkV", mean=bench_mean, sd=bench_sd, spacer=FALSE),
  data.frame(Category="BenchmarkV", slot="R", series="Spacer",     mean=0, sd=0, spacer=TRUE)
)
dat <- rbind(bench, dat)

# Orders
dat$Category <- factor(dat$Category,
  levels=c("BenchmarkV","Controller","Gaza","Head","Body","Face"))
dat$slot   <- factor(dat$slot,   levels=c("L","C","R"))
dat$series <- factor(dat$series, levels=c("Easy","BenchmarkV","Hard","Spacer"))

# ================= Style =================
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

# 关键：同一 position，padding=0；宽度相同=>组内无缝贴合，误差棒与柱完全同位
pd <- position_dodge2(width = 1, preserve = "single", padding = 0)

# Y 轴：主刻度=5，次刻度无；上限对齐到5的倍数
y_top    <- ceiling(max(dat$mean + dat$sd) / 5) * 5
y_breaks <- seq(0, y_top, by = 5)

# ================= Plot: no labels =================
p1 <- ggplot(dat, aes(x = Category, y = mean, group = slot, fill = series)) +
  geom_col(position = pd, width = 1, colour = NA,
           aes(alpha = ifelse(series == "Spacer", 0, 1))) +
  geom_errorbar(
    data = subset(dat, series != "Spacer"),
    aes(ymin = mean - sd, ymax = mean + sd, group = slot),
    position = pd, width = 0.26, color = err_col, linewidth = 0.6
  ) +
  scale_fill_manual(values = c(Easy=col_easy, BenchmarkV=col_bench, Hard=col_hard, Spacer="#000000")) +
  scale_alpha_continuous(range = c(0,1), guide = "none") +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, y_top), expand = 0) +
  theme_base

print(p1)

# ================= Plot: with value labels =================
lab <- subset(dat, series != "Spacer")
lab$label <- sprintf("%.2f (±%.2f)", lab$mean, lab$sd)

p2 <- ggplot(dat, aes(x = Category, y = mean, group = slot, fill = series)) +
  geom_col(position = pd, width = 1, colour = NA,
           aes(alpha = ifelse(series == "Spacer", 0, 1))) +
  geom_errorbar(
    data = lab,
    aes(ymin = mean - sd, ymax = mean + sd, group = slot),
    position = pd, width = 0.26, color = err_col, linewidth = 0.6
  ) +
  geom_text(
    data = lab,
    aes(label = label, group = slot),
    position = pd, vjust = -0.5, size = 3.4
  ) +
  scale_fill_manual(values = c(Easy=col_easy, BenchmarkV=col_bench, Hard=col_hard, Spacer="#000000")) +
  scale_alpha_continuous(range = c(0,1), guide = "none") +
  scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, y_top * 1.02), expand = 0) +
  theme_base

print(p2)

# ================= (Optional) exact-pixel export =================
# Exact canvas size (e.g., 1531x800 px). If ragg exists, it gives sharper text.
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
ggsave("bars_centered_nogap.png", p1, width = 8, height = 4.2, dpi = 300, bg = "#EFEFEF")
ggsave("bars_centered_nogap_labels.png", p2, width = 8, height = 4.6, dpi = 300, bg = "#EFEFEF")