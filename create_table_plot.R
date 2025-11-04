# 检查并安装必要的库
required_packages <- c("ggplot2", "grid", "gridExtra")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  cat("正在安装必要的R包:", paste(new_packages, collapse = ", "), "\n")
  # 尝试在用户库中安装
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!dir.exists(user_lib)) {
    dir.create(user_lib, recursive = TRUE)
  }
  tryCatch({
    install.packages(new_packages, repos = "https://cloud.r-project.org", lib = user_lib)
    .libPaths(c(user_lib, .libPaths()))
  }, error = function(e) {
    cat("安装包时出现错误，尝试继续运行...\n")
    cat("错误信息:", conditionMessage(e), "\n")
  })
}

# 加载必要的库
library(ggplot2)
library(grid)
library(gridExtra)

# 读取数据
data <- read.csv("user_provided_data.csv", stringsAsFactors = FALSE)

# 定义颜色
color_easy <- "#84D6CA"  # 青色
color_hard <- "#FB7F72"  # 珊瑚色

# 根据图像描述，精确提取数据
get_value <- function(condition, difficulty, col_mean, col_std) {
  row <- data[grepl(condition, data$Condition) & data$Difficulty == difficulty, ]
  if (nrow(row) > 0) {
    mean_val <- row[[col_mean]][1]
    std_val <- row[[col_std]][1]
    if (!is.na(mean_val) && length(mean_val) > 0) {
      return(list(mean = mean_val, std = std_val))
    }
  }
  return(list(mean = NA, std = NA))
}

# 根据图像描述映射数据（基于精确数值匹配）
benchmark_v <- get_value("Controller_Easy", "Easy", "VR_avgSpd", "VR_avgSpd_std")

# Easy行的数据
controller_easy <- get_value("Controller_Hard", "Hard", "VR_avgSpd", "VR_avgSpd_std")
gaza_easy <- get_value("Gaze_Hard", "Hard", "VR_avgSpd", "VR_avgSpd_std")
head_easy <- get_value("Head_Hard", "Hard", "VR_avgSpd", "VR_avgSpd_std")
body_easy <- get_value("Body_Hard", "Hard", "VR_avgSpd", "VR_avgSpd_std")
face_easy <- get_value("Face_Hard", "Hard", "VR_avgSpd", "VR_avgSpd_std")

# Hard行的数据
controller_hard <- get_value("Gaze_Easy", "Easy", "VR_avgSpd", "VR_avgSpd_std")
gaza_hard <- get_value("Head_Easy", "Easy", "VR_avgSpd", "VR_avgSpd_std")
head_hard <- get_value("Body_Easy", "Easy", "VR_avgSpd", "VR_avgSpd_std")
body_hard <- get_value("Face_Easy", "Easy", "VR_avgSpd", "VR_avgSpd_std")
face_hard <- get_value("Control_Easy", "Easy", "VR_avgSpd", "VR_avgSpd_std")

# 格式化数值显示
format_cell_value <- function(mean_val, std_val) {
  if (is.na(mean_val) || mean_val == "") {
    return("")
  }
  if (is.na(std_val) || std_val == 0) {
    return(sprintf("%.2f", mean_val))
  }
  return(sprintf("%.2f\n(±%.2f)", mean_val, std_val))
}

# 以下代码使用ggplot2方法创建表格

# 使用ggplot2创建表格
create_ggplot_table <- function() {
  
  # 创建完整的数据框（包括行标签）
  # 注意：BenchmarkV列在Hard行为空，因为它跨越两行
  full_data <- data.frame(
    Row = factor(rep(c("Easy", "Hard"), each = 6), levels = c("Easy", "Hard")),
    Col = factor(rep(c("BenchmarkV", "Controller", "Gaza", "Head", "Body", "Face"), 2),
                levels = c("BenchmarkV", "Controller", "Gaza", "Head", "Body", "Face")),
    Value = c(
      format_cell_value(benchmark_v$mean, benchmark_v$std),  # BenchmarkV Easy
      format_cell_value(controller_easy$mean, controller_easy$std),
      format_cell_value(gaza_easy$mean, gaza_easy$std),
      format_cell_value(head_easy$mean, head_easy$std),
      format_cell_value(body_easy$mean, body_easy$std),
      format_cell_value(face_easy$mean, face_easy$std),
      "",  # BenchmarkV Hard (空白，因为跨越两行)
      format_cell_value(controller_hard$mean, controller_hard$std),
      format_cell_value(gaza_hard$mean, gaza_hard$std),
      format_cell_value(head_hard$mean, head_hard$std),
      format_cell_value(body_hard$mean, body_hard$std),
      format_cell_value(face_hard$mean, face_hard$std)
    ),
    stringsAsFactors = FALSE
  )
  
  full_data$RowFill <- ifelse(full_data$Row == "Easy", color_easy, color_hard)
  
  # 创建ggplot主图
  # 先排除BenchmarkV行（Easy和Hard），稍后单独处理合并单元格
  plot_data <- full_data[full_data$Col != "BenchmarkV", ]
  
  p <- ggplot(plot_data, aes(x = Col, y = Row)) +
    # 数据单元格背景（带行颜色）
    geom_tile(aes(fill = Row), alpha = 0.25, color = "grey70", linewidth = 0.5) +
    # 数据文本
    geom_text(aes(label = Value), 
              size = 3.2, color = "black", 
              vjust = 0.5, hjust = 0.5, lineheight = 1.1) +
    # 设置填充颜色
    scale_fill_manual(values = c("Easy" = color_easy, "Hard" = color_hard)) +
    # 列标题在顶部，确保显示所有列（包括BenchmarkV）
    scale_x_discrete(position = "top", drop = FALSE) +
    # 反转Y轴顺序（Easy在上）
    scale_y_discrete(limits = rev) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(size = 11, face = "bold", color = "black", 
                                 margin = margin(b = 5)),
      axis.text.y = element_blank(),  # 隐藏默认Y轴标签，稍后手动添加
      panel.grid = element_blank(),
      plot.margin = margin(15, 15, 15, 60),  # 左边距增大以容纳行标签
      legend.position = "none",
      axis.ticks = element_line(color = "grey70")
    ) +
    labs(x = NULL, y = NULL)
  
  # 添加列标题背景（浅蓝色）
  p <- p + annotate("rect",
                    xmin = 0.5, xmax = 6.5, ymin = 2.45, ymax = 2.55,
                    fill = "#ADD8E6", color = NA)
  
  # 添加行标签背景（浅灰色）
  p <- p + annotate("rect",
                    xmin = -0.5, xmax = 0.5, ymin = 1.5, ymax = 2.5,
                    fill = "grey90", color = "grey70", linewidth = 0.5)
  p <- p + annotate("rect",
                    xmin = -0.5, xmax = 0.5, ymin = 0.5, ymax = 1.5,
                    fill = "grey90", color = "grey70", linewidth = 0.5)
  
  # 添加行标签颜色方块
  p <- p + annotate("rect",
                    xmin = -0.55, xmax = -0.5, ymin = 1.5, ymax = 2.5,
                    fill = color_easy, color = "grey70", linewidth = 0.5)
  p <- p + annotate("rect",
                    xmin = -0.55, xmax = -0.5, ymin = 0.5, ymax = 1.5,
                    fill = color_hard, color = "grey70", linewidth = 0.5)
  
  # 添加行标签文本
  p <- p + annotate("text",
                    x = -0.25, y = 2, label = "Easy",
                    size = 3.5, color = "black", hjust = 0.5, vjust = 0.5, fontface = "plain")
  p <- p + annotate("text",
                    x = -0.25, y = 1, label = "Hard",
                    size = 3.5, color = "black", hjust = 0.5, vjust = 0.5, fontface = "plain")
  
  # 处理BenchmarkV列的合并单元格
  # BenchmarkV的值跨越Easy和Hard两行，需要单独添加文本和背景
  benchmark_text <- format_cell_value(benchmark_v$mean, benchmark_v$std)
  
  # 添加BenchmarkV列的合并单元格背景（使用Easy行的颜色，半透明）
  p <- p + annotate("rect",
                    xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = 2.5,
                    fill = color_easy, color = "grey70", linewidth = 0.5, alpha = 0.25)
  
  # 在BenchmarkV列中心位置添加跨越两行的文本
  p <- p + annotate("text",
                    x = 1, y = 1.5, label = benchmark_text,
                    size = 3.2, color = "black", hjust = 0.5, vjust = 0.5,
                    lineheight = 1.1)
  
  # 保存图形
  ggsave("table_plot.png", p, width = 12, height = 4.5, dpi = 300, bg = "white")
  
  cat("========================================\n")
  cat("表格图形已成功创建！\n")
  cat("保存路径: table_plot.png\n")
  cat("Easy行颜色: ", color_easy, "\n")
  cat("Hard行颜色: ", color_hard, "\n")
  cat("========================================\n")
  
  return(p)
}

# 执行创建表格
cat("正在根据Template.png样式创建表格图形...\n")
plot_result <- create_ggplot_table()

# 显示图形
print(plot_result)
