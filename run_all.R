#!/usr/bin/env Rscript

# 脚本：运行所有R文件生成图表
# 使用方法：在终端运行 Rscript run_all.R

# 获取当前脚本所在目录
script_dir <- dirname(normalizePath(commandArgs(trailingOnly = FALSE)[4]))
if (length(script_dir) == 0) {
  script_dir <- getwd()
}
setwd(script_dir)

# 定义要运行的R文件列表
r_files <- c(
  "Gaze/HR.R",
  "Gaze/DoG.R",
  "Gaze/NoG.R",
  "Operation/DoA.R",
  "Operation/NoA.R",
  "RW performance/RW.R",
  "performance/performance_sal.R",
  "performance/performance_NoC.R",
  "performance/performace_toc.R",
  "performance/Performace_D.r",
  "performance/performance_speed.r",
  "performance_speed.r",
  "speed1.r",
  "speed2.r",
  "NASA/mental.R",
  "NASA/physical.R",
  "NASA/effort.R",
  "NASA/time.R",
  "NASA/perform.R",
  "NASA/Frustration.R",
  "NASA/WR.R",
  "SSQ/Nausea.R",
  "SSQ/Oculomotor.R",
  "SSQ/Disorientation.R",
  "SSQ/Total.R",
  "SSQ/Total_MFP_score.R",
  "SSQ/Ranking.R"
)

cat("开始运行所有R脚本生成图表...\n\n")

# 记录成功和失败的文件
success_files <- c()
failed_files <- c()

# 遍历所有文件并运行
for (file in r_files) {
  if (file.exists(file)) {
    cat(sprintf("正在运行: %s\n", file))
    tryCatch({
      source(file)
      success_files <- c(success_files, file)
      cat(sprintf("✓ 成功: %s\n\n", file))
    }, error = function(e) {
      failed_files <- c(failed_files, file)
      cat(sprintf("✗ 失败: %s - %s\n\n", file, conditionMessage(e)))
    })
  } else {
    cat(sprintf("⚠ 文件不存在: %s\n\n", file))
    failed_files <- c(failed_files, file)
  }
}

# 输出总结
cat("\n" , rep("=", 50), "\n", sep="")
cat("运行完成！\n\n")
cat(sprintf("成功: %d 个文件\n", length(success_files)))
cat(sprintf("失败: %d 个文件\n", length(failed_files)))

if (length(success_files) > 0) {
  cat("\n成功的文件:\n")
  for (file in success_files) {
    cat(sprintf("  ✓ %s\n", file))
  }
}

if (length(failed_files) > 0) {
  cat("\n失败的文件:\n")
  for (file in failed_files) {
    cat(sprintf("  ✗ %s\n", file))
  }
}

cat("\n")

