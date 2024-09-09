
# 安装和加载raster包
install.packages("raster")
library(raster)
library(tidyverse)
# 读取1米分辨率的栅格数据
input_file <- "F:/NEON/PLOT_HARV/CHM_ALL/HARV_CHM_2019_1m.tif"
input_raster <- raster(input_file)

# 获取栅格数据的行数和列数
nrows <- nrow(input_raster)
ncols <- ncol(input_raster)

# 将栅格数据转换为表格

input_values <- getValues(input_raster)
input_table <- matrix(input_values, nrow = nrows, ncol = ncols, byrow = TRUE)


old_dim <- dim(input_table)
old_rows <- old_dim[1]
old_cols <- old_dim[2]


# 计算新表格的维度
new_rows <- old_rows %/% 30 + 1  # 新表格的行数，多出的行单独计算
new_cols <- old_cols %/% 30 + 1  # 新表格的列数，右侧剩余的10x30格子单独计算

# 创建新表格
new_table <- matrix(NA, nrow = new_rows, ncol = new_cols)

# 计算新表格的维度
new_rows <- old_rows %/% 30 + ifelse(old_rows %% 30 > 0, 1, 0)  # 新表格的行数，多出的行单独计算
new_cols <- old_cols %/% 30 + ifelse(old_cols %% 30 > 0, 1, 0)  # 新表格的列数，右侧剩余的10x30格子单独计算

# 创建新表格
new_table <- matrix(NA, nrow = new_rows, ncol = new_cols)

# 计算新表格中每个格子的95%分位数值
for (i in 1:new_rows) {
  for (j in 1:new_cols) {
    # 在旧表格中获取对应位置的数据
    start_row <- (i - 1) * 30 + 1
    end_row <- min(i * 30, old_rows)
    start_col <- (j - 1) * 30 + 1
    end_col <- min(j * 30, old_cols)
    subset_data <- input_table[start_row:end_row, start_col:end_col]
    
    # 忽略空值计算95%分位数值
    non_na_data <- subset_data[!is.na(subset_data)]
    if (length(non_na_data) > 0) {
      percentile <- 0.95
      new_table[i, j] <- quantile(non_na_data, probs = percentile)
    }
  }
}


new_raster <- raster(extent(input_raster), resolution = c(30, 30), crs = crs(input_raster ))
values(new_raster) <- as.matrix(new_table)
# 使用projectRaster函数将投影坐标系转换为WGS84地理坐标系
output_raster <- projectRaster(new_raster, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


# 保存输出栅格数据
output_file <- "F:/NEON/PLOT_HARV/CHM_ALL/HARV_CHM_2019_30m_rh95_WGS.tif"
writeRaster(output_raster, output_file, format = "GTiff", overwrite = TRUE)

# 输出成功提示
cat("栅格数据转换完成，并保存为", output_file)
