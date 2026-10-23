
# ============================================================
# 01_doc_du_lieu.R
# Phân tích mối quan hệ giữa cổ phiếu ngành Thép
# và Bất động sản tại Việt Nam giai đoạn 2021-2025
# Đọc, kiểm tra dữ liệu ban đầu
# ============================================================

# 1. Khai báo thư mục dữ liệu
input_dir <- "C:/Users/Admin/Desktop/data/raw"

# 2. Khai báo tên 5 file CSV
file_map <- data.frame(
  stock_name = c("HPG", "HSG", "KDH", "NLG", "VNIndex"),
  filename = c(
    "Dữ liệu Lịch sử HPG.csv",
    "Dữ liệu Lịch sử HSG.csv",
    "KDH.csv",
    "NLG.csv",
    "Dữ liệu Lịch sử VN Index.csv"
  ),
  stringsAsFactors = FALSE
)

# 3. Kiểm tra các file có tồn tại không
cat("===== KIEM TRA FILE DU LIEU =====\n")

file_map$file_path <- file.path(
  input_dir,
  file_map$filename
)

file_map$exists <- file.exists(file_map$file_path)

print(file_map[, c("stock_name", "filename", "exists")])

if (!all(file_map$exists)) {
  missing_files <- file_map$filename[!file_map$exists]
  
  stop(
    "Khong tim thay cac file: ",
    paste(missing_files, collapse = ", "),
    "\nHay kiem tra lai ten file trong thu muc raw."
  )
}

# 4. Đọc dữ liệu gốc
raw_data <- list()

for (i in seq_len(nrow(file_map))) {
  stock <- file_map$stock_name[i]
  path <- file_map$file_path[i]
  
  raw_data[[stock]] <- read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  
  cat("\n===== DU LIEU:", stock, "=====\n")
  
  # 5. Kiểm tra tên cột
  cat("\nTen cot:\n")
  print(names(raw_data[[stock]]))
  
  # 6. Kiểm tra kiểu dữ liệu
  cat("\nKieu du lieu:\n")
  str(raw_data[[stock]])
  
  # 7. Kiểm tra kích thước dữ liệu
  cat("\nSo dong va so cot:\n")
  print(dim(raw_data[[stock]]))
  
  # 8. Kiểm tra dữ liệu thiếu
  cat("\nSo gia tri thieu theo cot:\n")
  print(colSums(is.na(raw_data[[stock]])))
  
  # 9. Kiểm tra ngày giao dịch
  if ("Ngày" %in% names(raw_data[[stock]])) {
    dates <- as.Date(
      raw_data[[stock]][["Ngày"]],
      format = "%d/%m/%Y"
    )
    
    cat("\nSo ngay khong doc duoc:\n")
    print(sum(is.na(dates)))
    
    cat("\nKhoang ngay giao dich:\n")
    print(range(dates, na.rm = TRUE))
  } else {
    warning(
      "Khong tim thay cot Ngay trong file ",
      stock
    )
  }
}
