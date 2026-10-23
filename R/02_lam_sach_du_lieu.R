
# ============================================================
# 02_lam_sach_du_lieu.R
# Lam sach va ghep 4 co phieu + VN-Index
# ============================================================
library(tibble)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)

input_dir  <- "C:/Users/Admin/Desktop/data/raw"
output_dir <- "C:/Users/Admin/Desktop/data/processed"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Chuyen gia sang dang so
parse_price <- function(x) {
  parse_number(
    as.character(x),
    locale = locale(decimal_mark = ".", grouping_mark = ",")
  )
}

# 2. Chuyen khoi luong K/M sang so
parse_volume <- function(x) {
  x <- str_trim(as.character(x))
  
  multiplier <- case_when(
    str_detect(x, regex("M$", ignore_case = TRUE)) ~ 1e6,
    str_detect(x, regex("K$", ignore_case = TRUE)) ~ 1e3,
    TRUE ~ 1
  )
  
  parse_number(
    x,
    locale = locale(decimal_mark = ".", grouping_mark = ",")
  ) * multiplier
}

# 3. Doc va chuan hoa mot file
read_stock <- function(filename, stock_name) {
  
  file_path <- file.path(input_dir, filename)
  
  if (!file.exists(file_path)) {
    stop("Khong tim thay file: ", file_path)
  }
  
  raw <- read_csv(
    file_path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )
  
  required_cols <- c("Ngày", "Lần cuối", "KL")
  
  if (!all(required_cols %in% names(raw))) {
    stop("File khong dung cau truc: ", filename)
  }
  
  price_col  <- paste0(stock_name, "_Close")
  volume_col <- paste0(stock_name, "_Volume")
  
  clean <- raw %>%
    transmute(
      Date = dmy(`Ngày`),
      !!price_col := parse_price(`Lần cuối`),
      !!volume_col := parse_volume(KL)
    ) %>%
    filter(!is.na(Date)) %>%
    arrange(Date)
  
  # Bao cao ngay trung lap
  if (anyDuplicated(clean$Date)) {
    warning(filename, " co ngay trung lap; giu ban ghi dau tien.")
    clean <- clean %>% distinct(Date, .keep_all = TRUE)
  }
  
  # Bao cao gia thieu / khong hop le
  if (any(is.na(clean[[price_col]]) |
          clean[[price_col]] <= 0)) {
    warning(filename, " co gia thieu hoac khong hop le.")
  }
  
  message(
    stock_name, ": ", nrow(clean), " dong | ",
    min(clean$Date), " - ", max(clean$Date)
  )
  
  clean
}

# 4. Khai bao danh sach 5 file
file_map <- tibble(
  filename = c(
    "Dữ liệu Lịch sử HPG.csv",
    "Dữ liệu Lịch sử HSG.csv",
    "KDH.csv",
    "NLG.csv",
    "Dữ liệu Lịch sử VN Index.csv"
  ),
  stock_name = c("HPG", "HSG", "KDH", "NLG", "VNIndex")
)

# 5. Doc va chuan hoa ca 5 file
stock_list <- map2(
  file_map$filename,
  file_map$stock_name,
  read_stock
)

names(stock_list) <- file_map$stock_name

# 6. Ghép theo ngay
# full_join giu cac ngay xuat hien trong bat ky chuoi nao
data_merged <- reduce(
  stock_list,
  full_join,
  by = "Date"
) %>%
  arrange(Date)

# 7. Kiem tra du lieu sau khi ghep
cat("\nSo dong sau khi ghep:", nrow(data_merged), "\n")
cat("So cot sau khi ghep:", ncol(data_merged), "\n")
cat("Ngay bat dau:", as.character(min(data_merged$Date, na.rm = TRUE)), "\n")
cat("Ngay ket thuc:", as.character(max(data_merged$Date, na.rm = TRUE)), "\n")

cat("\nSo gia tri thieu theo cot:\n")
print(colSums(is.na(data_merged)))

# 8. Luu du lieu da lam sach
write_csv(
  data_merged,
  file.path(output_dir, "du_lieu_sach.csv")
)

cat("\nDa luu file du lieu sach thanh cong.\n")

# Check
# 1. Kiem tra kich thuoc du lieu
dim(data_merged)
names(data_merged)

# 2. Kiem tra kieu du lieu
str(data_merged)

# 3. Kiem tra ngay giao dich trung lap
sum(duplicated(data_merged$Date))

# 4. Kiem tra ngay bat dau va ket thuc
range(data_merged$Date, na.rm = TRUE)

# 5. Dem gia tri thieu theo cot
colSums(is.na(data_merged))

# 6. Kiem tra gia dong cua khong hop le
price_cols <- c(
  "HPG_Close", "HSG_Close",
  "KDH_Close", "NLG_Close",
  "VNIndex_Close"
)

sapply(data_merged[price_cols], function(x) {
  sum(x <= 0, na.rm = TRUE)
})

# Kiem tra thu tu ngay
all(diff(data_merged$Date) > 0)

# Kiem tra so ngay giao dich duy nhat
length(unique(data_merged$Date))

# Xem ngay dau va ngay cuoi
head(data_merged$Date, 3)
tail(data_merged$Date, 3)

# Thong ke co ban cac cot gia
summary(data_merged[, c(
  "HPG_Close", "HSG_Close",
  "KDH_Close", "NLG_Close",
  "VNIndex_Close"
)])






