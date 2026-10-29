# 1. Xác định vị trí thư mục file dữ liệu sạch
input_file <- "Data/processed/du_lieu_sach.csv"
output_dir <- "Output/tables"

if (!file.exists(input_file)) {
  stop(
    "Khong tim thay file du lieu: ", input_file,
    "\nHay kiem tra ten file va thu muc Data/processed."
  )
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2. DOC VA KIEM TRA DU LIEU 

df <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_cols <- c(
  "Date",
  "HPG_Close",
  "HSG_Close",
  "KDH_Close",
  "NLG_Close",
  "VNIndex_Close"
)

missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(
    "Thieu cot trong file du lieu: ",
    paste(missing_cols, collapse = ", ")
  )
}

# chuyển cột Date thành ngày tháng R hiểu
df$Date <- as.Date(df$Date)

if (any(is.na(df$Date))) {
  stop("Co ngay giao dich khong doc duoc. Hay kiem tra cot Date.")
}

df <- df[order(df$Date), ]

if (anyDuplicated(df$Date) > 0) {
  stop("Phat hien ngay giao dich trung lap.")
}

assets <- c("HPG", "HSG", "KDH", "NLG", "VNIndex")

price_cols <- c(
  HPG = "HPG_Close",
  HSG = "HSG_Close",
  KDH = "KDH_Close",
  NLG = "NLG_Close",
  VNIndex = "VNIndex_Close"
)

prices <- df[, price_cols, drop = FALSE]

# Chuyen cac cot gia ve dang numeric
prices[] <- lapply(prices, function(x) {
  as.numeric(as.character(x))
})

# Khong tu dong thay gia thieu hoac gia khong hop le bang 0
if (any(!is.finite(as.matrix(prices)))) {
  stop("Du lieu gia co NA, Inf hoac gia tri khong hop le.")
}

if (any(as.matrix(prices) <= 0)) {
  stop("Phat hien gia <= 0. Hay kiem tra du lieu goc.")
}

# 3. TINH LOI NHUAN HANG NGAY 
# Loi nhuan don:
# Return_t = (Gia_t / Gia_t-1) - 1

returns <- prices[-1, , drop = FALSE] /
  prices[-nrow(prices), , drop = FALSE] - 1

returns <- as.data.frame(returns)
names(returns) <- assets

returns <- data.frame(
  Date = df$Date[-1],
  returns,
  check.names = FALSE
)

# Xuat bang loi nhuan hang ngay
write.csv(
  returns,
  file.path(output_dir, "loi_nhuan_hang_ngay.csv"),
  row.names = FALSE,
  na = ""
)

# 4. HAM TINH MAXIMUM DRAWDOWN
# Drawdown = Gia tri hien tai / Dinh cao truoc do - 1
# MDD la drawdown am lon nhat trong giai doan.

calc_mdd <- function(price_series) {
  running_peak <- cummax(price_series)
  drawdown <- price_series / running_peak - 1
  min(drawdown, na.rm = TRUE)
}

# 5. TINH THONG KE CHO TUNG MA

summary_stock <- data.frame(
  Ma = assets,
  Loi_nhuan_trung_binh_ngay = NA_real_,
  Do_lech_chuan_ngay = NA_real_,
  Loi_nhuan_trung_binh_nam_xap_xi = NA_real_,
  Do_lech_chuan_nam = NA_real_,
  Maximum_Drawdown = NA_real_
)

for (i in seq_along(assets)) {
  asset <- assets[i]
  r <- returns[[asset]]
  p <- prices[[price_cols[[asset]]]]
  
  summary_stock$Loi_nhuan_trung_binh_ngay[i] <-
    mean(r, na.rm = TRUE)
  
  summary_stock$Do_lech_chuan_ngay[i] <-
    sd(r, na.rm = TRUE)
  
  # Quy doi xap xi theo 252 ngay giao dich/nam
  summary_stock$Loi_nhuan_trung_binh_nam_xap_xi[i] <-
    mean(r, na.rm = TRUE) * 252
  
  summary_stock$Do_lech_chuan_nam[i] <-
    sd(r, na.rm = TRUE) * sqrt(252)
  
  summary_stock$Maximum_Drawdown[i] <- calc_mdd(p)
}

# 6. SO SANH HAI NGANH 
# Thep: HPG, HSG
# Bat dong san: KDH, NLG
# Moi co phieu co trong so bang nhau trong tung ngay.

industry_returns <- data.frame(
  Date = returns$Date,
  Thep = rowMeans(returns[, c("HPG", "HSG")], na.rm = FALSE),
  Bat_dong_san = rowMeans(
    returns[, c("KDH", "NLG")],
    na.rm = FALSE
  ),
  VNIndex = returns$VNIndex
)

write.csv(
  industry_returns,
  file.path(output_dir, "loi_nhuan_hang_ngay_theo_nganh.csv"),
  row.names = FALSE,
  na = ""
)

calc_return_stats <- function(r, initial_wealth = 1) {
  wealth <- c(initial_wealth, cumprod(1 + r))
  running_peak <- cummax(wealth)
  drawdown <- wealth / running_peak - 1
  
  c(
    Loi_nhuan_trung_binh_ngay = mean(r, na.rm = TRUE),
    Do_lech_chuan_ngay = sd(r, na.rm = TRUE),
    Loi_nhuan_trung_binh_nam_xap_xi =
      mean(r, na.rm = TRUE) * 252,
    Do_lech_chuan_nam = sd(r, na.rm = TRUE) * sqrt(252),
    Maximum_Drawdown = min(drawdown, na.rm = TRUE)
  )
}

industry_stats <- rbind(
  Thep = calc_return_stats(industry_returns$Thep),
  Bat_dong_san = calc_return_stats(
    industry_returns$Bat_dong_san
  ),
  VNIndex = calc_return_stats(industry_returns$VNIndex)
)

industry_stats <- data.frame(
  Nhom = rownames(industry_stats),
  industry_stats,
  row.names = NULL,
  check.names = FALSE
)

# 7. LAM TRON VA XUAT BANG

numeric_cols_stock <- vapply(summary_stock, is.numeric, logical(1))
summary_stock[numeric_cols_stock] <-
  lapply(summary_stock[numeric_cols_stock], round, digits = 6)

numeric_cols_industry <- vapply(
  industry_stats, is.numeric, logical(1)
)
industry_stats[numeric_cols_industry] <-
  lapply(industry_stats[numeric_cols_industry], round, digits = 6)

write.csv(
  summary_stock,
  file.path(output_dir, "thong_ke_loi_nhuan_rui_ro_tung_ma.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  industry_stats,
  file.path(output_dir, "so_sanh_loi_nhuan_rui_ro_theo_nganh.csv"),
  row.names = FALSE,
  na = ""
)

# 8. THONG BAO KET QUA ---------------------------------------

cat("\n===== THONG KE TUNG MA =====\n")
print(summary_stock)

cat("\n===== SO SANH THEO NGANH =====\n")
print(industry_stats)

cat("\nDa xuat cac file vao thu muc:", output_dir, "\n")
