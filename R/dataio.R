#'「おんどとり」により作成されるデータ(温湿度2ch)を読み込む
#'
#' 「おんどとり(T&D)」により採取し、付属ソフトウェアで出力したカンマ区切りテキストファイルを
#'  読み込み、データフレームに加工する。その際、2種類の飽差(VPD及びHD)を計算し、
#'  列として付加する。飽差の計算のためには相対湿度情報が必要であるため、
#'  ch1が温度、ch2が湿度である必要がある。TR-72シリーズなどにより採取されたデータから
#'  テキスト形式でそのまま出力した場合は該当の形式になっている。
#'  温度1chのみのデータの読み込みは\code{\link{read_ocsv_s}}を参照。
#'
#'@param fname ファイルネーム。ファイルの形式はカンマ区切りテキスト(CSV)とする。
#'@return 返り値はデータフレームであり、以下の列を持つ。
#' \item{datetime}{POSIXct形式の日時。}
#' \item{day}{Date形式の日付。}
#' \item{temp}{気温(C)}
#' \item{RH}{相対湿度(\%)}
#' \item{vpd}{飽差(VPD, hPa)}
#' \item{w.dif}{飽差(HD, g/m^3)}
#'@export
#'@import dplyr
read_ocsv_rh <- function(fname){
  # おんどとりから出力のカンマ区切り形式テキストファイルの読み込み
  # 温度・湿度の2chデータ対象
  # 飽差(VPD及びHD)を追記して出力
  read.csv(fname, skip = 4, stringsAsFactors = FALSE,
           header = FALSE, row.names = NULL) %>% # 不要な行はスキップ
    select(1, 3:4) %>%  # シリアル値を除外して温度・湿度列を選択
    mutate(datetime = as.POSIXct(V1, "%Y/%m/%d %H:%M'%S", tz = "UTC"), # データフレーム内ではPOSIXct
           day = as.Date(datetime, tz = "UTC"), # tzをそろえる
           temp= V3, #リネーム
           RH = V4, #リネーム
           vpd = vpd(temp, RH), # 飽差VPD
           w.dif = w.dif(temp, RH) #飽差HD
    ) %>%
    select(-(1:3)) # 不要な列の削除
}

#'「おんどとり」により作成されるデータ(温度1ch)を読み込む
#'
#' 「おんどとり(T&D)」により採取し、付属ソフトウェアで出力したカンマ区切りテキストファイルを
#'  読み込み、データフレームに加工する。
#'  ch1に記録されたデータを温度として扱う。
#'  湿度データをch2に含むデータの読み込みは\code{\link{read_ocsv_rh}}を参照。
#'@param fname ファイルネーム。ファイルの形式はカンマ区切りテキスト(CSV)とする。
#'@return 返り値はデータフレームであり、以下の列を持つ。
#' \item{datetime}{POSIXct形式の日時。}
#' \item{day}{Date形式の日付。}
#' \item{temp}{気温(C)}
#'@export
read_ocsv_s <- function(fname){
  # おんどとりから出力のカンマ区切り形式テキストファイルの読み込み
  # 温度のみ1chのデータ対象
  read.csv(fname, skip = 4, stringsAsFactors = FALSE,
           row.names = NULL, header = FALSE) %>%
    select(1, 3) %>%
    mutate(datetime = as.POSIXct(V1, "%Y/%m/%d %H:%M'%S", tz = "UTC"),
           day = as.Date(datetime, tz = "UTC"),
           temp = V3) %>%
    select(-(1:2))
}
