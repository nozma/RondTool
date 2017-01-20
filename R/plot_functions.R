#' 温湿度・飽差の日最高・平均・最低値の推移をプロットする
#'
#' \code{\link{read_ocsv_rh}}により読み込まれたデータを加工し、
#' 温度、相対湿度、飽差について日最高・平均・最低値を算出し、
#' これらの推移をプロットする。
#'@param ondo \code{\link{read_ocsv_rh}}で読み込んだデータフレーム。
#'@param start 集計の開始日。\code{"yyyy/mm/dd"}形式で指定する。
#'@param end 集計の終了日。指定方法はstartと同様。
#'@export
#'@import ggplot2 dplyr tidyr
ondo.summarized.plot <- function(ondo, start=range(ondo$day)[1], end=range(ondo$day)[2]){
  labeli <- as_labeller(c(`RH` = "相対湿度(%)",
                          `temp` = "気温(C)",
                          `w.dif` = "飽差(g/m^3)"))
  if(Sys.info()["sysname"] == "Darwin"){ # macの場合日本語フォントを設定する
    mytheme = theme_bw(base_family = "HiraKakuPro-W3")
  } else {
    mytheme = theme_bw()
  }
  ondo %>% group_by(day) %>%
    filter(day >= start, day <= end) %>%
    summarize_each(funs = funs(min, mean, max), temp, RH, w.dif) %>%
    gather(type, value, -day) %>%
    separate(type, c("v.type", "s.type"), sep="_") %>%
    ggplot(aes(x = day, y = value)) +
    facet_grid(v.type~., scale="free_y", labeller = labeli) +
    geom_line(aes(col = s.type)) +
    labs(y = "", x = "日付") +
    mytheme +
    scale_x_date(date_labels = "%m/%d", date_breaks = "7 day") +
    scale_color_manual(name = "凡例", values = c("red", "green", "blue"),
                       labels = c("最高", "平均", "最低"))
}


#' 温湿度・飽差の時系列データを1日毎にプロットする
#'
#' \code{\link{read_ocsv_rh}}により読み込んだデータについて、
#' \code{start}から\code{end}までの期間の温湿度・飽差データのプロットを作成する。
#' プロットは1日おきに分割され、横に並べられる。
#' それぞれのプロットには温湿度・飽差の日平均・最高・最低値がテキストとして表示される。
#'
#'@param ondo \code{\link{read_ocsv_rh}}で読み込んだデータフレーム。
#'@param start 集計の開始日。\code{"yyyy/mm/dd"}形式で指定する。
#'@param end 集計の終了日。指定方法はstartと同様。
#'@export
#'@import ggplot2 dplyr tidyr
ondo.raw.plot <- function(ondo, start=range(ondo$day)[1], end=range(ondo$day)[2]){
  # macの場合は日本語フォントを設定する
  if(Sys.info()["sysname"] == "Darwin"){
    fname = "Osaka-Mono"
    mytheme = theme_bw(base_family = fname)
  } else {
    fname = ""
    mytheme = theme_bw()
  }
  # テキスト用にデータを集計
  tmp <- filter(ondo, day >= start, day <= end) %>%
    group_by(day) %>%
    summarize(tmax = max(temp), tmean = mean(temp), tmin = min(temp),
              rmax = max(RH),   rmean = mean(RH),   rmin = min(RH),
              dmax = max(w.dif),dmean = mean(w.dif),dmin = min(w.dif))
  tmp <- cbind(round(tmp[,2:10],1))
  # 日付ごとにラベルを生成
  lab <- apply(tmp, MARGIN = 1,
               function(x){
                 x <- sprintf("%5.1f", x)
                 paste(
                                 "最高　",  "平均　", "最低\n",
                   "相対湿度 ", x[4], " ", x[5], " ", x[6], "\n",
                   "気温 ",     x[1], " ", x[2], " ", x[3], "\n",
                   "飽差 ",     x[7], " ", x[8], " ", x[9],  sep = ""
                 )})


  gather(ondo, RH.Temp, value, temp, RH, w.dif) %>% #日別の生データプロット
    filter(day >= start, day <= end) %>%
    ggplot(aes(x = as.POSIXlt(datetime)$hour + as.POSIXlt(datetime)$min/60,
               y = value)) +
    facet_wrap(~day, nrow = 1) +
    scale_y_continuous(breaks = seq(0, 100, by = 10), limit = c(0, 100)) +
    scale_x_continuous(breaks = seq(0, 24, by = 6), limit = c(0, 24)) +
    geom_line(aes(color = RH.Temp)) +
    annotate("text", label = lab, x = 24, y = 50, adj = "right",
            color = "black", family = fname) +
    labs(y = "", x = "時刻") +
    scale_colour_discrete(name = "凡例",
                          labels = c("相対湿度（％）","気温（℃）",  "飽差（g/m^3)")) +
    scale_linetype_discrete(name ="凡例", labels = c("相対湿度（％）", "気温（℃）", "飽差（g/m^3)")) +
    mytheme
}
