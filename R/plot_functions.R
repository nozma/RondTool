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
