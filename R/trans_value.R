#'セ氏気温から飽和水蒸気圧を計算する。
#'
#' 引数として与えられたセ氏温度に対応する飽和水蒸気圧を計算する。
#' 計算式はAdduchov and Eskridge(1996)による。
#' 通常、飽和水蒸気圧は氷上であるか水面上であるかで異なるが、
#' この関数では単にセ氏温度が0度以下であるか否かで判断している。
#'
#'@param t 気温(セルシウス度)
#'@return 飽和水蒸気圧(hPa)
#'@examples
#' svp(0)
#' svp(100)
#'@export
svp <- function(t){
  ifelse(t > 0,
         6.1094 * exp(17.625 * t / (243.04 + t)),
         6.1121 * exp(22.587 * t / (273.86 + t)))
}

#'セ氏温度と相対湿度から飽差(VPD)を計算する
#'
#' セ氏温度と相対湿度から飽差を計算する。
#' 飽差は水蒸気分圧の差(Vapor Pressure Deficit)として求める。
#' 水蒸気量の差として求める飽差(Humidity Deficit)については\code{\link{w.dif}}を参照。
#'
#'@param t 気温(セルシウス度)
#'@param RH 相対湿度(\%)
#'@return 飽差(hPa)
#'@examples
#'vpd(25, 50)
#'vpd(10, 50)
#'@export
vpd <- function(t, RH){ #飽差（VPD：水蒸気分圧差）
  svp(t) * (1 - RH/100)
}

#'セ氏温度と相対湿度から飽差(HD)を計算する
#'
#' セ氏温度と相対湿度から飽差を計算する。
#' 飽差は水蒸量の差(Humidity Deficit)として求める。
#' 水蒸気分圧の差として求める飽差(Vapor Pressure Deficit)については\code{\link{vpd}}を参照。
#'
#' @param t 気温(セルシウス度)
#' @param RH 相対湿度(\%)
#' @return 飽差（g/m^3）
w.dif <- function(t, RH){ #飽差（HD：水蒸気量差）
  2.166740 * 100 * (100 - RH) * svp(t)/(100 * (t + 273.15))
}
