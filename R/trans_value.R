#'セ氏気温から飽和水蒸気圧を計算する。式はAdduchov and Eskridge(1996)による。
#'
#'@param t 気温（セルシウス度）
#'
#'@return 飽和水蒸気圧（Pa）
#'
#'@export
svp <- function(t){
  ifelse(t > 0,
         6.1094 * exp(17.625 * t / (243.04 + t)),
         6.1121 * exp(22.587 * t / (273.86 + t)))
}
