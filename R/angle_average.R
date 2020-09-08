#' @name angle_average
#' @title Circular average of a set of angle degrees
#' @description Calculating the circular average from a set of angle degrees.
#' @param data a numeric vector of angle degrees.
#' @return a number, the mean degree from the input `data`.
#' @export
#' @examples
#' deg.ave = angle_average(data = c(350, 5, 20))

angle_average = function(data)
{
  stopifnot(is.numeric(data))

  rad.angle = data * pi / 180
  sin.data = sin(rad.angle)
  cos.data = cos(rad.angle)
  ave.sin = mean(sin.data)
  ave.cos = mean(cos.data)
  rad.angle.ave = atan2(ave.sin,ave.cos)
  deg.angle.ave = rad.angle.ave * 180 / pi
  deg.ave.round = round(deg.angle.ave, digits = 1)
  deg.ave = ifelse(deg.ave.round < 0, deg.ave.round + 360, deg.ave.round)

  return(deg.ave)
}

