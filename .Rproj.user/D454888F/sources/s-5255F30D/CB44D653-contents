#' @name emd_simple
#' @title The Earth mover's distance between two sequences with the same length.
#' @description Calculating the Earth mover's distance (Wasserstein L-1) between
#' the observations of any two stations in the input data, which have the same
#' reporting datetimes.
#' @param data a data.frame of observations by different stations, each
#' column represents a station, and the rows represent standard recording datetimes.
#' @return a matrix, the value at row i and column j is the Earth
#' mover's distance between station i and j in `data`.
#' @export
#' @examples
#' wow_emd = emd_simple(wow_ws)

emd_simple = function(data)
{

  len = ncol(data)
  emd_matrix_ws = matrix(NA, nrow = len, ncol = len)
  for (i in 2:len) {
    for (j in 1:(i-1)) {
      laws = which(!is.na(all_bc_ws[,i]) & !is.na(all_bc_ws[,j]))
      if (length(laws) != 0) {
        emd_matrix_ws[i,j] = sum( abs( sort(all_bc_ws[laws,i]) - sort(all_bc_ws[laws,j]) ) ) / length(laws)
      } else {
        emd_matrix_ws[i,j] = NA
      }
      # emd_matrix_ws[i,j] = wasserstein1d(all_bc_ws[laws,i], all_bc_ws[laws,j])
    }
  }
  for (i in 2:len) {
    for (j in 1:(i-1)) {
      emd_matrix_ws[j,i] = emd_matrix_ws[i,j]
    }
  }
  for (i in 1:len) {
    emd_matrix_ws[i,i] = 0
  }

  return(emd_matrix_ws)
}
