#' Average values for soil texture classes based on the ROSETTA model
#'
#' These values are average values for theta_r, theta_s, log_alpha, log_n, Ks, K0, L
#' (including uncertainty) by soil textural class. They are provided
#'
#' @format A data frame with 8735 rows and  19 variables. There are many columns,
#'   only several of which are used within this package.
#' \describe{
#'   \item{TextureClass}{the name of the texture class (one of Clay, C loam, Loam, L Sand, Sand,
#'         S C L, S Clay, Si C L, Si Clay, Si Loam, Silt, S loam)}
#'   \item{N}{the number of observations on which the parameters are based}
#'   \item{variable}{the variable of interest (one of theta_r, theta_s, log_alpha, log_n, Ks, K0, L)}
#'   \item{value}{the value of the parameter}
#'   \item{stdev}{the uncertainty associated with the value measurement (one standard deviation)}
#' }
#'
#' @references
#' Schaap, M. G., Leij, F. J., & Van Genuchten, M. T. (2001). ROSETTA: a computer program
#' for estimating soil hydraulic parameters with hierarchical pedotransfer functions.
#' Journal of Hydrology, 251(3), 163–176. \url{https://doi.org/10.1016/S0022-1694(01)00466-8}

#'
#' @source \url{https://www.cals.arizona.edu/research/rosetta/download/rosetta.pdf}
"rosettaSoilClass"

# load within package so the data can be used in getClimateSites()
data("rosettaSoilClass", envir=environment())
