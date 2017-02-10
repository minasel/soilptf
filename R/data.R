#' Average values for soil texture classes based on the ROSETTA model
#'
#' These values are average values for theta_r, theta_s, log_alpha, log_n, ln_Ks, ln_K0, L
#' (including uncertainty) by soil textural class. The following variables are covered:
#' \describe{
#'   \item{theta_r}{Residual water content (cm3/cm3)}
#'   \item{theta_s}{Saturated water content (cm3/cm3)}
#'   \item{log_alpha}{The (log10 of the) alpha parameter in the Mualem (1976) model (log 1/cm)}
#'   \item{log_n}{The (log10 of the) n parameter in the Mualem (1976) model (log unitless)}
#'   \item{ln_Ks}{The (natural log of the) saturated conductivity (ln cm/day)}
#'   \item{ln_K0}{The (natural log of the) K0 conductivity (ln cm/day)}
#'   \item{L}{The L parameter in the van Genuchten (1980) model for unsaturated hydraulic conductivity
#'   (unitless)}
#' }
#'
#' @format A data frame with 84 rows and 5 variables.
#' \describe{
#'   \item{TextureClass}{The name of the texture class (one of Cl, ClLo, Lo, LoSa, Sa, SaCl, SaClLo,
#'     SaLo, Si, SiCl, SiClLo, or SiLo; see \link{texture.class})}
#'   \item{N}{the number of observations on which the parameters are based}
#'   \item{variable}{the variable of interest (one of theta_r, theta_s, log_alpha, log_n, ln_Ks, ln_K0, L)}
#'   \item{value}{The value of the parameter.}
#'   \item{stdev}{The uncertainty associated with the value measurement (one standard deviation)}
#' }
#'
#' @references
#' Schaap, M. G., Leij, F. J., & van Genuchten, M. T. (2001). ROSETTA: a computer program
#' for estimating soil hydraulic parameters with hierarchical pedotransfer functions.
#' Journal of Hydrology, 251(3), 163–176. \url{https://dx.doi.org/10.1016/S0022-1694(01)00466-8}

#'
#' @source \url{https://www.cals.arizona.edu/research/rosetta/}
"rosettaSoilClass"

# load within package so the data can be used internally
data("rosettaSoilClass", envir=environment())


#' Soil Texture Polygons for the 12 USDA Texture Classes
#'
#' @format A data frame with 58 rows and 4 variables.
#' \describe{
#'   \item{TextureClass}{The name of the texture class (one of Cl, ClLo, Lo, LoSa, Sa, SaCl, SaClLo,
#'     SaLo, Si, SiCl, SiClLo, or SiLo; see \link{texture.class})}
#'   \item{clay}{The proportion of clay (between 0 and 1)}
#'   \item{silt}{The proportion of silt (between 0 and 1)}
#'   \item{sand}{The proportion of sand (between 0 and 1)}
#' }
"TextureClassPolygonsUSDA"

# load within package so the data can be used internally
data("TextureClassPolygonsUSDA", envir=environment())
