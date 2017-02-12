
#' Get ROSETTA soil parameters from a Texture Class vector
#'
#' @param textureclass A vector of texture classes (will be sanitized with \link{texture.class})
#' @param delog Pass TRUE to de-log the parameters that are log-ed (alpha and n) or ln-ed (Ks and K0)
#' @param ranges Pass TRUE to return the min/max of one standard deviation in addition to the values. If
#'   \code{delog=TRUE} these will transformed accordingly, and therefore may not be symmetric around each
#'   value
#'
#' @return A \code{data.frame} with the same number of rows as \code{length(textureclass)}, and the
#'   following columns:
#'  \describe{
#'   \item{theta_r}{Residual water content (cm3/cm3)}
#'   \item{theta_s}{Saturated water content (cm3/cm3)}
#'   \item{log_alpha}{The (log10 of the) alpha parameter in the Mualem (1976) model (log 1/cm).
#'     Will be just \code{alpha} if \code{delog=TRUE}}
#'   \item{log_n}{The (log10 of the) n parameter in the Mualem (1976) model (log unitless).
#'     Will be just \code{n} if \code{delog=TRUE}}
#'   \item{ln_Ks}{The (natural log of the) saturated conductivity (ln cm/day),
#'     Will be just \code{Ks} if \code{delog=TRUE}}
#'   \item{ln_K0}{The (natural log of the) K0 conductivity (ln cm/day).
#'     Will be just \code{Ks} if \code{delog=TRUE}}
#'   \item{L}{The L parameter in the van Genuchten (1980) model for unsaturated hydraulic conductivity
#'   (unitless)}
#'  }
#'
#'  In addition, columns with the names above and suffixes "_min" and "_max" will be returned
#'  if \code{ranges=TRUE}.
#'
#' @seealso rosettaSoilClass
#'
#' @export
#'
soilinfo.rosetta <- function(textureclass, delog=TRUE, ranges=FALSE) {
  rows <- match(texture.class(textureclass, validate=FALSE), rosettaSoilClassWide$TextureClass)
  if(ranges) {
    cols <- names(rosettaSoilClassWide)[grepl("_value|_min|_max", names(rosettaSoilClassWide))]
  } else {
    cols <- names(rosettaSoilClassWide)[grepl("_value", names(rosettaSoilClassWide))]
  }

  soilinfo <- rosettaSoilClassWide[rows, cols]
  names(soilinfo) <- gsub("_value", "", names(soilinfo))
  if(delog) {
    soilinfo$Ks <- exp(soilinfo$ln_Ks); soilinfo$ln_Ks <- NULL
    soilinfo$K0 <- exp(soilinfo$ln_K0); soilinfo$ln_K0 <- NULL
    soilinfo$n <- 10^soilinfo$log_n; soilinfo$log_n <- NULL
    soilinfo$alpha <- 10^soilinfo$log_alpha; soilinfo$log_alpha <- NULL
    if(ranges) {
      soilinfo$Ks_min <- exp(soilinfo$ln_Ks_min); soilinfo$ln_Ks_min <- NULL
      soilinfo$Ks_max <- exp(soilinfo$ln_Ks_max); soilinfo$ln_Ks_max <- NULL
      soilinfo$K0_min <- exp(soilinfo$ln_K0_min); soilinfo$ln_K0_min <- NULL
      soilinfo$K0_max <- exp(soilinfo$ln_K0_max); soilinfo$ln_K0_max <- NULL
      soilinfo$n_max <- 10^soilinfo$log_n_max; soilinfo$log_n_max <- NULL
      soilinfo$n_min <- 10^soilinfo$log_n_min; soilinfo$log_n_min <- NULL
      soilinfo$alpha_max <- 10^soilinfo$log_alpha_max; soilinfo$log_alpha_max <- NULL
      soilinfo$alpha_min <- 10^soilinfo$log_alpha_min; soilinfo$log_alpha_min <- NULL
    }
  }
  row.names(soilinfo) <- NULL
  # order columns predictably
  colorder <- c("theta_r", "theta_s", "log_alpha", "alpha", "log_n", "n", "ln_Ks", "Ks",
                "ln_K0", "K0", "L")
  colorder <- c(colorder, paste0(colorder, "_min"), paste0(colorder, "_max"))
  return(soilinfo[order(match(names(soilinfo), colorder))])
}
