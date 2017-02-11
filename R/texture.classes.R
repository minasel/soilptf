
.allclasses <- c("Cl", "ClLo",
                 "Lo", "LoSa",
                 "Sa", "SaCl", "SaClLo", "SaLo",
                 "Si", "SiCl", "SiClLo", "SiLo")

.sand <- "sand|sandy|sa|(\\s+s\\s+)|(^s\\s+)|(\\s+s$)|(\\s+sa\\s+)|(^sa\\s+)|(\\sa+s$)"
.silt <- "silt|silty|si|(\\s+si\\s+)|(^si\\s+)|(\\s+si$)"
.clay <- "clay|clayey|cl|(\\s+c\\s+)|(^c\\s+)|(\\s+c$)|((\\s+cl\\s+)|(^cl\\s+)|(\\s+cl$))"
.loam <- "loam|loamy|lo|(\\s+l\\s+)|(^l\\s+)|(\\s+l$)|(\\s+lo\\s+)|(^lo\\s+)|(\\s+lo$)"

#' Sanitize texture class descriptions
#'
#' This collection of functions transforms input to a vector of USDA soil classes that
#' can be used as input to other functions in this package. If \code{x} is a character
#' or factor, it is transformed by assuming "sand" is represented by "Sa", "S", or "Sand";
#' "silt" is represented by "Si" or "Silt"; "clay" is represented by "Cl", "C", or "Clay";
#' and "loam" is represented by "Lo", "L", or "Loam". The components must be separated by
#' a space. If input is numeric, the input order is sand, then clay, then silt. If silt
#' is provided, input checked to make sure sand + clay + silt == 1 or
#' sand + clay + silt == 100. If all values are less than or equal to 1, a total sum of 1
#' is assumed; if not, a total sum of 100 is assumed. \code{x} is a \code{data.frame},
#' columns for sand, clay, and silt are found (as described above) and passed to
#' \code{texture.class.numeric}.
#'
#' @param x An object to be converted to a vector of texture classes, or a
#'  numeric vector describing sand content.
#' @param clay A numeric vector describing relative clay content
#' @param silt A numeric vector describing relative silt content
#' @param stringsAsFactors Pass TRUE to output a factor vector, FALSE will generate
#'   a character vector.
#' @param validate Pass TRUE to ensure output is within the 12 USDA texture classes.
#' @param ... Pass to/from methods
#'
#' @return A factor with levels Cl, ClLo, Lo, LoSa, Sa, SaCl, SaClLo,
#'     SaLo, Si, SiCl, SiClLo, or SiLo; or a character vector of the above
#'     if \code{stringsAsFactors} is FALSE.
#'
#' @examples
#' texture.class(c("silty clay loam", "sandy clay loam", "s c l", "S C L"))
#'
#' @export
texture.class <- function(x, ..., stringsAsFactors=TRUE, validate=TRUE) UseMethod("texture.class")

#' @rdname texture.class
#' @export
texture.class.character <- function(x, ..., stringsAsFactors=TRUE, validate=TRUE) {
  x <- gsub("\\s", "",
            gsub(.sand, " Sa ", ignore.case = TRUE,
                 gsub(.silt, " Si ", ignore.case = TRUE,
                      gsub(.clay, " Cl ", ignore.case = TRUE,
                           gsub(.loam, " Lo ", ignore.case=TRUE, x)))))
  if(validate) {
    invalid <- unique(x[!is.na(x) & !( x %in% .allclasses)])
    if(length(invalid) > 0) {
      warning("texture.class generated the following invalid soil classes: ",
              paste("'", invalid, "'", collapse=", "))
    }
  }
  if(stringsAsFactors) {
    factor(x, levels=.allclasses)
  } else {
    x
  }
}

texture.class.factor <- function(x, ..., stringsAsFactors=TRUE, validate=TRUE) {
  if(stringsAsFactors) {
    levs <- levels(x)
    if(all(levs==.allclasses)) {
      return(x)
    } else if(all(levs %in% .allclasses)) {
      return(factor(x, levels=.allclasses))
    }
  }
  return(texture.class.character(as.character(x), stringsAsFactors = stringsAsFactors,
                                 validate = validate))
}

#' @rdname texture.class
#' @export
texture.class.numeric <- function(x, clay, silt=NULL, ..., stringsAsFactors=TRUE, validate=TRUE) {
  # creating df ensures 'proper' recycling of vectors
  df <- data.frame(sand=x, clay=as.numeric(clay))
  df$silt <- silt
  if(any(c(df$sand, df$silt, df$clay) > 1.0)) {
    df$sand <- df$sand / 100.0
    df$clay <- df$clay / 100.0
    if(!is.null(df$silt)) {
      df$silt <- df$silt / 100.0
    }
  }

  if(!is.null(df$silt) && any(abs(1.0 - df$sand - df$clay - df$silt) > 1e-7 )) {
    stop("Sand+Silt+Clay must equal 1 or 100")
  }

  tc <- rep(NA, nrow(df))
  # looping because there will only ever be 12 texture classes
  for(tclass in .allclasses) {
    polygon <- TextureClassPolygonsUSDA[TextureClassPolygonsUSDA$TextureClass==tclass,]
    inclass <- point.in.polygon(df$sand, df$clay, polygon$sand, polygon$clay)
    tc[is.na(tc) & inclass] <- tclass
  }
  return(texture.class(tc, stringsAsFactors = stringsAsFactors))
}

#' @rdname texture.class
#' @export
texture.class.data.frame <- function(x, ..., stringsAsFactors=TRUE, validate=TRUE) {
  sandcols <- names(x)[grepl(.sand, names(x))]
  claycols <- names(x)[grepl(.clay, names(x))]
  siltcols <- names(x)[grepl(.silt, names(x))]
  if(length(sandcols) == 0) stop("No column defining 'sand' was found")
  if(length(sandcols) > 1) warning("More than one column defining 'sand' was found. Using '",
                                   sandcols[1], "'")
  sandcol <- x[[sandcols[1]]]
  if(length(claycols) == 0) stop("No column defining 'clay' was found")
  if(length(claycols) > 1) warning("More than one column defining 'clay' was found. Using '",
                                   claycols[1], "'")
  claycol <- x[[claycols[1]]]
  if(length(siltcols) > 1) warning("More than one column defining 'silt' was found. Using '",
                                   siltcols[1], "'")
  if(length(siltcols) == 0) {
    siltcol <- NULL
  } else {
    siltcol <- x[[siltcols[1]]]
  }
  texture.class.numeric(x=sandcol, clay=claycol, silt=siltcol, stringsAsFactors = stringsAsFactors,
                        validate = validate)
}

