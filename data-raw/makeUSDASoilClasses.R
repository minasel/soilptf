
# from https://github.com/rforge/soiltexture/blob/master/pkg/soiltexture/R/soiltexture.R#L456

USDA_points <- data.frame(
  "CLAY"      = c( 0.550, 0.600, 0.350, 0.350, 0.400, 0.400, 0.400, 0.200, 0.200, 0.275, 0.275, 0.275,
                   0.275, 0.150, 0.100, 0.075, 0.075, 0.125, 0.125, 0.000, 0.000, 0.000, 0.000,
                   1.000, 0.000, 0.000  ),
  #
  "SILT"      = c( 0.000, 0.400, 0.000, 0.200, 0.150, 0.400, 0.600, 0.000, 0.275, 0.275, 0.500, 0.525,
                   0.725, 0.000, 0.000, 0.400, 0.500, 0.800, 0.875, 0.150, 0.300, 0.500, 0.800,
                   0.000, 0.000, 1.000  ),
  #
  "SAND"      = c( 0.450, 0.000, 0.650, 0.450, 0.450, 0.200, 0.000, 0.800, 0.525, 0.450, 0.225, 0.200,
                   0.000, 0.850, 0.900, 0.525, 0.425, 0.075, 0.000, 0.850, 0.700, 0.500, 0.200,
                   0.000, 1.000, 0.000  )
)

USDA_verticies <- list(
  "Cl"        = list( "name" = "clay",            "points" = c(24,01,05,06,02)            ),
  "SiCl"      = list( "name" = "silty clay",      "points" = c(02,06,07)                  ),
  "SaCl"      = list( "name" = "sandy clay",      "points" = c(01,03,04,05)               ),
  "ClLo"      = list( "name" = "clay loam",       "points" = c(05,04,10,11,12,06)         ),
  "SiClLo"    = list( "name" = "silty clay loam", "points" = c(06,12,13,07)               ),
  "SaClLo"    = list( "name" = "sandy clay loam", "points" = c(03,08,09,10,04)            ),
  "Lo"        = list( "name" = "loam",            "points" = c(10,09,16,17,11)            ),
  "SiLo"      = list( "name" = "silty loam",      "points" = c(11,17,22,23,18,19,13,12)   ),
  "SaLo"      = list( "name" = "sandy loam",      "points" = c(08,14,21,22,17,16,09)      ),
  "Si"        = list( "name" = "silt",            "points" = c(18,23,26,19)               ),
  "LoSa"      = list( "name" = "loamy sand",      "points" = c(14,15,20,21)               ),
  "Sa"        = list( "name" = "sand",            "points" = c(15,25,20)                  )
)

USDA_df <- plyr::ldply(USDA_verticies, function(item) {
  USDA_points[item$points,]
})
TextureClassPolygonsUSDA <- plyr::rename(USDA_df, c(.id="TextureClass", SAND="sand", CLAY="clay", SILT="silt"))
TextureClassPolygonsUSDA$TextureClass <- texture.class(TextureClassPolygonsUSDA$TextureClass)
# library(ggplot2)
# ggplot(TextureClassPolygons, aes(sand, clay, fill=TextureClass)) + geom_polygon()

devtools::use_data(TextureClassPolygonsUSDA, overwrite = TRUE)



