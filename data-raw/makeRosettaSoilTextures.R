
avs <- read.delim('data-raw/rosettaav.txt', colClasses="character", header=F)[[1]]
avs <- matrix(avs, byrow = TRUE, ncol = 16)
avs <- as.data.frame(avs)
vals <- avs[c(1, 2, 3, 5, 7, 9, 11, 13, 15)]
errs <- avs[c(1, 2, 4, 6, 8, 10, 12, 16, 16)]
labels <- c("TextureClass", "N", "theta_r", "theta_s", "log_alpha", "log_n", "ln_Ks", "ln_sK0", "L")
names(vals) <- labels
names(errs) <- labels

vals$N <- as.integer(as.character(vals$N))
errs$N <- as.integer(as.character(errs$N))
for(name in labels[3:length(labels)]) {
  vals[[name]] <- as.numeric(as.character(vals[[name]]))
  errs[[name]] <- as.numeric(gsub(x=as.character(errs[[name]]), pattern="[()]", replacement = ""))
}

rosettaSoilClass <- cbind(reshape2::melt(vals, id.vars=c("TextureClass", "N")),
                          reshape2::melt(errs, id.vars=c("TextureClass", "N"),
                                         value.name="stdev")[-1:-3])

rosettaSoilClass <- rosettaSoilClass[order(rosettaSoilClass$TextureClass, rosettaSoilClass$variable),]

devtools::use_data(rosettaSoilClass, overwrite = TRUE)
