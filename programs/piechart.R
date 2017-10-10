# pie chart
# Mariko Ohtsuka
# 2017/10/10
# package : sas7bdat

# Import library
library(sas7bdat)

# READ SAS Output Data
sasdatprt.path <- "//aronas/Stat/Trials"
sasdatads.path <- "JSH2017/ADS"
sasdat.nm <- "ads.sas7bdat"
sasdat.path <- paste(sasdatprt.path, sasdatads.path, sasdat.nm, sep="/")
sasdat <- read.sas7bdat(sasdat.path)
sasdat$wk.sum <- rep(1:1, nrow(sasdat))

# summary
wk.ds <- tapply(sasdat$wk.sum, list(sasdat$MHGRPTERM, sasdat$MHTERM), sum)
