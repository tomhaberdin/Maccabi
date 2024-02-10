
library(shinylive)
## the cars folder contains last weeks shiny app (bit.ly/TidyX_Ep160)
## Convert your shiny app into all the assets for running the app 
## in the browser
shinylive::export(
  appdir = "Maccabi",
  destdir = "docs"
)


## with development version of httpuv, run shinylive app locally
## remotes::install_github("rstudio/httpuv")
httpuv::runStaticServer(
  dir = "docs", 
  port = 8888
)