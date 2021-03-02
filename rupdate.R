#---------------------

library(devtools)
install_github('andreacirilloac/updateR')
library(updateR)
updateR()

#----------------------

packages <- c("tidyverse", "shiny", "maps", "sf", "spData", "lubridate")

for (i in packages) {
  install.packages(i)
}

list <- installed.packages()
