
#delete console ctrl + l or cat("\014")

#delete all the objects
rm(list=ls())

#running garbage collector
gc(reset=TRUE)

#clear plots
graphics.off()

dev.off()

#other possible ways
#if(!is.null(dev.list())) dev.off()
#while (dev.cur()>1) dev.off()
#if use RGL a lot, add:
#while (rgl.cur()) rgl.close()

# pkgs <- c("",  "")
# install.packages(pkgs)

install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("readxl")
install.packages("xlsx")
install.packages("Hmisc")
install.packages("LEGIT")
install.packages("XLConnectJars")
install.packages("rJava")
install.packages("wesanderson")
install.packages("ggthemes") 
install.packages("ggrepel")
install.packages("plotrix")
install.packages("lme4")
install.packages("fBasics")
install.packages("tseries")
install.packages("DescTools")
install.packages("nortest")
install.packages("caret")
install.packages("e1071")
install.packages("forecast")
install.packages("poibin")
install.packages("sgof")
install.packages("haven")
install.packages("car")
install.packages("agricolae")
install.packages("ggplot2")
install.packages("lattice")
install.packages("leaflet")
install.packages("rgl")
install.packages("highcharter")
install.packages("circlize")
install.packages("relaimpo")
install.packages("mice")
install.packages("semTools")
install.packages("lavaan")
install.packages("ggiraphExtra")
install.packages("devtools")
install.packages("ggiraph")
install.packages("plyr")
install.packages("vioplot")
install.packages("nlme")
install.packages("caret")
install.packages("glmnet")
install.packages("mitools")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("MASS")
install.packages("XLConnect")
install.packages("rJava")
install.packages("lme4")
install.packages("ggpubr")
install.packages("plyr")
install.packages("corrplot")
install.packages("factoextra")
install.packages("NbClust")
install.packages("cluster")
install.packages("magrittr")
install.packages("broom")
install.packages("foreign")
install.packages("skimr")
install.packages("summarytools")
install.packages("coda")
install.packages("VIM")
install.packages("missForest")
install.packages("ggvis")
install.packages("readogr")
install.packages("rgdal")
install.packages("choroplethr")
install.packages("maptools")
install.packages("gpclib")
install.packages("R6")
install.packages("DT")
install.packages("pwr")
install.packages("xgboost")
install.packages("relaimpo")
install.packages("ggmap")
install.packages("digest")
install.packages("glue")
install.packages("marmap")
install.packages("lattice")
install.packages("VIM")
install.packages("naniar")
install.packages("missMDA")
install.packages("Amelia")
install.packages("mice")
install.packages("missForest")
install.packages("FactoMineR")
install.packages("Tidyverse")
install.packages("MissMech")
install.packages("LittleMCAR")
install.packages("randomForest")  
install.packages("pdp")        
install.packages("vip")  
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("Rcpp")
install.packages("glmnet")
install.packages("magick")
install.packages("extrafont")
install.packages("caret") 
install.packages("parallelsugar")
install.packages("data.table")
install.packages("glmnetUtils")
install.packages("poolr")
install.packages("expss")
install.packages("jtools")
install.packages("ggstance")
install.packages("huxtable")
install.packages("flextable")
install.packages("sandwich")
install.packages("broom.mixed")
install.packages("ggforce")
install.packages("psych")
install.packages("lcmm")
install.packages("AER")
install.packages("flextable")
install.packages("interactions")
install.packages("plotly")
install.packages("ContourFunctions")
#install.packages("LEGIT")
install.packages("/Users/*/Downloads/margins", repos = NULL, type="source")
install.packages("margins")
install.packages("semPlot")
install.packages("pastecs")
install.packages("sjmisc")
install.packages("simpleboot")
install.packages("flextable")
install.packages("moderndive")
install.packages("infer")
install.packages("sig_regions")
install.packages("lmThresh")
install.packages("pequod")
install.packages("reghelper")
install.packages("plsRglm")
install.packages("pls")
install.packages("AlphaPart")

pkgs <- c("factoextra", "NbClust")
install.packages(pkgs)


# load packages into workspace
allPackages <- c("ggplot2", "lattice", "leaflet", "rgl", "highcharter", "circlize")
lapply(allPackages, library, character.only = TRUE)

lapply(allPackages, function(x) sprintf("%s version %s", x, packageVersion(x)))

pkgs <- c("semTools", "lavaan")
install.packages(pkgs)

allPackages <- c("semTools", "lavaan")
lapply(allPackages, library, character.only = TRUE)

devtools::install_github("simsem/semTools/semTools")

install.packages("relaimpo", dep=c("Depends"))  
 
#calc.relimp( MyRegressionModel, type = c("last", "first") )

library(Rcpp)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(xlsx)
library(Hmisc)
library(LEGIT)
library(MASS)
library(XLConnect)
library(rJava)
library(wesanderson)
library(ggthemes)
library(ggrepel) 
library(plotrix)
library(lme4)
library(fBasics)
library(tseries)
library(DescTools)
library(nortest)
library(caret)
library(e1071)
library(forecast)
library(poibin)
library(sgof)
library(haven)
library(car)
library(agricolae)
library(ggpubr)
library(ggplot2)
library(nlme)
library(ggiraphExtra)
library(devtools)
library(ggiraph)
library(plyr)
library(vioplot)
library(corrplot)
library(factoextra)
library(NbClust)
library(caret)
library(glmnet)
library(cluster)
library(magrittr)
library(broom)
library(foreign)
library(skimr)
library(summarytools)
library(ggplot2)
library(lattice)
library(leaflet)
library(rgl)
library(highcharter)
library(circlize)
library(relaimpo) 
library(semTools)
library(lavaan)
library(mice)
library(mitools)
library(semTools)
library(coda)
library(VIM)
library(missForest)
library(rgdal)
library(readogr)
library(ggvis)
library(DT)
library(choroplethr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(maptools)
library(gpclib)
library(readr)
library(R6)
library(raster)
library(maps)
library(mapdata)
library(ggmap)
library(digest)
library(glue)
library(marmap)
library(lattice)
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(pwr)
library(xgboost)
library(relaimpo)
library(naniar)
library(missMDA)
library(Amelia)
library(missForest)
library(FactoMineR)
library(MissMech)
library(LittleMCAR)
library(randomForest)  # for fitting random forests
library(pdp)           # for partial dependence plots
library(vip)           # for variable importance plots
library(sjPlot)
library(sjmisc)
library(magick)
library(extrafont)
font_import("Arial")
library(caret) #For cross-validation
library(parallelsugar) #For parallel computations
library(data.table)
library(glmnetUtils) #Elastic net
library(poolr) # Load poolr library for eigenvalue method
library(expss)
library(jtools)
library(ggstance)
library(huxtable)
library(flextable)
library(sandwich)
library(broom.mixed)
library(ggforce)
library(psych)
library(lcmm)
library(AER)
library(flextable)
library(interactions)
library(plotly)
library(ContourFunctions)
library(margins)
library(semPlot)
library(pastecs)
library(sjmisc)
library(simpleboot)
library(flextable)
library(moderndive)
library(infer)
library(sig_regions)
library(lmThresh)
library(pequod)
library(reghelper)
library(plsRglm)
library(pls)
library(AlphaPart)
update.packages(checkBuilt=TRUE, ask=FALSE)

data(iris)
data(nottem)
data(mtcars)
