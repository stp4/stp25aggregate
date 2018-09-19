
#  Recast2
#  usethis::use_test("Recast2")
 
 require(stp25aggregate)
 
 
 

 library(stp25data)

 #library(tidyverse)
 #library(stp25aggregate)
 #

 hyper1<-hyper[, c("g","chol0","chol1","chol6","chol12")]
 hyper_long<- Melt2(hyper1, id.vars=1)
 aggregate( value~variable, hyper_long, mean)

 hyper_long<-Melt2(chol0+chol1+chol6+chol12~g,
                   hyper, "Zeit", "Cholesterin")
 # #-- Spread + aggragate
 # aggregate(Cholesterin~Zeit+g, hyper_long, mean) %>%
 #   spread(g, Cholesterin)
 # 
 # #- das gleiche wie aggragate
 # hyper_long %>% group_by(Zeit, g) %>%
 #   summarise(Cholesterin=mean(Cholesterin)) %>%
 #   spread(g, Cholesterin)

 #-- Gather das gleiche wie oben aber ohne die Labels
 # hyper  %>%
 #   tidyr::gather("time", "chol", chol0:chol12) %>%
 #   dplyr::select(g, time, chol)

 #-- Recast2 das gleiche wie oben
 Recast2(chol0+chol1+chol6+chol12~g, hyper, mean,
         "Zeit", "Cholesterin") %>%
   spread(g, Cholesterin)

 #-- Recast2 das gleiche wie oben nur ohne  spread
 Recast2(chol0+chol1+chol6+chol12~g, hyper, mean,
         formula=variable~g)


 Recast2(chol0+chol1+chol6+chol12~g, hyper,
         mean,formula=variable~g, margins=TRUE)
 # 