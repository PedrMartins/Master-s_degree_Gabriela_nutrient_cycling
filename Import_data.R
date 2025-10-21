library(readxl)
library(ggplot2)
Mestrado_Gabriela_Litter_Bags <- read_excel("Mestrado Gabriela G. de Matos - Litter Bags.xlsx")
Mestrado_Gabriela_Litter_Bags <- Mestrado_Gabriela_Litter_Bags [,-c(9:11)]
Mestrado_Gabriela_Litter_Bags <- na.omit(Mestrado_Gabriela_Litter_Bags)

names (Mestrado_Gabriela_Litter_Bags) [c(4:7)] <- c ("galho_g", "folha_g",
                                                     "perda_galho_percentage",
                                                     "perda_folha_percentage")



######anova one way######

 anova (aov (galho_g ~ cor,
             data = Mestrado_Gabriela_Litter_Bags)
        )
 anova (aov (folha_g ~ cor,
             data = Mestrado_Gabriela_Litter_Bags)
        )


 anova (aov (galho_g ~ local,
             data = Mestrado_Gabriela_Litter_Bags)
        )

 anova (aov (folha_g ~ local,data = Mestrado_Gabriela_Litter_Bags)
        )

 anova (aov (galho_g ~ tempo,
             data = Mestrado_Gabriela_Litter_Bags)
 )

 anova (aov (folha_g ~ tempo,data = Mestrado_Gabriela_Litter_Bags)
 )

######anova two way local cor####


 anova (aov (galho_g ~ local*cor,
             data = Mestrado_Gabriela_Litter_Bags)
 )

 anova (aov (folha_g ~ cor*local,
             data = Mestrado_Gabriela_Litter_Bags)
 )

############anova two way local tempo#############


 anova (aov (galho_g ~ local*tempo,
             data = Mestrado_Gabriela_Litter_Bags)
 )


 anova (aov (folha_g ~ local*tempo,
             data = Mestrado_Gabriela_Litter_Bags)
 )


#########################


is.na (Mestrado_Gabriela_Litter_Bags$galho_g )



 par  (mfrow = c(1,2))
 boxplot (folha_g ~tempo
          , data = Mestrado_Gabriela_Litter_Bags)
 boxplot (galho_g ~tempo
          , data = Mestrado_Gabriela_Litter_Bags)


 cores <-  c(rep ("blue",3),
             rep ("pink",3),
             rep ("green",3))

 par (mfrow = c(1,2), bty = "l")
 boxplot (galho_g ~ tempo + cor,
          data =Mestrado_Gabriela_Litter_Bags,
          col = cores, pch = "*", xaxt = "n")

 boxplot (folha_g ~ tempo + cor,
          data =Mestrado_Gabriela_Litter_Bags,
          col = cores, pch = "*", xaxt = "n")

dev.off ()

out=boxplot.stats(Mestrado_Gabriela_Litter_Bags$folha_g)
out <- c (out$out)
Mestrado_Gabriela_Litter_Bags_folha_s_outlier <- Mestrado_Gabriela_Litter_Bags[
  !Mestrado_Gabriela_Litter_Bags$folha_g %in% out,
]

boxplot.stats(Mestrado_Gabriela_Litter_Bags$galho_g)

par (mfrow = c(1,2), bty = "l")
boxplot (galho_g ~ local + tempo,
         data =Mestrado_Gabriela_Litter_Bags
         , pch = "*")

boxplot (folha_g ~ local + tempo,
         data =Mestrado_Gabriela_Litter_Bags_folha_s_outlier,
          pch = "*")
range()

dev.off ()

Mestrado_Gabriela_Litter_Bags_folha_na_free <- Mestrado_Gabriela_Litter_Bags [!is.na(
  Mestrado_Gabriela_Litter_Bags$folha_g),]
is.na(Mestrado_Gabriela_Litter_Bags_folha_na_free)

plot(folha_g~tempo,
      data=Mestrado_Gabriela_Litter_Bags_folha_na_free)




