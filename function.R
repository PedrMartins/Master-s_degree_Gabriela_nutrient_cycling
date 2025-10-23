stats_bag <- function (x, nivel = c("site", "color"), part  = "folha"){
  site <- as.data.frame(x)
  nivel <- match.arg(nivel)
  part_n <- switch (part,
                  folha = 7,
                  galho = 6
  )
  if (nivel == "site"){
    site_loc <- unique (site$local)
    data <- data.frame()

    for (i in site_loc){
      site_sub <- site [site$local==i,]
      parcel <- unique(site_sub$parcela)

      for (j in parcel){

        site_sub_parcela <- site_sub [site_sub$parcela==j,]
        time <- unique(site$tempo)

        for (v in time) {
          site_sub_parcela_tempo <- site_sub_parcela [site_sub_parcela$tempo==v,]
          part_data <- c (site_sub_parcela_tempo [,part_n])
          sub_data <- site_sub_parcela_tempo %>%
            summarise(parte= part,
                      nivel= paste (nivel, i, sep= "_"),
                      tempo = v,
                      parcel = j,
                      mean=mean(part_data,
                                na.rm = TRUE ),
                      SD=sd(part_data,
                            na.rm = T),
                      N=length(
                        na.omit(part_data)
                      )
            )
          data <-  rbind(sub_data, data)

        }

      }

    }

  }
  else {
    site_cor <- unique (site$cor)
    data <- data.frame()

    for (i in site_cor){
      site_sub <- site [site$cor==i,]
      time <- unique(site_sub$tempo)

      for (j in time){
        site_sub_tempo <- site_sub [site_sub$tempo==j,]
        part_data <- c (site_sub_tempo [,part_n])
        sub_data <- site_sub_tempo %>%
          summarise(parte= part,
                    nivel= paste (nivel, i, sep= "_"),
                    tempo = j,
                    mean=mean(part_data,
                              na.rm = TRUE ),
                    SD=sd(part_data,
                          na.rm = T),
                    N=length(
                      na.omit(part_data)
                    )
          )
        data <-  rbind(sub_data, data)
      }

    }

  }
  return(data)
}

