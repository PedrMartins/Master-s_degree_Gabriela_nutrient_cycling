stats_bag <- function (x, nivel = c("time", "site", "color"), part  = "folha"){
  site <- as.data.frame(x)
  nivel <- match.arg(nivel)
  part <- switch (part,
                  folha = 7,
                  galho = 6
  )
  if (nivel == "time"){
    time <- unique (site$tempo)
    data <- data.frame()

    for (i in time){

      site_sub <- site [site$tempo==i,]
      part_data <- c (site_sub [,part])
      sub_data <- site_sub %>%
        summarise(nivel= paste (nivel, i, sep= "_"),
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

  } else if (nivel == "site"){
    site_loc <- unique (site$local)
    data <- data.frame()

    for (i in site_loc){
      site_sub <- site [site$local==i,]
      part_data <- c (site_sub [,part])
      sub_data <- site_sub %>%
        summarise(nivel= paste (nivel, i, sep= "_"),
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
  return(data)
}
