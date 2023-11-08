

# USVI <- getRvcData(2017:2021, c("STTSTJ", "STX"))
library(gridExtra)
library(grid)
library(rvc)
library(tidyverse)

plot_density <- function(df, spp) {
  
  a <- getDomainDensity(df, spp) %>% 
        mutate( SE = sqrt(var),
                YEAR = as.factor(YEAR),
                REGION = as.factor(REGION)) %>% 
                left_join(select(df$taxonomic_data, c("SPECIES_CD", "COMNAME")))
  
  p   <-  ggplot(a, aes(YEAR, density, group = REGION, color = REGION)) +
            geom_point() +
            geom_line() +
            geom_errorbar(aes(ymin = density - SE, ymax = density + SE), width = 0.35) +
            ylab("Density") +
            xlab("Year")
  
  return(p)
  
}

plot_occurrence <- function(df, spp) {
  
  a <- getDomainOccurrence(df, spp) %>% 
    mutate( SE = sqrt(var),
            YEAR = as.factor(YEAR),
            REGION = as.factor(REGION)) %>% 
    left_join(select(df$taxonomic_data, c("SPECIES_CD", "COMNAME")))
  
  p   <-  ggplot(a, aes(YEAR, occurrence, group = REGION, color = REGION)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE), width = 0.35) +
    ylab("Occurrence") +
    xlab("Year")
  
  return(p)
  
}

plot_biomass <- function(df, spp) {
  
  a <- getDomainBiomass(df, spp) %>% 
    mutate( SE = sqrt(var),
            YEAR = as.factor(YEAR),
            REGION = as.factor(REGION)) %>% 
    left_join(select(df$taxonomic_data, c("SPECIES_CD", "COMNAME")))
  
  p   <-  ggplot(a, aes(YEAR, biomass, group = REGION, color = REGION)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = biomass - SE, ymax = biomass + SE), width = 0.35) +
    ylab("Biomass") +
    xlab("Year")
  
  return(p)
  
}

create_spp_page <- function(df, spp, title) {

  density <-    plot_density(df, spp)
  occurrence <- plot_occurrence(df, spp)
  biomass <-    plot_biomass(df, spp)
  
  page <- grid.arrange(
    density,
    occurrence,
    biomass,
    ncol = 1,
    top = textGrob(title,gp=gpar(fontsize=20,font=3))
  )
   
  return(page)
}
