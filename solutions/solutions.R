library(rvc)
library(tidyverse)
library(broom)


drto_psu_density <- read_csv("data/drto_psu_denisty.csv")


prot_significance_dens <- drto_psu_density %>% 
  group_by(SPECIES_CD) %>% 
  nest() %>% 
  mutate(exp_tests = map(.x=data, ~kruskal.test(density~PROT, data=.) %>% 
                        tidy())) %>% 
  unnest(exp_tests) %>% 
  mutate(p.exp = p.adjust(p.value, method = "BH")) %>% 
  select(SPECIES_CD, data, p.exp) %>% 
  filter(p.exp < 0.05)

prot_pairwise_dens <- prot_significance_dens %>% 
  mutate(pairwise  = map(.x= data,
                         ~pairwise.wilcox.test(x = .x$density,
                                               g = .x$PROT,
                                               p.adjust.method = "BH") %>% 
                           tidy())) %>% 
  unnest(pairwise) %>% 
  filter(p.value < 0.05)
