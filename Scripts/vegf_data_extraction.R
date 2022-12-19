library(tidyverse)
library(metafor)
library(meta)
library(ggplot2)

## Data sheet containing extracted data from each trial 

vegf <- readRDS("Scratch_data/vegf.Rds")

## Meta-analysis for whole population ----

## Cardiorenal outcomes
# Hypertension

htn_mod <- metabin(event.e = htn_act,
                   n.e = n_act,
                   event.c = htn_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   fixed = TRUE,
                   random = FALSE,
                   data = vegf %>% filter(!is.na(htn_act)))
summary(htn_mod)

pdf("Outputs/htn_plot.pdf", width = 10, height = 14)
forest(htn_mod, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

pdf("Outputs/htn_funnel.pdf")
htn_mod2 <- trimfill(htn_mod)
funnel(htn_mod2)
dev.off()

# Heart failure

hf_mod <- metabin(event.e = newhf_act,
                   n.e = n_act,
                   event.c = newhf_cont,
                   n.c = n_cont,  
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   fixed = TRUE,
                   random = FALSE,
                   data = vegf %>% filter(!is.na(newhf_act)))
summary(hf_mod)

pdf("Outputs/hf_plot.pdf", width = 10, height = 14)
forest(hf_mod, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

pdf("Outputs/hf_funnel.pdf")
hf_mod2 <- trimfill(hf_mod)
funnel(hf_mod2)
dev.off()

# Proteinuria

pcr_mod <- metabin(event.e = pcr_act,
                   n.e = n_act,
                   event.c = pcr_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   fixed = TRUE,
                   random = FALSE,
                   data = vegf %>% filter(!is.na(pcr_act)))
summary(pcr_mod)

pdf("Outputs/pcr_plot.pdf", width = 10, height = 14)
forest(pcr_mod, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

pdf("Outputs/pcr_funnel.pdf") # cannot run - needs min 3 studies
funnel(pcr_mod)
trimfill(pcr_mod)
dev.off()

#  eGFR decline/CKD

ckd_mod <- metabin(event.e = ckd_act,
                   n.e = n_act,
                   event.c = ckd_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   fixed = TRUE,
                   random = FALSE,
                   data = vegf %>% filter(!is.na(ckd_act)))
summary(ckd_mod)

pdf("Outputs/ckd_plot.pdf", width = 10, height = 14)
forest(ckd_mod, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

pdf("Outputs/ckd_funnel.pdf")
ckd_mod2 <- trimfill(ckd_mod)
funnel(ckd_mod2)
dev.off() 

## Cardiovascular events and death

# Cardovascular events

cve_mod <- metabin(event.e = cve_act,
                   n.e = n_act,
                   event.c = cve_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   fixed = TRUE,
                   random = FALSE,
                   data = vegf %>% filter(!is.na(cve_act)))
summary(cve_mod)

pdf("Outputs/cve_plot.pdf", width = 10, height = 14)
forest(cve_mod, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

pdf("Outputs/cve_funnel.pdf")
cve_mod2 <- trimfill(cve_mod)
funnel(cve_mod2)
dev.off()

# Death

dead_mod <- metabin(event.e = dead_act,
                   n.e = n_act,
                   event.c = dead_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   fixed = TRUE,
                   random = FALSE,
                   data = vegf %>% filter(!is.na(dead_act)))
summary(dead_mod)

pdf("Outputs/dead_plot.pdf", width = 10, height = 14)
forest(dead_mod, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

pdf("Outputs/dead_funnel.pdf")
dead_mod2 <- trimfill(dead_mod)
funnel(dead_mod2)
dev.off()

## Meta-analysis only in diabetic populations ----

## Cardiorenal

# Hypertension

htn_mod_dm <- metabin(event.e = htn_act,
                   n.e = n_act,
                   event.c = htn_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup.name = FALSE,
                   data = vegf %>% filter(!is.na(htn_act) & dm_trial == "Diabetic"))
summary(htn_mod_dm)

pdf("Outputs/htn_plot_dm.pdf", width = 10, height = 14)
forest(htn_mod_dm, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

funnel(htn_mod_dm)
trimfill(htn_mod_dm)

# Heart failure 

hf_mod_dm <- metabin(event.e = newhf_act,
                  n.e = n_act,
                  event.c = newhf_cont,
                  n.c = n_cont, 
                  subgroup = dm_trial, 
                  print.subgroup = FALSE,
                  data = vegf %>% filter(!is.na(newhf_act) & dm_trial == "Diabetic"))
summary(hf_mod_dm)

pdf("Outputs/hf_plot_dm.pdf", width = 10, height = 14)
forest(hf_mod_dm, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

funnel(hf_mod_dm)
trimfill(hf_mod_dm)

# Proteinuria

pcr_mod_dm <- metabin(event.e = pcr_act,
                   n.e = n_act,
                   event.c = pcr_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   data = vegf %>% filter(!is.na(pcr_act) & dm_trial == "Diabetic"))
summary(pcr_mod_dm)

pdf("Outputs/pcr_plot_dm.pdf", width = 10, height = 14)
forest(pcr_mod_dm, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

funnel(pcr_mod_dm)
trimfill(pcr_mod_dm)

# eGFR decline/CKD

ckd_mod_dm <- metabin(event.e = ckd_act,
                   n.e = n_act,
                   event.c = ckd_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   data = vegf %>% filter(!is.na(ckd_act) & dm_trial == "Diabetic"))
summary(ckd_mod_dm)

pdf("Outputs/ckd_plot_dm.pdf", width = 10, height = 14)
forest(ckd_mod_dm, studlab = paste(author, yr), sortvar = TE, comb.fixed = TRUE, comb.random = FALSE)
dev.off()

funnel(ckd_mod_dm)
trimfill(ckd_mod_dm)

## Cardiovascular events and death

# Cardiovascualr events

cve_mod_dm <- metabin(event.e = cve_act,
                   n.e = n_act,
                   event.c = cve_cont,
                   n.c = n_cont, 
                   subgroup = dm_trial, 
                   print.subgroup = FALSE,
                   data = vegf %>% filter(!is.na(cve_act) & dm_trial == "Diabetic"))
summary(cve_mod_dm)

forest(cve_mod_dm, studlab = paste(author, yr), sortvar = TE)
funnel(cve_mod_dm)
trimfill(cve_mod_dm)

# Death

dead_mod_dm <- metabin(event.e = dead_act,
                    n.e = n_act,
                    event.c = dead_cont,
                    n.c = n_cont, 
                    subgroup = dm_trial, 
                    print.subgroup = FALSE,
                    data = vegf %>% filter(!is.na(dead_act) & dm_trial == "Diabetic"))
summary(dead_mod_dm)
forest(dead_mod_dm, studlab = paste(author, yr), sortvar = TE)
funnel(dead_mod_dm)
trimfill(dead_mod_dm)

## Meta-regression for factors associated with death ----

# Diabetes trial significant
metareg(dead_mod, ~no_inj + durn_rx + vegf_name + dm_trial, method.tau="REML", control = list(verbose = FALSE), digits = 3) 
dead_mod

# Create bubble plot to show relationship between diabetes trial and death

dead_dm = metareg(dead_mod, dm_trial) 
bubble(dead_dm, ylim = c(0.1,10))



