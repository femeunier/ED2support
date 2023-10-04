rm(list = ls())

library(dplyr)
library(tibble)

load("bci.spptable.rdata")
load("bci.tree8.rdata")

################################################################################
# Functions

extremum <- function(vector, ...){
  return(c(min(vector, ...),max(vector, ...)))
}

patchnumber_from_position <- function(x,y,patch_X,patch_Y,X0 = 0,Y0 = 0){

  extr_x <- round(extremum(x,na.rm = TRUE))
  extr_y <- round(extremum(y,na.rm = TRUE))

  N_X <- ceiling((extr_x[2]-extr_x[1])/patch_X)
  N_Y <- ceiling((extr_y[2]-extr_y[1])/patch_Y)

  N_x_float <- (extr_x[2]-extr_x[1])/patch_X
  N_y_float <- (extr_y[2]-extr_y[1])/patch_Y

  x <- (x - X0)
  y <- (y - Y0)

  x[x < 0] <- x[x < 0] + N_x_float*patch_X
  y[y < 0] <- y[y < 0] + N_y_float*patch_Y

  N <- N_X*N_Y

  ix = 1 + (x - x%%patch_X)/patch_X
  iy = 1 + (y - y%%patch_Y)/patch_Y

  patch = (iy-1)*N_X + ix

  patch.size <- rep(patch_X*patch_Y,length(x))

  if (N_x_float != N_X) {
    patch.size[ix == N_X] <- (N_x_float - (N_X - 1))*patch.size[ix == N_X]
  }

  if (N_y_float != N_Y) {
    patch.size[iy == N_Y] <- (N_y_float - (N_Y - 1))*patch.size[iy == N_Y]
  }

  return(list(patch = patch,
              patch_X = ix,
              patch_Y = iy,
              patch_size = patch.size))
}

##########################################################################

# Database of Wood density in Panama
WD.Panama <- read.csv("./WD.DB.csv") %>% mutate(Latin = paste(genus,species),
                                                WD = WD..g.cm3.) %>% dplyr::select(Latin,WD)

# Merge species on BCI with WD database
species.WD <- bci.spptable %>% dplyr::select(sp,Latin) %>% ungroup() %>% left_join(WD.Panama %>% ungroup(),
                                                                                            by = "Latin") %>% group_by(sp) %>%
  summarise(Latin = Latin[1],
            WD = mean(WD))

# Replace unobserved WD with average
species.WD.NA <- species.WD %>%
  mutate(WD.type = case_when(is.na(WD) ~ "Average",
                             TRUE ~ "DB"),
         WD = case_when(is.na(WD) ~ mean(species.WD$WD,na.rm = TRUE),
                        TRUE ~ WD))

# BCI census
tree.BCI.WD <- bci.tree8 %>% filter(!is.na(dbh),
                                   dbh > 0,
                                   gx <= 1000,gx >= 0,
                                   gy <= 500,gx >= 0) %>% left_join(species.WD.NA,
                                                                    by = "sp") %>%
  mutate(WD.type = case_when(is.na(WD) ~ "Average2",
                             TRUE ~ WD.type),
         WD = case_when(is.na(WD) ~ mean(species.WD$WD,na.rm = TRUE),
                        TRUE ~ WD))

# Grid the 50 ha into small (20x20) patches
Delta_X = Delta_Y = 20
patches <- patchnumber_from_position(tree.BCI.WD$gx,
                                     tree.BCI.WD$gy,
                                     patch_X = Delta_X,
                                     patch_Y = Delta_Y)

trees <- tree.BCI.WD %>%
  ungroup() %>%
  mutate(patch = patches$patch,
         patch_x = patches$patch_X,
         patch_y = patches$patch_Y) %>%
  dplyr::select(treeID,patch,dbh,gx,gy,WD)


# Classify the trees into PFTs
WD.low = 0.5 ; WD.high = 0.7
b1ht_tree = 0.0352 ; b2ht_tree = 0.694 ; hmax_tree = 35 ; href_tree = 61.7 # Tree
cohorts <- trees %>% mutate(pft = case_when(WD < WD.low ~ 2,
                                            WD > WD.high ~ 4,
                                            TRUE ~ 3)) %>%
  mutate(h = pmin(hmax_tree,href_tree*(1-exp(-b1ht_tree*(dbh**b2ht_tree))))) %>%
  arrange(patch) %>%
  mutate(time = 2015,
         n = 1/(Delta_X*Delta_Y),
         bdead = 0,
         balive = 0,
         lai = 0,
         tag = 1:nrow(trees)) %>%
  dplyr::select(time,patch,tag,dbh,h,pft,n,bdead,balive,lai) %>%
  ungroup() %>%
  rename(cohort = tag,
         hite = h)

cohorts <- cohorts %>% dplyr::filter(patch == 1)

# Write cohort file
csspssfile_name_post <- ".lat9.000lon-79.000"
write.table(x = as_tibble(cohorts %>% mutate(patch = as.integer(patch),
                                             cohort = as.integer(cohort))),
            file = file.path(".",paste0("BCI",
                                        csspssfile_name_post,".css")),
            row.names = FALSE,
            col.names = TRUE)

# Soil C par
FSC <- 0.15 ; STSC <- 5 ; SSC <- 4.546
patches <- data.frame(time = 2015L,
                      patch = as.integer(unique(cohorts$patch)),
                      trk = 2,
                      age = 0,
                      area = 1/length(unique(cohorts$patch)),
                      water = 0,fsc = FSC,stsc =  STSC, stsl = STSC,ssc = SSC, lai = 0, msn = 0,fsn = 0, nep = 0, gpp = 0, rh = 0)



write.table(x = as_tibble(patches),
            file = file.path(".",paste0("BCI",
                                        csspssfile_name_post,".pss")),
            row.names = FALSE,
            col.names = TRUE)
