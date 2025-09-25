##################################################################################################################
#                                     Mammal networks and landscape structure
##################################################################################################################

#FMestre
#25-09-2025

# 0. Config -------------------------------------------------------

source("config.R")

# 1. Load data ----------------------------------------------------

metaweb <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metaweb_brasil.csv", sep=";")
site_metrics <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metricas_paisagem.csv", sep = ";")
site_species <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\sites_species.csv", sep = ";")

rownames(metaweb) <- metaweb$X
metaweb <- metaweb[,-1]

View(metaweb)
View(site_metrics)
View(site_species)

# 2. Create local networks ----------------------------------------

sites_names <- site_metrics$paisagem
local_networks_list <- list()

for(i in 1:length(sites_names)){
  
  site1 <- sites_names[i]
  species1 <- site_species[site_species$paisagem == site1,]
local_matrix1 <- matrix(0, ncol=nrow(species1), nrow=nrow(species1))
  colnames(local_matrix1) <- species1$especie
  rownames(local_matrix1) <- species1$especie
        for(j in 1:nrow(local_matrix1)){
          #getting the preys for 'predator1' from the metaweb
          predator1 <- species1$especie[j]
          preys1 <- metaweb[predator1,]
          #select columns with value 1
          preys1 <- colnames(preys1)[as.vector(preys1) == 1]
          preys1 <- preys1[!is.na(preys1)]
          #keeping only the preys that are present in the site
          preys_in_site <- preys1[preys1 %in% species1$especie]
          #filling the local matrix
          local_matrix1[j, colnames(local_matrix1) %in% preys_in_site] <- 1
          #local_matrix1[j, !(colnames(local_matrix1) %in% preys_in_site)] <- 0
          
        }
  #Sendo to list of local matrices
  local_networks_list[[site1]] <- local_matrix1
  
  message(site1)
          
}

#length(local_networks_list)
  




# 3. Derive network metrics ----------------------------------------

#
