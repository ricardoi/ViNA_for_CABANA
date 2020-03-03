# CABANA Workshop, San Jose Costa Rica
# Virome Network Analysis (ViNA)
# ViNA framework adapoted for VirusDetect 
# Code developed by RI Alcala at Garrett lab | University of Florida
# contact: ralcala@ufl.edu
# 
#
#
# R version 3.6.2 
#------------------------ SET UP ENVIRONMENT ------------------------ 
#------ packages #uncomment to install -----
# Install Libraries:
# install.packages("tidyr")
# install.packages("igraph")
# install.packages("bipartite")
# install.packages("XML")
# install.packages("htmltab")

# load libraries
library(plyr)
library(tidyverse)
library(tidyr)
library(igraph)
library(bipartite)
library(XML)
library(htmltab)

#---- Parsing function -----
dat.parse <- function(df, location, host){
  # Formatting metadata
  df$sampleID <- location
  df$host <- host
  # ReGex and residuals removal  
  names <- str_extract(df$Description, ".*virus[:space:][:alnum:]")
  a <- gsub("virus i", "virus", names)
  a <- gsub("virus s", "virus", a)
  # if there are more 'CHARACTER' after virus, add another line as follows 
  #   a <- gsub("virus s", "virus", 'CHARACTER')
  df$Species <- gsub("virus p", "virus", a)
  # Generating pseudo-acronyms
  df$acronym <- abbreviate(df$Species)
  print(df)
}

#----- setwd -----
setwd("ADD_YOUR_PATH")

#------------------------ DATA MANIPULATION ------------------------

#---- Loading data from Virus Detect  -----
loaded_files <- list(
vd.re1 <- readHTMLTable('example_blastn.html'),
vd.re2 <- readHTMLTable('example_blastn.html'),
#complete
)

loaded_files[[1]] # change this number to check other inputs [['n']]

#--- Formatting as tibble and numbers  -----
results.ls = temp = list(NULL) 
  for (k in seq_along(loaded_files)){
    temp[[k]] <- as_tibble(loaded_files[[k]]$`NULL`[1:dim(loaded_files[[k]]$`NULL`)[2]])
    results.ls[[k]] <- temp[[k]]  %>%
      mutate(`Depth (Norm)` = as.numeric(as.character(`Depth (Norm)`)) )
    }

#--- Adding metadata to Virus Detect Results  ----- 
host <- rep("potato", 2) # add the number of samples to study
locations <- c("Location1", "Location2") # add the name of the different locations
# LOCATION KEY
# CZO= Cuzco
# HUA= HUANCAVELICA
# ICA=ICA
# JIN= JUNIN
# CCA=CAJAMARCA

#--- Generating final data ----- 
res <- list(NULL)
for (i in seq_along(results.ls)){
  print(i)
res[[i]] <- dat.parse(results.ls[[i]], locations[i], host[i])
}
# Create a data frame with all individual information
df <- na.omit(as_tibble(rbind.fill(res)))

#----- Create summary data and incidence matrix ----- 
dfs <- ddply(df, .(sampleID, acronym), summarise, cov = mean(`Depth (Norm)`))
dfs <- t(spread(dfs, sampleID, cov,  drop=TRUE , fill = 0))
in.mat = t(apply(dfs[2:3,], 1, as.numeric))
colnames(in.mat) <- dfs[1,]

#--- incidence matrix results 
in.mat

#------------------------ IGRAPH ------------------------
#---- Generating graph object  ----- 
g <- graph.incidence(in.mat)

# Network attributes
V(g)$name # Check the vertex names 
V(g)$type # Check vertex types 

# plotting
plot(g, vertex.label.color='black', vertex.label.dist= ((V(g)$type * 4)-2)*-1  , layout = layout_as_bipartite)
plot(g,  vertex.label.color='black')

#---- Adding attributes
# Shapes to nodes
shapes = c(rep("square", length(V(g)$type[V(g)$type == "FALSE"])), 
           rep("circle", length(V(g)$type[V(g)$type == "TRUE"])))
# plotting
plot(g, vertex.shape=shapes, vertex.label.color='black')

#------------------------
# Add colors to nodes

V(g)$color <- c(rep("#046A38", length(V(g)$type[V(g)$type == "FALSE"])), 
                rep("#FA4616", length(V(g)$type[V(g)$type == "TRUE"])))

# plotting
plot(g, vertex.shape=shapes, vertex.label.color='black')

#------------------------ BIPARTITE ------------------------ 
#library(bipartite)
# Plot bipartite network using bipartite package
# data in incidence matrix format
plotweb(sortweb(in.mat, sort.order="inc"), method="normal")

# Plot in matrix format
visweb(sortweb(in.mat, sort.order="dec"), type= "none", # change to nested or diagonal 
       labsize= 2, square= "interaction", text= "none", textsize= 4)

#----- Calculating metrics ----
## Node metrics
node.metrics <- specieslevel(round(in.mat))

# Exploring metrics
str(node.metrics)
# How many levels are in the list?

# node.metrics$`higher level` # Want to know about the metrics? Call ?specieslevel

# Exploring $`higher level`
(h.nd <- node.metrics$`higher level`[1]) # node degree OR node.metrics$`higher level`$degree
(h.bc <- node.metrics$`higher level`[2]) # species strength 

# Exploring $`lower level`
(l.nd <- node.metrics$`lower level`[1]) # node degree
(l.bc <- node.metrics$`lower level`[2]) # species strength 


## Network metrics
network.metrics <-  networklevel(in.mat)
# network.metrics # Want to know about the metrics? Call ?networklevel

# Exploring by metric
network.metrics["connectance"] # Connectance
network.metrics["weighted nestedness"] # Nestedness *weighted

# Computing modularity
computeModules(in.mat) # Default method: Becket
(modularity <-  LPA_wb_plus(in.mat)) 

mod <- convert2moduleWeb(in.mat, modularity)
plotModuleWeb(mod, weighted = F)

#----Plotting with attributes ----
# Selecting metadata from Virus Detect
meta <- df %>% select(Genus, host, Species, acronym) %>% 
                unique()

str(meta)

# Adding colors
meta$colors <- ifelse( meta$Genus == "badnavirus", "#FA4616", 
                      ifelse( meta$Genus == 'begomovirus', "yellow", 
                              ifelse( meta$Genus == "cavemovirus", "brown",
                                      ifelse( meta$Genus == "mastrevirus", "orange",
                                              ifelse( meta$Genus == "potyvirus", "green", "purple")))))

# Combining node attributes from bipartite and igraph 
V(g)$color <- c(rep("#046A38", length(V(g)$type[V(g)$type == "FALSE"])),
                meta$colors)

V(g)$xx <- c(unlist(l.nd), unlist(h.bc)+5) # adding node degree + species strength plus a constant

# Types of layout algorithms
#default: Kamada-Kawai 
plot(g, vertex.shape=shapes, vertex.label=NA, vertex.size= as.numeric(V(g)$xx))
# Davidson-Harel
plot(g, vertex.shape=shapes, vertex.size= as.numeric(V(g)$xx), layout=layout_with_dh(g) )
# Distributed Recursive
plot(g, vertex.shape=shapes, vertex.size= as.numeric(V(g)$xx), layout=layout_with_drl(g) )

