# Library -----------------------------------------------------------------
# rm(list=ls()) # Clear Global Environment

library(ggplot2)
library(dplyr)
library(igraph)
library(ggridges)
library(readxl)
library(patchwork)
library(tidyverse)
library(networktools)
library(cluster)
library(cowplot)

# Script Outline ----------------------------------------------------------
#' Throughout the script you will find a lot of comments and notes - please read carefully to make sure the analysis is as accurate to your network as possible. 
#'
#'
#' 1 - 5 : Original Network extrapolation and Analyis
#' 6 - 10 : Restoration Work Targeted
#' 11 - 13 : Restoration Work Random

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/----
# ORIGINAL NETWORK --------------------------------------------------------
# 1. DATA IMPORT AND VISUALISATION  -----------------------------------------
## 1.1 Patch data  ---------------------------------------------------------
#' Import data frame with patch, area_m2, longitude, latitude, cluster(or group - if patches are divided by predetermined clusters or groups)
#' NOTE: Longitude and Latitude are converted into values expressing meters 
df_patch_og <- read.csv("your_path_here.csv")



# Visualise original nodes
# If no clustering is present the color metric and scale can be omitted
# Change group1 and group2 depending on the grouping you have in the group factor
ggplot(data=df_patch_og, 
       aes(x=longitude, y=latitude, size=area_m2, col=group)) +
  geom_point() +
  geom_text(aes(label=patch), hjust=-0.1, vjust=-0.1, col="darkgrey", size=4) +
  coord_fixed() +
  scale_color_manual(values = c("group1" = "#56B4E9", "group2" = "#FF4000")) + 
  labs(size="Patch area\n(m2)") +
  theme_bw() +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_rect(fill="white", colour="grey"),
        panel.grid.major=element_line(colour="grey"),
        legend.position="bottom")



## 1.2 Organism data -------------------------------------------------------
#' This section is pretty targeted - need to use theta for the exponential curve to calculate mean distances - skip to 3. if you already have the distances you want to use for the network construction.
#' You might want a dataframe that has your species to analyse and a thetha value to compute distribution kernel
#' Depending on what type of analysis - specific species vs group of organism you might also want a column that states which group they belong to 
#' In this study we have calculated means for GROUPS, not individuals

df_species_original <- read_excel("your_excel_here")
str(df_species_original)

# View(df_species_original)
str(df_species_original)
head(df_species_original)

# Check which group you have
unique(df_species_original$group)


# 2. ORGANISM DISTRIBUTION ANALYSIS ---------------------------------------
## 2.1 Functions ------------------------------------------------------------
### 2.1.1 Exponential dispersal probability function ---------------------------
#' Negative exponential function for kernel construction
#' Adapted from Bullock et al. (2016) - Synthesis of empirical plant dispersal kernels
#' a = theta value 
#' d = distance
exp_dens_fun <- function(a, d) {
  (a^2/(2* pi)) * exp(- a * d)
}


### 2.1.1 Dispersal density calculation ---------------------------------------
#' NOTE: this function uses specific column names such as "group" and "theta_per_m". Format df accordingly.
#' NOTE: calculations are done for the mean metric value of a group of organisms, not each organism separately 
#' d = distance, to be specified outside the function depending on species range (this is an educated guess)

density_fun <- function(df, d) {
  org_groups <- unique(df$group)
  res_list <- lapply(org_groups, function(x) {
    mean_theta <- mean(df$theta_per_m[df$group == x], na.rm = TRUE)
    dens <- exp_dens_fun(mean_theta, d)
    data.frame(
      group = x,
      distance = d,
      dens = dens
    )
  })
  
  result_df <- do.call(rbind, res_list)
  rownames(result_df) <- NULL
  return(result_df)
}

## 2.2 Application & Extraction  -------------------------------------------
#### __ Kernel Creation -------------------------------------------------------
# Set range of distances - educated guess 
d_range <- seq(0, 3000, by = 1)
df_dens_species <- density_fun(df_species_original, d_range)

head(df_dens_species)
# View(df_dens_species)  



#### __ Visualisation  ---------------------------------------------------------
# Trim for visualisation --> exponential will never reach 0, so we cut close enough to 0
threshold <- 1e-10 

df_dens_species_trim <- df_dens_species %>%
  group_by(group) %>%
  filter(dens > threshold)

disp_kern <- ggplot(df_dens_species_trim, aes(x = distance, y = dens, colour = group)) +
  geom_line()+
  facet_wrap(~group, scale = "free")+
  labs(
    title = "Exponential Distribution Kernel",
    x = "Distance",
    y = ""
  ) +
  theme_bw()

disp_kern

#### __ Distance values --------------------------------------------------------
df_dist_stat <- df_dens_species %>%
  group_by(group) %>%
  arrange(distance) %>%
  mutate(     # to get probabilities and threshold keeping in mind areas under the curve (sum then = 1)
    prob = dens / sum(dens),        
    cum_prob = cumsum(prob)     
  ) %>%
  summarise(
    mean_distance_m   = sum(distance * prob),                 
    median_distance_m = distance[which.min(abs(cum_prob - 0.5))],  
    q25_distance_m    = distance[which.min(abs(cum_prob - 0.25))], 
    q75_distance_m    = distance[which.min(abs(cum_prob - 0.75))], 
    q95_distance_m    = distance[which.min(abs(cum_prob - 0.95))]  
  )

# add mean theta values for each group
df_dist_stat <- df_dist_stat %>%
  left_join(df_species_original %>%
              group_by(group) %>%
              summarise(mean_theta = mean(theta_per_m, na.rm = TRUE)))

## 2.3 Final Distribution Objects --------------------------------------------
disp_kern
df_dist_stat


# 3. EUCLIDEAN NETWORK FUNCTION ------------------------------------------
#' This function only need the original patch DF and the preferred maximum distance at which the edges will be created 
#' Generally speaking, the function can be modified slightly depending on needs (see comments)
#' NOTE: this created links edge to edge and not centroid to centroid (see dorset_n2n_cm for centroid to centroid).
#' NOTE: There are certain objects and passages within the function that have been commented - can be uncommented if they are needed
#' NOTE: the function cluster_infomap() used here is specifically selected for undirected, disconnected graphs - can be switched to different clustering functions (see igraph package)

euclidean_network_e2e <- function(d, df_patch) {
  # matrix of distances between all pairs of patches
  mat_dist <- as.matrix(dist(select(df_patch, longitude,latitude), method="euclidean", diag=TRUE, upper=TRUE))
  
  # Calculate and extrapolates the radius of individual patches
  df_patch$radius <- sqrt(df_patch$area_m2 / pi)
  radius_list <- select(df_patch, radius)
  
  
  # creation a new distance matrix considering edge to edge distance
  e2e_mat_dist <- mat_dist
  e2e_mat_dist[] <- 0 
  
  for (i in 1:nrow(mat_dist)) {
    for (j in 1:ncol(mat_dist)) {
      e2e_mat_dist[i, j] <- mat_dist[i, j] - (radius_list$radius[i] + radius_list$radius[j])
    }
  }
  
  # Make sure diagonal and negative values are set to 0 
  diag(e2e_mat_dist) <- 0 
  # sum(is.na(e2e_mat_dist))
  # sum(e2e_mat_dist < 0)
  e2e_mat_dist[e2e_mat_dist < 0] <- 0
  # sum(e2e_mat_dist == 0.1)
  
  # adjacency matrix - 1 = link between patch i and j; 0 = no link between patch i and j
  # assign 1 if distance between patch i and j is less or equal to threshold distance
  # assign 0 if distance between patch i and j is greater than threshold distance
  m_adj <- e2e_mat_dist
  m_adj[e2e_mat_dist <= d] <- 1
  m_adj[e2e_mat_dist > d] <- 0
  diag(m_adj) <- 0 # set diagonal to 0
  # sum(m_adj == 1)
  # sum(m_adj == 0)
  # sum(diag(m_adj))
  
  
  # # Degree (number of connections) of each patch, uncomment if you want function to create preliminary graphs
  # deg = rowSums(m_adj)
  # barplot(deg, main = paste("Degree distribution, d =", d))
  # hist(deg, main = paste("Degree distribution, d =", d))
  
  # Connectance (number of realised links / number of all possible links)
  n = nrow(m_adj) # number of patches
  c = sum(m_adj) / (n*(n-1))
  
  # create dataframe with connections between patches
  edge_index <- which(m_adj==1, arr.ind=TRUE)
  patch_names <- df_patch$patch
  edges <- data.frame(from = patch_names[edge_index[, "row"]],
                      to = patch_names[edge_index[, "col"]]) %>%
    rowwise() %>%
    mutate(a = min(from, to),
           b = max(from, to)) %>%
    ungroup() %>%
    select(from = a, to = b) %>%
    distinct()
  
  # rename site to name (for igraph)
  nodes <- df_patch %>% rename(name = patch)
  
  # create igraph object
  g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
  
  
  # # plot igraph object (Uncomment if you want the function to create preliminary graph)
  # # define node coordinates (igraph layout)
  # layout_coords <- layout_coords <- nodes %>%
  #   select(name, longitude, latitude) %>%
  #   filter(name %in% V(g)$name) %>% 
  #   arrange(match(name, V(g)$name)) %>%
  #   select(longitude, latitude) %>%
  #   as.matrix()
  
  # plot(g,
  #      layout = layout_coords,
  #      vertex.size = sqrt(V(g)$area_m2)/200,
  #      vertex.label = V(g)$name,
  #      vertex.color = "#56B4E9",
  #      edge.color = "gray40",
  #      edge.width = 2,
  #      main = paste0("Spatial Network, d = ",d) )
  
  # Modularity
  modules <- cluster_infomap(g) #switch to different function if needed
  m <- modularity(modules)
  
  # Centrality metrics
  c_betwenness <- betweenness(g, directed=FALSE)
  c_closeness <- closeness(g)
  c_bridge <- bridge(g, communities = modules)
  deg_list <- degree(g)
  
  # Compute components
  comp <- components(g, mode = "weak")  # Use mode = "strong" for directed Strongly Connected Component (SCC)
  largest_size <- max(comp$csize)  
  total_nodes <- vcount(g)   
  fraction <- largest_size / total_nodes  # Fraction of nodes in largest component
  
  # Calculate percentage of nodes connected
  connected_nodes <- sum(deg_list >= 1)
  percent_connected <- (connected_nodes / nrow(m_adj)) * 100
  
  return(list(
    edge_index = edge_index,
    distance = d, 
    connectance = c, 
    modularity = m,
    betweenness = c_betwenness,
    closeness = c_closeness,
    bridge = c_bridge,
    degree = deg_list,
    comp_number = comp$no,
    largest_comp = largest_size,
    fraction_comp = fraction,
    percent_connected = percent_connected,
    graph = g
  ))
}

# 4. DISTANCE NETWORKS -----------------------------------------------------
# Network Construction & objects for analysis
# Uncomment/comment below in case you are using your own threshold distance for edge creation -

dist_edge <- 500
# dist_edge <- mean(df_dist_stat$mean_distance_m)
net_res_list <- euclidean_network_e2e(dist_edge, df_patch_og)

#### __ Network metrics -------------------------------------------------------
net_metrics <-  data.frame(
  distance = net_res_list$distance,
  percent_connected = net_res_list$percent_connected,
  connectance = net_res_list$connectance,
  modularity = net_res_list$modularity,
  comp_number = net_res_list$comp_number, 
  comp_largest = net_res_list$largest_comp,
  comp_fraction = net_res_list$fraction_comp
)

# # Change NaN --> 0 (uncomment if you notice NA in the data)
# numeric_cols <- sapply(net_metrics, is.numeric)
# net_metrics[numeric_cols] <- lapply(net_metrics[numeric_cols], function(col) {
#   col[is.nan(col)] <- 0
#   return(col)
# })


net_metrics
str(net_metrics)



#### __ Node metrics  ---------------------------------------------------------
node_metrics <- data.frame(
  distance = net_res_list$distance,
  node = names(net_res_list$betweenness),
  betweenness = net_res_list$betweenness,
  closeness = net_res_list$closeness,
  degree = net_res_list$degree,
  bridge = net_res_list$bridge$`Bridge Betweenness`
)


# # Change NaN --> 0 (uncomment if you notice NA in the data)
# numeric_cols <- sapply(node_metrics, is.numeric)
# node_metrics[numeric_cols] <- lapply(node_metrics[numeric_cols], function(col) {
#   col[is.nan(col)] <- 0
#   return(col)
# })


node_metrics
str(node_metrics)

# Here we add size of patch and different metric rankings for further analysis
node_metrics <- node_metrics %>%
  mutate(area_m2 = df_patch_og$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )



#### __ Network Vis --------------------------------------------------
#' Functions are created to visualise networks - easier to access later
#' NOTE: these functions take up R memory - use them sparingly when computing

g_net <- net_res_list$graph
g_net_plot <- function () {
  g_net <- net_res_list$graph
  nodes <- igraph::as_data_frame(g_net, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_net)$name) %>% 
    arrange(match(name, V(g_net)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_net,
       layout = layout_coords,
       vertex.size = sqrt(V(g_net)$area_m2)/10, # /10 can be changed depending on patch size
       vertex.label = V(g_net)$name,
       vertex.color = "#56B4E9",
       vertex.label.cex = 1,              
       vertex.label.dist = 0.6, 
       edge.color = "#FF4000",
       edge.width = 2,
       asp = 0,
       main = paste0("Original -  Spatial Network, \n d = ", round(net_res_list$distance, 2)))
}

g_net_plot()

# 5. ANALYSIS  ------------------------------------------------------------
### __ Degree - Top nodes ---------------------------------------------------------
# Ranking to show top nodes can also be done for other metrics - use ranking from node_metrics
# Change metric (eg. degree) and metric_rank (eg. degree_rank) accordingly below

# Extrapolate top nodes from graph -> if >10 then top 10, if not, max number of edges
edge_num <- if (sum(node_metrics$degree >= 1) > 10) {10} else {sum(!is.na(node_metrics$degree_rank))}

top_nodes <- node_metrics %>%
  arrange(degree_rank) %>%
  slice(1:edge_num) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()


g_rank_plot <- function() {
  g_rank <- net_res_list$graph
  nodes <- igraph::as_data_frame(g_rank, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rank)$name) %>% 
    arrange(match(name, V(g_rank)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  # Assign different colours to top nodes 
  v_colours <- ifelse(V(g_rank)$name %in% top_nodes, "#CC79A7", "#56B4E9")
  
  plot(g_rank,
       layout = layout_coords,
       vertex.size = sqrt(V(g_rank)$area_m2)/10, 
       vertex.label = V(g_rank)$name,
       vertex.color = v_colours,
       vertex.label.cex = 1.2,              
       vertex.label.dist = 0.4,            
       edge.color = "#FF4000",
       edge.width = 2.5,
       asp = 0
  )
  title(main = paste0("\n\n > YOUR TITLE HERE <\n d = ", round(net_res_list$distance, 2)), cex.main = 2)
  mtext(paste("Top nodes:", paste(top_nodes, collapse = ", ")), side = 1, line = 1.5, cex = 1.5)
}

g_rank_plot()

top_nodes

### __ Modularity - Degree-controlled null --------------------------------------------------

#### > Function for modularity analysis ----------------------------------------
random_mod_cal_fun <- function(g, num_graphs) {
  random_modularity <- numeric(num_graphs)
  for (i in 1:num_graphs) {
    g_rand <- sample_degseq(degree(g), method = "configuration")
    mod_rand <- cluster_infomap(g_rand) #can be changed to other clustering functions - for comparison better to match with same as in euclidean_network_e2e function
    random_modularity[i] <- modularity(mod_rand)
  }
  return(random_modularity)
}


#### >  Generate OG-network comparison --------------------------------------------------
# Generate random graphs and compute modularity - Degree controlled
num_g <- 1000     # how many random graphs you want
g_net_mod <- net_metrics$modularity # extract modularity from original network
random_modularity_og <- random_mod_cal_fun(g_net, num_g) # this is a built-in function - see {igraph}


#### > Visualisation -----------------------------------------------------

hist(random_modularity_og, breaks = 30, col = "#A4C9EC",
     main = "Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_og, na.rm = TRUE),
              max(c(random_modularity_og, g_net_mod), na.rm = TRUE))
)
abline(v = g_net_mod, col = "#FF4000", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "#FF4000", lwd = 2)

#### > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score <- (g_net_mod - mean(random_modularity_og, na.rm = TRUE)) / sd(random_modularity_og, na.rm = TRUE)
print(paste("Z-score (Degree controlled):", round(z_score, 5)))

###__ Degree distribution ------------------------------------
#' This section aims to understand which degree distribution our network has - classify if the OG is closer to a random or scale-free network
#' Functions to construct theoretical distributions are from {igraph}
#### > Original network --------------------------------------------------------
g_net_plot()

#These functions are here to compare between the different networks later
vcount(g_net)
ecount(g_net)
transitivity(g_net) # clustering coefficient
components(g_net)
mean(degree(g_net))

#Visualisation is done through the use of functions to access later - could also create ggplot which allow for object storage
og_deg_hist <- function() {hist(degree(g_net), xlim = c(0, 35))
  text(x = max(degree(g_net)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


#### > Random network ----------------------------------------------------------
set.seed(54367) # always set seed before creating random network so that you get same results
random_net <- sample_correlated_gnp(g_net, p = edge_density(g_net), corr = 0.6)

vcount(random_net)
ecount(random_net)
transitivity(random_net) 
components(random_net)
mean(degree(random_net))


#Visualisation 
rand_deg_hist <- function () {hist(degree(random_net), 
                                        xlim = c(0, 35),
)
  abline(v = mean(degree(g_net)), col = "#FF4000", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_net)), 2)),
       col = "#FF4000",
       cex = 1)
  text(x = max(degree(random_net)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


#### > Scale-free network ------------------------------------------------------
set.seed(54367)
m_estimate <- ceiling(ecount(g_net) / vcount(g_net))
scalefree_net <- sample_pa(n = vcount(g_net), m = m_estimate, directed = FALSE, zero.appeal = 0)

vcount(scalefree_net)
ecount(scalefree_net)
transitivity(scalefree_net) 
components(scalefree_net)
mean(degree(scalefree_net))

#Visualisation 
sf_deg_hist <- function() {hist(degree(scalefree_net), xlim = c(0, max(degree(scalefree_net))))
  abline(v = mean(degree(g_net)), col = "#FF4000", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_net)), 2)),
       col = "#FF4000",
       cex = 1)
  text(x = max(degree(scalefree_net)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}


# # If you want to create small-world aswell, here's a function to use
# sw_net <- sample_smallworld(dim = 1, size = vcount(g_net), nei = floor(mean(degree(g_net))/2), p = 0.1)


x11()
par(mfrow = c(3,1))
og_deg_hist()
rand_deg_hist()
sf_deg_hist()

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/----
# TARGETED RESTORATION PLANS ----------------------------------------------
# 6. RESTORATION DATA ----------------------------------------------------
## 6.1 Dataset cleanup & Visualisation----------------------------------------------------
df_patch_rest <- read.csv("your_path_here.csv")
# If a group/clustering is not present - add one onto both original and restored dataframe 
df_patch_og$group <- "original"
df_patch_rest$group <- "restored"
# We need to change the number of the patches so that they do not double the numbers assigned to the OG Network
max_patch_og <- max(range(df_patch_og$patch))
# here we make sure we start numbering from one number after the last OG node
start_patch_num <- max_patch_og +1 

df_patch_rest$patch <- seq(start_patch_num, length.out = nrow(df_patch_rest))
df_patch_rest$group <- as.factor(df_patch_rest$group)

#Create a DF with all patches, restored and original
df_patch_tot <- rbind(df_patch_og, df_patch_rest)

ggplot(data=df_patch_tot, 
       aes(x=longitude, y=latitude, size=area_m2, col=group)) +
  geom_point() +
  geom_text(aes(label=patch), hjust=-0.1, vjust=-0.1, col="grey", size=4) +
  coord_fixed() +
  labs(size="Patch area\n(m2)") +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_rect(fill="white", colour="grey"),
        panel.grid.major=element_line(colour="grey"),
        legend.position="bottom")


## 6.2 100% Restored Node Metric  ------------------------------------------
# For subsequent analysis we need the node metrics of the fully restored network - so that we can make gradual restoration analysis based on ranking.
rest100_res_list <- euclidean_network_e2e(dist_edge, df_patch_tot)

#### __ 100% Rest  Node metrics  ---------------------------------------------------------
rest100_node_metrics <- data.frame(
  distance = rest100_res_list$distance,
  node = names(rest100_res_list$betweenness),
  betweenness = rest100_res_list$betweenness,
  closeness = rest100_res_list$closeness,
  degree = rest100_res_list$degree,
  bridge = rest100_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(rest100_node_metrics, is.numeric)
rest100_node_metrics[numeric_cols] <- lapply(rest100_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest100_node_metrics
str(rest100_node_metrics)

# Create ranking for later
rest100_node_metrics <- rest100_poll_node_metrics %>%
  mutate(area_m2 = df_tot_rest$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )


# 7. NODE SELECTION AND DATAFRAME CREATION -----------------------------------------------------

## 7.1 Targeted Node Selection Function  ------------------------------------------------
#' Function which progressively subsets only restored nodes from DF with ranks on them 
#' The percentage (perc_rest) is a number between 0 and 1 depending on how much of the total patches you want restored 
#' centr_measure = eg. "degree_rank"

node_select_targ_rest_fun <- function(df_patch, node_metrics_df, perc_rest, centr_measure, start_patch_num) {
  df_sub_rank <- node_metrics_df %>% 
    filter(as.numeric(node) >= start_patch_num & as.numeric(node) <= length.out(df_patch_tot))
  rest_num <- floor(nrow(df_sub_rank) * perc_rest)
  
  node_select <- df_sub_rank %>%
    arrange(.data[[centr_measure]]) %>%
    slice(1:rest_num) %>%
    pull(node)
  
# Create total restoration DF containing OG nodes and new
  final_df <- df_patch %>% 
    filter(
      group == "original" |
        (group == "restored" & patch  %in% node_select)
    )
}


##  7.2 Application of Targeted Node Selection Function  ----------------------------
# Here we create a list with progressive restoration plans containing more and more nodes each time
df_targ_rest_list <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100)
  df_targ_rest_list[[name]] <- node_select_targ_rest_fun(df_patch_tot, rest100_node_metrics, perc, "degree_rank")
}

# View new object created
df_targ_rest_list
df_targ_rest_list$df_targ_rest10 # this is just an example


# 8. TARGETED RESTORED DISTANCE NETWORKS -----------------------------------------------------
## 8.1 Network Metric Dataframe Function ---------------------------------------
# This function creates a dataframe with network metric which contains results for each restoration
# Can be used for both targeted and random restoration --> change rest_type to "targeted" or "random"
df_net_metrics_fun <- function(rest_res_list, rest_type){
  new_df <- data.frame()
  for (name in names(rest_res_list)) {
    curr_net <- rest_res_list[[name]]
    perc <- as.numeric(gsub("[^0-9]", "", name)) # this select only digits from the original name to extract percentage of restored patches 
    
    df <- data.frame(
      distance = curr_net$distance,
      percent_connected = curr_net$percent_connected,
      connectance = curr_net$connectance,
      modularity = curr_net$modularity,
      comp_number = curr_net$comp_number,
      comp_largest = curr_net$largest_comp,
      comp_fraction = curr_net$fraction_comp,
      restoration_percent = perc,
      restoration_type = rest_type
    )
    new_df <- rbind(new_df, df)
  }
  return(new_df)
}


## 8.2 Restoration Networks creation ---------------------------------------------------------
#' Here we take all the different dataframes from the df_targ_rest_list and create networks for all of them
#' This part here takes a very long time to run so once you use it I suggest commenting it (data will be saved below as an object that can be later loaded if needed)
net_targ_rest_res <- list()

for(name in names(df_targ_rest_list)){
  df <- df_targ_rest_list[[name]]
  result <- euclidean_network_e2e(dist_edge, df)
  net_targ_rest_res[[name]] <- result
}


#### __ Rest Network metrics -------------------------------------------------------
targ_rest_net_metrics <- df_net_metrics_fun(net_targ_rest_res, "targeted")
targ_rest_net_metrics


# 9. ANALYSIS  ------------------------------------------------------------
### 9.1 Modularity & Z-score - Degree-controlled null --------------------------------------------------
#### > Creation of modularity curves for all networks----------------------------------------------------------
# This also takes a long time to run - comment once you create the object and save as an external object
# Creates a data frame with restoreation percent, real modularity, mean random mod, sd and z_scores
z_score_targ <- data.frame()

for(name in names(net_targ_rest_res)){
  cat("Current G:", name, "\n")
  g <- net_targ_rest_res[[name]]$graph
  restoration_percent <- as.numeric(gsub("[^0-9]", "", name))
  curr_rand_mod <- random_mod_cal_fun(g, num_g)
  curr_sd <- sd(curr_rand_mod, na.rm = TRUE)
  mean_rand_mod <- mean(curr_rand_mod, na.rm = TRUE)
  real_m <- net_targ_rest_res[[name]]$modularity
  z_score <- (real_m - mean_rand_mod) / curr_sd
  z_score_targ <- rbind(z_score_targ,
    data.frame(
      restoration_percent = restoration_percent,
      real_modularity = real_m,
      mean_random_modularity = mean_rand_mod,
      sd_random_modularity = curr_sd,
      z_score = z_score
  ))
}

 z_score_targ


####  __Visualisation of z-score -----------------------------------------------------
# Plot standard normal distribution
ggplot(z_score_targ, aes(x = restoration_percent, y = z_score)) +
  geom_point() +
  theme_bw() +
  ggtitle("Comparison of Z-scores for modularity analysis \n > Targeted <") +
  xlab("Restoration Percent (%)") +
  ylab("Modularity Z-score") +
  scale_x_continuous(breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, max(z_score_targ$z_score)+1, by = 5),
                     limits = c(0, NA)) +
  geom_hline(yintercept = 1.96, col = "red", linetype = 2, size = 1)+
  annotate(geom = "text", label = "p < 0.05", y = 3, x = 100, size = 4, col = "red")+
  annotate(geom = "text", label = "z = 1.96", y = 1, x = 100, size = 4, col = "red")


### 9.2 Degree distribution - Restoration ------------------------------------
# NOTE: The 100% analysis can be changed to any percentage - only change the g_rest100 object accordingly - names of objects can also be changed to be more meaningful
### > Original Network - 100% -----------------------------------------------------


g_rest100 <- net_targ_rest_res$df_targ_rest100$graph

vcount(g_rest100)
ecount(g_rest100)
transitivity(g_rest100) 
components(g_rest100)
mean(degree(g_rest100))

og_deg_rest100_hist <- function() {hist(degree(g_rest100),
                                             xlim = c(0, 25),
)
  text(x = max(degree(g_rest100)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


### > Random network ------------------------------------------------
set.seed(54367) 
random_net_targ_rest100 <- sample_correlated_gnp(g_rest100, p = edge_density(g_rest100), corr = 0.6)

vcount(random_net_targ_rest100)
ecount(random_net_targ_rest100)
transitivity(random_net_targ_rest100) 
components(random_net_targ_rest100)
mean(degree(random_net_targ_rest100))

#Visualisation 
rand_deg_targ_rest100_hist <- function () {hist(degree(random_net_targ_rest100), 
                                                     xlim = c(0, 25),
)
  abline(v = mean(degree(g_rest100)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_rest100)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(random_net_targ_rest100)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}


### > Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_rest100) / vcount(g_rest100))

set.seed(54367)
scalefree_net_targ_rest100 <- sample_pa(n = vcount(g_rest100), m = m_estimate, directed = FALSE, zero.appeal = 0)

vcount(scalefree_net_targ_rest100)
ecount(scalefree_net_targ_rest100)
transitivity(scalefree_net_targ_rest100) 
components(scalefree_net_targ_rest100)
mean(degree(scalefree_net_targ_rest100))

#Visualisation 

sf_deg_targ_rest100_hist <- function() {hist(degree(scalefree_net_targ_rest100), 
                                                  xlim = c(0, 25),
)
  abline(v = mean(degree(g_rest100)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_rest100)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(scalefree_net_targ_rest100)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_deg_rest100_hist()
rand_deg_targ_rest100_hist()
sf_deg_targ_rest100_hist()



### > Degree Distribution Across Restoration %  ------------------------
# Create dataframe with degree distributions from the result list
# Original network
df_deg <- data.frame(degree = res_list$degree, perc_rest = as.factor(0))

# Create the degree distribution for all the restoration plans
df_deg_rest <- do.call(rbind, lapply(names(net_targ_rest_res), function(name) {
  data.frame(degree = net_targ_rest_res[[name]]$degree, perc_rest = as.factor(gsub("[^0-9]", "", name)))
}))

#Bind them together
df_deg_rest <- rbind(df_deg, df_deg_rest)


ggplot(df_deg_rest, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "darkorange3", color = "white") +
  facet_wrap(~ perc_rest) +
  theme_bw() +
  labs(title = "Degree Distributions by Targeted Restoration %", x = "Degree", y = "Frequency")

# Can also subset to only see a few restoration plans - in this case every 20.
deg_deg_subset <- df_deg_rest %>%
  filter(as.numeric(as.character(perc_rest)) %% 20 == 0)

ggplot(deg_deg_subset, aes(x = degree, color = as.numeric(as.character(perc_rest)), group = perc_rest)) +
  geom_density(size = 0.8, fill = NA, key_glyph = draw_key_path, adjust = 3) + 
 # scale_x_continuous(breaks = seq(0, 20, by = 2))+ # this needs to be adapted to your own network
  scale_y_continuous(limit = c(0,1))+
  theme_bw(base_size = 16) +
  labs(title = "> Trends in degree distribution <", x = "Degree", y = "Density",color = "Restoration %", linetype = 1)  +
  scale_color_gradient(low = "#D6A9F0", high = "#0072B2")

# 10. FINAL VISUALISATION -----------------------------------------------------
## 10.1 Data frames - Metrics --------------------------------------------------------------------
# Add Original Network metrics to the restored one 
 net_metrics$restoration_percent <- 0
 net_metrics$restoration_type <- "targeted"

full_comp_targ_rest <- net_metrics %>% 
  rbind(targ_rest_net_metrics)

full_comp_targ_rest

# long format for the data so that they can all be plotted in the same graph 
full_comp_targ_rest_long <- full_comp_targ_rest %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")


## 10.2 Visualisation -----------------------------------------------------------
### > Metrics  ----------------------------------------------------------------

tot_targ_rest_metrics_plot <- ggplot(full_comp_targ_rest_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios \n > Targeted <",
       x = "Network Type",
       y = "Metric Value",
       color = "Metric") +
  scale_color_manual(name = "Metric",
                     values = c(
                       percent_connected = "#1b9e77",
                       connectance = "#d95f02",
                       modularity = "#7570b3",
                       comp_fraction = "#e7298a"
                     ),
                     labels = c(
                       percent_connected = "Percent Connected",
                       connectance = "Connectance",
                       modularity = "Modularity",
                       comp_fraction = "Component Fraction"
                     )) +
  theme_bw() 



### > Network Visualisation  -------------------------------------------------

# to ease up the process, function for creating graphs
#top node selection depending on their degree
top_node_degree_select_fun <- function(res_list) {
  curr_df <- data.frame(
    node = names(res_list$degree), 
    degree = res_list$degree)
  node_select <- curr_df %>%
    arrange(desc(degree)) %>%
    slice(1:10) %>%
    pull(node) %>% 
    as.numeric() %>% 
    sort()
}


# NOTE: it is not possible to save the result of the function as its own object, to do so, it is better to create one function for each graph as in the previous plots
# In this case I created a function to explore all the different newtork from the result list
# 

g_plot_creation<- function(g, perc, d, rest_type, top_nodes = NULL) {
  g_curr <- g
  nodes <- igraph::as_data_frame(g_curr, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_curr)$name) %>% 
    arrange(match(name, V(g_curr)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  v_colours <- if (!is.null(top_nodes)) {
    ifelse(V(g_curr)$name %in% top_nodes, "#CC79A7", "#56B4E9")
  } else {
    rep("#56B4E9", vcount(g_curr))
  }
  plot(g_curr,
       layout = layout_coords,
       vertex.size = sqrt(V(g_curr)$area_m2)/300,
       # vertex.size = 2, # this is there if you do not want to see the size of the patches - uncomment this - comment one above
       vertex.label = V(g_curr)$name,
       vertex.color = v_colours,
       vertex.label.cex = 1.2,
       # vertex.label.degree=-pi/2, 
       vertex.label.dist = 0.4, 
       edge.color = "#FF4000",
       edge.width = 2.5,
       asp = 0)
  title(paste0(rest_type, "\n\n  Rest ", perc, "% \n > ", " <\n d = ", round(d, 2)), cex.main = 2)
  if (!is.null(top_nodes)) {
    mtext(paste("Top nodes:", paste(top_nodes, collapse = ", ")), side = 1, line = 1.5, cex = 1.5)
  }
}





#Some examples
net_targ_rest_res
g_plot_creation(net_targ_rest_res$df_targ_rest10$graph, 10, dist, "Targeted")
g_plot_creation(net_targ_rest_res$df_targ_rest30$graph, 30, dist, "Targeted")
g_plot_creation(net_targ_rest_res$df_targ_rest50$graph, 50, dist, "Targeted")
g_plot_creation(net_targ_rest_res$df_targ_rest100$graph, 100, dist, "Targeted")



# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/----
# RANDOM RESTORATION PLANS ----------------------------------------------
#' The analysis of restoration stays almost the same and starts with the same total DF df_patch_tot
#' NOTE: This section is shorter than the one before - if wanted, the above analysis can be applied to the random restoration as well (eg modularity and degree distribution).
# 11. NODE SELECTION AND DATAFRAME CREATION -----------------------------------

## 11.1 Targeted Node Selection Function  ------------------------------------------------
#' Function similar to "node_select_targ_rest_fun" - but progressively subsets restored nodes randomly

node_select_rand_rest_fun <- function(df_patch, net_metrics, perc_rest, seed) {
  set.seed(seed)
  df_sub_rank <- net_metrics %>% 
    filter(as.numeric(node) >= start_patch_num & as.numeric(node) <= length.out(df_patch_tot))
  rest_num <- floor(nrow(df_sub_rank) * perc_rest)
  
  total_nodes <- df_sub_rank$node
  total_nodes_shuffled <- sample(total_nodes)  
  
  node_select <- total_nodes_shuffled[1:rest_num]  
  
  # Subset total restoration DF to contain OG nodes and new 50% 
  final_df <- df_patch %>% 
    filter(
      group == "original" |
        (group == "restored" & patch  %in% node_select)
    )
}



## 11.2 Application of Random Node Selection Function ----------------------
seed_n <- 182955309 #random number 

df_rand_rest_list <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_rand_rest", perc * 100, "")
  df_rand_rest_list[[name]] <- node_select_rand_rest_fun(df_patch_tot, rest100_node_metrics, perc, seed_n)
}



# 12. RANDOM RESTORED DISTANCE NETWORKS -----------------------------------------------------
## 12.1 Restoration Network creation ---------------------------------------------------------
# Again here I would suggest saving the object externally
net_rand_rest_res <- list()

for(name in names(df_rand_rest_list)){
  df <- df_rand_rest_list[[name]]
  result <- euclidean_network_e2e(dist_edge, df)
  net_rand_rest_res[[name]] <- result
}

#### __ Rest Network metrics -------------------------------------------------------
rand_rest_net_metrics <- df_net_metrics_fun(net_rand_rest_res, "random")
rand_rest_net_metrics


# 13. FINAL VISUALISATION -----------------------------------------------------
## 13.1 Data frames - Metrics --------------------------------------------------------------------
rand_net_metrics <- net_metrics
rand_net_metrics$restoration_type <- "random"


full_comp_rand_rest <- rand_net_metrics %>% 
  rbind(rand_rest_net_metrics)

full_comp_rand_rest

# long format for the data so that they can all be plotted in the same graph 
full_comp_rand_rest_long <- full_comp_rand_rest %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")



## 13.2. Visualisation -----------------------------------------------------------
### > Metrics  ----------------------------------------------------------------
tot_rand_rest_metrics_plot <- ggplot(full_comp_rand_rest_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios\n > Random <",
       x = "Network Type",
       y = "Metric Value",
       color = "Metric") +
  scale_color_manual(name = "Metric",
                     values = c(
                       percent_connected = "#1b9e77",
                       connectance = "#d95f02",
                       modularity = "#7570b3",
                       comp_fraction = "#e7298a"
                     ),
                     labels = c(
                       percent_connected = "Percent Connected",
                       connectance = "Connectivity",
                       modularity = "Modularity",
                       comp_fraction = "Component Fraction"
                     )) +
  theme_bw() 



### > Targeted vs Random  -----------------------------------------------------
rvt_full_comp <- full_comp_targ_rest %>% 
  rbind(full_comp_rand_rest)


#create long version for visualisation 
rvt_full_comp_long <- rvt_full_comp %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance) %>% 
  pivot_longer(
    cols = c(percent_connected, connectance, modularity, comp_fraction),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    rest_type = factor(restoration_type, levels = c("targeted", "random"), labels = c("Targeted", "Random")),
    metric = factor(metric,
                    levels = c("percent_connected", "connectance", "modularity", "comp_fraction"),
                    labels = c("Percent Connected", "Connectance", "Modularity", "Component Fraction"))
  )



rvt_full_rest_metrics_plot <- ggplot(filter(rvt_full_comp_long, metric %in% c("Connectance", "Percent Connected")), aes(x = restoration_percent, y = value, color = metric, linetype = rest_type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  labs(
    title = " > Random vs Targeted <",
    x = "Percent Restored",
    y = "Metric Value",
    color = "Metric",
    linetype = "Restoration Type"
  ) +
  facet_wrap(~ metric, scale = "free_y")+ 
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(
    "Percent Connected" = "#009900",
    "Connectance" = "#FF4000",
    "Modularity" = "#0072B2",
    "Component Fraction" = "#CC79A7"
  )) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

rvt_full_rest_metrics_plot



### > Network Visualisation  -------------------------------------------------
# Some examples using the previously made function 
g_plot_creation(poll_net_rand_rest_res$df_rand_rest10$graph, 10, dist, "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest30$graph, 30, dist, "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest50$graph, 50, dist, "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest100$graph, 100, dist, "Random")





# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/----



