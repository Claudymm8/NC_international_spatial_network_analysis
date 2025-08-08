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
library(caseconverter) 


# 1. Import data & Visualisation  -----------------------------------------
## 1.1 Patch data  ---------------------------------------------------------
# Import data frame with patch, area_m2, longitude, latitude, cluster (if patches are divided by predetermined clusters)
df_patch <- read.csv("D:/ADD_PATH/heaths_dorset.csv")

## ___ South Cluster only ------------------------------------------------------
# Subset for south cluster only
df_patch_s <- filter(df_patch, cluster=="south")
head(df_patch_s)
str(df_patch_s)
# View(df_patch_s)
df_patch_s$cluster <- as.factor(df_patch_s$cluster)


# Visualise original nodes
ggplot(data=df_patch, 
       aes(x=longitude, y=latitude, size=area_m2, col=cluster)) +
  geom_point() +
  geom_text(aes(label=patch), hjust=-0.1, vjust=-0.1, col="darkgrey", size=4) +
  coord_fixed() +
  scale_color_manual(values = c("north" = "#56B4E9", "south" = "#FF4000")) + 
  labs(size="Patch area\n(m2)") +
  theme_bw() +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_rect(fill="white", colour="grey"),
        panel.grid.major=element_line(colour="grey"),
        legend.position="bottom")



## 1.2 Organism data -------------------------------------------------------
### 1.2.1 Plant species data -------------------------------------------------------
# This section is pretty targeted to this study - need to use theta for the exponential curve to calculate mean distances - skip to 3. if you already have the distances you want to use for the network construction.

# NOTE: This was later changed - the new data for the plants can be directly added at 2.2.1

# df_plant_original <- read_excel("D:/ADD_PATH/plant_pollinator_dispersal.xlsm", "plants")
# str(df_plant_original)
# 
# df_plant <-  df_plant_original[-c(0,9:12)] #delete empty or not needed columns
# colnames(df_plant)[2] <- "group"
# colnames(df_plant)[7] <- "perc_dist_75"
# colnames(df_plant)[8] <- "perc_dist_95"
# str(df_plant)
# head(df_plant)
# # View(df_plant)
# 
# unique(df_plant_original$growth_form)

### 1.2.2 Pollinator species data --------------------------------------------------
df_poll_original <- read_excel("C:/ADD_PATH/plant_pollinator_dispersal.xlsm", "pollinators")
str(df_poll_original)

df_poll <-  df_poll_original[-c(0,9:12)] #delete empty or not needed columns
colnames(df_poll)[7] <- "perc_dist_75"
colnames(df_poll)[8] <- "perc_dist_95"
str(df_poll)
head(df_poll)
# View(df_poll)



# 2. ORGANISM DISTRIBUTION ANALYSIS ---------------------------------------
## 2.1 Functions ------------------------------------------------------------
### 2.1.1 Exponential dispersal probability function ---------------------------
# Negative exponential function for kernel construction
# Adapted from Bullock et al. (2016) - Synthesis of empirical plant dispersal kernels
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
### 2.2.1 Plant Kernel -------------------------------------------------------- 
# # Set range of distances - this is an educated guess, can change as you wish if you want more or less range and bins
# d_plant <- seq(0, 100, by = 0.01)
# 
# # Apply function 
# df_dens_plant <- density_fun(df_plant, d_plant)
# head(df_dens_plant)
# # View(df_dens_plant)

tot_jbdf_metrics <- read.csv("D:/ADD_PATH/tot_jbdf_metrics.csv")
tot_jbdf_metrics 

#### ___ Visualisation  ----------------------------------------------------------
# The following has been commented due to change in dispersal kernels

# Trim the values to only show up to maximum distances
# threshold_plant <- 1e-5 # near zero threshold, so when the probability hits 0 we do not visualise it 
# 
# df_dens_plant_trim <- df_dens_plant %>%
#   group_by(group) %>%
#   filter(dens > threshold_plant)
# 
# 
# disp_kern_plants <- ggplot(df_dens_plant_trim, aes(x = distance, y = dens, colour = group)) +
#   geom_line()+
#   facet_wrap(~group, scales = "free")+
#   labs(
#     title = "Exponential Distribution Kernels Plants",
#     x = "Distance (m)",
#     y = "Probability of dispersal"
#   ) +
#   theme_bw()
# disp_kern_plants

#### ___ Distance values -------------------------------------------------------
# df_dist_stat_plant <- df_dens_plant %>%
#   group_by(group) %>%
#   arrange(distance) %>%
#   mutate(     # to get probabilities and threshold keeping in mind areas under the curve (sume then = 1)
#     prob = dens / sum(dens),        
#     cum_prob = cumsum(prob)
#   ) %>%
#   summarise(
#     mean_distance_m   = sum(distance * prob),                 
#     median_distance_m = distance[which.min(abs(cum_prob - 0.5))],  
#     q25_distance_m    = distance[which.min(abs(cum_prob - 0.25))], 
#     q75_distance_m    = distance[which.min(abs(cum_prob - 0.75))], 
#     q95_distance_m    = distance[which.min(abs(cum_prob - 0.95))]  
#   )

#add mean_theta to the dataframe
# df_dist_stat_plant <- df_dist_stat_plant %>%
#   left_join(df_plant %>%
#               group_by(group) %>%
#               summarise(mean_theta = mean(theta_per_m, na.rm = TRUE)))


### 2.2.2 Pollinator Kernel -------------------------------------------------------
# Set range of distances - educated guess
d_poll <- seq(0, 3000, by = 1)
df_dens_poll <- density_fun(df_poll, d_poll)
head(df_dens_poll)
# View(df_dens_poll)  



#### ___ Visualisation  ---------------------------------------------------------
# Trim for visualisation --> exponential will never reach 0, so we cut close enough to 0
threshold_poll <- 1e-10 

df_dens_poll_trim <- df_dens_poll %>%
  group_by(group) %>%
  filter(dens > threshold_poll)

disp_kern_poll <- ggplot(df_dens_poll_trim, aes(x = distance, y = dens, colour = group)) +
  geom_line()+
  facet_wrap(~group, scale = "free")+
  labs(
    title = "Exponential Distribution Kernels Pollinators",
    x = "Distance",
    y = ""
  ) +
  theme_bw()
disp_kern_poll

#### ___ Distance values --------------------------------------------------------
df_dist_stat_poll <- df_dens_poll %>%
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
df_dist_stat_poll <- df_dist_stat_poll %>%
  left_join(df_poll %>%
              group_by(group) %>%
              summarise(mean_theta = mean(theta_per_m, na.rm = TRUE)))

## 2.3 Final Distribution Objects --------------------------------------------
# disp_kern_plants + disp_kern_poll

# df_dist_stat_plant
df_dist_stat_poll

# 3. Euclidean distance function ------------------------------------------
#' This function only need the original patch DF and the preferred maximum distance at which the edges will be created 
#' Generally speaking, the function can be modified slightly depending on needs (see comments)
#' NOTE: this created links edge to edge and not centroid to centroid.
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
## 4.1 Pollinator ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)
pollinator_res_list <- euclidean_network_e2e(dist_poll, df_patch_s)

#### __ Pollinators Network metrics -------------------------------------------------------

poll_dist_metrics <-  data.frame(
  distance = pollinator_res_list$distance,
  percent_connected = pollinator_res_list$percent_connected,
  connectance = pollinator_res_list$connectance,
  modularity = pollinator_res_list$modularity,
  comp_number = pollinator_res_list$comp_number, 
  comp_largest = pollinator_res_list$largest_comp,
  comp_fraction = pollinator_res_list$fraction_comp
)

poll_dist_metrics
str(poll_dist_metrics)

#### __ Pollinator Node metrics  ---------------------------------------------------------
poll_node_metrics <- data.frame(
  distance = pollinator_res_list$distance,
  node = names(pollinator_res_list$betweenness),
  betweenness = pollinator_res_list$betweenness,
  closeness = pollinator_res_list$closeness,
  degree = pollinator_res_list$degree,
  bridge = pollinator_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(poll_node_metrics, is.numeric)
poll_node_metrics[numeric_cols] <- lapply(poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


poll_node_metrics
str(poll_node_metrics)

# Add ranking of nodes depending on measure for later analysis
poll_node_metrics <- poll_node_metrics %>%
  mutate(area_m2 = df_patch_s$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )


#### __ Pollinator Network Vis --------------------------------------------------
#' Functions are created to visualise networks - easier to access later
#' NOTE: these functions take up R memory - use them sparingly when computing

g_poll <- pollinator_res_list$graph
g_poll_plot <- function () {
g_poll <- pollinator_res_list$graph
nodes <- igraph::as_data_frame(g_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_poll)$name) %>% 
  arrange(match(name, V(g_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_poll)$area_m2)/300, # /300 just to see the edges better
     vertex.label = V(g_poll)$name,
     vertex.color = "#56B4E9",
     vertex.label.cex = 1,              
     vertex.label.dist = 0.6, 
     edge.color = "#FF4000",
     edge.width = 2,
     asp = 0,
     main = paste0("Original - Pollinator Spatial Network, \n d = ", round(pollinator_res_list$distance, 2)))
}

g_poll_plot()

## 4.2 Plant ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)
plant_res_list <- euclidean_network_e2e(dist_plant, df_patch_s)
# 5 edges created!



#### __ Plant Network metrics -------------------------------------------------------
plant_dist_metrics <-  data.frame(
  distance = plant_res_list$distance,
  percent_connected = plant_res_list$percent_connected,
  connectance = plant_res_list$connectance,
  modularity = plant_res_list$modularity,
  comp_number = plant_res_list$comp_number, 
  comp_largest = plant_res_list$largest_comp,
  comp_fraction = plant_res_list$fraction_comp
)

# Change NaN --> 0
numeric_cols <- sapply(plant_dist_metrics, is.numeric)
plant_dist_metrics[numeric_cols] <- lapply(plant_dist_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


plant_dist_metrics
str(plant_dist_metrics)

#### __ Plant Node metrics  ---------------------------------------------------------
plant_node_metrics <- data.frame(
  distance = plant_res_list$distance,
  node = names(plant_res_list$betweenness),
  betweenness = plant_res_list$betweenness,
  closeness = plant_res_list$closeness,
  degree = plant_res_list$degree,
  bridge = plant_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(plant_node_metrics, is.numeric)
plant_node_metrics[numeric_cols] <- lapply(plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})

# Add ranking of nodes depending on measure for later analysis
plant_node_metrics <- plant_node_metrics %>%
  mutate(area_m2 = df_patch_s$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )


plant_node_metrics
str(plant_node_metrics)



#### __ Plant Network Vis -------------------------------------------------------
g_plant <- plant_res_list$graph
g_plant_plot <- function() { 
g_plant <- plant_res_list$graph
nodes <- igraph::as_data_frame(g_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_plant)$name) %>% 
  arrange(match(name, V(g_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_plant,
     layout = layout_coords,
     vertex.size = sqrt(V(g_plant)$area_m2)/300, # /300 just to see the edges better 
     vertex.label = V(g_plant)$name,
     vertex.color = "#56B4E9",
     vertex.label.cex = 1,              
     vertex.label.dist = 0.6, 
     edge.color = "#FF4000",
     edge.width = 2,
     asp = 0,
     main = paste0("Original - Plant Spatial Network, \n d = ", round(plant_res_list$distance, 2)))
}

g_plant_plot()

# 5. ANALYSIS  ------------------------------------------------------------
## 5.1 Pollinators ----------------------------------------------------------
### __ Degree - Top nodes ---------------------------------------------------------

# Change metric accordingly
# Extrapolate top nodes from graph -> if >10 then top 10, if not, max number of edges
edge_num_poll <- if (sum(poll_node_metrics$degree >= 1) > 10) {10} else {sum(!is.na(poll_node_metrics$degree_rank))}

top_nodes_poll <- poll_node_metrics %>%
  arrange(degree_rank) %>%
  slice(1:edge_num_poll) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()


g_rank_poll_plot <- function() {
g_rank_poll <- pollinator_res_list$graph
nodes <- igraph::as_data_frame(g_rank_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rank_poll)$name) %>% 
  arrange(match(name, V(g_rank_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rank_poll)$name %in% top_nodes_poll, "#CC79A7", "#56B4E9")

plot(g_rank_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rank_poll)$area_m2)/200, 
     vertex.label = V(g_rank_poll)$name,
     vertex.color = v_colours,
     vertex.label.cex = 1.2,              
     vertex.label.dist = 0.4,            
     edge.color = "#FF4000",
     edge.width = 2.5,
     asp = 0
)
title(main = paste0("\n\n > Pollinators <\n d = ", round(pollinator_res_list$distance, 2)), cex.main = 2)
mtext(paste("Top nodes:", paste(top_nodes_poll, collapse = ", ")), side = 1, line = 1.5, cex = 1.5)
}

g_rank_poll_plot()

top_nodes_poll

### __ Modularity - Degree-controlled null --------------------------------------------------
# num_nodes_poll <- vcount(g_poll) # number of nodes from graph
# num_edges_poll <- ecount(g_poll) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_poll_mod <- poll_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_poll <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_poll), method = "configuration")
  mod_rand <- cluster_infomap(g_rand) #can be changed to other clustering functions - for comparison better to match with same as in euclidean_network_e2e function
  random_modularity_poll[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_poll, breaks = 30, col = "#A4C9EC",
     main = "Pollinators - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_poll, na.rm = TRUE),
              max(c(random_modularity_poll, g_poll_mod), na.rm = TRUE))
)
abline(v = g_poll_mod, col = "#FF4000", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "#FF4000", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_poll <- (g_poll_mod - mean(random_modularity_poll, na.rm = TRUE)) / sd(random_modularity_poll, na.rm = TRUE)
print(paste("Pollinator - Z-score (Degree controlled):", round(z_score_poll, 5)))

###__ Degree distribution ------------------------------------
# This section aims to understand which degree distribution our network has to classify if the OG is closer to a random or scale-free network
#### > Original network --------------------------------------------------------
g_poll_plot()
vcount(g_poll)
ecount(g_poll)
transitivity(g_poll) # clustering coefficient
components(g_poll)
mean(degree(g_poll))

og_deg_poll_hist <- function() {hist(degree(g_poll), xlim = c(0, 35))
  text(x = max(degree(g_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


#### > Random network ----------------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_poll <- sample_correlated_gnp(g_poll, p = edge_density(g_poll), corr = 0.6)

vcount(random_net_poll)
ecount(random_net_poll)
transitivity(random_net_poll) 
components(random_net_poll)
mean(degree(random_net_poll))



#Visualisation 
rand_deg_poll_hist <- function () {hist(degree(random_net_poll), 
                                        xlim = c(0, 35),
)
  abline(v = mean(degree(g_poll)), col = "#FF4000", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_poll)), 2)),
       col = "#FF4000",
       cex = 1)
  text(x = max(degree(random_net_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


#### > Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_poll) / vcount(g_poll))

set.seed(54367)
scalefree_net_poll <- sample_pa(n = vcount(g_poll), m = m_estimate, directed = FALSE, zero.appeal = 0)
vcount(scalefree_net_poll)
ecount(scalefree_net_poll)
transitivity(scalefree_net_poll) 
components(scalefree_net_poll)
mean(degree(scalefree_net_poll))

#Visualisation 

sf_deg_poll_hist <- function() {hist(degree(scalefree_net_poll), xlim = c(0, max(degree(scalefree_net_poll))))
  abline(v = mean(degree(g_poll)), col = "#FF4000", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_poll)), 2)),
       col = "#FF4000",
       cex = 1)
  text(x = max(degree(scalefree_net_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}


# 
# # small world
# sw_net_poll <- sample_smallworld(dim = 1, size = vcount(g_poll), nei = floor(mean(degree(g_poll))/2), p = 0.1)
# 
# hist(degree(sw_net_poll))

x11()
par(mfrow = c(3,1))
og_deg_poll_hist()
rand_deg_poll_hist()
sf_deg_poll_hist()

#' OG network seem to resemble more a Random Network. 
#' Problem is that when we are attaching edges to the scale-free it is impossible to attach less than 1 edge per node, so our histograms becomes extremely skewed.
#' One possible solution would be that of randomly deleting some edges after constructing the network, but that might cause biases.
#' Generally, this type of analysis is easier when you have well connected networks



## 5.2 Plants ----------------------------------------------------------
### __ Degree - Top nodes ---------------------------------------------------------

# Can be changed to different metrics
# Extrapolate top nodes from graph -> if >10 then top 10, if not, max number of edges
edge_num_plant <- if (sum(plant_node_metrics$degree >= 1) > 10) {10} else {sum(!is.na(plant_node_metrics$degree_rank))}

top_nodes_plant <- plant_node_metrics %>%
  arrange(degree_rank) %>%
  slice(1:edge_num_plant) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

g_rank_plant_plot <- function() {
g_rank_plant <- plant_res_list$graph
nodes <- igraph::as_data_frame(g_rank_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rank_plant)$name) %>% 
  arrange(match(name, V(g_rank_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rank_plant)$name %in% top_nodes_plant, "#CC79A7", "#56B4E9")

plot(g_rank_plant,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rank_plant)$area_m2)/200, 
     vertex.label = V(g_rank_plant)$name,
     vertex.color = v_colours,
     vertex.label.cex = 1.2,              
     vertex.label.dist = 0.4,            
     edge.color = "#FF4000",
     edge.width = 2.5,
     asp = 0,
)
title(main = paste0("\n\n > Shrubs <\n d = ", round(plant_res_list$distance, 2)), cex.main = 2)
mtext(paste("Top nodes:", paste(top_nodes_plant, collapse = ", ")), side = 1, line = 1.5, cex = 1.5)
}

g_rank_plant_plot()

top_nodes_plant

# should be noted that the representation of the areas might confuse the viewer - even though nodes seem close enough to be connected we do not see a connection because the real distance is not short enough --> all connections in this case were created because the areas are adjacent/overlapping, which can not be seen due to scaling.

### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_plant <- vcount(g_plant) # number of nodes from graph
num_edges_plant <- ecount(g_plant) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_plant_mod <- plant_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_plant <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_plant), method = "configuration")
  mod_rand <- cluster_infomap(g_rand) #can be changed to other clustering functions 
  random_modularity_plant[i] <- modularity(mod_rand)
}
random_modularity_plant[is.nan(random_modularity_plant)] <- 0

####  > Visualisation -----------------------------------------------------

hist(random_modularity_plant, breaks = 30, col = "#A4C9EC",
     main = "Plant - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_plant, na.rm = TRUE),
              max(c(random_modularity_plant, g_plant_mod), na.rm = TRUE))
)
abline(v = g_plant_mod, col = "#FF4000", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "#FF4000", lwd = 2)

#### > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_plant <- (g_plant_mod - mean(random_modularity_plant, na.rm = TRUE)) / sd(random_modularity_plant, na.rm = TRUE)
print(paste("Plant - Z-score (Degree controlled):", round(z_score_plant, 5)))


###__ Degree distribution ------------------------------------
# This section aims to understand which degree distribution our network has to classify if the OG is closer to a random or scale-free network
#### > Original network --------------------------------------------------------
g_plant_plot()
vcount(g_plant)
ecount(g_plant)
transitivity(g_plant) # clustering coefficient
components(g_plant)
mean(degree(g_plant))

og_deg_plant_hist <- function() {hist(degree(g_plant), xlim = c(0, 35))
  text(x = max(degree(g_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


#### > Random network ----------------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_plant <- sample_correlated_gnp(g_plant, p = edge_density(g_plant), corr = 0.6)

vcount(random_net_plant)
ecount(random_net_plant)
transitivity(random_net_plant) 
components(random_net_plant)
mean(degree(random_net_plant))

#Visualisation 
rand_deg_plant_hist <- function () {hist(degree(random_net_plant), 
                                        xlim = c(0, 35),
)
  abline(v = mean(degree(g_plant)), col = "#FF4000", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_plant)), 2)),
       col = "#FF4000",
       cex = 1)
  text(x = max(degree(random_net_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


#### > Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_plant) / vcount(g_plant))

set.seed(54367)
scalefree_net_plant <- sample_pa(n = vcount(g_plant), m = 5, directed = FALSE, , zero.appeal = 0)
vcount(scalefree_net_plant)
ecount(scalefree_net_plant)
transitivity(scalefree_net_plant) 
components(scalefree_net_plant)
mean(degree(scalefree_net_plant))

#Visualisation 

sf_deg_plant_hist <- function() {hist(degree(scalefree_net_plant), xlim = c(0, max(degree(scalefree_net_plant))))
  abline(v = mean(degree(g_plant)), col = "#FF4000", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_plant)), 2)),
       col = "#FF4000",
       cex = 1)
  text(x = max(degree(scalefree_net_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_deg_plant_hist()
rand_deg_plant_hist()
sf_deg_plant_hist()

