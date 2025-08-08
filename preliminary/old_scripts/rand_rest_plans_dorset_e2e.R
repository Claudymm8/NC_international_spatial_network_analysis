# source("C:/ADD_PATH/dorset_e2e_cm.R")
 source("C:/ADD_PATH/targ_rest_plans_dorset_e2e.R")
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# /////////// 50% RANDOM RESTORATION PLANS ----------------------------------------------

# 0. Node selection  -----------------------------------------------------
# calculate number of nodes to keep
rest50_rand_num <- floor(length(df_sub_rank_poll_rest100$node)* 0.5)
rest50_select_rand <- sample(df_sub_rank_poll_rest100$node, rest50_rand_num)

# Subset total restoration DF to contain OG nodes and new random 50% 
df_patch_rest50_rand <- df_tot_rest %>% 
  filter(cluster == "south" |
      (cluster == "restored" & patch  %in% rest50_select_rand)
  )

df_patch_rest50_rand # this is the new DF to use 

# 1. RESTORED DISTANCE NETWORKS -----------------------------------------------------
## 1.1 Pollinator ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)
rest50_rand_pollinator_res_list <- euclidean_network_e2e(dist_poll, df_patch_rest50_rand)

#### __ 50% Random Rest Pollinators Network metrics -------------------------------------------------------

rest50_rand_poll_dist_metrics <-  data.frame(
  distance = rest50_rand_pollinator_res_list$distance,
  percent_connected = rest50_rand_pollinator_res_list$percent_connected,
  connectance = rest50_rand_pollinator_res_list$connectance,
  modularity = rest50_rand_pollinator_res_list$modularity,
  comp_number = rest50_rand_pollinator_res_list$comp_number, 
  comp_largest = rest50_rand_pollinator_res_list$largest_comp,
  comp_fraction = rest50_rand_pollinator_res_list$fraction_comp,
  from_net = "rest50_rand"
)

rest50_rand_poll_dist_metrics
str(rest50_rand_poll_dist_metrics)
# compare to original & 100% & non-random
poll_dist_metrics
rest50_poll_dist_metrics
rest100_poll_dist_metrics

#### __ 50% Random Rest Pollinator Node metrics  ---------------------------------------------------------
rest50_rand_poll_node_metrics <- data.frame(
  distance = rest50_rand_pollinator_res_list$distance,
  node = names(rest50_rand_pollinator_res_list$betweenness),
  betweenness = rest50_rand_pollinator_res_list$betweenness,
  closeness = rest50_rand_pollinator_res_list$closeness,
  degree = rest50_rand_pollinator_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest50_rand_poll_node_metrics, is.numeric)
rest50_rand_poll_node_metrics[numeric_cols] <- lapply(rest50_rand_poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest50_rand_poll_node_metrics
str(rest50_rand_poll_node_metrics)

#### __ 50% Random Rest Pollinator Network Vis --------------------------------------------------
g_rest50_rand_poll <- rest50_rand_pollinator_res_list$graph

g_rest50_rand_poll_plot <- function() {
  g_rest50_rand_poll <- rest50_rand_pollinator_res_list$graph
  nodes <- igraph::as_data_frame(g_rest50_rand_poll, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest50_rand_poll)$name) %>% 
    arrange(match(name, V(g_rest50_rand_poll)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_rest50_rand_poll,
       layout = layout_coords,
       vertex.size = sqrt(V(g_rest50_rand_poll)$area_m2)/300,
       # vertex.size = 1
       vertex.label = V(g_rest50_rand_poll)$name,
       vertex.color = "skyblue",
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.5, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Random Rest 50% - Pollinator Spatial Network, \n d = ", round(rest50_rand_pollinator_res_list$distance, 2)))
}

g_rest50_rand_poll_plot()


ecount(g_rest50_rand_poll)
ecount(g_poll) # just to compare
ecount(g_rest50_poll)
ecount(g_rest100_poll)

## 1.2 Plant ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)
rest50_rand_plant_res_list <- euclidean_network_e2e(dist_plant, df_patch_rest50_rand)

#### __ 50% Random Rest Plant Network metrics -------------------------------------------------------
rest50_rand_plant_dist_metrics <-  data.frame(
  distance = rest50_rand_plant_res_list$distance,
  percent_connected = rest50_rand_plant_res_list$percent_connected,
  connectance = rest50_rand_plant_res_list$connectance,
  modularity = rest50_rand_plant_res_list$modularity,
  comp_number = rest50_rand_plant_res_list$comp_number, 
  comp_largest = rest50_rand_plant_res_list$largest_comp,
  comp_fraction = rest50_rand_plant_res_list$fraction_comp,
  from_net = "rest50_rand"
)

# Change NaN --> 0
numeric_cols <- sapply(rest50_rand_plant_dist_metrics, is.numeric)
rest50_rand_plant_dist_metrics[numeric_cols] <- lapply(rest50_rand_plant_dist_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest50_rand_plant_dist_metrics
str(rest50_rand_plant_dist_metrics)
rest50_plant_dist_metrics
plant_dist_metrics # compare
rest100_plant_dist_metrics

#### __ 50% Random Rest Plant Node metrics  ---------------------------------------------------------
rest50_rand_plant_node_metrics <- data.frame(
  distance = rest50_rand_plant_res_list$distance,
  node = names(rest50_rand_plant_res_list$betweenness),
  betweenness = rest50_rand_plant_res_list$betweenness,
  closeness = rest50_rand_plant_res_list$closeness,
  degree = rest50_rand_plant_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest50_rand_plant_node_metrics, is.numeric)
rest50_rand_plant_node_metrics[numeric_cols] <- lapply(rest50_rand_plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest50_rand_plant_node_metrics
str(rest50_rand_plant_node_metrics)



#### __ 50% Random Rest Plant Network Vis -------------------------------------------------------
g_rest50_rand_plant <- rest50_rand_plant_res_list$graph

g_rest50_rand_plant_plot <- function() {
  g_rest50_rand_plant <- rest50_rand_plant_res_list$graph
  nodes <- igraph::as_data_frame(g_rest50_rand_plant, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest50_rand_plant)$name) %>% 
    arrange(match(name, V(g_rest50_rand_plant)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_rest50_rand_plant,
       layout = layout_coords,
       # vertex.size = 1,
       vertex.size = sqrt(V(g_rest50_rand_plant)$area_m2)/300, 
       vertex.label = V(g_rest50_rand_plant)$name,
       vertex.color = "skyblue",
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.6, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Random Rest 50% - Plant Spatial Network, \n d = ", round(plant_res_list$distance, 2)))
}

g_rest50_rand_plant_plot()

ecount(g_rest50_rand_plant)
ecount(g_rest50_plant)
ecount(g_rest100_plant)
ecount(g_plant)


# 1. 50% RANDOM REST ANALYSIS  ------------------------------------------------------------
## 1.1 Pollinators ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest50_rand_poll_node_metrics <- rest50_rand_poll_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA))



# Extrapolate number of top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_poll_rest50_rand <- if (sum(rest50_rand_poll_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest50_rand_poll_node_metrics$betweenness_rank))}


top_nodes_poll_rest50_rand <- rest50_rand_poll_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_poll_rest50_rand) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

g_btwn_rest50_rand_poll_plot <- function() {
  nodes <- igraph::as_data_frame(g_rest50_rand_poll, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest50_rand_poll)$name) %>% 
    arrange(match(name, V(g_rest50_rand_poll)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  # Assign different colours to top nodes 
  v_colours <- ifelse(V(g_rest50_rand_poll)$name %in% top_nodes_poll_rest50_rand, "pink", "skyblue")
  
  plot(g_rest50_rand_poll,
       layout = layout_coords,
       vertex.size = sqrt(V(g_rest50_rand_poll)$area_m2)/300, 
       vertex.label = V(g_rest50_rand_poll)$name,
       vertex.color = v_colours,
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.2,            
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Random 50% Rest Betweenness Network - Pollinators \n d = ", round(rest50_rand_pollinator_res_list$distance, 2)),
       sub = paste("Top nodes: ", paste(top_nodes_poll_rest50_rand, collapse = ", ")))
}

g_btwn_rest50_rand_poll_plot()


top_nodes_poll_rest100
top_nodes_poll_rest50_rand
top_nodes_poll_rest50
top_nodes_poll # for comparison
top_nodes_poll_rest50_rand %in% top_nodes_poll
top_nodes_poll_rest50_rand[top_nodes_poll_rest50_rand %in% top_nodes_poll]
top_nodes_poll_rest50_rand[top_nodes_poll_rest50_rand %in% top_nodes_poll_rest50]
# 4 nodes stay the same between the original and the 50% restored
# 4 nodes stay the same between the random and non-random 50% restored
# 5/10 nodes are new restored nodes 
top_nodes_poll_rest50_rand[top_nodes_poll_rest50_rand %in% top_nodes_poll_rest100]
# no node stayed between the 100% and 50% rest random plans

### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_poll_rest50_rand <- vcount(g_rest50_rand_poll) # number of nodes from graph
num_edges_poll_rest50_rand <- ecount(g_rest50_rand_poll) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_poll_mod_rest50_rand <- rest50_rand_poll_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_poll_rest50_rand <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest50_rand_poll), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_poll_rest50_rand[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_poll_rest50_rand, breaks = 30, col = "lightblue",
     main = "Rest 50% - Pollinators - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_poll_rest50_rand),
              max(c(random_modularity_poll_rest50_rand, g_poll_mod_rest50_rand)))
)
abline(v = g_poll_mod_rest50_rand, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_poll_rest50_rand <- (g_poll_mod_rest50_rand - mean(random_modularity_poll_rest50_rand, na.rm = TRUE)) / sd(random_modularity_poll_rest50_rand, na.rm = TRUE)
print(paste("Rest 50% Random - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest50_rand, 5)))
print(paste("Rest 50%  - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest50, 5)))
print(paste("Rest 100% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest100, 5)))
print(paste("OG - Pollinator - Z-score (Degree controlled):", round(z_score_poll, 5)))


## 5.2 Plants ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest50_rand_plant_node_metrics <- rest50_rand_plant_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA)) # , NA can also be changed to  depending on how you want to analyse data - 0 might bring up some nodes when not a lot of connections are created



# Extrapolate top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_plant_rest50_rand <- if (sum(rest50_rand_plant_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest50_rand_plant_node_metrics$betweenness_rank))}


top_nodes_plant_rest50_rand <- rest50_rand_plant_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_plant_rest50_rand) %>%
  pull(node) %>% 
  as.numeric %>% 
  sort()

g_btwn_rest50_rand_plant_plot <- function() {
  nodes <- igraph::as_data_frame(g_rest50_rand_plant, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest50_rand_plant)$name) %>% 
    arrange(match(name, V(g_rest50_rand_plant)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  # Assign different colours to top nodes 
  v_colours <- ifelse(V(g_rest50_rand_plant)$name %in% top_nodes_plant_rest50_rand, "pink", "skyblue")
  
  plot(g_rest50_rand_plant,
       layout = layout_coords,
       vertex.size = sqrt(V(g_rest50_rand_plant)$area_m2)/300, 
       vertex.label = V(g_rest50_rand_plant)$name,
       vertex.color = v_colours,
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.4,            
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("50% Random Rest Betweenness Network - Plant \n d = ", round(rest50_rand_plant_res_list$distance, 2)),
       sub = paste("Top Nodes: ", paste(top_nodes_plant_rest50_rand, collapse = ", ")))
}

g_btwn_rest50_rand_plant_plot()


top_nodes_plant_rest50_rand
top_nodes_plant
top_nodes_plant_rest50
top_nodes_plant_rest50_rand %in% top_nodes_plant
# We went from only 1 node (5edges) to 10 top nodes - nodes completely differ
# 7/10 top nodes are new restored nodes 
# 4 nodes stayed the same between random and non-random 50% 

top_nodes_plant_rest50_rand[top_nodes_plant_rest50_rand %in% top_nodes_plant_rest100]
# 1 node stayed between the 100% and 50% rest plans for plants
top_nodes_plant_rest50_rand[top_nodes_plant_rest50_rand %in% top_nodes_plant_rest50]
# 4 nodes stayed the same between rand and non-rand

top_nodes_plant_rest50_rand[top_nodes_plant_rest50_rand %in% top_nodes_poll_rest50_rand]
#Node 131 is top node across both plants and pollinators (50% - rest)


### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_plant_rest50_rand <- vcount(g_rest50_rand_plant) # number of nodes from graph
num_edges_plant_rest50_rand <- ecount(g_rest50_rand_plant) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_plant_mod_rest50_rand <- rest50_rand_plant_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_plant_rest50_rand <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest50_rand_plant), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_plant_rest50_rand[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_plant_rest50_rand, breaks = 30, col = "lightblue",
     main = "Random Rest 50% - Plant - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_plant_rest50_rand),
              max(c(random_modularity_plant_rest50_rand, g_plant_mod_rest50_rand)))
)
abline(v = g_plant_mod_rest50_rand, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_plant_rest50_rand <- (g_plant_mod_rest50_rand - mean(random_modularity_plant_rest50_rand, na.rm = TRUE)) / sd(random_modularity_plant_rest50_rand, na.rm = TRUE)

print(paste("Random Rest 50% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest50_rand, 5)))
print(paste("Rest 50% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest50, 5)))
print(paste("Rest 100% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest100, 5)))
print(paste("OG - Plant - Z-score (Degree controlled):", round(z_score_plant, 5)))


# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# /////////// 30% RANDOM RESTORATION PLANS ----------------------------------------------
# 0. Node selection  -----------------------------------------------------
rest30_rand_num <- floor(length(df_sub_rank_poll_rest100$node)* 0.3)
rest30_select_rand <- sample(df_sub_rank_poll_rest100$node, rest30_rand_num)

# Subset total restoration DF to contain OG nodes and new random 50% 
df_patch_rest30_rand <- df_tot_rest %>% 
  filter(cluster == "south" |
           (cluster == "restored" & patch  %in% rest30_select_rand)
  )


df_patch_rest30_rand # this is the new DF to use 

# NOTE: patches were selected from total pool and not existing 50% pool!  

# 1. RESTORED DISTANCE NETWORKS -----------------------------------------------------
## 1.1 Pollinator ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)
rest30_rand_pollinator_res_list <- euclidean_network_e2e(dist_poll, df_patch_rest30_rand)

#### __ 30% Random Rest Pollinators Network metrics -------------------------------------------------------

rest30_rand_poll_dist_metrics <-  data.frame(
  distance = rest30_rand_pollinator_res_list$distance,
  percent_connected = rest30_rand_pollinator_res_list$percent_connected,
  connectance = rest30_rand_pollinator_res_list$connectance,
  modularity = rest30_rand_pollinator_res_list$modularity,
  comp_number = rest30_rand_pollinator_res_list$comp_number, 
  comp_largest = rest30_rand_pollinator_res_list$largest_comp,
  comp_fraction = rest30_rand_pollinator_res_list$fraction_comp,
  from_net = "rest30_rand"
)

rest30_rand_poll_dist_metrics
rest50_rand_poll_dist_metrics
# str(rest30_rand_poll_dist_metrics)
# compare to original,50% & 100% 
poll_dist_metrics
rest30_poll_dist_metrics
rest100_poll_dist_metrics

#### __ 30% Random Rest Pollinator Node metrics  ---------------------------------------------------------
rest30_rand_poll_node_metrics <- data.frame(
  distance = rest30_rand_pollinator_res_list$distance,
  node = names(rest30_rand_pollinator_res_list$betweenness),
  betweenness = rest30_rand_pollinator_res_list$betweenness,
  closeness = rest30_rand_pollinator_res_list$closeness,
  degree = rest30_rand_pollinator_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest30_rand_poll_node_metrics, is.numeric)
rest30_rand_poll_node_metrics[numeric_cols] <- lapply(rest30_rand_poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest30_rand_poll_node_metrics
str(rest30_rand_poll_node_metrics)

#### __ 30% Random Rest Pollinator Network Vis --------------------------------------------------
g_rest30_rand_poll <- rest30_rand_pollinator_res_list$graph

g_rest30_rand_poll_plot <- function() {
  g_rest30_rand_poll <- rest30_rand_pollinator_res_list$graph
  nodes <- igraph::as_data_frame(g_rest30_rand_poll, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest30_rand_poll)$name) %>% 
    arrange(match(name, V(g_rest30_rand_poll)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_rest30_rand_poll,
       layout = layout_coords,
       vertex.size = sqrt(V(g_rest30_rand_poll)$area_m2)/300,
       # vertex.size = 1
       vertex.label = V(g_rest30_rand_poll)$name,
       vertex.color = "skyblue",
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.5, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Random Rest 30% - Pollinator Spatial Network, \n d = ", round(rest30_rand_pollinator_res_list$distance, 2)))
}

g_rest30_rand_poll_plot()

ecount(g_rest30_rand_poll)
ecount(g_rest30_poll)
ecount(g_poll) # just to compare
ecount(g_rest50_rand_poll)
ecount(g_rest100_poll)

## 1.2 Plant ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)
rest30_rand_plant_res_list <- euclidean_network_e2e(dist_plant, df_patch_rest30_rand)

#### __ 30% Random Rest Plant Network metrics -------------------------------------------------------
rest30_rand_plant_dist_metrics <-  data.frame(
  distance = rest30_rand_plant_res_list$distance,
  percent_connected = rest30_rand_plant_res_list$percent_connected,
  connectance = rest30_rand_plant_res_list$connectance,
  modularity = rest30_rand_plant_res_list$modularity,
  comp_number = rest30_rand_plant_res_list$comp_number, 
  comp_largest = rest30_rand_plant_res_list$largest_comp,
  comp_fraction = rest30_rand_plant_res_list$fraction_comp,
  from_net = "rest30_rand"
)

# Change NaN --> 0
numeric_cols <- sapply(rest30_rand_plant_dist_metrics, is.numeric)
rest30_rand_plant_dist_metrics[numeric_cols] <- lapply(rest30_rand_plant_dist_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest30_rand_plant_dist_metrics
rest30_plant_dist_metrics
# str(rest30_rand_plant_dist_metrics)
plant_dist_metrics # compare
rest50_rand_plant_dist_metrics
rest100_plant_dist_metrics

#### __ 30% Random Rest Plant Node metrics  ---------------------------------------------------------
rest30_rand_plant_node_metrics <- data.frame(
  distance = rest30_rand_plant_res_list$distance,
  node = names(rest30_rand_plant_res_list$betweenness),
  betweenness = rest30_rand_plant_res_list$betweenness,
  closeness = rest30_rand_plant_res_list$closeness,
  degree = rest30_rand_plant_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest30_rand_plant_node_metrics, is.numeric)
rest30_rand_plant_node_metrics[numeric_cols] <- lapply(rest30_rand_plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest30_rand_plant_node_metrics
str(rest30_rand_plant_node_metrics)



#### __ 30% Random Rest Plant Network Vis -------------------------------------------------------
g_rest30_rand_plant <- rest30_rand_plant_res_list$graph

g_rest30_rand_plant_plot <- function () {
  g_rest30_rand_plant <- rest30_rand_plant_res_list$graph
  nodes <- igraph::as_data_frame(g_rest30_rand_plant, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest30_rand_plant)$name) %>% 
    arrange(match(name, V(g_rest30_rand_plant)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_rest30_rand_plant,
       layout = layout_coords,
       # vertex.size = 1,
       vertex.size = sqrt(V(g_rest30_rand_plant)$area_m2)/300, 
       vertex.label = V(g_rest30_rand_plant)$name,
       vertex.color = "skyblue",
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.4, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Random Rest 30% - Plant Spatial Network, \n d = ", round(plant_res_list$distance, 2)))
}

g_rest30_rand_plant_plot() 

ecount(g_rest30_rand_plant)
ecount(g_rest30_plant)
ecount(g_rest50_rand_plant)
ecount(g_rest100_plant)
ecount(g_plant)
# intresting point --> random 50% is as efficient as 30% targeted at creating edges --> can achieve similar results if areas are carefully selected 

# 1. 30% RANDOM REST ANALYSIS  ------------------------------------------------------------
## 1.1 Pollinators ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest30_rand_poll_node_metrics <- rest30_rand_poll_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA))



# Extrapolate number of top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_poll_rest30_rand <- if (sum(rest30_rand_poll_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest30_rand_poll_node_metrics$betweenness_rank))}


top_nodes_poll_rest30_rand <- rest30_rand_poll_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_poll_rest30_rand) %>%
  pull(node) %>% 
  as.numeric %>% 
  sort()

g_btwn_rest30_rand_poll_plot <- function() {
  nodes <- igraph::as_data_frame(g_rest30_rand_poll, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest30_rand_poll)$name) %>% 
    arrange(match(name, V(g_rest30_rand_poll)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  # Assign different colours to top nodes 
  v_colours <- ifelse(V(g_rest30_rand_poll)$name %in% top_nodes_poll_rest30_rand, "pink", "skyblue")
  
  plot(g_rest30_rand_poll,
       layout = layout_coords,
       vertex.size = sqrt(V(g_rest30_rand_poll)$area_m2)/300, 
       vertex.label = V(g_rest30_rand_poll)$name,
       vertex.color = v_colours,
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.4,            
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Random 30% Rest Betweenness Network - Pollinators \n d = ", round(rest30_rand_pollinator_res_list$distance, 2)),
       sub = paste("Top nodes: ", paste(top_nodes_poll_rest30_rand, collapse = ", ")))
}

g_btwn_rest30_rand_poll_plot()

top_nodes_poll_rest30_rand
top_nodes_poll_rest100
top_nodes_poll # for comparison
top_nodes_poll_rest30_rand[top_nodes_poll_rest30_rand %in% top_nodes_poll]
# 2 node stayed the same between the original and the 30% restored
# 5/10 nodes are new restored nodes 
top_nodes_poll_rest30_rand[top_nodes_poll_rest30_rand %in% top_nodes_poll_rest100]
# 1 node between 100% and 30% random rest plans

### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_poll_rest30_rand <- vcount(g_rest30_rand_poll) # number of nodes from graph
num_edges_poll_rest30_rand <- ecount(g_rest30_rand_poll) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_poll_mod_rest30_rand <- rest30_rand_poll_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_poll_rest30_rand <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest30_rand_poll), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_poll_rest30_rand[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_poll_rest30_rand, breaks = 30, col = "lightblue",
     main = "Random Rest 30% - Pollinators - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_poll_rest30_rand),
              max(c(random_modularity_poll_rest30_rand, g_poll_mod_rest30_rand)))
)
abline(v = g_poll_mod_rest30_rand, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_poll_rest30_rand <- (g_poll_mod_rest30_rand - mean(random_modularity_poll_rest30_rand, na.rm = TRUE)) / sd(random_modularity_poll_rest30_rand, na.rm = TRUE)

print(paste("Random Rest 30% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest30_rand, 5)))
print(paste("Rest 30% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest30, 5)))
print(paste("Rest 100% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest100, 5)))
print(paste("OG - Pollinator - Z-score (Degree controlled):", round(z_score_poll, 5)))


## 5.2 Plants ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest30_rand_plant_node_metrics <- rest30_rand_plant_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA)) # , NA can also be changed to  depending on how you want to analyse data - 0 might bring up some nodes when not a lot of connections are created



# Extrapolate top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_plant_rest30_rand <- if (sum(rest30_rand_plant_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest30_rand_plant_node_metrics$betweenness_rank))}


top_nodes_plant_rest30_rand <- rest30_rand_plant_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_plant_rest30_rand) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort

g_btwn_rest30_rand_plant_plot <- function () {
  nodes <- igraph::as_data_frame(g_rest30_rand_plant, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_rest30_rand_plant)$name) %>% 
    arrange(match(name, V(g_rest30_rand_plant)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  # Assign different colours to top nodes 
  v_colours <- ifelse(V(g_rest30_rand_plant)$name %in% top_nodes_plant_rest30_rand, "pink", "skyblue")
  
  plot(g_rest30_rand_plant,
       layout = layout_coords,
       vertex.size = sqrt(V(g_rest30_rand_plant)$area_m2)/300, 
       vertex.label = V(g_rest30_rand_plant)$name,
       vertex.color = v_colours,
       vertex.label.cex = 0.8,              
       vertex.label.dist = 0.4,            
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Random 30% Rest Betweenness Network - Plants \n d = ", round(rest30_rand_plant_res_list$distance, 2)),
       sub = paste("Top nodes: ", paste(top_nodes_plant_rest30_rand, collapse = ", ")))
}

g_btwn_rest30_rand_plant_plot()

top_nodes_plant_rest30_rand
top_nodes_plant
top_nodes_plant_rest30_rand %in% top_nodes_plant
# We went from only 1 nodes to 10 top nodes - nodes completely differ
# 5/10 top nodes are new restored nodes 

top_nodes_plant_rest30_rand[top_nodes_plant_rest30_rand %in% top_nodes_plant_rest100]
# 147 stayed between the 100% and 30% rest plans for plants

top_nodes_plant_rest30_rand[top_nodes_plant_rest30_rand %in% top_nodes_poll_rest30_rand]
# no top node across plants and pollinators (30% - random rest)


### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_plant_rest30_rand <- vcount(g_rest30_rand_plant) # number of nodes from graph
num_edges_plant_rest30_rand <- ecount(g_rest30_rand_plant) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_plant_mod_rest30_rand <- rest30_rand_plant_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_plant_rest30_rand <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest30_rand_plant), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_plant_rest30_rand[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_plant_rest30_rand, breaks = 30, col = "lightblue",
     main = "Random Rest 30% - Plant - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_plant_rest30_rand),
              max(c(random_modularity_plant_rest30_rand, g_plant_mod_rest30_rand)))
)
abline(v = g_plant_mod_rest30_rand, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_plant_rest30_rand <- (g_plant_mod_rest30_rand - mean(random_modularity_plant_rest30_rand, na.rm = TRUE)) / sd(random_modularity_plant_rest30_rand, na.rm = TRUE)

print(paste("Random Rest 30% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest30_rand, 5)))
# First time the network is significantly random !! 
print(paste("Rest 30% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest30, 5)))
print(paste("Rest 100% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest100, 5)))
print(paste("OG - Plant - Z-score (Degree controlled):", round(z_score_plant, 5)))





# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# FINAL VISUALISATION -----------------------------------------------------
## 1. Data frames - Metrics --------------------------------------------------------------------
### > Pollinators ---------------------------------------------------
full_comp_poll_rand <- poll_dist_metrics %>% 
  rbind(rest30_rand_poll_dist_metrics, rest50_rand_poll_dist_metrics, rest100_poll_dist_metrics)

# long format for the data so that they can all be plotted in the same graph 
# from_net = factor() makes sure that the networks are presented in order
full_comp_poll_rand_long <- full_comp_poll_rand %>%
  mutate(percent_connected = percent_connected/100, 
         from_net = factor(from_net, levels = c("original", "rest30_rand", "rest50_rand", "rest100"))) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")

### > Plants --------------------------------------------------------
full_comp_plant_rand <- plant_dist_metrics %>% 
  rbind(rest30_rand_plant_dist_metrics, rest50_rand_plant_dist_metrics, rest100_plant_dist_metrics)

# long format for the data so that they can all be plotted in the same graph 
# from_net = factor() makes sure that the netowrks re presented in order
full_comp_plant_rand_long <- full_comp_plant_rand %>%
  mutate(percent_connected = percent_connected/100, 
         from_net = factor(from_net, levels = c("original", "rest30_rand", "rest50_rand", "rest100"))) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")


## 2. Visualisation -----------------------------------------------------------
### 2.1 Networks ----------------------------------------------------------------
### > Pollinators -----------------------------------------------------------
x11()
par(mfrow = c(2,2))
g_poll_plot() # OG pollinator network 
g_rest100_poll_plot() # 100% rest
g_rest50_rand_poll_plot() # 50% rest
g_rest30_rand_poll_plot() # 30% rest

#Random v Targeted 
x11()
par(mfrow = c(2,2))
g_rest50_poll_plot() # 50% rand rest
g_rest30_poll_plot() # 30% rand rest
g_rest50_rand_poll_plot() # 50% rand rest
g_rest30_rand_poll_plot() # 30% rand rest

#poll betweenness
x11()
par(mfrow = c(2,2))
g_btwn_poll_plot() # OG
g_btwn_rest100_poll_plot() # 100% rest
g_btwn_rest50_rand_poll_plot() # 50% rest
g_btwn_rest30_rand_poll_plot() # 30% rest

#Random v Targeted btwn
x11()
par(mfrow = c(2,2))
g_btwn_rest50_poll_plot() # 50% rest
g_btwn_rest30_poll_plot() # 30% rest
g_btwn_rest50_rand_poll_plot() # 50% rand rest
g_btwn_rest30_rand_poll_plot() # 30% rand rest

### > Plants -----------------------------------------------------------
x11()
par(mfrow = c(2,2))
g_plant_plot() # OG plant network 
g_rest100_plant_plot() # 100% rest
g_rest50_rand_plant_plot() # 30% rest
g_rest30_rand_plant_plot() # 50% rest

#Random v Targeted 
x11()
par(mfrow = c(2,2))
g_rest50_plant_plot() # 50% rand rest
g_rest30_plant_plot() # 30% rand rest
g_rest50_rand_plant_plot() # 50% rand rest
g_rest30_rand_plant_plot() # 30% rand rest

#plant betwenness
x11()
par(mfrow = c(2,2))
g_btwn_plant_plot() # OG
g_btwn_rest100_plant_plot() # 100%
g_btwn_rest50_rand_plant_plot() # 50% rest
g_btwn_rest30_rand_plant_plot() # 30% rest 

#Random v Targeted btwn
x11()
par(mfrow = c(2,2))
g_btwn_rest50_plant_plot() # 50% rest
g_btwn_rest30_plant_plot() # 30% rest
g_btwn_rest50_rand_plant_plot() # 50% rand rest
g_btwn_rest30_rand_plant_plot() # 30% rand rest

### 2.2 Metrics  ----------------------------------------------------------------
### > Pollinators -----------------------------------------------------------
rand_rest_poll_metrics_plot <- ggplot(full_comp_poll_rand_long, aes(x = from_net, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 2) +
  labs(title = "Changes in Network Metrics in Different Random Restoration Scenarios\n > Pollinators <",
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
  theme_bw() +  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right")


### > Plants -----------------------------------------------------------
rand_rest_plant_metrics_plot <- ggplot(full_comp_plant_rand_long, aes(x = from_net, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 2) +
  labs(title = "Changes in Network Metrics in Different Random Restoration Scenarios \n > Plants (Shrubs) <",
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
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right")


### > Together ----------------------------------------------------------------
rand_rest_poll_metrics_plot + rand_rest_plant_metrics_plot


### > Random vs Targeted ------------------------------------------------------
#### _ Pollinators -------------------------------------------------------------
rvt_comp_poll <- poll_dist_metrics %>% 
  rbind(rest30_poll_dist_metrics, rest30_rand_poll_dist_metrics, rest50_rand_poll_dist_metrics, rest50_poll_dist_metrics, rest100_poll_dist_metrics)


#add a column with a percentage that can be plotted 
rvt_comp_poll <- rvt_comp_poll %>%
  mutate(restoration_perc = case_when(
    from_net == "original"     ~ 0,
    from_net %in% c("rest30", "rest30_rand") ~ 30,
    from_net %in% c("rest50", "rest50_rand") ~ 50,
    from_net == "rest100"      ~ 100,
  )) %>% 
  mutate(rest_type = ifelse(grepl("_rand$", from_net), "rand", "targ"))

extra_0_100 <- rvt_comp_poll %>% 
  filter(from_net %in% c("original", "rest100")) %>%
  slice(rep(1:n(), each = 2)) %>%
  mutate(rest_type = rep(c("rand", "targ"), times = n() / 2))

# bind them together - first delete original 0 and 100
rvt_comp_poll <- rvt_comp_poll %>% 
  filter(!from_net %in% c("original", "rest100")) %>%
  rbind(extra_0_100)

#create long version for visualisation 
rvt_comp_poll_long <- rvt_comp_poll %>%
  mutate(percent_connected = percent_connected/100) %>% 
  pivot_longer(
    cols = c(percent_connected, connectance, modularity, comp_fraction),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    rest_type = factor(rest_type, levels = c("targ", "rand"), labels = c("Targeted", "Random")),
    metric = factor(metric,
                    levels = c("percent_connected", "connectance", "modularity", "comp_fraction"),
                    labels = c("Percent Connected", "Connectivity", "Modularity", "Component Fraction"))
  )

 

rvt_rest_poll_metrics_plot <- ggplot(rvt_comp_poll_long, aes(x = restoration_perc, y = value, color = metric, linetype = rest_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Changes in Network Metrics Across Different Restoration Scenarios \n > Pollinators < \n > Random vs Targeted <",
    x = "Percent Restored",
    y = "Metric Value",
    color = "Metric",
    linetype = "Restoration Type"
  ) +
  scale_x_continuous(breaks = c(0, 30, 50, 100)) +
  scale_color_manual(values = c(
    "Percent Connected" = "#1b9e77",
    "Connectivity" = "#d95f02",
    "Modularity" = "#7570b3",
    "Component Fraction" = "#e7298a"
  )) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

rvt_rest_poll_metrics_plot

#### _ Plants -------------------------------------------------------------
rvt_comp_plant <- plant_dist_metrics %>% 
  rbind(rest30_plant_dist_metrics, rest30_rand_plant_dist_metrics, rest50_rand_plant_dist_metrics, rest50_plant_dist_metrics, rest100_plant_dist_metrics)


#add a column with a percentage that can be plotted 
rvt_comp_plant <- rvt_comp_plant %>%
  mutate(restoration_perc = case_when(
    from_net == "original"     ~ 0,
    from_net %in% c("rest30", "rest30_rand") ~ 30,
    from_net %in% c("rest50", "rest50_rand") ~ 50,
    from_net == "rest100"      ~ 100,
  )) %>% 
  mutate(rest_type = ifelse(grepl("_rand$", from_net), "rand", "targ"))

extra_0_100 <- rvt_comp_plant %>% 
  filter(from_net %in% c("original", "rest100")) %>%
  slice(rep(1:n(), each = 2)) %>%
  mutate(rest_type = rep(c("rand", "targ"), times = n() / 2))

# bind them together - first delete original 0 and 100
rvt_comp_plant <- rvt_comp_plant %>% 
  filter(!from_net %in% c("original", "rest100")) %>%
  rbind(extra_0_100)

#create long version for visualisation 
rvt_comp_plant_long <- rvt_comp_plant %>%
  mutate(percent_connected = percent_connected/100) %>% 
  pivot_longer(
    cols = c(percent_connected, connectance, modularity, comp_fraction),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    rest_type = factor(rest_type, levels = c("targ", "rand"), labels = c("Targeted", "Random")),
    metric = factor(metric,
                    levels = c("percent_connected", "connectance", "modularity", "comp_fraction"),
                    labels = c("Percent Connected", "Connectivity", "Modularity", "Component Fraction"))
  )



rvt_rest_plant_metrics_plot <- ggplot(rvt_comp_plant_long, aes(x = restoration_perc, y = value, color = metric, linetype = rest_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Changes in Network Metrics Across Different Restoration Scenarios \n > Plants (Shrubs) < \n > Random vs Targeted <",
    x = "Percent Restored",
    y = "Metric Value",
    color = "Metric",
    linetype = "Restoration Type"
  ) +
  scale_x_continuous(breaks = c(0, 30, 50, 100)) +
  scale_color_manual(values = c(
    "Percent Connected" = "#1b9e77",
    "Connectivity" = "#d95f02",
    "Modularity" = "#7570b3",
    "Component Fraction" = "#e7298a"
  )) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )



x11()
rvt_rest_poll_metrics_plot + rvt_rest_plant_metrics_plot

### z score comparison! 

