# # RUN FIRST OTHER SCRIPT --> dorset_e2e.R
 source("C:/ADD_PATH/dorset_e2e.R")
## ___ Restoration Data ----------------------------------------------------
df_patch_rest <- read.csv("C:/ADD_PATH/heaths_dorset_restore.csv")
str(df_patch_rest)
range(df_patch_rest$patch)
max_patch_s <- max(range(df_patch_s$patch))
start_patch_num <- max_patch_s +1

df_patch_rest$patch <- seq(start_patch_num, length.out = nrow(df_patch_rest))
df_patch_rest$cluster <- as.factor(df_patch_rest$cluster)

#Create a DF with all patches, restored and original
df_tot_rest <- rbind(df_patch_s, df_patch_rest)

ggplot(data=df_tot_rest, 
       aes(x=longitude, y=latitude, size=area_m2, col=cluster)) +
  geom_point() +
  geom_text(aes(label=patch), hjust=-0.1, vjust=-0.1, col="grey", size=4) +
  coord_fixed() +
  labs(size="Patch area\n(m2)") +
  theme(axis.title=element_blank(), axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background=element_rect(fill="white", colour="grey"),
        panel.grid.major=element_line(colour="grey"),
        legend.position="bottom")

# /////////// 100% RESTORATION PLANS ----------------------------------------------
# 1. RESTORED DISTANCE NETWORKS -----------------------------------------------------
## 1.1 Pollinator ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)
rest100_pollinator_res_list <- euclidean_network_e2e(dist_poll, df_tot_rest)

#### __ 100% Rest Pollinators Network metrics -------------------------------------------------------

rest100_poll_dist_metrics <-  data.frame(
  distance = rest100_pollinator_res_list$distance,
  percent_connected = rest100_pollinator_res_list$percent_connected,
  connectance = rest100_pollinator_res_list$connectance,
  modularity = rest100_pollinator_res_list$modularity,
  comp_number = rest100_pollinator_res_list$comp_number, 
  comp_largest = rest100_pollinator_res_list$largest_comp,
  comp_fraction = rest100_pollinator_res_list$fraction_comp
)

rest100_poll_dist_metrics
str(rest100_poll_dist_metrics)
# compare to original 
poll_dist_metrics

#### __ 100% Rest Pollinator Node metrics  ---------------------------------------------------------
rest100_poll_node_metrics <- data.frame(
  distance = rest100_pollinator_res_list$distance,
  node = names(rest100_pollinator_res_list$betweenness),
  betweenness = rest100_pollinator_res_list$betweenness,
  closeness = rest100_pollinator_res_list$closeness,
  degree = rest100_pollinator_res_list$degree,
  bridge = rest100_pollinator_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(rest100_poll_node_metrics, is.numeric)
rest100_poll_node_metrics[numeric_cols] <- lapply(rest100_poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest100_poll_node_metrics
str(rest100_poll_node_metrics)

#### __ 100% Rest Pollinator Network Vis --------------------------------------------------
g_rest100_poll <- rest100_pollinator_res_list$graph

g_rest100_poll_plot <- function() {
g_rest100_poll <- rest100_pollinator_res_list$graph
nodes <- igraph::as_data_frame(g_rest100_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest100_poll)$name) %>% 
  arrange(match(name, V(g_rest100_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_rest100_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest100_poll)$area_m2)/300,
     # vertex.size = 1
     vertex.label = V(g_rest100_poll)$name,
     vertex.color = "skyblue",
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.5, 
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("Rest 100% - Pollinator Spatial Network, \n d = ", round(rest100_pollinator_res_list$distance, 2)))
}

g_rest100_poll_plot()

ecount(g_rest100_poll)
ecount(g_poll) # just to compare


## 1.2 Plant ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <-mean(tot_jbdf_metrics$mean_dist)
rest100_plant_res_list <- euclidean_network_e2e(dist_plant, df_tot_rest)

#### __ 100% Rest Plant Network metrics -------------------------------------------------------
rest100_plant_dist_metrics <-  data.frame(
  distance = rest100_plant_res_list$distance,
  percent_connected = rest100_plant_res_list$percent_connected,
  connectance = rest100_plant_res_list$connectance,
  modularity = rest100_plant_res_list$modularity,
  comp_number = rest100_plant_res_list$comp_number, 
  comp_largest = rest100_plant_res_list$largest_comp,
  comp_fraction = rest100_plant_res_list$fraction_comp
)

# Change NaN --> 0
numeric_cols <- sapply(rest100_plant_dist_metrics, is.numeric)
rest100_plant_dist_metrics[numeric_cols] <- lapply(rest100_plant_dist_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest100_plant_dist_metrics
str(rest100_plant_dist_metrics)
plant_dist_metrics # compare

#### __ 100% Rest Plant Node metrics  ---------------------------------------------------------
rest100_plant_node_metrics <- data.frame(
  distance = rest100_plant_res_list$distance,
  node = names(rest100_plant_res_list$betweenness),
  betweenness = rest100_plant_res_list$betweenness,
  closeness = rest100_plant_res_list$closeness,
  degree = rest100_plant_res_list$degree,
  bridge = rest100_plant_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(rest100_plant_node_metrics, is.numeric)
rest100_plant_node_metrics[numeric_cols] <- lapply(rest100_plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest100_plant_node_metrics
str(rest100_plant_node_metrics)



#### __ 100% Rest Plant Network Vis -------------------------------------------------------
g_rest100_plant <- rest100_plant_res_list$graph

g_rest100_plant_plot <- function() {
g_rest100_plant <- rest100_plant_res_list$graph
nodes <- igraph::as_data_frame(g_rest100_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest100_plant)$name) %>% 
  arrange(match(name, V(g_rest100_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_rest100_plant,
     layout = layout_coords,
     # vertex.size = 1,
     vertex.size = sqrt(V(g_rest100_plant)$area_m2)/300, 
     vertex.label = V(g_rest100_plant)$name,
     vertex.color = "skyblue",
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.5, 
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("Rest 100% - Plant Spatial Network, \n d = ", round(plant_res_list$distance, 2)))
}

g_rest100_plant_plot()

ecount(g_rest100_plant)
ecount(g_plant)


# 1. 100% REST ANALYSIS  ------------------------------------------------------------
## 1.1 Pollinators ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest100_poll_node_metrics <- rest100_poll_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA))



# Extrapolate top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_poll_rest100 <- if (sum(rest100_poll_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest100_poll_node_metrics$betweenness_rank))}


top_nodes_poll_rest100 <- rest100_poll_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_poll_rest100) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

g_btwn_rest100_poll_plot <- function() {
nodes <- igraph::as_data_frame(g_rest100_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest100_poll)$name) %>% 
  arrange(match(name, V(g_rest100_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rest100_poll)$name %in% top_nodes_poll_rest100, "pink", "skyblue")

plot(g_rest100_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest100_poll)$area_m2)/300, 
     vertex.label = V(g_rest100_poll)$name,
     vertex.color = v_colours,
     vertex.label.cex = 1,              
     vertex.label.dist = 0.2,            
     edge.color = "red",
     edge.width = 1,
     asp = 0,
     main = paste0("100% Rest Betweenness Network - Pollinators \n d = ", round(rest100_pollinator_res_list$distance, 2)),
     sub = paste("Top nodes: ", paste(top_nodes_poll_rest100, collapse = ", ")))
}

g_btwn_rest100_poll_plot()

top_nodes_poll_rest100
top_nodes_poll # for comparison
top_nodes_poll_rest100 %in% top_nodes_poll
# only 1 node stays the same between the original and the 100% restored (#29)
# 5/10 nodes are new restored nodes 

### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_poll_rest100 <- vcount(g_rest100_poll) # number of nodes from graph
num_edges_poll_rest100 <- ecount(g_rest100_poll) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_poll_mod_rest100 <- rest100_poll_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_poll_rest100 <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest100_poll), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_poll_rest100[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_poll_rest100, breaks = 30, col = "lightblue",
     main = "Pollinators - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_poll_rest100),
            max(c(random_modularity_poll_rest100, g_poll_mod_rest100)))
)
abline(v = g_poll_mod_rest100, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_poll_rest100 <- (g_poll_mod_rest100 - mean(random_modularity_poll_rest100, na.rm = TRUE)) / sd(random_modularity_poll_rest100, na.rm = TRUE)
print(paste("Rest 100% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest100, 5)))
print(paste("OG - Pollinator - Z-score (Degree controlled):", round(z_score_poll, 5)))


## 5.2 Plants ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest100_plant_node_metrics <- rest100_plant_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA)) # , NA can also be changed to  depending on how you want to analyse data - 0 might bring up some nodes when not a lot of connections are created


# Extrapolate top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_plant_rest100 <- if (sum(rest100_plant_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest100_plant_node_metrics$betweenness_rank))}


top_nodes_plant_rest100 <- rest100_plant_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_plant_rest100) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

g_btwn_rest100_plant_plot <- function() {
nodes <- igraph::as_data_frame(g_rest100_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest100_plant)$name) %>% 
  arrange(match(name, V(g_rest100_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rest100_plant)$name %in% top_nodes_plant_rest100, "pink", "skyblue")

plot(g_rest100_plant,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest100_plant)$area_m2)/300, 
     vertex.label = V(g_rest100_plant)$name,
     vertex.color = v_colours,
     vertex.label.cex = 1,              
     vertex.label.dist = 0.2,            
     edge.color = "red",
     edge.width = 1,
     asp = 0,
     main = paste0("100% Rest Betweenness Network - Plant \n d = ", round(rest100_plant_res_list$distance, 2)),
     sub = paste("Top nodes: ", paste(top_nodes_plant_rest100, collapse = ", ")))
}

g_btwn_rest100_plant_plot()

top_nodes_plant_rest100
top_nodes_plant
top_nodes_plant_rest100 %in% top_nodes_plant
# We went from only 2 nodes (5edges) to 10 top nodes - nodes completely differ
# 7/10 top nodes are new restored nodes 
top_nodes_plant_rest100[top_nodes_plant_rest100 %in% top_nodes_poll_rest100]
#Node 186 is a top node across both plants and pollinators (100% - rest)


### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_plant_rest100 <- vcount(g_rest100_plant) # number of nodes from graph
num_edges_plant_rest100 <- ecount(g_rest100_plant) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_plant_mod_rest100 <- rest100_plant_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_plant_rest100 <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest100_plant), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_plant_rest100[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_plant_rest100, breaks = 30, col = "lightblue",
     main = "Rest 100% - Plant - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_plant_rest100),
              max(c(random_modularity_plant_rest100, g_plant_mod_rest100)))
)
abline(v = g_plant_mod_rest100, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_plant_rest100 <- (g_plant_mod_rest100 - mean(random_modularity_plant_rest100, na.rm = TRUE)) / sd(random_modularity_plant_rest100, na.rm = TRUE)
print(paste("Rest 100% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest100, 5)))
print(paste("OG - Plant - Z-score (Degree controlled):", round(z_score_plant, 5)))

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# /////////// 50% RESTORATION PLANS ----------------------------------------------

# 0. Node selection  -----------------------------------------------------
# subset only restored nodes from DF with ranks on them
# The following creates 2 different top nodes for poll and plants separately
df_sub_rank_poll_rest100 <- rest100_poll_node_metrics %>% 
  filter(as.numeric(node) >= 116 & as.numeric(node) <= 200)

rest50_num <- floor(nrow(df_sub_rank_poll_rest100) / 2)

rest50_node_select_poll <- df_sub_rank_poll_rest100 %>%
  arrange(betweenness_rank) %>%
  slice(1:rest50_num) %>%
  pull(node)

df_sub_rank_plant_rest100 <- rest100_plant_node_metrics %>% 
  filter(as.numeric(node) >= 116 & as.numeric(node) <= 200)

rest50_node_select_plant <- df_sub_rank_plant_rest100 %>%
  arrange(betweenness_rank) %>%
  slice(1:rest50_num) %>%
  pull(node)

rest50_node_select_poll %in% rest50_node_select_plant

# Create communal ranking - accounts for both plant and pollinator rankings and picks the top 50% of both of them 
# create new DF with both nodes and ranks
df_comm_rank <- inner_join(
  df_sub_rank_poll_rest100 %>% 
    select(node, betweenness_rank) %>% 
    rename(btwn_rank_poll = betweenness_rank),
  df_sub_rank_plant_rest100 %>% 
    select(node, betweenness_rank) %>% 
    rename(btwn_rank_plant = betweenness_rank),
  by = "node") 

#Combines the ranking and pulls the top 50%
rest50_comb_top_nodes <- df_comm_rank %>% 
  mutate(comb_rank = btwn_rank_poll + btwn_rank_plant) %>% 
  arrange(comb_rank) %>%
  slice(1:rest50_num) %>%
  pull(node)

# Subset total restoration DF to contain OG nodes and new 50% 
df_patch_rest50 <- df_tot_rest %>% 
  filter(
    cluster == "south" |
    (cluster == "restored" & patch  %in% rest50_comb_top_nodes)
  )

df_patch_rest50 # this is the new DF to use 

# 1. RESTORED DISTANCE NETWORKS -----------------------------------------------------
## 1.1 Pollinator ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)
rest50_pollinator_res_list <- euclidean_network_e2e(dist_poll, df_patch_rest50)

#### __ 50% Rest Pollinators Network metrics -------------------------------------------------------

rest50_poll_dist_metrics <-  data.frame(
  distance = rest50_pollinator_res_list$distance,
  percent_connected = rest50_pollinator_res_list$percent_connected,
  connectance = rest50_pollinator_res_list$connectance,
  modularity = rest50_pollinator_res_list$modularity,
  comp_number = rest50_pollinator_res_list$comp_number, 
  comp_largest = rest50_pollinator_res_list$largest_comp,
  comp_fraction = rest50_pollinator_res_list$fraction_comp
)

rest50_poll_dist_metrics
str(rest50_poll_dist_metrics)
# compare to original & 100% 
poll_dist_metrics
rest100_poll_dist_metrics

#### __ 50% Rest Pollinator Node metrics  ---------------------------------------------------------
rest50_poll_node_metrics <- data.frame(
  distance = rest50_pollinator_res_list$distance,
  node = names(rest50_pollinator_res_list$betweenness),
  betweenness = rest50_pollinator_res_list$betweenness,
  closeness = rest50_pollinator_res_list$closeness,
  degree = rest50_pollinator_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest50_poll_node_metrics, is.numeric)
rest50_poll_node_metrics[numeric_cols] <- lapply(rest50_poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest50_poll_node_metrics
str(rest50_poll_node_metrics)

#### __ 50% Rest Pollinator Network Vis --------------------------------------------------
g_rest50_poll <- rest50_pollinator_res_list$graph

g_rest50_poll_plot <- function() {
g_rest50_poll <- rest50_pollinator_res_list$graph
nodes <- igraph::as_data_frame(g_rest50_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest50_poll)$name) %>% 
  arrange(match(name, V(g_rest50_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_rest50_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest50_poll)$area_m2)/300,
     # vertex.size = 1
     vertex.label = V(g_rest50_poll)$name,
     vertex.color = "skyblue",
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.5, 
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("Rest 50% - Pollinator Spatial Network, \n d = ", round(rest50_pollinator_res_list$distance, 2)))
}

g_rest50_poll_plot()


ecount(g_rest50_poll)
ecount(g_poll) # just to compare
ecount(g_rest100_poll)

## 1.2 Plant ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)
rest50_plant_res_list <- euclidean_network_e2e(dist_plant, df_patch_rest50)

#### __ 50% Rest Plant Network metrics -------------------------------------------------------
rest50_plant_dist_metrics <-  data.frame(
  distance = rest50_plant_res_list$distance,
  percent_connected = rest50_plant_res_list$percent_connected,
  connectance = rest50_plant_res_list$connectance,
  modularity = rest50_plant_res_list$modularity,
  comp_number = rest50_plant_res_list$comp_number, 
  comp_largest = rest50_plant_res_list$largest_comp,
  comp_fraction = rest50_plant_res_list$fraction_comp
)

# Change NaN --> 0
numeric_cols <- sapply(rest50_plant_dist_metrics, is.numeric)
rest50_plant_dist_metrics[numeric_cols] <- lapply(rest50_plant_dist_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest50_plant_dist_metrics
str(rest50_plant_dist_metrics)
plant_dist_metrics # compare
rest100_plant_dist_metrics

#### __ 50% Rest Plant Node metrics  ---------------------------------------------------------
rest50_plant_node_metrics <- data.frame(
  distance = rest50_plant_res_list$distance,
  node = names(rest50_plant_res_list$betweenness),
  betweenness = rest50_plant_res_list$betweenness,
  closeness = rest50_plant_res_list$closeness,
  degree = rest50_plant_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest50_plant_node_metrics, is.numeric)
rest50_plant_node_metrics[numeric_cols] <- lapply(rest50_plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest50_plant_node_metrics
str(rest50_plant_node_metrics)



#### __ 50% Rest Plant Network Vis -------------------------------------------------------
g_rest50_plant <- rest50_plant_res_list$graph

g_rest50_plant_plot <- function() {
g_rest50_plant <- rest50_plant_res_list$graph
nodes <- igraph::as_data_frame(g_rest50_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest50_plant)$name) %>% 
  arrange(match(name, V(g_rest50_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_rest50_plant,
     layout = layout_coords,
     # vertex.size = 1,
     vertex.size = sqrt(V(g_rest50_plant)$area_m2)/300, 
     vertex.label = V(g_rest50_plant)$name,
     vertex.color = "skyblue",
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.6, 
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("Rest 50% - Plant Spatial Network, \n d = ", round(plant_res_list$distance, 2)))
}

g_rest50_plant_plot()

ecount(g_rest50_plant)
ecount(g_rest100_plant)
ecount(g_plant)


# 1. 50% REST ANALYSIS  ------------------------------------------------------------
## 1.1 Pollinators ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest50_poll_node_metrics <- rest50_poll_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA))



# Extrapolate number of top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_poll_rest50 <- if (sum(rest50_poll_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest50_poll_node_metrics$betweenness_rank))}


top_nodes_poll_rest50 <- rest50_poll_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_poll_rest50) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

g_btwn_rest50_poll_plot <- function() {
nodes <- igraph::as_data_frame(g_rest50_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest50_poll)$name) %>% 
  arrange(match(name, V(g_rest50_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rest50_poll)$name %in% top_nodes_poll_rest50, "pink", "skyblue")

plot(g_rest50_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest50_poll)$area_m2)/300, 
     vertex.label = V(g_rest50_poll)$name,
     vertex.color = v_colours,
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.2,            
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("50% Rest Betweenness Network - Pollinators \n d = ", round(rest50_pollinator_res_list$distance, 2)),
     sub = paste("Top nodes: ", paste(top_nodes_poll_rest50, collapse = ", ")))
}

g_btwn_rest50_poll_plot()


top_nodes_poll_rest100
top_nodes_poll_rest50
top_nodes_poll # for comparison
top_nodes_poll_rest50 %in% top_nodes_poll
top_nodes_poll_rest50[top_nodes_poll_rest50 %in% top_nodes_poll]
# 3 nodes stay the same between the original and the 50% restored
# 5/10 nodes are new restored nodes 
top_nodes_poll_rest50[top_nodes_poll_rest50 %in% top_nodes_poll_rest100]
# 149 is the only node which stayed between the 100% and 50% rest plans

### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_poll_rest50 <- vcount(g_rest50_poll) # number of nodes from graph
num_edges_poll_rest50 <- ecount(g_rest50_poll) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_poll_mod_rest50 <- rest50_poll_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_poll_rest50 <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest50_poll), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_poll_rest50[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_poll_rest50, breaks = 30, col = "lightblue",
     main = "Rest 50% - Pollinators - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_poll_rest50),
              max(c(random_modularity_poll_rest50, g_poll_mod_rest50)))
)
abline(v = g_poll_mod_rest50, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_poll_rest50 <- (g_poll_mod_rest50 - mean(random_modularity_poll_rest50, na.rm = TRUE)) / sd(random_modularity_poll_rest50, na.rm = TRUE)
print(paste("Rest 50% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest50, 5)))
print(paste("Rest 100% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest100, 5)))
print(paste("OG - Pollinator - Z-score (Degree controlled):", round(z_score_poll, 5)))


## 5.2 Plants ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest50_plant_node_metrics <- rest50_plant_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA)) # , NA can also be changed to  depending on how you want to analyse data - 0 might bring up some nodes when not a lot of connections are created



# Extrapolate top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_plant_rest50 <- if (sum(rest50_plant_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest50_plant_node_metrics$betweenness_rank))}


top_nodes_plant_rest50 <- rest50_plant_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_plant_rest50) %>%
  pull(node) %>% 
  as.numeric %>% 
  sort()

g_btwn_rest50_plant_plot <- function() {
nodes <- igraph::as_data_frame(g_rest50_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest50_plant)$name) %>% 
  arrange(match(name, V(g_rest50_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rest50_plant)$name %in% top_nodes_plant_rest50, "pink", "skyblue")

plot(g_rest50_plant,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest50_plant)$area_m2)/300, 
     vertex.label = V(g_rest50_plant)$name,
     vertex.color = v_colours,
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.4,            
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("50% Rest Betweenness Network - Plant \n d = ", round(rest50_plant_res_list$distance, 2)),
     sub = paste("Top Nodes: ", paste(top_nodes_plant_rest50, collapse = ", ")))
}

g_btwn_rest50_plant_plot()


top_nodes_plant_rest50
top_nodes_plant
top_nodes_plant_rest50 %in% top_nodes_plant
# We went from only 2 nodes (5edges) to 10 top nodes - nodes completely differ
# 7/10 top nodes are new restored nodes 
# 5/10 nodes are new restored nodes 

# top_nodes_poll_rest50[top_nodes_plant_rest50 %in% top_nodes_plant]
# no node between the original and the 50% restored

top_nodes_plant_rest50[top_nodes_plant_rest50 %in% top_nodes_plant_rest100]
# no node stayed between the 100% and 50% rest plans for plants

top_nodes_plant_rest50[top_nodes_plant_rest50 %in% top_nodes_poll_rest50]
#Nodes 144 and 65 are top nodes across both plants and pollinators (50% - rest)


### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_plant_rest50 <- vcount(g_rest50_plant) # number of nodes from graph
num_edges_plant_rest50 <- ecount(g_rest50_plant) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_plant_mod_rest50 <- rest50_plant_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_plant_rest50 <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest50_plant), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_plant_rest50[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_plant_rest50, breaks = 30, col = "lightblue",
     main = "Rest 50% - Plant - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_plant_rest50),
              max(c(random_modularity_plant_rest50, g_plant_mod_rest50)))
)
abline(v = g_plant_mod_rest50, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_plant_rest50 <- (g_plant_mod_rest50 - mean(random_modularity_plant_rest50, na.rm = TRUE)) / sd(random_modularity_plant_rest50, na.rm = TRUE)

print(paste("Rest 50% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest50, 5)))
print(paste("Rest 100% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest100, 5)))
print(paste("OG - Plant - Z-score (Degree controlled):", round(z_score_plant, 5)))


# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# /////////// 30% RESTORATION PLANS ----------------------------------------------
# 0. Node selection  -----------------------------------------------------
# subset only restored nodes from DF with ranks on them
# The following creates 2 different top nodes for poll and plants separately
df_sub_rank_poll_rest50 <- rest50_poll_node_metrics %>% 
  filter(as.numeric(node) >= 116 & as.numeric(node) <= 200)

rest30_num <- floor(nrow(df_sub_rank_poll_rest100)* 0.3)

rest30_node_select_poll <- df_sub_rank_poll_rest50 %>%
  arrange(betweenness_rank) %>%
  slice(1:rest30_num) %>%
  pull(node)

df_sub_rank_plant_rest50 <- rest50_plant_node_metrics %>% 
  filter(as.numeric(node) >= 116 & as.numeric(node) <= 200)

rest30_node_select_plant <- df_sub_rank_plant_rest50 %>%
  arrange(betweenness_rank) %>%
  slice(1:rest30_num) %>%
  pull(node)

rest30_node_select_poll %in% rest30_node_select_plant

# Create communal ranking - accounts for both plant and pollinator rankings and picks the top 30% of both of them - FROM 50% RANKING - NOT 100!
# create new DF with both nodes and ranks
df_comm_rank <- inner_join(
  df_sub_rank_poll_rest50 %>% 
    select(node, betweenness_rank) %>% 
    rename(btwn_rank_poll = betweenness_rank),
  df_sub_rank_plant_rest50 %>% 
    select(node, betweenness_rank) %>% 
    rename(btwn_rank_plant = betweenness_rank),
  by = "node") 

#Combines the ranking and pulls the top 30%
rest30_comb_top_nodes <- df_comm_rank %>% 
  mutate(comb_rank = btwn_rank_poll + btwn_rank_plant) %>% 
  arrange(comb_rank) %>%
  slice(1:rest30_num) %>%
  pull(node)

# Subset total restoration DF to contain OG nodes and new 30% 
df_patch_rest30 <- df_tot_rest %>% 
  filter(
    cluster == "south" |
      (cluster == "restored" & patch  %in% rest30_comb_top_nodes)
  )

df_patch_rest30 # this is the new DF to use 
dim(df_patch_rest30)
# 1. RESTORED DISTANCE NETWORKS -----------------------------------------------------
## 1.1 Pollinator ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)
rest30_pollinator_res_list <- euclidean_network_e2e(dist_poll, df_patch_rest30)

#### __ 30% Rest Pollinators Network metrics -------------------------------------------------------

rest30_poll_dist_metrics <-  data.frame(
  distance = rest30_pollinator_res_list$distance,
  percent_connected = rest30_pollinator_res_list$percent_connected,
  connectance = rest30_pollinator_res_list$connectance,
  modularity = rest30_pollinator_res_list$modularity,
  comp_number = rest30_pollinator_res_list$comp_number, 
  comp_largest = rest30_pollinator_res_list$largest_comp,
  comp_fraction = rest30_pollinator_res_list$fraction_comp
)

rest30_poll_dist_metrics
str(rest30_poll_dist_metrics)
# compare to original,50% & 100% 
poll_dist_metrics
rest50_poll_dist_metrics
rest100_poll_dist_metrics

#### __ 30% Rest Pollinator Node metrics  ---------------------------------------------------------
rest30_poll_node_metrics <- data.frame(
  distance = rest30_pollinator_res_list$distance,
  node = names(rest30_pollinator_res_list$betweenness),
  betweenness = rest30_pollinator_res_list$betweenness,
  closeness = rest30_pollinator_res_list$closeness,
  degree = rest30_pollinator_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest30_poll_node_metrics, is.numeric)
rest30_poll_node_metrics[numeric_cols] <- lapply(rest30_poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest30_poll_node_metrics
str(rest30_poll_node_metrics)

#### __ 30% Rest Pollinator Network Vis --------------------------------------------------
g_rest30_poll <- rest30_pollinator_res_list$graph

g_rest30_poll_plot <- function() {
g_rest30_poll <- rest30_pollinator_res_list$graph
nodes <- igraph::as_data_frame(g_rest30_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest30_poll)$name) %>% 
  arrange(match(name, V(g_rest30_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_rest30_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest30_poll)$area_m2)/300,
     # vertex.size = 1
     vertex.label = V(g_rest30_poll)$name,
     vertex.color = "skyblue",
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.5, 
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("Rest 30% - Pollinator Spatial Network, \n d = ", round(rest30_pollinator_res_list$distance, 2)))
}

g_rest30_poll_plot()

ecount(g_rest30_poll)
ecount(g_poll) # just to compare
ecount(g_rest50_poll)
ecount(g_rest100_poll)

## 1.2 Plant ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)
rest30_plant_res_list <- euclidean_network_e2e(dist_plant, df_patch_rest30)

#### __ 30% Rest Plant Network metrics -------------------------------------------------------
rest30_plant_dist_metrics <-  data.frame(
  distance = rest30_plant_res_list$distance,
  percent_connected = rest30_plant_res_list$percent_connected,
  connectance = rest30_plant_res_list$connectance,
  modularity = rest30_plant_res_list$modularity,
  comp_number = rest30_plant_res_list$comp_number, 
  comp_largest = rest30_plant_res_list$largest_comp,
  comp_fraction = rest30_plant_res_list$fraction_comp
)

# Change NaN --> 0
numeric_cols <- sapply(rest30_plant_dist_metrics, is.numeric)
rest30_plant_dist_metrics[numeric_cols] <- lapply(rest30_plant_dist_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest30_plant_dist_metrics
str(rest30_plant_dist_metrics)
plant_dist_metrics # compare
rest50_plant_dist_metrics
rest100_plant_dist_metrics

#### __ 30% Rest Plant Node metrics  ---------------------------------------------------------
rest30_plant_node_metrics <- data.frame(
  distance = rest30_plant_res_list$distance,
  node = names(rest30_plant_res_list$betweenness),
  betweenness = rest30_plant_res_list$betweenness,
  closeness = rest30_plant_res_list$closeness,
  degree = rest30_plant_res_list$degree
)


# Change NaN --> 0
numeric_cols <- sapply(rest30_plant_node_metrics, is.numeric)
rest30_plant_node_metrics[numeric_cols] <- lapply(rest30_plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


rest30_plant_node_metrics
str(rest30_plant_node_metrics)



#### __ 30% Rest Plant Network Vis -------------------------------------------------------
g_rest30_plant <- rest30_plant_res_list$graph

g_rest30_plant_plot <- function () {
g_rest30_plant <- rest30_plant_res_list$graph
nodes <- igraph::as_data_frame(g_rest30_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest30_plant)$name) %>% 
  arrange(match(name, V(g_rest30_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
plot(g_rest30_plant,
     layout = layout_coords,
     # vertex.size = 1,
     vertex.size = sqrt(V(g_rest30_plant)$area_m2)/300, 
     vertex.label = V(g_rest30_plant)$name,
     vertex.color = "skyblue",
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.4, 
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("Rest 30% - Plant Spatial Network, \n d = ", round(plant_res_list$distance, 2)))
}

g_rest30_plant_plot() 

ecount(g_rest30_plant)
ecount(g_rest50_plant)
ecount(g_rest100_plant)
ecount(g_plant)


# 1. 30% REST ANALYSIS  ------------------------------------------------------------
## 1.1 Pollinators ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest30_poll_node_metrics <- rest30_poll_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA))



# Extrapolate number of top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_poll_rest30 <- if (sum(rest30_poll_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest30_poll_node_metrics$betweenness_rank))}


top_nodes_poll_rest30 <- rest30_poll_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_poll_rest30) %>%
  pull(node) %>% 
  as.numeric %>% 
  sort()

g_btwn_rest30_poll_plot <- function() {
nodes <- igraph::as_data_frame(g_rest30_poll, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest30_poll)$name) %>% 
  arrange(match(name, V(g_rest30_poll)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rest30_poll)$name %in% top_nodes_poll_rest30, "pink", "skyblue")

plot(g_rest30_poll,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest30_poll)$area_m2)/300, 
     vertex.label = V(g_rest30_poll)$name,
     vertex.color = v_colours,
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.4,            
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("30% Rest Betweenness Network - Pollinators \n d = ", round(rest30_pollinator_res_list$distance, 2)),
     sub = paste("Top nodes: ", paste(top_nodes_poll_rest30, collapse = ", ")))
}

g_btwn_rest30_poll_plot()

top_nodes_poll_rest30
top_nodes_poll_rest50
top_nodes_poll_rest100
top_nodes_poll # for comparison
top_nodes_poll_rest30 %in% top_nodes_poll
top_nodes_poll_rest30[top_nodes_poll_rest30 %in% top_nodes_poll]
# 1 node stayed the same between the original and the 30% restored
# 5/10 nodes are new restored nodes 
top_nodes_poll_rest30[top_nodes_poll_rest30 %in% top_nodes_poll_rest50]
# 168 is the only node which stayed between the 50% and 30% rest plans
top_nodes_poll_rest30[top_nodes_poll_rest30 %in% top_nodes_poll_rest100]
# no node between 100% and 30% rest plans

### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_poll_rest30 <- vcount(g_rest30_poll) # number of nodes from graph
num_edges_poll_rest30 <- ecount(g_rest30_poll) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_poll_mod_rest30 <- rest30_poll_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_poll_rest30 <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest30_poll), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_poll_rest30[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_poll_rest30, breaks = 30, col = "lightblue",
     main = "Rest 30% - Pollinators - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_poll_rest30),
              max(c(random_modularity_poll_rest30, g_poll_mod_rest30)))
)
abline(v = g_poll_mod_rest30, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_poll_rest30 <- (g_poll_mod_rest30 - mean(random_modularity_poll_rest30, na.rm = TRUE)) / sd(random_modularity_poll_rest30, na.rm = TRUE)
print(paste("Rest 30% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest30, 5)))
print(paste("Rest 50% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest50, 5)))
print(paste("Rest 100% - Pollinator - Z-score (Degree controlled):", round(z_score_poll_rest100, 5)))
print(paste("OG - Pollinator - Z-score (Degree controlled):", round(z_score_poll, 5)))


## 5.2 Plants ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
rest30_plant_node_metrics <- rest30_plant_node_metrics %>%
  mutate(betweenness_rank = ifelse(betweenness != 0, rank(-betweenness[betweenness != 0], ties.method = "min"), NA)) # , NA can also be changed to  depending on how you want to analyse data - 0 might bring up some nodes when not a lot of connections are created



# Extrapolate top nodes from graph -> if number of nodes with betweenness values >10 then top 10, if not, max number of nodes with betweenness values
edge_num_plant_rest30 <- if (sum(rest30_plant_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest30_plant_node_metrics$betweenness_rank))}


top_nodes_plant_rest30 <- rest30_plant_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_plant_rest30) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort

g_btwn_rest30_plant_plot <- function () {
nodes <- igraph::as_data_frame(g_rest30_plant, what = "vertices")
layout_coords <- nodes %>%
  select(name, longitude, latitude) %>%
  filter(name %in% V(g_rest30_plant)$name) %>% 
  arrange(match(name, V(g_rest30_plant)$name)) %>%
  select(longitude, latitude) %>%
  as.matrix()
# Assign different colours to top nodes 
v_colours <- ifelse(V(g_rest30_plant)$name %in% top_nodes_plant_rest30, "pink", "skyblue")

plot(g_rest30_plant,
     layout = layout_coords,
     vertex.size = sqrt(V(g_rest30_plant)$area_m2)/300, 
     vertex.label = V(g_rest30_plant)$name,
     vertex.color = v_colours,
     vertex.label.cex = 0.8,              
     vertex.label.dist = 0.4,            
     edge.color = "red",
     edge.width = 2,
     asp = 0,
     main = paste0("30% Rest Betweenness Network - Plants \n d = ", round(rest30_plant_res_list$distance, 2)),
     sub = paste("Top nodes: ", paste(top_nodes_plant_rest30, collapse = ", ")))
}

g_btwn_rest30_plant_plot()

top_nodes_plant_rest30
top_nodes_plant
top_nodes_plant_rest30 %in% top_nodes_plant
# We went from only 2 nodes (5edges) to 10 top nodes - nodes completely differ
# 7/10 top nodes are new restored nodes 

top_nodes_plant_rest30[top_nodes_plant_rest30 %in% top_nodes_plant_rest100]
# 124 stayed between the 100% and 30% rest plans for plants
top_nodes_plant_rest30[top_nodes_plant_rest30 %in% top_nodes_plant_rest50]
# 30 stayed between the 50% and 30% rest plans for plants

top_nodes_plant_rest30[top_nodes_plant_rest30 %in% top_nodes_poll_rest30]
#node 30 also is a top node across both plants and pollinators (30% - rest)


### __ Modularity - Degree-controlled null --------------------------------------------------
num_nodes_plant_rest30 <- vcount(g_rest30_plant) # number of nodes from graph
num_edges_plant_rest30 <- ecount(g_rest30_plant) # number of edges from graph
num_graphs <- 1000     # how many random graphs you want

g_plant_mod_rest30 <- rest30_plant_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
random_modularity_plant_rest30 <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(g_rest30_plant), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  random_modularity_plant_rest30[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(random_modularity_plant_rest30, breaks = 30, col = "lightblue",
     main = "Rest 30% - Plant - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(random_modularity_plant_rest30),
              max(c(random_modularity_plant_rest30, g_plant_mod_rest30)))
)
abline(v = g_plant_mod_rest30, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
z_score_plant_rest30 <- (g_plant_mod_rest30 - mean(random_modularity_plant_rest30, na.rm = TRUE)) / sd(random_modularity_plant_rest30, na.rm = TRUE)

print(paste("Rest 30% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest30, 5)))
print(paste("Rest 50% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest50, 5)))
print(paste("Rest 100% - Plant - Z-score (Degree controlled):", round(z_score_plant_rest100, 5)))
print(paste("OG - Plant - Z-score (Degree controlled):", round(z_score_plant, 5)))





# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------

# FINAL VISUALISATION -----------------------------------------------------
## 1. Data frames - Metrics --------------------------------------------------------------------
### > Pollinators ---------------------------------------------------
poll_dist_metrics$from_net <- "original" 
rest30_poll_dist_metrics$from_net <- "rest30" 
rest50_poll_dist_metrics$from_net <- "rest50" 
rest100_poll_dist_metrics$from_net <- "rest100" 


full_comp_poll <- poll_dist_metrics %>% 
  rbind(rest30_poll_dist_metrics, rest50_poll_dist_metrics, rest100_poll_dist_metrics)

# long format for the data so that they can all be plotted in the same graph 
# from_net = factor() makes sure that the networks are presented in order
full_comp_poll_long <- full_comp_poll %>%
  mutate(percent_connected = percent_connected/100, 
         from_net = factor(from_net, levels = c("original", "rest30", "rest50", "rest100"))) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")

### > Plants --------------------------------------------------------
plant_dist_metrics$from_net <- "original" 
rest30_plant_dist_metrics$from_net <- "rest30" 
rest50_plant_dist_metrics$from_net <- "rest50" 
rest100_plant_dist_metrics$from_net <- "rest100" 


full_comp_plant <- plant_dist_metrics %>% 
  rbind(rest30_plant_dist_metrics, rest50_plant_dist_metrics, rest100_plant_dist_metrics)

# long format for the data so that they can all be plotted in the same graph 
# from_net = factor() makes sure that the netowrks re presented in order
full_comp_plant_long <- full_comp_plant %>%
  mutate(percent_connected = percent_connected/100, 
         from_net = factor(from_net, levels = c("original", "rest30", "rest50", "rest100"))) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")


# 2. Visualisation -----------------------------------------------------------
### 2.1 Networks ----------------------------------------------------------------
#### > Pollinators -----------------------------------------------------------
x11()
par(mfrow = c(2,2))
g_poll_plot() # OG pollinator network 
g_rest100_poll_plot() # 100% rest
g_rest50_poll_plot() # 50% rest
g_rest30_poll_plot() # 30% rest



#poll betweenness
x11()
par(mfrow = c(2,2))
g_btwn_poll_plot() # OG
g_btwn_rest100_poll_plot() # 100% rest
g_btwn_rest50_poll_plot() # 50% rest
g_btwn_rest30_poll_plot() # 30% rest

#### > Plants -----------------------------------------------------------
x11()
par(mfrow = c(2,2))
g_plant_plot() # OG plant network 
g_rest100_plant_plot() # 100% rest
g_rest50_plant_plot() # 50% rest
g_rest30_plant_plot() # 30% rest

#plant betwenness
x11()
par(mfrow = c(2,2))
g_btwn_plant_plot() # OG
g_btwn_rest100_plant_plot() # 100%
g_btwn_rest50_plant_plot() # 50% rest
g_btwn_rest30_plant_plot() # 30% rest 


### 2.2 Metrics  ----------------------------------------------------------------
#### > Pollinators -----------------------------------------------------------
rest_poll_metrics_plot <- ggplot(full_comp_poll_long, aes(x = from_net, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 2) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios\n > Pollinators",
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


#### > Plants -----------------------------------------------------------
rest_plant_metrics_plot <- ggplot(full_comp_plant_long, aes(x = from_net, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 2) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios \n > Plants (Shrubs)",
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


#### > Together ----------------------------------------------------------------
x11()
rest_poll_metrics_plot + rest_plant_metrics_plot




