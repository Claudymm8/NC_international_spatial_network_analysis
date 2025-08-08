# source("C:/Users/clamam/Desktop/R/targ_rest_e2e_full.R")
#load large data that take a long time to create - uncomment accordingly if data needs to be created
load("D:/ADD_PATH/q95_net_creation_plant_res_list.RData")
load("D:/ADD_PATH/final_scripts/data_output/q95_net_creation_poll_res_list.RData")

# 1. Import data & Visualisation  -----------------------------------------
## 1.1 Patch data  ---------------------------------------------------------

head(df_patch_s)
str(df_patch_s)


### 2.1 Plant Distance values -------------------------------------------------------- 
tot_jbdf_metrics 

### 2.2 Pollinator Distance values -------------------------------------------------------
df_dist_stat_poll 



# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# 95% QUANTILE  -----------------------------------------------------------
# 3. DISTANCE NETWORKS -----------------------------------------------------
## 3.1 Pollinator ---------------------------------------------------------
q95_dist_poll <- mean(df_dist_stat_poll$q95_distance_m)
q95_pollinator_res_list <- euclidean_network_e2e(q95_dist_poll, df_patch_s)
mean(df_dist_stat_poll$mean_distance_m)
mean(df_dist_stat_poll$q75_distance_m)
mean(df_dist_stat_poll$q95_distance_m)
#### __ Pollinators Network metrics -------------------------------------------------------

q95_poll_dist_metrics <-  data.frame(
  distance = q95_pollinator_res_list$distance,
  percent_connected = q95_pollinator_res_list$percent_connected,
  connectance = q95_pollinator_res_list$connectance,
  modularity = q95_pollinator_res_list$modularity,
  comp_number = q95_pollinator_res_list$comp_number, 
  comp_largest = q95_pollinator_res_list$largest_comp,
  comp_fraction = q95_pollinator_res_list$fraction_comp
)

q95_poll_dist_metrics
str(q95_poll_dist_metrics)

#### __ Pollinator Node metrics  ---------------------------------------------------------
q95_poll_node_metrics <- data.frame(
  distance = q95_pollinator_res_list$distance,
  node = names(q95_pollinator_res_list$betweenness),
  betweenness = q95_pollinator_res_list$betweenness,
  closeness = q95_pollinator_res_list$closeness,
  degree = q95_pollinator_res_list$degree,
  bridge = q95_pollinator_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(q95_poll_node_metrics, is.numeric)
q95_poll_node_metrics[numeric_cols] <- lapply(q95_poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


q95_poll_node_metrics
str(q95_poll_node_metrics)


#### __ Pollinator Network Vis --------------------------------------------------
q95_g_poll <- q95_pollinator_res_list$graph
q95_g_poll_plot <- function () {
  g_poll <- q95_pollinator_res_list$graph
  nodes <- igraph::as_data_frame(g_poll, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_poll)$name) %>% 
    arrange(match(name, V(g_poll)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_poll,
       layout = layout_coords,
       vertex.size = sqrt(V(g_poll)$area_m2)/300, 
       vertex.label = V(g_poll)$name,
       vertex.color = "skyblue",
       vertex.label.cex = 1,              
       vertex.label.dist = 0.6, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Original - Pollinator Spatial Network, \n d = ", round(q95_pollinator_res_list$distance, 2)))
}

q95_g_poll_plot()
g_poll_plot()

## 3.2 Plant ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
q95_dist_plant <- mean(tot_jbdf_metrics$q95_dist)
q95_plant_res_list <- euclidean_network_e2e(q95_dist_plant, df_patch_s)




#### __ Plant Network metrics -------------------------------------------------------
q95_plant_dist_metrics <-  data.frame(
  distance = q95_plant_res_list$distance,
  percent_connected = q95_plant_res_list$percent_connected,
  connectance = q95_plant_res_list$connectance,
  modularity = q95_plant_res_list$modularity,
  comp_number = q95_plant_res_list$comp_number, 
  comp_largest = q95_plant_res_list$largest_comp,
  comp_fraction = q95_plant_res_list$fraction_comp
)

# Change NaN --> 0
numeric_cols <- sapply(q95_plant_dist_metrics, is.numeric)
q95_plant_dist_metrics[numeric_cols] <- lapply(q95_plant_dist_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


q95_plant_dist_metrics
str(q95_plant_dist_metrics)
#no changes for plants in original network

#### __ Plant Node metrics  ---------------------------------------------------------
q95_plant_node_metrics <- data.frame(
  distance = q95_plant_res_list$distance,
  node = names(q95_plant_res_list$betweenness),
  betweenness = q95_plant_res_list$betweenness,
  closeness = q95_plant_res_list$closeness,
  degree = q95_plant_res_list$degree,
  bridge = q95_plant_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(q95_plant_node_metrics, is.numeric)
q95_plant_node_metrics[numeric_cols] <- lapply(q95_plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


q95_plant_node_metrics
str(q95_plant_node_metrics)



#### __ Plant Network Vis -------------------------------------------------------
q95_g_plant <- q95_plant_res_list$graph
q95_g_plant_plot <- function() { 
  g_plant <- q95_plant_res_list$graph
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
       vertex.color = "skyblue",
       vertex.label.cex = 1,              
       vertex.label.dist = 0.6, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Original - Plant Spatial Network, \n d = ", round(q95_plant_res_list$distance, 2)))
}

q95_g_plant_plot()

# 4. ANALYSIS  ------------------------------------------------------------
## 4.1 Pollinators ----------------------------------------------------------
### __ Betweenness ---------------------------------------------------------
# Add Rank to dataframe
q95_poll_node_metrics <- q95_poll_node_metrics %>%
  mutate(degree_rank = rank(-degree, ties.method = "min"))

# Extrapolate top nodes from graph -> if >10 then top 10, if not, max number of edges
q95_edge_num_poll <- if (sum(q95_poll_node_metrics$degree >= 1) > 10) {10} else {sum(!is.na(q95_poll_node_metrics$degree_rank))}

q95_top_nodes_poll <- q95_poll_node_metrics %>%
  arrange(degree_rank) %>%
  slice(1:q95_edge_num_poll) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()



q95_g_poll_plot <- function() {
  q_95g_poll <- q95_pollinator_res_list$graph
  nodes <- igraph::as_data_frame(q_95g_poll, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(q_95g_poll)$name) %>% 
    arrange(match(name, V(q_95g_poll)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  # Assign different colours to top nodes 
  v_colours <- ifelse(V(q_95g_poll)$name %in% q95_top_nodes_poll, "pink", "skyblue")
  
  plot(q_95g_poll,
       layout = layout_coords,
       vertex.size = sqrt(V(q_95g_poll)$area_m2)/300, 
       vertex.label = V(q_95g_poll)$name,
       vertex.color = v_colours,
       vertex.label.cex = 1,              
       vertex.label.dist = 0.25,            
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Original - Betweenness Network - Pollinators \n d = ", round(q95_pollinator_res_list$distance, 2)),
       sub = paste("Top nodes: ", paste(q95_top_nodes_poll, collapse =", ")))
}

q95_g_poll_plot()



### __ Modularity - Degree-controlled null --------------------------------------------------
num_graphs <- 1000     # how many random graphs you want

q54_g_poll_mod <- poll_dist_metrics$modularity # extract modularity

# Generate random graphs and compute modularity - Degree controlled
q95_random_modularity_poll <- numeric(num_graphs)

for (i in 1:num_graphs) {
  g_rand <- sample_degseq(degree(q95_g_poll), method = "configuration")
  mod_rand <- cluster_louvain(g_rand)
  q95_random_modularity_poll[i] <- modularity(mod_rand)
}

####  > Visualisation -----------------------------------------------------

hist(q95_random_modularity_poll, breaks = 30, col = "lightblue",
     main = "Pollinators - Distribution of Modularity\n (Degree-controlled Random Graphs)",
     xlab = "Modularity",
     xlim = c(min(q95_random_modularity_poll, na.rm = TRUE),
              max(c(q95_random_modularity_poll, q54_g_poll_mod)+0.1, na.rm = TRUE))
)
abline(v = g_poll_mod, col = "red", lwd = 2)
legend("topright", legend = "Real Network Modularity", col = "red", lwd = 2)

####  > Z-score  ----------------------------------------------------------------
# Calculate Z-score
q95_z_score_poll <- (q54_g_poll_mod - mean(q95_random_modularity_poll, na.rm = TRUE)) / sd(q95_random_modularity_poll, na.rm = TRUE)
print(paste("Pollinator - Z-score (Degree controlled):", round(z_score_poll, 5)))





###__ Degree distribution ------------------------------------

#### > Original network --------------------------------------------------------
q95_g_poll_plot()
vcount(q95_g_poll)
ecount(q95_g_poll)
transitivity(q95_g_poll) # clustering coefficient
components(q95_g_poll)
mean(degree(q95_g_poll))

q95_og_deg_poll_hist <- function() {hist(degree(q95_g_poll), xlim = c(0, 35))
  text(x = max(degree(q95_g_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}



#### > Random network ----------------------------------------------------------


set.seed(54367) # always run this before creating random network so that we get same results
q95_random_net_poll <- sample_correlated_gnp(q95_g_poll, corr = 0.8)

vcount(q95_random_net_poll)
ecount(q95_random_net_poll)
transitivity(q95_random_net_poll) 
components(q95_random_net_poll)
mean(degree(q95_random_net_poll))

#Visualisation 
q95_rand_deg_poll_hist <- function () {hist(degree(q95_random_net_poll), 
                                        xlim = c(0, 35),
)
  abline(v = mean(degree(q95_g_poll)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(q95_g_poll)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(q95_random_net_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


#### > Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(q95_g_poll) / vcount(q95_g_poll))

set.seed(54367)
q95_scalefree_net_poll <- sample_pa(n = vcount(q95_g_poll), m = 5, directed = FALSE, zero.appeal = 0)
vcount(q95_scalefree_net_poll)
ecount(q95_scalefree_net_poll)
transitivity(q95_scalefree_net_poll) 
components(q95_scalefree_net_poll)
mean(degree(q95_scalefree_net_poll))

#Visualisation 

q95_sf_deg_poll_hist <- function() {hist(degree(q95_scalefree_net_poll), xlim = c(0, max(degree(q95_scalefree_net_poll))))
  abline(v = mean(degree(q95_g_poll)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(q95_g_poll)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(q95_scalefree_net_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
q95_og_deg_poll_hist()
q95_rand_deg_poll_hist()
q95_sf_deg_poll_hist()

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------

# Restoration comparison  -------------------------------------------------
q95_rest100_pollinator_res_list <- euclidean_network_e2e(q95_dist_poll, df_tot_rest)



# > Pollinators -------------------------------------------------------------
#### __ Pollinators Network metrics -------------------------------------------------------

q95_rest100_poll_dist_metrics <-  data.frame(
  distance = q95_rest100_pollinator_res_list$distance,
  percent_connected = q95_rest100_pollinator_res_list$percent_connected,
  connectance = q95_rest100_pollinator_res_list$connectance,
  modularity = q95_rest100_pollinator_res_list$modularity,
  comp_number = q95_rest100_pollinator_res_list$comp_number, 
  comp_largest = q95_rest100_pollinator_res_list$largest_comp,
  comp_fraction = q95_rest100_pollinator_res_list$fraction_comp
)

q95_rest100_poll_dist_metrics
str(q95_rest100_poll_dist_metrics)

#### __ Pollinator Node metrics  ---------------------------------------------------------
q95_rest100_poll_node_metrics <- data.frame(
  distance = q95_rest100_pollinator_res_list$distance,
  node = names(q95_rest100_pollinator_res_list$betweenness),
  betweenness = q95_rest100_pollinator_res_list$betweenness,
  closeness = q95_rest100_pollinator_res_list$closeness,
  degree = q95_rest100_pollinator_res_list$degree,
  bridge = q95_rest100_pollinator_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(q95_rest100_poll_node_metrics, is.numeric)
q95_rest100_poll_node_metrics[numeric_cols] <- lapply(q95_rest100_poll_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


q95_rest100_poll_node_metrics
str(q95_rest100_poll_node_metrics)


#### __ Pollinator Network Vis --------------------------------------------------
q95_rest100_g_poll <- q95_rest100_pollinator_res_list$graph
q95_rest100_g_poll <- function () {
  g_poll <- q95_rest100_pollinator_res_list$graph
  nodes <- igraph::as_data_frame(g_poll, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_poll)$name) %>% 
    arrange(match(name, V(g_poll)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_poll,
       layout = layout_coords,
       vertex.size = sqrt(V(g_poll)$area_m2)/300, 
       vertex.label = V(g_poll)$name,
       vertex.color = "skyblue",
       vertex.label.cex = 1,              
       vertex.label.dist = 0.6, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Original - Pollinator Spatial Network, \n d = ", round(q95_rest100_pollinator_res_list$distance, 2)))
}

q95_rest100_g_poll()
g_rest100_poll_plot()
q95_g_poll_plot()



q95_dist_poll 


#POLLINATOR RESTORATION STRATEGIES LIST
# Add Rank to dataframe
q95_rest100_poll_node_metrics <- q95_rest100_poll_node_metrics %>%
  mutate(area_m2 = df_tot_rest$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )


q95_df_targ_rest_poll_list <- list()


for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_poll")
  q95_df_targ_rest_poll_list[[name]] <- node_select_targ_rest_fun(df_tot_rest, q95_rest100_poll_node_metrics, perc, "degree_rank")
}

q95_poll_net_targ_rest_res <- list()

for(name in names(q95_df_targ_rest_poll_list)){
  cat("Working on:", name, "\n")
  df <- q95_df_targ_rest_poll_list[[name]]
  result <- euclidean_network_e2e(q95_dist_poll, df)
  q95_poll_net_targ_rest_res[[name]] <- result
}


q95_targ_rest_poll_dist_metrics <- df_dist_metrics_fun(q95_poll_net_targ_rest_res, "targeted")
q95_targ_rest_poll_dist_metrics$rank_metric <- "degree"
q95_targ_rest_poll_dist_metrics

q95_poll_0 <- q95_poll_dist_metrics
q95_poll_0$restoration_percent <- 0
q95_poll_0$restoration_type <- "targeted"
q95_poll_0$rank_metric <- "degree"

q95_targ_rest_poll_dist_metrics <- q95_poll_0 %>%
  rbind(q95_targ_rest_poll_dist_metrics)


# > Plants ------------------------------------------------------------------
q95_rest100_plant_res_list <- euclidean_network_e2e(q95_dist_plant, df_tot_rest)

#### __ Plants Network metrics -------------------------------------------------------

q95_rest100_plant_dist_metrics <-  data.frame(
  distance = q95_rest100_plant_res_list$distance,
  percent_connected = q95_rest100_plant_res_list$percent_connected,
  connectance = q95_rest100_plant_res_list$connectance,
  modularity = q95_rest100_plant_res_list$modularity,
  comp_number = q95_rest100_plant_res_list$comp_number, 
  comp_largest = q95_rest100_plant_res_list$largest_comp,
  comp_fraction = q95_rest100_plant_res_list$fraction_comp
)

q95_rest100_plant_dist_metrics
str(q95_rest100_plant_dist_metrics)

#### __ Plant Node metrics  ---------------------------------------------------------
q95_rest100_plant_node_metrics <- data.frame(
  distance = q95_rest100_plant_res_list$distance,
  node = names(q95_rest100_plant_res_list$betweenness),
  betweenness = q95_rest100_plant_res_list$betweenness,
  closeness = q95_rest100_plant_res_list$closeness,
  degree = q95_rest100_plant_res_list$degree,
  bridge = q95_rest100_plant_res_list$bridge$`Bridge Betweenness`
)


# Change NaN --> 0
numeric_cols <- sapply(q95_rest100_plant_node_metrics, is.numeric)
q95_rest100_plant_node_metrics[numeric_cols] <- lapply(q95_rest100_plant_node_metrics[numeric_cols], function(col) {
  col[is.nan(col)] <- 0
  return(col)
})


q95_rest100_plant_node_metrics
str(q95_rest100_plant_node_metrics)


#### __ Plants Network Vis --------------------------------------------------
q95_rest100_g_plant <- q95_rest100_plant_res_list$graph
q95_rest100_g_plant <- function () {
  g_plant <- q95_rest100_plant_res_list$graph
  nodes <- igraph::as_data_frame(g_plant, what = "vertices")
  layout_coords <- nodes %>%
    select(name, longitude, latitude) %>%
    filter(name %in% V(g_plant)$name) %>% 
    arrange(match(name, V(g_plant)$name)) %>%
    select(longitude, latitude) %>%
    as.matrix()
  plot(g_plant,
       layout = layout_coords,
       vertex.size = sqrt(V(g_plant)$area_m2)/300, 
       vertex.label = V(g_plant)$name,
       vertex.color = "skyblue",
       vertex.label.cex = 1,              
       vertex.label.dist = 0.6, 
       edge.color = "red",
       edge.width = 2,
       asp = 0,
       main = paste0("Original - Plant Spatial Network, \n d = ", round(q95_rest100_plant_res_list$distance, 2)))
}

q95_rest100_g_plant()
g_rest100_plant_plot()
q95_g_plant_plot()


#PLANT RESTORATION STRATEGIES LIST
# Add Rank to dataframe
q95_rest100_plant_node_metrics <- q95_rest100_plant_node_metrics %>%
  mutate(area_m2 = df_tot_rest$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )


q95_df_targ_rest_plant_list <- list()


for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_plant")
  q95_df_targ_rest_plant_list[[name]] <- node_select_targ_rest_fun(df_tot_rest, q95_rest100_plant_node_metrics, perc, "degree_rank")
}

q95_plant_net_targ_rest_res <- list()

for(name in names(q95_df_targ_rest_plant_list)){
  cat("Working on:", name, "\n")
  df <- q95_df_targ_rest_plant_list[[name]]
  result <- euclidean_network_e2e(q95_dist_plant, df)
  q95_plant_net_targ_rest_res[[name]] <- result
}


q95_targ_rest_plant_dist_metrics <- df_dist_metrics_fun(q95_plant_net_targ_rest_res, "targeted")
q95_targ_rest_plant_dist_metrics$rank_metric <- "degree"
q95_targ_rest_plant_dist_metrics

q95_plant_0 <- q95_plant_dist_metrics
q95_plant_0$restoration_percent <- 0
q95_plant_0$restoration_type <- "targeted"
q95_plant_0$rank_metric <- "degree"

q95_targ_rest_plant_dist_metrics <- q95_plant_0 %>%
  rbind(q95_targ_rest_plant_dist_metrics)


# 1. Data frames - Metrics --------------------------------------------------------------------
### > Pollinators ---------------------------------------------------

q95_targ_rest_poll_dist_metrics
full_comp_targ_rest_poll

# long format for the data so that they can all be plotted in the same graph 
q95_targ_rest_poll_dist_metrics_long <- q95_targ_rest_poll_dist_metrics %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")

### > Plants ---------------------------------------------------

q95_targ_rest_plant_dist_metrics
full_comp_targ_rest_plant

# long format for the data so that they can all be plotted in the same graph 
q95_targ_rest_plant_dist_metrics_long <- q95_targ_rest_plant_dist_metrics %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")



# 2. Visualisation -----------------------------------------------------------
### 2.1 Metrics  ----------------------------------------------------------------
#### > Pollinators -----------------------------------------------------------
q95_tot_targ_rest_poll_metrics_plot <- ggplot(q95_targ_rest_poll_dist_metrics_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios \n> Targeted < \n> Pollinators q95 < \n> d = 1229.5 <",
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



####  _ q50 vs q95----------------------------------------------------------------
x11()
tot_targ_rest_poll_metrics_plot + q95_tot_targ_rest_poll_metrics_plot



#### > Plants -----------------------------------------------------------
q95_tot_targ_rest_plant_metrics_plot <- ggplot(q95_targ_rest_plant_dist_metrics_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios \n> Targeted < \n> Plants q95 <\n> d = 3.381268 < ",
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



#### _ q50 vs q95----------------------------------------------------------------
x11()
tot_targ_rest_plant_metrics_plot + q95_tot_targ_rest_plant_metrics_plot


# Save all large data created ---------------------------------------------
save(q95_pollinator_res_list ,q95_rest100_pollinator_res_list, q95_poll_net_targ_rest_res, file = "C:/ADD_PATH/q95_net_creation_poll_res_list.RData")

save(q95_plant_res_list, q95_rest100_plant_res_list, q95_plant_net_targ_rest_res, file = "C:/ADD_PATH/q95_net_creation_plant_res_list.RData")


