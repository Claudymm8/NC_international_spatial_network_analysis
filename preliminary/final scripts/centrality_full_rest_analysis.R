# source("C:/ADD_PATH/rand_rest_e2e_full.R")

# 1. CENTRALITY ANALYSIS - PRELIMINARY  --------------------------------------
#' In this section we look at our data and see top nodes for different centrality measures 
#' This is used mostly to get an idea of what we are looking at - full analysis starts at 2.
## 1.1 Centrality ranking - OG network -----------------------------------------
### > Pollinators -------------------------------------------------------------
poll_node_metrics
g_poll_plot()

# Extrapolate top nodes -> if >10 then top 10, if not, max number of edges - can be applied to other metrics if needed
edge_num_poll_btwn <- if (sum(poll_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(poll_node_metrics$betweenness_rank))}
edge_num_poll_bridge <- if (sum(poll_node_metrics$bridge >= 1) > 10) {10} else {sum(!is.na(poll_node_metrics$bridge_rank))}
edge_num_poll_close <- if (sum(poll_node_metrics$closeness > 0 & poll_node_metrics$closeness < 1) > 10) {10} else {sum(!is.na(poll_node_metrics$closeness_rank))}

betwn_top_nodes_poll <- poll_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_poll_btwn) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

bridge_top_nodes_poll <- poll_node_metrics %>%
  arrange(edge_num_poll_bridge) %>%
  slice(1:edge_num_poll) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

closeness_top_nodes_poll <- poll_node_metrics %>%
  arrange(closeness_rank) %>%
  slice(1:edge_num_poll_close) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()


### > Plants ------------------------------------------------------------------
plant_node_metrics
g_plant_plot()

# Extrapolate top nodes from graph -> if >10 then top 10, if not, max number of edges
edge_num_plant_btwn <- if (sum(plant_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(plant_node_metrics$betweenness_rank))}
edge_num_plant_bridge <- if (sum(plant_node_metrics$bridge >= 1) > 10) {10} else {sum(!is.na(plant_node_metrics$bridge_rank))}
edge_num_plant_close <- if (sum(plant_node_metrics$closeness > 0 & plant_node_metrics$closeness < 1) > 10) {10} else {sum(!is.na(plant_node_metrics$closeness_rank))}


betwn_top_nodes_plant <- plant_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_plant_btwn) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

bridge_top_nodes_plant <- plant_node_metrics %>%
  arrange(bridge_rank) %>%
  slice(1:edge_num_plant_bridge) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

closeness_top_nodes_plant <- plant_node_metrics %>%
  arrange(closeness_rank) %>%
  slice(1:edge_num_plant_close) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()



## 1.2 100% Rest Network -------------------------------------------------------
### > Pollinators -------------------------------------------------------------
rest100_poll_node_metrics
g_rest100_poll_plot()


# Extrapolate top nodes from graph -> if >10 then top 10, if not, max number of edges
edge_num_poll_btwn <- if (sum(rest100_poll_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest100_poll_node_metrics$betweenness_rank))}
edge_num_poll_bridge <- if (sum(rest100_poll_node_metrics$bridge >= 1) > 10) {10} else {sum(!is.na(rest100_poll_node_metrics$bridge_rank))}
edge_num_poll_close <- if (sum(rest100_poll_node_metrics$closeness > 0 & rest100_poll_node_metrics$closeness < 1) > 10) {10} else {sum(!is.na(rest100_poll_node_metrics$closeness_rank))}

betwn_top_nodes_poll <- rest100_poll_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_poll_btwn) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

bridge_top_nodes_poll <- rest100_poll_node_metrics %>%
  arrange(bridge_rank) %>%
  slice(1:edge_num_poll_bridge) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

closeness_top_nodes_poll <- rest100_poll_node_metrics %>%
  arrange(closeness_rank) %>%
  slice(1:edge_num_poll_close) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()


### > Plants ------------------------------------------------------------------
rest100_plant_node_metrics
g_rest100_plant_plot()

# Extrapolate top nodes from graph -> if >10 then top 10, if not, max number of edges
edge_num_plant_btwn <- if (sum(rest100_plant_node_metrics$betweenness >= 1) > 10) {10} else {sum(!is.na(rest100_plant_node_metrics$betweenness_rank))}
edge_num_plant_bridge <- if (sum(rest100_plant_node_metrics$bridge >= 1) > 10) {10} else {sum(!is.na(rest100_plant_node_metrics$bridge_rank))}
edge_num_plant_close <- if (sum(rest100_plant_node_metrics$closeness > 0 & rest100_plant_node_metrics$closeness < 1) > 10) {10} else {sum(!is.na(rest100_plant_node_metrics$closeness_rank))}


betwn_top_nodes_plant <- rest100_plant_node_metrics %>%
  arrange(betweenness_rank) %>%
  slice(1:edge_num_plant_btwn) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

bridge_top_nodes_plant <- rest100_plant_node_metrics %>%
  arrange(bridge_rank) %>%
  slice(1:edge_num_plant_bridge) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()

closeness_top_nodes_plant <- rest100_plant_node_metrics %>%
  arrange(closeness_rank) %>%
  slice(1:edge_num_plant_close) %>%
  pull(node) %>% 
  as.numeric() %>% 
  sort()




# 2. CLOSENESS CENTRALITY ANALYSIS  ------------------------------------------------

## 2.1 Gradual restoration lists -------------------------------------------
### > Pollinators -----------------------------------------------------------
df_targ_rest_poll_list_close <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_poll")
  df_targ_rest_poll_list_close[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_poll_node_metrics, perc, "closeness_rank")
}


### > Plants ----------------------------------------------------------------
df_targ_rest_plant_list_close <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_plant")
  df_targ_rest_plant_list_close[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_plant_node_metrics, perc, "closeness_rank")
}


## 2.2. Restored distance networks -----------------------------------------------------
### > Pollinator - Network creation ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)

poll_net_targ_rest_res_close <- list()

for(name in names(df_targ_rest_poll_list_close)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_poll_list_close[[name]]
  result <- euclidean_network_e2e(dist_poll, df)
  poll_net_targ_rest_res_close[[name]] <- result
}


#### __ Rest Pollinators Network metrics -------------------------------------------------------
targ_rest_poll_dist_metrics_close <- df_dist_metrics_fun(poll_net_targ_rest_res_close, "targeted")
targ_rest_poll_dist_metrics_close$rank_metric <- "closeness"
targ_rest_poll_dist_metrics_close

poll_0_close <- poll_dist_metrics_new_targ
poll_0_close$rank_metric <- "closeness"

targ_rest_poll_dist_metrics_close <- poll_0_close %>% 
  rbind(targ_rest_poll_dist_metrics_close)


### > Plant Network creation ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)

plant_net_targ_rest_res_close <- list()

for(name in names(df_targ_rest_plant_list_close)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_plant_list_close[[name]]
  result <- euclidean_network_e2e(dist_plant, df)
  plant_net_targ_rest_res_close[[name]] <- result
}

#### __ Rest Plant Network metrics -------------------------------------------------------
targ_rest_plant_dist_metrics_close <- df_dist_metrics_fun(plant_net_targ_rest_res_close, "targeted")
targ_rest_plant_dist_metrics_close$rank_metric <- "closeness"
targ_rest_plant_dist_metrics_close

plant_0_close <- plant_dist_metrics_new_targ
plant_0_close$rank_metric <- "closeness"

targ_rest_plant_dist_metrics_close <- plant_0_close %>% 
  rbind(targ_rest_plant_dist_metrics_close)

# 3. BRIDGE CENTRALITY ANALYSIS  ------------------------------------------------
## 3.1 Gradual restoration lists -------------------------------------------
### > Pollinators  ----------------------------------------------------------
df_targ_rest_poll_list_bridge <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_poll")
  df_targ_rest_poll_list_bridge[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_poll_node_metrics, perc, "bridge_rank")
}

### > Plants ----------------------------------------------------------------
df_targ_rest_plant_list_bridge <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_plant")
  df_targ_rest_plant_list_bridge[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_plant_node_metrics, perc, "bridge_rank")
}


## 3.2. Restored distance networks -----------------------------------------------------
### > Pollinator - Network creation ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)

poll_net_targ_rest_res_bridge <- list()

for(name in names(df_targ_rest_poll_list_bridge)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_poll_list_bridge[[name]]
  result <- euclidean_network_e2e(dist_poll, df)
  poll_net_targ_rest_res_bridge[[name]] <- result
}


#### __ Rest Pollinators Network metrics -------------------------------------------------------
targ_rest_poll_dist_metrics_bridge <- df_dist_metrics_fun(poll_net_targ_rest_res_bridge, "targeted")
targ_rest_poll_dist_metrics_bridge$rank_metric <- "bridge"
targ_rest_poll_dist_metrics_bridge

poll_0_bridge <- poll_dist_metrics_new_targ
poll_0_bridge$rank_metric <- "bridge"

targ_rest_poll_dist_metrics_bridge <- poll_0_bridge %>% 
  rbind(targ_rest_poll_dist_metrics_bridge)

### > Plant Network creation ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)

plant_net_targ_rest_res_bridge <- list()

for(name in names(df_targ_rest_plant_list_bridge)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_plant_list_bridge[[name]]
  result <- euclidean_network_e2e(dist_plant, df)
  plant_net_targ_rest_res_bridge[[name]] <- result
}


#### __ Rest Plant Network metrics -------------------------------------------------------
targ_rest_plant_dist_metrics_bridge <- df_dist_metrics_fun(plant_net_targ_rest_res_bridge, "targeted")
targ_rest_plant_dist_metrics_bridge$rank_metric <- "bridge"
targ_rest_plant_dist_metrics_bridge

plant_0_bridge <- plant_dist_metrics_new_targ
plant_0_bridge$rank_metric <- "bridge"

targ_rest_plant_dist_metrics_bridge <- plant_0_bridge %>% 
  rbind(targ_rest_plant_dist_metrics_bridge)

# 4. BETWEENNESS CENTRALITY ANALYSIS  ------------------------------------------------
## 4.1 Gradual restoration lists -------------------------------------------
### > Pollinators -----------------------------------------------------------
df_targ_rest_poll_list_btwn <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_poll")
  df_targ_rest_poll_list_btwn[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_poll_node_metrics, perc, "betweenness_rank")
}


### > Plants ----------------------------------------------------------------
df_targ_rest_plant_list_btwn <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_plant")
  df_targ_rest_plant_list_btwn[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_plant_node_metrics, perc, "betweenness_rank")
}


## 4.2 Restored distance networks -----------------------------------------------------
### > Pollinator - Network creation ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)

poll_net_targ_rest_res_btwn <- list()

for(name in names(df_targ_rest_poll_list_btwn)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_poll_list_btwn[[name]]
  result <- euclidean_network_e2e(dist_poll, df)
  poll_net_targ_rest_res_btwn[[name]] <- result
}


#### __ Rest Pollinators Network metrics -------------------------------------------------------
targ_rest_poll_dist_metrics_btwn <- df_dist_metrics_fun(poll_net_targ_rest_res_btwn, "targeted")
targ_rest_poll_dist_metrics_btwn$rank_metric <- "betweenness"
targ_rest_poll_dist_metrics_btwn

poll_0_btwn <- poll_dist_metrics_new_targ
poll_0_btwn$rank_metric <- "betweenness"

targ_rest_poll_dist_metrics_btwn <- poll_0_btwn %>% 
  rbind(targ_rest_poll_dist_metrics_btwn)

### > Plant Network creation ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)

plant_net_targ_rest_res_btwn <- list()

for(name in names(df_targ_rest_plant_list_btwn)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_plant_list_btwn[[name]]
  result <- euclidean_network_e2e(dist_plant, df)
  plant_net_targ_rest_res_btwn[[name]] <- result
}


#### __ Rest Plant Network metrics -------------------------------------------------------
targ_rest_plant_dist_metrics_btwn <- df_dist_metrics_fun(plant_net_targ_rest_res_btwn, "targeted")
targ_rest_plant_dist_metrics_btwn$rank_metric <- "betweenness"
targ_rest_plant_dist_metrics_btwn

plant_0_btwn <- plant_dist_metrics_new_targ
plant_0_btwn$rank_metric <- "betweenness"

targ_rest_plant_dist_metrics_btwn <- plant_0_btwn %>% 
  rbind(targ_rest_plant_dist_metrics_btwn)


# 5. AREA CENTRALITY ANALYSIS  ------------------------------------------------
## 5.1 Gradual restoration lists -------------------------------------------
###  > Pollinators ----------------------------------------------------------
df_targ_rest_poll_list_area <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_poll")
  df_targ_rest_poll_list_area[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_poll_node_metrics, perc, "area_rank")
}


### > Plants ----------------------------------------------------------------
df_targ_rest_plant_list_area <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_plant")
  df_targ_rest_plant_list_area[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_plant_node_metrics, perc, "area_rank")
}


## 5.2. Restored distance networks -----------------------------------------------------
### > Pollinator - Network creation ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)

poll_net_targ_rest_res_area <- list()

for(name in names(df_targ_rest_poll_list_area)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_poll_list_area[[name]]
  result <- euclidean_network_e2e(dist_poll, df)
  poll_net_targ_rest_res_area[[name]] <- result
}


#### __ Rest Pollinators Network metrics -------------------------------------------------------
targ_rest_poll_dist_metrics_area <- df_dist_metrics_fun(poll_net_targ_rest_res_area, "targeted")
targ_rest_poll_dist_metrics_area$rank_metric <- "area"
targ_rest_poll_dist_metrics_area

poll_0_area <- poll_dist_metrics_new_targ
poll_0_area$rank_metric <- "area"

targ_rest_poll_dist_metrics_area <- poll_0_area %>% 
  rbind(targ_rest_poll_dist_metrics_area)

### > Plant Network creation ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)

plant_net_targ_rest_res_area <- list()

for(name in names(df_targ_rest_plant_list_area)){
  cat("Working on =", name, "\n")
  df <- df_targ_rest_plant_list_area[[name]]
  result <- euclidean_network_e2e(dist_plant, df)
  plant_net_targ_rest_res_area[[name]] <- result
}


#### __ Rest Plant Network metrics -------------------------------------------------------
targ_rest_plant_dist_metrics_area <- df_dist_metrics_fun(plant_net_targ_rest_res_area, "targeted")
targ_rest_plant_dist_metrics_area$rank_metric <- "area"
targ_rest_plant_dist_metrics_area

plant_0_area <- plant_dist_metrics_new_targ
plant_0_area$rank_metric <- "area"

targ_rest_plant_dist_metrics_area <- plant_0_area %>% 
  rbind(targ_rest_plant_dist_metrics_area)

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------

# 6. FINAL VISUALISATION -----------------------------------------------------
## 6.1 Data frames - Metrics --------------------------------------------------------------------
### > Pollinators ---------------------------------------------------
#Change previously created metric table (based on degree ranking)
full_comp_targ_rest_poll$rank_metric <- "degree"
full_comp_rand_rest_poll$rank_metric <- "random"

# create full comparison of centrality table 
full_centrality_comp_poll <- full_comp_targ_rest_poll %>% 
  rbind(targ_rest_poll_dist_metrics_bridge, targ_rest_poll_dist_metrics_close, targ_rest_poll_dist_metrics_btwn, targ_rest_poll_dist_metrics_area, full_comp_rand_rest_poll)

# long format for the data so that they can all be plotted in the same graph 
full_centrality_comp_poll_long <- full_centrality_comp_poll %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")

### > Plants --------------------------------------------------------
#Change previously created metric table (based on degree ranking)
full_comp_targ_rest_plant$rank_metric <- "degree"
full_comp_rand_rest_plant$rank_metric <- "random"


# create full comparison of centrality table 
full_centrality_comp_plant <- full_comp_targ_rest_plant %>% 
  rbind(targ_rest_plant_dist_metrics_bridge, targ_rest_plant_dist_metrics_close, targ_rest_plant_dist_metrics_btwn, targ_rest_plant_dist_metrics_area, full_comp_rand_rest_plant)

# long format for the data so that they can all be plotted in the same graph 
full_centrality_comp_plant_long <- full_centrality_comp_plant %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")




## 6.2. Visualisation -----------------------------------------------------------
#### > Pollinators -----------------------------------------------------------


centr_comp_poll_plot <- ggplot(full_centrality_comp_poll_long, aes(x = restoration_percent, y = value, group = rank_metric, colour = rank_metric, linetype = rank_metric)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Percentage of Connected Nodes Across Different Centrality Metrics \n > Targeted < \n > Pollinators <",
       x = "Network Type",
       y = "Metric Value",
       color = "rank_metric") +
  facet_wrap(~metric, scale = "free_y")+
  scale_color_manual(name = "Ranking metric",
                     values = c(
                       betweenness = "#1b9e77",
                       bridge = "#d95f02",
                       closeness = "#7570b3",
                       degree = "#B81466",
                       area = "#5FE8EC",
                       random = "darkgrey"),
                     labels = c(
                       betweenness = "Betweenness",
                       bridge = "Bridge",
                       closeness = "Closeness",
                       area = "Area (m2)",
                       degree = "Node Degree",
                       random = "Random")) +
  scale_linetype_manual(
                     name = "Ranking Metric",
                     values = c(
                       betweenness = "solid",
                       bridge = "solid",
                       closeness = "solid",
                       degree = "solid",
                       area = "solid",
                       random = "dashed"),
                     labels = c(
                       betweenness = "Betweenness",
                       bridge = "Bridge",
                       closeness = "Closeness",
                       area = "Area (m2)",
                       degree = "Node Degree",
                       random = "Random")) +
  guides(
    colour = guide_legend(title = "Ranking Metric"),
    linetype = guide_legend(title = "Ranking Metric")
  ) +
  theme_bw() 





#### > Plants -----------------------------------------------------------

centr_comp_plant_plot <- ggplot(full_centrality_comp_plant_long, aes(x = restoration_percent, y = value, group = rank_metric, colour = rank_metric, linetype = rank_metric)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Percentage of Connected Nodes Across Different Centrality Metrics \n > Targeted < \n > Plants <",
       x = "Network Type",
       y = "Metric Value",
       color = "rank_metric") +
  facet_wrap(~metric, scale = "free_y")+
  scale_color_manual(name = "Ranking metric",
                     values = c(
                       betweenness = "#1b9e77",
                       bridge = "#d95f02",
                       closeness = "#7570b3",
                       degree = "#B81466",
                       area = "#5FE8EC",
                       random = "darkgrey"),
                     labels = c(
                       betweenness = "Betweenness",
                       bridge = "Bridge",
                       closeness = "Closeness",
                       area = "Area (m2)",
                       degree = "Node Degree",
                       random = "Random")) +
  scale_linetype_manual(
                    name = "Ranking Metric",
                    values = c(
                      betweenness = "solid",
                      bridge = "solid",
                      closeness = "solid",
                      degree = "solid",
                      area = "solid",
                      random = "dashed"),
                    labels = c(
                      betweenness = "Betweenness",
                      bridge = "Bridge",
                      closeness = "Closeness",
                      area = "Area (m2)",
                      degree = "Node Degree",
                      random = "Random")) +
  guides(
    colour = guide_legend(title = "Ranking Metric"),
    linetype = guide_legend(title = "Ranking Metric")
  ) +
  theme_bw() 





#### > Together ----------------------------------------------------------------
x11()
centr_comp_poll_plot+centr_comp_plant_plot



# Save large data ---------------------------------------------------------

save(poll_net_targ_rest_res_close,  
     poll_net_targ_rest_res_bridge,  
     poll_net_targ_rest_res_btwn,  
     plant_net_targ_rest_res_area,
     file = "ADD_PATH/centrality_poll_res.Rdata")
     

save(plant_net_targ_rest_res_close, 
     plant_net_targ_rest_res_bridge, 
     plant_net_targ_rest_res_btwn, 
     poll_net_targ_rest_res_area,
     file = "ADD_PATH/centrality_plant_res.Rdata")

