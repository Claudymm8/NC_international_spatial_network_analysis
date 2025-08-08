# # SOURCE OTHER SCRIPTS - UNLESS ALREADY RUN 
# source("C:/Users/clamam/Desktop/R/dorset_e2e_cm.R")

## ___ RESTORATION DATA ----------------------------------------------------
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


rest100_poll_node_metrics <- rest100_poll_node_metrics %>%
  mutate(area_m2 = df_tot_rest$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )

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

rest100_plant_node_metrics <- rest100_plant_node_metrics %>%
  mutate(area_m2 = df_tot_rest$area_m2) %>% 
  mutate(
    betweenness_rank = ifelse( betweenness != 0,rank(-betweenness, ties.method = "min"),NA),
    bridge_rank = ifelse(bridge != 0, rank(-bridge, ties.method = "min"), NA),
    closeness_rank = ifelse(closeness != 0 & closeness != 1, rank(-closeness, ties.method = "min"), NA),
    degree_rank = ifelse(degree != 0, rank(-degree, ties.method = "min"), NA),
    area_rank = rank(-area_m2)
  )


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



# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# ///////////  RESTORATION PLANS ----------------------------------------------
# 0. Node selection and Dataframe creation -----------------------------------------------------
# Function which subsets only restored nodes from DF with ranks on them - degree ranked 
# 
node_select_targ_rest_fun <- function(df_tot_patch, net_metrics, perc_rest, centr_measure) {
  df_sub_rank <- net_metrics %>% 
    filter(as.numeric(node) >= 116 & as.numeric(node) <= 200)
  rest_num <- floor(nrow(df_sub_rank) * perc_rest)
  
  node_select <- df_sub_rank %>%
    arrange(.data[[centr_measure]]) %>%
    slice(1:rest_num) %>%
    pull(node)
  
  # Subset total restoration DF to contain OG nodes and new
  final_df <- df_tot_patch %>% 
    filter(
      cluster == "south" |
        (cluster == "restored" & patch  %in% node_select)
    )
}

#POLLINATOR RESTORATION STRATEGIES LIST
df_targ_rest_poll_list <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_poll")
  df_targ_rest_poll_list[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_poll_node_metrics, perc, "degree_rank")
}

#PLANT RESTORATION STRATEGIES LIST
df_targ_rest_plant_list <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_targ_rest", perc * 100, "_plant")
  df_targ_rest_plant_list[[name]] <- node_select_targ_rest_fun(df_tot_rest, rest100_plant_node_metrics, perc, "degree_rank")
}


# 1. RESTORED DISTANCE NETWORKS -----------------------------------------------------
## >> Network Distance Metric Dataframe Function ---------------------------------------
df_dist_metrics_fun <- function(res_list, rest_type){
  new_df <- data.frame()
  for (name in names(res_list)) {
    curr_net <- res_list[[name]]
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


## 1.2 Pollinator - Network creation ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)

poll_net_targ_rest_res <- list()

for(name in names(df_targ_rest_poll_list)){
  df <- df_targ_rest_poll_list[[name]]
  result <- euclidean_network_e2e(dist_poll, df)
  poll_net_targ_rest_res[[name]] <- result
}


#### __ Rest Pollinators Network metrics -------------------------------------------------------
targ_rest_poll_dist_metrics <- df_dist_metrics_fun(poll_net_targ_rest_res, "targeted")
targ_rest_poll_dist_metrics


## 1.3 Plant Network creation ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)

plant_net_targ_rest_res <- list()

for(name in names(df_targ_rest_plant_list)){
  df <- df_targ_rest_plant_list[[name]]
  result <- euclidean_network_e2e(dist_plant, df)
  plant_net_targ_rest_res[[name]] <- result
}


#### __ Rest Plant Network metrics -------------------------------------------------------
targ_rest_plant_dist_metrics <- df_dist_metrics_fun(plant_net_targ_rest_res, "targeted")
targ_rest_plant_dist_metrics



# 2. ANALYSIS  ------------------------------------------------------------
### 2.1 Modularity & Z-score - Degree-controlled null --------------------------------------------------
random_mod_cal_fun <- function(g, num_graphs) {
  random_modularity <- numeric(num_graphs)
  for (i in 1:num_graphs) {
    g_rand <- sample_degseq(degree(g), method = "configuration")
    mod_rand <- cluster_infomap(g_rand)
    random_modularity[i] <- modularity(mod_rand)
  }
  return(random_modularity)
}

### > Pollinators ----------------------------------------------------------
num_g <- 1000     # how many random graphs you want

z_score_targ_poll <- data.frame()

for(name in names(poll_net_targ_rest_res)){
  cat("Current G:", name, "\n")
  g <- poll_net_targ_rest_res[[name]]$graph
  restoration_percent <- as.numeric(gsub("[^0-9]", "", name))
  curr_rand_mod <- random_mod_cal_fun(g, num_g)
  curr_sd <- sd(curr_rand_mod, na.rm = TRUE)
  mean_rand_mod <- mean(curr_rand_mod, na.rm = TRUE)
  real_m <- poll_net_targ_rest_res[[name]]$modularity
  z_score <- (real_m - mean_rand_mod) / curr_sd
  z_score_targ_poll <- rbind(z_score_targ_poll,
    data.frame(
      restoration_percent = restoration_percent,
      real_modularity = real_m,
      mean_random_modularity = mean_rand_mod,
      sd_random_modularity = curr_sd,
      z_score = z_score
  ))
}

 z_score_targ_poll

 
# ####  __Visualisation of z-score -----------------------------------------------------
# Plot standard normal distribution
ggplot(z_score_targ_poll, aes(x = restoration_percent, y = z_score)) +
  geom_point() +
  theme_bw() +
  ggtitle("Comparison of Z-scores for modularity analysis \n > Targeted < \n > Pollinators <") +
  xlab("Restoration Percent (%)") +
  ylab("Modularity Z-score") +
  scale_x_continuous(breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, max(z_score_targ_poll$z_score)+1, by = 5),
                     limits = c(0, NA)) +
  geom_hline(yintercept = 1.96, col = "red", linetype = 2, size = 1)+
  annotate(geom = "text", label = "p < 0.05", y = 3, x = 100, size = 4, col = "red")+
  annotate(geom = "text", label = "z = 1.96", y = 1, x = 100, size = 4, col = "red")

### > Plants ----------------------------------------------------------
 num_g <- 1000     # how many random graphs you want

z_score_targ_plant <- data.frame()

for(name in names(plant_net_targ_rest_res)){
  cat("Current G:", name, "\n")
  g <- plant_net_targ_rest_res[[name]]$graph
  restoration_percent <- as.numeric(gsub("[^0-9]", "", name))
  curr_rand_mod <- random_mod_cal_fun(g, num_g)
  curr_sd <- sd(curr_rand_mod, na.rm = TRUE)
  mean_rand_mod <- mean(curr_rand_mod, na.rm = TRUE)
  real_m <- plant_net_targ_rest_res[[name]]$modularity
  z_score <- (real_m - mean_rand_mod) / curr_sd
  z_score_targ_plant <- rbind(z_score_targ_plant,
                             data.frame(
                               restoration_percent = restoration_percent,
                               real_modularity = real_m,
                               mean_random_modularity = mean_rand_mod,
                               sd_random_modularity = curr_sd,
                               z_score = z_score
                             ))
}

 z_score_targ_plant


####   __Visualisation of z-score -----------------------------------------------------
ggplot(z_score_targ_plant, aes(x = restoration_percent, y = z_score)) +
  geom_point() +
  theme_bw() +
  ggtitle("Comparison of Z-scores for modularity analysis \n > Targeted < \n > Plants <") +                   
  xlab("Restoration Percent (%)") +                   
  ylab("Modularity Z-score") +                        
  scale_x_continuous(breaks = seq(0, 100, by = 10),   
                     limits = c(0, 100)) +              
  scale_y_continuous(breaks = seq(0, max(z_score_targ_plant$z_score)+1, by = 5), 
                     limits = c(0, NA)) +
  geom_hline(yintercept = 1.96, col = "red", linetype = 2, size = 1)+
  annotate(geom = "text", label = "p < 0.05", y = 2.2, x = 100, size = 4, col = "red")+
  annotate(geom = "text", label = "z = 1.96", y = 1.8, x = 100, size = 4, col = "red")


 # \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------

# FINAL VISUALISATION -----------------------------------------------------
## 1. Data frames - Metrics --------------------------------------------------------------------
### > Pollinators ---------------------------------------------------

poll_dist_metrics_new_targ <-  poll_dist_metrics[-c(0,8)]
poll_dist_metrics_new_targ$restoration_percent <- 0
poll_dist_metrics_new_targ$restoration_type <- "targeted"

 
full_comp_targ_rest_poll <- poll_dist_metrics_new_targ %>% 
  rbind(targ_rest_poll_dist_metrics)

full_comp_targ_rest_poll

# long format for the data so that they can all be plotted in the same graph 
full_comp_targ_rest_poll_long <- full_comp_targ_rest_poll %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")
  
### > Plants --------------------------------------------------------
plant_dist_metrics_new_targ <-  plant_dist_metrics[-c(0,8)]
plant_dist_metrics_new_targ$restoration_percent <- 0
plant_dist_metrics_new_targ$restoration_type <- "targeted"


full_comp_targ_rest_plant <- plant_dist_metrics_new_targ %>% 
  rbind(targ_rest_plant_dist_metrics)

full_comp_targ_rest_plant

# long format for the data so that they can all be plotted in the same graph 
full_comp_targ_rest_plant_long <- full_comp_targ_rest_plant %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")

# 2. Visualisation -----------------------------------------------------------
### 2.1 Metrics  ----------------------------------------------------------------
#### > Pollinators -----------------------------------------------------------
tot_targ_rest_poll_metrics_plot <- ggplot(full_comp_targ_rest_poll_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios \n> Targeted < \n> Pollinators < \n> d = 419.81 <",
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


#### > Plants -----------------------------------------------------------
tot_targ_rest_plant_metrics_plot <- ggplot(full_comp_targ_rest_plant_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios \n > Targeted < \n> Plants (Shrubs) < \n > d = 1.43 <",
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


#### > Together ----------------------------------------------------------------
x11()
tot_targ_rest_poll_metrics_plot + tot_targ_rest_plant_metrics_plot

### 2.2 Network Visualisation  -------------------------------------------------

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


# NOTE: it is not possible to save the result of the function as its own object, to do so, it is better to create one function for each graph as in the previous scripts
# Can change the scale of the nodes to be representative of their areas
g_plot_creation<- function(g, perc, d, species, rest_type, top_nodes = NULL) {
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
       # vertex.size = 2,
       vertex.label = V(g_curr)$name,
       vertex.color = v_colours,
       vertex.label.cex = 1.2,
      # vertex.label.degree=-pi/2, 
       vertex.label.dist = 0.4, 
       edge.color = "#FF4000",
       edge.width = 2.5,
       asp = 0)
  title(paste0(rest_type, "\n\n  Rest ", perc, "% \n > ", species, " <\n d = ", round(d, 2)), cex.main = 2)
  if (!is.null(top_nodes)) {
    mtext(paste("Top nodes:", paste(top_nodes, collapse = ", ")), side = 1, line = 1.5, cex = 1.5)
  }
}





#pollinators
poll_net_targ_rest_res

g_poll_plot()
g_plot_creation(poll_net_targ_rest_res$df_targ_rest10_poll$graph, 10, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest20_poll$graph, 20, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest30_poll$graph, 30, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest40_poll$graph, 40, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest50_poll$graph, 50, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest60_poll$graph, 60, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest70_poll$graph, 70, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest80_poll$graph, 80, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest90_poll$graph, 90, dist_poll, "Pollinators", "Targeted")
g_plot_creation(poll_net_targ_rest_res$df_targ_rest100_poll$graph, 100, dist_poll, "Pollinators", "Targeted")


x11()
par(mfrow = c(2,2))
g_poll_plot()

top_nodes_rest30_poll <- top_node_degree_select_fun(poll_net_targ_rest_res$df_targ_rest30_poll)
g_plot_creation(poll_net_targ_rest_res$df_targ_rest30_poll$graph, 30, dist_poll,"Pollinators", "Targeted", top_nodes_rest30_poll)

top_nodes_rest100_poll <- top_node_degree_select_fun(poll_net_targ_rest_res$df_targ_rest100_poll)
g_plot_creation(poll_net_targ_rest_res$df_targ_rest100_poll$graph, 100, dist_poll, "Pollinators", "Targeted", top_nodes_rest100_poll)




#plants 
plant_net_targ_rest_res

g_plant_plot()
g_plot_creation(plant_net_targ_rest_res$df_targ_rest10_plant$graph, 10, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest20_plant$graph, 20, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest30_plant$graph, 30, dist_plant, "Shrubs", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest40_plant$graph, 40, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest50_plant$graph, 50, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest60_plant$graph, 60, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest70_plant$graph, 70, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest80_plant$graph, 80, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest90_plant$graph, 90, dist_plant, "Plants", "Targeted")
g_plot_creation(plant_net_targ_rest_res$df_targ_rest100_plant$graph, 100, dist_plant, "Shrubs", "Targeted")

x11()
par(mfrow = c(2,2))
g_plant_plot()

top_nodes_rest30_plant <- top_node_degree_select_fun(plant_net_targ_rest_res$df_targ_rest30_plant)
g_plot_creation(plant_net_targ_rest_res$df_targ_rest30_plant$graph, 30, dist_plant, "Shrubs", "Targeted", top_nodes_rest30_plant)

top_nodes_rest100_plant <- top_node_degree_select_fun(plant_net_targ_rest_res$df_targ_rest100_plant)
g_plot_creation(plant_net_targ_rest_res$df_targ_rest100_plant$graph, 100, dist_plant, "Shrubs", "Targeted", top_nodes_rest100_plant)


top_nodes_rest100_poll[which(top_nodes_rest100_poll %in% top_nodes_rest100_plant)]
top_nodes_rest30_poll[which(top_nodes_rest30_poll %in% top_nodes_rest30_plant)]

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# degree distribution work in progress ------------------------------------

# pollinators -------------------------------------------------------------


#100% rest -----------------------------------------
# Original Network 
g_rest100_poll_plot()
vcount(g_rest100_poll)
ecount(g_rest100_poll)
transitivity(g_rest100_poll) # clustering coefficient
components(g_rest100_poll)
mean(degree(g_rest100_poll))

og_rest100_deg_poll_hist <- function() {hist(degree(g_rest100_poll),
                                             xlim = c(0, 25),
)
  text(x = max(degree(g_rest100_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


# Random network ------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_targ_rest100_poll <- sample_correlated_gnp(g_rest100_poll, p = edge_density(g_rest100_poll), corr = 0.6)

vcount(random_net_targ_rest100_poll)
ecount(random_net_targ_rest100_poll)
transitivity(random_net_targ_rest100_poll) 
components(random_net_targ_rest100_poll)
mean(degree(random_net_targ_rest100_poll))

#Visualisation 
rand_deg_targ_rest100_poll_hist <- function () {hist(degree(random_net_targ_rest100_poll), 
                                       xlim = c(0, 25),
)
  abline(v = mean(degree(g_rest100_poll)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_rest100_poll)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(random_net_targ_rest100_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
  }


# Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_rest100_poll) / vcount(g_rest100_poll))

set.seed(54367)
scalefree_net_targ_rest100_poll <- sample_pa(n = vcount(g_rest100_poll), m = m_estimate, directed = FALSE, zero.appeal = 0)
vcount(scalefree_net_targ_rest100_poll)
ecount(scalefree_net_targ_rest100_poll)
transitivity(scalefree_net_targ_rest100_poll) 
components(scalefree_net_targ_rest100_poll)
mean(degree(scalefree_net_targ_rest100_poll))

#Visualisation 

sf_deg_targ_rest100_poll_hist <- function() {hist(degree(scalefree_net_targ_rest100_poll), 
                                             xlim = c(0, 25),
                                             )
  abline(v = mean(degree(g_rest100_poll)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_rest100_poll)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(scalefree_net_targ_rest100_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_rest100_deg_poll_hist()
rand_deg_targ_rest100_poll_hist()
sf_deg_targ_rest100_poll_hist()



#50% rest ------------------------------- 
# Original Network 
g_plot_creation(poll_net_targ_rest_res$df_targ_rest50_poll$graph, 50, dist_poll, "Pollinators", "Targeted")

g_poll_targ_rest50 <- poll_net_targ_rest_res$df_targ_rest50_poll$graph
vcount(g_poll_targ_rest50)
ecount(g_poll_targ_rest50)
transitivity(g_poll_targ_rest50) # clustering coefficient
components(g_poll_targ_rest50)
mean(degree(g_poll_targ_rest50))

og_targ_rest50_deg_poll_hist <- function() {hist(degree(g_poll_targ_rest50),
                                             xlim = c(0, 25),
)
  text(x = max(degree(g_poll_targ_rest50)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


# Random network ------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_targ_rest50_poll <- sample_correlated_gnp(g_poll_targ_rest50, p = edge_density(g_poll_targ_rest50), corr = 0.6)

vcount(random_net_targ_rest50_poll)
ecount(random_net_targ_rest50_poll)
transitivity(random_net_targ_rest50_poll) 
components(random_net_targ_rest50_poll)
mean(degree(random_net_targ_rest50_poll))

#Visualisation 
rand_deg_targ_rest50_poll_hist <- function () {hist(degree(random_net_targ_rest50_poll), 
                                                xlim = c(0, 25),
)
  abline(v = mean(degree(g_poll_targ_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_poll_targ_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(random_net_targ_rest50_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

# Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_poll_targ_rest50) / vcount(g_poll_targ_rest50))

set.seed(54367)
scalefree_net_targ_rest50_poll <- sample_pa(n = vcount(g_poll_targ_rest50), m = m_estimate, directed = FALSE, zero.appeal = 0)
vcount(scalefree_net_targ_rest50_poll)
ecount(scalefree_net_targ_rest50_poll)
transitivity(scalefree_net_targ_rest50_poll) 
components(scalefree_net_targ_rest50_poll)
mean(degree(scalefree_net_targ_rest50_poll))

#Visualisation 

sf_deg_targ_rest50_poll_hist <- function() {hist(degree(scalefree_net_targ_rest50_poll), 
                                             xlim = c(0, 25),
)
  abline(v = mean(degree(g_poll_targ_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_poll_targ_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(scalefree_net_targ_rest50_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_targ_rest50_deg_poll_hist()
rand_deg_targ_rest50_poll_hist()
sf_deg_targ_rest50_poll_hist()



# plants ------------------------------------------------------------------
#100% rest -----------------------------------------
# Original Network 
g_rest100_plant_plot()
vcount(g_rest100_plant)
ecount(g_rest100_plant)
transitivity(g_rest100_plant) # clustering coefficient
components(g_rest100_plant)
mean(degree(g_rest100_plant))

og_rest100_deg_plant_hist <- function() {hist(degree(g_rest100_plant),
                                             xlim = c(0, 25),
)
  text(x = max(degree(g_rest100_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


# Random network ------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_targ_rest100_plant <- sample_correlated_gnp(g_rest100_plant, p = edge_density(g_rest100_plant), corr = 0.6)

vcount(random_net_targ_rest100_plant)
ecount(random_net_targ_rest100_plant)
transitivity(random_net_targ_rest100_plant) 
components(random_net_targ_rest100_plant)
mean(degree(random_net_targ_rest100_plant))

#Visualisation 
rand_deg_targ_rest100_plant_hist <- function () {hist(degree(random_net_targ_rest100_plant), 
                                                     xlim = c(0, 25),
)
  abline(v = mean(degree(g_rest100_plant)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_rest100_plant)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(random_net_targ_rest100_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
  }


# Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_rest100_plant) / vcount(g_rest100_plant))

set.seed(54367)
scalefree_net_targ_rest100_plant <- sample_pa(n = vcount(g_rest100_plant), m = m_estimate, directed = FALSE, zero.appeal = 0)
vcount(scalefree_net_targ_rest100_plant)
ecount(scalefree_net_targ_rest100_plant)
transitivity(scalefree_net_targ_rest100_plant) 
components(scalefree_net_targ_rest100_plant)
mean(degree(scalefree_net_targ_rest100_plant))

#Visualisation 

sf_deg_targ_rest100_plant_hist <- function() {hist(degree(scalefree_net_targ_rest100_plant), 
                                                  xlim = c(0, 25),
)
  abline(v = mean(degree(g_rest100_plant)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_rest100_plant)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(scalefree_net_targ_rest100_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_rest100_deg_plant_hist()
rand_deg_targ_rest100_plant_hist()
sf_deg_targ_rest100_plant_hist()



#50% rest ------------------------------- 
# Original Network 
g_plot_creation(plant_net_targ_rest_res$df_targ_rest50_plant$graph, 50, dist_plant, "plantinators", "Targeted")

g_plant_targ_rest50 <- plant_net_targ_rest_res$df_targ_rest50_plant$graph
vcount(g_plant_targ_rest50)
ecount(g_plant_targ_rest50)
transitivity(g_plant_targ_rest50) # clustering coefficient
components(g_plant_targ_rest50)
mean(degree(g_plant_targ_rest50))

og_targ_rest50_deg_plant_hist <- function() {hist(degree(g_plant_targ_rest50),
                                                 xlim = c(0, 25),
)
  text(x = max(degree(g_plant_targ_rest50)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


# Random network ------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_targ_rest50_plant <- sample_correlated_gnp(g_plant_targ_rest50, p = edge_density(g_plant_targ_rest50), corr = 0.6)

vcount(random_net_targ_rest50_plant)
ecount(random_net_targ_rest50_plant)
transitivity(random_net_targ_rest50_plant) 
components(random_net_targ_rest50_plant)
mean(degree(random_net_targ_rest50_plant))

#Visualisation 
rand_deg_targ_rest50_plant_hist <- function () {hist(degree(random_net_targ_rest50_plant), 
                                                    xlim = c(0, 25),
)
  abline(v = mean(degree(g_plant_targ_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_plant_targ_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(random_net_targ_rest50_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

# Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_plant_targ_rest50) / vcount(g_plant_targ_rest50))

set.seed(54367)
scalefree_net_targ_rest50_plant <- sample_pa(n = vcount(g_plant_targ_rest50), m = m_estimate, directed = FALSE, zero.appeal = 0)
vcount(scalefree_net_targ_rest50_plant)
ecount(scalefree_net_targ_rest50_plant)
transitivity(scalefree_net_targ_rest50_plant) 
components(scalefree_net_targ_rest50_plant)
mean(degree(scalefree_net_targ_rest50_plant))

#Visualisation 

sf_deg_targ_rest50_plant_hist <- function() {hist(degree(scalefree_net_targ_rest50_plant), 
                                                 xlim = c(0, 25),
)
  abline(v = mean(degree(g_plant_targ_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_plant_targ_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(scalefree_net_targ_rest50_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_targ_rest50_deg_plant_hist()
rand_deg_targ_rest50_plant_hist()
sf_deg_targ_rest50_plant_hist()



# Degree distribution across restoration %  ------------------------

# Pollinators -------------------------------------------------------------
df_deg_poll <- data.frame(degree = pollinator_res_list$degree, perc_rest = as.factor(0))

df_deg_rest_poll <- do.call(rbind, lapply(names(poll_net_targ_rest_res), function(name) {
  data.frame(degree = poll_net_targ_rest_res[[name]]$degree, perc_rest = as.factor(gsub("[^0-9]", "", name)))
}))

df_deg_rest_poll <- rbind(df_deg_poll, df_deg_rest_poll)


ggplot(df_deg_rest_poll, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "darkorange3", color = "white") +
  facet_wrap(~ perc_rest) +
  theme_bw() +
  labs(title = "Degree Distributions by Targeted Restoration %", x = "Degree", y = "Frequency")


deg_deg_subset_poll <- df_deg_rest_poll %>%
  filter(as.numeric(as.character(perc_rest)) %% 20 == 0)
# 
# ggplot(deg_deg_subset_poll, aes(x = degree, fill = factor(perc_rest))) +
#   geom_histogram(binwidth = 1, color = "black", alpha = 0.3, position = "identity") +
#   scale_fill_brewer(palette = "Spectral", name = "Restoration %") +
#   theme_bw() +
#   labs(title = "Degree Distributions by Restoration %", x = "Degree", y = "Frequency")


ggplot(deg_deg_subset_poll, aes(x = degree, color = as.numeric(as.character(perc_rest)), group = perc_rest)) +
  geom_density(size = 0.8, fill = NA, key_glyph = draw_key_path, adjust = 3) + #key_glyph changes rectangles into lines 
  scale_x_continuous(breaks = seq(0, 20, by = 2))+
  scale_y_continuous(limit = c(0,1))+
  theme_bw(base_size = 16) +
  labs(title = "> Pollinators <", x = "Degree", y = "Density",color = "Restoration %", linetype = 1)  +
  scale_color_gradient(low = "#D6A9F0", high = "#0072B2")

# Random graph becomes more and more pronounced with more restoration 

# Plants ------------------------------------------------------------------
df_deg_plant <- data.frame(degree = plant_res_list$degree, perc_rest = as.factor(0))

df_deg_rest_plant <- do.call(rbind, lapply(names(plant_net_targ_rest_res), function(name) {
  data.frame(degree = plant_net_targ_rest_res[[name]]$degree, perc_rest = as.factor(gsub("[^0-9]", "", name)))
}))

df_deg_rest_plant <- rbind(df_deg_plant, df_deg_rest_plant)


ggplot(df_deg_rest_plant, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  facet_wrap(~ perc_rest) +
  theme_bw() +
  labs(title = "Degree Distributions by Targeted Restoration %", x = "Degree", y = "Frequency")

deg_deg_subset_plant <- df_deg_rest_plant %>%
  filter(as.numeric(as.character(perc_rest)) %% 20 == 0)

# ggplot(deg_deg_subset_plant, aes(x = degree, fill = perc_rest)) +
#   geom_histogram(binwidth = 1, color = "black", alpha = 0.6, position = "identity") +
#   scale_fill_brewer(palette = "Set2", name = "Restoration %") +
#   theme_bw() +
#   labs(title = "Degree Distributions by Restoration %", x = "Degree", y = "Frequency")


ggplot(deg_deg_subset_plant, aes(x = degree, color = as.numeric(as.character(perc_rest)), group = perc_rest)) +
  geom_density(size = 0.8, fill = NA, key_glyph = draw_key_path, adjust = 3) + #key_glyph changes rectangles into lines 
  scale_x_continuous(breaks = seq(0, 20, by = 2))+
  scale_y_continuous(limit = c(0,1))+
  theme_bw(base_size = 16) +
  labs(title = "> Plants <", x = "Degree", y = "Density", color = "Restoration %", linetype = 1)+
  scale_color_gradient(low = "#D6A9F0", high = "#0072B2")

#goes from random to multimodal distribution --> multiple peaks, could suggest the presence of subgroups within the network with different connectivity patterns

# Save all large data created ---------------------------------------------
save(poll_net_targ_rest_res, z_score_targ_poll, 
     file = "ADD_PATH/poll_net_targ_res.RData")

save(plant_net_targ_rest_res, z_score_targ_plant,  
     file = "C:ADD_PATH/plant_net_targ_res.RData")

