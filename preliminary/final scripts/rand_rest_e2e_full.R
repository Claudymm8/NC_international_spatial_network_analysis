# RUN FIRST OTHER SCRIPTS --> dorset_e2e.R & rest_dorset_e2e.R
# source("C:/Users/clamam/Desktop/R/dorset_e2e_cm.R")
source("C:/Users/clamam/Desktop/R/targ_rest_e2e_full.R")

## ___ Restoration Data ----------------------------------------------------
#Create a DF with all patches, restored and original
df_tot_rest 

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------
# ///////////  RESTORATION PLANS ----------------------------------------------
# 0. Node selection and Dataframe creation -----------------------------------------------------
# Function which subsets only restored nodes from DF and randomly picks them systematically
# 

node_select_rand_rest_fun <- function(df_tot_patch, net_metrics, perc_rest, seed) {
  set.seed(seed)
  df_sub_rank <- net_metrics %>% 
    filter(as.numeric(node) >= 116 & as.numeric(node) <= 200)
  rest_num <- floor(nrow(df_sub_rank) * perc_rest)
  
  total_nodes <- df_sub_rank$node
  total_nodes_shuffled <- sample(total_nodes)  
  
  node_select <- total_nodes_shuffled[1:rest_num]  
  
  # Subset total restoration DF to contain OG nodes and new 50% 
  final_df <- df_tot_patch %>% 
    filter(
      cluster == "south" |
        (cluster == "restored" & patch  %in% node_select)
    )
}

seed_n <- 182955309 #random number 
#POLLINATOR RESTORATION STRATEGIES LIST
df_rand_rest_poll_list <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_rand_rest", perc * 100, "_poll")
  df_rand_rest_poll_list[[name]] <- node_select_rand_rest_fun(df_tot_rest, rest100_poll_node_metrics, perc, seed_n)
}

#PLANT RESTORATION STRATEGIES LIST
df_rand_rest_plant_list <- list()

for(perc in seq(0.1, 1, 0.1)){
  name <- paste0("df_rand_rest", perc * 100, "_plant")
  df_rand_rest_plant_list[[name]] <- node_select_rand_rest_fun(df_tot_rest, rest100_plant_node_metrics, perc, seed_n)
}


# 1. RESTORED DISTANCE NETWORKS -----------------------------------------------------
## >> Network Distance Metric Dataframe Function ---------------------------------------
# df_dist_metrics_fun(res_list, rest_type)

## 1.2 Pollinator - Network creation ---------------------------------------------------------
dist_poll <- mean(df_dist_stat_poll$mean_distance_m)

poll_net_rand_rest_res <- list()

for(name in names(df_rand_rest_poll_list)){
  df <- df_rand_rest_poll_list[[name]]
  result <- euclidean_network_e2e(dist_poll, df)
  poll_net_rand_rest_res[[name]] <- result
}

#### __ Rest Pollinators Network metrics -------------------------------------------------------
rand_rest_poll_dist_metrics <- df_dist_metrics_fun(poll_net_rand_rest_res, "random")
rand_rest_poll_dist_metrics


## 1.3 Plant Network creation ----------------------------------------------------------------
#Shrub were picked as main plant group to analyse
dist_plant <- mean(tot_jbdf_metrics$mean_dist)

plant_net_rand_rest_res <- list()

for(name in names(df_rand_rest_plant_list)){
  df <- df_rand_rest_plant_list[[name]]
  result <- euclidean_network_e2e(dist_plant, df)
  plant_net_rand_rest_res[[name]] <- result
}


#### __ Rest Plant Network metrics -------------------------------------------------------
rand_rest_plant_dist_metrics <- df_dist_metrics_fun(plant_net_rand_rest_res, "random")
rand_rest_plant_dist_metrics



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

z_score_rand_poll <- data.frame()

for(name in names(poll_net_rand_rest_res)){
  cat("Current G:", name, "\n")
  g <- poll_net_rand_rest_res[[name]]$graph
  restoration_percent <- as.numeric(gsub("[^0-9]", "", name))
  curr_rand_mod <- random_mod_cal_fun(g, num_g)
  curr_sd <- sd(curr_rand_mod, na.rm = TRUE)
  mean_rand_mod <- mean(curr_rand_mod, na.rm = TRUE)
  real_m <- poll_net_rand_rest_res[[name]]$modularity
  z_score <- (real_m - mean_rand_mod) / curr_sd
  z_score_rand_poll <- rbind(z_score_rand_poll,
                             data.frame(
                               restoration_percent = restoration_percent,
                               real_modularity = real_m,
                               mean_random_modularity = mean_rand_mod,
                               sd_random_modularity = curr_sd,
                               z_score = z_score
                             ))
}

z_score_rand_poll

####  __Visualisation of z-score -----------------------------------------------------
# Plot standard normal distribution
ggplot(z_score_rand_poll, aes(x = restoration_percent, y = z_score)) +
  geom_point() +
  theme_bw() +
  ggtitle("Comparison of Z-scores for modularity analysis \n > Random < \n > Pollinators <") +                   
  xlab("Restoration Percent (%)") +                   
  ylab("Modularity Z-score") +                        
  scale_x_continuous(breaks = seq(0, 100, by = 10),   
                     limits = c(0, 100)) +              
  scale_y_continuous(breaks = seq(0, max(z_score_rand_poll$z_score)+1, by = 5), 
                     limits = c(0, NA)) +
  geom_hline(yintercept = 1.96, col = "red", linetype = 2, size = 1)+
  annotate(geom = "text", label = "p < 0.05", y = 3, x = 100, size = 4, col = "red")+
  annotate(geom = "text", label = "z = 1.96", y = 1, x = 100, size = 4, col = "red")          

### > Plants ----------------------------------------------------------
num_g <- 1000     # how many random graphs you want

z_score_rand_plant <- data.frame()

for(name in names(plant_net_rand_rest_res)){
  cat("Current G:", name, "\n")
  g <- plant_net_rand_rest_res[[name]]$graph
  restoration_percent <- as.numeric(gsub("[^0-9]", "", name))
  curr_rand_mod <- random_mod_cal_fun(g, num_g)
  curr_sd <- sd(curr_rand_mod, na.rm = TRUE)
  mean_rand_mod <- mean(curr_rand_mod, na.rm = TRUE)
  real_m <- plant_net_rand_rest_res[[name]]$modularity
  z_score <- (real_m - mean_rand_mod) / curr_sd
  z_score_rand_plant <- rbind(z_score_rand_plant,
                              data.frame(
                                restoration_percent = restoration_percent,
                                real_modularity = real_m,
                                mean_random_modularity = mean_rand_mod,
                                sd_random_modularity = curr_sd,
                                z_score = z_score
                              ))
}

z_score_rand_plant


####   __Visualisation of z-score -----------------------------------------------------
ggplot(z_score_rand_plant, aes(x = restoration_percent, y = z_score)) +
  geom_point() +
  theme_bw() +
  ggtitle("Comparison of Z-scores for modularity analysis \n > Random < \n > Plants <") +                   
  xlab("Restoration Percent (%)") +                   
  ylab("Modularity Z-score") +                        
  scale_x_continuous(breaks = seq(0, 100, by = 10),   
                     limits = c(0, 100)) +              
  scale_y_continuous(breaks = seq(0, max(z_score_rand_plant$z_score)+1, by = 5), 
                     limits = c(0, NA)) +
  geom_hline(yintercept = 1.96, col = "red", linetype = 2, size = 1)+
  annotate(geom = "text", label = "p < 0.05", y = 2.2, x = 100, size = 4, col = "red")+
  annotate(geom = "text", label = "z = 1.96", y = 1.8, x = 100, size = 4, col = "red")
  

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ --------

# FINAL VISUALISATION -----------------------------------------------------
## 1. Data frames - Metrics --------------------------------------------------------------------
### > Pollinators ---------------------------------------------------

poll_dist_metrics_new_rand <-  poll_dist_metrics[-c(0,8)]
poll_dist_metrics_new_rand$restoration_percent <- 0
poll_dist_metrics_new_rand$restoration_type <- "random"


full_comp_rand_rest_poll <- poll_dist_metrics_new_rand %>% 
  rbind(rand_rest_poll_dist_metrics)

full_comp_rand_rest_poll

# long format for the data so that they can all be plotted in the same graph 
full_comp_rand_rest_poll_long <- full_comp_rand_rest_poll %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")

### > Plants --------------------------------------------------------
plant_dist_metrics_new_rand <-  plant_dist_metrics[-c(0,8)]
plant_dist_metrics_new_rand$restoration_percent <- 0
plant_dist_metrics_new_rand$restoration_type <- "random"


full_comp_rand_rest_plant <- plant_dist_metrics_new_rand %>% 
  rbind(rand_rest_plant_dist_metrics)

full_comp_rand_rest_plant

# long format for the data so that they can all be plotted in the same graph 
full_comp_rand_rest_plant_long <- full_comp_rand_rest_plant %>%
  mutate(percent_connected = percent_connected/100) %>% 
  mutate(connectance = connectance*10) %>% 
  pivot_longer(cols = c(percent_connected, connectance, modularity, comp_fraction),
               names_to = "metric",
               values_to = "value")

# 2. Visualisation -----------------------------------------------------------
### 2.1 Metrics  ----------------------------------------------------------------
#### > Pollinators -----------------------------------------------------------
tot_rand_rest_poll_metrics_plot <- ggplot(full_comp_rand_rest_poll_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios\n > Random < \n > Pollinators <",
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
tot_rand_rest_plant_metrics_plot <- ggplot(full_comp_rand_rest_plant_long, aes(x = restoration_percent, y = value, group = metric, colour = metric)) +
  geom_line(aes(colour = metric)) +
  geom_point(size = 1.5) +
  labs(title = "Changes in Network Metrics in Different Restoration Scenarios \n > Random < \n > Plants (Shrubs) <",
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
tot_rand_rest_poll_metrics_plot + tot_rand_rest_plant_metrics_plot


#### > Targeted vs Random  -----------------------------------------------------
#### _ Pollinators -------------------------------------------------------------
rvt_full_comp_poll <- full_comp_targ_rest_poll %>% 
  rbind(full_comp_rand_rest_poll)


#create long version for visualisation 
rvt_full_comp_poll_long <- rvt_full_comp_poll %>%
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



rvt_full_rest_poll_metrics_plot <- ggplot(filter(rvt_full_comp_poll_long, metric %in% c("Connectance", "Percent Connected")), aes(x = restoration_percent, y = value, color = metric, linetype = rest_type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  labs(
    #title = "Changes in Network Metrics Across Different Restoration Scenarios (full) \n > Pollinators < \n > Random vs Targeted <",
    title = " > Pollinators < \n > Random vs Targeted <",
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

rvt_full_rest_poll_metrics_plot

#### _ Plants -------------------------------------------------------------
rvt_full_comp_plant <- full_comp_targ_rest_plant %>% 
  rbind(full_comp_rand_rest_plant)


#create long version for visualisation 
rvt_full_comp_plant_long <- rvt_full_comp_plant %>%
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



rvt_full_rest_plant_metrics_plot <- ggplot(filter(rvt_full_comp_plant_long, metric != "Connectivity (value * 10)"), aes(x = restoration_percent, y = value, color = metric, linetype = rest_type)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  labs(
   # title = "Changes in Network Metrics Across Different Restoration Scenarios (full) \n > Plants < \n > Random vs Targeted <",
    title = "> Shrubs < \n > Random vs Targeted <",
    x = "Percent Restored",
    y = "Metric Value",
    color = "Metric",
    linetype = "Restoration Type"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(
    "Percent Connected" = "#009900",
    "Connectivity (value * 10)" = "#FF4000",
    "Modularity" = "#0072B2",
    "Component Fraction" = "#CC79A7"
  )) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5,  face = "bold"),
    legend.position = "right"
  )

rvt_full_rest_plant_metrics_plot


x11()
rvt_full_rest_poll_metrics_plot + rvt_full_rest_plant_metrics_plot

# can see if you can add the legend of the lines direcly next to the lines

### 2.2 Network Visualisation  -------------------------------------------------
#pollinators
poll_net_rand_rest_res

g_poll_plot()
g_plot_creation(poll_net_rand_rest_res$df_rand_rest10_poll$graph, 10, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest20_poll$graph, 20, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest30_poll$graph, 30, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest40_poll$graph, 40, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest50_poll$graph, 50, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest60_poll$graph, 60, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest70_poll$graph, 70, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest80_poll$graph, 80, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest90_poll$graph, 90, dist_poll, "Pollinators", "Random")
g_plot_creation(poll_net_rand_rest_res$df_rand_rest100_poll$graph, 100, dist_poll, "Pollinators", "Random")


x11()
par(mfrow = c(2,2))
g_poll_plot()

top_nodes_rand_rest30_poll <- top_node_degree_select_fun(poll_net_rand_rest_res$df_rand_rest30_poll)
g_plot_creation(poll_net_rand_rest_res$df_rand_rest30_poll$graph, 30, dist_poll,"Pollinators", "Random", top_nodes_rand_rest30_poll)


g_plot_creation(poll_net_rand_rest_res$df_rand_rest100_poll$graph, 100, dist_poll, "Pollinators", "Random")

#plants 
plant_net_rand_rest_res

g_plant_plot()
g_plot_creation(plant_net_rand_rest_res$df_rand_rest10_plant$graph, 10, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest20_plant$graph, 20, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest30_plant$graph, 30, dist_plant, "Shrubs", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest40_plant$graph, 40, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest50_plant$graph, 50, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest60_plant$graph, 60, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest70_plant$graph, 70, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest80_plant$graph, 80, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest90_plant$graph, 90, dist_plant, "Plants", "Random")
g_plot_creation(plant_net_rand_rest_res$df_rand_rest100_plant$graph, 100, dist_plant, "Plants", "Random")

x11()
par(mfrow = c(2,2))
g_plant_plot()

top_nodes_rand_rest30_plant <- top_node_degree_select_fun(plant_net_rand_rest_res$df_rand_rest30_plant)
g_plot_creation(plant_net_rand_rest_res$df_rand_rest30_plant$graph, 30, dist_plant, "Shrubs", "Random", top_nodes_rand_rest30_plant)



# degree distribution work in progress ------------------------------------
# For random comparison of networks we only need to do it for 50% as 0 and 100% will be the same.
# pollinators -------------------------------------------------------------
#50% rest ------------------------------- 
# Original Network 
g_plot_creation(poll_net_rand_rest_res$df_rand_rest50_poll$graph, 50, dist_poll, "Pollinators", "Targeted")

g_poll_rand_rest50 <- poll_net_rand_rest_res$df_rand_rest50_poll$graph
vcount(g_poll_rand_rest50)
ecount(g_poll_rand_rest50)
transitivity(g_poll_rand_rest50) # clustering coefficient
components(g_poll_rand_rest50)
mean(degree(g_poll_rand_rest50))

og_rand_rest50_deg_poll_hist <- function() {hist(degree(g_poll_rand_rest50),
                                                 xlim = c(0, 25),
)
  text(x = max(degree(g_poll_rand_rest50)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


# Random network ------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_rand_rest50_poll <- sample_correlated_gnp(g_poll_rand_rest50, corr = 0.8)

vcount(random_net_rand_rest50_poll)
ecount(random_net_rand_rest50_poll)
transitivity(random_net_rand_rest50_poll) 
components(random_net_rand_rest50_poll)
mean(degree(random_net_rand_rest50_poll))

#Visualisation 
rand_deg_rand_rest50_poll_hist <- function () {hist(degree(random_net_rand_rest50_poll), 
                                                    xlim = c(0, 25),
)
  abline(v = mean(degree(g_poll_rand_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_poll_rand_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(random_net_rand_rest50_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

# Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_poll_rand_rest50) / vcount(g_poll_rand_rest50))

set.seed(54367)
scalefree_net_rand_rest50_poll <- sample_pa(n = vcount(g_poll_rand_rest50), m = m_estimate, directed = FALSE, , zero.appeal = 0)
vcount(scalefree_net_rand_rest50_poll)
ecount(scalefree_net_rand_rest50_poll)
transitivity(scalefree_net_rand_rest50_poll) 
components(scalefree_net_rand_rest50_poll)
mean(degree(scalefree_net_rand_rest50_poll))

#Visualisation 

sf_deg_rand_rest50_poll_hist <- function() {hist(degree(scalefree_net_rand_rest50_poll), 
                                                 xlim = c(0, 25),
)
  abline(v = mean(degree(g_poll_rand_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_poll_rand_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(scalefree_net_rand_rest50_poll)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_targ_rest50_deg_poll_hist()
rand_deg_rand_rest50_poll_hist()
sf_deg_rand_rest50_poll_hist()



# plants ------------------------------------------------------------------
#50% rest ------------------------------- 
# Original Network 
g_plot_creation(plant_net_rand_rest_res$df_rand_rest50_plant$graph, 50, dist_plant, "plantinators", "Targeted")

g_plant_rand_rest50 <- plant_net_rand_rest_res$df_rand_rest50_plant$graph
vcount(g_plant_rand_rest50)
ecount(g_plant_rand_rest50)
transitivity(g_plant_rand_rest50) # clustering coefficient
components(g_plant_rand_rest50)
mean(degree(g_plant_rand_rest50))

og_rand_rest50_deg_plant_hist <- function() {hist(degree(g_plant_rand_rest50),
                                                  xlim = c(0, 25),
)
  text(x = max(degree(g_plant_rand_rest50)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)}


# Random network ------------------------------------------------
set.seed(54367) # always run this before creating random network so that we get same results
random_net_rand_rest50_plant <- sample_correlated_gnp(g_plant_rand_rest50, corr = 0.8)

vcount(random_net_rand_rest50_plant)
ecount(random_net_rand_rest50_plant)
transitivity(random_net_rand_rest50_plant) 
components(random_net_rand_rest50_plant)
mean(degree(random_net_rand_rest50_plant))

#Visualisation 
rand_deg_rand_rest50_plant_hist <- function () {hist(degree(random_net_rand_rest50_plant), 
                                                     xlim = c(0, 25),
)
  abline(v = mean(degree(g_plant_rand_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_plant_rand_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(random_net_rand_rest50_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

# Scale-free network ------------------------------------------------------
m_estimate <- ceiling(ecount(g_plant_rand_rest50) / vcount(g_plant_rand_rest50))

set.seed(54367)
scalefree_net_rand_rest50_plant <- sample_pa(n = vcount(g_plant_rand_rest50), m = m_estimate, directed = FALSE, zero.appeal = 0)
vcount(scalefree_net_rand_rest50_plant)
ecount(scalefree_net_rand_rest50_plant)
transitivity(scalefree_net_rand_rest50_plant) 
components(scalefree_net_rand_rest50_plant)
mean(degree(scalefree_net_rand_rest50_plant))

#Visualisation 

sf_deg_rand_rest50_plant_hist <- function() {hist(degree(scalefree_net_rand_rest50_plant), 
                                                  xlim = c(0, 25),
)
  abline(v = mean(degree(g_plant_rand_rest50)), col = "red", lwd = 2, lty = 2)
  text(x = 2.1,
       y = par("usr")[4] * 0.9,
       labels = paste("Original Network \n Mean = ", round(mean(degree(g_plant_rand_rest50)), 2)),
       col = "red",
       cex = 1)
  text(x = max(degree(scalefree_net_rand_rest50_plant)),
       y = 1,
       labels = "*",
       col = "black",
       cex = 4)
}

x11()
par(mfrow = c(3,1))
og_rand_rest50_deg_plant_hist()
rand_deg_rand_rest50_plant_hist()
sf_deg_rand_rest50_plant_hist()

# Degree distribution across restoration %  ------------------------

# Pollinators -------------------------------------------------------------
df_deg_rest_poll_rand <- do.call(rbind, lapply(names(poll_net_rand_rest_res), function(name) {
  data.frame(degree = poll_net_rand_rest_res[[name]]$degree, perc_rest = as.factor(gsub("[^0-9]", "", name)))
}))

df_deg_rest_poll_rand <- rbind(df_deg_poll, df_deg_rest_poll_rand)


ggplot(df_deg_rest_poll_rand, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "darkorange3", color = "white") +
  facet_wrap(~ perc_rest) +
  theme_bw() +
  labs(title = "Degree Distributions by Random Restoration %", x = "Degree", y = "Frequency")


ggplot(df_deg_rest_poll_rand, aes(x = degree, color = perc_rest)) +
  geom_density(size = 0.8, fill = NA, key_glyph = draw_key_path) + #key_glyph changes rectangles into lines 
  theme_bw() +
  labs(title = "Changes in Degree Distribution \n > Pollinators <", x = "Degree", y = "Density", linetype = 1)+
  guides(colour = guide_legend(title = "Restoration %"))
# Random graph becomes more and more pronounced with more restoration 

# Plants ------------------------------------------------------------------
df_deg_rest_plant_rand <- do.call(rbind, lapply(names(plant_net_rand_rest_res), function(name) {
  data.frame(degree = plant_net_rand_rest_res[[name]]$degree, perc_rest = as.factor(gsub("[^0-9]", "", name)))
}))

df_deg_rest_plant_rand <- rbind(df_deg_plant, df_deg_rest_plant_rand)

ggplot(df_deg_rest_plant_rand, aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white") +
  facet_wrap(~ perc_rest) +
  theme_bw() +
  labs(title = "Degree Distributions by Random Restoration %", x = "Degree", y = "Frequency")


ggplot(df_deg_rest_plant_rand, aes(x = degree, color = perc_rest)) +
  geom_density(size = 0.8, fill = NA, key_glyph = draw_key_path) + #key_glyph changes rectangles into lines 
  theme_bw() +
  labs(title = "Changes in Degree Distribution \n > Plants <", x = "Degree", y = "Density", linetype = 1)+
  guides(colour = guide_legend(title = "Restoration %"))



# Save all large data created ---------------------------------------------
save(poll_net_rand_rest_res, z_score_rand_poll, 
     file = "C:/ADD_PATH/poll_net_rand_res.RData")

save(plant_net_rand_rest_res, z_score_rand_plant,  
     file = "C:/ADD_PATH/plant_net_rand_res.RData")



