# ============================================================================
# PATENT INNOVATION NETWORK ANALYSIS - STAGE 1
# Data Preparation & Network Construction
# Publication-Quality Analysis for Nature Journal
# ============================================================================

library(data.table)
library(dplyr)
library(igraph)
library(ggplot2)
library(viridis)
library(scales)
library(lubridate)
library(RColorBrewer)
library(ggraph)
library(tidygraph)
library(patchwork)

# Define directories
BASE_DIR <- "PATH/TO/YOUR/PROJECT/DIRECTORY"
FINAL_RESULTS_DIR <- file.path(BASE_DIR, "finalresults")

# Create directory structure
dir.create(FINAL_RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(FINAL_RESULTS_DIR, "figures"), showWarnings = FALSE)
dir.create(file.path(FINAL_RESULTS_DIR, "tables"), showWarnings = FALSE)
dir.create(file.path(FINAL_RESULTS_DIR, "data"), showWarnings = FALSE)

setwd(BASE_DIR)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║  STAGE 1: DATA PREPARATION & NETWORK CONSTRUCTION                 ║\n")
cat("║  Patent Innovation Analysis Pipeline                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# 1.1: LOAD AND SAMPLE DATA
# ============================================================================

cat("Step 1.1: Loading patent data files...\n")
cat("─────────────────────────────────────────\n")

patents_full <- fread("g_patent.tsv", sep="\t")
inventors_full <- fread("g_inventor_disambiguated.tsv", sep="\t")
patent_citations_full <- fread("g_us_patent_citation.tsv", sep="\t")
app_citations_full <- fread("g_us_application_citation.tsv", sep="\t")

cat(sprintf("✓ Loaded %s patents\n", format(nrow(patents_full), big.mark=",")))
cat(sprintf("✓ Loaded %s inventor records\n", format(nrow(inventors_full), big.mark=",")))
cat(sprintf("✓ Loaded %s patent citations\n", format(nrow(patent_citations_full), big.mark=",")))
cat(sprintf("✓ Loaded %s application citations\n\n", format(nrow(app_citations_full), big.mark=",")))

# Strategic sampling
SAMPLE_SIZE <- 10000
set.seed(42)

all_patent_ids <- as.character(patents_full$patent_id)
sample_size_actual <- min(SAMPLE_SIZE, length(all_patent_ids))
sampled_patent_ids <- sample(all_patent_ids, size = sample_size_actual, replace = FALSE)

cat(sprintf("Step 1.2: Sampling %s patents for analysis\n", format(sample_size_actual, big.mark=",")))
cat("─────────────────────────────────────────\n")

patents <- patents_full %>% filter(as.character(patent_id) %in% sampled_patent_ids)
inventors <- inventors_full %>% filter(as.character(patent_id) %in% sampled_patent_ids)
patent_citations <- patent_citations_full %>%
  filter(as.character(patent_id) %in% sampled_patent_ids | 
         as.character(citation_patent_id) %in% sampled_patent_ids)
app_citations <- app_citations_full %>% filter(as.character(patent_id) %in% sampled_patent_ids)

rm(patents_full, inventors_full, patent_citations_full, app_citations_full)
gc()

cat("✓ Sample created successfully\n\n")

# ============================================================================
# 1.2: DATA CLEANING
# ============================================================================

cat("Step 1.3: Data cleaning and preprocessing\n")
cat("─────────────────────────────────────────\n")

patents_clean <- patents %>%
  filter(!is.na(patent_id) & !is.na(patent_date)) %>%
  mutate(
    patent_date = as.Date(patent_date),
    patent_year = year(patent_date),
    num_claims = as.numeric(num_claims),
    has_title = !is.na(patent_title) & patent_title != ""
  ) %>%
  filter(withdrawn == 0)

citations_clean <- patent_citations %>%
  filter(!is.na(patent_id) & !is.na(citation_patent_id)) %>%
  mutate(
    patent_id = as.character(patent_id),
    citation_patent_id = as.character(citation_patent_id)
  ) %>%
  distinct(patent_id, citation_patent_id, .keep_all = TRUE) %>%
  filter(patent_id %in% sampled_patent_ids & citation_patent_id %in% sampled_patent_ids)

inventors_clean <- inventors %>%
  filter(!is.na(patent_id) & !is.na(inventor_id)) %>%
  mutate(patent_id = as.character(patent_id))

cat(sprintf("✓ Cleaned patents: %s\n", format(nrow(patents_clean), big.mark=",")))
cat(sprintf("✓ Cleaned citations: %s\n", format(nrow(citations_clean), big.mark=",")))
cat(sprintf("✓ Cleaned inventor records: %s\n\n", format(nrow(inventors_clean), big.mark=",")))

# ============================================================================
# 1.3: CALCULATE PATENT METRICS
# ============================================================================

cat("Step 1.4: Calculating patent-level metrics\n")
cat("─────────────────────────────────────────\n")

forward_citations <- citations_clean %>%
  group_by(citation_patent_id) %>%
  summarise(forward_citations = n(), .groups = "drop") %>%
  rename(patent_id = citation_patent_id)

backward_citations <- citations_clean %>%
  group_by(patent_id) %>%
  summarise(backward_citations = n(), .groups = "drop")

inventor_count <- inventors_clean %>%
  group_by(patent_id) %>%
  summarise(
    num_inventors = n(),
    num_female = sum(gender_code == "F", na.rm = TRUE),
    num_male = sum(gender_code == "M", na.rm = TRUE),
    pct_female = num_female / num_inventors * 100,
    .groups = "drop"
  )

cat("✓ Forward citations calculated\n")
cat("✓ Backward citations calculated\n")
cat("✓ Inventor statistics calculated\n\n")

# ============================================================================
# 1.4: CREATE MASTER DATASET
# ============================================================================

cat("Step 1.5: Creating master patent dataset\n")
cat("─────────────────────────────────────────\n")

master_data <- patents_clean %>%
  mutate(patent_id = as.character(patent_id)) %>%
  left_join(forward_citations, by = "patent_id") %>%
  left_join(backward_citations, by = "patent_id") %>%
  left_join(inventor_count, by = "patent_id") %>%
  mutate(
    forward_citations = ifelse(is.na(forward_citations), 0, forward_citations),
    backward_citations = ifelse(is.na(backward_citations), 0, backward_citations),
    num_inventors = ifelse(is.na(num_inventors), 0, num_inventors),
    citation_percentile = percent_rank(forward_citations),
    is_breakthrough = citation_percentile >= 0.90,
    patent_age = as.numeric(Sys.Date() - patent_date) / 365.25
  )

cat(sprintf("✓ Master dataset created: %s patents\n", format(nrow(master_data), big.mark=",")))
cat(sprintf("✓ Breakthrough patents: %s (top 10%%)\n", sum(master_data$is_breakthrough)))
cat(sprintf("✓ Mean forward citations: %.2f\n", mean(master_data$forward_citations)))
cat(sprintf("✓ Mean backward citations: %.2f\n\n", mean(master_data$backward_citations)))

# ============================================================================
# 1.5: BUILD CITATION NETWORK
# ============================================================================

cat("Step 1.6: Constructing citation network graph\n")
cat("─────────────────────────────────────────\n")

edge_list <- citations_clean %>% select(from = patent_id, to = citation_patent_id)
g <- graph_from_data_frame(edge_list, directed = TRUE)

cat(sprintf("✓ Network created with %s nodes\n", vcount(g)))
cat(sprintf("✓ Network edges: %s\n", ecount(g)))
cat(sprintf("✓ Network density: %.6f\n", edge_density(g)))
cat(sprintf("✓ Network is connected: %s\n\n", is_connected(g, mode = "weak")))

# ============================================================================
# 1.6: SAVE STAGE 1 OUTPUTS
# ============================================================================

cat("Step 1.7: Saving Stage 1 outputs\n")
cat("─────────────────────────────────────────\n")

saveRDS(master_data, file.path(FINAL_RESULTS_DIR, "data", "stage1_master_data.rds"))
saveRDS(citations_clean, file.path(FINAL_RESULTS_DIR, "data", "stage1_citations.rds"))
saveRDS(inventors_clean, file.path(FINAL_RESULTS_DIR, "data", "stage1_inventors.rds"))
saveRDS(g, file.path(FINAL_RESULTS_DIR, "data", "stage1_network.rds"))

# TABLE 1: Descriptive Statistics
table1 <- data.frame(
  Metric = c(
    "Total Patents",
    "Date Range",
    "Mean Patent Age (years)",
    "Total Citations (within sample)",
    "Mean Forward Citations",
    "Median Forward Citations",
    "Max Forward Citations",
    "Mean Backward Citations",
    "Total Unique Inventors",
    "Mean Inventors per Patent",
    "Patents with Female Inventors",
    "Mean % Female Inventors",
    "Breakthrough Patents (Top 10%)",
    "Network Nodes",
    "Network Edges",
    "Network Density"
  ),
  Value = c(
    format(nrow(master_data), big.mark=","),
    paste(min(master_data$patent_date), "to", max(master_data$patent_date)),
    sprintf("%.2f", mean(master_data$patent_age, na.rm=TRUE)),
    format(nrow(citations_clean), big.mark=","),
    sprintf("%.2f", mean(master_data$forward_citations)),
    sprintf("%.0f", median(master_data$forward_citations)),
    sprintf("%.0f", max(master_data$forward_citations)),
    sprintf("%.2f", mean(master_data$backward_citations)),
    format(length(unique(inventors_clean$inventor_id)), big.mark=","),
    sprintf("%.2f", mean(master_data$num_inventors, na.rm=TRUE)),
    format(sum(master_data$num_female > 0, na.rm=TRUE), big.mark=","),
    sprintf("%.2f%%", mean(master_data$pct_female, na.rm=TRUE)),
    format(sum(master_data$is_breakthrough), big.mark=","),
    format(vcount(g), big.mark=","),
    format(ecount(g), big.mark=","),
    sprintf("%.6f", edge_density(g))
  )
)

write.csv(table1, file.path(FINAL_RESULTS_DIR, "tables", "Table_1_Descriptive_Statistics.csv"), 
          row.names = FALSE)

cat("✓ Saved master_data.rds\n")
cat("✓ Saved citations.rds\n")
cat("✓ Saved inventors.rds\n")
cat("✓ Saved network.rds\n")
cat("✓ Saved Table 1: Descriptive Statistics\n\n")

cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║  STAGE 1 COMPLETE                                                 ║\n")
cat("║  Proceed to Stage 2: Network Analysis & Centrality Metrics        ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")













































# ============================================================================
# PATENT INNOVATION NETWORK ANALYSIS - STAGE 2
# Network Analysis & Advanced Visualizations
# Publication-Quality Analysis for Nature Journal
# ============================================================================

library(data.table)
library(dplyr)
library(igraph)
library(ggplot2)
library(viridis)
library(scales)
library(RColorBrewer)
library(ggraph)
library(tidygraph)
library(patchwork)
library(gridExtra)
library(grid)

# Define directories
BASE_DIR <- "PATH/TO/YOUR/PROJECT/DIRECTORY"
FINAL_RESULTS_DIR <- file.path(BASE_DIR, "finalresults")
setwd(BASE_DIR)

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║  STAGE 2: NETWORK ANALYSIS & CENTRALITY METRICS                   ║\n")
cat("║  Advanced Network Visualizations                                  ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")

# ============================================================================
# 2.1: LOAD STAGE 1 DATA
# ============================================================================

cat("Step 2.1: Loading Stage 1 outputs\n")
cat("─────────────────────────────────────────\n")

master_data <- readRDS(file.path(FINAL_RESULTS_DIR, "data", "stage1_master_data.rds"))
citations_clean <- readRDS(file.path(FINAL_RESULTS_DIR, "data", "stage1_citations.rds"))
g <- readRDS(file.path(FINAL_RESULTS_DIR, "data", "stage1_network.rds"))

cat("✓ Data loaded successfully\n\n")

# ============================================================================
# 2.2: CALCULATE NETWORK CENTRALITY METRICS
# ============================================================================

cat("Step 2.2: Calculating network centrality metrics\n")
cat("─────────────────────────────────────────\n")

in_degree <- degree(g, mode = "in")
out_degree <- degree(g, mode = "out")
pagerank_scores <- page_rank(g, directed = TRUE)$vector
betweenness_scores <- betweenness(g, directed = TRUE, normalized = TRUE)
eigen_scores <- eigen_centrality(g, directed = TRUE)$vector
closeness_scores <- closeness(g, mode = "all", normalized = TRUE)

network_metrics <- data.frame(
  patent_id = V(g)$name,
  in_degree = in_degree,
  out_degree = out_degree,
  pagerank = pagerank_scores,
  betweenness = betweenness_scores,
  eigenvector = eigen_scores,
  closeness = closeness_scores,
  stringsAsFactors = FALSE
) %>% mutate(
  pagerank_percentile = percent_rank(pagerank),
  betweenness_percentile = percent_rank(betweenness),
  eigenvector_percentile = percent_rank(eigenvector),
  is_hub = pagerank_percentile >= 0.95,
  is_bridge = betweenness_percentile >= 0.95,
  is_authority = eigenvector_percentile >= 0.95
)

cat("✓ PageRank calculated\n")
cat("✓ Betweenness centrality calculated\n")
cat("✓ Eigenvector centrality calculated\n")
cat("✓ Closeness centrality calculated\n")
cat("✓ Network roles identified\n\n")

# ============================================================================
# 2.3: MERGE WITH MASTER DATA
# ============================================================================

cat("Step 2.3: Merging network metrics with master data\n")
cat("─────────────────────────────────────────\n")

master_with_network <- master_data %>%
  left_join(network_metrics, by = "patent_id") %>%
  mutate(
    pagerank = ifelse(is.na(pagerank), 0, pagerank),
    betweenness = ifelse(is.na(betweenness), 0, betweenness),
    eigenvector = ifelse(is.na(eigenvector), 0, eigenvector),
    is_hub = ifelse(is.na(is_hub), FALSE, is_hub),
    is_bridge = ifelse(is.na(is_bridge), FALSE, is_bridge),
    is_authority = ifelse(is.na(is_authority), FALSE, is_authority)
  )

cat(sprintf("✓ Merged dataset: %s patents with network metrics\n\n", 
            format(nrow(master_with_network), big.mark=",")))

# ============================================================================
# 2.4: COMMUNITY DETECTION
# ============================================================================

cat("Step 2.4: Detecting network communities\n")
cat("─────────────────────────────────────────\n")

g_undirected <- as.undirected(g, mode = "collapse")
communities_walktrap <- cluster_walktrap(g_undirected)
communities_louvain <- cluster_louvain(g_undirected)

V(g)$community_walktrap <- communities_walktrap$membership[match(V(g)$name, V(g_undirected)$name)]
V(g)$community_louvain <- communities_louvain$membership[match(V(g)$name, V(g_undirected)$name)]

cat(sprintf("✓ Walktrap: %d communities detected\n", length(communities_walktrap)))
cat(sprintf("✓ Louvain: %d communities detected\n", length(communities_louvain)))
cat(sprintf("✓ Modularity (Walktrap): %.4f\n", modularity(communities_walktrap)))
cat(sprintf("✓ Modularity (Louvain): %.4f\n\n", modularity(communities_louvain)))

# ============================================================================
# 2.5: ADVANCED VISUALIZATIONS
# ============================================================================

cat("Step 2.5: Creating advanced network visualizations\n")
cat("─────────────────────────────────────────\n")

## ---- Figure 1 ------------------------------------------------------------
cat("Creating Figure 1: Network Overview (4-panel layout)...\n")

top_250 <- master_with_network %>% 
  arrange(desc(pagerank)) %>% head(250) %>% pull(patent_id)

g_sub <- induced_subgraph(g, top_250) %>% simplify(remove.loops = TRUE)

# Add node attributes
V(g_sub)$pagerank      <- master_with_network$pagerank[match(V(g_sub)$name, master_with_network$patent_id)]
V(g_sub)$is_breakthrough <- master_with_network$is_breakthrough[match(V(g_sub)$name, master_with_network$patent_id)]
V(g_sub)$forward_cit   <- master_with_network$forward_citations[match(V(g_sub)$name, master_with_network$patent_id)]
V(g_sub)$community     <- communities_louvain$membership[match(V(g_sub)$name, V(g)$name)]

tg <- as_tbl_graph(g_sub)

# (A) Fruchterman-Reingold + communities
p1a <- ggraph(tg, layout = 'fr') +
  geom_edge_link(aes(alpha = 0.3), edge_colour = "gray70",
                 arrow = arrow(length = unit(1, 'mm')), end_cap = circle(2, 'mm')) +
  geom_node_point(aes(size = pagerank, color = as.factor(community)), alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), name = "PageRank") +
  scale_color_viridis_d(option = "turbo", name = "Community") +
  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(title = "(A) Community Structure",
       subtitle = "Fruchterman-Reingold layout")

# (B) Breakthrough vs Regular
p1b <- ggraph(tg, layout = 'fr') +
  geom_edge_link(aes(alpha = 0.3), edge_colour = "gray70",
                 arrow = arrow(length = unit(1, 'mm')), end_cap = circle(2, 'mm')) +
  geom_node_point(aes(size = pagerank, color = is_breakthrough), alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), name = "PageRank") +
  scale_color_manual(values = c("FALSE" = "#3B9AB2", "TRUE" = "#F21A00"),
                     name = "Type", labels = c("Regular", "Breakthrough")) +
  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(title = "(B) Innovation Impact",
       subtitle = "Breakthrough vs Regular Patents")

# (C) Circular layout
p1c <- ggraph(tg, layout = 'circle') +
  geom_edge_arc(aes(alpha = 0.3), edge_colour = "gray70",
                arrow = arrow(length = unit(1, 'mm')), strength = 0.3) +
  geom_node_point(aes(size = forward_cit, color = pagerank), alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), name = "Citations") +
  scale_color_viridis_c(option = "magma", name = "PageRank") +
  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(title = "(C) Circular View",
       subtitle = "Citation patterns")

# (D) Kamada-Kawai layout
p1d <- ggraph(tg, layout = 'kk') +
  geom_edge_link(aes(alpha = 0.3), edge_colour = "gray70",
                 arrow = arrow(length = unit(1, 'mm')), end_cap = circle(2, 'mm')) +
  geom_node_point(aes(size = forward_cit, color = pagerank), alpha = 0.8) +
  scale_size_continuous(range = c(2, 12), name = "Citations") +
  scale_color_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B",
                        midpoint = median(V(g_sub)$pagerank), name = "PageRank") +
  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(title = "( bev) Kamada-Kawai Layout",
       subtitle = "Optimized for readability")

fig1_combined <- (p1a | p1b) / (p1c | p1d)

ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_1_Network_Overview_4Panel.png"),
       fig1_combined, width = 20, height = 18, dpi = 300, bg = "white")

cat("✓ Figure 1 saved (4-panel network overview)\n")

## ---- Figure 2 ------------------------------------------------------------
cat("Creating Figure 2: Hierarchical network structure...\n")

p2 <- ggraph(tg, layout = 'tree', circular = TRUE) +
  geom_edge_diagonal(aes(alpha = 0.5), edge_colour = "gray60") +
  geom_node_point(aes(size = pagerank, color = is_breakthrough), alpha = 0.9) +
  scale_size_continuous(range = c(1, 10), name = "PageRank") +
  scale_color_manual(values = c("FALSE" = "#21908CFF", "TRUE" = "#FDE725FF"),
                     name = "Type", labels = c("Regular", "Breakthrough")) +
  theme_graph(base_family = "sans") +
  theme(legend.position = "right") +
  labs(title = "Hierarchical Citation Structure",
       subtitle = "Circular dendrogram revealing citation hierarchies and breakthrough patterns")

ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_2_Hierarchical_Network.png"),
       p2, width = 14, height = 14, dpi = 300, bg = "white")

cat("✓ Figure 2 saved\n")

## ---- Figure 3 ------------------------------------------------------------
cat("Creating Figure 3: Centrality measure relationships...\n")

p3a <- master_with_network %>%
  filter(pagerank > 0 & forward_citations > 0) %>%
  ggplot(aes(x = pagerank, y = forward_citations)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c(option = "plasma", trans = "log10", name = "Count (log)") +
  scale_x_log10(labels = scientific) +
  scale_y_log10(labels = scientific) +
  geom_smooth(method = "lm", color = "#FF0000", linewidth = 1.5, linetype = "dashed") +
  labs(title = "(A) PageRank vs Forward Citations",
       x = "PageRank Score (log scale)",
       y = "Forward Citations (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", plot.title = element_text(face = "bold"))

p3b <- master_with_network %>%
  filter(betweenness > 0 & eigenvector > 0) %>%
  ggplot(aes(x = betweenness, y = eigenvector)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c(option = "viridis", trans = "log10", name = "Count (log)") +
  scale_x_log10(labels = scientific) +
  scale_y_log10(labels = scientific) +
  geom_smooth(method = "lm", color = "#00FF00", linewidth = 1.5, linetype = "dashed") +
  labs(title = "(B) Betweenness vs Eigenvector Centrality",
       x = "Betweenness Centrality (log scale)",
       y = "Eigenvector Centrality (log scale)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", plot.title = element_text(face = "bold"))

fig3_combined <- p3a / p3b

ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_3_Centrality_Relationships.png"),
       fig3_combined, width = 12, height = 14, dpi = 300, bg = "white")

cat("✓ Figure 3 saved\n")

## ---- Figure 4 ------------------------------------------------------------
cat("Creating Figure 4: Network role distributions...\n")

role_data <- master_with_network %>%
  mutate(
    Network_Role = case_when(
      is_hub & is_bridge ~ "Hub & Bridge",
      is_hub ~ "Hub Only",
      is_bridge ~ "Bridge Only",
      TRUE ~ "Regular Node"
    ),
    Network_Role = factor(Network_Role,
                          levels = c("Hub & Bridge", "Hub Only", "Bridge Only", "Regular Node"))
  )

p4a <- ggplot(role_data, aes(x = Network_Role,
                             y = log10(pagerank + 1e-10), fill = Network_Role)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "(A) PageRank by Network Role",
       x = "", y = "PageRank (log10)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))

p4b <- ggplot(role_data, aes(x = Network_Role, y = forward_citations, fill = Network_Role)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_log10() +
  labs(title = "(B) Forward Citations by Network Role",
       x = "", y = "Forward Citations (log10)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"))

fig4_combined <- p4a | p4b

ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_4_Network_Roles.png"),
       fig4_combined, width = 14, height = 7, dpi = 300, bg = "white")

cat("✓ Figure 4 saved\n")

# ============================================================================
# 2.6: CREATE TABLES
# ============================================================================

cat("\nStep 2.6: Creating analytical tables\n")
cat("─────────────────────────────────────────\n")

table2 <- data.frame(
  Metric = c("PageRank", "Betweenness", "Eigenvector", "Closeness", "In-Degree", "Out-Degree"),
  Mean = c(mean(network_metrics$pagerank), mean(network_metrics$betweenness),
           mean(network_metrics$eigenvector), mean(network_metrics$closeness, na.rm=TRUE),
           mean(network_metrics$in_degree), mean(network_metrics$out_degree)),
  Median = c(median(network_metrics$pagerank), median(network_metrics$betweenness),
             median(network_metrics$eigenvector), median(network_metrics$closeness, na.rm=TRUE),
             median(network_metrics$in_degree), median(network_metrics$out_degree)),
  SD = c(sd(network_metrics$pagerank), sd(network_metrics$betweenness),
         sd(network_metrics$eigenvector), sd(network_metrics$closeness, na.rm=TRUE),
         sd(network_metrics$in_degree), sd(network_metrics$out_degree)),
  Max = c(max(network_metrics$pagerank), max(network_metrics$betweenness),
          max(network_metrics$eigenvector), max(network_metrics$closeness, na.rm=TRUE),
          max(network_metrics$in_degree), max(network_metrics$out_degree))
) %>% mutate(across(where(is.numeric), ~sprintf("%.6f", .)))

write.csv(table2,
          file.path(FINAL_RESULTS_DIR, "tables", "Table_2_Network_Centrality_Statistics.csv"),
          row.names = FALSE)

table3 <- master_with_network %>%
  filter(pagerank > 0) %>%
  mutate(pagerank_decile = ntile(pagerank, 10)) %>%
  group_by(pagerank_decile) %>%
  summarise(
    N_Patents = n(),
    Avg_Forward_Citations = round(mean(forward_citations, na.rm = TRUE), 2),
    Avg_Backward_Citations = round(mean(backward_citations, na.rm = TRUE), 2),
    Avg_Inventors = round(mean(num_inventors, na.rm = TRUE), 2),
    Pct_Breakthrough = round(100 * mean(is_breakthrough, na.rm = TRUE), 2),
    Avg_PageRank = format(mean(pagerank), scientific = TRUE, digits = 4),
    .groups = "drop"
  ) %>% arrange(desc(pagerank_decile))

write.csv(table3,
          file.path(FINAL_RESULTS_DIR, "tables", "Table_3_PageRank_Decile_Analysis.csv"),
          row.names = FALSE)

cat("✓ Table 2: Network Centrality Statistics saved\n")
cat("✓ Table 3: PageRank Decile Analysis saved\n\n")

# ============================================================================
# 2.7: SAVE STAGE 2 OUTPUTS
# ============================================================================

cat("Step 2.7: Saving Stage 2 outputs\n")
cat("─────────────────────────────────────────\n")

saveRDS(master_with_network,
        file.path(FINAL_RESULTS_DIR, "data", "stage2_master_with_network.rds"))
saveRDS(network_metrics,
        file.path(FINAL_RESULTS_DIR, "data", "stage2_network_metrics.rds"))
saveRDS(g_sub,
        file.path(FINAL_RESULTS_DIR, "data", "stage2_top250_network.rds"))

cat("✓ Saved master_with_network.rds\n")
cat("✓ Saved network_metrics.rds\n")
cat("✓ Saved top250_network.rds\n\n")

cat("╔════════════════════════════════════════════════════════════════════╗\n")
cat("║  STAGE 2 COMPLETE                                                 ║\n")
cat("║  Proceed to Stage 3: Machine Learning & Prediction               ║\n")
cat("╚════════════════════════════════════════════════════════════════════╝\n\n")


































# ============================================================================
# STAGE 3: MACHINE LEARNING - Breakthrough Prediction (FIXED)
# ============================================================================

# --- LIBRARIES ---
library(randomForest)
library(caret)
library(pROC)
library(ggplot2)
library(dplyr)
library(igraph)
library(patchwork)     # <--- ADD THIS for | operator
library(tibble)

# --- PATHS ---
BASE_DIR <- "PATH/TO/YOUR/PROJECT/DIRECTORY"
FINAL_RESULTS_DIR <- file.path(BASE_DIR, "finalresults")
setwd(BASE_DIR)

cat("\n════════════════════════════════════════════════════════════════════╗\n")
cat("║  STAGE 3: MACHINE LEARNING & PREDICTIVE MODELING                  ║\n")
cat("║  Random Forest Classification for Breakthrough Innovation         ║\n")
cat("════════════════════════════════════════════════════════════════════╝\n\n")

# ---------------------------------------------------------------------------
# 3.1: LOAD DATA
# ---------------------------------------------------------------------------
cat("Step 3.1: Loading data...\n")
master_with_network <- readRDS(file.path(FINAL_RESULTS_DIR, "data", "stage2_master_with_network.rds"))
g <- readRDS(file.path(FINAL_RESULTS_DIR, "data", "stage1_network.rds"))
network_size <- vcount(g)
cat("✓ Loaded. Network size:", network_size, "\n\n")

# ---------------------------------------------------------------------------
# 3.2: FEATURE ENGINEERING (NO LEAKAGE!)
# ---------------------------------------------------------------------------
cat("Step 3.2: Engineering features (no leakage)...\n")

ml_data <- master_with_network %>%
  filter(!is.na(pagerank) & !is.na(num_inventors)) %>%
  mutate(
    breakthrough = factor(is_breakthrough, levels = c(FALSE, TRUE),
                          labels = c("Regular", "Breakthrough")),
    
    # LOG TRANSFORMS
    log_pagerank = log10(pagerank + 1e-10),
    log_betweenness = log10(betweenness + 1e-10),
    log_eigenvector = log10(eigenvector + 1e-10),
    log_closeness = log10(closeness + 1e-10),
    log_backward_cit = log10(backward_citations + 1),
    log_num_claims = log10(num_claims + 1),
    log_num_inventors = log10(num_inventors + 1),
    log_patent_age = log10(patent_age + 1),
    
    # RATIOS & FLAGS
    citation_ratio = ifelse(backward_citations > 0, forward_citations / backward_citations, 0),
    has_female = factor(num_female > 0, levels = c(FALSE, TRUE)),
    is_hub = factor(is_hub, levels = c(FALSE, TRUE)),
    is_bridge = factor(is_bridge, levels = c(FALSE, TRUE)),
    is_authority = factor(is_authority, levels = c(FALSE, TRUE)),
    
    # NETWORK DEGREES
    log_in_degree = log10(in_degree + 1),
    log_out_degree = log10(out_degree + 1),
    degree_centralization = (in_degree + out_degree) / (2 * network_size)
  ) %>%
  select(
    breakthrough,
    log_pagerank, log_betweenness, log_eigenvector, log_closeness,
    log_backward_cit, citation_ratio,
    log_num_claims, log_num_inventors, log_patent_age,
    has_female, is_hub, is_bridge, is_authority,
    log_in_degree, log_out_degree, degree_centralization,
    patent_id, forward_citations  # kept for analysis, NOT used in model
  ) %>%
  na.omit()

cat("✓ Features ready. N =", nrow(ml_data), "\n\n")

# ---------------------------------------------------------------------------
# 3.3: TRAIN-TEST SPLIT
# ---------------------------------------------------------------------------
set.seed(42)
trainIndex <- createDataPartition(ml_data$breakthrough, p = 0.7, list = FALSE)
train_data <- ml_data[trainIndex, ]
test_data  <- ml_data[-trainIndex, ]

predictor_vars <- setdiff(names(ml_data), c("patent_id", "forward_citations", "breakthrough"))

train_X <- train_data %>% select(all_of(predictor_vars))
train_y <- train_data$breakthrough

test_X  <- test_data  %>% select(all_of(predictor_vars))
test_y  <- test_data$breakthrough

# Align test to train
test_X_aligned <- test_X %>%
  select(names(train_X)) %>%
  mutate(across(where(is.factor), ~factor(., levels = levels(train_X[[cur_column()]]))))

cat("✓ Split: Train =", nrow(train_X), "| Test =", nrow(test_X), "\n\n")

# ---------------------------------------------------------------------------
# 3.4: TRAIN MODEL
# ---------------------------------------------------------------------------
cat("Step 3.4: Training Random Forest...\n")

class_weights <- table(train_y)
class_weights <- max(class_weights) / class_weights

rf_model <- randomForest(
  x = train_X,
  y = train_y,
  ntree = 500,
  mtry = 4,
  importance = TRUE,
  classwt = class_weights,
  nodesize = 5
)

cat("✓ Model trained\n\n")

# ---------------------------------------------------------------------------
# 3.5: PREDICTION
# ---------------------------------------------------------------------------
cat("Step 3.5: Predicting...\n")

test_pred <- predict(rf_model, newdata = test_X_aligned, type = "response")
test_prob <- predict(rf_model, newdata = test_X_aligned, type = "prob")[, "Breakthrough"]

conf_matrix <- confusionMatrix(test_pred, test_y, positive = "Breakthrough")
roc_obj <- roc(test_y, test_prob)
auc_value <- auc(roc_obj)

cat("✓ AUC =", round(auc_value, 4), "\n")
if (auc_value >= 0.99) cat("   WARNING: AUC ~1 → possible data leakage or overfitting!\n")
cat("\n")

# ---------------------------------------------------------------------------
# 3.6: FEATURE IMPORTANCE (FIXED)
# ---------------------------------------------------------------------------
imp <- importance(rf_model)
importance_df <- as.data.frame(imp) %>%
  rownames_to_column("Feature") %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  mutate(
    Feature_Label = tools::toTitleCase(gsub("_", " ", Feature)),
    Rank = row_number()
  )

# ---------------------------------------------------------------------------
# 3.7: PLOTS (patchwork + valid data)
# ---------------------------------------------------------------------------
cat("Step 3.7: Saving figures...\n")

# Figure 5: Importance
p5a <- ggplot(head(importance_df, 15),
              aes(x = reorder(Feature_Label, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "#440154", alpha = 0.9) +
  coord_flip() +
  labs(title = "(A) Mean Decrease in Accuracy", x = "", y = "Importance") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p5b <- ggplot(head(importance_df, 15),
              aes(x = reorder(Feature_Label, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "#FDE725", alpha = 0.9) +
  coord_flip() +
  labs(title = "(B) Mean Decrease in Gini", x = "", y = "Importance") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# COMBINE with patchwork
fig5 <- p5a | p5b
ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_5_Feature_Importance.png"),
       fig5, width = 16, height = 10, dpi = 300, bg = "white")

# Figure 6: ROC
roc_data <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

p6 <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
  geom_line(color = "#2C728EFF", size = 1.5) +
  geom_abline(linetype = "dashed", color = "red") +
  annotate("text", x = 0.6, y = 0.2,
           label = paste("AUC =", round(auc_value, 4)),
           size = 6, color = "#2C728EFF", fontface = "bold") +
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal(base_size = 14)

ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_6_ROC_Curve.png"),
       p6, width = 10, height = 10, dpi = 300, bg = "white")

cat("✓ Figures saved\n\n")

# ---------------------------------------------------------------------------
# 3.8: SAVE OUTPUTS
# ---------------------------------------------------------------------------
saveRDS(rf_model, file.path(FINAL_RESULTS_DIR, "data", "stage3_rf_model.rds"))
saveRDS(test_data %>% mutate(pred_class = test_pred, pred_prob = test_prob),
        file.path(FINAL_RESULTS_DIR, "data", "stage3_predictions.rds"))

cat("════════════════════════════════════════════════════════════════════╗\n")
cat("║  STAGE 3 COMPLETE                                                 ║\n")
cat("║  Check finalresults/figures and tables                             ║\n")
cat("════════════════════════════════════════════════════════════════════╝\n\n")









































# ============================================================================
# STAGE 4 – SURVIVAL & MULTI-TASK ANALYSIS (FULLY FIXED)
# ============================================================================

# --- LOAD LIBRARIES ---
library(survival)
library(survminer)
library(glmnet)
library(ggplot2)
library(dplyr)
library(igraph)
library(patchwork)
library(viridis)
library(scales)
library(tidyr)     # for scientific notation

# --- PATHS ---
BASE_DIR <- "pATH/TO/YOUR/PROJECT/DIRECTORY"
FINAL_RESULTS_DIR <- file.path(BASE_DIR, "finalresults")
setwd(BASE_DIR)

# --- LOAD DATA ---
cat("Step 4.1: Loading Stage 2 data...\n")
master_with_network <- readRDS(file.path(FINAL_RESULTS_DIR, "data", "stage2_master_with_network.rds"))
g <- readRDS(file.path(FINAL_RESULTS_DIR, "data", "stage1_network.rds"))
network_size <- vcount(g)
cat("Loaded. N =", nrow(master_with_network), "| Network size =", network_size, "\n\n")

# --- DEFINE STUDY END ---
STUDY_END_DATE <- as.Date("2024-12-31")

# ---------------------------------------------------------------------------
# COX PROPORTIONAL HAZARDS MODEL (WITH SCALING & CONVERGENCE)
# ---------------------------------------------------------------------------
cat("Step 4.2: Cox model (scaled + maxit=100)...\n")

survival_data <- master_with_network %>%
  filter(!is.na(patent_date) & !is.na(pagerank)) %>%
  mutate(
    event_status = as.integer(is_breakthrough),
    TTE = as.numeric(STUDY_END_DATE - patent_date),
    log_pagerank = log10(pagerank + 1e-10),
    log_backward_cit = log10(backward_citations + 1),
    log_num_inventors = log10(num_inventors + 1),
    has_female = factor(num_female > 0),
    is_hub = factor(is_hub)
  ) %>%
  filter(TTE > 0) %>%
  mutate(
    # SCALE numeric predictors for convergence
    log_pagerank_s = scale(log_pagerank)[,1],
    log_backward_cit_s = scale(log_backward_cit)[,1],
    log_num_inventors_s = scale(log_num_inventors)[,1]
  )

surv_obj <- Surv(time = survival_data$TTE, event = survival_data$event_status)

cox_model <- coxph(
  surv_obj ~ log_pagerank_s + log_backward_cit_s + log_num_inventors_s +
             has_female + is_hub,
  data = survival_data,
  control = coxph.control(iter.max = 100)  # increase iterations
)

cat("Cox model fitted. Convergence:", cox_model$info$converged, "\n\n")

# ---------------------------------------------------------------------------
# Figure 8: Kaplan-Meier Survival Curves
# ---------------------------------------------------------------------------
cat("Step 4.3: Creating Figure 8 (Kaplan-Meier)...\n")

survival_plot_data <- survival_data %>%
  mutate(
    PageRank_Group = factor(ntile(log_pagerank, 4),
                            labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)")),
    Hub_Status = factor(ifelse(is_hub == TRUE, "Hub Patents", "Non-Hub Patents"))
  )

km_pagerank <- survfit(Surv(TTE, event_status) ~ PageRank_Group, data = survival_plot_data)
km_hub <- survfit(Surv(TTE, event_status) ~ Hub_Status, data = survival_plot_data)

p8a <- ggsurvplot(
  km_pagerank, data = survival_plot_data,
  palette = viridis(4, option = "plasma"),
  conf.int = TRUE, pval = TRUE,
  xlab = "Time (Days Since Grant)", ylab = "Breakthrough Probability",
  legend.title = "PageRank Quartile",
  legend.labs = c("Q4 (Highest)", "Q3", "Q2", "Q1 (Lowest)"),
  ggtheme = theme_minimal(base_size = 12)
)$plot + labs(title = "(A) Survival by PageRank Quartile")

p8b <- ggsurvplot(
  km_hub, data = survival_plot_data,
  palette = c("#3B9AB2", "#F21A00"),
  conf.int = TRUE, pval = TRUE,
  xlab = "Time (Days Since Grant)", ylab = "Breakthrough Probability",
  legend.title = "Network Role",
  ggtheme = theme_minimal(base_size = 12)
)$plot + labs(title = "(B) Survival by Hub Status")

fig8 <- p8a + p8b
ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_8_Survival_Curves.png"),
       fig8, width = 16, height = 8, dpi = 300, bg = "white")

cat("Figure 8 saved\n\n")




# ---------------------------------------------------------------------------
# Figure 9: Hazard Ratios (FIXED: use se(coef), not robust.se)
# ---------------------------------------------------------------------------
cat("Step 4.4: Creating Figure 9 (Hazard Ratios)...\n")

cox_coef <- summary(cox_model)$coefficients
cox_plot_data <- as.data.frame(cox_coef) %>%
  mutate(
    Feature = rownames(.),
    HR = exp(coef),
    CI_Lower = exp(coef - 1.96 * `se(coef)`),   # FIXED: use se(coef)
    CI_Upper = exp(coef + 1.96 * `se(coef)`),   # FIXED
    Significant = `Pr(>|z|)` < 0.05,
    Feature_Label = case_when(
      Feature == "log_pagerank_s" ~ "Log PageRank (scaled)",
      Feature == "log_backward_cit_s" ~ "Log Backward Citations (scaled)",
      Feature == "log_num_inventors_s" ~ "Log Number of Inventors (scaled)",
      Feature == "has_femaleTRUE" ~ "Has Female Inventor",
      Feature == "is_hubTRUE" ~ "Is Hub Patent",
      TRUE ~ Feature
    )
  )

p9 <- ggplot(cox_plot_data,
             aes(x = HR, y = reorder(Feature_Label, HR), color = Significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = .3, linewidth = 1.5) +
  geom_point(size = 5) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#F21A00"),
                     labels = c("Not Sig.", "Sig. (p<0.05)")) +
  scale_x_log10() +
  labs(title = "Cox Model: Hazard Ratios",
       x = "Hazard Ratio (log scale)", y = "", color = "Significance") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_9_Cox_Hazard_Ratios.png"),
       p9, width = 12, height = 8, dpi = 300, bg = "white")

cat("Figure 9 saved\n\n")


# ---------------------------------------------------------------------------
# MULTI-TASK LASSO
# ---------------------------------------------------------------------------
cat("Step 4.5: Multi-Task LASSO...\n")

ml_multitask <- master_with_network %>%
  filter(!is.na(pagerank) & !is.na(num_inventors)) %>%
  mutate(
    Y1_Breakthrough = as.numeric(is_breakthrough),
    Y2_High_Team = as.numeric(num_inventors >= 3),
    Y3_High_Citations = as.numeric(forward_citations > median(forward_citations, na.rm = TRUE)),
    log_pagerank = log10(pagerank + 1e-10),
    log_betweenness = log10(betweenness + 1e-10),
    log_backward_cit = log10(backward_citations + 1),
    log_num_claims = log10(num_claims + 1),
    has_female = as.numeric(num_female > 0),
    is_hub = as.numeric(is_hub),
    team_diversity = ifelse(num_inventors > 1,
                            1 - (num_female/num_inventors)^2 - (num_male/num_inventors)^2, 0)
  ) %>%
  select(Y1_Breakthrough, Y2_High_Team, Y3_High_Citations,
         log_pagerank, log_betweenness, log_backward_cit, log_num_claims,
         has_female, is_hub, team_diversity) %>%
  na.omit()

X_multi <- as.matrix(ml_multitask %>% select(-Y1_Breakthrough, -Y2_High_Team, -Y3_High_Citations))
Y_multi <- as.matrix(ml_multitask %>% select(Y1_Breakthrough, Y2_High_Team, Y3_High_Citations))

set.seed(42)
multi_lasso_cv <- cv.glmnet(X_multi, Y_multi, family = "mgaussian",
                            type.measure = "mse", alpha = 1, nfolds = 5)
coef_matrix <- coef(multi_lasso_cv, s = "lambda.min")

# ---------------------------------------------------------------------------
# Figure 10: Heatmap (NOW WORKS with tidyr)
# ---------------------------------------------------------------------------
coef_df <- data.frame(
  Feature = rownames(coef_matrix[[1]]),
  Breakthrough = as.numeric(coef_matrix[[1]]),
  HighTeam = as.numeric(coef_matrix[[2]]),
  HighCitations = as.numeric(coef_matrix[[3]])
) %>% filter(Feature != "(Intercept)") %>%
  mutate(
    Feature_Label = case_when(
      Feature == "log_pagerank" ~ "Log PageRank",
      Feature == "log_betweenness" ~ "Log Betweenness",
      Feature == "log_backward_cit" ~ "Log Backward Cit.",
      Feature == "log_num_claims" ~ "Log Claims",
      Feature == "has_female" ~ "Has Female",
      Feature == "is_hub" ~ "Is Hub",
      Feature == "team_diversity" ~ "Team Diversity",
      TRUE ~ Feature
    )
  ) %>%
  pivot_longer(                     # NOW WORKS
    cols = c(Breakthrough, HighTeam, HighCitations),
    names_to = "Outcome",
    values_to = "Coefficient"
  ) %>%
  mutate(
    Outcome_Label = case_when(
      Outcome == "Breakthrough" ~ "Breakthrough\nStatus",
      Outcome == "HighTeam" ~ "Large Team\nSize",
      Outcome == "HighCitations" ~ "High Citation\nImpact"
    )
  )

p10 <- ggplot(coef_df,
              aes(x = Outcome_Label, y = Feature_Label, fill = Coefficient)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.3f", Coefficient)), color = "white", size = 4) +
  scale_fill_gradient2(low = "#3B9AB2", mid = "white", high = "#F21A00",
                       midpoint = 0, name = "Coefficient") +
  labs(title = "Multi-Task LASSO Coefficients", x = "Outcome", y = "Predictor") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"))

ggsave(file.path(FINAL_RESULTS_DIR, "figures", "Figure_10_MultiTask_Heatmap.png"),
       p10, width = 10, height = 8, dpi = 300, bg = "white")

cat("Figure 10 saved\n\n")
# ---------------------------------------------------------------------------
# SAVE MODELS & DATA
# ---------------------------------------------------------------------------
saveRDS(cox_model, file.path(FINAL_RESULTS_DIR, "data", "stage4_cox_model.rds"))
saveRDS(multi_lasso_cv, file.path(FINAL_RESULTS_DIR, "data", "stage4_multitask_model.rds"))
saveRDS(survival_data, file.path(FINAL_RESULTS_DIR, "data", "stage4_survival_data.rds"))

cat("════════════════════════════════════════════════════════════════════╗\n")
cat("║  STAGE 4 COMPLETE                                                 ║\n")
cat("║  All figures and models saved in finalresults/                    ║\n")
cat("════════════════════════════════════════════════════════════════════╝\n\n")


