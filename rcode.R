###########MAP###########
library(tidyverse)
library(igraph)
library(ggraph)

df <- read.csv("D:/B硕士-CITYU/IS6335 Data Visualization/individual project/forR.csv", stringsAsFactors = FALSE)

df_clean <- df %>%
  select(Country, category) %>%
  drop_na()

df_clean <- df_clean %>%
  mutate(
    Country = trimws(Country),
    category = trimws(category)
  ) %>%
  filter(Country != "", category != "")

edgelist <- df_clean %>%
  count(Country, category, name = "weight") %>%
  filter(weight > 0)

g <- graph_from_data_frame(
  edgelist,
  directed = FALSE,
  vertices = data.frame(
    name = c(unique(edgelist$Country), unique(edgelist$category)),
    type = c(rep(TRUE, length(unique(edgelist$Country))),  # TRUE = Country
             rep(FALSE, length(unique(edgelist$category))) # FALSE = Category
    ),
    stringsAsFactors = FALSE
  )
)

V(g)$label <- V(g)$name

set.seed(123)
ggraph(g, layout = "nicely") +
  geom_edge_link(
    aes(edge_width = weight),
    alpha = 0.3,
    color = "steelblue"
  ) +
  geom_node_point(
    aes(color = as.factor(type)),
    size = 3
  ) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    size = 2.5,
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.5, 4)) +
  scale_color_manual(
    values = c("TRUE" = "#D95F02", "FALSE" = "#1B9E77"),
    labels = c("Country", "Category")
  ) +
  labs(
    title = "Network: YouTube Channel Categories by Country (Top Channels)",
    subtitle = "Edges represent presence of at least one top channel in a category from a country",
    caption = "Edge width ∝ number of channels"
  ) +
  theme_graph(base_family = "Arial") +
  theme(legend.title = element_blank())










########sankey_plot#######
library(tidyverse)
library(networkD3)

df <- read.csv("D:/B硕士-CITYU/IS6335 Data Visualization/individual project/forR.csv", stringsAsFactors = FALSE)

df_clean <- df %>%
  select(category, uploads, highest_monthly_earnings) %>%
  mutate(
    uploads = as.numeric(uploads),
    earnings = as.numeric(highest_monthly_earnings)
  ) %>%
  drop_na() %>%
  filter(uploads > 0, earnings > 0)

create_tiers <- function(x) {
  cut(x,
      breaks = quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("Low", "Medium", "High"),
      include.lowest = TRUE)
}

df_binned <- df_clean %>%
  mutate(
    Upload_Tier = create_tiers(uploads),
    Earnings_Tier = create_tiers(earnings),
    category = as.character(category)
  )

links <- df_binned %>%
  count(Upload_Tier, Earnings_Tier, category, name = "value")

nodes_df <- data.frame(
  name = c(
    paste0("Uploads: ", unique(links$Upload_Tier)),
    paste0("Earnings: ", unique(links$Earnings_Tier))
  ),
  stringsAsFactors = FALSE
)

links$source_id <- match(paste0("Uploads: ", links$Upload_Tier), nodes_df$name) - 1
links$target_id <- match(paste0("Earnings: ", links$Earnings_Tier), nodes_df$name) - 1

links <- links[complete.cases(links[c("source_id", "target_id")]), ]

unique_cats <- sort(unique(links$category))

okabe_ito_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
  "#D55E00", "#CC79A7", "#999999", "#000000", "#E69F00"
)

if (length(unique_cats) > length(okabe_ito_palette)) {
  cat_colors <- setNames(viridis::viridis(length(unique_cats)), unique_cats)
} else {
  cat_colors <- setNames(okabe_ito_palette[1:length(unique_cats)], unique_cats)
}

links$color <- cat_colors[links$category]

sankey_plot <- sankeyNetwork(
  Links = links,
  Nodes = nodes_df,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "name",
  LinkGroup = "category",
  colourScale = JS(paste0(
    "d3.scaleOrdinal().domain([\"", paste(unique_cats, collapse = "\", \""), "\"])",
    ".range([\"", paste(cat_colors, collapse = "\", \""), "\"])")),
  fontSize = 13,
  nodeWidth = 30,
  iterations = 0
)
sankey_plot