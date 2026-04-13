## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(prefviz)
library(ggplot2)
library(tourr)
library(dplyr)

## -----------------------------------------------------------------------------
input_df <- prefviz::aecdop22_transformed |> 
   filter(DivisionNm %in% c("Higgins", "Monash", "Melbourne"))
head(input_df)

# Create ternable object
tern22 <- as_ternable(input_df, ALP:Other)

## -----------------------------------------------------------------------------
# The base plot
p <- get_tern_data(tern22, plot_type = "2D") |> 
  ggplot(aes(x = x1, y = x2)) +
  add_ternary_base() +
  geom_ternary_region(
    aes(fill = after_stat(vertex_labels)),
    vertex_labels = tern22$vertex_labels,
    alpha = 0.3, color = "grey50",
    show.legend = FALSE
  ) +
  geom_point(aes(color = ElectedParty)) +
  add_vertex_labels(tern22$simplex_vertices) +
  scale_color_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70"),
    aesthetics = c("fill", "colour")
  )

# Add ordered paths
p + stat_ordered_path(
  aes(group = DivisionNm, order_by = CountNumber, color = ElectedParty), 
  size = 0.5
)

## -----------------------------------------------------------------------------
input_df2 <- prefviz::aecdop25_transformed |>
  filter(DivisionNm %in% c("Monash", "Melbourne"))
head(input_df2)

# Create ternable object
tern25 <- as_ternable(input_df2, ALP:IND, group = DivisionNm)

## -----------------------------------------------------------------------------
# Add colors
party_colors <- c(
  "ALP" = "#E13940",    # Red
  "LNP" = "#1C4F9C",    # Blue
  "GRN" = "#10C25B",    # Green
  "IND" = "#F39C12",    # Orange
  "Other" = "#95A5A6"   # Gray
)

color_vector <- c(rep("black", 5),
  party_colors[input_df2$ElectedParty])

edges_color <- c(rep("black", nrow(tern25$simplex_edges)),
  party_colors[input_df2$ElectedParty])

# Animate the tour
animate_xy(
  get_tern_data(tern25, plot_type = "HD"), 
  col = color_vector,
  edges = get_tern_edges(tern25, include_data = TRUE),
  edges.col = edges_color,
  obs_labels  = get_tern_labels(tern25),
  axes = "bottomleft"
)

