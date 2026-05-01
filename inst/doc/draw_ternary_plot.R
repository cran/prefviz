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
aecdop22_transformed <- prefviz::aecdop22_transformed |> 
  filter(CountNumber == 0)
head(aecdop22_transformed)

tern22 <- as_ternable(data = aecdop22_transformed, items = ALP:Other)
tern22

## -----------------------------------------------------------------------------
tern22 <- as_ternable(aecdop22_transformed, ALP:Other)

## -----------------------------------------------------------------------------
input_df <- get_tern_data2d(tern22)
head(input_df)

## -----------------------------------------------------------------------------
p <- ggplot(input_df, aes(x = x1, y = x2)) +
  # Draw the ternary space as an equilateral triangle
  add_ternary_base() + 
  # Plot the observations as points
  geom_point(aes(color = ElectedParty)) + 
  # Add vertex labels, taken from the ternable object
  add_vertex_labels(tern22$simplex_vertices) + 
  labs(title = "First preference in 2022 Australian Federal election")

p

## -----------------------------------------------------------------------------
p + 
  geom_ternary_region(
    x1 = 1/3, x2 = 1/3, x3 = 1/3, # Default reference points. Must sum to 1
    vertex_labels = tern22$vertex_labels, # Labels for the regions
    aes(fill = after_stat(vertex_labels)), 
    alpha = 0.3, color = NA, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c("ALP" = "red", "LNP" = "blue", "Other" = "grey70"),
    aesthetics = c("fill", "colour")
  )

## -----------------------------------------------------------------------------
# Load the data
aecdop25_transformed <- prefviz::aecdop25_transformed |> 
  filter(CountNumber == 0)
head(aecdop25_transformed)

tern25 <- as_ternable(aecdop25_transformed, ALP:IND)

# Animate the tour
tourr_data <- get_tern_datahd(tern25)
animate_xy(
  dplyr::select(tourr_data, starts_with("x")), # Coordinates of the observations and vertices
  edges = get_tern_edges(tern25), # Edges of the simplex
  obs_labels = tourr_data[["labels"]], # Labels for the vertices
  axes = "bottomleft"
)

## -----------------------------------------------------------------------------
# Define color mapping
party_colors <- c(
  "ALP" = "#E13940",    # Red
  "LNP" = "#1C4F9C",    # Blue
  "GRN" = "#10C25B",    # Green
  "IND" = "#F39C12",    # Orange
  "Other" = "#95A5A6"   # Gray
)

# Map to your data (assuming your column is called elected_party)
color_vector <- c(rep("black", 5),
  party_colors[aecdop25_transformed$ElectedParty])

# Animate the tour (tourr_data already defined above)
animate_xy(
  dplyr::select(tourr_data, starts_with("x")),
  edges = get_tern_edges(tern25),
  obs_labels = tourr_data[["labels"]],
  col = color_vector,
  axes = "bottomleft"
)

