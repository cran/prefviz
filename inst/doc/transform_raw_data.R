## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(prefviz)
library(prefio)
library(dplyr)
library(kableExtra)

## -----------------------------------------------------------------------------
df <- tibble(
  electorate = c("A", "B", "C"),
  PartyA = c(0.5, 0.4, 0.6),
  PartyB = c(0.3, 0.4, 0.2),
  PartyC = c(0.2, 0.2, 0.2)
)

df

## -----------------------------------------------------------------------------
nswla <- read_preflib("00058 - nswla/00058-00000171.soi", from_preflib = TRUE)
nswla

## -----------------------------------------------------------------------------
dop_irv(
  nswla, value_type = "percentage",
  preferences_col = preferences,
  frequency_col = frequency)

## -----------------------------------------------------------------------------
ballot_long <- tibble(
  ballot_id = c(1, 1, 1,
                2, 2, 2,
                3, 3, 3,
                4, 4, 4,
                5, 5, 5),
  elect_division = "Melbourne",
  party = c("ALP", "LNP", "Other",
            "ALP", "LNP", "Other",
            "ALP", "LNP", "Other",
            "ALP", "LNP", "Other",
            "ALP", "LNP", "Other"),
  preference_rank = c(1, 2, 3,
                 2, 1, 3,
                 3, 2, 1,
                 1, 3, 2,
                 2, 3, 1)
)

ballot_long |> kable()

## -----------------------------------------------------------------------------
# Convert to PrefLib format
preflib_long <- prefio::long_preferences(
  ballot_long,
  vote,
  id_cols = c(ballot_id, elect_division),
  item_col = party,
  rank_col = preference_rank
)

# Convert to ternable-friendly format
dop_irv(preflib_long$vote, value_type = "percentage")

## -----------------------------------------------------------------------------
ballot_wide <- tibble(
  ballot_id = 1:5,
  elect_division = "Melbourne",
  ALP = c(1, 2, 3, 1, 2),
  LNP = c(2, 1, 2, 3, 3),
  Other = c(3, 3, 1, 2, 1)
)

ballot_wide |> kable()

## -----------------------------------------------------------------------------
# Convert to PrefLib format
preflib_wide <- prefio::wide_preferences(ballot_wide, vote, ALP:Other)

# Convert to ternable-friendly format
dop_irv(preflib_wide$vote, value_type = "percentage")

## -----------------------------------------------------------------------------
# only include preference percentage and parties of interest

aecdop_2025 <- aecdop_2025 |> 
  filter(CalculationType == "Preference Percent") |>
  mutate(Party = case_when(
    # all parties not in the main parties are grouped into "Other"
    !(PartyAb %in% c("LP", "ALP", "NP", "LNP", "LNQ", "GRN", "IND")) ~ "Other", 
    # group all parties in the Coalition into "LNP"
    PartyAb %in% c("LP", "NP", "LNP", "LNQ") ~ "LNP",
    TRUE ~ PartyAb
  ))

## -----------------------------------------------------------------------------
dop_transform(
  data = aecdop_2025,
  key_cols = c(DivisionNm, CountNumber),
  value_col = CalculationValue,
  item_col = Party,
  winner_col = Elected,
  winner_identifier = "Y"
)

