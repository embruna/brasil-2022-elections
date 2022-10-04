# if (!require("tidyverse")) install.packages("tidyverse")
# if (!require("jsonlite")) install.packages("jsonlite")
library(tidyverse)
library(jsonlite)
library(tidyverse)

# nationwide totals -------------------------------------------------------

# url <- "https://resultados.tse.jus.br/oficial/ele2022/544/dados-simplificados/br/br-c0001-e000544-r.json"
#
# summary <- jsonlite::fromJSON(url) %>%
#   purrr::pluck("cand") %>%
#   dplyr::select(nm, vap)


# a single state ----------------------------------------------------------

# url <- "https://resultados.tse.jus.br/oficial/ele2022/544/dados-simplificados/ac/ac-c0001-e000544-r.json"


# grab data ---------------------------------------------------------------

# postal codes

postal <- c(
  "AC", "AL", "AP",
  "AM", "BA", "CE",
  "DF", "ES", "GO",
  "MA", "MT", "MS",
  "MG", "PA", "PB",
  "PR", "PE", "PI",
  "RJ", "RN", "RS",
  "RO", "RR", "SC",
  "SP", "SE", "TO",
  "ZZ"
) # ZZ=exterior

postal <- tolower(postal)

# Set up the function to query each state's vote totals

# i=1
api_return <- data.frame(
  postal = as.factor(NA),
  # seq=NA,
  # sqcand=NA,
  nm = as.factor(NA),
  # cc=NA,
  # nv=NA,
  # e=NA,
  # st=NA,
  # dvt=NA,
  # pvap=NA,
  vap = as.double(NA)
)

for (i in seq_along(postal)) {
  a <- "https://resultados.tse.jus.br/oficial/ele2022/544/dados-simplificados/"
  b <- "/"
  c <- "-c0001-e000544-r.json"


  url <- paste(a, postal[i], b, postal[i], c, sep = "")
  s <- jsonlite::fromJSON(url) %>%
    purrr::pluck("cand")

  elect_count <- bind_cols(as.factor(postal[i]), as.factor(s$nm), as.double(s$vap))

  names(elect_count) <- c("postal", "nm", "vap")

  api_return <- bind_rows(elect_count, api_return)
}

api_return <- api_return %>%
  mutate(
    postal = as.factor(postal),
    nm = as.factor(nm),
    vap = as.numeric(vap)
  ) %>%
  drop_na()
api_return

# write_csv(api_return,"./results.csv")


# national totals - all candidates ----------------------------------------

all_sum <- api_return %>%
  group_by(postal, nm) %>%
  summarize(n = sum(vap))


# state summaries - all candidates ----------------------------------------
# remove exterior

state_sum <- api_return %>%
  filter(postal != "zz") %>%
  group_by(postal, nm) %>%
  summarize(n = sum(vap))


# total votes - each state ------------------------------------------------


state_total_votes <- api_return %>%
  filter(postal != "zz") %>%
  group_by(postal) %>%
  summarize(state_total_votes = sum(vap))


# Lula and Bolsonaro - only -----------------------------------------------

LB <- api_return %>%
  filter(nm == "LULA" | nm == "JAIR BOLSONARO") %>%
  filter(postal != "zz") %>%
  # group_by(postal) %>%
  # arrange(desc(vap)) %>%
  # slice(1) %>%
  pivot_wider(names_from = c(nm), values_from = vap) %>%
  rename(JB = `JAIR BOLSONARO`) %>%
  left_join(state_total_votes) %>%
  mutate(total_LJB = LULA + JB) %>%
  mutate(percent_Lula = LULA / state_total_votes * 100) %>%
  arrange(desc(percent_Lula))



# cartogram ---------------------------------------------------------------



# https://r-charts.com/spatial/cartogram-ggplot2/


theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cartogram)
# remotes::install_github("ropensci/rnaturalearthhires") # ONLY OPTION THAT WORKED
library(rnaturalearthhires)

# maps --------------------------------------------------------------------

bz <- ne_states(country = "brazil", returnclass = "sf")
names(bz)
bz$postal <- tolower(bz$postal)
bz1 <- left_join(bz, LB)
names(bz1)
class(bz1)


# 54009 World Mollweide [equal area]	Mollweide
# 3857 Google mercator
bz_cart <- st_transform(bz1, 3857)

# https://geocompr.github.io/post/2019/crs-projections-transformations/

# actual area size
bz %>%
  st_transform("+proj=moll") %>%
  st_geometry() %>%
  plot()

bz_cart <- st_transform(bz1, "+proj=moll")

# names(bz_cart)
# https://stackoverflow.com/questions/54831322/cartogram-in-r-not-distorting-countries-as-expected
cartog_cont <- cartogram_cont(bz_cart,
  weight = "state_total_votes"
)

ggplot(cartog_cont) +
  geom_sf()

ggplot(cartog_cont) +
  geom_sf(aes(fill = percent_Lula))
#
# ggplot(cartog_cont) +
#   geom_sf(aes(fill = percent_Lula), color = "black") +
#   scale_fill_gradient(low = "navyblue", high = "red", na.value = NA)
#
#


Fig1 <- ggplot(cartog_cont) +
  geom_sf(aes(fill = percent_Lula), color = "black") +
  scale_fill_gradient(low = "navyblue", high = "red", na.value = NA) +
  theme(
    legend.position = "right",
    legend.margin = margin(t = 10, b = 2),
    # legend.title = element_text(size = 7),
    legend.title = element_blank(),
    legend.text = element_text(
      angle = 45,
      margin = margin(t = 5)
    )
  ) +
  labs(
    title = "% Vote for Lula",
    subtitle = "State size proportional to the number of votes cast there\n(map by @BrunaLab)"
  )


Fig1

