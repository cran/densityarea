## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  comment = "#>",
  dev = "ragg_png",
  dpi = 300  
)

## ----setup--------------------------------------------------------------------
# package dependencies
library(densityarea)
library(dplyr)
library(purrr)
library(sf)

## ----setup2, eval=FALSE-------------------------------------------------------
# # package suggests
# library(tidyr)
# library(stringr)
# library(ggplot2)

## ----include=F----------------------------------------------------------------
tidyr_inst <- require(tidyr)
stringr_inst <- require(stringr)
ggplot2_inst <- require(ggplot2)
forcats_inst <- require(forcats)

all_suggest <- all(c(tidyr_inst, stringr_inst, ggplot2_inst, forcats_inst))

## -----------------------------------------------------------------------------
data(s01)

s01 |> 
  mutate(lF1 = -log(F1),
         lF2 = -log(F2)) ->
  s01

## -----------------------------------------------------------------------------
s01 |> 
  filter(
    plt_vclass %in% c("iy", 
                      "ey", 
                      "o", 
                      "oh")
  ) ->
  vowel_subset

## -----------------------------------------------------------------------------
vowel_subset |> 
  group_by(plt_vclass) |> 
  reframe(
    density_polygons(lF2, 
                     lF1, 
                     probs = 0.8,
                     as_sf = TRUE)
  ) |> 
  st_sf()->
  vowel_polygons

vowel_polygons

## ----eval = ggplot2_inst, fig.width=5, fig.height=5, out.width="80%", fig.align='center'----
ggplot(vowel_polygons) +
  geom_sf(
    aes(fill = plt_vclass),
    alpha = 0.6
  ) +
    scale_fill_brewer(palette = "Dark2")

## -----------------------------------------------------------------------------
vowel_polygons |> 
  mutate(
    area = st_area(geometry)
  ) ->
  vowel_polygons

## ----eval=ggplot2_inst, fig.width=5, fig.height=5, out.width="80%", fig.align='center'----
ggplot(vowel_polygons) +
  geom_sf(
    aes(fill = area),
    alpha = 0.6
  )+
  scale_fill_viridis_c()

## ----eval=ggplot2_inst, fig.width=5, fig.height=5, out.width="80%", fig.align='center'----
vowel_polygons |> 
  st_centroid() |> 
  ggplot()+
    geom_sf_label(
      aes(label = plt_vclass,
          color = plt_vclass,
          size = area)
    )+
    scale_color_brewer(palette = "Dark2")+
    coord_fixed()

## -----------------------------------------------------------------------------
vowel_polygons |> 
  st_intersection() -> 
  vowel_intersections

vowel_intersections

## ----eval=ggplot2_inst, fig.width=5, fig.height=5, out.width="80%", fig.align='center'----
ggplot(vowel_intersections) +
  geom_sf(
    aes(fill = n.overlaps)
  )+
  scale_fill_viridis_c()

## ----eval=stringr_inst, echo=stringr_inst-------------------------------------
new_label <- function(indices, labels){
  str_c(labels[indices],
        collapse = "~")
}

## ----eval=!stringr_inst, echo=!stringr_inst-----------------------------------
# new_label <- function(indicies, labels){
#   paste0(labels[indicies], collapse = "~")
# }

## -----------------------------------------------------------------------------
vowel_intersections |> 
  mutate(
    groups = map_chr(
      origins,
      .f = new_label,
      labels = vowel_polygons$plt_vclass
    ) 
  ) |> 
  relocate(groups, .after = plt_vclass)->
  vowel_intersections

vowel_intersections

## ----eval = ggplot2_inst, fig.width=5, fig.height=5, out.width="80%", fig.align='center'----
ggplot(vowel_intersections)+
  geom_sf(
    aes(fill = groups)
  )+
  scale_fill_brewer(palette = "Dark2")

## ----eval=ggplot2_inst, fig.width=5, fig.height=3, out.width="80%", fig.align='center'----
vowel_intersections |> 
  mutate(
    group_area = st_area(geometry),
    overlapped_proportion = 1-(group_area/area)
  ) |> 
  filter(n.overlaps == 1) |> 
  ggplot(
    aes(plt_vclass, overlapped_proportion)
  )+
    geom_col()+
    ylim(0,1)

## -----------------------------------------------------------------------------
library(sfheaders)

## ----eval=F-------------------------------------------------------------------
# library(forcats)

## -----------------------------------------------------------------------------
s01 |> 
  sfheaders::sf_point(
    x = "lF2",
    y = "lF1",
    keep = TRUE
  ) ->
  s01_sf

s01_sf

## ----eval=ggplot2_inst, fig.width=5, fig.height=5, out.width="80%", fig.align='center'----
s01_sf |> 
  ggplot()+
    geom_sf()

## -----------------------------------------------------------------------------
s01 |> 
  filter(plt_vclass == "iy") |> 
  reframe(
    density_polygons(lF2, lF1, probs = 0.8, as_sf =T)
  ) |> 
  st_sf()->
  iy_sf

## -----------------------------------------------------------------------------
s01_sf |> 
  st_filter(
    iy_sf,
    .predicate = st_covered_by
  )->
  covered_by_iy

## ----eval=all_suggest, fig.width=5, fig.height=5, out.width="80%", fig.align='center'----
covered_by_iy |> 
  mutate(plt_vclass = plt_vclass |> 
           fct_infreq() |> 
           fct_lump_n(5)) |> 
  ggplot()+
    geom_sf(data = iy_sf)+
    geom_sf(aes(color = plt_vclass))+
    scale_color_brewer(palette = "Dark2")

## ----eval=all_suggest, fig.width=5, fig.height=3, out.width="80%", fig.align='center'----
covered_by_iy |> 
  mutate(plt_vclass = plt_vclass |> 
           fct_infreq() |> 
           fct_lump_n(5)) |> 
  count(plt_vclass) |> 
  ggplot(aes(plt_vclass, n))+
    geom_col(
      aes(fill = plt_vclass)
    )+
    scale_fill_brewer(palette = "Dark2")

## -----------------------------------------------------------------------------
set.seed(100)
s01_sf |> 
  slice_sample(n = 1)->
  rand_vowel

rand_vowel

## -----------------------------------------------------------------------------
s01 |>
  group_by(plt_vclass) |>
  reframe(
    density_polygons(
      lF2,
      lF1,
      probs = ppoints(5),
      range_mult = 0.5,
      as_sf = T
    )
  ) |> 
  st_sf() ->
  vowel_probs

## ----eval=tidyr_inst, echo=tidyr_inst-----------------------------------------
vowel_probs |> 
  st_join(
    rand_vowel,
    .predicate = st_covers
  ) |> 
  drop_na()->
  vowel_within

## ----eval=!tidyr_inst, echo=!tidyr_inst---------------------------------------
# vowel_probs |>
#   st_join(
#     rand_vowel,
#     .predicate = st_covers
#   ) |>
#   filter(
#     !is.na(plt_vclass.y)
#   ) ->
#   vowel_within

## ----eval=forcats_inst, echo = forcats_inst-----------------------------------
vowel_within |> 
  group_by(plt_vclass.x) |> 
  filter(prob == min(prob)) |> 
  ungroup() |> 
  mutate(plt_vclass = fct_reorder(plt_vclass.x, prob)) ->
  vowel_min_prob

## ----eval=!forcats_inst, echo = !forcats_inst---------------------------------
# vowel_within |>
#   group_by(plt_vclass.x) |>
#   filter(prob == min(prob)) |>
#   ungroup() |>
#   mutate(plt_vclass = reorder(factor(plt_vclass.x),
#                               prob)) ->
#   vowel_min_prob

## ----eval = ggplot2_inst, fig.width=5, fig.height = 5, out.width="80%"--------
vowel_min_prob |> 
  ggplot()+
    geom_sf(aes(fill = prob)) +
    geom_sf(data = rand_vowel |> mutate(plt_vclass = NULL),
            color = "red") +
    facet_wrap(~plt_vclass)

