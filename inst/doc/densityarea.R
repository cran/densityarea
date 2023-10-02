## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message = FALSE,
  comment = "#>",
  dev = "ragg_png",
  dpi = 300
)

## ----setup--------------------------------------------------------------------
# package depends
library(densityarea)
library(dplyr)
library(sf)
library(ggdensity)

## ----setup2, eval = F---------------------------------------------------------
#  library(ggplot2)

## ----include=F----------------------------------------------------------------
ggplot2_inst <- require(ggplot2)

## ----dataload-----------------------------------------------------------------
data(s01, package = "densityarea")
head(s01)

## ----eval=ggplot2_inst, fig.width=5, fig.height=3, fig.align='center', out.width="100%"----
ggplot(data = s01,
       aes(x = F2,
           y = F1)
       )+
  geom_point(alpha = 0.1)+
  stat_hdr(probs = c(0.8, 0.5),
           aes(fill = after_stat(probs)),
           color = "black",
           alpha = 0.8)+
  scale_y_reverse()+
  scale_x_reverse()+
  scale_fill_brewer(type = "seq")+
  coord_fixed()

## -----------------------------------------------------------------------------
s01 |> 
  mutate(
    lF1 = -log(F1),
    lF2 = -log(F2)
  ) -> 
  s01

## -----------------------------------------------------------------------------
s01 |> 
  group_by(name) |> 
  reframe(
    density_area(lF2, lF1, probs = 0.8)
  ) 

## -----------------------------------------------------------------------------
s01 |> 
  group_by(name, vowel) |> 
  reframe(
    density_area(lF2, lF1, probs = 0.8)
  ) ->
  vowel_areas

## -----------------------------------------------------------------------------
vowel_areas |> 
  arrange(desc(area))

## -----------------------------------------------------------------------------
s01 |> 
  group_by(name) |> 
  reframe(
    density_polygons(lF2, lF1, probs = 0.6)
  )->
  sixty_poly_df

head(sixty_poly_df)

## ----eval=ggplot2_inst, fig.width=4, fig.height=4, fig.align='center', out.width="80%"----
ggplot(sixty_poly_df,
       aes(lF2, lF1))+
  geom_polygon(
    aes(color = prob,
        group = prob),
    fill = NA,
    linewidth = 1
  )+
  coord_fixed()

## -----------------------------------------------------------------------------
s01 |> 
  group_by(name) |> 
  reframe(
    density_polygons(lF2, 
                     lF1, 
                     probs = c(0.6, 0.8))
  )->
  multi_poly_df

head(multi_poly_df)

## ----eval=ggplot2_inst, fig.width=4, fig.height=4, fig.align='center', out.width="80%"----
ggplot(multi_poly_df,
       aes(lF2, lF1))+
  geom_polygon(
    aes(color = prob,
        group = prob),
    fill = NA,
    linewidth = 1
  )+
  coord_fixed()

## -----------------------------------------------------------------------------
s01 |> 
  group_by(name) |> 
  reframe(
    density_polygons(lF2,
                     lF1,
                     probs = c(0.8, 0.6),
                     as_sf = TRUE)
  ) |> 
  st_sf()->
  multi_poly_sf

## -----------------------------------------------------------------------------
multi_poly_sf

## ----eval=ggplot2_inst, fig.width=4, fig.height=4, fig.align='center', out.width="80%"----
ggplot(multi_poly_sf)+
  geom_sf(aes(color = prob),
          fill = NA)

