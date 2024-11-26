#' ---
#' title: "Nó Inicial de uma Rede"
#' subtitle: "A partir da camada de hidrografia encontrar os nós iniciais da rede"
#' author: Vinicio Coelho Lima
#' email: viniciovcl@gmail.com
#' date: Novembro, 2024
#' output:
#'    pdf_document:
#'      toc: true
#'      toc_depth: 4
#'      highlight: tango
#'      latex_engine: lualatex
#'    html_document:
#'      toc: true
#'      toc_depth: 4
#'      highlight: tango
#' header-includes:
#'     - \usepackage{float}
#'     - \renewcommand{\contentsname}{Conteúdo}
#' ---

#' ## Pacotes

#+ pacotes, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE


library(dplyr)
library(sf)
library(lwgeom)


#' ## Área de Interesse

#+ AOI, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

# Subset da area - Limite exemplo
# xmin      ymin      xmax      ymax
# -52.45996 -20.05684 -51.66483 -19.57295

pts <- st_sf(pt = 1:2,
             geom = st_sfc(st_point(c(
               -52.45996, -20.05684
             )), st_point(c(
               -51.66483, -19.57296
             ))),
             crs = 4326)

pol <- pts %>% st_bbox() %>% st_as_sfc(., crs = 4326)

mask <- st_as_text(st_geometry(pol))

#+ IBGE, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

#' ## Base Cartográfica IBGE 1:250.000

bc_250 <- "/home/vinicio/Documentos/bc250_ibge.gpkg" #

sf::st_layers(bc_250) # layers do gdb

#' ## Hidrografia

#+ Hidro, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

hid <- st_read(bc_250, layer = "hid_trecho_drenagem_l",
               wkt_filter = mask) # Evita carregar a layer BR

#+ start, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

#' ## Nó inicial

start <- lwgeom::st_startpoint(hid) %>% st_difference() %>% st_as_sf()

#' ## Nó final

#+ end, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

end <- lwgeom::st_endpoint(hid) %>% st_difference() %>% st_as_sf()

#' ### Buffer

#+ Buffer, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

end_pol <- st_buffer(end, 100)
# mapview::mapview(end_pol)
start_pol <- st_buffer(start, 100)

#' ###  Diferença

#+ Diff, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

no_start <- st_erase(st_union(st_combine(start)), st_union(end_pol))


#' ##  Plot

#+ Plot, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

library(ggplot2)

ext.mapa <- c(xmin = -51.8, xmax = -52.2, ymin = -19.55, ymax = -20,2)

p <- ggplot() +
  geom_sf(
    data = hid,
    fill = "darkblue",
    color = "darkblue",
    linewidth = .25,
    alpha = 100
  ) +
  geom_sf(data = no_start,
          color = "red",
          size = 1.3) +
  coord_sf(xlim = c(ext.mapa["xmin"], ext.mapa["xmax"]),
           ylim = c(ext.mapa["ymin"], ext.mapa["ymax"])) +
  theme_minimal()  +
  labs(
    title = "Nó Inicial da Rede",
    subtitle = "",
    caption = "
    A partir da camada de hidrografia encontrar os nós iniciais da rede."
  ) +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      family = "serif"
    ),
    plot.subtitle = element_text(
      size = 9,
      face = "plain",
      family = "mono"
    ),
    plot.caption = element_text(
      size = 6,
      face = "plain",
      family = "mono"
    ),
    axis.text = element_text(family = "mono"),
    axis.text.x = element_text(family = "mono"),
    axis.text.y = element_text(family = "mono"),
    legend.key.height = unit(0.10, 'npc'),
    legend.key.width = unit(0.04, 'npc'))

ggsave(
  plot = p,
  filename = "./figura_no_inicial.png",
  width = 9.5,
  height = 12.5,
  units = "cm",
  device = "png",
  dpi = 200,
  bg = "white")


#+ figura, echo=FALSE, eval=TRUE, warning=FALSE, message= FALSE, out.height = '75%',  fig.align='center', fig.pos="H"

knitr::include_graphics("./figura_no_inicial.png")

#' \newpage

#' ##  Anexo

#' ### Sessão R

#+ sessaoR, echo=TRUE, eval=TRUE, warning=FALSE, message= FALSE

sessioninfo::session_info()

#+ render, echo=TRUE, eval=FALSE, warning=FALSE, message= FALSE

rmarkdown::render("no_inicial_rede.R", output_format = "pdf_document")

