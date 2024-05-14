library(dplyr)
library(sf)
library(lwgeom)

# limite exemplo
# xmin      ymin      xmax      ymax
# -52.45996 -20.05684 -51.66483 -19.57296

pts <- st_sf(pt = 1:2, geom = st_sfc(st_point(c(-52.45996, -20.05684)), st_point(c(-51.66483, -19.57296))), crs = 4326)
pol <- pts %>% st_bbox() %>% st_as_sfc(., crs = 4326)
mask <- st_as_text(st_geometry(pol))

bc_250 <- "/media/vinicio/Vinicio Lima/Arauco_MS/BASE_MS/IBGE/BC_250_2023/bc250_2023_11_23.gpkg"
sf::st_layers(bc_250)

hidro_ms <- st_read(bc_250, layer = "hid_trecho_drenagem_l",
                 wkt_filter = mask)


start <- lwgeom::st_startpoint(hidro_ms) %>% st_difference() %>% st_as_sf()
end <- lwgeom::st_endpoint(hidro_ms) %>% st_difference() %>% st_as_sf()


end_pol <- st_buffer(end, 100)
# mapview::mapview(end_pol)
start_pol <- st_buffer(start, 100)
erase <- st_erase(st_union(st_combine(start)), st_union(end_pol))

# not_covered_by  = function(x, y) !st_covered_by(x, y)
# not_cover <- st_filter(  end, start_pol,     .predicate= not_covered_by) %>%  st_centroid()

