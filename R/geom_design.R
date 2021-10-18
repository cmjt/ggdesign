
GeomDesign <-  ggplot2::ggproto("GeomDesign", ggplot2::Geom,
                                required_aes = c("id"),
                                default_aes = aes(shape = 22, size = 5,
                                                  fill = "black", colour = NA),
                                setup_data = function(data, params) {
                                    transform(
                                        data,
                                        x = data$id,
                                        y = data$block
                                    )
                                },
                                draw_key = draw_key_point,
                                draw_panel = function(data, panel_params, coord) {
                                    ## Transform the data first
                                    coords <- coord$transform(data, panel_params)
                                    str(coords)
                                    if(!"block" %in% names(coords)) coords$block <- rep(1, length(coords$id))
                                    ## Construct a grid grob
                                    design_sketch(
                                        x =  coords$x,
                                        y =  coords$y,
                                        pch = coords$shape
                                    )   
                                }
                                )


design_sketch <- function(x, y, pch){
    
    grid::pointsGrob(
              default.units = "native",
              x =  x,
              y =  y,
              pch = pch)
    
}


geom_design <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE,
                        show.legend = TRUE, inherit.aes = TRUE, ...) {
    lst <-  list( ggplot2::layer(
                               geom = GeomDesign, mapping = mapping,
                               data = data, stat = stat, position = position,
                               show.legend = show.legend, inherit.aes = inherit.aes,
                               params = list(na.rm = na.rm, ...)
                           ),
                 ggplot2::theme_void()

                 )
    lst
}
