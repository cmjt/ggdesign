#' Design
#'
#' @details Used to plot illustrative figures of experimental design structures
#'
#' @inheritParams ggplot
#' @param type specifies the format of the units to plot; one of
#' ["square"], ["hex"], or ["hex_tri"]. By default
#' this is ["square"]
#' @param seg ignored unless [type = "hex_tri"]. Specifies the
#' `side` of the hex unit to fill; one of [1] or [2] (by default = [1])
#' @export
#' @examples
#' crd <- data.frame(plots = 1:6, trt = c("C", "B", "B", "A", "A", "C"))
#'
#' ggplot(crd, aes(plots = plots, fill = trt)) + geom_design()
#'
#' ggplot(crd, aes(plots = plots, fill = trt)) +
#' geom_design(type = "hex") + scale_fill_brewer(palette = "Dark2")

geom_design <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE,
                        show.legend = TRUE, inherit.aes = TRUE, ...) {
    lst <-  list( ggplot2::layer(
        geom = GeomDesign, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    ),
    ggplot2::theme_void() +
        theme(legend.position = "bottom",
              legend.key.size = unit(1.5, 'cm'),
              legend.title = element_blank(),
              legend.text = element_text(size = 15))

    )
    lst
}

#' @format NULL
#' @usage NULL
GeomDesign <-  ggplot2::ggproto("GeomDesign", ggplot2::Geom,
                                extra_params = c("na.rm", "type", "seg"),
                                required_aes = c("plots", "fill"),
                                default_aes = ggplot2::aes(shape = 22, size = 6,
                                                           colour = NA,
                                                           alpha = 1),
                                setup_data = function(data, params) {
                                    if(!"block" %in% names(data)) {
                                        data$block <- rep(1, length(data$plots))
                                        transform(
                                            data,
                                            x = 1:length(data$plots),
                                            y = data$block
                                        )
                                    }else{
                                        ntr <- length(unique(data$block))
                                        mat <- matrix(data$plots, byrow = TRUE, nrow = ntr)
                                        transform(
                                            data,
                                            x = rep(1:ncol(mat), times = ntr),
                                            y = rep(nrow(mat):1, each = ncol(mat)),
                                            xmin = 0,xmax = ncol(mat) + 1,
                                            ymin = 0, ymax = nrow(mat) + 1
                                        )
                                    }
                                },
                                draw_key = ggplot2::draw_key_polygon,
                                draw_panel = function(data, panel_params, coord,
                                                      na.rm = FALSE, type = "square", seg = 1) {
                                    ## Transform the data first
                                    coords <- coord$transform(data, panel_params)
                                    ## Construct a grid grob
                                    design_sketch(
                                        type = type,
                                        x =  coords$x,
                                        y =  coords$y,
                                        pch = coords$shape,
                                        seg = seg,
                                        gp =  grid::gpar(cex = coords$size,
                                                         fill = scales::alpha(coords$fill, coords$alpha),
                                                         col = scales::alpha(coords$colour,coords$alpha),
                                                         lwd = coords$size)
                                    )
                                }
                                )

#' Different plotting designs
design_sketch <- function(x, y, pch, gp, seg, type){
    if(type[1] == "square"){
        grid::pointsGrob(
                  default.units = "native",
                  x =  x,
                  y =  y,
                  pch = pch,
                  gp  = gp)
    }else{
        if(type[1] == "hex"){
            dx <- ggplot2::resolution(x, FALSE)
            dy <- ggplot2::resolution(y, FALSE) / sqrt(3) / 2 * 1.15
            n <- length(x)
            hex_c <- hexbin::hexcoords(dx, dy, n = 1)
            hex_x <- hex_c$x
            hex_y <- hex_c$y
            grid::polygonGrob(
                      x = rep.int(hex_x, n) * (gp$cex/10) / sqrt(2) + rep(x, each = 6),
                      y = rep.int(hex_y, n) * gp$cex/10 + rep(y, each = 6),
                      default.units = "native",
                      id.lengths = rep(6, n),
                      gp = gp)
        }else{
            if(type[1] == "hex_tri"){
                dx <- ggplot2::resolution(x, FALSE)
                dy <- ggplot2::resolution(y, FALSE) / sqrt(3) / 2 * 1.15
                n <- length(x)
                hex_c <- hexbin::hexcoords(dx, dy, n = 1)
                hex_x <- hex_c$x
                hex_y <- hex_c$y
                gp$cex <- rep(gp$cex, each = 18)
                gp$cex <- matrix(gp$cex, nrow = 3)
                segs <- c(1, 1, 1, 2, 2, 2)
                mask <- ifelse(seg == 1, 2, 1)
                gp$cex[, segs %in% mask] <- 0
                tri <- get_tri(hex_x, hex_y, 0, 0)
                cx <- rep.int(tri$x, n) * (gp$cex/10) / sqrt(2) + rep(x, each = 18)
                cy <- rep.int(tri$y, n) * gp$cex/10 + rep(y, each = 18)
                grid::polygonGrob(
                          x = cx,
                          y = cy,
                          default.units = "native",
                          id.lengths = rep(18, n),
                          gp = gp)
            }
        }
    }
}
#' Function to "get" triangle vertices from vectors of hex coords
#' @format NULL
#' @usage NULL
get_tri <- function(x, y, cenx, ceny) {
    tx <- c(rep(x, times = c(1, 2, 2, 2, 2, 2)), x[1])
    tx <- matrix(tx, nrow = 2)
    tx <- rbind(tx, cenx)
    ty <- c(rep(y, times = c(1, 2, 2, 2, 2, 2)), y[1])
    ty <- matrix(ty, nrow = 2)
    ty <- rbind(ty, ceny)
    return(data.frame(x = c(tx), y = c(ty)))
}

#' @import ggplot2
#' @importFrom hexbin hexcoords
#' @importFrom grid polygonGrob pointsGrob
NULL
