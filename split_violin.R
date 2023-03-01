
library(tidyverse)

theme_default <- theme_light()+
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
  )

# Violin plot

## Different Sides

GeomSplitViolin <- ggproto(
  "GeomSplitViolin", GeomViolin, 
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1, "group"]
    newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
    
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function(
    mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
    draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE  
  ) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

mtcars %>% as_tibble() %>% 
  mutate(
    vs = ifelse(mtcars$vs == 1, 'straight', 'V_shaped') %>% factor()
    , am = ifelse(mtcars$am == 1, 'manual', 'automatic') %>% factor()
  ) %>% 
  ggplot(aes(x = am, y = mpg, fill = vs))+
  geom_split_violin()+
  scale_fill_manual(values=c("#597dbf", "#d98b5f"))+
  theme_default
