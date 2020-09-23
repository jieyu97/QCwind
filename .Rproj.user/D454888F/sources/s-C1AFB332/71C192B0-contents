#' @name windrose_plot
#' @title Plotting a windrose
#' @description Plot a windrose of the given wind speed and direction data
#' @param data a dataframe that contains the wind speed and direction data
#' @param speed the wind speed column from data
#' @param direction the wind direction column from data
#' @return a windrose plot
#' @export

# @examples
# to be added
# windrose_plot(wind_df, wind_df$windspeed, wind_df$winddirection)

windrose_plot <- function(data, speed, direction)
{
  # require(ggplot2)
  # require(RColorBrewer)
  spd.breaks <- seq(0,28,2)
  n.spd.seq <- length(spd.breaks)
  spd.labels <- paste(c(spd.breaks[1:n.spd.seq-1]),
                      '-',
                      c(spd.breaks[2:n.spd.seq]))
  data$spd.binned <- cut(x = speed,
                         breaks = spd.breaks,
                         labels = spd.labels,
                         include.lowest = TRUE,
                         ordered_result = TRUE)
  dirres = 30
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  data$dir.binned <- cut(x = direction,
                         breaks = dir.breaks,
                         labels = dir.labels,
                         ordered_result = TRUE)
  n.colors.in.range <- n.spd.seq - 1
  #  spd.colors <- rev(brewer.pal(9, 'YlGnBu'))

  p.windrose <- ggplot(data = subset(data, !is.na(dir.binned)),
                       aes(x = dir.binned,
                           fill = forcats::fct_rev(spd.binned))) +
    geom_bar( ) +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    labs(fill = "Wind Speed (m/s)",
         x = "Wind Direction",
         y = "Count") +
    theme(panel.grid.major = element_line(colour="grey65"))
  return(p.windrose)
}
