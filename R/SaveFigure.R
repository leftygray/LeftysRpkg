#' Wrapper function to save ggplots
#'
#' This is ggsave Wrapper function with defaiults so we don't have to keep
#' entering everything in ggsave.
#'
#' @param folder character string for the folder where figure will be saved
#' @param filename character string specifiying the file name
#' @param figure ggplot object
#' @param format string specifying the file format of the figure. Default ".png"
#' @param width saved plot width. Default is 15 cm
#' @param height saved plot height. Default 10 cm
#' @param units string specifying units for plot dimension. Default centimetres
#' @param ... other ggsave inputs
#'
#' @return none. Saves file in specified directory
#'
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#'
#' @export
#' @importFrom ggplot2 ggsave
#'
library("ggplot2")

SaveFigure <- function(folder, filename, figure,
                       format = ".png", width = 15,
                       height = 10, units = "cm", ...) {

  ggsave(file.path(folder, paste(filename, format, sep = "")),
         plot = figure,
         width = width, height = height, units = units, ...)
}
