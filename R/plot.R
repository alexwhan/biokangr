#' Makes a ggplot2 figure of alignment blocks arranged by reads along target
#' @param df A data_frame output from make_block_df
#' @return A ggplot2 object
#' @import ggplot2
#' @export
#'
block_alignments <- function(df) {
  p <- ggplot2::ggplot(df, aes(tStart, readname_sort)) +
    ggplot2::geom_segment(aes(xend = tEnd, yend = readname_sort, group = seqname, colour = strand)) +
    ggplot2::theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())

  return(p)
}
