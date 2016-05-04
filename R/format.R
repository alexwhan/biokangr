#' Read a blitz output tsv file
#'
#' @param path The path to a blitz tsv
#' @return A data_frame, where blockSizes, qStarts and tStarts are stored as lists
#' @importFrom magrittr %>%
#' @export
#'
read_blitz_tsv <- function(path) {
  #Deal with header format
  #First read header only
  df_names <- readr::read_tsv(path, skip = 2, n_max = 1)

  #Header is in two rows for some names
  #Get those that have two rows and paste together
  names(df_names)[!is.na(df_names[1,])] <- paste(names(df_names)[!is.na(df_names[1,])], df_names[1,!is.na(df_names[1,])], sep = "_")

  #Remove odd characters
  names(df_names) <- sub("'| |\\.|-", "", names(df_names))

  #Read file proper, using names from previous
  df <- readr::read_tsv(path, skip = 5, col_names = names(df_names),
                        col_types = readr::cols(blockSizes = "c", qStarts = "c", tStarts = "c")) %>%
    dplyr::tbl_df()

  df <- within(df, {
    blockSizes <- make_num_list(blockSizes)
    qStarts <- make_num_list(qStarts)
    tStarts <- make_num_list(tStarts)
  })

  return(df)
}

#' Takes a row from a data_frame output from read_blitz_tsv, converts into a GenomicRanges object
#'
#' @param row A row from a data_frame, output from read_blitz_tsv
#' @return A GenomicRanges object
#'
make_gr_rows <- function(row) {
  seqnames <- row$Q_name
  gr <- GenomicRanges::GRanges(seqnames = seqnames,
                  ranges = IRanges::IRanges(start = row$tStarts[[1]],
                                   width = row$blockSizes[[1]]),
                  strand = rep(row$strand, row$block_count))

  return(gr)
}

#' Takes a row form a data_frame output from read_blitz_tsv and expands list columns into rows
#'
#' @param row A row from a data_frame, output from read_blitz_tsv
#' @return A data_frame
#'
make_block_rows <- function(row) {
  seqnames <- paste(row$Q_name, 1:row$block_count, sep = "_")
  readnames <- rep(row$Q_name, row$block_count)
  df <- dplyr::data_frame(seqname = seqnames,
                     readname = readnames,
                     qStart = row$qStarts,
                     qEnd = row$qStarts + row$blockSizes,
                     tStart = row$tStarts,
                     tEnd = row$tStarts + row$blockSizes,
                     strand = row$strand)

  return(df)
}

#' Takes data_frame output from read_blitz_tsv and returns a GRanges object
#' @param df data_frame output from read_blitz_tsv
#' @return A GRanges objects
#' @importFrom magrittr %>%
#' @export
#'
make_block_df <- function(df) {
  gr <- df %>%
    dplyr::rowwise() %>%
    dplyr::do(seq = make_block_rows(.))

  gr <- dplyr::bind_rows(gr$seq) %>%
    dplyr::group_by(readname) %>%
    dplyr::mutate(minStart = min(tStart)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(minStart, readname, tStart)

  gr$readname_sort <- factor(gr$readname, levels = unique(gr$readname))

  return(gr)
}
