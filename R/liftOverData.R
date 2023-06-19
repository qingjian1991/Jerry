#' liftOverData
#'
#' The liftOver facilities developed in conjunction with the UCSC browser track infrastructure are available for transforming data in GRanges format
#' @param data: data frame: Three columns are required, including Chromosome, Start and End
#' @param chainFiles: liftover files. see details.
#' @param chrVersion: chr version of the output.
#'
#' @examples
#' #load examples data.
#'
#' data("ASCAT_hg38_examples")
#'
#' hg38ToHg19.over.chain = system.file(package="Jerry", "extdata", "hg38ToHg19.over.chain")
#'
#' # Three columns, including Chromosome, Start and End, are required to liftover.
#' head(stad_ascat)
#'
#' stad_ascat_hg19 = liftOverData(data = stad_ascat,
#'                               chainFiles = hg38ToHg19.over.chain,
#'                               chrVersion = "hg19")
#' head(stad_ascat_hg19)
#'
#' @details
#' chainFiles can be downloaded from the following urls:
#' @details - hg19tohg38: http://hgdownload.soe.ucsc.edu/goldenPath/hg19/liftOver/hg19ToHg38.over.chain.gz
#' @details - hg38tohg19: http://hgdownload.soe.ucsc.edu/goldenPath/hg38/liftOver/hg38ToHg19.over.chain.gz
#'
#' @export


liftOverData =function(data, chainFiles = "hg38ToHg19.over.chain",  chrVersion = "hg19"){

  if( !all(c("Chromosome","Start","End") %in% colnames(data))  ){
     stop("check columns in data": colnames(data))
  }

  gr_muts = GenomicRanges::GRanges(data$Chromosome, IRanges::IRanges(data$Start,data$End))
  S4Vectors::mcols(gr_muts) = data[, !colnames(data) %in% c("Chromosome","Start","End")]

  chain = rtracklayer::import.chain(chainFiles)
  seqlevelsStyle(gr_muts) = "UCSC"

  gr_muts_ch = unlist( rtracklayer::liftOver(gr_muts, chain) )

  GenomeInfoDb::genome(gr_muts)=chrVersion


  gr_muts_ch_data=data.frame(gr_muts_ch) %>%
    mutate(chrVersion = chrVersion) %>%
    mutate(strand = NULL) %>%
    dplyr::rename(
      Chromosome = seqnames,
      Start= start,
      End = end
    )
  return(
    list(gr_muts_ch =gr_muts_ch,
         gr_muts_ch_data = gr_muts_ch_data
         )
    )

}




