
#' Read the model output CCC raw file ("THE" CCC file)
#'
#' @param filename
#' @param stocks A data frame that could have the columns named "stockname" &
#'   "stocknum", other columns can be included (origin, region, etc). If NA,
#'   then this data frame is defined inside the function based on the newmodel
#'   argument below. If NULL (the defualt) then no stock definition is used and
#'   only stock number is exported in the results.
#' @param fisheries A data frame with columns named "fisheryname" & "fishnum"
#'   other columns can be included (origin, region, etc). If NA then this data
#'   frame is defined inside the function based on the newmodel.  If NULL (the
#'   defualt) then no fishery definition is used and only fishery number is
#'   exported in the results.
#' @param newmodel A Boolean. If TRUE, this relies on the new definition of 40
#'   stocks and 48 fisheries. If FALSE, this uses the definition of 30 stocks
#'   and 25 fisheries. If NA (the default) no stock or fishery definition
#'   columns are linked to the data.
#' @param agegroups.n An integer of length one. The number of ages found in row
#'   1 of each data set. Default is 4.
#' @param fisheries.n An integer of length one. The number of fisheries. The
#'   default is NA. If the fisheries argument above (a data frame of the
#'   fisheries) is NA or user defined then fisheries.n can remain NA as the
#'   function will estimate this value from the fisheries definition data frame.
#'   If the fisheries argument is NULL (no fishery definitions needed) then
#'   fisheries.n must be included (likely 25 or 40 fisheries).
#'
#' @description
#'
#' @return A list of five lists. First element is the stock definition data
#'   frame. The second element is the fishery definition data frame. The third
#'   element is a list comprising as many elements as data sets in the CCC file.
#'   A single data set is a stock-year combination. If there are 40 years and 30
#'   stocks, the list will have a length of 40*30=1200. Each CCC element is a
#'   list of 7 elements. The forth element is a data frame ("data.pop"), which
#'   has data for all years and stocks combined. The fifth element is a data
#'   frame ("data.fishery"), which has data for all stocks, years, and fisheries
#'   combined.
#' @export
#'
#' @examples
#' \dontrun{
#' #new stock and fishery:
#' data.ccc <- readTHECCC("2017p.ccc", stocks=NA, fisheries=NA, newmodel = TRUE)
#'
#' #or current/old stock and fishery:
#' data.ccc <- readTHECCC("1702p.ccc", stocks=NA, fisheries=NA, newmodel = FALSE)
#' length(data.ccc)
#' names(data.ccc)
#' length(data.ccc$data.ccc)
#' str(data.ccc$data.ccc[[1]])
#' }
readTHECCC <- function(filename, stocks=NULL, fisheries=NULL, newmodel=c(NA, TRUE, FALSE), agegroups.n=4, fisheries.n=NA){



#create stock definition data frame (if needed)
    if(!is.null(stocks)){
      if(is.na(stocks)) {
        if(!newmodel){
           #stocks based on "old" (current) model:
      stockname <- c("AKS", "NTH", "FRE", "FRL", "RBH", "RBT", "GSQ", "GST", "GSH", "NKF", "PSF", "PSN", "PSY", "NKS", "SKG", "STL", "SNO", "WCH", "URB", "SPR", "BON", "CWF", "LRW", "WSH", "CWS", "SUM", "ORC", "WCN", "LYF", "MCB")
      stockorigin <- c("US", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US", "US")
      stockregion <- c("AK", "BC", "FR", "FR", "WC", "WC", "GS", "GS", "GS", "PS", "PS", "PS", "PS", "PS", "PS", "PS", "PS", "WA", "CR", "CR", "CR", "CR", "CR", "CR", "CR", "CR", "OR", "WA", "CR", "CR")
      stocktypenum <- c(3, 3, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 3, 0, 0, 0, 1, 0, 1, 1, 1, 0, 2, 2, 0, 0, 0, 0, 1)
      stocks <- data.frame(stocknum=1:length(stockname), stockname=stockname, stockorigin=stockorigin, stockregion=stockregion, stocktypenum=stocktypenum, stringsAsFactors = FALSE)

# See Excel 'NewModelStocksandFisheries_Sep2016.xlsx' for the assembly of the stock and fishery arrays following below values for stocktype, taken from the Model *.BSE file, are as follows:
# 0=fall/wild;  1=fall/hatchery; 2=spring/hatchery; 3=spring/wild
  stocktype.def <- data.frame(stocktypenum=0:3, stocktype=c( "fall/wild", "fall/hatchery", "spring/hatchery", "spring/wild"), stringsAsFactors = FALSE)

  stocks <- merge(stocks, stocktype.def)
  stocks <- stocks[order(stocks$stocknum),]
    } else {
    #define stocks based on new model:

      stocks <- matrix( ncol=4, byrow = TRUE, data=c(
		"SSA", "US", "AK", 3,
		"NSA", "US", "AK", 3,
		"YAK", "US", "AK", 3,
		"TBR", "TB", "TB", 3,
		"NBC", "CA", "NC", 3,
		"CBC", "CA", "CC", 0,
		"FS2", "CA", "FR", 0,
		"FS3", "CA", "FR", 3,
		"FSO", "CA", "FR", 0,
		"FSS", "CA","FR", 3,
		"FLN", "CA","FR", 0,
		"FLH", "CA","FR", 1,
		"WVH", "CA","WV", 1,
		"WVN", "CA","WV", 0,
		"UGS", "CA","GS", 0,
		"PPS", "CA","GS", 1,
		"LGS", "CA","GS", 0,
		"MGS", "CA","GS", 1,
		"NKF", "US","PS", 1,
		"PSF", "US","PS", 1,
		"PSN", "US","PS", 0,
		"PSY", "US","PS", 1,
		"NKS", "US","PS", 3,
		"SKG", "US","PS", 0,
		"STL", "US","PS", 0,
		"SNO", "US","PS", 0,
		"WCH", "US","WA", 1,
		"WCN", "US","WA", 0,
		"WSH", "US","CR", 2,
		"CWS", "US","CR", 2,
		"SUM", "US","CR", 0,
		"URB", "US","CR", 0,
		"SPR", "US","CR", 1,
		"BON", "US","CR", 1,
		"CWF", "US","CR", 1,
		"LRW", "US","CR", 0,
		"LYF", "US","CR", 0,
		"MCB", "US","CR", 1,
		"NOC", "US","OR", 0,
		"MOC", "US","OR", 0))
      stocks <- data.frame(stocks, stringsAsFactors = FALSE)
      colnames(stocks) <- c("stockname", "stockorigin", "stockregion", "stocktypenum")
      stocks$stocktypenum <- as.integer(stocks$stocktypenum)
      stocks$stocknum <- 1:nrow(stocks)

      } #END if(!newmodel)

    } else {
       #stocks is user supplied and not NA, use that.
    }#END if(is.na(stocks))

  } else {
    #stocks is NULL so no stock defs will be included (just the index)

  }#END if(!is.null(stocks)){

#create fisheries definition data frame
  if(!is.null(fisheries)){
   if(is.na(fisheries)){
      if(!newmodel){
        #fisheries based on "old" (current) model:
      fisheryname <- c("AlaskaT", "NorthT", "CentrT", "WCVIT", "WA/ORT", "GeoStT", "AlaskaN", "NorthN", "CentrN", "WCVIN", "JDeFN", "PgtNthN", "PgtSthN", "WashCstN", "TermN", "JohnStN", "FraserN", "AlaskaS", "Nor/CenS", "WCVIS", "WashOcnS", "PgtNthS", "PgtSthS", "GeoStS", "TermS")
        fisheryorigin <- c("US", "CA", "CA", "CA", "US", "CA", "US", "CA", "CA", "CA", "CA", "US", "US", "US", "NA", "CA", "CA", "US", "CA", "CA", "US", "US", "US", "CA", "NA")
        fisheryregion <- c("SEAK", "NCBC", "NCBC", "WCVI", "SUS", "GS", "SEAK", "NCBC", "NCBC", "WCVI", "GS", "SUS", "SUS", "SUS", "TA", "GS", "FR", "SEAK", "NCBC", "WCVI", "SUS", "SUS", "SUS", "GS", "TA")
        fisherygear <- c("T", "T", "T", "T", "T", "T", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "S", "S", "S", "S", "S", "S", "S", "S")
        fisheymantype <- c("AABM", "AABM", "ISBM", "AABM", "ISBM", "ISBM", "AABM", "ISBM", "ISBM", "ISBM", "ISBM", "ISBM", "ISBM", "ISBM", "ISBM", "ISBM", "ISBM", "AABM", "AABM", "AABM", "ISBM", "ISBM", "ISBM", "ISBM", "ISBM")
        fisheryloc <- c("P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "P", "T", "P", "P", "P", "P", "P", "P", "P", "P", "P", "T")
        fisheries <- data.frame(fishnum=1:length(fisheryname), fisheryname=fisheryname, fisheryorigin=fisheryorigin, fisheryregion=fisheryregion, fisherygear=fisherygear, fisheymantype=fisheymantype, fisheryloc=fisheryloc, stringsAsFactors = FALSE)

      }else{
        #fisheries based on new model:
    fisheries <- matrix(ncol = 6, byrow=TRUE, data=c(
    "ALASKAT", "US", "SEAK", "T", "AABM", "P",
		"TAKTERMT", "US", "SEAK", "T", "AABM", "T",
		"NORTHT", "CA", "NBC", "T", "AABM", "P",
		"CENTRLT", "CA", "CBC", "T", "ISBM", "P",
		"WCVIT", "CA", "WCVI", "T", "AABM", "P",
		"NFALCONT", "US", "SUS", "T", "ISBM", "P",
		"SFALCONT", "US", "SUS", "T", "ISBM", "P",
		"GEOSTT", "CA", "GS", "T", "ISBM", "P",
		"ALASKAN", "US", "SEAK", "N", "AABM", "P",
		"NORTHN", "CA", "NBC", "N", "ISBM", "P",
		"CENTRLN", "CA", "CBC", "N", "ISBM", "P",
		"WCVIN", "CA", "WCVI", "N", "ISBM", "P",
		"JDEFN", "CA", "GS", "N", "ISBM", "P",
		"PGSDNN", "US", "SUS", "N", "ISBM", "P",
		"PGSDON", "US", "SUS", "N", "ISBM", "P",
		"WASHCSTN", "US", "SUS", "N", "ISBM", "P",
		"TCOLRN", "US", "SUS", "N", "ISBM", "T",
		"TAKTERMN", "US", "SEAK", "N", "AABM", "T",
		"TNORTHFN", "CA", "NBC", "N", "ISBM", "T",
		"TCENTRALFN", "CA", "CBC", "N", "ISBM", "T",
		"TGEOSTFN", "CA", "GS", "N", "ISBM", "T",
		"TFRASFN", "CA", "FR", "N", "ISBM", "T",
		"TPSFN", "US", "SUS", "N", "ISBM", "T",
		"TWACFN", "US", "SUS", "N", "ISBM", "T",
		"JNSTN", "CA", "GS", "N", "ISBM", "P",
		"FRASERN", "CA", "FR", "N", "ISBM", "P",
		"ALASKAS", "US", "SEAK", "S", "AABM", "P",
		"CBCS", "CA", "CBC", "S", "ISBM", "P",
		"NBCAABMS", "CA", "NBC", "S", "AABM", "P",
		"NBCISBMS", "CA", "NBC", "S", "ISBM", "P",
		"WCVIAABMS", "CA", "WCVI", "S", "AABM", "P",
		"WCVIISBMS", "CA", "WCVI", "S", "ISBM", "P",
		"NFALCONS", "US", "SUS", "S", "ISBM", "P",
		"SFALCONS", "US", "SUS", "S", "ISBM", "P",
		"PGSDNS", "US", "SUS", "S", "ISBM", "P",
		"PGSDOS", "US", "SUS", "S", "ISBM", "P",
		"TCANTBRTERMFN", "CA", "TB", "N", "TBR", "T",
		"GEOSTS", "CA", "GS", "S", "ISBM", "P",
		"BCJFS", "CA", "GS", "S", "ISBM", "P",
		"TCOLRS", "US", "SUS", "S", "ISBM", "T",
		"TAKTERMS", "US", "SEAK", "S", "AABM", "T",
		"TNORTHFS", "CA", "NBC", "S", "ISBM", "T",
		"TCENTRALFS", "CA", "CBC", "S", "ISBM", "T",
		"TWCVIFS", "CA", "WCVI", "S", "ISBM", "T",
		"TFRASERFS", "CA", "FR", "S", "ISBM", "T",
		"TGSFS", "CA", "GS", "S", "ISBM", "T",
		"TPSFS", "US", "SUS", "S", "ISBM", "T",
		"TSFFS", "US", "SUS", "S", "ISBM", "T"))
    fisheries <- data.frame(fisheries, stringsAsFactors = FALSE)
    colnames(fisheries) <- c("fisheryname", "fisheryorigin", "fisheryregion", "fisherygear", "fisherymantype", "fisheryloc")
    fisheries$fishnum <- 1:nrow(fisheries)

      }#END if(!newmodel)
   }else{
     #fisheries argument is not NA. definiton data frame supplied as argument

   }#END if(is.na(fisheries))
  }else{
    #fisheries is NULL. No fisheries definition data frame will be created.

  }#END if(!is.null(fisheries))

#_________
# the end of stock and fishery def

  #if the user includes a fishery definition data frame in the argument (or it
  #got created). If neither is true then fisheries.n must have been included as
  #an argument:
  if(!is.null(fisheries)) fisheries.n <- nrow(fisheries)

  dat.tmp <- readLines(filename)
  dat.rows <- length(dat.tmp)
  defining.row <- seq(1, dat.rows, by=agegroups.n*fisheries.n+1)

  table.row.start <- defining.row+1
  table.row.end <- table.row.start+fisheries.n*agegroups.n-1


  rowindex.df <- data.frame(defining.row=defining.row, table.row.start=table.row.start, table.row.end=table.row.end)

  dat.list <- apply(rowindex.df,1, FUN = function(x){

    first.row <- unlist(strsplit(trimws(dat.tmp[x["defining.row"]]), split = "  *"))
    #this allows each value to be converted based on its unique trait:
    first.row <- lapply(first.row, type.convert)
    year <- first.row[[1]]
    stocknum <- first.row[[2]]
    #stock <- stocks[stocks$stocknum==stocknum,]
    var.list <- lapply(c(3,4,11,12),FUN=function(start.ind){
      unlist(first.row[seq(start.ind, by=2, length.out = 4)])
    })

    tc <- textConnection(dat.tmp[x["table.row.start"]:x["table.row.end"]])
    #data.df <- read.table(tc, fill=TRUE)
    data.df <- read.fwf(tc,widths = c(2,2,9,10,10,10))

    #not all data frames have the final two: "CNRLEG", "CNRSUB"
    colnames(data.df) <- tolower(c("FISHNUM", "AGE", "CATCH", "SHAKER", "CNRLEG", "CNRSUB"))[1:ncol(data.df)]
    close(tc)
    #data.df <- merge(data.df, fisheries)

   return(list(year=year, stocknum=stocknum, aeq=var.list[[1]], coh=var.list[[2]], terminalrun=var.list[[3]], escapement=var.list[[4]], data.df=data.df))

  })#END apply

  data.pop <- lapply(dat.list, function(x){
  	data.frame(year=x$year, stocknum=x$stocknum, age=unique(x$data.df$age), aeq=x$aeq, coh=x$coh, terminalrun=x$terminalrun, escapement=x$escapement, stringsAsFactors = FALSE)
  })
  data.pop <- do.call("rbind", data.pop)
  data.pop <- merge(data.pop, stocks, all.x=TRUE)
  data.pop <- data.pop[order(data.pop$stocknum, data.pop$year, data.pop$age),]

  data.fishery <- lapply(dat.list, function(x){
  	data.frame(year=x$year, stocknum=x$stocknum, x$data.df, stringsAsFactors = FALSE)
  })
  data.fishery <- do.call("rbind", data.fishery)
  data.fishery <- merge(data.fishery, fisheries, all.x=TRUE)
  data.fishery <- data.fishery[order(data.fishery$stocknum, data.fishery$year, data.fishery$fishnum, data.fishery$age),]

  return(list(stocks=stocks, fisheries=fisheries, data.ccc=dat.list, data.pop=data.pop, data.fishery=data.fishery))

}#END readTHECCC

