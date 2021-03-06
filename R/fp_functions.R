#' @title (FP) Calculate fisheries policy (FP) scaler.
#'
#' @param dat.er.long Data frame returned by \code{\link{read_stkfile}}.
#' @param dat.mdl.long Data frame returned by \code{\link{read_mdl}}.
#' @param dat.spfi Data frame in long format with same structure as the element
#'   \code{S.ty}, which is output of \code{\link{calc_SPFI}}.
#' @param allowMissingStocks A Boolean. The stocks available in the .stk file
#'   should have a matching MDL file. The function will always check for missing
#'   stocks and throw a warning if MDL stocks are missing. If this is set to
#'   FALSE the calculation stops when stocks are missing. If this is set to
#'   TRUE, the function proceeds without the missing stocks. Default is FALSE.
#'
#' @return A list of two data frames. First data frame comprising the FP values.
#'   Second data frame consists of the intermediate calculated fields.
#' @export
#'
#' @examples
#' \dontrun{
#' dat.fp <- calc_fp(dat.er.long = dat.er.long.sub,
#'   dat.mdl.long = dat.mdl.long.sub, dat.spfi = dat.spfi.long)
#'
#' }
calc_fp <- function(dat.er.long, dat.mdl.long, dat.spfi, allowMissingStocks=FALSE){

  if(any(!unique(dat.er.long$stock.short) %in% unique(dat.mdl.long$stock.short))){
    str1 <- "There are stocks in the .stk file that lack a matching mdl file. The missing stocks are:"
    str2 <- unique(dat.er.long$stock.short)[!unique(dat.er.long$stock.short) %in% unique(dat.mdl.long$stock.short)]

    cat(stringi::stri_wrap(c(str1,"\n\n",str2, "\n\n", "The missing MDL stock files should be added.\n")), sep="\n")
  }#END if

  if(allowMissingStocks & any(!unique(dat.er.long$stock.short) %in% unique(dat.mdl.long$stock.short))){
    cat(stringi::stri_wrap("Argument 'allowMissingStocks' is set to TRUE, so proceeding with calculations."), sep="\n")
  } else if(isFALSE(allowMissingStocks) & any(!unique(dat.er.long$stock.short) %in% unique(dat.mdl.long$stock.short))) {
    #this terminates the function without results:
    return("Function terminated without results.")}


  dat.for.calc <- merge(dat.mdl.long, dat.er.long, by=c("stock.short", "baseperiodfishery.name", "age"))

  dat.mdl.long.sum <- aggregate(r~age+stock.short, FUN = sum, data=dat.mdl.long)
  colnames(dat.mdl.long.sum)[colnames(dat.mdl.long.sum)=="r"] <- "r.sum"

  dat.for.calc <- merge(dat.for.calc, dat.mdl.long.sum, by=c("stock.short", "age"))
  dat.for.calc$sbper <- dat.for.calc$r * dat.for.calc$bper/dat.for.calc$r.sum

  dat.spfi <- dat.spfi[dat.spfi$fishery.index %in% unique(dat.for.calc$fishery.index), c("fishery.index", "return.year", "S.ty")]

  dat.for.calc <- merge(dat.spfi, dat.for.calc, by="fishery.index", all.x = TRUE)
  dat.for.calc$ser <- dat.for.calc$sbper * dat.for.calc$S.ty

  ser.sum <- aggregate(ser~stock.short+age+return.year, data = dat.for.calc, sum)
  colnames(ser.sum)[colnames(ser.sum)=="ser"] <- "ser.sum"

  dat.fp <- merge(ser.sum, dat.er.long, by=c("stock.short", "age"))
  dat.fp$fp <- dat.fp$ser.sum/dat.fp$bper
  return(list(dat.fp=dat.fp, dat.for.calc=dat.for.calc))

}#END calc_fp




#' @title Plot series found in *.fpa files.
#'
#' @param dat.fpa A data frame. Output from \code{\link{read_FPA}}.
#' @param stock.var A character vector of length one. The name of the variable
#'   representing stock names.
#' @param age.var  A character vector of length one. The name of the variable
#'   representing age data (i.e. the age index or true age).
#' @param plot.true.age Logical (TRUE/FALSE). Plot true age data. Default is
#'   TRUE. If TRUE, then \code{age.var} should likely be "age".
#' @param aabm A character vector of length one. The AABM name. This is used in
#'   the ouput filename.
#' @param fpa.filename A character vector of length one. The FPA name. This is
#'   used in the ouput filename.
#'
#' @return A single png file of lattice plot showing the FPA series by stock and
#'   age or age index.
#' @export
#'
#' @examples
#' \dontrun{
#' plotFPAseries(dat.fpa = dat.fpa, stock.var = "stockname_long", age.var = "age",
#'  plot.true.age = TRUE, aabm = aabm, fpa.filename = fpa.filename)
#' }
plotFPAseries <- function(dat.fpa, stock.var= "stockname_long" , age.var= "age", plot.true.age=TRUE, aabm, fpa.filename){

	if(plot.true.age){
		df.unique <- expand.grid(age=unique(dat.fpa$age), stockname_long=unique(dat.fpa$stockname_long))
		dat.fpa.expanded <- merge(df.unique, dat.fpa, by=c("age", "stockname_long"), all.x = TRUE)
		dat.fpa.expanded <- dat.fpa.expanded[order(dat.fpa.expanded$stockname_long, dat.fpa.expanded$age, dat.fpa.expanded$year),]
		dat.fpa <- dat.fpa.expanded
	}

	filename <- paste("HRI_by_stock-age", aabm, fpa.filename, ".png", sep="_")
	png(filename = filename, height = length(unique(dat.fpa$model.stocknumber)), width=8, uni="in", res=600)
	print(lattice::xyplot(value~year|as.factor(dat.fpa[,age.var])+as.factor(dat.fpa[,stock.var]), data=dat.fpa, type='b', as.table=TRUE,
							 par.strip.text=list(cex=0.6),
							 xlab = "Year", ylab = "HRI",
							 layout=c(length(unique(dat.fpa[,age.var])),length(unique(dat.fpa[,stock.var]))), scales=list(alternating=FALSE),
							 panel=function(x,y){
							 	lattice::panel.abline(v=seq(1900,2100, by=5), col='grey')
							 	lattice::panel.points(x,y, type='b', col='black', pch=16, cex=0.5)
							 }
	))
	dev.off()
	cat(c("Plot written to:\n", filename))

}#END plotFPAseries



#' @title (FP) Plot time series of FP values.
#'
#' @param dat.fp A data frame. Output from \code{\link{calc_fp}}.
#' @param savepng A Boolean. Option to send output to a png file instead of plot
#'   window. Default is FALSE.
#'
#' @return A lattice plot of the FP series, by stock and age. If \code{savepng}
#'   is TRUE then each stock is sent to an individual png file and the stock
#'   name is included in the file name.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_fpseries(dat.fp = dat.fp, savepng = TRUE)
#' }
plot_fpseries <- function(dat.fp, savepng=FALSE, filename=NA){
  dat.fp <- dat.fp[order(dat.fp$return.year),]
  for(stock in unique(dat.fp$stock.short)){
    dat.fp.sub <- dat.fp[dat.fp$stock.short==stock,]

    xyplot.tmp <- lattice::xyplot(fp~return.year|as.factor(age), data=dat.fp.sub, as.table=TRUE,
     panel=function(x,y, subscripts,...){
      lattice::panel.abline(v=seq(1900,2100, by=5), col='grey')
     lattice::panel.xyplot(x, y, type='b', pch=16, cex=.5)
     if(any(colnames(dat.fp) %in% "fp.xls")){lattice::panel.lines(x,dat.fp$fp.xls[subscripts],type='b', pch=1,cex=0.75, col='black')}
           })

    if(savepng){
      filename <- paste("fp_validate", stock, ".png", sep = "_")
      png(filename = filename, width = 8, hei=8, units = "in", res = 600)
      print(xyplot.tmp)
      dev.off()
    }else{
      print(xyplot.tmp)
    }
    }#for(stock
}#END plot_fpseries



#' Read BSE file
#'
#' @param filename
#'
#' @return A list comprising multiple elements, including unique representations of the tables in the .BSE file. The final element is a data frame of the stocks translation table.
#' @export
#'
#' @examples
#' \dontrun{
#' dat.bse <- read_BSE("2017BPC_PII_V1.22.BSE")
#' }
read_BSE <- function(filename){

	dat.tmp <- readLines(filename)
	dat.meta <- as.data.frame(t(as.integer(dat.tmp[1:5])))
	colnames(dat.meta) <- c("stocks.n", NA, "fishery.n", "year.start", NA)

	binarytable.rows <- dat.meta[1,1]+1

	fishery.startrow <- 6
	fishery.names <- dat.tmp[seq(fishery.startrow, length.out = dat.meta$fishery.n)]

	dat1 <- read.table(filename, skip = fishery.startrow-1+dat.meta$fishery.n, nrows = dat.meta$fishery.n)
	dat1 <- data.frame(fishery.names, dat1, stringsAsFactors = FALSE)

	dat2 <- read.table(filename, skip = fishery.startrow-1+2*dat.meta$fishery.n, nrows = 1)

	dat3 <- read.table(filename, skip = fishery.startrow-1+2*dat.meta$fishery.n+1, nrows = binarytable.rows )
	stocks.df <- read.csv(filename, skip=fishery.startrow-1+2*dat.meta$fishery.n+1+binarytable.rows, stringsAsFactors = FALSE, header = FALSE)
	data.parsed <- strsplit(trimws(stocks.df[,2]), "\\s+")
	data.parsed <- as.data.frame(do.call("rbind", data.parsed), stringsAsFactors = FALSE)
	data.parsed <- type.convert(data.parsed)
	stocks.df <- data.frame(stocks.df[,1], data.parsed, stocks.df[,3:4], stringsAsFactors = FALSE)
	colnames(stocks.df)[c(1,9)] <- c("stockname", "stock.acronym")
	stocks.df[,c("stockname", "stock.acronym")] <- apply(stocks.df[,c("stockname", "stock.acronym")], 2, trimws)

	return(list(metadata=dat.meta, fishery.names=fishery.names, table1=dat1, table2=dat2, table3=dat3, stocks.df=stocks.df))



}#END read_BSE



#' @title Read FPA files.
#'
#' @param filenames A character vector. The names of *.fpa files for importing.
#'
#' @description This reads in one or more fpa files and exports the data in two
#'   formats. The first is similar to the structure found in the fpa file (wide
#'   format), the second is in long format, which is more useful for analysis in
#'   R.
#'
#' @return A list, the same length as the number of fpa files in
#'   \code{filenames}. Each list element is also a list and comprises of four
#'   elements: the filename, metadata (information in the first four rows of the
#'   file), and two data frames, the data in wide format similar to that found
#'   in the fpa file, and data in long format. The rows are defined by two row
#'   variables. The variable \code{model.stocknumber} is the value found in the first
#'   column of the fpa file (after row 4, the year row). \code{age.index} is
#'   a sequence of 1 to 4 indicating the subrow location below each "main" row.
#' @export
#'
#' @examples
#' \dontrun{
#' filenames <- list.files(pattern = "*\\.fpa")
#' dat.out <- read_FPA(filenames)
#' }
read_FPA <- function(filepaths){
  dat.out <- lapply(filepaths, FUN=function(filepath){

  	slash.ind <- max(unlist(gregexpr(pattern = "/", text = filepath)))
  	filename <- substr(x = filepath, start = slash.ind+1, stop = nchar(filepath))

    dat.tmp <- readLines(filepath)
    meta.tmp <- trimws( unlist(strsplit(dat.tmp[1], ",")))
    meta01 <- as.integer(meta.tmp[1])
    meta02 <- as.integer(meta.tmp[2])
    meta03 <- meta.tmp[3]
    aabm <- tolower(substr(meta03, 1, regexpr(" ", meta03)-1))
    year.start <- as.integer(dat.tmp[2])
    year.end <- as.integer(dat.tmp[3])
    year.series <- dat.tmp[4]

    metadata <- list(meta01=meta01, meta02=meta02, meta03=meta03, aabm=aabm, year.start=year.start, year.end=year.end)

    #exclude first cell as it defines length of the series:
    year.series <- unlist(strsplit(year.series, "\t"))[-1]
    year.series <- as.integer(year.series)
    year.series <- ifelse(year.series>=79, year.series+1900, year.series+2000)

    row.ind <- seq(5,length(dat.tmp), by=4)
    model.stocknumber <- dat.tmp[row.ind]
    model.stocknumber <- strsplit(model.stocknumber, "\t")
    model.stocknumber <- as.integer(unlist(lapply(model.stocknumber, FUN=function(x) x[1])))
    df.tmp <- expand.grid( age.index=1:4, model.stocknumber=model.stocknumber)
    df.tmp <- df.tmp[,c("model.stocknumber", "age.index")]

    mat.tmp <- sapply(dat.tmp[5:length(dat.tmp)], FUN = function(x) as.numeric(unlist(strsplit(x, "\t"))) )
    mat.tmp <- mat.tmp[-1,]
    mat.tmp <- t(mat.tmp)
    row.names(mat.tmp) <- NULL
    dat.wide <- data.frame(df.tmp, mat.tmp)
    colnames(dat.wide)[3:ncol(dat.wide)] <- paste0("year", year.series)

    dat.long <- reshape(dat.wide, dir="long", varying = list(3:ncol(dat.wide)), v.names = "value")
    dat.long$year <- year.series[dat.long$time]
    dat.long <- dat.long[,c("model.stocknumber", "age.index", "year", "value")]
    dat.long <- dat.long[order(dat.long$model.stocknumber, dat.long$age.index, dat.long$year),]

    return(list(filepath=filepath, filename=filename, metadata=metadata, dat.wide=dat.wide, dat.long=dat.long))

  })

  names(dat.out) <- unlist(lapply(dat.out, "[[", "filename"))
  return(dat.out)

}#END read_FPA


#' @title Read in stk file of base period exploitation rates.
#'
#' @param filename A character vector of length 1. The stk file of base period
#'   exploitation rates.
#' @param baseperiodfishery.names A character vector. Likely based on the file
#'   "48FisheryName.txt".
#'
#' @return A list comprising two elements. The first, named \code{dat.meta.long}
#'   is a data frame of data other than the ER values. This includes the initial
#'   cohort abundance (\code{initcohortabun}), the maturation rates
#'   (\code{matrates}), and the AEQ factor (\code{aeqfactor}). The second list
#'   element is \code{dat.er.long}, which is a data frame of the ER data.
#' @export
#'
#' @examples
#' \dontrun{
#' baseperiodfishery.names <- readLines(
#'    "../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/48FisheryName.txt")
#' dat.stk <- read_stkfile(
#'   "../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/2015BPC_PII_V1.5.STK",
#'    baseperiodfishery.names)
#' }
read_stkfile <- function(filename, baseperiodfishery.names){
  filename <- filename[1]
  dat.tmp <- readLines(filename)

  stock.short.ind <- which(nchar(dat.tmp) ==3)

  stocks <- dat.tmp[stock.short.ind]

  #first line is initial cohort abundance
  #second line is maturation rates
  #third line is the AEQ factor
  dat.meta <- dat.tmp[sort(rep(stock.short.ind, 3))+1:3]
  dat.meta <- do.call("rbind", strsplit(dat.meta, split = "  "))
  dat.meta <- as.data.frame(apply(dat.meta, 2, as.numeric))
  colnames(dat.meta) <- paste0("age",1:ncol(dat.meta))
  dat.meta$data.type <- c("initcohortabun", "matrates", "aeqfactor")
  dat.meta$stock.short <- rep(stocks, rep(3, length(stocks)))

  dat.meta.long <- reshape(dat.meta[,c('stock.short', 'data.type', colnames(dat.meta)[1:(ncol(dat.meta)-2)])], dir='long', varying = list(3:ncol(dat.meta)), timevar = 'age.index', v.names= 'value' , idvar = c('stock.short', 'data.type'))
  rownames(dat.meta.long) <- NULL


  #4th to end of fishery designation is the ER rates by fisheries
  dat.er <- dat.tmp[-(sort(rep(stock.short.ind, 4))+0:3)]
  dat.er <- do.call("rbind", strsplit(dat.er, split = "  "))
  dat.er <- as.data.frame(apply(dat.er, 2, as.numeric))
  colnames(dat.er) <- paste0("age",1:ncol(dat.er))
  dat.er$data.type <- "bper"
  dat.er$baseperiodfishery.name <- baseperiodfishery.names
  dat.er$stock.short <- rep(stocks, rep(length(baseperiodfishery.names), length(stocks)))
  nondata.colnames <- c('baseperiodfishery.name', "stock.short", 'data.type')
  dat.er <- dat.er[,c(nondata.colnames, colnames(dat.er)[!colnames(dat.er) %in% nondata.colnames])]

  dat.er.long <- reshape(dat.er, dir='long', varying = list((length(nondata.colnames)+1):ncol(dat.er)), timevar = 'age.index', v.names= 'value' )

  #this is making the assumption that all starting ages=2 :
  dat.er.long$age <- dat.er.long$age.index+1

  dat.er.long <- subset(dat.er.long, select = -id)

  return(list(dat.meta.long=dat.meta.long, dat.er.long=dat.er.long))
}#END read_stkfile


#' @title Read in mdl files of cwt recovery data.
#'
#' @param filenames A character vector of the mdl file names.
#' @param comma.delimited A Boolean. The new format has comma delimited recovery
#'   data. The old format was not delimeted, but has a 5 column structure.
#'   Default is TRUE (for the new format).
#'
#' @return A list of two elements. The first element, named \code{dat.mdl}, is
#'   also a list. Each element of \code{dat.mdl} is also a list and represents
#'   the data from one mdl file. The second element of the output list, named
#'   \code{dat.mdl.long}, is a data frame of all the combined mdl files. This
#'   latter list element is the preferred source of data for analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' mdl.filenames <- list.files(
#'   path = "../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/56FMDL",
#'   pattern = "MDL$")
#' mdl.filepath <- paste(
#'   "../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/56FMDL",
#'   mdl.filenames, sep="/")
#' dat.mdl <- read_mdl(mdl.filepath)
#' fisheries.needed <- fishery.def[fishery.def$aabm %in% aabm,]
#' dat.mdl.long.sub <- dat.mdl$dat.mdl.long[
#'    dat.mdl$dat.mdl.long$fishery.name %in% fisheries.needed$fishery.name,]
#' }
read_mdl <- function(filenames, fishery.def=NA, comma.delimited=TRUE){

  .read_singlemdl <- function(filename){

     mdl.tmp <- readLines(filename)
     stock.short <- mdl.tmp[1]
     #number of fish tagged
     tagged.n <- as.numeric(mdl.tmp[3])
     #number of fish released
     released.n <- as.numeric(mdl.tmp[4])
     #max age
     age.max <- as.numeric(mdl.tmp[5])
     #number of fisheries
     fisheries.n <- as.numeric(mdl.tmp[6])

     fisheries.names <- as.vector(mdl.tmp[7:(6+fisheries.n)])
     #Last lines of the file contain the number of recoveries by age and fishery
     age.n <- length(mdl.tmp)-6 - fisheries.n

     if(comma.delimited){
       cwt.recoveries <- strsplit(mdl.tmp[(7+fisheries.n):length(mdl.tmp)],split = ",")
       cwt.recoveries <- lapply(cwt.recoveries, function(x) as.numeric(x))
       cwt.recoveries <- as.data.frame(cwt.recoveries)

       #remove final row (esc recoveries), which is same as final column in the mdl file:
       cwt.recoveries.esc <- cwt.recoveries[nrow(cwt.recoveries),]
       cwt.recoveries <- cwt.recoveries[-nrow(cwt.recoveries),]

     } else {
       #this is for the old data format that wasn't comma delimeted, but commited 5 columns per data value
       cwt.recoveries <- sapply(mdl.tmp[(7+fisheries.n):length(mdl.tmp)], FUN = function(x){
       as.integer(substring(x,  c(1,1+(1:(fisheries.n-1))*5), c(5,5+(1:(fisheries.n-1))*5)))})
       cwt.recoveries <- as.data.frame(cwt.recoveries)
     }#END if(comma.delimited)

     ages <- rev(seq(age.max,by=-1, len=age.n))
     colnames(cwt.recoveries) <- paste0("age", ages)

     cwt.recoveries$fishery.name <- fisheries.names
     cwt.recoveries.long <- reshape(cwt.recoveries, dir='long', varying = list(1:age.n), v.names= 'value')
     cwt.recoveries.long$age <- ages[cwt.recoveries.long$time]
     cwt.recoveries.long <- subset(cwt.recoveries.long, select= -c(id, time))

     return(list(filename= filename, stock.short=stock.short, tagged.n=tagged.n, released.n=released.n, age.max=age.max, fisheries.n=fisheries.n, fisheries.names=fisheries.names, cwt.recoveries=cwt.recoveries, cwt.recoveries.long=cwt.recoveries.long))
  }#END .read_singlemdl


  dat.mdl <- lapply(filenames, FUN = .read_singlemdl)

  dat.mdl <- lapply(dat.mdl, FUN = function(x){
    x$cwt.recoveries.long$stock.short <- x$stock.short
    return(x)
  })
  dat.mdl.long <- lapply(dat.mdl, "[[", "cwt.recoveries.long")
  dat.mdl.long <- do.call('rbind', args = dat.mdl.long)
  colnames(dat.mdl.long)[colnames(dat.mdl.long)=="value"] <- "r"

  dat.mdl.long <- merge(dat.mdl.long, fishery.def[,c('fishery.name', "fishery.index", "baseperiodfishery.name")], by="fishery.name")

  return(list(dat.mdl=dat.mdl, dat.mdl.long=dat.mdl.long))

}#END read_mdl


#' @title Read model stock list file.
#'
#' @param filename A character vector of length one.
#'
#' @return A list with two elements. The first element is the meta-data found at
#'   the top of the file. The second element is a data frame comprising the data
#'   of stock names etc.
#' @export
#'
#' @examples
#' \dontrun{
#' model.stocklist <- read_modelstocklist.csv("NewModelStockList.csv")
#' }
read_modelstocklist.csv <- function(filename){
	dat.meta <- read.csv(filename, nrows = 1, header=FALSE, stringsAsFactors = FALSE)
	dat.stock <- read.csv(filename, skip = 2, stringsAsFactors=FALSE)
	return(list(metadata=dat.meta, data=dat.stock))
}#END read_modelstocklist.csv




#' @title Write FPA text file
#'
#' @param fpdat A data frame, equivalent structure to the first element of the
#'   list that is produced by \code{\link{calc_fp}}
#' @param spfi A data frame. Annual estimates of the SPFI. Must include columns named
#'   'return.year' and 'S.y' (the SPFI).
#' @param stocks A data frame with the first column identifying the three letter
#'   stock name and the second column being the stock number. Column names are
#'   not necessary.
#' @param comment A character vector of length one. The comment to be included in line 1
#'   of the FPA file.
#' @param filename A character vector of length one. Required filename for the
#'   output FPA file.
#'
#' @return This function only writes the FPA text file. Nothing is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' dat.fp <- calc_fp(...)
#' write_fpa(dat.fp[[1]], spfi, stocks, comment="wcvi", "fpa_results.fpa", )
#' }
write_fpa <- function(fpdat, spfi, stocks, comment=NA, filename=NA){
  if(is.na(filename)) {stop("filename required")}

  colnames(stocks)[2] <- "stocknumber"
  #add in stock mumber
  fpdat <- merge(fpdat, stocks, by.x = "stock.short", by.y = 1)
  fpdat <- fpdat[,c("stocknumber", "age", "return.year", "fp"),]

  stocks.n <- length(unique(fpdat$stocknumber))

  #add stock 0, which has just spfi values
  stockzero <- expand.grid(stocknumber=0, age=sort(unique(fpdat$age)))
  stockzero <- merge(stockzero, spfi)
  colnames(stockzero)[colnames(stockzero)=="S.y"] <- "fp"
  fpdat <- rbind( stockzero, fpdat)

  #add ages to stocks missing them, fp will be zero
  stockage.df <- expand.grid(stocknumber=unique(fpdat$stocknumber), age=unique(fpdat$age), return.year=unique(fpdat$return.year), fp=0)
  stockage.df$stockageyear <- paste0(stockage.df$stocknumber, stockage.df$age, stockage.df$return.year)
  #exclude what's already available:
  exclude <- paste0(fpdat$stocknumber, fpdat$age, fpdat$return.year)
  stockage.df <- stockage.df[! stockage.df$stockageyear %in% exclude,]

  fpdat <- rbind(fpdat, stockage.df[,colnames(fpdat)])

  #base years are 1
  fpdat$fp[fpdat$return.year<1983] <- 1

  #calculate three year average and apply to next three years
  year.max <- max(fpdat$return.year)

  #calculate future 3 years as mean of final 3
  dat.future <- aggregate(fp~stocknumber+age, data=fpdat[fpdat$return.year>= year.max-2,], sum)
  dat.future$fp <- dat.future$fp/3
  years.future <- seq(year.max+1,length.out = 3)
  future.df <- merge(unique(dat.future[,c("stocknumber", "age")]), years.future)
  colnames(future.df)[colnames(future.df)=="y"] <- "return.year"

  dat.future <- merge(future.df, dat.future)
  fpdat.append <- rbind(fpdat[,colnames(dat.future)], dat.future)

  fpdat.append$fp <- round(fpdat.append$fp,6)
  fpdat.wide <- reshape(fpdat.append, direction="wide", idvar = c("stocknumber", "age"), timevar = "return.year")

  #sort cols:
  fpdat.wide <- fpdat.wide[,c("stocknumber", "age", sort(colnames(fpdat.wide)[-(1:2)]))]

  fpdat.wide <- fpdat.wide[order(fpdat.wide$stocknumber, fpdat.wide$age),]

  #remove 2nd through 4th stock number:
  fpdat.wide$stocknumber[fpdat.wide$age != min(fpdat.wide$age)] <- NA
  #drop the age column:
  fpdat.wide <- subset(fpdat.wide, select = -age)

  #create 2 digit year for column names
  year.short <- substr(sort(unique(fpdat.append$return.year)), 3,4)
  year.start <- min(fpdat.append$return.year)
  year.end <- max(fpdat.append$return.year)

  line1 <- paste(5, 1, comment, sep = ", ")
  line2 <- year.start
  line3 <- year.end
  line4 <- paste(c(paste0(stocks.n,","), year.short), collapse = "\t")
  res <- list(line1, line2, line3, line4, fpdat.wide)
  file.create(filename)
  invisible(
  lapply(res, function(x){
    write.table(x = x,file = filename, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE, sep="\t", na="")
  })
  )

#example header:
#5,    1,    WCVI Troll: (2016-2018 = 2013-2015 average)
#1979
#2018
#36,	79	80	81	82	83	84	85	86	87	88	89	90	91	92	93	94	95	96	97	98	99	00	01	02	03	04	05	06	07	08	09	10	11	12	13	14	15	16	17	18

}#END write_fpa




#' @title (FP) Build, save, and open an R script to help execute FP calculations.
#'
#' @description This creates and opens a script named "fp_script.R". This is a
#'   template for the user to work with when doing FP estimates. This is
#'   intended to help the user understand the work flow and due to file path
#'   differences, is unlikely to work as is. Some object values will need
#'   updating (for example the datapath).
#'
#' @return Opens an R script that includes the template of functions to
#'   calculate FP values.
#' @export
#'
#' @examples
#' writeScriptFP()
writeScriptFP <- function(){

script.str <- c('
####### SETUP #######
rm(list=ls())
###### COMMENTS ########


#### LIBRARIES ####


#### DATA ####

aabm <- "seak"

#these fishery subsets match what is defined in the VB
fishery.df <- data.frame(aabm=c(rep("seak",6), rep("nbc",1), rep("wcvi",3)), fishery.index=c(1:6, 8, 10:12), baseperiodfishery.name=c(rep("ALASKA_T",6), rep("NORTH_T",1), rep("WCVI_T",3)), stringsAsFactors = FALSE)
fishery.def <- merge(fishery.def, fishery.df, by="fishery.index", all.x = TRUE)

#read in spfi file:
datapath <- "C:/Users/folkesm/Documents/Projects/chinook/spfi/data/20180920_2016estimate_stockfilenew/output"
filepath <- list.files(path = datapath, pattern = "\\\\.rds", ignore.case = TRUE, full.names = TRUE)
spfi.output <- readRDS(filepath[5])
dat.spfi.long <- spfi.output$S.ty
View(dat.spfi.long[order(dat.spfi.long$fishery.index, dat.spfi.long$return.year), ])

#this adds zero data for missing years
#wcvi
if(aabm=="wcvi"){
dat.spfi.long.append <- expand.grid(fishery.index=10:12, return.year=c(1996, 1998))
dat.spfi.long <- plyr::rbind.fill(dat.spfi.long, dat.spfi.long.append)
}
dat.spfi.long$S.ty[is.na(dat.spfi.long$S.ty)] <- 0

spfi.output$S.ty <- dat.spfi.long
saveRDS(object = spfi.output, filepath[5])

# or use spfi from AK spreadsheet:
# dat.spfi <- read.csv("../data/AKFP_CLB16b_spfi.csv" )
# dat.spfi <- dat.spfi[,c("YEAR", colnames(dat.spfi[,4:ncol(dat.spfi)]))]
# dat.spfi.long <- reshape(dat.spfi, dir="long", varying = list(2:ncol(dat.spfi)), v.names="S.ty", timevar = "fishery.index")
# colnames(dat.spfi.long)[colnames(dat.spfi.long)=="YEAR"] <- "return.year"
# dat.spfi.long <- subset(dat.spfi.long, select = -id)
# dat.spfi.long$source <- "AKFP_CLB16b.xls"


#read in STK file:
stk.filepath <- list.files(pattern = "\\\\.stk$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
fisheryname.filepath <- list.files(pattern = "FisheryName", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
baseperiodfishery.names <- readLines(fisheryname.filepath)
dat.stk <- read_stkfile(filename = stk.filepath[1], baseperiodfishery.names)

baseperiodfishery.name <- unique(fishery.def$baseperiodfishery.name[fishery.def$aabm %in% aabm])
dat.er.long.sub <- dat.stk$dat.er.long[dat.stk$dat.er.long$baseperiodfishery.name == baseperiodfishery.name,]

# As the ER and mdl data are being merged, the common column named "value" needs
# to be renamed:
colnames(dat.er.long.sub)[colnames(dat.er.long.sub)=="value"] <- "bper"



#read in MDL files
mdl.filepath <- list.files(path="BPC_PII_V1.22_25Sep/56F-adj/56F-adj" , pattern = "MDL$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
dat.mdl <- read_mdl(mdl.filepath)

#View(dat.mdl$dat.mdl.long[order(dat.mdl$dat.mdl.long$stock.short, dat.mdl$dat.mdl.long$age, dat.mdl$dat.mdl.long$fishery.index),])

fisheries.needed <- fishery.def[fishery.def$aabm %in% aabm,]
dat.mdl.long.sub <- dat.mdl$dat.mdl.long[dat.mdl$dat.mdl.long$fishery.name %in% fisheries.needed$fishery.name,]

#### FP CALCULATION ####

dat.fp <- calc_fp(dat.er.long = dat.er.long.sub, dat.mdl.long = dat.mdl.long.sub, dat.spfi = dat.spfi.long, allowMissingStocks = TRUE)
filename <- paste("dat.fp", aabm, "ctctools.rds", sep=".")
saveRDS(object = dat.fp, filename)

stocks <- read_BSE("2017BPC_PII_V1.22.BSE")
stocks <- data.frame(stocks$stocks.df$stock.acronym, 1:nrow(stocks$stocks.df))

filename <- paste0(aabm, format(Sys.Date(), "%Y%m%d"), ".fpa")

write_fpa(fpdat = dat.fp$dat.fp, spfi = spfi.output$S.y[,c("return.year", "S.y"),],  stocks = stocks, filename=filename, comment = aabm)



####### VALIDATION ########

fp.xls <- read.csv("../data/fp.xls.csv", stringsAsFactors = FALSE)

dat.fp <- merge(dat.fp, fp.xls, by=c("stock.short", "age", "return.year"))

#plot_fpseries(dat.fp = dat.fp, savepng = TRUE, filename = "fp_validate_ssa_mfSPFI.png")
plot_fpseries(dat.fp = dat.fp, savepng = TRUE)
')

write(script.str, file="fp_script.R")
file.edit("fp_script.R" )

}#END writeScriptFP
