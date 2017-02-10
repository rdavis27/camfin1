#library(UsingR)
#data(galton)
shinyServer(
    function(input, output) {
        output$myImage <- renderImage({
            input$tabs
            list(src = "plot1.png",
                 contentType = 'image/png',
                 width = 840,
                 height = 840,
                 alt = "plot")
        }, deleteFile = FALSE)
        output$myPlot <- renderPlot({
            calc(input)
            if (nrow(xxnum) > 0){
                plot1(input, xxnum) # xxnum calculated by output$myText
            }
        })
        output$myText <- renderPrint({
            xx <- calc(input)
            print(xx)
        })
        calc <- function(input){
            library(plyr)
            yr <- 16
            end_date <- "June 2016"
            iname       <- toupper(trimws(input$name))
            icity       <- toupper(trimws(input$city))
            istate      <- toupper(trimws(input$state))
            iemployer   <- toupper(trimws(input$employer))
            icmte_nm    <- toupper(trimws(input$cmte_nm))
            iprty       <- toupper(trimws(input$prty))
            icandidate  <- toupper(trimws(input$candidate))
            ioccupation <- toupper(trimws(input$occupation))

            nname       <- nchar(iname)
            ncity       <- nchar(icity)
            nstate      <- nchar(istate)
            nemployer   <- nchar(iemployer)
            ncmte_nm    <- nchar(icmte_nm)
            nprty       <- nchar(iprty)
            ncandidate  <- nchar(icandidate)
            noccupation <- nchar(ioccupation)

            sh <- ""
            if (nname       > 0){
                print(paste("Search NAME for", iname))
                #cat(file=stderr(), "Search NAME for", iname, "\n")
                sh <- paste0(sh,", NAME=",iname)
            }
            if (ncity       > 0){
                print(paste("Search CITY for", icity))
                sh <- paste0(sh,", CITY=",icity)
            }
            if (nstate      > 0){
                print(paste("Search STATE for", istate))
                sh <- paste0(sh,", STATE=",istate)
            }
            if (nemployer   > 0){
                print(paste("Search EMPLOYER for", iemployer))
                sh <- paste0(sh,", EMPLOYER=",iemployer)
            }
            if (ncmte_nm    > 0){
                print(paste("Search CMTE_NM for", icmte_nm))
                sh <- paste0(sh,", CMTE_NM=",icmte_nm)
            }
            if (nprty       > 0){
                print(paste("Search PRTY for", iprty))
                sh <- paste0(sh,", PRTY=",iprty)
            }
            if (ncandidate  > 0){
                print(paste("Search CANDIDATE for", icandidate))
                sh <- paste0(sh,", CANDIDATE=",icandidate)
            }
            if (noccupation > 0){
                print(paste("Search OCCUPATION for", ioccupation))
                sh <- paste0(sh,", OCCUPATION=",ioccupation)
            }
            sh <- paste0(sh," (through ", end_date, ")")
            sh <- sub("^, ","",sh)
            subhdr <<- sh
            cat(file=stderr(), "SEARCH", sh, "\n")
 
            csvfile <- paste("inemcm", yr, ".csv", sep="")
            #csvfile <- paste("incm", yr, ".csv", sep="")
            # Read csv file if necessary
            if (!exists("oo")){
                print(paste("UNZIP", csvfile))
                unzip("inemcm16.zip")
                print(paste("READ", csvfile))
                withProgress(message = "Loading file",
                             detail = "this can take a few minutes to load the first time...", value = 0, {
                                 for (i in 1:10){
                                     incProgress(1/10)
                                     Sys.sleep(0.5)
                                 }
                                 incProgress(1)
                                 oo <<- read.csv(csvfile)
                                 incProgress(1/2)
                             })
            }
            xx <- oo
            if (nname       > 0) xx <- xx[grep(iname,       xx$NAME),]
            if (ncity       > 0) xx <- xx[grep(icity,       xx$CITY),]
            if (nstate      > 0) xx <- xx[grep(istate,      xx$STATE),]
            if (nemployer   > 0) xx <- xx[grep(iemployer,   xx$EMPLOYER),]
            if (ncmte_nm    > 0) xx <- xx[grep(icmte_nm,    xx$CMTE_NM),]
            if (nprty       > 0) xx <- xx[grep(iprty,       xx$PRTY),]
            if (ncandidate  > 0) xx <- xx[grep(icandidate,  xx$CANDIDATE),]
            if (noccupation > 0) xx <- xx[grep(ioccupation, xx$OCCUPATION),]
            
            # Sort by specified sort fields
            if (input$xsortdir == "Ascending"){
                if (input$xsort == "LAST_DATE")      xx <- xx[order(as.Date(xx$LAST_DATE), -xx$TOTAL_CONTRIB),]
                else if (input$xsort == "N_CONTRIB") xx <- xx[order(xx$N_CONTRIB, -xx$TOTAL_CONTRIB),]
                else                                 xx <- xx[order(xx$TOTAL_CONTRIB),]
            }
            else{
                if (input$xsort == "LAST_DATE")      xx <- xx[rev(order(as.Date(xx$LAST_DATE), xx$TOTAL_CONTRIB)),]
                else if (input$xsort == "N_CONTRIB") xx <- xx[order(-xx$N_CONTRIB, -xx$TOTAL_CONTRIB),]
                else                                 xx <- xx[order(-xx$TOTAL_CONTRIB),]
            }
            print(paste0("Sort by ", input$xsort, ", ", input$xsortdir))
            cat(file=stderr(), "Sort by", input$xsort, ",", input$xsortdir, "\n")
            print("")
            print("CAMPAIGN FINANCE CONTRIBUTIONS FOR 2015-2016 ELECTION CYCLE THROUGH JUNE 2016")
            #print("(grouped by name, city, state, employer, and cmte_nm, sorted by total_contrib)")
            print("(grouped by name, city, state, and cmte_nm, sorted by total_contrib)")
            print("")
            print(paste("SUM(TOTAL_CONTRIB) =", format(sum(xx$TOTAL_CONTRIB), big.mark=",",scientific=FALSE)))
            print(paste("SUM(N_CONTRIB)     =", format(sum(xx$N_CONTRIB), big.mark=",",scientific=FALSE)))
            print(paste("NUMBER OF ROWS     =", format(length(xx$N_CONTRIB), big.mark=",",scientific=FALSE)))
            print("")
            itotrows <- as.integer(input$totrows)
            if (nrow(xx) > itotrows) xx <- head(xx, n = itotrows)
            
            xx$NAME       <- strtrim(xx$NAME, width=input$colwidth)
            xx$CITY       <- strtrim(xx$CITY, width=input$colwidth)
            xx$EMPLOYER   <- strtrim(xx$EMPLOYER, width=input$colwidth)
            xx$CMTE_NM    <- strtrim(xx$CMTE_NM, width=input$colwidth)
            xx$OCCUPATION <- strtrim(xx$OCCUPATION, width=input$colwidth)
            #plot1(input, xx) # plot before formatting TOTAL_CONTRIB
            xxnum <<- xx
            xx$TOTAL_CONTRIB <- format(xx$TOTAL_CONTRIB, big.mark=",",scientific=FALSE)
            if (nrow(xx) > 0) row.names(xx) <- 1:nrow(xx)
            options(width = input$totwidth)
            xx <- subset(xx, select = input$xshow)
            #print(xx)
            return(xx)
        }
    }
)
plot1 <- function(input, xx){
    len_plot <- as.integer(input$grfnum)
    if (is.na(len_plot)) len_plot = 50
    pos_left <- 4
    yy <- head(xx, n=len_plot)
    minx <- as.integer(input$grfminx)
    maxx <- as.integer(input$grfmaxx)
    midx <- as.integer(input$grfmidx)
    if (is.na(minx)) minx <- min(yy$TOTAL_CONTRIB/1000)
    if (is.na(maxx)) maxx <- max(yy$TOTAL_CONTRIB/1000)
    if (is.na(midx)) midx <- (minx+maxx)/2
    yy$col <- "green3"
    yy$col[yy$PRTY == "D"] <- "blue3"
    yy$col[yy$PRTY == "R"] <- "red3"
    yy$pos <- 4
    yy$pos[yy$TOTAL_CONTRIB/1000 > midx] <- 2
    
    yy$pch <- 16
    yy$pch[yy$CANDIDATE==""]         <- 3
    yy$pch[yy$CANDIDATE=="CLINTON"]  <- 8
    yy$pch[yy$CANDIDATE=="SANDERS"]  <- 1
    yy$pch[yy$CANDIDATE=="BUSH"]     <- 8
    yy$pch[yy$CANDIDATE=="CHRISTIE"] <- 1
    yy$pch[yy$CANDIDATE=="CRUZ"]     <- 2
    yy$pch[yy$CANDIDATE=="HUCKABEE"] <- 4
    yy$pch[yy$CANDIDATE=="JINDAL"]   <- 4
    yy$pch[yy$CANDIDATE=="PAUL"]     <- 0
    yy$pch[yy$CANDIDATE=="PERRY"]    <- 4
    yy$pch[yy$CANDIDATE=="RUBIO"]    <- 6
    yy$pch[yy$CANDIDATE=="TRUMP"]    <- 9
    yy$pch[yy$CANDIDATE=="WALKER"]   <- 4
    #"CARSON, BENJAMIN S SR MD"
    #"COCHRAN, THAD"
    #"FIORINA"
    #"KASICH, JOHN R"
    #"LOPEZ-CANTERA, CARLOS"
    #"MOULTON, SETH"
    #"MURPHY, ???"
    #"NUNN, MARY MICHELLE"
    #"PATAKI, GEORGE E"
    #"PORTMAN, ROB"
    #"ROMNEY, MITT / PAUL D. RYAN"
    #"SCHWEITZER, ???"
    #"WARD, ???"
    legtxt <- c("Clinton","Sanders","Bush","Christie","Cruz","Paul","Rubio","Trump","(withdrawn)","(blank)","(other)")
    legcol <- c("blue3",  "blue3",  "red3","red3",    "red3","red3","red3", "red3",  "red3",      "red3",   "red3")
    legpch <- c(       8,        1,     8,         1,     2,     0,      6,      9,           4,        3,       16)
    
    RANK <- c(1:nrow(yy))
    title <- paste0("TOP ", len_plot, " TOTAL CONTRIBUTIONS BY INDIVIDUAL AND COMMITTEE\n", subhdr)
    xlabel <- "TOTAL CONTRIBUTIONS (thousands of dollars)"
    #sub <- "Source: http://econdataus.com/cfin16.htm"
    #png("plot1.png", width=840, height=840, pointsize=20)
    #x11()
    if (input$xrankdir == "Ascending"){
        plot(yy$TOTAL_CONTRIB/1000, RANK, cex=0.6, col=yy$col, pch=yy$pch, main=title, xlab=xlabel, xlim=c(minx,maxx), ylim=c(1,len_plot))
        legend("topright", legtxt, cex=1.2, col=legcol, pch=legpch)
    }
    else{
        plot(yy$TOTAL_CONTRIB/1000, RANK, cex=0.6, col=yy$col, pch=yy$pch, main=title, xlab=xlabel, xlim=c(minx,maxx), ylim=c(len_plot, 1))
        legend("bottomright", legtxt, cex=1.2, col=legcol, pch=legpch)
    }
    grid()
    yy$labs <- paste0(yy$NAME, " (", yy$CANDIDATE, ")")
    sfld <- yy[,which(colnames(yy) == input$grfsfld)]
    if (length(sfld) > 0) yy$labs <- paste0(yy$NAME, " (", sfld, ")")
    #varname <- "yy$CANDIDATE"
    #yy$labs <- paste(yy$NAME, " (", get(varname), ")", sep="")
    #with(yy, text(yy$TOTAL_CONTRIB/1000, RANK, labels=labs, cex=0.6, col=col, pch=pch, pos=pos))
    with(yy, text(yy$TOTAL_CONTRIB/1000, RANK, labels=labs, cex=1.0, col=col, pch=pch, pos=pos))
    #legend("bottomleft", c("Democrat", "Republican", "Non-partisan"), cex=0.7, col=c("blue3", "red3", "green3"), pch=3)
    #dev.off()
}