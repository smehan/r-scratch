###########################################################
### Creates a complex plot with three columns of text
### Saves as a png externally.
###########################################################

P.three_column_list_of_text <- 
    function (filename = "three_column_list_of_text.png", seed=18) 
    {
        if(length(filename)) {
            png(file=filename, width=512, height=700)
            par(mar=rep(0,4)+.1)
        }
        middle_text <- c('First', 'Second', 'Third', 'Fourth',
                       'Fifth', 'Sixth', 'Seventh', 'Eighth', 
                       'Ninth, and some additional text', 
                       'Ten and text', 'Eleventh', 'Twelve!',
                       'Thirteenth', 'Forteenth', 'Fifteen',
                       'Siteeen', 'Seventeen', 'Eighteen', 
                       'Nineteen', "And, finally, 20")
        plot.new()
        plot.window(xlim=c(0,4), ylim=c(21,0))
        hl <- 1.7
        pl <- 2.3
        
        if(length(seed) && !is.na(seed)) set.seed(seed)
        
        text(c(.5, 2, 3.5), .5, c("Column 1", "Middle Column", "Column 3"), font=2)
        rect(0, 1.5, hl, 9.5, col=do.call('rgb', as.list(runif(3, .8, 1))), border=NA)
        rect(0, 9.5, hl, 12.5, col=do.call('rgb', as.list(runif(3, .8, 1))), border=NA)
        rect(0, 12.5, hl, 15.5, col=do.call('rgb', as.list(runif(3, .8, 1))), border=NA)
        rect(0, 15.5, hl, 20.5, col=do.call('rgb', as.list(runif(3, .8, 1))), border=NA)
        
        rect(pl, 1.5, 4, 4.5, col=do.call('rgb', as.list(runif(3, .8, 1))), border=NA)
        rect(pl, 5.5, 4, 14.5, col=do.call('rgb', as.list(runif(3, .8, 1))), border=NA)
        rect(pl, 14.5, 4, 20.5, col=do.call('rgb', as.list(runif(3, .8, 1))), border=NA)
        
        text(2, 1:20, middle_text)
        text(hl/2, 5.5, "C1L1")
        text(hl/2, 11, "C1L2")
        text(hl/2, 14, "C1L3")
        text(hl/2, 18, "C1L4")
        text((4+pl)/2, 3, "C3L1")
        text((4+pl)/2, 10, "C3L2")
        text((4+pl)/2, 17.5, "C3L3")
        
        if(length(filename)) {
            dev.off()
        }
    }

