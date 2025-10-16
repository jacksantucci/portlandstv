##################################################
# Portland, OR -- Analysis of first STV election. Convert Jaehun's endorsement data into a table: candidates by number of endorsments and victory status.
# Jack Santucci
# 2024-12-18
# Updated 2025-01-07, 2025-03-03, 2025-10-16 (to verify that 03.03 produces the same results as 02.27)
##################################################

##################################################
# LOAD DATA
##################################################

# d <- read.csv('../data/portlandcitycouncil.endorsements_02.27.csv') # bug, not working

d <- read.csv('../data/portlandcitycouncil.endorsements_03.03.csv')

##################################################
# TABULATE ENDORSEMENT DATA
##################################################

### Isolate endorsements and win status

endorse.idx <- grep('\\.endorsed', names(d))

candidate <- gsub(' ', '', d$Candidate.Name)

win <- d[,'Win.']

district <- d[,'District']

endorsements <- d[,endorse.idx]

names(endorsements) <- gsub('\\.endorsed', '', names(endorsements))

rownames(endorsements) <- candidate

### Tabulation: endorsements per candidate

epc <- rowSums(endorsements) # endorsements per candidate

epc.tab <- table(epc, win)

epc.mat <- cbind(rownames(epc.tab), epc.tab)

dimnames(epc.mat)[[2]] <- c('Endorsements', 'Losers', 'Winners')

### Tabulation: endorsements per endorser

epe <- colSums(endorsements)

epe.tab <- table()

### Make nice table

library(xtable)

tab.out <- xtable(epc.mat)

writeLines(print(tab.out, type='html'), con='../output/endorsements_by_win-status.html')
