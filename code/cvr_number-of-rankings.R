##################################################
# Portland, OR -- Analysis of first STV election. Voters' use of rankings by district.
# Jack Santucci
# 2025-03-05
# Updated...
##################################################

# From Jaehun on 2025-02-07

# Here's what Multnomah County told me regarding the variables and what they mean. Happy to follow up with them if needed—just let me know!
	# •	Box ID, Box Position - Notes how and where a ballot is physically stored at the elections division office
	# •	Ballot ID and Ballot Style - Notes the ballot design (i.e. which contests appear on it, based on their Portland district)
	# •	Precinct ID - Internal code for the precinct of the voter
	# •	Precinct Style - The voter's precinct
	# •	Scan Computer Name - The scanner that was used to count the ballot
	# •	Status/Remade - For internal organization purposes. Notes whether the ballot had to be remade because the paper ballot was not scannable. The columns balance each other -- if there is a -1 in remade, there will be a 1 in status. 
# The remainder of the columns show how a ballot was marked. A '0' indicates no marking, a '1' indicates that an oval was filled in on the ballot. 

##################################################
# LOAD DATA FOR ALL FOUR DISTRICTS
##################################################

d1 <- read.csv("../data/portland city council cvrs/citycouncil1.cvr.csv")

d2 <- read.csv("../data/portland city council cvrs/citycouncil2.cvr.csv")

d3 <- read.csv("../data/portland city council cvrs/citycouncil3.cvr.csv")

d4 <- read.csv("../data/portland city council cvrs/citycouncil4.cvr.csv")

# head(d[,grep('Owens', names(d1))]) # Examine structure of file by looking at Peggy Sue Owens columns

# foo <- d1[,grep('Owens', names(d1))]

##################################################
# FUNCTION TO EXTRACT SERIOUSNESS MEASURES
##################################################

### Function to compute measures of ranking use

getRanked <- function(d){
	
	vote.cols <- grep('Councilor', names(d))
	
	rankings <- d[,vote.cols]
	
	new.names <- substr(names(rankings), start=regexpr('District.[0-9].', names(rankings)), stop=nchar(names(rankings)))
	
	new.names <- gsub('District.[0-9].', '', new.names)
	
	new.names <- gsub('Number.of.Winners.[0-9].', '', new.names)
	
	names(rankings) <- new.names
	
	### rankings used
	
	by.voter <- rowSums(rankings)
	
	# # look at ballots with 6
	# idx <- which(by.voter==6)
	# head(rankings[idx,])
	
	rankings.nonempty <- rankings[which(by.voter>0),] # zero out ballots without any rankings
	
	by.voter.nonempty <- rowSums(rankings.nonempty)
	
	by.voter.nonempty.tab <- table(by.voter.nonempty)
	
	table.of.use <- c(by.voter.nonempty.tab[1:6], sum(by.voter.nonempty.tab[7:length(by.voter.nonempty.tab)]))
	
	names(table.of.use) <- c(1:6, "7+")
	
	freq <- table.of.use
	
	pct <- 100*(table.of.use/sum(table.of.use))
	
	total.ballots <- nrow(rankings)
	
	total.ballots.nonempty <- nrow(rankings.nonempty)
	
	# by.voter.pct <- 100*(by.voter.tab/sum(by.voter.tab))
	
	### duplicate rankings, etc -- for later
	
	out <- list(freq, pct, total.ballots, total.ballots.nonempty)
	
	names(out) <- c("freq", "pct", "total.ballots", "total.ballots.nonempty")
	
	return(out)
	
}

##################################################
# RUN FUNCTION; CREATE TABLE
##################################################

d1out <- getRanked(d1)

d2out <- getRanked(d2)

d3out <- getRanked(d3)

d4out <- getRanked(d4)

### Extract information

ranking.table <- rbind(d1out$pct, d2out$pct, d3out$pct, d4out$pct)

ranking.table <- round(ranking.table, 2)

ranking.table <- cbind("District"=c(1:4), ranking.table)

library(xtable)

table.out <- print(xtable(ranking.table, caption="Table X: Percent of ballots containing N rankings, by district"), type="html")

writeLines(table.out, con="../output/table_use-of-ranking.html")

cbind(ranking.table, "Ballots ranked"=c(d1out$total.ballots.nonempty, d2out$total.ballots.nonempty, d3out$total.ballots.nonempty, d4out$total.ballots.nonempty), "Total ballots"=c(d1out$total.ballots, d2out$total.ballots, d3out$total.ballots, d4out$total.ballots))
