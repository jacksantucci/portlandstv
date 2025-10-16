##################################################
# Portland, OR -- Analysis of first STV election. Histograms of first-choice share, by district (percent of quota).
# Jack Santucci
# 2025-02-11
# Updated 2025-03-03
# Updated 2025-05-28 -- export data points for reproduction of seriousness histograms
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

head(d[,grep('Owens', names(d1))]) # Examine structure of file by looking at Peggy Sue Owens columns

foo <- d1[,grep('Owens', names(d1))]

##################################################
# FUNCTION TO EXTRACT SERIOUSNESS MEASURES
##################################################

### Function to compute potential measures of seriousness

getSerious <- function(d){
	
	fc.cols <- grep('1.Number.of.Winners', names(d))
	
	fcd <- d[,fc.cols]
	
	new.names <- substr(names(fcd), start=regexpr('Number.of.Winners.3', names(fcd))+20, stop=nchar(names(fcd)))
	
	names(fcd) <- new.names
	
	fc.votes <- colSums(fcd)
	
	quota <- floor(sum(fc.votes)/(3+1)+1) # quota rules here: https://www.portland.gov/code/2/08/030. Rounding rule is "disregarding any fractions."
	
	shares.of.quota <- fc.votes/quota
	
	ncand <- length(shares.of.quota)
	
	fc.shares <- fc.votes/sum(fc.votes)
	
	enc <- 1/sum(fc.shares^2)
	
	frac <- sum(fc.shares^2)
	
	out <- list(shares.of.quota, fc.shares, ncand, enc, frac)
	
	names(out) <- c("quotaShare", "fcShare", "ncand", "enc", "frac")
	
	return(out)
	
}

##################################################
# RUN FUNCTION
##################################################

d1out <- getSerious(d1)

d2out <- getSerious(d2)

d3out <- getSerious(d3)

d4out <- getSerious(d4)

##################################################
# CREATE DATA FRAMES FOR PRODUCTION TEAM
##################################################

##### Make data frame of first-choice shares of quota for each candidate

district1 <- cbind.data.frame(
	"candidate"=names(d1out$quotaShare),
	"quotaShare"=d1out$quotaShare,
	"winner"=ifelse(grepl("Avalos|Dunphy|Smith", names(d1out$quotaShare)), 1, 0)
)

district2 <- cbind.data.frame(
	"candidate"=names(d2out$quotaShare),
	"quotaShare"=d2out$quotaShare,
	"winner"=ifelse(grepl("Kanal|Pirtle|Ryan", names(d2out$quotaShare)), 1, 0)
)

district3 <- cbind.data.frame(
	"candidate"=names(d3out$quotaShare),
	"quotaShare"=d3out$quotaShare,
	"winner"=ifelse(grepl("Novick|Morillo|Koyama", names(d3out$quotaShare)), 1, 0)
)

district4 <- cbind.data.frame(
	"candidate"=names(d4out$quotaShare),
	"quotaShare"=d4out$quotaShare,
	"winner"=ifelse(grepl("Clark|Green|Zimmerman", names(d4out$quotaShare)), 1, 0)
)

##### Write those data frames to an Excel workbook

library(openxlsx2)

book <- wb_workbook()

book <- book$add_worksheet("district1")
book <- book$add_data("district1", district1)

book <- book$add_worksheet("district2")
book <- book$add_data("district2", district2)

book <- book$add_worksheet("district3")
book <- book$add_data("district3", district3)

book <- book$add_worksheet("district4")
book <- book$add_data("district4", district4)

book$save("../output/forProductionTeam/droop_by_district.xlsx")

##################################################
# CREATE GRAPHICS FOR DRAFT VERSION OF REPORT
##################################################

pdf('../output/seriousness_histograms.pdf')

par(mfrow=c(2,2))

h1 <- hist(d1out$quotaShare, xlim=c(0,1), ylim=c(0,30), xlab="Share of Droop quota", border=F, axes=F, main="District 1")
axis(1, tick=T)
axis(2, tick=T, las=2)
mtext(text=paste0("Total candidates: ", d1out$ncand), side=3)
points(x=d1out$quotaShare[grep('Avalos', names(d1out$quotaShare))], y=0, pch=25, bg="black")
points(x=d1out$quotaShare[grep('Dunphy', names(d1out$quotaShare))], y=0, pch=25, bg="black")
points(x=d1out$quotaShare[grep('Smith', names(d1out$quotaShare))], y=0, pch=25, bg="black")

h2 <- hist(d2out$quotaShare, xlim=c(0,1), ylim=c(0,30), xlab="Share of Droop quota", border=F, axes=F, main="District 2")
axis(1, tick=T)
axis(2, tick=T, las=2)
mtext(text=paste0("Total candidates: ", d2out$ncand), side=3)
points(x=d2out$quotaShare[grep('Kanal', names(d2out$quotaShare))], y=0, pch=25, bg="black")
points(x=d2out$quotaShare[grep('Pirtle', names(d2out$quotaShare))], y=0, pch=25, bg="black")
points(x=d2out$quotaShare[grep('Ryan', names(d2out$quotaShare))], y=0, pch=25, bg="black")

h3 <- hist(d3out$quotaShare, xlim=c(0,1), ylim=c(0,30), xlab="Share of Droop quota", border=F, axes=F, main="District 3")
axis(1, tick=T)
axis(2, tick=T, las=2)
mtext(text=paste0("Total candidates: ", d3out$ncand), side=3)
points(x=d3out$quotaShare[grep('Novick', names(d3out$quotaShare))], y=0, pch=25, bg="black")
points(x=d3out$quotaShare[grep('Morillo', names(d3out$quotaShare))], y=0, pch=25, bg="black")
points(x=d3out$quotaShare[grep('Koyama', names(d3out$quotaShare))], y=0, pch=25, bg="black")

h4 <- hist(d4out$quotaShare, xlim=c(0,1), ylim=c(0,30), xlab="Share of Droop quota", border=F, axes=F, main="District 4")
axis(1, tick=T)
axis(2, tick=T, las=2)
mtext(text=paste0("Total candidates: ", d4out$ncand), side=3)
points(x=d4out$quotaShare[grep('Clark', names(d4out$quotaShare))], y=0, pch=25, bg="black")
points(x=d4out$quotaShare[grep('Green', names(d4out$quotaShare))], y=0, pch=25, bg="black")
points(x=d4out$quotaShare[grep('Zimmerman', names(d4out$quotaShare))], y=0, pch=25, bg="black")

dev.off()
