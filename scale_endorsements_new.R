##################################################
# Portland, OR -- Analysis of first STV election. Convert Jaehun's endorsement data into a matrix of integer opinions on candidates, and then scale the matrix to recover 'ideological' locations of endorsers and candidates.
# Jack Santucci
# 2024-12-18
# Updated 2025-01-07, 2025-03-03, 2025-06-17
##################################################

##################################################
# LOAD DATA
##################################################

d <- read.csv('../data/portlandcitycouncil.endorsements_02.27.csv')

### Add "endorsed" to variable name for AFT of Oregon

names(d)[names(d)=="AFT.Oregon"] <- "AFT.Oregon.endorsed"

##################################################
# EXTRACT ENDORSMENTS, GREENLIGHTS, REDLIGHTS, AND CANDIDATE NAMES
##################################################

endorse.idx <- grep('\\.endorsed', names(d))

endorse <- d[,endorse.idx]

greenlight.idx <- grep('\\.greenlight', names(d))

greenlight <- d[,greenlight.idx]

redlight.idx <- grep('\\.red.light', names(d))

redlight <- d[,redlight.idx]

candidate <- gsub(' ', '', d$Candidate.Name)

rownames(endorse) <- candidate

##################################################
# RECODE AND ADD COLUMNS TO CREATE SINGLE SCORE FOR EACH CANDIDATE-GROUP
##################################################

### Endorsements to 1

endorse[endorse==1] <- 1

### Greenlight to 0.5

greenlight[greenlight==1] <- 0.5

### Redlight to -1

redlight[redlight==1] <- -1

### Remove kind of endorsement from each data frame's variable names

names(endorse) <- gsub('\\.endorsed', '', names(endorse))

names(greenlight) <- gsub('\\.greenlight', '', names(greenlight))

names(redlight) <- gsub('\\.red.light', '', names(redlight))

### Add matching columns to each other, using main endorsement matrix

all <- endorse

all[,which(names(all) %in% names(redlight))] <- all[,which(names(all) %in% names(redlight))] + redlight

# all[,which(names(all) %in% names(greenlight))] <- all[,which(names(all) %in% names(greenlight))] + greenlight # drop greenlight

rownames(all) <- candidate

##################################################
# PRINCIPAL COMPONENTS ANALYSIS
##################################################

### rotate matrix

for.pca <- t(all)

pca1 <- prcomp(for.pca)

summary(pca1)

##################################################
# PLOT
##################################################

x <- pca1$x[,1]

y <- -1*pca1$x[,2] # note correction for rotational invariance

pdf('../output/pca_scatterplot_of_endorsers.pdf')

plot(x, y, pch=NA, axes=F, xlab="First dimension", ylab="Second dimension", ylim=c(-3,3))
text(x, y, labels=rownames(pca1$x))
axis(1, tick=T)
axis(2, tick=T, las=2)

dev.off()

### Export data for production team

for.production <- cbind.data.frame("group"=rownames(pca1$x), x, y)

write.csv(for.production, file='../output/pca_scatterplot_of_endorsers_raw-data.csv')

