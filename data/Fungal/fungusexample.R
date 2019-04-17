## Started 1 April 2019 ##
## By Lizzie ##

# Code to convert from https://nt.ars-grin.gov/fungaldatabases to useful genus and species names #

########################################################
## This first bit deals with the synonyms part (exmp1) ##
########################################################

exmp1 <- read.delim("~/Documents/git/projects/misc/darwin/winegrapedisease/R/cleandatabasenames/fungusexample.txt", header=FALSE)
head(exmp1)

# We probably need to delete either all the two or threebar things ... one is a synonym or such? You should check. For now I pulled out that info in a column
exmp1$whichsymbol <- NA
exmp1$whichsymbol[grep("≡", exmp1$V1)] <- "threebars"
exmp1$whichsymbol[grep("=", exmp1$V1)] <- "twobars"

# Now, in an ungly way I delete the bars and leading white spaces ...
exmp1$editV1 <- exmp1$V1
exmp1$editV1 <- gsub("≡", " ", exmp1$V1)
exmp1$editV1 <- gsub("=", " ", exmp1$editV1)
# Remove the leading white spaces
exmp1$editV1 <- gsub("^\\s+", "", exmp1$editV1)

# Now I break up the columb by white space and take the first and second objects...
breakbywhitespace <- strsplit(as.character(exmp1$editV1), " ", fixed=TRUE) 
exmp1$genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
exmp1$spepithet <- unlist(lapply(breakbywhitespace, function(x) x[2]))

#######################################################
## This second bit deals with the hosts part (exmp2) ##
#######################################################
exmp2 <- read.delim("~/Documents/git/projects/misc/darwin/winegrapedisease/R/cleandatabasenames/fungusexamplehosts.txt", header=FALSE)

# Now I break up the columb by white space and take the first and second objects...
breakbywhitespace <- strsplit(as.character(exmp2$V1), " ", fixed=TRUE) 
exmp2$genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
exmp2$spepithet <- unlist(lapply(breakbywhitespace, function(x) x[2]))
exmp2$location <- unlist(lapply(breakbywhitespace, function(x) x[3]))
