#Darwin 
### script to clean pest datasets for wine grape analysis

#load Pseudococcus longispinus dataset into R
Pseudococcus_longispinus<- read.delim("~/Documents/Ph.D/Wine grape Disease/Pests/Pseudococcus longispinus.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Pseudococcus_longispinus$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Pseudococcus_longispinus$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Pseudococcus_longispinus$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

#remove data that doesn't include genus and species 
#removes all NAs
Pseudococcus_longispinus<- Pseudococcus_longispinus[complete.cases(Pseudococcus_longispinus),]

#removes all vertical bars and replaces with a space
Pseudococcus_longispinus$host_species<- gsub("\\|", "", Pseudococcus_longispinus$host_species)

#removes all rows with spaces in the host_species column 
Pseudococcus_longispinus<- Pseudococcus_longispinus[!(Pseudococcus_longispinus$host_species == ""), ]

### adding colum with pathogen name
Pseudococcus_longispinus<- cbind(Pathogen = "Pseudococcus_longispinus", Pseudococcus_longispinus)

### Creating final dataset for pest pathogen with only the relevant information
Pseudococcus_longispinus<- subset(Pseudococcus_longispinus, select = c(-2))

# load Pseudococcus viburni dataset into R
Pseudococcus_viburni<- read.delim("~/Documents/Ph.D/Wine grape Disease/Pests/Pseudococcus viburni.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Pseudococcus_viburni$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Pseudococcus_viburni$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Pseudococcus_viburni$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

#remove data that doesn't include genus and species 
#removes all NAs
Pseudococcus_viburni<- Pseudococcus_viburni[complete.cases(Pseudococcus_viburni),]

#removes all vertical bars and replaces with a space
Pseudococcus_viburni$host_species<- gsub("\\|", "", Pseudococcus_viburni$host_species)

#removes all rows with spaces in the host_species column 
Pseudococcus_viburni<- Pseudococcus_viburni[!(Pseudococcus_viburni$host_species == ""), ]

### adding colum with pathogen name
Pseudococcus_viburni<- cbind(Pathogen = "Pseudococcus_viburni", Pseudococcus_viburni)

### Creating final dataset for pest pathogen with only the relevant information
Pseudococcus_viburni<- subset(Pseudococcus_viburni, select = c(-2))

### merging all fungal datasets into one
pesthost<- Reduce(function(x, y) merge(x, y, all=TRUE), list(Pseudococcus_longispinus, Pseudococcus_viburni))

