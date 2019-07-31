#Darwin 
### script to clean fungal datasets for wine grape analysis

### Alternaria_alternata load dataset to R
Alternaria_alternata<- read.delim("~/Documents/Ph.D/Wine grape Disease/Fungal/Alternaria alternata.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Alternaria_alternata$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Alternaria_alternata$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Alternaria_alternata$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Alternaria_alternata$host_species <- as.character(gsub(":","",Alternaria_alternata$host_species))

### adding colum with pathogen name
Alternaria_alternata<- cbind(Pathogen = "Alternaria_alternata", Alternaria_alternata)

### Creating final dataset for fungal pathogen with only the relevant information
Alternaria_alternata <- subset(Alternaria_alternata, select = c(1, 3, 4))


### Botrytiscinerea load dataset to R
Botrytiscinerea<- read.delim("~/Documents/Ph.D/Wine grape Disease/Fungal/Botrytiscinerea.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Botrytiscinerea$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Botrytiscinerea$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Botrytiscinerea$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Botrytiscinerea$host_species <- as.character(gsub(":","",Botrytiscinerea$host_species))

### adding colum with pathogen name
Botrytiscinerea<- cbind(Pathogen = "Botrytis_cinerea", Botrytiscinerea)

### Creating final dataset for fungal pathogen with only the relevant information
Botrytiscinerea <- subset(Botrytiscinerea, select = c(1, 3, 4))


### Eutypalata load dataset to R
Eutypalata<- read.delim("~/Documents/Ph.D/Wine grape Disease/Fungal/Eutypalata.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Eutypalata$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Eutypalata$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Eutypalata$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Eutypalata$host_species <- as.character(gsub(":","",Eutypalata$host_species))

### adding colum with pathogen name
Eutypalata<- cbind(Pathogen = "Eutypa_lata", Eutypalata)

### Creating final dataset for fungal pathogen with only the relevant information
Eutypalata <- subset(Eutypalata, select = c(1, 3, 4))


### Lasiodiplodiatheobromae load dataset to R
Lasiodiplodiatheobromae<- read.delim("~/Documents/Ph.D/Wine grape Disease/Fungal/Lasiodiplodiatheobromae.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Lasiodiplodiatheobromae$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Lasiodiplodiatheobromae$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Lasiodiplodiatheobromae$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Lasiodiplodiatheobromae$host_species <- as.character(gsub(":","",Lasiodiplodiatheobromae$host_species))

### adding colum with pathogen name
Lasiodiplodiatheobromae<- cbind(Pathogen = "Lasiodiplodia_theobromae", Lasiodiplodiatheobromae)

### Creating final dataset for fungal pathogen with only the relevant information
Lasiodiplodiatheobromae <- subset(Lasiodiplodiatheobromae, select = c(1, 3, 4))

### merging all fungal datasets into one
Fungalhost<- Reduce(function(x, y) merge(x, y, all=TRUE), list(Alternaria_alternata, Botrytiscinerea, Eutypalata, Lasiodiplodiatheobromae))
