#Darwin 
### script to clean Botryosphaeriaceae datasets for wine grape analysis

# housekeeping
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/Wine grape Disease")

### Botryosphaeria_dothidea load dataset to R
Botryosphaeria_dothidea<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Botryosphaeria_dothidea.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Botryosphaeria_dothidea$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Botryosphaeria_dothidea$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Botryosphaeria_dothidea$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Botryosphaeria_dothidea$host_species <- as.character(gsub(":","",Botryosphaeria_dothidea$host_species))

### adding colum with pathogen name
Botryosphaeria_dothidea<- cbind(Pathogen = "Botryosphaeria_dothidea", Botryosphaeria_dothidea)

### Creating final dataset for fungal pathogen with only the relevant information
Botryosphaeria_dothidea <- subset(Botryosphaeria_dothidea, select = c(1, 3, 4))


### Diplodia_corticola load dataset to R
Diplodia_corticola<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Diplodia_corticola.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Diplodia_corticola$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Diplodia_corticola$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Diplodia_corticola$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Diplodia_corticola$host_species <- as.character(gsub(":","",Diplodia_corticola$host_species))

### adding colum with pathogen name
Diplodia_corticola<- cbind(Pathogen = "Diplodia_corticola", Diplodia_corticola)

### Creating final dataset for fungal pathogen with only the relevant information
Diplodia_corticola <- subset(Diplodia_corticola, select = c(1, 3, 4))


### Diplodia_mutila load dataset to R
Diplodia_mutila<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Diplodia_mutila.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Diplodia_mutila$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Diplodia_mutila$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Diplodia_mutila$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Diplodia_mutila$host_species <- as.character(gsub(":","",Diplodia_mutila$host_species))

### adding colum with pathogen name
Diplodia_mutila<- cbind(Pathogen = "Diplodia_mutila", Diplodia_mutila)

### Creating final dataset for fungal pathogen with only the relevant information
Diplodia_mutila <- subset(Diplodia_mutila, select = c(1, 3, 4))


### Diplodia_seriata load dataset to R
Diplodia_seriata<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Diplodia_seriata.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Diplodia_seriata$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Diplodia_seriata$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Diplodia_seriata$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Diplodia_seriata$host_species <- as.character(gsub(":","",Diplodia_seriata$host_species))

### adding colum with pathogen name
Diplodia_seriata<- cbind(Pathogen = "Diplodia_seriata", Diplodia_seriata)

### Creating final dataset for fungal pathogen with only the relevant information
Diplodia_seriata <- subset(Diplodia_seriata, select = c(1, 3, 4))


### Dothiorella_americana load dataset to R
Dothiorella_americana<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Dothiorella_americana.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Dothiorella_americana$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Dothiorella_americana$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Dothiorella_americana$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Dothiorella_americana$host_species <- as.character(gsub(":","",Dothiorella_americana$host_species))

### adding colum with pathogen name
Dothiorella_americana<- cbind(Pathogen = "Dothiorella_americana", Dothiorella_americana)

### Creating final dataset for fungal pathogen with only the relevant information
Dothiorella_americana <- subset(Dothiorella_americana, select = c(1, 3, 4))


### Dothiorella_iberica load dataset to R
Dothiorella_iberica<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Dothiorella_iberica.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Dothiorella_iberica$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Dothiorella_iberica$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Dothiorella_iberica$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Dothiorella_iberica$host_species <- as.character(gsub(":","",Dothiorella_iberica$host_species))

### adding colum with pathogen name
Dothiorella_iberica<- cbind(Pathogen = "Dothiorella_iberica", Dothiorella_iberica)

### Creating final dataset for fungal pathogen with only the relevant information
Dothiorella_iberica <- subset(Dothiorella_iberica, select = c(1, 3, 4))


### Guignardia_bidwellii load dataset to R
Guignardia_bidwellii<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Guignardia_bidwellii.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Guignardia_bidwellii$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Guignardia_bidwellii$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Guignardia_bidwellii$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Guignardia_bidwellii$host_species <- as.character(gsub(":","",Guignardia_bidwellii$host_species))

### adding colum with pathogen name
Guignardia_bidwellii<- cbind(Pathogen = "Guignardia_bidwellii", Guignardia_bidwellii)

### Creating final dataset for fungal pathogen with only the relevant information
Guignardia_bidwellii <- subset(Guignardia_bidwellii, select = c(1, 3, 4))


### Lasiodiplodia_crassispora load dataset to R
Lasiodiplodia_crassispora<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Lasiodiplodia_crassispora.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Lasiodiplodia_crassispora$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Lasiodiplodia_crassispora$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Lasiodiplodia_crassispora$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Lasiodiplodia_crassispora$host_species <- as.character(gsub(":","",Lasiodiplodia_crassispora$host_species))

### adding colum with pathogen name
Lasiodiplodia_crassispora<- cbind(Pathogen = "Lasiodiplodia_crassispora", Lasiodiplodia_crassispora)

### Creating final dataset for fungal pathogen with only the relevant information
Lasiodiplodia_crassispora <- subset(Lasiodiplodia_crassispora, select = c(1, 3, 4))


### Lasiodiplodia_missouriana load dataset to R
Lasiodiplodia_missouriana<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Lasiodiplodia_missouriana.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Lasiodiplodia_missouriana$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Lasiodiplodia_missouriana$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Lasiodiplodia_missouriana$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Lasiodiplodia_missouriana$host_species <- as.character(gsub(":","",Lasiodiplodia_missouriana$host_species))

### adding colum with pathogen name
Lasiodiplodia_missouriana<- cbind(Pathogen = "Lasiodiplodia_missouriana", Lasiodiplodia_missouriana)

### Creating final dataset for fungal pathogen with only the relevant information
Lasiodiplodia_missouriana <- subset(Lasiodiplodia_missouriana, select = c(1, 3, 4))


### Lasiodiplodia_viticola load dataset to R
Lasiodiplodia_viticola<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Lasiodiplodia_viticola.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Lasiodiplodia_viticola$V1), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Lasiodiplodia_viticola$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Lasiodiplodia_viticola$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Lasiodiplodia_viticola$host_species <- as.character(gsub(":","",Lasiodiplodia_viticola$host_species))

### adding colum with pathogen name
Lasiodiplodia_viticola<- cbind(Pathogen = "Lasiodiplodia_viticola", Lasiodiplodia_viticola)

### Creating final dataset for fungal pathogen with only the relevant information
Lasiodiplodia_viticola <- subset(Lasiodiplodia_viticola, select = c(1, 3, 4))


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


### Neofusicoccum_australe load dataset to R
Neofusicoccum_australe<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_australe.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_australe$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_australe$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_australe$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_australe$host_species <- as.character(gsub(":","",Neofusicoccum_australe$host_species))

### adding colum with pathogen name
Neofusicoccum_australe<- cbind(Pathogen = "Neofusicoccum_australe", Neofusicoccum_australe)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_australe <- subset(Neofusicoccum_australe, select = c(1, 3, 4))


### Neofusicoccum_luteum load dataset to R
Neofusicoccum_luteum<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_luteum.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_luteum$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_luteum$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_luteum$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_luteum$host_species <- as.character(gsub(":","",Neofusicoccum_luteum$host_species))

### adding colum with pathogen name
Neofusicoccum_luteum<- cbind(Pathogen = "Neofusicoccum_luteum", Neofusicoccum_luteum)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_luteum <- subset(Neofusicoccum_luteum, select = c(1, 3, 4))


### Neofusicoccum_macroclavatum load dataset to R
Neofusicoccum_macroclavatum<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_macroclavatum.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_macroclavatum$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_macroclavatum$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_macroclavatum$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_macroclavatum$host_species <- as.character(gsub(":","",Neofusicoccum_macroclavatum$host_species))

### adding colum with pathogen name
Neofusicoccum_macroclavatum<- cbind(Pathogen = "Neofusicoccum_macroclavatum", Neofusicoccum_macroclavatum)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_macroclavatum <- subset(Neofusicoccum_macroclavatum, select = c(1, 3, 4))


### Neofusicoccum_mediterraneum load dataset to R
Neofusicoccum_mediterraneum<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_mediterraneum.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_mediterraneum$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_mediterraneum$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_mediterraneum$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_mediterraneum$host_species <- as.character(gsub(":","",Neofusicoccum_mediterraneum$host_species))

### adding colum with pathogen name
Neofusicoccum_mediterraneum<- cbind(Pathogen = "Neofusicoccum_mediterraneum", Neofusicoccum_mediterraneum)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_mediterraneum <- subset(Neofusicoccum_mediterraneum, select = c(1, 3, 4))


### Neofusicoccum_parvum load dataset to R
Neofusicoccum_parvum<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_parvum.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_parvum$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_parvum$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_parvum$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_parvum$host_species <- as.character(gsub(":","",Neofusicoccum_parvum$host_species))

### adding colum with pathogen name
Neofusicoccum_parvum<- cbind(Pathogen = "Neofusicoccum_parvum", Neofusicoccum_parvum)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_parvum<- subset(Neofusicoccum_parvum, select = c(1, 3, 4))


### Neofusicoccum_ribis load dataset to R
Neofusicoccum_ribis<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_ribis.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_ribis$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_ribis$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_ribis$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_ribis$host_species <- as.character(gsub(":","",Neofusicoccum_ribis$host_species))

### adding colum with pathogen name
Neofusicoccum_ribis<- cbind(Pathogen = "Neofusicoccum_ribis", Neofusicoccum_ribis)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_ribis<- subset(Neofusicoccum_ribis, select = c(1, 3, 4))


### Neofusicoccum_viticlavatum load dataset to R
Neofusicoccum_viticlavatum<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_viticlavatum.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_viticlavatum$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_viticlavatum$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_viticlavatum$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_viticlavatum$host_species <- as.character(gsub(":","",Neofusicoccum_viticlavatum$host_species))

### adding colum with pathogen name
Neofusicoccum_viticlavatum<- cbind(Pathogen = "Neofusicoccum_viticlavatum", Neofusicoccum_viticlavatum)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_viticlavatum<- subset(Neofusicoccum_viticlavatum, select = c(1, 3, 4))


### Neofusicoccum_vitifusiforme load dataset to R
Neofusicoccum_vitifusiforme<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Neofusicoccum_vitifusiforme.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Neofusicoccum_vitifusiforme$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Neofusicoccum_vitifusiforme$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Neofusicoccum_vitifusiforme$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Neofusicoccum_vitifusiforme$host_species <- as.character(gsub(":","",Neofusicoccum_vitifusiforme$host_species))

### adding colum with pathogen name
Neofusicoccum_vitifusiforme<- cbind(Pathogen = "Neofusicoccum_vitifusiforme", Neofusicoccum_vitifusiforme)

### Creating final dataset for fungal pathogen with only the relevant information
Neofusicoccum_vitifusiforme<- subset(Neofusicoccum_vitifusiforme, select = c(1, 3, 4))


### Phaeobotryosphaeria_porosa load dataset to R
Phaeobotryosphaeria_porosa<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Phaeobotryosphaeria_porosa.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Phaeobotryosphaeria_porosa$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Phaeobotryosphaeria_porosa$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Phaeobotryosphaeria_porosa$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Phaeobotryosphaeria_porosa$host_species <- as.character(gsub(":","",Phaeobotryosphaeria_porosa$host_species))

### adding colum with pathogen name
Phaeobotryosphaeria_porosa<- cbind(Pathogen = "Phaeobotryosphaeria_porosa", Phaeobotryosphaeria_porosa)

### Creating final dataset for fungal pathogen with only the relevant information
Phaeobotryosphaeria_porosa<- subset(Phaeobotryosphaeria_porosa, select = c(1, 3, 4))


### Spencermartinsia_viticola load dataset to R
Spencermartinsia_viticola<- read.delim("~/Documents/Ph.D/Wine grape Disease/Botryosphaeriaceae/Spencermartinsia_viticola.txt", header=FALSE)

breakbywhitespace <- strsplit(as.character(Spencermartinsia_viticola$V1), " ", fixed=TRUE)

### Now I break up the columb by white space and take the first and second objects...
Spencermartinsia_viticola$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Spencermartinsia_viticola$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### removes : and replaces with blank space 
Spencermartinsia_viticola$host_species <- as.character(gsub(":","",Spencermartinsia_viticola$host_species))

### adding colum with pathogen name
Spencermartinsia_viticola<- cbind(Pathogen = "Spencermartinsia_viticola", Spencermartinsia_viticola)

### Creating final dataset for fungal pathogen with only the relevant information
Spencermartinsia_viticola<- subset(Spencermartinsia_viticola, select = c(1, 3, 4))

### Merging all datasets 
Botryosphaeriaceae<- Reduce(function(x, y) merge(x, y, all=TRUE), list(Botryosphaeria_dothidea,Diplodia_corticola, Diplodia_mutila, Diplodia_seriata, Dothiorella_americana, Dothiorella_iberica, Guignardia_bidwellii, Lasiodiplodia_crassispora, Lasiodiplodia_missouriana, Lasiodiplodia_viticola, Lasiodiplodiatheobromae, Neofusicoccum_australe, Neofusicoccum_luteum, Neofusicoccum_macroclavatum, Neofusicoccum_mediterraneum, Neofusicoccum_parvum, Neofusicoccum_ribis, Neofusicoccum_viticlavatum, Neofusicoccum_vitifusiforme, Phaeobotryosphaeria_porosa, Spencermartinsia_viticola))

### remove duplicates from data set
### 669 rows deleted
Botryosphaeriaceae<- Botryosphaeriaceae [!duplicated(Botryosphaeriaceae[c(1,2,3)]),]

