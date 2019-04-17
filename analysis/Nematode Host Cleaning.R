#Darwin 
### script to clean nematode datasets for wine grape analysis

###Paratrichodorus_minor
Paratrichodorus_minor<- read.delim("~/Documents/Ph.D/Wine grape Disease/nematode/Paratrichodorus_minor.txt")

breakbywhitespace <- strsplit(as.character(Paratrichodorus_minor$PgenusPspec), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Paratrichodorus_minor$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Paratrichodorus_minor$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### adding colum with pathogen name
Paratrichodorus_minor<- cbind(Pathogen = "Paratrichodorus_minor", Paratrichodorus_minor)

### Creating final dataset for fungal pathogen with only the relevant information
Paratrichodorus_minor <- subset(Paratrichodorus_minor, select = c(1, 4, 5))


###Tylenchorhynchus_clarus
Tylenchorhynchus_clarus<- read.delim("~/Documents/Ph.D/Wine grape Disease/nematode/Tylenchorhynchus_clarus.txt")

breakbywhitespace <- strsplit(as.character(Tylenchorhynchus_clarus$PgenusPspec), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Tylenchorhynchus_clarus$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Tylenchorhynchus_clarus$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### adding colum with pathogen name
Tylenchorhynchus_clarus<- cbind(Pathogen = "Tylenchorhynchus_clarus", Tylenchorhynchus_clarus)

### Creating final dataset for fungal pathogen with only the relevant information
Tylenchorhynchus_clarus <- subset(Tylenchorhynchus_clarus, select = c(1, 4, 5))


###Xiphinema_americanum
Xiphinema_americanum<- read.delim("~/Documents/Ph.D/Wine grape Disease/nematode/Xiphinema_americanum.txt")

breakbywhitespace <- strsplit(as.character(Xiphinema_americanum$PgenusPspec), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Xiphinema_americanum$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Xiphinema_americanum$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### adding colum with pathogen name
Xiphinema_americanum<- cbind(Pathogen = "Xiphinema_americanum", Xiphinema_americanum)

### Creating final dataset for fungal pathogen with only the relevant information
Xiphinema_americanum <- subset(Xiphinema_americanum, select = c(1, 4, 5))


###Mesocriconema_xenoplax
Mesocriconema_xenoplax<- read.delim("~/Documents/Ph.D/Wine grape Disease/nematode/Mesocriconema_xenoplax.txt")

breakbywhitespace <- strsplit(as.character(Mesocriconema_xenoplax$PgenusPspec), " ", fixed=TRUE) 

### Now I break up the columb by white space and take the first and second objects...
Mesocriconema_xenoplax$host_genus <- unlist(lapply(breakbywhitespace, function(x) x[1]))
Mesocriconema_xenoplax$host_species <- unlist(lapply(breakbywhitespace, function(x) x[2]))

### adding colum with pathogen name
Mesocriconema_xenoplax<- cbind(Pathogen = "Mesocriconema_xenoplax", Mesocriconema_xenoplax)

### Creating final dataset for fungal pathogen with only the relevant information
Mesocriconema_xenoplax <- subset(Mesocriconema_xenoplax, select = c(1, 4, 5))

### merging all nematode datasets into one
nematodehost<- Reduce(function(x, y) merge(x, y, all=TRUE), list(Paratrichodorus_minor, Tylenchorhynchus_clarus, Xiphinema_americanum, Mesocriconema_xenoplax))

