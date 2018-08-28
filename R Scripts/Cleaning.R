LibraryData <- list()

libraryPaths <- paste("./rawData/ontario_library_stats_", 1999:2017, ".csv", sep = "") 
## All of the paths from the directory to the data, some of the names were altered manually

cs.as.numeric <- function(x) { ## For numbers read as strings due to the inclusion of commas
  x <- gsub(pattern = ",", replacement = "", x)
  x <- as.numeric(x)
  x
}

for (i in seq_along(1999:2017)) { ## Read all of the library data into R
  if (i == 19 | i == 16) {
    LibraryData[[i]] <- read.csv(libraryPaths[i], stringsAsFactors = FALSE, header = T, skip = 1)
  }
  else {
    LibraryData[[i]] <- read.csv(libraryPaths[i], stringsAsFactors = FALSE, header = T)
  }
}

names(LibraryData) <- paste("Year", 1999:2017, sep = ".")

headersRawData <- lapply(LibraryData, colnames)

#allSameAsYearBefore <- rep(NA, length(2000:2017))

#for (i in seq_along(2000:2017)) {
#  allSameAsYearBefore[i] <- all(headersRawData[[i]] %in% headersRawData[[i+1]])
#}

headersRawData[[2]][1] <- "LibID"

colnames(LibraryData[[2]])[1] <- "LibID" ## Makes 2000 and 2001's naming consistent

colnames(LibraryData$Year.2011)[1] <- "LibID"

colnames(LibraryData$Year.2012)[1] <- "LibID"

colnames(LibraryData$Year.2013)[1] <- "LibID"

colnames(LibraryData$Year.2014)[2] <- "LibID"

colnames(LibraryData$Year.2015)[2] <- "LibID"

colnames(LibraryData$Year.2016)[2] <- "LibID"

colnames(LibraryData$Year.2017)[2] <- "LibID"

## As of now, the data is divided into three categories based on data recording practices: 1999, 2000-2010, and 2011-2017.  The last group however also has more data reported as time goes on.

#for (i in seq_along(2012:2017)) { ## Find mismatches
#  print(headersRawData[[i+12]] %in% headersRawData[[i+13]])
#}

# headersRawData$Year.2012[25] ## Was removed in the following year.
# headersRawDat a$Year.2012[154] ## Was removed in the following year.
# headersRawData$Year.2012[275] ## Was removed in the following year.

## Between 2013 and 2014 they seem to have removed a single period in all of the questions (from ... to ..), I will go back and change every triple period to a double across everything.

for (i in seq_along(1999:2017)) {
  headersRawData[[i]] <- gsub(pattern = "\\.\\.\\.", replacement = "\\.\\.", headersRawData[[i]])
}

for (i in seq_along(1999:2017)) {
  colnames(LibraryData[[i]]) <- headersRawData[[i]]
}

# headersRawData$Year.2013[20] ## Question was omitted in later versions.
# headersRawData$Year.2013[277] ## Renamed due to program renaming - question replaced.
# headersRawData$Year.2014[26] ## Question was omitted in later versions.

for (i in seq_len(length(LibraryData$Year.2013$E2.1..Does.your.library.have.an.automated.catalogue.system.))) { ## Found one portion where F is 0 in one group and NA in another.
  if (is.na(LibraryData$Year.2013$E2.1..Does.your.library.have.an.automated.catalogue.system.[i])) {
    LibraryData$Year.2014$E2.1..Does.your.library.have.an.automated.catalogue.system.[i] <- 0
  }
}

# headersRawData$Year.2014[158:159] ## First column is useless in 2015 version.

LibraryData$Year.2015 <- LibraryData$Year.2015[ , -169]
headersRawData$Year.2015 <- headersRawData$Year.2015[-169]

LibraryData$Year.2016 <- LibraryData$Year.2016[ , -129]
headersRawData$Year.2016 <- headersRawData$Year.2016[-129]

colnames(LibraryData$Year.2015)[169] <- "E2.1..Does.your.library.have.an.automated.catalogue.system."
headersRawData$Year.2015 <- colnames(LibraryData$Year.2015)

colnames(LibraryData$Year.2016)[129] <- "E2.1..Does.your.library.have.an.automated.catalogue.system."
headersRawData$Year.2016 <- colnames(LibraryData$Year.2016)

LibraryData$Year.2015 <- LibraryData$Year.2015[ , -170]
colnames(LibraryData$Year.2015)[170] <- "E2.3..Does.your.library.provide.wireless.Internet.connection." ## First column is useless in 2015 version
headersRawData$Year.2015 <- colnames(LibraryData$Year.2015)

LibraryData$Year.2016 <- LibraryData$Year.2016[ , -130]
colnames(LibraryData$Year.2016)[130] <- "E2.3..Does.your.library.provide.wireless.Internet.connection." ## First column is useless in 2015 version
headersRawData$Year.2016 <- colnames(LibraryData$Year.2016)

LibraryData$Year.2017 <- LibraryData$Year.2017[ , -c(133, 135)]

colnames(LibraryData$Year.2017)[133:134] <- c("E2.1..Does.your.library.have.an.automated.catalogue.system.", "E2.3..Does.your.library.provide.wireless.Internet.connection.")

# headersRawData$Year.2015[19] ## Question's text was changed but the content was the same between 2015 and 2016.

colnames(LibraryData$Year.2015)[19] <- "B2.3..Contract.Revenue..funds.from.other.municipalities"
colnames(LibraryData$Year.2016)[19] <- "B2.3..Contract.Revenue..funds.from.other.municipalities"
colnames(LibraryData$Year.2017)[19] <- "B2.3..Contract.Revenue..funds.from.other.municipalities"

## Between 2015 and 2016, a large portion of the questions were rewritten but not changed which is annoying.

## Question below was changed to add the word 'Specialized' which doesn't seem to have changed much of anything.

colnames(LibraryData$Year.2016)[108] <- "D1.1.3.1.C..Other.Professional.Staff"
colnames(LibraryData$Year.2017)[111] <- "D1.1.3.1.C..Other.Professional.Staff"

colnames(LibraryData$Year.2016)[116:117] <- c("D1.2.3.1.C..Other.Professional.Staff", "D1.2.3.1.H..Other.Professional.Staff")
colnames(LibraryData$Year.2017)[119:120] <- c("D1.2.3.1.C..Other.Professional.Staff", "D1.2.3.1.H..Other.Professional.Staff")

headersRawData$Year.2016 <- colnames(LibraryData$Year.2016)
headersRawData$Year.2017 <- colnames(LibraryData$Year.2017)

# headersRawData$Year.2016[20] ## Question was removed in 2017.

## With the adjustments above, we have a much cleaner dataset with far more consistency from year to year.  As it stands now there are three occasions where the dataset undergoes major changes: 1999 to 2000, 2010 to 2011, and 2015 to 2016.  These could be treated as four distinct datasets if we wanted, or we could do more cleaning to find comparable data between surveys.  However this doesn't sound appealing so I am going to quit here.

## If you are reading this it is because I have made the cleaned datasets publically available, if you have interest in doing so you may fork this repository and try to clean it up further.  I wish you the best of luck and I will consider any push requests you send back my way!

for (i in seq_along(1999:2018)) {
  write.csv(LibraryData[[i]], paste0("./cleanData/ontario_public_libraries_", (1999:2018)[i], ".csv"))
}
