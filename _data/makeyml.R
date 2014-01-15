# R files

readr = list.files("../r/")
writer = "r.yml"

myfile = readr[1]
cat("- r: ", myfile, "\n", sep='', file= writer)

for(i in 2:length(readr)) {
	myfile = readr[i]
	cat("- r: ", myfile, "\n", sep='', file= writer, append=TRUE)
}


# Data files

readdat = list.files("../data/")
writedat = "data.yml"

myfile = readdat[1]
cat("- data: ", myfile, "\n", sep='', file= writedat)

for(i in 2:length(readdat)) {
	myfile = readdat[i]
	cat("- data: ", myfile, "\n", sep='', file= writedat, append=TRUE)
}


readexamples = list.files("../files/examples/")
writeexamples = "examples.yml"

myfile = readexamples[1]
cat("- example: ", myfile, "\n", sep='', file= writeexamples)

if(length(readexamples) > 1) {
	for(i in 2:length(readexamples)) {
		myfile = readexamples[i]
		cat("- example: ", myfile, "\n", sep='', file= writeexamples, append=TRUE)
	}
}
