Mon <-
  as.character(read.csv('Mon.txt', header = F, stringsAsFactors = F)[1,])
devtools::use_data(Mon, overwrite =T)


Month <-
  as.character(read.csv('Month.txt', header = F, stringsAsFactors = F)[1,])
devtools::use_data(Month, overwrite =T)