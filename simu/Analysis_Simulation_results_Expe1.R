#############################
#
# Sensitivity analysis of 
# Schelling with urban grids
# Clémentine August 21, 2015
#
#############################

#path = "~/Documents/spacematters/simu/"
path = "~/Documents/clementine/other/spacematters/"




###################### Cut the initial simu files by gridID

# res = read.csv(paste(path, "results.csv", sep=""), sep=",", dec=".", header = F)
# tail(res)
# 
# colnames(res) = c("gridName", "seed", "step", "freeSpace", "similarWanted", 
#                   "population", "unsatisfied", "dissimilarity", "moran", 
#                   "entropy", "exposureRG", "isolation", "delta")
# 
# write.csv(res, paste(path, "results_with_colnames.csv", sep=""))

res = read.csv(paste(path, "results_with_colnames.csv", sep=""), sep=",", dec=".", header = T)

# res = res[-1,]
# head(res)
# colnames(res) = c("ID","gridName", "seed", "step", "freeSpace", "similarWanted", 
#                   "population", "unsatisfied", "dissimilarity", "moran", 
#                   "entropy", "exposureRG", "isolation", "delta")



grids = list(unique(res$gridName))[[1]]
str(grids)

for (i in grids){
  results = subset(res, gridName == i)
  assign(paste("res_", i, sep=""), results)
  write.csv(results, paste(path, "res_", i, sep=""))
}



###################### Analyse Simu by grid

gridID = "gridAnas_4.csv"
df = read.csv(paste(path, "res_", gridID, sep=""), sep=",", dec=".", header = T, stringsAsFactors=F)

ttt = read.csv(paste(path, "res_", gridID, sep=""), sep=",", dec=".", header = T, stringsAsFactors=T)
summary(ttt)

ttt$freeSpace = as.numeric(ttt$freeSpace)

str(df)
summary(df)

plot(freeSpace ~ similarWanted, data = df)


ggplot(df, aes(x = similarWanted, y = freeSpace)) + geom_point(aes(fill = isolation, color = isolation)) + 
  scale_fill_gradient(low="red", high="blue") +  scale_color_gradient(low="red", high="blue")


