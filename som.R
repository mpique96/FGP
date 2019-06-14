#install.packages('kohonen')
library(kohonen)
library(ggplot2)


# NC - NC
SM_NC <- read.delim('SM_NC.csv', sep=',')
SM_NC <- as.matrix(SM_NC)
SM_NC.vec <- SM_NC[upper.tri(SM_NC)]

# Drug - Drug
SM_drug <- read.delim('SM_drug.csv', sep=',')
SM_drug <- as.matrix(SM_drug)
SM_drug.vec <- SM_drug[upper.tri(SM_drug)]

# Met - Met
SM_met <- read.delim('SM_met.csv', sep=',')
SM_met <- as.matrix(SM_met)
SM_met.vec <- SM_met[upper.tri(SM_met)]

# Drug - NC
SM_drug_NC <- read.delim('SM_drug_NC.csv', sep=',')
SM_drug_NC <- as.matrix(SM_drug_NC)
SM_drug_NC.vec <- SM_drug_NC[upper.tri(SM_drug_NC)]

# NC - Met
SM_NC_met <- read.delim('SM_NC_met.csv', sep=',')
SM_NC_met <- as.matrix(SM_NC_met)
SM_NC_met.vec <- SM_NC_met[upper.tri(SM_NC_met)]

# Drug - Met
SM_drug_met <- read.delim('SM_drug_met.csv', sep=',')
SM_drug_met <- as.matrix(SM_drug_met)
SM_drug_met.vec <- SM_drug_met[upper.tri(SM_drug_met)]

ggplot() + 
  geom_density(aes(SM_NC.vec, color = 'NC-NC')) + 
  geom_density(aes(SM_drug.vec, color='Drug-Drug')) +
  geom_density(aes(SM_met.vec, color='Met-Met')) +
  geom_density(aes(SM_drug_NC.vec, color='Drug-NC')) +
  geom_density(aes(SM_NC_met.vec, color='NC-Met')) +
  geom_density(aes(SM_drug_met.vec, color='Drug-Met')) +
  xlab('Similarity')
  


som.SM_NC <- som(scale(SM_NC), grid = somgrid(10,7, "hexagonal"), rlen=3000)
summary(som.SM_NC)
plot(som.SM_NC, type='changes')
plot(som.SM_NC, type = "dist.neighbours", labels = seq(0,73,1), 
     shape='straight', palette.name=rainbow, heatkey = FALSE, main="")
par(new=TRUE)
plot(som.SM_NC, type = "mapping", labels = seq(0,73,1), 
     shape='straight', palette.name=rainbow, main="")

clust.SM_HC <- cutree(hclust(object.distances(som.SM_NC, "codes")), 5)
add.cluster.boundaries(som.SM_NC, clust.SM_HC)

som.SM_NC$unit.classif


som.SM_drug <- som(scale(SM_drug), grid = somgrid(19, 15, "hexagonal"), rlen=3000)
summary(som.SM_drug)
plot(som.SM_drug, type='changes')
plot(som.SM_drug, type = "dist.neighbours", labels = seq(0,285,1), 
     shape='straight', palette.name=rainbow, heatkey=FALSE, main="")
par(new=TRUE)
plot(som.SM_drug, type = "mapping", labels = seq(0,285,1), 
     shape='straight', palette.name=rainbow, main="")


clust.SM_drug <- cutree(hclust(object.distances(som.SM_drug, "codes")), 8)
add.cluster.boundaries(som.SM_drug, clust.SM_drug)



SM_all <- read.delim('SM_all.csv', sep=',')
SM_all <- as.matrix(SM_all)
som.SM_all <- som(scale(SM_all), grid = somgrid(10,10,'hexagonal'), rlen=6000)
summary(som.SM_all)
plot(som.SM_all, type='changes')

plot(som.SM_all, type = 'dist.neighbours', labels = c(rep(1,285), rep(2,60), rep(3,21)), 
     palette.name=rainbow, shape='straight', heatkey = FALSE, main="")
par(new=TRUE)
plot(som.SM_all, type = "mapping", labels = seq(0,365,1), 
     shape='straight', palette.name=rainbow, main="")

clust.SM_all <- cutree(hclust(object.distances(som.SM_all, "codes")), 10)
add.cluster.boundaries(som.SM_all, clust.SM_all)

#disSM_all <- 1-SM_all

SM_all[1,]
scale(SM_all)[1,]

a <- c()
for (i in som.SM_all$unit.classif) {
  a <- c(a, clust.SM_all[i])
}

clust_mol <- data.frame(a,seq(0,365,1))


#library(cluster)
si <- silhouette(a,dmatrix=disSM_all)
summary(si)[1]
#plot(si)

for(i in 3:15){
  clust.SM_all <- cutree(hclust(object.distances(som.SM_all, "codes")), i)
  a <- c()
  for (i in som.SM_all$unit.classif){
    a <- c(a, clust.SM_all[i])
  }
  clust_mol <- data.frame(a,seq(0,365,1))
  si <- silhouette(a,dmatrix=disSM_all)
  print(summary(si)[1])
  plot(si)
}
plot(si)
