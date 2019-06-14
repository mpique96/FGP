FC_all <- read.delim('FC_target_metabolite2.csv', sep='\t')
HC_all <- read.delim('HC_target_metabolite2.csv', sep='\t')
drug_all <- read.csv('drug_target_met.csv', sep=',', header=TRUE)


ggplot() + geom_point(data=HC_all, aes(Affinity.metabolite, Affinity.HC, color='HC'), alpha=0.5) + 
  geom_point(data=drug_all, aes(Affinity.metabolite, Affinity.drug, color='drug'), alpha=0.5) +
  geom_point(data=FC_all, aes(Affinity.metabolite, Affinity.FC, color='FC'), alpha=0.5) +
  labs(x='Metabolite affinity', y='Drug/FC/HC affinity') +
  geom_abline() + theme_bw() + theme(legend.position = "bottom")


FC_all[,11] <- FC_all[,8] - FC_all[,10]
HC_all[,12] <- HC_all[,9] - HC_all[,11]
drug_all[,13] <- drug_all[,8] - drug_all[,11]

## PERCENTAGES
length(FC_all[FC_all$V11>2,1])/length(FC_all[,1])*100  # 2 orders above threshold percentage - HC
length(FC_all[FC_all$V11>0,1])/length(FC_all[,1])*100  # Above threshold percentage - HC
length(FC_all[FC_all$V11>-2,1])/length(FC_all[,1])*100  # 2 orders below threshold percentage - HC

length(HC_all[HC_all$V12>2,1])/length(HC_all[,1])*100  # 2 orders above threshold percentage - HC
length(HC_all[HC_all$V12>0,1])/length(HC_all[,1])*100  # Above threshold percentage - HC
length(HC_all[HC_all$V12>-2,1])/length(HC_all[,1])*100  # 2 orders below threshold percentage - HC

length(drug_all[drug_all$V13>2,1])/length(drug_all[,1])*100  # 2 orders above threshold percentage - drug
length(drug_all[drug_all$V13>0,1])/length(drug_all[,1])*100  # Above threshold percentage - drug
length(drug_all[drug_all$V13>-2,1])/length(drug_all[,1])*100  # 2 orders below threshold percentage - drug


ggplot() + 
  #geom_density(data=df_food2, aes(Affinity.compound, color='Food compounds')) + 
  #geom_vline(data=df_food2, aes(xintercept = mean(Affinity.compound), color='Food compounds'), linetype='dashed') +
  geom_density(data=drug_all, aes(Affinity.drug, color='Drugs')) +
  geom_density(data=HC_all, aes(Affinity.HC, color='Herbal compounds')) +
  geom_density(data=drug_all, aes(Affinity.metabolite, color='Metabolites')) +
  geom_vline(data=drug_all, aes(xintercept = mean(Affinity.drug), color='Drugs'), linetype='dashed') +
  geom_vline(data=HC_all, aes(xintercept = mean(Affinity.HC), color='Herbal compounds'), linetype='dashed') +
  geom_vline(data=drug_all, aes(xintercept = mean(Affinity.metabolite), color='Metabolites'), linetype='dashed') +
  labs(x = 'Affinity')


##############
