#to load the csv files and table
setwd("C:\\Users\\aryan\\OneDrive\\Dokumen\\MGNREGA analysis")
rrr<-read.csv("employment under MGNREGA.csv")
sde<-read.csv("aryany0919_1787680406208268.csv")
sde12<-read.csv("aryany0919_17888775255010207.csv")


#tidying data to join the sustainable development goals index and employment under MGNREGA table
sde<-sde[-c(6,9),]
sde12<-sde12[-c(6,8),]
rrr$state<-str_to_title(rrr$state)
e<-full_join(rrr,sde,by=c("state"="State"))
e<-full_join(e,sde12,by=c("state"="State"))
View(e)

#to create an additional column percentage of population
e$percentage_of_population_issued_job<-(e$total_population_employed/e$total_population)*100


#to find out rsquared value, p value,f-statistic value
View(lst)
lst<-e[6:21]
rs<-data.frame(
  parameters<-character(),
  R_squared<-numeric(),
  p_value<-numeric(),
  F_statistic<-numeric()
)
for (i in colnames(lst)){
   regre1<-lm(e$percentage_of_population_issued_job~e[[i]],data=e)
  df<-summary(regre1)

rs<-rbind(rs, data.frame(
  parameters<-i,
  R_squared<-df$r.squared,
  p_value<-df$coefficients[2,4],
  F_statistic<-df$fstatistic[1]
))
}
View(rs)

#to save the dataset

write.csv(rs, "rs.csv",row.names = FALSE)

#to plot graph between percentage of population employed under MGNREGA vs indicators of sustainable development goal index

for (col in colnames(lst)){
  p<-ggplot(e,aes(x=e$percentage_of_population_issued_job,y=e[[col]]))+geom_point()+geom_smooth(aes(group=1),method="lm")+labs(title=col)
  print(p)
  ggsave(paste0(col,"__plot.png"), plot=p,width=6,height=4)
  
}


#to find correlation between SDG 9 , SDG 11 and SDG 12
lst3<-e[, c(14,16,17)]
rs3<-data.frame(
  parameters<-character(),
  R_squared<-numeric(),
  p_value<-numeric(),
  F_statistic<-numeric()
)
for (i in colnames(lst3)){
  for (j in colnames(lst3)){if (j != i){
  regre1<-lm(lst3[[j]]~lst3[[i]],data=lst3)
  df<-summary(regre1)
  
  rs3<-rbind(rs3, data.frame(
    parameters<-paste(j,i),
    R_squared<-df$r.squared,
    p_value<-df$coefficients[2,4],
    F_statistic<-df$fstatistic[1]
  ))
}
}
}
write.csv(rs3,"rs3.csv", row.names = FALSE)

View(rs3)
#plot population employed under mngrega v/s sustainable cities and communitites and industry innovation and infrastructure and responsible consumption and production
model1<-lm(e$percentage_of_population_issued_job~e$Sdg.India.Index..Responsible.Consumption.And.Production..UOM...Percentage....Scaling.Factor.1+e$Sdg.India.Index..Sustainable.Cities.And.Communities..UOM...Percentage....Scaling.Factor.1+e$Sdg.India.Index..Industry..Innovation.And.Infrastructure..UOM...Percentage....Scaling.Factor.1,data=e)
summary(model1)


#linear regression between percentage of population vs subindicator of responsible consumption and production

lst2<-e[24:30]
colnames(lst2)
rs1<-data.frame(
  parameters<-character(),
  R_squared<-numeric(),
  p_value<-numeric(),
  F_statistic<-numeric()
)
for (i in colnames(lst2)){
  regre2<-lm(e$percentage_of_population_issued_job~e[[i]],data=e)
  df2<-summary(regre2)
  
  rs1<-rbind(rs1, data.frame(
    parameters<-i,
    R_squared<-df2$r.squared,
    p_value<-df2$coefficients[2,4],
    F_statistic<-df2$fstatistic[1]
  ))
}
View(rs1)

#saving the dataset

write.csv(rs1,"rs1.csv", row.names = FALSE)


#to plot graph between percentage of population vs subindicator of responsible consumption and production

for (col in colnames(lst2)){
  p1<-ggplot(e,aes(x=e$percentage_of_population_issued_job,y=.data[[col]]))+geom_point()+geom_smooth(aes(group=1),method="lm")+labs(title=col)
  print(p1)
  ggsave(paste0(col,"__plot.png"), plot=p1,width=6,height=4)
  
}


#regression between population employed under MGNREGA and plastic waste generated, per capita fossil fuel generation and uses of nitrogenous fertilizer out of total N.P.K.
model2<-lm(e$percentage_of_population_issued_job~e$Plastic.Waste.Generated.Per.1.000.Population.Implementation.Of.Plastic.Waste.Management.Rules..2011.Census.Population.Projections..UOM.Tonne.t.per1000populationperannum...Scaling.Factor.1+e$Use.Of.Nitrogenous.Fertilizer.Out.Of.Total.N.P.K...Nitrogen..Phosphorous..Potassium.......UOM...Percentage....Scaling.Factor.1+e$Per.Capita.Fossil.Fuel.Consumption..UOM.KG.Kilogram....Scaling.Factor.1,data=e)
summary(model2)
