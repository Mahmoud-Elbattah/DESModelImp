library(plyr)

data<- read.csv("E:2015.csv")

#Elderly males/females aged 50+
Y2013<- c(638249,688277,"2013")
Y2014<- c(654743,705956,"2014")
Y2015<- c(671958,724413,"2015")
popInfo<- data.frame(rbind(Y2013,Y2014,Y2015), stringsAsFactors = FALSE)
colnames(popInfo)<- c("Male","Female","Year")

maleArrivalRate <- 140/100000
femaleArrivalRate <-407/100000
###

sampleVal = function(n,columnName) { 
  dataSummary <- count(df = data, vars = columnName)# a plyr function to count frequency of column values
  dataSummary$probability <- dataSummary$freq/ nrow(data)
  sample(x = dataSummary[, columnName], n, replace = TRUE, prob = dataSummary$probability)
  }



GeneratePatients= function(patientSex,year,experimentNo) { 
  nPatients<-0
  if(patientSex=="Male"){
    elderlyPopulation<-popInfo[popInfo$Year ==year,"Male" ]
    nPatients<- round(maleArrivalRate * as.numeric(elderlyPopulation), 0)
  }
  else if(patientSex=="Female"){
    elderlyPopulation<-popInfo[popInfo$Year ==year,"Female" ]
    nPatients<- round(maleArrivalRate * as.numeric(elderlyPopulation), 0)
  }
    
  data.frame(Hosp=sampleVal(nPatients,"Hosp"),
             Sex=patientSex,
             Resid=sampleVal(nPatients,"Resid"),
             AGE=sampleVal(nPatients,"AGE"),
             Diag1=sampleVal(nPatients,"Diag1"),
             Fracture_Type=sampleVal(nPatients,"Fracture_Type"),
             Fragility=sampleVal(nPatients,"Fragility"),
             TimeToSurgery=sampleVal(nPatients,"TimeToSurgery"),
             TimeToAdmission=sampleVal(nPatients,"TimeToAdmission"),
             Year=year,
             Experiment=experimentNo
             )
}




RunSimulation= function(n) {
  experiments <-  data.frame()
  for(n in 1:n) #Running simulation experiments
  {
    malePatients2013<- GeneratePatients("Male","2013",n)
    femalePatients2013<-GeneratePatients("Female","2013",n)
    ###
    malePatients2014<- GeneratePatients("Male","2014",n)
    femalePatients2014<-GeneratePatients("Female","2014",n)
    ###
    malePatients2015<- GeneratePatients("Male","2015",n)
    femalePatients2015<-GeneratePatients("Female","2015",n)
   
    experiments<- rbind(experiments,
                        malePatients2013,
                        femalePatients2013,
                        malePatients2014,
                        femalePatients2014,
                        malePatients2015,
                        femalePatients2015
                        )
  }
  write.csv(experiments,"E:\\Experiments2015.csv")
}

RunSimulation(50)
