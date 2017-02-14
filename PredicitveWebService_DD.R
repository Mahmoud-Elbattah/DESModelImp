library("RCurl")
library("rjson")

generatedPatients<- read.csv("Experiments.csv")


# Accept SSL certificates issued by public Certificate Authorities
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

h = basicTextGatherer()
hdr = basicHeaderGatherer()
predictions <-  data.frame()
StepSize <-1000
for( i in seq(1,nrow(generatedPatients), by=StepSize)){
partition<- list(0)
counter <- i
for(j in 1:StepSize)
{
  partition[[j]]<-list( generatedPatients[counter,"Hosp"],
                        "0",
                   generatedPatients[counter,"Sex"], 
                   generatedPatients[counter,"Resid"], 
                   generatedPatients[counter,"AGE"], 
                   generatedPatients[counter,"Diag1"], 
                   generatedPatients[counter,"Fracture_Type"],
                   generatedPatients[counter,"Fragility"],
                   generatedPatients[counter,"PredictedLOS"], 
                   generatedPatients[counter,"TimeToSurgery"] 
                   )
  counter<- counter+1
}

req = list(
  
  Inputs = list(
 
    "input1" = list(
      "ColumnNames" = list("Hosp", "Discharge_Destination", "Sex", "Resid", "AGE", "Diag1", "Fracture_Type", "Fragility", "ActualLOS", "TimeToSurgery"),
      "Values" = partition
    )                ),
  GlobalParameters = setNames(fromJSON('{}'), character(0))
)

body = enc2utf8(toJSON(req))
api_key = "UMEu6Vm49qqrRYqff7DavjJC1Tq1uzb3CVbuoijXLhwxDwr733P5Tr9dD3hPGR7nEkHv7QHSQX/PzHTZ3+j8gw==" # Replace this with the API key for the web service
authz_hdr = paste('Bearer', api_key, sep=' ')

h$reset()
curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/ee457a75262244d4b299080febd209db/services/178d190112844a73931afaa84aa1294e/execute?api-version=2.0&details=true",
            httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
            postfields=body,
            writefunction = h$update,
            headerfunction = hdr$update,
            verbose = FALSE
)

headers = hdr$value()
httpStatus = headers["status"]
if (httpStatus >= 400)
{
  print(paste("The request failed with status code:", httpStatus, sep=" "))
  
  # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
  print(headers)
}

#print("Result:")
result = h$value()
#print(fromJSON(result))


##########
counter <- i
for(j in 1:StepSize)
{
  predictedDD <- fromJSON(result)$Results$output1$value$Values[[j]][11]
  #print(predictedDD)
  predictions[counter,"PredictedDD"]<-predictedDD
  counter <- counter+1
}

print(paste("#",i))
}
write.csv(predictions,"E:\\Predictions2014.csv")
print("Done")

