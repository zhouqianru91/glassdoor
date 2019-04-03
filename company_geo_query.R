# This script is to get the geological coordinates for the companies

library(ggmap)
library(plyr)

load("ca_jobs.rds")

myKey <- 'AIzaSyBjvWa8VgYGk6HBsvQJqd4LvAxAzXORBJg'
register_google(key = myKey, account_type = "standard", day_limit = 100000)


# for each company, get all the address, lagtitude and longitude in the form of dataframe
get_coord <- function(company){
  
  # for each company passed, get df containing lat, long, long address and status
  geo_reply = geocode(company, output='all', messaging=TRUE, override_limit=TRUE)
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  if (geo_reply$status != "OK"){
    return(NULL)
  }
  
  n_result <- length(geo_reply$results)
  answer <- data.frame(matrix(NA, nrow=n_result, ncol=4))
  colnames(answer) <- c("company", "lat", "long", "formatted_address")
  answer$company <- company
  
  for (i in 1:n_result){
    answer$lat[i] <- geo_reply$results[[i]]$geometry$location$lat
    answer$long[i] <- geo_reply$results[[i]]$geometry$location$lng 
    answer$formatted_address[i] <- geo_reply$results[[i]]$formatted_address
  }
  return(answer)
}

# apply this function to all address, merge into one single dataframe and save result as rds
geo_companies <- lapply(ca_jobs$company, get_coord)
geo_companies_df <- ldply(geo_companies, data.frame)
saveRDS(geo_companies_df, file = "company_geo_coordinates.rds")


