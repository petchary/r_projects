install.packages('googleway')
install.packages("writexl")
install.packages('geosphere')
library(googleway)
library(writexl)
library(dplyr)
library(geosphere)

myKpi = 'AIzaSyCzMc0FVwsEbSGEsBvTzQKY59UsnEN9fNE'

search_str <- google_places(search_string = 'Chun Shui Tang in Taiwan', 
              location=c(23.899504, 120.981795), 
              radius=50000, key=myKpi)
round2 <- google_places(search_string = 'Chun Shui Tang in Taiwan', 
                        location=c(23.899504, 120.981795), 
                        radius=50000, key=myKpi,
                        page_token = search_str$next_page_token)
round3 <- google_places(search_string = 'Chun Shui Tang in Taiwan', 
                        location=c(23.899504, 120.981795), 
                        radius=50000, key=myKpi,
                        page_token = round2$next_page_token)

result <- search_str$results
result2 <-round2$results
result3 <-round3$results


result <- search_str$results %>%
  cbind(., search_str$results$geometry$location)
result2 <- round2$results %>%
  cbind(., round2$results$geometry$location)
result3 <- round3$results %>%
  cbind(., round3$results$geometry$location)

df <- as.data.frame(result)
df2 <- as.data.frame(result2)
df3 <- as.data.frame(result3)

# Bind data frames together using dplyr's bind_rows function
final_df <- bind_rows(df, df2, df3)

# Specify the corrected file path with double backslashes or forward slashes
file_path <- "C:/Users/Phetcharee/Desktop/CSTProject/chun_shui_tang_branches.xlsx"

# Write final_df to Excel file
write_xlsx(final_df, path = file_path)

## Clustering Part
#Mean of Lat Lon
MeanLat <- mean(final_df$lat, na.rm = TRUE)
MeanLon <- mean(final_df$lng, na.rm = TRUE)

#Distance of all hotels from mean lat lon
CSTLatLon <- final_df[,c(17,18)]

#Distance Matrix for city  
Distance_Mat<- distm(CSTLatLon[2:1],CSTLatLon[2:1],
                     fun = distHaversine)
Distance_Mat<- as.data.frame(Distance_Mat)
Distance_Mat[is.na(Distance_Mat)]<-0
DMat<- as.dist(Distance_Mat)

#Hierarchical Clustering
hc <- hclust(DMat, method="complete")
final_df$Clusters <- cutree(hc, k = 4)
