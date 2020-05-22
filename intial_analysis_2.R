df<-read.csv("/Users/kanwarpal/Downloads/EdmontonRealEstateDataU3.csv")
names(df)
df<-subset(df , select=- c(landuse_description,market_building_class,taxroll_number,building_name,effective_build_year,house_number,street_name,
                           postal_code,city,full_address,geometry,lat,lon))
summary(df$net_area)

#names(df) <- c("b_year","n_area","a_value","l_size","b_count","s_cover","t_area")


sum(is.na(df$net_area))
hist(df$net_area)

# numeric variables in data
summary(df$net_area)
sum(is.na(df$net_area))

# Fill NA with mean

df[sapply(df, is.numeric)] <- 
  lapply(df[sapply(df, is.numeric)], 
         function(x) 
                      ifelse(is.na(x), mean(x, na.rm = TRUE), x))
summary(df)


#For plotting histogram,density plots for different property types can be selected from following code,different 
#propert types can be AGRICULTURAL,INDUSTRIAL
dfnew1<-df[df$property_type=='RESIDENTIAL',]
#df<-subset(df,select=-property_type)




#Following is the Histogram and density plots for property type residential. 

par(mfrow=c(3,3))
{
  hist(dfnew1$net_area,geom = "histogram",main = "Net Area",col = "blue") 
  hist(dfnew1$site_coverage,geom = "histogram",main="Site coverage",col = "Blue") 
  hist(dfnew1$lot_size.1,geom = "histogram",main = "Lot Size",col = "Blue") 
  hist(dfnew1$building_count,geom = "histogram",main = "Building count",col = "Blue") 
  hist(dfnew1$effective_build_year_1,geom = "histogram",main = "Effective build year",col = "Blue")
  hist(dfnew1$tot_gross_area_description.1,geom = "histogram",main = "Total gross area",col = "Blue") 
}



