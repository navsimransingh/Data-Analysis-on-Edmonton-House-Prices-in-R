library(e1071)
library(caret)
library(corrplot)



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




#Hsitogram for the whole dataset

par(mfrow=c(3,3))
{
  hist(df$net_area,geom = "histogram",main = "Net Area",col = "blue") 
  hist(df$site_coverage,geom = "histogram",main="Site coverage",col = "Blue") 
  hist(df$lot_size.1,geom = "histogram",main = "Lot Size",col = "Blue") 
  hist(df$building_count,geom = "histogram",main = "Building count",col = "Blue") 
  hist(df$effective_build_year_1,geom = "histogram",main = "Effective build year",col = "Blue")
  hist(df$tot_gross_area_description.1,geom = "histogram",main = "Total gross area",col = "Blue") 
}


#Density plot for whole dataset

par(mfrow=c(3,3))
{
  # Filled Density Plot
  d <- density(df$net_area)
  plot(d, main="Density of Net Area")
  polygon(d, col="blue", border="red") 
  
  e <- density(df$site_coverage)
  plot(e, main="Density of Site coverage")
  polygon(e, col="blue", border="red") 
  
  f <- density(df$lot_size.1)
  plot(f, main="Density of Lot size")
  polygon(f, col="blue", border="red") 
  
  g <- density(df$building_count)
  plot(g, main="Density of Building count")
  polygon(g, col="blue", border="red") 
  
  h <- density(df$effective_build_year_1)
  plot(h, main="Density of Effective building year")
  polygon(h, col="blue", border="red") 
  
  d <- density(df$tot_gross_area_description.1)
  plot(d, main="Density of Total gross area")
  polygon(d, col="blue", border="red") 
  
  
  
} 

#selecting numeric values from data for corelation plot
names(df) <- c("b_year","n_area","a_value","l_size","b_count","s_cover","t_area")
fac <- sapply(df,is.factor)
nums <- sapply(df, is.numeric)
dfnew <- df[ , nums]
cor_df_complete <-cor(dfnew)
corrplot(cor_df_complete, method = "color")

#Co-relation plot for the whole dataset

library(RColorBrewer)
corrplot(cor_df_complete, method = "color", outline = T, addgrid.col = "darkgray", order="hclust", addrect = 4, rect.col = "black", rect.lwd = 5,cl.pos = "b", tl.col = "indianred4", tl.cex = 1.5, cl.cex = 1.5, addCoef.col = "white", number.digits = 2, number.cex = 0.75, col = colorRampPalette(c("darkred","white","midnightblue"))(100))





