# My code for interesting discoveries
library(tidyverse)

load("allStates16.RData")
info = x16
colnames(info)

# I considered MA for my sample
ma = filter(info,STATE_CODE_001 == 25)
# Adding ratings of bridges to the data frame
ma = ma %>% mutate(ratings = pmin(DECK_COND_058 , SUPERSTRUCTURE_COND_059, 
                                 SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062,na.rm = T))

# Plot 1
ggplot(data = ma) + geom_point(mapping = aes(x = YEAR_BUILT_027, y = ADT_029, color = ratings)) + 
  ggtitle("Scatter Plot of Bridges' Year Built and Usage with Ratings")

# Modify code for longitude and latitude
tran.lat = function(x) {
  t = as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}

tran.long = function(x) {
  t = as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05
  x1 = x[t < 18]
  t1 = as.numeric(substr(x1,1,3)) + as.numeric(substr(x1,4,9))/6e+05
  t[t < 18] = t1
  return(t)
}

ma$LAT_016 = tran.lat(ma$LAT_016)
ma$LONG_017 = tran.long(ma$LONG_017)

# Plot 2
ggplot(data = ma) + geom_point(mapping = aes(y = LAT_016, x = -LONG_017, color = SERVICE_LEVEL_005C)) +
  ggtitle("Types of Bridges in MA")
