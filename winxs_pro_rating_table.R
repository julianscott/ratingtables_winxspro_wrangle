library(tidyverse)
# Read in XS data that includes station and elevation
# Note whether station = 0 is at LB or RB. WinXS Pro (as of 081919) requires sation = 0 at LB as first row.
# downstream boundary condition XS. Profile taken from ArcGIS
# FIRST_DIST = XS station,FIRST_Z = elevation. XS station = 0 is at right bank. 
dsbc <- read.csv("E:\\_DoD\\_Camp_Pendleton_Survey\\GIS Data\\GIS Data\\Dwn_LIDAR_XS.csv")

# Create a XS station column where 0 = left bank and order rows by this column
tmp <- dsbc %>%
  select(FIRST_DIST,FIRST_Z) %>%
  rename(D_RB = FIRST_DIST,Z = FIRST_Z) %>%
  mutate(D_LB = (D_RB - max(D_RB))*-1) %>%
  select(D_LB,Z) %>%
  arrange(D_LB)
head(tmp)

# reduce number of rows to ~1/4. This may help WinXS Pro complete solution. Tailor to appropriate.
tmpwrite <- select(tmp,D_LB,Z) %>%
  filter(row_number()%%2==0) %>%
  filter(row_number()%%2==0) 

# write file to tab delimited, *.sec text file
write.table(tmpwrite,
            "E:\\_DoD\\_Camp_Pendleton_Survey\\WinXS_Pro_work\\test_081919.sec",
            sep = "\t",
            col.names = F,
            row.names = F)

tmpwrite %>%
  ggplot()+
  geom_point(aes(x=D_LB,y=Z)) 
  # geom_hline(yintercept = 0.6)+
  xlim(50,100)
  # ylim(0,10)
# coord_cartesian(xlim = c(-100,-50))
# ggplotly()

# 570 cms has stage of ...
# stage_570 = abs(...-min(tmp$Z))
# stage_570 = ...
# c(seq(0.5,stage_570,0.25),stage_570)

# Get slope affecting XS from long profiles and GIS analysis. This is required for WinXS Pro. 
# I used 0.0068 m/m.

# After WinXS pro is run, the output file created needs some simple formating in excel to 
# to make it easily read into R. Open the *.out file, observe that two or more columns are 
# placed below the data in columns A, B, C, etc. Move these columns up to be in line with the rest of the data
# Save as csv to use this code
dwn_rt <- read.csv("E:\\_DoD\\_Camp_Pendleton_Survey\\WinXS_Pro_work\\dwn_LIDAR_rc.csv") 
head(dwn_rt)

# Plot downstream rating table
dwn_rt %>%
  ggplot(aes(x = Q_cms,y = STAGE_m)) +
           geom_point()

# use approx to interpolate stages from rating table.
q = 300
h = approx(x=dwn_rt$Q_cms,y=dwn_rt$STAGE_m,xout = q)$y
h


