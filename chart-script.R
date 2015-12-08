library(dplyr)
library(tidyr)
library(codemog)
library(ggplot2)
library(grid)
library(scales)


pph_co=readxl::read_excel("J:/Projections/Households/totalPphd1970to2050.xlsx")


p=pph_co%>%
  ggplot(aes(x=Year,y=PersonsPerHhd))+
  geom_line(color=rgb(31,74,126, max=255), size=1.5)+
  geom_vline(xintercept = 2015, color=rgb(191,32,38, max=255), size=1.1)+
  theme_codemog(base_size=15)+
  labs(x= "Year", y = "Persons Per Household", title= "Colorado Persons Per Household, 1970-2050\nSource: State Demography Office")
p

ggsave("personsPerHousehold_co.png", p, h=150, w=250, units="mm")


hh=readxl::read_excel("J:/Projections/Households/hhdata_transformV2014.xlsx")


hh_county=hh%>%
  rename(countyfips=area_code, year=Year, totalHouseholds=total_households)%>%
  group_by(countyfips, year)%>%
  summarize(totalHouseholds=sum(totalHouseholds))