library(dplyr)
library(tidyr)
library(codemog)
library(ggplot2)
library(grid)
library(scales)


pph_co=readxl::read_excel("totalPphd1970to2050.xlsx")


p=pph_co%>%
  ggplot(aes(x=Year,y=PersonsPerHhd))+
  geom_line(color=rgb(31,74,126, max=255), size=1.5)+
  geom_vline(xintercept = 2015, color=rgb(191,32,38, max=255), size=1.1)+
  theme_codemog(base_size=15)+
  labs(x= "Year", y = "Persons Per Household", title= "Colorado Persons Per Household, 1970-2050\nSource: State Demography Office")
p

ggsave("personsPerHousehold_co.png", p, h=150, w=250, units="mm")


hh=readxl::read_excel("hhdata_transformV2014.xlsx")


hh_county=hh%>%
  rename(countyfips=area_code, year=Year, totalHouseholds=total_households)%>%
  group_by(countyfips, year)%>%
  summarize(totalHouseholds=sum(totalHouseholds))

yrs=c(2010, 2015, 2020, 2025, 2030)

hh_age=hh%>%
  rename(countyfips=area_code, year=Year, totalHouseholds=total_households)%>%
  filter(year %in% yrs)%>%
  group_by( year, age_group_id)%>%
  summarize(totalHouseholds=sum(totalHouseholds))%>%
  ungroup()%>%
  mutate(age_cat=ordered(age_group_id, levels = 1:4,
                         labels = c("18 to 24", "25 to 44", "45 to 64", "65 and over" )))%>%
  group_by(year)%>%
  mutate(share=totalHouseholds/sum(totalHouseholds))

hh_age_test=hh%>%
  rename(countyfips=area_code, year=Year, totalHouseholds=total_households)%>%
  filter(year==2030)%>%
  group_by(age_group_id)%>%
  summarize(totalHouseholds=sum(totalHouseholds))%>%
  mutate(share=totalHouseholds/sum(totalHouseholds))

codemog_pal=c(rgb(31,73,125, max=255),
              rgb(192,80,77, max=255),
              rgb(101, 80, 60, max=255),
              rgb(239, 117, 33, max=255),
              rgb(119, 171, 67, max = 255),
              rgb(208, 210, 211, max = 255),
              rgb(210, 210, 210, max = 255))

p2=hh_age%>%
  ggplot(aes(x=year, y=share, color=as.factor(age_cat)))+
  geom_line(size=1.25)+
  geom_point(size=2.25)+
  scale_y_continuous(labels=percent)+
  scale_colour_manual(name="",values=codemog_pal)+
  theme_codemog(base_size = 15)+
  labs(x="Year", y="Share of Households", title= "Colorado Household Projections by Age\nSource: State Demography Office")
p2

ggsave("households_age.png", p2, h=150, w=250, units="mm")
