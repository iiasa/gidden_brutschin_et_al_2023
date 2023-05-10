####################
#Figures 5-7
###################

library(readxl)
library(tidyverse)
library(here)
library(vroom)
library(hrbrthemes)
library(zoo)
library(cowplot)


genie_all<-vroom(here("data","gidden_brutshin_et_al_2023_data.csv")) 
meta<-vroom(here("data","gidden_brutshin_et_al_2023_meta.csv")) 


#restructure key data sets

indicators=c(
  "Emissions|CO2",
  "Emissions|CO2|Supply|Electricity",
  "Emissions|Kyoto Gases",
  "Emissions|F-Gases",
  "Emissions|CH4",
  "Emissions|N2O",
  "GDP|PPP",
  "Population",
  "Land Cover",
  "Land Cover|Forest",
  "Land Cover|Cropland",
  "Land Cover|Pasture",
  "Carbon Sequestration|Land Use",
  "Secondary Energy|Electricity",
  "Secondary Energy|Electricity|Non-Biomass Renewables", 
  "Carbon Sequestration|Direct Air Capture",
  "Carbon Sequestration|CCS", 
  "Carbon Sequestration|CCS|Biomass", 
  "Carbon Sequestration|CCS|Fossil",
  "GDP|PPP",
  "Population",
  "Investment|Energy Supply", 
  "Investment|Energy Supply|Electricity",
  "Investment|Energy Supply|Electricity|Solar",
  "Investment|Energy Supply|Electricity|Wind",
  "Investment|Energy Supply|Electricity|Nuclear",
  "Investment|Energy Supply|Electricity|Transmission and Distribution",
  "Investment|Energy Supply|Electricity|Biomass|w/ CCS",
  "Investment|Energy Supply|Electricity|Coal|w/ CCS",
  "Capacity|Electricity|Coal",
  "Secondary Energy|Electricity|Coal",
  "Capacity|Electricity|Gas",
  "Capacity|Electricity|Wind",
  "Capacity|Electricity|Solar",
  "Capacity|Electricity|Biomass",
  "Secondary Energy|Electricity|Gas",
  "Primary Energy|Biomass",
  "Primary Energy|Oil",
  "Price|Primary Energy|Oil",
  "Price|Carbon",
  "Price|Secondary Energy|Electricity",
  "Secondary Energy",
  "Secondary Energy|Electricity",
  "Secondary Energy|Electricity|Oil",
  "Secondary Energy|Electricity|Nuclear",
  "Secondary Energy|Electricity|Wind",
  "Secondary Energy|Electricity|Solar",
  "Secondary Energy|Electricity|Hydro",
  "Secondary Energy|Electricity|Biomass",
  "Secondary Energy|Electricity|Gas",
  "Secondary Energy|Electricity|Biomass|w/ CCS",
  "Secondary Energy|Electricity|Coal|w/ CCS",
  "Secondary Energy|Electricity|Gas|w/ CCS",
  "Secondary Energy|Electricity|Gas|w/o CCS",
  "Secondary Energy|Electricity|Oil|w/ CCS",
  "Final Energy|Hydrogen",
  "Secondary Energy|Hydrogen",
  "Primary Energy|Gas|Electricity",
  "Final Energy|Transportation",
  "Final Energy|Transportation|Electricity",
  "Final Energy|Transportation|Liquids|Bioenergy",
  "Land Cover",
  "Land Cover|Forest",
  "Land Cover|Cropland",
  "Land Cover|Pasture",
  "Carbon Sequestration|Land Use",
  "Final Energy",
  "Final Energy|Electricity",
  "Final Energy|Residential and Commercial",
  "Final Energy|Industry",
  "Food Demand",
  "Food Demand|Livestock",
  "Final Energy|Industry|Electricity",
  "Final Energy|Residential and Commercial|Electricity",
  "Price|Agriculture|Livestock|Index",
  "Price|Agriculture|Corn|Index",
  "Emissions|NOx", 
  "Carbon Sequestration|CCS|Biomass",
  "Carbon Sequestration|CCS|Fossil",
  "Primary Energy",
  "Secondary Energy|Hydrogen|Biomass",
  "Secondary Energy|Hydrogen|Biomass|w/ CCS",
  "Secondary Energy|Hydrogen|Biomass|w/o CCS",
  "Secondary Energy|Hydrogen|Coal",
  "Secondary Energy|Hydrogen|Coal|w/ CCS",
  "Secondary Energy|Hydrogen|Coal|w/o CCS",
  "Secondary Energy|Hydrogen|Electricity",
  "Secondary Energy|Hydrogen|Fossil",
  "Secondary Energy|Hydrogen|Fossil|w/ CCS", 
  "Secondary Energy|Hydrogen|Fossil|w/o CCS",
  "Secondary Energy|Hydrogen|Gas",
  "Secondary Energy|Hydrogen|Gas|w/ CCS",
  "Secondary Energy|Hydrogen|Gas|w/o CCS",
  "Trade|Secondary Energy|Hydrogen|Volume", 
  "Primary Energy|Biomass|w/ CCS",
  "Primary Energy|Biomass|w/o CCS",
  "Primary Energy|Gas|w/ CCS",
  "Primary Energy|Gas|w/o CCS",
  "Primary Energy|Solar",
  "Primary Energy|Wind",
  "Capacity|Electricity|Storage", 
  "Secondary Energy|Heat|Gas" 
  
)  

years<-c("2020", "2025","2030","2035","2040", "2045","2050", "2055","2060", "2070", "2080", "2090", "2100")




scenario_set<-genie_all %>%
  select(-Unit) %>%
  filter(Variable %in% indicators)%>%
  mutate(Variable=str_replace_all(Variable,"[|/ ]", "_")) %>%
  mutate(Variable=str_to_lower(Variable, locale = "en")) %>%
  pivot_longer(-c(Model,Scenario, Region, Variable), names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(id_cols=c(Model, Scenario,Region, Year), names_from = Variable, values_from = Value)%>%
  unite("Scenario_ID", Model:Scenario, remove = FALSE) %>%
  filter(Year %in% years) %>%
  mutate(Year=as.numeric(Year)) %>%
  right_join(meta)


scenario_final<-scenario_set %>%
  mutate(temperature2 = case_when( 
    Temperature == "1.5C" ~ "C1 - 1.5°C low/no overshoot", 
    Temperature == "1.5C - OS" ~ "C2 - 1.5°C high overshoot",
    Temperature == "2C" ~ "C3 - Below 2°C>67%")) %>%
  mutate(scenario=case_when(
    `Short Name`=="C1 without DAC"~"No DACCS/Immediate Global Action",
    `Short Name`=="C1 with DAC"~"DACCS/Immediate Global Action",
    `Short Name`=="C2 without DAC"~"No DACCS/Immediate Global Action",
    `Short Name`=="C2 with DAC"~"DACCS/Immediate Global Action",
    `Short Name`=="C2 with DAC and Governance SSP1"~"DACCS/Governance Constrained Action",
    `Short Name`=="C3 without DAC"~"No DACCS/Immediate Global Action",
    `Short Name`=="C3 with DAC"~"DACCS/Immediate Global Action",
    `Short Name`=="C3 with DAC and Governance SSP2"~"DACCS/Governance Constrained Action",
    `Short Name`=="C3 with Governance SSP2"~"No DACCS/Governance Constrained Action"))


scenarios_key<-c("C1 without DAC", "C1 with DAC","C2 without DAC", "C2 with DAC", 
                 "C2 with DAC and Governance SSP1",
                 "C3 without DAC",
                 "C3 with DAC",
                 "C3 with DAC and Governance SSP2", "C3 with Governance SSP2")

line_types<-c("No DACCS/Immediate Global Action"="solid","DACCS/Immediate Global Action"="dashed", "No DACCS/Governance Constrained Action"="dotdash", "DACCS/Governance Constrained Action"="dotted")
color_lines<-c("No DACCS/Immediate Global Action"="black","DACCS/Immediate Global Action"="brown4", "No DACCS/Governance Constrained Action"="blue", "DACCS/Governance Constrained Action"="green4")

col_fill<-c("2030"="black","2035"="grey")
col_fill2<-c("2025"="black","2030"="grey")

##################################
#AR6 data preparation


genie_all<-vroom(here("data","AR6_Scenarios_Database_World_v1.0.csv")) 
meta2<-vroom(here("data","AR6_Scenarios_Database_metadata_indicators_v1.1.csv"))%>%
  select(Model, Scenario, Category)


ar6_set<-ar6 %>%
  select(-Unit) %>%
  filter(Variable %in% indicators)%>%
  mutate(Variable=str_replace_all(Variable,"[|/ ]", "_")) %>%
  mutate(Variable=str_to_lower(Variable, locale = "en")) %>%
  pivot_longer(-c(Model,Scenario, Region, Variable), names_to = "Year", values_to = "Value" ) %>%
  pivot_wider(id_cols=c(Model, Scenario,Region, Year), names_from = Variable, values_from = Value)%>%
  unite("Scenario_ID", Model:Scenario, remove = FALSE) %>%
  filter(Year %in% years) %>%
  mutate(Year=as.numeric(Year)) %>%
  left_join(meta2) %>%
  mutate(temperature2 = case_when( 
    Category == "C1" ~ "C1 - 1.5°C low/no overshoot", 
    Category == "C2" ~ "C2 - 1.5°C high overshoot",
    Category == "C3" ~ "C3 - Below 2°C>67%"))


###############################
#Figure 5A
###############################

biomass1<-scenario_final %>%
  filter(Region=="World", Year<2055) %>%
  filter(`Short Name` %in% scenarios_key) %>%
  select(Scenario_ID, scenario, temperature2, Year, primary_energy_biomass) %>%
  drop_na() %>%
  mutate(scenario=factor(scenario, levels=c("No DACCS/Immediate Global Action", "DACCS/Immediate Global Action", "No DACCS/Governance Constrained Action", "DACCS/Governance Constrained Action")))

biomass2<-ar6_set %>%
  filter(Region=="World", Year<2055) %>%
  select(Scenario_ID, temperature2, Year, primary_energy_biomass) %>%
  drop_na()

biomass3<-ar6_set %>%
  filter(Region=="World", Year==2050) %>%
  select(Scenario_ID, temperature2, Year, primary_energy_biomass) %>%
  drop_na()


biomass_trajectory<-ggplot()+
  geom_line(data=biomass2, aes(x=Year, y=primary_energy_biomass, group=Scenario_ID),color="grey", alpha=0.3)+
  geom_line(data=biomass1, aes(x=Year, y=primary_energy_biomass, linetype=scenario, group=Scenario_ID), size=1.1)+
  geom_line(data=biomass1, aes(x=Year, y=primary_energy_biomass, linetype=scenario, group=Scenario_ID), size=1.1)+
  geom_boxplot(data=biomass3, aes(x=Year, y=primary_energy_biomass))+
  theme_bw()+
  facet_wrap(~temperature2)+
  xlab("")+  
  ylab("Biomass EJ/year")+
  scale_linetype_manual(values = line_types)+
  annotate("rect", xmin = 2020, xmax = 2050, ymin = 100, ymax = 245,
           alpha = .3, fill="royalblue")+
  annotate("rect", xmin = 2020, xmax = 2050, ymin = 245, ymax = 300,
           alpha = .3, fill="hotpink1")+
  theme(legend.position="none")

biomass_trajectory


###############################
#Figure 5B
###############################


ccs1<-scenario_final %>%
 # filter(Region=="World", Illustrative=="Yes", Year<2055) %>%
  filter(Region=="World", Year<2055) %>%
  filter(`Short Name` %in% scenarios_key) %>%
  select(Scenario_ID, scenario, temperature2, Year, carbon_sequestration_ccs,carbon_sequestration_direct_air_capture) %>%
  drop_na()%>%
  mutate(scenario=factor(scenario, levels=c("No DACCS/Immediate Global Action", "DACCS/Immediate Global Action", "No DACCS/Governance Constrained Action", "DACCS/Governance Constrained Action")))


ccs2<-ar6_set %>%
  filter(Region=="World", Year<2055) %>%
  select(Scenario_ID, temperature2, Year, carbon_sequestration_ccs) %>%
  drop_na()

ccs3<-ar6_set %>%
  filter(Region=="World", Year==2050) %>%
  select(Scenario_ID, temperature2, Year, carbon_sequestration_ccs) %>%
  drop_na()




ccs_trajectory<-ggplot()+
  geom_line(data=ccs2, aes(x=Year, y=carbon_sequestration_ccs, group=Scenario_ID),color="grey", alpha=0.3)+
  geom_line(data=ccs1, aes(x=Year, y=carbon_sequestration_ccs+carbon_sequestration_direct_air_capture, linetype=scenario, group=Scenario_ID), size=1.1)+
  geom_boxplot(data=ccs3, aes(x=Year, y=carbon_sequestration_ccs))+
  facet_wrap(~temperature2)+
  theme_bw()+
  xlab("")+  
  ylab("CCS MtCO2/year")+
  scale_linetype_manual(values = line_types)+
  annotate("rect", xmin = 2020, xmax = 2050, ymin = 4000, ymax = 8600,
           alpha = .3, fill="royalblue")+
  annotate("rect", xmin = 2020, xmax = 2050, 
           ymin =8600, ymax = 30000,
           alpha = .3, fill="hotpink1")+
 theme(legend.position="top")


ccs_trajectory

#Put together in one graph Biomass and CCS

global<-plot_grid(biomass_trajectory,ccs_trajectory, labels=c("A", "B"), nrow = 2)
ggsave("global.jpeg", units="in", width=12, height=14, dpi=300)

#########################################
#Regional Figures - Figure 6
#########################################

solar_trajectory<-scenario_final %>%
  filter(Region!="World", Year<2045) %>%
  filter(`Short Name` %in% scenarios_key) %>%
  mutate(region2=case_when(
    Region=="LAM"~"Latin America and Caribbean",
    Region=="CPA"~"China and Centrally Planned Asia",
    Region=="NAM"~"Developed Regions",
    Region=="WEU"~"Developed Regions",
    Region=="EEU"~"Developed Regions",
    Region=="FSU"~"Developed Regions",
    Region=="PAO"~"Developed Regions",
    Region=="AFR"~"Middle East and Africa",
    Region=="MEA"~"Middle East and Africa",
    Region=="PAS"~"South and South East Asia",
    Region=="SAS"~"South and South East Asia")) %>%
  ungroup()%>%
  group_by(region2, Scenario_ID, Year, temperature2, scenario) %>%
  summarise(capacity_electricity_solar=sum(capacity_electricity_solar)) %>%
  ungroup() %>%
  group_by(region2,Scenario_ID) %>%
  mutate(growth=((capacity_electricity_solar/lag(capacity_electricity_solar))^(1/5)-1)*100) %>%
  filter(Year %in%c(2030,2035)) %>%
  mutate(Year=as.factor(Year))%>%  
  filter(region2!="Latin America and Caribbean") %>%
  drop_na() %>%
  mutate(scenario=factor(scenario, levels=c("No DACCS/Immediate Global Action", "DACCS/Immediate Global Action", "No DACCS/Governance Constrained Action", "DACCS/Governance Constrained Action"))) %>%
  mutate(region2=factor(region2, level=c("Developed Regions", "China and Centrally Planned Asia", "Latin America and Caribbean", "South and South East Asia","Middle East and Africa"))) %>%
  ggplot(aes(x=scenario, y=growth,fill=Year))+
  geom_col(position = "dodge", stat = "identity")+
  theme_bw()+
  #facet_wrap(~region2+temperature2, ncol = 3)+
  #facet_grid(region2~temperature2,scales = "free_x", space="free")+
  facet_grid(region2~temperature2)+
  xlab("")+  
  ylab("Solar Capacity yearly growth rate")+
  scale_fill_manual(values = col_fill)+
  annotate("rect",xmin = -Inf, xmax = Inf, ymin = 30, ymax = 40,
           alpha = .3, fill="royalblue")+
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 40, ymax = 50,
           alpha = .3, fill="hotpink1")+
  theme(legend.position="top", axis.text.x = element_text(angle =90))

solar_trajectory
ggsave("solar.jpeg", units="in", width=9, height=12, dpi=300)



#########################################
#Regional Figures - Figure 7
#########################################

coal_trajectory<-scenario_final %>%
  filter(Region!="World", Year<2045) %>%
  filter(`Short Name` %in% scenarios_key) %>%
  mutate(region2=case_when(
    Region=="LAM"~"Latin America and Caribbean",
    Region=="CPA"~"China and Centrally Planned Asia",
    Region=="NAM"~"Developed Regions",
    Region=="WEU"~"Developed Regions",
    Region=="EEU"~"Developed Regions",
    Region=="FSU"~"Developed Regions",
    Region=="PAO"~"Developed Regions",
    Region=="AFR"~"Middle East and Africa",
    Region=="MEA"~"Middle East and Africa",
    Region=="PAS"~"South and South East Asia",
    Region=="SAS"~"South and South East Asia")) %>%
  ungroup()%>%
  group_by(region2, Scenario_ID, Year, temperature2, scenario) %>%
  summarise(secondary_energy_electricity_coal=sum(secondary_energy_electricity_coal),secondary_energy_electricity=sum(secondary_energy_electricity)) %>%
  ungroup() %>%
  group_by(region2,Scenario_ID) %>%
  mutate(coal_pp=100*(lag(secondary_energy_electricity_coal)/lag(secondary_energy_electricity)-secondary_energy_electricity_coal/secondary_energy_electricity)) %>%
  filter(Year %in%c(2025,2030)) %>%
  mutate(Year=as.factor(Year))%>%  
  drop_na() %>%
  mutate(scenario=factor(scenario, levels=c("No DACCS/Immediate Global Action", "DACCS/Immediate Global Action", "No DACCS/Governance Constrained Action", "DACCS/Governance Constrained Action"))) %>%
  mutate(region2=factor(region2, level=c("Developed Regions", "China and Centrally Planned Asia", "Latin America and Caribbean", "South and South East Asia","Middle East and Africa"))) %>%
  ggplot(aes(x=scenario, y=coal_pp,fill=Year))+
  geom_col(position = "dodge", stat = "identity")+
  theme_bw()+
  facet_grid(region2~temperature2)+
  xlab("")+
  ylab("Coal decline rate")+
  scale_fill_manual(values = col_fill2)+
  annotate("rect",xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20,
           alpha = .3, fill="royalblue")+
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 20, ymax = 40,
           alpha = .3, fill="hotpink1")+
  theme(legend.position="top", axis.text.x = element_text(angle =90))

coal_trajectory
ggsave("coal.jpeg", units="in", width=9, height=14, dpi=300)

#########################################
#Regional Figures - Wind SI
#########################################

wind_trajectory<-scenario_final %>%
  filter(Region!="World", Year<2045) %>%
  filter(`Short Name` %in% scenarios_key) %>%
  mutate(region2=case_when(
    Region=="LAM"~"Latin America and Caribbean",
    Region=="CPA"~"China and Centrally Planned Asia",
    Region=="NAM"~"Developed Regions",
    Region=="WEU"~"Developed Regions",
    Region=="EEU"~"Developed Regions",
    Region=="FSU"~"Developed Regions",
    Region=="PAO"~"Developed Regions",
    Region=="AFR"~"Middle East and Africa",
    Region=="MEA"~"Middle East and Africa",
    Region=="PAS"~"South and South East Asia",
    Region=="SAS"~"South and South East Asia")) %>%
  ungroup()%>%
  group_by(region2, Scenario_ID, Year, temperature2, scenario) %>%
  summarise(capacity_electricity_wind=sum(capacity_electricity_wind)) %>%
  ungroup() %>%
  group_by(region2,Scenario_ID) %>%
  mutate(growth=((capacity_electricity_wind/lag(capacity_electricity_wind))^(1/5)-1)*100) %>%
  filter(Year %in%c(2030,2035)) %>%
  mutate(Year=as.factor(Year))%>%  
  drop_na() %>%
  mutate(scenario=factor(scenario, levels=c("No DACCS/Immediate Global Action", "DACCS/Immediate Global Action", "No DACCS/Governance Constrained Action", "DACCS/Governance Constrained Action"))) %>%
  mutate(region2=factor(region2, level=c("Developed Regions", "China and Centrally Planned Asia", "Latin America and Caribbean", "South and South East Asia","Middle East and Africa"))) %>%
  ggplot(aes(x=scenario, y=growth,fill=Year))+
  geom_col(position = "dodge", stat = "identity")+
  theme_bw()+
  facet_grid(region2~temperature2,scales = "free_x", space="free")+
  xlab("")+  
  ylab("Wind Capacity yearly growth rate")+
  scale_fill_manual(values = col_fill)+
  annotate("rect",xmin = -Inf, xmax = Inf, ymin = 30, ymax = 40,
           alpha = .3, fill="royalblue")+
  annotate("rect", xmin = -Inf, xmax = Inf,  ymin = 40, ymax = 50,
           alpha = .3, fill="hotpink1")+
  theme(legend.position="top", axis.text.x = element_text(angle =90))

wind_trajectory
ggsave("wind.jpeg", units="in", width=9, height=14, dpi=300)

