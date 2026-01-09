# ============================================================================
# Merged script from Rachel:
#   - Figure_Formatting.R
#   - Functions.R
#   - Database Processing.R
#   - Figures.R
# ============================================================================

# 0. Load Packages ------------------------------------------------------------

library(rgcam)
library(tidyverse)
library(devtools)
library(magrittr)
library(stringr)
library(data.table)
library(purrr)

# ----------------------------------------------------------------------------
# 1. Figure formatting 
# ----------------------------------------------------------------------------


# Theme for ggplot
theme_basic <- list(theme_bw() ,
  theme(legend.text = element_text(size = 7, family = "Arial", vjust = 0.5)) ,
  theme(legend.title = element_text(size = 8, family = "Arial", vjust = 2)) ,
  theme(axis.text = element_text(size = 8, family = "Arial")) ,
  theme(axis.title = element_text(size = 8, family = "Arial", face = "bold")) ,
  theme(plot.title = element_text(size = 8, family = "Arial", face = "bold", vjust = 1)) ,
  theme(plot.subtitle = element_text(size = 7, family = "Arial", vjust = 1)) ,
  theme(strip.text = element_text(size = 8, family = "Arial")),
  theme(panel.border = element_blank(), panel.background = element_rect(fill = "#f7f5f2")))

# Color Scheme
light_blue_grey <- "#C4CEDC"
light_grey <- "#ECECEC"
extra_light_grey <- "#FAFAFA"
grey <- "#9E9E9E"
dark_grey <- "#565656"
black <- "#080808"
dark_blue <- "#06458C"
light_blue <- "#62C0F5"
green <- "#01AB8B"
dark_green = "#10632f"
yellow <- "#ECE03F"
marigold <- "#E6B200"
orange <- "#E68700"
red <- "#AB1700"
dark_red <- "#800101"
purple <- "#A40374"

color_palette <- c(purple = "#A40374",
                         dark_red = "#800101",
                         red = "#AB1700",
                         orange = "#E68700",
                         marigold = "#E6B200",
                         yellow = "#ECE03F",
                         green = "#01AB8B",
                         dark_green = "#10632f",
                         light_blue = "#62C0F5",
                         dark_blue = "#06458C",
                         black = "#080808",
                         dark_grey = "#565656",
                         grey = "#9E9E9E",
                         extra_light_grey = "#FAFAFA",
                         light_grey = "#ECECEC",
                         light_blue_grey = "#C4CEDC")

color_palette_rainbow_order <- c("#A40374",
                    "#800101",
                    "#AB1700",
                    "#E68700",
                    "#E6B200",
                    "#ECE03F",
                    "#01AB8B",
                    "#10632f",
                    "#62C0F5",
                    "#06458C",
                    "#080808",
                    "#565656",
                    "#9E9E9E",
                    "#FAFAFA",
                    "#ECECEC",
                    "#C4CEDC")

color_palette_scenarios<- c("#ECE03F",
                            "#01AB8B",
                            "#62C0F5",
                            "#800101",
                            "#E68700",
                            "#E6B200")


color_palette_order <- c("#A40374",
                         "#62C0F5",
                         "#ECE03F",
                         "#AB1700",
                         "#01AB8B",
                         "#06458C",
                         "#E6B200",
                         "#9E9E9E",
                         "#800101",
                         "#10632f",
                         "#E68700",
                         "#080808",
                         "#FAFAFA",
                         "#565656",
                          "#ECECEC",
                         "#C4CEDC")


color_palette_3 <- c("#06458C",
                     "#E6B200",
                     "#A40374" )

# ----------------------------------------------------------------------------
# 2. Processing functions
# ----------------------------------------------------------------------------

# Data Processing Functions

# Scope 3 emissions
# data_region = 'EU-12'
data_region = 'China'
# exp_gas_tech_data = exp_gas_tech
# tra_gas_tech_data =  tra_gas_tech
# reg_gas_tech_data = reg_gas_tech
# CH4_exported_NG_data = CH4_exported_NG
# CH4_traded_LNG_data = CH4_traded_LNG
# CH4_res_prod_data = CH4_res_prod
calculate_scope3 <- function(data_region, exp_gas_tech_data = exp_gas_tech,
                             tra_gas_tech_data =  tra_gas_tech, reg_gas_tech_data = reg_gas_tech,
                             CH4_exported_NG_data = CH4_exported_NG,
                             CH4_traded_LNG_data = CH4_traded_LNG,
                             CH4_res_prod_data = CH4_res_prod){


  # TRADE SHARE CALCULATION -------------------------------------------------

  # For each region, calculate:
  # 1) Region's share of exports (out of that region's total production), and
  # 2) Region's share of exports (out of that region's total exports).
  # For #2, we want this separate for traded LNG and traded pipeline

  # Domestic natural gas by region
  DomNG <- reg_gas_tech_data %>%
    filter(technology == "domestic natural gas") %>%
    group_by(Units, scenario, region, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    mutate(sector = "domestic natural gas")

  # Exported natural gas by region
  ExpNG <- exp_gas_tech_data %>%
    filter(sector %in% c("exported LNG", "exported pipeline gas")) %>%
    group_by(Units, scenario, region, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    mutate(sector = "exported natural gas")

  # Exported LNG/PipeGas by region
  ExpLNGPipeGas <- exp_gas_tech_data %>%
    filter(sector %in% c("exported LNG", "exported pipeline gas")) %>%
    group_by(Units, scenario, region, year, sector) %>%
    dplyr::summarise(value = sum(value))

  # Produced natural gas by region = Domestic + Exported
  ProdNG <- bind_rows(DomNG, ExpNG) %>%
    group_by(Units, scenario, region, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    mutate(sector = "produced natural gas")

  # Exported NG to Region by region
  region_TraNG <- tra_gas_tech_data %>%
    filter(region == data_region,
           sector %in% c("traded LNG", "traded pipeline gas")) %>%
    separate(subsector, into = c("Exporting_Region", NA), sep = c(" traded ")) %>%
    select(-region) %>%
    # NOTE: changing region to the exporting region here
    rename(region = Exporting_Region) %>%
    group_by(Units, scenario, region, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    mutate(sector = "exported natural gas")

  # Exported LNG/PipeGas to Region by region
  region_TraLNGPipeGas <- tra_gas_tech_data %>%
    filter(region == data_region,
           sector %in% c("traded LNG", "traded pipeline gas")) %>%
    separate(subsector, into = c("Exporting_Region", NA), sep = c(" traded ")) %>%
    select(-region) %>%
    # NOTE: changing region to the exporting region here
    rename(region = Exporting_Region) %>%
    # NOTE: changing sector names to exported
    mutate(sector = gsub("traded", "exported", sector)) %>%
    group_by(Units, scenario, region, year, sector) %>%
    dplyr::summarise(value = sum(value))

  # 1) Region's share of exports (out of that region's total production)
  # Used to calculate share of "natural gas" resource emissions
  region_TraNGShare_TotalProd <- region_TraNG %>%
    left_join(ProdNG, by = c("Units", "scenario", "region", "year"), suffix = c(".CN_exp", ".total_prod")) %>%
    mutate(share = value.CN_exp/value.total_prod)

  # 2) Region's share of exports (out of that region's total exports)
  # Used to calculate share of "exported LNG" or "exported pipeline gas" emissions
  # Therefore, we need to keep exported LNG and exported pipeline gas separate here
  region_TraLNGPipeGasShare_TotalExports <- region_TraLNGPipeGas %>%
    left_join(ExpLNGPipeGas, by = c("Units", "scenario", "region", "year", "sector"), suffix = c(".CN_exp", ".total_exp")) %>%
    mutate(share = value.CN_exp/value.total_exp)


  # PORTION OUT SHARE OF EMISSIONS ------------------------------------------

  # Region's Scope 3 emissions contains 3 parts:
  # 1. natural gas resource emissions = Production and processing emissions from the exporting regiob
  # 2. transmission and liquefaction (from LNG) emissions from the exporting region
  # 3. shipping emissions (from LNG)

  # Scope 3 emissions
  # 1. 'natural gas' resource
  # Region's share of emissions = 'natural gas' resource by region * Region's share of exports (out of that region's total production)
  region_Scope3_CH4_NG_res <- CH4_res_prod_data %>%
    filter(ghg == "CH4", resource == "natural gas") %>%
    left_join(region_TraNGShare_TotalProd, by = c("scenario", "region", "year"), suffix = c(".CH4", ".NG")) %>%
    select(scenario, region, year, value, share) %>%
    mutate(emissions = value * share,
           Units = "Tg") %>%
    filter(year >= 2010) %>%
    # OMIT NA ROWS, REGION HAS NO IMPORTS FROM THOSE REGIONS
    na.omit() %>%
    group_by(scenario, year, Units) %>%
    dplyr::summarise(emissions = sum(emissions),
                     sector = "Production and processing")

  # 2. 'exported LNG' and 'exported pipeline gas' sectors
  # Region's share of emissions = 'exported LNG/pipe gas' by region * Region's share of exports (out of that region's total exports)
  region_Scope3_CH4_ExportedNG <- CH4_exported_NG_data %>%
    left_join(region_TraLNGPipeGasShare_TotalExports, by = c("scenario", "region", "sector", "year"), suffix = c(".CH4", ".NG")) %>%
    select(scenario, region, sector, year, value, share) %>%
    mutate(emissions = value * share,
           Units = "Tg") %>%
    filter(year >= 2010) %>%
    # OMIT NA ROWS, Region HAS NO IMPORTS FROM THOSE REGIONS
    na.omit() %>%
    group_by(scenario, sector, year, Units) %>%
    dplyr::summarise(emissions = sum(emissions, na.rm = T)) %>%
    mutate(sector = if_else(sector == "exported LNG", "Transmission + Liquefaction (LNG)",
                            "Transmission (Pipeline)"))

  # # 3. Region's traded LNG emissions (shipping emissions) should also be added to Scope 3
  # region_Scope3_CH4_TradedLNG <- CH4_traded_LNG_data %>%
  #   filter(region == data_region) %>%
  #   mutate(sector = "Shipping (LNG)",
  #          emissions = value) %>%
  #   select(scenario, sector, year, Units)

  # Combine all Scope 3 emissions into a single table
  region_Scope3_CH4_emissions <- bind_rows(region_Scope3_CH4_NG_res,
                                         region_Scope3_CH4_ExportedNG)
                                         # ,region_Scope3_CH4_TradedLNG)

  return(region_Scope3_CH4_emissions) }


# Fix negatives with base year
# data_in <- CH4_non_resource_sector

# CH4_non_resource_sector <- getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
#   filter(ghg %in% c("CH4","CH4_AGR"))
# data_in <- CH4_non_resource_sector
fix_2021_neg <- function(data_in){

  BAU_replacement <- data_in %>%
    filter(scenario == 'Ref') %>%
    select(-scenario)

  bad_corrected <- data_in %>%
    filter(value < 0) %>%
    left_join(BAU_replacement %>% dplyr::rename(new_value = value)) %>%
    mutate(value = ifelse(value<1, new_value, value)) %>%
    select(-value) %>%
    dplyr::rename(value = new_value)

 all_corrected <-  data_in %>%
   filter(value > 0) %>%
   bind_rows(bad_corrected) %>%
   arrange(scenario, year, region)
  if(nrow(all_corrected) != nrow(data_in))error('fix neg row error')
  return(all_corrected)
}

# ----------------------------------------------------------------------------
# 3. Database processing
# ----------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. User Input Script Options -------------------------------------------------

database_pathway <- "/Users/rach919/GCAM/CGS/CGS-A/gcam-cgs/output"

# ------------------------------------------------------------------------------
# 1. Connect to GCAM ----------------------------------------------------------
# Connect a GCAM database using rgcam

database_pathway <- "/Users/rach919/GCAM/CGS/CGS-A/gcam-cgs/output"

conn <- localDBConn(database_pathway, "database_basexdb")

prj <- addScenario(conn = conn,
                   proj = "CH4_NG_figures.dat",
                   scenario = "Ref",
                   queryFile = "./input/CH4_CH_queries.xml", clobber = T)
prj <- addScenario(conn = conn,
                   proj = "CH4_NG_figures.dat",
                   scenario = "Ref_Lead",
                   queryFile = "./input/CH4_CH_queries.xml" , clobber = T)
prj <- addScenario(conn = conn,
                   proj = "CH4_NG_figures.dat",
                   scenario = "Ref_LeadLag",
                   queryFile = "./input/CH4_CH_queries.xml" , clobber = T)
prj <- addScenario(conn = conn,
                   proj = "CH4_NG_figures.dat",
                   scenario = "NZ",
                   queryFile = "./input/CH4_CH_queries.xml" , clobber = T)
prj <- addScenario(conn = conn,
                   proj = "CH4_NG_figures.dat",
                   scenario = "NZ_Lead",
                   queryFile = "./input/CH4_CH_queries.xml" , clobber = T)
prj <- addScenario(conn = conn,
                   proj = "CH4_NG_figures.dat",
                   scenario = "NZ_LeadLag",
                   queryFile = "./input/CH4_CH_queries.xml" , clobber = T)
# prj <- addScenario(conn = conn,
#                    proj = "CH4_NG_figures.dat",
#                    scenario = "GCAM REF",
#                    queryFile = "./input/CH4_CH_queries.xml" , clobber = T)

# ------------------------------------------------------------------------------
# 2. Retrieve Queries ----------------------------------------------------------

exp_gas_tech <- getQuery(prj, "exported gas by tech")
tra_gas_tech <- getQuery(prj, "traded gas by tech")
reg_gas_tech <- getQuery(prj, "regional natural gas by tech (nest)")

CH4_exported_NG <- getQuery(prj, "CH4 - Exported NG (transmission + liquefaction (for LNG) )") %>%
  fix_2021_neg
CH4_traded_LNG <- getQuery(prj, "CH4 - Traded LNG (shipping)") %>%
  fix_2021_neg
CH4_res_prod <- getQuery(prj, "CH4 emissions by resource production") %>%
  fix_2021_neg()
CH4_non_resource_sector <- getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
  filter(ghg %in% c("CH4","CH4_AGR")) %>%
  fix_2021_neg()
CH4_non_resource <- CH4_non_resource_sector %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(value)) %>%
  arrange(region, year, scenario)
CH4_region <- CH4_non_resource %>%
  bind_rows(CH4_res_prod %>%
              group_by(scenario, region, year) %>%
              summarize(value = sum(value))) %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(value))
energy_by_fuel <-getQuery(prj, "final energy consumption by fuel")

CH4_total_NG_in_region <- CH4_non_resource_sector %>%
  filter(sector %in% c("traded LNG","exported pipeline gas" ,"exported LNG","delivered gas" , "regional natural gas"  )) %>%
  bind_rows(CH4_res_prod %>%
              filter(resource == 'natural gas')) %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(value)) %>%
  arrange(region, year, scenario)

CH4_region <- CH4_non_resource_sector %>%
  filter(sector %in% c("traded LNG","exported pipeline gas" ,"exported LNG","delivered gas" , "regional natural gas"  )) %>%
  bind_rows(CH4_res_prod %>%
              group_by(scenario, region, year) %>%
              summarize(value = sum(value))) %>%
  group_by(scenario, region, year) %>%
  summarize(value = sum(value))

China_NG_sector <- CH4_non_resource_sector %>%
  filter(region == 'China') %>%
  filter(sector %in% c("traded LNG","exported pipeline gas" ,"exported LNG","delivered gas" , "regional natural gas"  )) %>%
  filter(year == 2021) %>%
  filter(scenario == 'Ref')

# ----------------------------------------------------------------------------
# 4. Figure generation 
# ----------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0.1 Define Helper Functions --------------------------------------------------
# Define function to define the short name of the scenarios given by IEF
# the short name is the letters between the first two dashes in the full scenario name
# For example: the IEF Scenario "MinReg-BAU-2060-Nov2022-v02", will be "BAU"
get_scenario_name <- function(string){
  str_sub(string,
          unlist(gregexpr('-',string))[1]+1, #location of the first dash
          unlist(gregexpr('-',string))[2]-1)  }
# Vectorize function
Vget_scenario_name <- Vectorize(get_scenario_name)

# ------------------------------------------------------------------------------
# Region groups

leaders <- c('Australia_NZ', 'Canada', 'EU-12', 'EU-15',
             'European Free Trade Association', 'Japan', 'Middle East',
             'South Korea', 'Taiwan', 'USA')

climate_forward <- c( 'China', 'EU-15', 'EU-12', 'Japan', 'South Korea')

all_interest <- c(leaders, climate_forward, "Russia")

lng_region_map <- read.csv('./input/lng_region_map.csv')

# ------------------------------------------------------------------------------
# 02. Define Constants, Themes, Colors -----------------------------------------

plot_folder <- "./output/figures/"
plot_years <- c(2021, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060)

# ------------------------------------------------------------------------------
# 0. Define Constants, Themes, Colors -----------------------------------------

#-----------------------------------
# 1. Methane across policies

# Global Methane
df_plot <- CH4_region %>%
  group_by(scenario, year) %>%
  summarize(value = sum(value))

ggplot(df_plot, aes(x = year, y = value, color = scenario))+
  geom_line(linewidth = 1.5) +
  theme_basic +
  ggtitle(paste('Global Methane Emissions across Scenarios'))+
  labs(y = "Methane [Tg]")+
  scale_color_manual(values = color_palette_scenarios)

ggsave(paste0(plot_folder, "Global Methane Line Chart.png"),
       width = 3.5, height = 3, units = 'in')

# Global Natural Gas System Methane
df_plot <- CH4_total_NG_in_region %>%
  group_by(scenario, year) %>%
  summarize(value = sum(value)) %>%
  filter(year %in% plot_years)

ggplot(df_plot, aes(x = year, y = value, color = scenario))+
  geom_line(linewidth = 1.5) +
  theme_basic +
  ggtitle(paste('Global Natural Gas System Methane Emissions across Scenarios'))+
  labs(y = "Methane [Tg]")+
  scale_color_manual(values = color_palette_scenarios)

ggsave(paste0(plot_folder, "Global NG Methane Line Chart.png"),
       width = 3.5, height = 3, units = 'in')

# --------------------
# NG methane production

df <- CH4_res_prod %>%
  filter(resource == 'natural gas') %>%
  filter(year %in% plot_years) %>%
  group_by(year, scenario) %>%
  summarize(value = sum(value))

ggplot(df, aes(x = year, y = value, color = scenario))+
  geom_line(linewidth = 1.5) +
  theme_basic +
  ggtitle(paste('Global Natural Gas Production + Processing Methane Emissions '))+
  labs(y = "Methane [Tg]")+
  scale_color_manual(values = color_palette_scenarios)

ggsave(paste0(plot_folder, "Production NG Methane Line Chart Leader vs Lag.png"),
       width = 4, height = 3, units = 'in')
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Methane Attributable to Leader Policies

scope12_CH4 <- CH4_res_prod %>%
  bind_rows( CH4_non_resource ) %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value))

scope3_China <- calculate_scope3( "China" ) %>% mutate(region = "China" )
scope3_EU15 <- calculate_scope3( "EU-15" )%>% mutate(region = "EU-15" )
scope3_EU12 <- calculate_scope3( "EU-12" )%>% mutate(region = "EU-12" )
scope3_Japan <- calculate_scope3( "Japan" )%>% mutate(region = "Japan" )
scope3_SKorea <- calculate_scope3( "South Korea" )%>% mutate(region = "South Korea" )

# scope3 <- scope3_China %>%
#   bind_rows(scope3_EU15) %>%
#   bind_rows(scope3_EU12) %>%
#   bind_rows(scope3_Japan) %>%
#   bind_rows(scope3_SKorea) %>%
#   filter(scenario %in% c('BAU',"BAU_Lead_CH_R-EU")) %>%
#   group_by(scenario, year, region) %>%
#   summarise(value = sum(emissions)) %>%
#   mutate(Scope = 'Scope 3')
# df <- scope12_CH4 %>%
#   filter(scenario %in% c('BAU',"BAU_Lead_CH_R-EU")) %>%
#   mutate(Scope = 'Scope 1+2') %>%
#   bind_rows(scope3) %>%
#   filter(year %in% plot_years) %>%
#   group_by(scenario, year, region) %>%
#   summarise(value = sum(value)) %>%
#   pivot_wider(names_from = 'scenario', values_from = 'value') %>%
#   mutate(reduction = BAU-`BAU_Lead_CH_R-EU`) %>%
#   filter(region %in% c(climate_forward, "China")) %>%
#   filter(year == 2060 ) %>%
#   mutate(year = as.factor(year)) %>%

#   ggplot(data = df, aes(x = region, y = reduction , fill = region))+
#   geom_col() +
#   theme_basic +
#   ggtitle(paste('Methane Reduction Attributable from Leader Policies'))+
#   labs(y = "CH4 Reduction [Tg]")+
#   theme(strip.background = element_blank(),
#         strip.placement = "outside")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.title.x = element_blank())
#
# ggsave(paste0(plot_folder, "Methane Attributable to Leader Policy - 260 Bar.png"),
#        width = 7, height = 3, units = 'in')
#------------------------------------
# Scope 1,2,3 climate forward
scope3 <- scope3_China %>%
  bind_rows(scope3_EU15) %>%
  bind_rows(scope3_EU12) %>%
  bind_rows(scope3_Japan) %>%
  bind_rows(scope3_SKorea) %>%
  mutate(Scope = 'Upstream - Outside Borders') %>%
  rename(value = emissions)
scope12_CH4 <- CH4_res_prod %>%
  bind_rows( CH4_non_resource ) %>%
  group_by(scenario, region, year) %>%
  summarise(value = sum(value))
df_scope <- scope12_CH4 %>%
  mutate(Scope = 'Direct Emissions') %>%
  filter(region %in% climate_forward) %>%
  group_by(scenario, year, region, Scope) %>%
  summarize(value = sum(value)) %>%
  filter(year %in% plot_years)
df_total <- df_scope %>%
  mutate(region = ifelse(region == 'China','China','Rest of Climate Forward')) %>%
  group_by(scenario, year, region) %>%
  summarize(value = sum(value)) %>%
  filter(year %in% plot_years)
# %>%
#   mutate(value = ifelse(scenario == "NZ_LeadLag" &
#                           region == 'China' & year == 2025, 47, value)) %>%
#   mutate(value = ifelse(scenario == "NZ_Lead" &
#                           region == 'China' & year == 2025, 46.5, value))

ggplot(df_total, aes(x = year, y = value, color = scenario))+
  geom_line() +
  theme_basic +
  facet_wrap(~region, scales = "free")+
  ggtitle(paste('Direct Methane Emissions across Scenarios for Climate Forward Regions'))+
  labs(y = "Methane [Tg]")+
  scale_color_manual(values = color_palette_scenarios)+
  expand_limits( y = 0)

ggsave(paste0(plot_folder, "Direct Methane for climate foward nations across scenarios.png"),
       width = 6, height = 3, units = 'in')

#-----------------------------------------
# Scope 1,2,3 climate forward CHINA
# scope3 <- scope3_China %>%
#   rename(value = emissions) %>%
#   mutate(Scope = 'Upstream Emissions')
# scope12_CH4 <- CH4_res_prod
#   bind_rows( CH4_non_resource ) %>%
#   group_by(scenario, region, year) %>%
#   summarise(value = sum(value)) %>%
#   filter(region == 'China') %>%
#   mutate(Scope = 'Direct Emissions')
# df_scope <- scope12_CH4 %>%
#   bind_rows(scope3) %>%
#   filter(region == 'China') %>%
#   group_by(scenario, year, region, Scope) %>%
#   summarize(value = sum(value)) %>%
#   filter(year %in% plot_years)
# # %>%
# #   filter(scenario %in% c('NZ',"NZ_Lead","NZ_LeadLag"))
# #
# # %>%
# #   mutate(value = ifelse(scenario == "NZ_LeadLag" &
# #                           region == 'China' & year == 2025, 47, value)) %>%
# #   mutate(value = ifelse(scenario == "NZ_Lead" &
# #                           region == 'China' & year == 2025, 46.5, value))
# # df_total <- df_scope %>%
# #   group_by(scenario, year, region) %>%
# #   summarize(value = sum(value)) %>%
# #   filter(year %in% plot_years) %>%
# #   filter(year != 2021) %>%
# #   filter(region == 'China')
#
# ggplot(df_scope, aes(x = year, y = value, fill = Scope))+
#   geom_col() +
#   theme_basic +
#   facet_wrap(~ scenario)+
#   ggtitle(paste('Direct and Upstream Methane Emissions across Scenarios - China'))+
#   labs(y = "Methane [Tg]")+
#   scale_color_manual(values = color_palette_scenarios)
#
# ggsave(paste0(plot_folder, "Scope123 Methane for climate foward nations across scenarios CHINA.png"),
#        width = 6, height = 3, units = 'in')

# ------------------------
# Scope 1,2,3 NG climate forward
scope3 <- scope3_China %>%
  bind_rows(scope3_EU15) %>%
  bind_rows(scope3_EU12) %>%
  bind_rows(scope3_Japan) %>%
  bind_rows(scope3_SKorea) %>%
  mutate(Scope = 'Scope 3') %>%
  rename(value = emissions)
df_ng <- CH4_total_NG_in_region  %>%
  bind_rows(scope3) %>%
  filter(region %in% climate_forward) %>%
  mutate(region = ifelse(region == 'China','China','Rest of Climate Forward')) %>%
  group_by(scenario, year, region) %>%
  summarize(value = sum(value)) %>%
  filter(year %in% plot_years) %>%
  filter(!(year == 2025 & scenario %in% c('NZ','NZ_Lead','NZ_LeadLag')))

ggplot(df_ng, aes(x = year, y = value, color = scenario))+
  geom_line() +
  theme_basic +
  facet_wrap(~ region, scales = "free")+
  ggtitle(paste('Upstream and Downstream Methane from Natural Gas System across Scenarios by region'))+
  labs(y = "Methane [Tg]")+
  scale_color_manual(values = color_palette_scenarios)

ggsave(paste0(plot_folder, "Scope123 NG Methane for climate foward nations across scenarios.png"),
       width = 6, height = 3, units = 'in')

# --------- Leaders and BAU
df_ng <- CH4_NG_in_region  %>%
  bind_rows(scope3) %>%
  filter(region %in% climate_forward) %>%
  group_by(scenario, year, region) %>%
  summarize(value = sum(value)) %>%
  filter(year %in% plot_years) %>%
  filter(year != 2021) %>%
  filter(scenario %in% c('BAU','CN',"BAU_Lead_CH_R-EU","CN_Lead_CH_R-EU"))

ggplot(df_ng, aes(x = year, y = value, color = scenario))+
  geom_line() +
  theme_basic +
  facet_wrap(~region, scales = "free")+
  ggtitle(paste('Scope 1+2+3 Methane from Natural Gas System'))+
  labs(y = "Methane [Tg]")+
  scale_color_manual(values = c(dark_red, orange, green, light_blue))

ggsave(paste0(plot_folder, "Scope123 NG Methane for climate foward nations Leaders.png"),
       width = 6, height = 3, units = 'in')

# ------------------------
# Scope 1,2,3 NG  CHINA
scope3 <- scope3_China %>%
  mutate(Scope = 'Scope 3') %>%
  rename(value = emissions)
df_ng <- CH4_NG_in_region  %>%
  filter(region == 'China') %>%
  bind_rows(scope3) %>%
  filter(region %in% climate_forward) %>%
  group_by(scenario, year, region) %>%
  summarize(value = sum(value)) %>%
  filter(year %in% plot_years) %>%
  filter(year != 2021)

ggplot(df_ng, aes(x = year, y = value, color = scenario))+
  geom_line(linewidth = 1.5) +
  theme_basic +
  ggtitle(paste('Scope 1+2+3 NG Methane by Scenario - China'))+
  labs(y = "Methane [Tg]")+
  scale_color_manual(values = color_palette_rainbow_order[1:length(df_plot$scenario %>% unique)])

ggsave(paste0(plot_folder, "Scope123 NG Methane China.png"),
       width = 6, height = 3, units = 'in')



# --------
# China
