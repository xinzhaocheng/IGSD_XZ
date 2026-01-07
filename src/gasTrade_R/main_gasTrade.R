# Load libraries: -----------------

# Load libraries and settings --------------------------
library(tidyverse)
library(ggplot2)
library(tinytex)
library(assertthat) # For explanatory error messages
library(readxl)
library(rgcam)

# Load settings
source("src/functions.R")

# Make the date label for use in output file name
date_label <- format(Sys.Date(), "%Y%m%d")
date_label <- gsub("^20", "", date_label)

# (optionally) Add an additional version letter (eg. "b" or "c") to be appended to date 
version_letter <- ""

print(date_label)

# Define directory locations -----------

# Define working directories
# Remove existing directories if they exist
# You can change the names of these directories as needed
if (exists("dir_output")) {rm(dir_output)}
if (exists("dir_data")) {rm(dir_data)}
if (exists("dir_scenarios")) {rm(dir_scenarios)}
if (exists("dir_figures")) {rm(dir_figures)}

# Guess the name of the project from the subfolder name:
dir_src <- rev(strsplit(rstudioapi::getSourceEditorContext()$path, split = "/")[[1]])[2]
project_name <- gsub("_R", "", dir_src)
print(paste("Project name:", project_name))

# # If this is not the desired name, then re-define it:
# project_name <- "exampleProject"

# If no directories defined above, then define them based on the project name
if (!exists("dir_output")) {
  dir_output <- file.path("output", paste0(project_name, "_output"), "/")
  dir.create(dir_output, showWarnings = F)  
}
print("Using the processed results in this output directory: "); dir_output


if (!exists("dir_data")) {
  dir_data <- file.path("data", paste0(project_name, "_data"), "/")
  dir.create(dir_data, showWarnings = F)  
}
print("Using the un-processed data in this directory: "); dir_data


if (!exists("dir_scenarios")) {
  dir_scenarios <- file.path("data", "scenarios", "/")
  dir.create(dir_scenarios, showWarnings = F)  
}
print("Using the IAM runs in this directory: "); dir_scenarios

if (!exists("dir_figures")) {
  dir_figures <- file.path(dir_output, "figures", "/")
  dir.create(dir_figures, recursive = TRUE, showWarnings = FALSE)
}
cat("Using figures directory:", dir_figures, "\n")


# Load data, select scenarios, regions, years --------------

## Scenario results ----------------------------------
# Load GCAM or other IAMs, processed in IAMC-format
fls <- list.files(dir_scenarios); fls

# scen_file_overwrite <- file.path(dir_scenarios,"scenmip_v40i.csv")
scen_file_overwrite <- file.path(dir_scenarios,fls[1]); print(scen_file_overwrite)

data_scen_overwrite <- read.csv(scen_file_overwrite)
colnames(data_scen_overwrite) <- tolower(colnames(data_scen_overwrite))

data_scen_overwrite_additional <- bind_rows(read.csv(file.path(dir_scenarios, fls[2])),
                                            read.csv(file.path(dir_scenarios, fls[3])))
                                            
colnames(data_scen_overwrite_additional) <- tolower(colnames(data_scen_overwrite_additional))

data_scen_overwrite <- data_scen_overwrite |>
  # bind_rows(read_xlsx(file.path(dir_scenarios,"vllo new submission R3 gcam_plus2020.xlsx"))) |>
  # bind_rows(read_xlsx(file.path(dir_scenarios,"ssp2 vllo gcam R3 submission_plus2020.xlsx")))
  bind_rows(data_scen_overwrite_additional)

data_scen_overwrite <- data_scen_overwrite |>
  filter(model != "Model") ## |>
  # mutate(model = "GCAM-v8.0-CGS")

## Choose scenarios of interest ------------------------------
scenarios_avail <- unique(data_scen_overwrite$scenario); scenarios_avail
# scenarios_selected <- scenarios_avail[c(10:13)]; scenarios_selected
scenarios_selected <- scenarios_avail; scenarios_selected

## Choose a region(s) of interest --------------------------------
regions_avail <- unique(data_scen_overwrite$region); regions_avail
regions_selected <- regions_avail[c(11)]; regions_selected
print("Regions selected: _______________________"); print(regions_selected)

## Choose years --------------------------------------------------
# Define the max year to plot 
year_max_reg_overwrite <- 2060
year_max_glob_overwrite <- 2060

# Define the min year to plot
year_min_overwrite <- 2000

data_scen <- data_scen_overwrite 

data_scen <- data_scen |> 
  rename_with(tolower)

# Pivot the data to long format
if (!("year" %in% names(data_scen))){
  data_scen <- data_scen |>
    pivot_longer(cols = -c(model, variable, region, unit, scenario),
                 names_to = "year",
                 values_to = "value") |>
    mutate(year=as.numeric(gsub("x","",year))) 
  
}

data_scen <- data_scen |>
  filter(region %in% regions_selected,
         scenario %in% scenarios_selected,
         between(year, year_min_overwrite, max(year_max_reg_overwrite, year_max_glob_overwrite)))


data_scen <- data_scen |>
  arrange(model, region, year, variable, scenario) |>
  filter(!is.na(value)) |>
  mutate(value=as.numeric(value)) |>
  mutate(scenario = gsub("^scenmip_","",scenario)) |>
  mutate(scenario = gsub("^smip_","",scenario)) |>
  mutate(scenario = gsub("^smip8_","",scenario)) |>
  mutate(scenario = gsub("^chinaGas_","",scenario),
         scenario = gsub("250", "", scenario),
         scenario = gsub("fewerAddon", "fA", scenario))
# scenario = gsub("h2_250410", "SSP2 ", scenario),
# scenario = gsub("h5_250410", "High Emissions", scenario),
# scenario = gsub("l2_250410b", "Low Emissions", scenario))


### Load in additional query data ----------------

fls_dat <- grep(".dat", fls, value = T); fls_dat

fls_dat_select <- file.path(dir_scenarios, fls_dat[c(1:6)]); fls_dat_select

prj <- loadProject(file.path(dir_scenarios,"combined.dat"))

for (i in 1:length(fls_dat_select)) {
  prj_new <- loadProject(fls_dat_select[i])
  prj <- rgcam::mergeProjects(prjname = file.path(dir_scenarios,"combined.dat"), prjlist = list(prj, prj_new), saveProj = F)
} 

## Historical data -------------------------------------------------------
# Load IAMC-format processed historical results

# Define historical data location
hist_file_overwrite <- "D:/CGS-natural gas project/historical_gcam32.csv"
# hist_file_overwrite <- "../rhistiamc/output/historical_gcam32.csv"
data_hist_overwrite <- read_csv(hist_file_overwrite)


if (!("year" %in% names(data_hist_overwrite))){
data_hist_overwrite <- data_hist_overwrite |>
  pivot_longer(cols = -c(model, variable, region, unit, scenario),
               names_to = "year",
               values_to = "value") |>
  arrange(model, region, year, variable, scenario) |>
  filter(!is.na(value)) |> 
  mutate(value=as.numeric(value),
         year=as.numeric(year))
}


# Add any China-specific data not in all-regions dataset
data_hist_overwrite <- data_hist_overwrite |>
  bind_rows(read_csv(file.path(dir_data,"International Energy Agency - final consumption of gas by sector in China.csv"), comment = "#") |>
              select(variable = `final consumption of gas by sector in China`,
                     unit = `Units`,
                     year = `Year`,
                     value = "Value") |>
              mutate(variable = case_when(variable == "Transport" ~ "Transportation",
                                          variable == "Non-energy use" ~ "Industry",
                                          variable == "Residential" ~ "Residential and Commercial",
                                          variable == "Commercial and public services" ~ "Residential and Commercial",
                                          variable == "Agriculture / Forestry" ~ "Industry",
                                          .default = variable),
                     region = "China",
                     value = value * 10^(-6),
                     unit = "EJ/yr",
                     model = "IEA",
                     scenario = "historical") |>
              mutate(variable = paste0("Final Energy|", variable, "|Gases")) |>
              group_by(across(-value)) |>
              summarise(value = sum(value, na.rm = T)) |>
              ungroup()) 

data_hist_overwrite <- data_hist_overwrite |>
  bind_rows(new <- read_csv(file.path(dir_data,"International Energy Agency - total energy supply in China.csv"), comment = "#") |>
              select(variable = `total energy supply in China`,
                     unit = `Units`,
                     year = `Year`,
                     value = "Value") |>
              mutate(region = "China",
                     value=value/1e6, 
                     unit="EJ/yr",
                     variable = gsub("Natural gas", "Gas", variable),
                     scenario="historical",
                     model="IEA") |>
              mutate(variable = paste0("Primary Energy|", variable)))


iea_file <- file.path(dir_data, "China NG Statistics from IEA from 2015.xlsx")
iea_raw <- read_excel(iea_file, sheet = 1, skip = 0)

iea_long <- iea_raw %>%
  pivot_longer(
    cols      = matches("^20\\d{2}$"),
    names_to  = "year",
    values_to = "value"
  ) |>
  mutate(year = as.numeric(year))

iea_hist <- iea_long %>%
  mutate(

    variable = case_when(
      `Sector` == "MAINELEC" ~ "Secondary Energy|Electricity|Gas",
      
      `Sector` == "TOTIND"   ~ "Final Energy|Industry|Gases",
      `Sector` == "IRONSTL" ~ "Final Energy|Industry|Iron and Steel|Gases",
      `Sector` == "CHEMICAL" ~ "Final Energy|Industry|Chemicals|Gases",
      `Sector` == "NONFERR" ~ "Final Energy|Industry|Non-Ferrous Metals|Gases",
      `Sector` == "NONMET" ~ "Final Energy|Industry|Non-Metallic Minerals|Gases",
      `Sector` == "PAPERPRO" ~ "Final Energy|Industry|Pulp and Paper|Gases",
      
      `Sector` == "TOTTRANS" ~ "Final Energy|Transportation|Gases",
      
      `Sector` == "RESIDENT" ~ "Final Energy|Residential|Gases",
      `Sector` == "COMMPUB" ~ "Final Energy|Commercial|Gases",
      
      `Sector` == "HEATOUT" ~ "Secondary Energy|Heat|Gas",
    ),
    model    = "IEA Stats",
    region   = "China",
    scenario = "historical",
    value    = as.numeric(value) * 1e-6,
    unit     = "EJ"
  ) %>%
  filter(!is.na(variable), 
         !is.na(value)) |>
  select(model, variable, region, unit, scenario, year, value)

data_hist_overwrite <- bind_rows(data_hist_overwrite, iea_hist)
        

if (data_hist_overwrite[data_hist_overwrite$model == "OWID" & data_hist_overwrite$variable == "Emissions|CH4" & data_hist_overwrite$region == "World"  & data_hist_overwrite$year == 2020, ]$value > 1000) {
  data_hist_overwrite <- data_hist_overwrite |>
    filter(!(model == "OWID" & grepl("Emissions\\|CH4", variable)))
}


data_hist <- data_hist_overwrite

colnames(data_hist) <- gsub("^X", "", colnames(data_hist))

if (exists("regions_selected")) {
  data_hist <- data_hist |>
    filter(region %in% regions_selected)
}

# Pivot the data to long format
if (!("year" %in% names(data_hist))){
  data_hist <- data_hist |>
    pivot_longer(cols = -c(model, variable, region, unit, scenario),
                 names_to = "year",
                 values_to = "value") |>
    arrange(model, region, year, variable, scenario) |>
    filter(!is.na(value)) |> 
    mutate(value=as.numeric(value),
           year=as.numeric(year))
}

if (exists("year_min_overwrite")) {
  data_hist <- data_hist |>
    filter(year >= year_min_overwrite)
} 


              
# Run analysis scripts -----------------------

## Make gas demand plots -----------------------
source(file.path("src", dir_src, "demand_gas_plots.R"))


## Make gas supply / trade plots ---------------------
#Identify all “gas‑trade.csv” files in the runs directory
files <- list.files(
  path       = dir_scenarios,
  pattern    = "gas-trade\\.csv$",
  full.names = TRUE
)
# Specify which model years to include in the plots
yrs_keep <- c("2019","2020","2021","2025","2030","2035",
              "2040","2045","2050","2055","2060")
# Map each input file path to a short scenario label
scen_labs <- setNames(
  c("Cut", "No_Cut"),
  files
)

source(file.path("src", dir_src, "traded_gas_plots.R"))

## Make IMAC plots (https://docs.google.com/presentation/d/1ZHOh4nqpWO2LAGsO8uB0l0sbBHiVdflv/edit?slide=id.p1#slide=id.p1)

# dir_scenarios <- "D:/IGSD/runs/To_xinzhao_runs"

# Identify all queryout_*_IAMC.csv files in the runs directory
files <- list.files(
  path       = dir_scenarios,
  pattern    = "^queryout_.*_IAMC\\.csv$",
  full.names = TRUE
)

# Scenario labels for each input file
scen_labs <- setNames(
  dplyr::case_when(
    grepl("queryout_NEU_key-addon_test1028_IAMC\\.csv$", basename(files))       ~ "NetZero_default",
    grepl("queryout_RUS_NEU_1029_IAMC\\.csv$",          basename(files))       ~ "NetZero_core",
    grepl("queryout_RUS_NEU_CHN_1103_IAMC\\.csv$",      basename(files))       ~ "NetZero_CH4_CN",
    grepl("queryout_NetZero_CH4_group_1107_IAMC\\.csv$", basename(files))      ~ "NetZero_CH4_group",
    TRUE                                                                      ~ "Unknown"
  ),
  files
)

# Years to include in plots
yrs_keep <- c("2021","2025","2030","2035","2050","2060")

# Variables/technologies of interest
var_need  <- "CH4 emissions by tech (excluding resource production)"
tech_need <- c("imported LNG", "imported PAC pipeline gas", "imported RUS pipeline gas")

# Source plot function and run
source(file.path("src", "gasTrade_R", "traded_gas_plots_IAMC.R"))

traded_plots <- make_traded_gas_plots(
  gas_trade_files = files,
  output_dir      = dir_figures,
  yrs_keep        = yrs_keep,
  scen_labs       = scen_labs
)


## Make 

## Make energy mix and line plots ------------

file_name <- "analyis_chinaGas"; print(file_name)

### Make a PDF of results
rmarkdown::render(file.path("src", dir_src, "overview.R"), output_file = file.path("../../",dir_figures,paste0(file_name,"_", date_label, ".pdf")))

# ### Just make the figures
# source(file.path("src", dir_src, "overview.R"))



