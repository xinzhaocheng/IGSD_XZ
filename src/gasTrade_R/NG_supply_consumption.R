library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(patchwork) 

file_path_ngp <- "D:\\CGS-natural_gas_project\\Output 1125\\NG supply.csv"
output_dir <- "D:\\CGS-natural_gas_project\\Output 1125"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

yrs_plot <- c("2021", "2030", "2035","2045", "2060")
scen_order <- c(
  "NZ", 
  "NZ_Lead",
  "NZ_LeadLag",
  "Ref", 
  "Ref_Lead", 
  "Ref_LeadLag"
)
scen_2021 <- "Ref"


data_ngp <- read_csv(file_path_ngp)


all_techs <- sort(unique(data_ngp$technology))
num_colors <- length(all_techs)
if (num_colors > 12) {
  tech_colors <- colorRampPalette(brewer.pal(12, "Set3"))(num_colors)
} else {
  tech_colors <- brewer.pal(max(3, num_colors), "Set3")[1:num_colors]
}
tech_palette <- setNames(tech_colors, all_techs)

data_plot_ngp <- data_ngp %>%
  pivot_longer(
    cols = `1990`:`2060`,
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Value = as.numeric(Value)) %>%
  filter(
    region == "China",
    Year %in% yrs_plot,
    (Year == "2021" & scenario == scen_2021) | (Year != "2021")
  ) %>%
  mutate(
    Year = factor(Year, levels = yrs_plot),
    scenario = factor(scenario, levels = scen_order),
    technology = factor(technology, levels = all_techs)
  )


y_max <- data_plot_ngp %>%
  group_by(Year, scenario) %>%
  summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  summarise(y_max = max(total, na.rm = TRUE)) %>%
  pull(y_max)


y_coord_shared <- coord_cartesian(ylim = c(0, y_max))


data_2021 <- data_plot_ngp %>% filter(Year == "2021")
data_others <- data_plot_ngp %>% filter(Year != "2021")


theme_shared <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.title.x = element_blank(),
    legend.position = "right",
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16)
  )


p_2021 <- ggplot(data_2021, aes(x = scenario, y = Value, fill = technology)) +
  geom_col(position = "stack", width = 0.4) +
  facet_wrap(~ Year, nrow = 1) +
  scale_fill_manual(values = tech_palette, drop = FALSE) +
  y_coord_shared +  
  labs(
    y = "EJ",
    fill = "Technology"
  ) +
  theme_shared +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


p_others <- ggplot(data_others, aes(x = scenario, y = Value, fill = technology)) +
  geom_col(position = "stack", width = 0.9) +
  facet_wrap(~ Year, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = tech_palette, drop = FALSE) +
  y_coord_shared +  
  labs(
    y = NULL,
    fill = "Technology"
  ) +
  theme_shared +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

final_plot <- p_2021 + p_others +
  plot_layout(
    guides = 'collect',
    widths = c(0.7, 3)
  ) +
  plot_annotation(
    title = 'NG Supply'
  ) &
  theme(plot.title = element_text(size = 16, face = "bold"))

print(final_plot)

output_filename <- file.path(output_dir, "China_NG_supply.png")
ggsave(
  output_filename,
  plot = final_plot,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)


library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)


file_path_ngc <- "D:\\CGS-natural_gas_project\\Output 1125\\NG consumption.csv"
output_dir    <- "D:\\CGS-natural_gas_project\\Output 1125"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

yrs_plot <- c("2021", "2030", "2035", "2045", "2060")
scen_order <- c(
  "NZ", 
  "NZ_Lead",
  "NZ_LeadLag",
  "Ref", 
  "Ref_Lead", 
  "Ref_LeadLag"
)
scen_2021 <- "Ref"


data_ngc <- read_csv(file_path_ngc)


fuel_order <- c(
  "wind", "solar", "geothermal", "biomass", "hydro",
  "nuclear", "natural gas", "oil", "coal"
)


pe_fuel.color <- c(
  "biomass"     = "#88CEB9",
  "solar"       = "#FEE12B",
  "wind"        = "#6BCAF1",
  "geothermal"  = "#335f7a",
  "hydro"       = "#0288A7",
  "coal"        = "#4A4A4A",
  "nuclear"     = "#F8991F",
  "natural gas"         = "#A4A4A4", 
  "oil"         = "#787878"
)
pe_fuel.color <- setNames(pe_fuel.color, tolower(names(pe_fuel.color)))


fuel_palette <- pe_fuel.color[fuel_order]
missing_fuels <- fuel_order[is.na(fuel_palette)]
if (length(missing_fuels) > 0) {
  fuel_palette[missing_fuels] <- "grey80"
}


data_plot_ngc <- data_ngc %>%
  pivot_longer(
    cols = `1990`:`2060`,
    names_to  = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Value = as.numeric(Value),
    fuel  = tolower(fuel)   
  ) %>%
  filter(
    region == "China",
    Year %in% yrs_plot,
    (Year == "2021" & scenario == scen_2021) | (Year != "2021")
  ) %>%
  mutate(
    Year     = factor(Year, levels = yrs_plot),
    scenario = factor(scenario, levels = scen_order),
    fuel     = factor(fuel, levels = fuel_order)  
  )


y_max <- data_plot_ngc %>%
  group_by(Year, scenario) %>%
  summarise(total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  summarise(y_max = max(total, na.rm = TRUE)) %>%
  pull(y_max)

y_coord_shared <- coord_cartesian(ylim = c(0, y_max))


data_2021   <- data_plot_ngc %>% filter(Year == "2021")
data_others <- data_plot_ngc %>% filter(Year != "2021")


theme_shared <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background  = element_rect(fill = "white", colour = "white"),
    axis.title.x     = element_blank(),
    legend.position  = "right",
    panel.border     = element_rect(colour = "black", fill = NA, size = 0.5),
    plot.title       = element_text(size = 16, face = "bold"),
    axis.text.y      = element_text(size = 16),
    axis.title.y     = element_text(size = 16),
    legend.title     = element_text(size = 16),
    legend.text      = element_text(size = 16),
    strip.text       = element_text(size = 16)
  )


p_2021 <- ggplot(data_2021, aes(x = scenario, y = Value, fill = fuel)) +
  geom_col(position = "stack", width = 0.4) +
  facet_wrap(~ Year, nrow = 1) +
  scale_fill_manual(
    values = fuel_palette,
    drop   = FALSE,
    breaks = fuel_order,
    labels = tools::toTitleCase(fuel_order)  
  ) +
  y_coord_shared +
  labs(y = "EJ", fill = "Fuel") +
  theme_shared +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())


p_others <- ggplot(data_others, aes(x = scenario, y = Value, fill = fuel)) +
  geom_col(position = "stack", width = 0.9) +
  facet_wrap(~ Year, scales = "free_x", nrow = 1) +
  scale_fill_manual(
    values = fuel_palette,
    drop   = FALSE,
    breaks = fuel_order,
    labels = tools::toTitleCase(fuel_order)
  ) +
  y_coord_shared +
  labs(y = NULL, fill = "Fuel") +
  theme_shared +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 16),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())


final_plot_ngc <- p_2021 + p_others +
  plot_layout(guides = "collect", widths = c(0.7, 3)) +
  plot_annotation(title = "NG Consumption") &
  theme(plot.title = element_text(size = 16, face = "bold"))

print(final_plot_ngc)

output_filename <- file.path(output_dir, "China_NG_consumption.png")
ggsave(
  output_filename,
  plot = final_plot_ngc,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)
