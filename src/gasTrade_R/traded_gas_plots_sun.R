library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)

# Make sure the csv.file related with gas-trade in the correct format
parse_weird_csv <- function(path) {
  lns <- readLines(path, encoding = "UTF-8")
  is_title <- !grepl(",", lns) & nzchar(str_trim(lns))
  title_pos <- which(is_title)
  # For each title, extract the data block until the next title (or end)
  blocks <- map2(
    title_pos,
    c(title_pos[-1] - 1, length(lns)),
    ~ {
      ttl <- str_trim(lns[.x])
      block_ln <- lns[(.x + 1):.y]
      if (length(block_ln) < 2) return(NULL)
      # Read the block into a data.frame
      df <- read.csv(
        text = paste(block_ln, collapse = "\n"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      df$Variable <- ttl
      df
    }
  )
  # Combine all blocks into one data.frame
  bind_rows(blocks)
}
#-----------------------------------------------------------------
# make_traded_gas_plots: generates and saves figures based on
#                        parsed GCAM CSV outputs.
#   - files:      vector of file paths to parse
#   - output_dir: directory where PNGs will be saved
#   - yrs_keep:   character vector of year columns to retain
#   - scen_labs:  labels for each scenario/file (must match files)
# Returns a list of the three ggplot objects (invisibly).
#-----------------------------------------------------------------
make_traded_gas_plots <- function(
    files,
    output_dir,
    yrs_keep,
    scen_labs,
    var_need = "CH4 emissions by tech (excluding resource production)",
    tech_need = c(
      "imported LNG",
      "imported PAC pipeline gas",
      "imported RUS pipeline gas"
    )
) {
  dfs <- lapply(files, parse_weird_csv)
  for (i in seq_along(dfs)) {
    dfs[[i]]$Source <- scen_labs[i]
  }
  df_all <- bind_rows(dfs)
  yrs_use <- intersect(yrs_keep, names(df_all))
  # ----------------------------------------------------------------
  # Plot 1: Regional natural gas by technology (nest) for China
  # ----------------------------------------------------------------
  plot_df_nest <- df_all %>%
    filter(
      Variable == "regional natural gas by tech (nest)",
      trimws(region) == "China"
    ) %>%
    select(Source, technology, all_of(yrs_use)) %>%
    pivot_longer(all_of(yrs_use), names_to = "year", values_to = "value") %>%
    mutate(
      year = factor(year, levels = yrs_keep),
      Source = factor(Source, levels = scen_labs)
    )
  p1 <- ggplot(plot_df_nest, aes(x = Source, y = value, fill = technology)) +
    geom_col(width = 0.75) +
    facet_wrap(~ year, nrow = 2) +
    labs(x = NULL, y = NULL, fill = "Technology", title = "Regional natural gas by tech (nest) – China") +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "right",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  ggsave(
    filename = file.path(output_dir, "Regional natural gas by tech.png"),
    plot = p1,
    width = 9,
    height = 6,
    bg = "white"
  )
  # ----------------------------------------------------------------
  # Plot 2: CH4 emissions by region for China over time
  # ----------------------------------------------------------------
  plot_df_ch4_region <- df_all %>%
    filter(
      Variable == "CH4 emissions by region",
      trimws(region) == "China"
    ) %>%
    select(Source, all_of(yrs_use)) %>%
    pivot_longer(all_of(yrs_use), names_to = "year", values_to = "value") %>%
    mutate(
      year = as.integer(year),
      Source = factor(Source, levels = scen_labs)
    )
  p2 <- ggplot(plot_df_ch4_region, aes(x = year, y = value, color = Source)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(
      name = "Scenario",
      breaks = scen_labs,
      labels = scen_labs,
      values = setNames(c("#1f77b4", "#ff7f0e"), scen_labs)
    ) +
    labs(x = NULL, y = "CH4 (Tg)", title = "CH4 emissions by region – China") +
    theme_bw(base_size = 12)
  ggsave(
    filename = file.path(output_dir, "CH4 emission in China.png"),
    plot = p2,
    width = 9,
    height = 5,
    bg = "white"
  )
  # ----------------------------------------------------------------
  # Plot 3: CH4 emissions by technology for selected techs
  # ----------------------------------------------------------------
  plot_df_ch4_tech <- df_all %>%
    mutate(
      Variable = str_squish(Variable),
      region = str_squish(region),
      technology = str_squish(technology)
    ) %>%
    filter(
      Variable == var_need,
      region == "China",
      technology %in% tech_need
    ) %>%
    select(Source, technology, all_of(yrs_use)) %>%
    pivot_longer(all_of(yrs_use), names_to = "year", values_to = "value") %>%
    mutate(
      year = factor(year, levels = yrs_keep),
      technology = factor(technology, levels = tech_need),
      Source = factor(Source, levels = scen_labs)
    ) %>%
    complete(Source, year, technology, fill = list(value = 0))
  p3 <- ggplot(plot_df_ch4_tech, aes(x = Source, y = value, fill = technology)) +
    geom_col(width = 0.7) +
    facet_wrap(~ year, nrow = 2) +
    labs(
      x = NULL,
      y = paste0(var_need, " (Tg)"),
      fill = "Technology",
      title = paste0(var_need, " by tech – China")
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  ggsave(
    filename = file.path(output_dir, "CH4 emission by tech.png"),
    plot = p3,
    width = 9,
    height = 6,
    bg = "white"
  )
  # Return the plot objects (invisible so function is silent)
  invisible(list(nest = p1, region = p2, tech = p3))
}
