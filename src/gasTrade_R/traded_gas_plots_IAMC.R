library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(forcats)
library(cowplot)
library(tidyverse)
library(grid)
library(patchwork)
library(stringr)

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
    gas_trade_files, # Argument name
    output_dir,
    yrs_keep,
    scen_labs,
    var_need = "CH4 emissions by tech (excluding resource production)",
    tech_need = c(
      "imported LNG",
      "imported PAC pipeline gas",
      "imported RUS pipeline gas"
    )
) 

{
  dfs <- lapply(gas_trade_files, parse_weird_csv)
  for (i in seq_along(dfs)) {
    dfs[[i]]$Source <- scen_labs[i]
  }
  df_all <- bind_rows(dfs)
  yrs_use <- intersect(yrs_keep, names(df_all))


  
  ##################################### IAMC ##############################################
  
  ######################################################################################
  ########################## Russia exported gas ######################### ##############
  #######################################################################################
  
  get_first_col <- function(df, base) {
    cand <- names(df)[stringr::str_detect(names(df), paste0("^", base, "(\\.\\.\\.[0-9]+)?$"))]
    if (length(cand) == 0) base else cand[1]
  }
  
  col_region   <- get_first_col(df_all, "region")
  col_variable <- get_first_col(df_all, "Variable")
  col_sector   <- get_first_col(df_all, "sector")
  col_source   <- "Source"
  
  yrs_keep <- c("2021","2025","2035","2060")
  yrs_use  <- intersect(yrs_keep, names(df_all))
  
  sectors_need <- c("exported LNG", "exported pipeline gas")
  sources_need <- c("NetZero_default", "NetZero_core", "NetZero_CH4_CN", "NetZero_CH4_group")
  
  # ---------------- data prep ----------------
  plot_df_rus_exp <- df_all %>%
    dplyr::filter(
      trimws(.data[[col_variable]]) == "exported gas by tech",
      trimws(.data[[col_region]])   == "Russia",
      trimws(.data[[col_sector]])   %in% sectors_need,
      .data[[col_source]]           %in% sources_need
    ) %>%
    dplyr::select(
      Source,
      sector = dplyr::all_of(col_sector),
      dplyr::all_of(yrs_use)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(yrs_use),
      names_to = "year", values_to = "value"
    ) %>%
    dplyr::mutate(
      year   = factor(year, levels = yrs_keep),
      Source = factor(Source, levels = sources_need),
      sector = factor(trimws(sector), levels = sectors_need),
      value  = as.numeric(value)
    ) %>%
    dplyr::filter(
      (year == "2021" & Source == sources_need[1]) | (year != "2021")
    )
  
  # ---------------- position plots ----------------
  n_scen <- length(sources_need)
  eps    <- 0.06      
  year_gap <- 0.3   
  
  # 2021→2025 = year_gap/2，others = year_gap
  centers <- numeric(length(yrs_keep))
  centers[1] <- year_gap
  for (i in 2:length(yrs_keep)) {
    add <- if (yrs_keep[i-1] == "2021" && yrs_keep[i] == "2025") year_gap/1.5 else year_gap
    centers[i] <- centers[i-1] + add
  }
  names(centers) <- yrs_keep
  
）
  scen_index <- match(plot_df_rus_exp$Source, sources_need)
  
  plot_df_rus_exp2 <- plot_df_rus_exp %>%
    dplyr::mutate(
      grp        = as.character(year),
      grp_center = centers[grp],
      x = grp_center + ifelse(
        year == "2021",
        0,  # 2021 one pole
        (scen_index - mean(seq_len(n_scen))) * eps
      )
    )
  
  # X-axis labels: hide scenario name for 2021, show for other years
  x_ticks  <- plot_df_rus_exp2 %>%
    dplyr::distinct(x, grp, year, Source) %>%
    dplyr::arrange(grp, x)
  x_breaks <- x_ticks$x
  x_labels <- ifelse(x_ticks$year == "2021", "", as.character(x_ticks$Source))
  
  sec_centers <- as.numeric(centers)
  
  scale_x_continuous(
    breaks = x_breaks,
    labels = x_labels,
    expand = ggplot2::expansion(mult = c(0.01, 0.02)),
    sec.axis = ggplot2::dup_axis(
      name   = NULL,
      breaks = sec_centers,  
      labels = yrs_keep
    )
  )
  
  
  # ---------------- color and poltting ----------------
  pal_sec <- c("exported LNG" = "#4DB6AC", "exported pipeline gas" = "#fc8d62")
  
  p_rus_exp <- ggplot2::ggplot(plot_df_rus_exp2, ggplot2::aes(x = x, y = value, fill = sector)) +
    ggplot2::geom_col(width = 0.05) +
    ggplot2::scale_fill_manual(values = pal_sec, name = "Sector") +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.01, 0.02)),
      sec.axis = ggplot2::dup_axis(
        name   = NULL,
        breaks = centers,
        labels = yrs_keep
      )
    ) +
    ggplot2::labs(
      x = NULL, y = "Natural Gas (EJ)",
      title = "Russia exported gas"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position    = "right",
      legend.key.height  = grid::unit(10, "pt"),
      legend.key.width   = grid::unit(14, "pt"),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.x.top    = ggplot2::element_text(angle = 0,  hjust = 0.5, vjust = 0.5,
                                                 margin = ggplot2::margin(b = 4)),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(t = 18, r = 35, b = 12, l = 8, unit = "pt")
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "Internal_IGSD_RUS_exported_gas.png"),
    plot = p_rus_exp, width = 7.2, height = 4.8, dpi = 300, bg = "white"
  )
  
  
  ######################################################################################
  ########################## Russia traded LNG and Pipeline (slide 6 left) ##############
  #######################################################################################
  
  suppressPackageStartupMessages({
    library(tidyverse); library(ggplot2); library(grid); library(patchwork); library(cowplot)
  })
  
  # ----------------------------- grouping -----------------------------
  group_region7 <- function(x) {
    europe <- c("EU-15","EU-12","Europe_Non_EU","European Free Trade Association","Ukraine","EFTA","Europe")
    dplyr::case_when(
      x %in% europe                                   ~ "Europe",
      x == "China"                                    ~ "China",
      x %in% c("Japan")                               ~ "Japan",
      x %in% c("South Korea","Korea","Republic of Korea") ~ "Korea",
      x %in% c("India")                               ~ "India",
      x == "Central Asia"                             ~ "Central Asia",
      TRUE                                            ~ "ROW"
    )
  }
  group_region6 <- function(x) {
    europe <- c("EU-15","EU-12","Europe_Non_EU","European Free Trade Association","Ukraine","EFTA","Europe")
    dplyr::case_when(
      x %in% europe                                   ~ "Europe",
      x == "China"                                    ~ "China",
      x %in% c("Japan")                               ~ "Japan",
      x %in% c("South Korea","Korea","Republic of Korea") ~ "Korea",
      x %in% c("India")                               ~ "India",
      TRUE                                            ~ "ROW"
    )
  }
  
  grp_levels <- c("Europe","China","Japan","Korea","India","Central Asia","ROW")
  
  group_lng_source <- function(tech) {
    base <- sub("\\s*traded\\s*LNG\\s*$", "", trimws(tech))
    if (base %in% c("Australia_NZ", "Australia")) return("Australia")
    if (base %in% c("Middle East"))               return("Middle East")
    if (base %in% c("Russia"))                    return("Russia")
    other_high <- c("Ukraine","South Korea","USA","South America_Southern","Indonesia",
                    "Central Asia","Africa_Northern","Africa_Western","Central America and Caribbean",
                    "India","EU-15","Brazil","Africa_Southern","Colombia")
    if (base %in% other_high) return("Other high-methane producers")
    other_low <- c("Canada","European Free Trade Association","Southeast Asia","Japan")
    if (base %in% other_low) return("Other low-methane producers")
    return("Other low-methane producers")
  }
  
  # ----------------------------- color -----------------------------
  pal_region_global <- c(
    "Europe"       = "#1f77b4",
    "China"        = "#d62728",
    "Japan"        = "#9467bd",
    "Korea"        = "#ff7f0e",
    "India"        = "#2ca02c",
    "Central Asia" = "#bcbd22",
    "ROW"          = "#7f7f7f"
  )
  
  present_breaks <- function(x) intersect(grp_levels, unique(as.character(x)))
  
  col_region     <- get_first_col(df_all, "region")
  col_technology <- get_first_col(df_all, "technology")
-
  yrs_keep    <- c("2021","2025","2035","2060")
  yrs_use     <- intersect(yrs_keep, names(df_all))
  scen_levels <- c("NetZero_default", "NetZero_core", "NetZero_CH4_CN", "NetZero_CH4_group")
  
  eps      <- 0.06  
  year_gap <- 0.3   
  
  centers <- numeric(length(yrs_keep))
  centers[1] <- year_gap
  for (i in 2:length(yrs_keep)) {
    add <- if (yrs_keep[i-1] == "2021" && yrs_keep[i] == "2025") year_gap/1.5 else year_gap
    centers[i] <- centers[i-1] + add
  }
  names(centers) <- yrs_keep
  sec_centers <- as.numeric(centers)
  
  # ----------------------------- data and function prep -----------------------------
  prep_rus_data <- function(tech_string, group_fun) {
    df <- df_all %>%
      dplyr::filter(
        Variable == "traded gas by tech",
        trimws(.data[[col_technology]]) == tech_string
      ) %>%
      dplyr::select(Source, region = dplyr::all_of(col_region), dplyr::all_of(yrs_use)) %>%
      tidyr::pivot_longer(dplyr::all_of(yrs_use), names_to = "year", values_to = "value") %>%
      dplyr::mutate(
        year   = factor(year, levels = yrs_keep),
        Source = factor(Source, levels = scen_levels),
        value  = as.numeric(value),
        region_grp = factor(group_fun(region), levels = grp_levels)
      ) %>%
      dplyr::filter(year %in% yrs_keep) %>%
      dplyr::filter((year == "2021" & Source == scen_levels[1]) | year != "2021") %>%
      dplyr::group_by(Source, year, region_grp) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    scen_index <- match(df$Source, scen_levels)
    df2 <- df %>%
      dplyr::mutate(
        grp_center = centers[as.character(year)],
        x = grp_center + ifelse(year == "2021", 0,
                                (scen_index - mean(seq_len(length(scen_levels)))) * eps)
      )
  
    x_ticks  <- df2 %>% dplyr::distinct(x, year, Source) %>% dplyr::arrange(year, x)
    x_breaks <- x_ticks$x
    x_labels <- ifelse(x_ticks$year == "2021", "", as.character(x_ticks$Source))
    
    list(df = df, df2 = df2, x_breaks = x_breaks, x_labels = x_labels)
  }
  
  base_rus_plot <- function(df2, legend_name, x_breaks, x_labels,
                            show_top_years = TRUE, angle_bottom = 45,
                            add_margin = ggplot2::margin(t = 18, r = 35, b = 12, l = 8, unit = "pt")) {
    
    p <- ggplot2::ggplot(df2, ggplot2::aes(x = x, y = value, fill = region_grp)) +
      ggplot2::geom_col(width = 0.05) +
      ggplot2::scale_fill_manual(
        values = pal_region_global,
        breaks = present_breaks(df2$region_grp),
        name   = legend_name
      ) +
      ggplot2::scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels,
        expand = ggplot2::expansion(mult = c(0.01, 0.02)),
        sec.axis = if (show_top_years) ggplot2::dup_axis(name = NULL, breaks = sec_centers, labels = yrs_keep) else waiver()
      ) +
      ggplot2::labs(x = NULL, y = "Natural Gas (EJ)") +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        legend.position    = "right",
        legend.key.height  = grid::unit(10, "pt"),
        legend.key.width   = grid::unit(14, "pt"),
        axis.text.x        = ggplot2::element_text(angle = angle_bottom, hjust = 1, vjust = 1),
        axis.text.x.top    = if (show_top_years)
          ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 0.5, margin = ggplot2::margin(b = 4))
        else ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.margin        = add_margin
      ) +
      ggplot2::coord_cartesian(clip = "off")
    
    p
  }
  
  # strip on the right
  add_right_strip <- function(p, label, w_bar_data = 0.05) {
    gb <- ggplot2::ggplot_build(p)
    panel_params <- gb$layout$panel_params[[1]]
    xr <- if (!is.null(panel_params$x.range)) panel_params$x.range else c(panel_params$x$range$range[1], panel_params$x$range$range[2])
    x_span <- diff(xr)
    w_npc <- w_bar_data / x_span
    w_npc <- max(min(w_npc, 0.12), 0.01)
    
    p +
      ggplot2::annotation_custom(
        grob = grid::rectGrob(
          x = unit(1, "npc"), y = unit(0.5, "npc"),
          width  = unit(w_npc, "npc"), height = unit(1, "npc"),
          just = c("right", "center"),
          gp = grid::gpar(fill = "#E5E5E5", col = "grey50", lwd = 0.6)
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) +
      ggplot2::annotation_custom(
        grob = grid::textGrob(
          label, x = unit(1, "npc") - unit(w_npc/2, "npc"), y = unit(0.5, "npc"),
          rot = 90, gp  = grid::gpar(fontface = "bold", cex = 0.9)
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
  }
  
  # ========================  Plot 4: Russia traded LNG  =====================
  lng_list <- prep_rus_data("Russia traded LNG", group_region6)
  p_rus_lng <- base_rus_plot(
    df2 = lng_list$df2,
    legend_name = "Region",            
    x_breaks = lng_list$x_breaks,
    x_labels = lng_list$x_labels,
    show_top_years = TRUE,
    angle_bottom = 45
  ) + ggplot2::labs(title = "Russia traded LNG")
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "IAMC_Russia_traded_LNG.png"),
    plot = p_rus_lng, width = 6.2, height = 4.8, dpi = 300, bg = "white"
  )
  
  # ===============  Plot 5: Russia traded pipeline gas  =====================
  pip_list <- prep_rus_data("Russia traded pipeline gas", group_region7)
  p_rus_pip <- base_rus_plot(
    df2 = pip_list$df2,
    legend_name = "Region",
    x_breaks = pip_list$x_breaks,
    x_labels = pip_list$x_labels,
    show_top_years = TRUE,
    angle_bottom = 45
  ) + ggplot2::labs(title = "Russia traded pipeline gas")
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "IAMC_Russia_traded_pipeline.png"),
    plot = p_rus_pip, width = 6.2, height = 4.8, dpi = 300, bg = "white"
  )
  
  # ================= Russia traded LNG & pipeline ==================
  # plot on the top
  p_rus_lng_u <- base_rus_plot(
    df2 = lng_list$df2,
    legend_name = "Region",
    x_breaks = lng_list$x_breaks,
    x_labels = rep("", length(lng_list$x_breaks)), 
    show_top_years = TRUE,
    angle_bottom = 0
  )
  p_rus_lng_u <- p_rus_lng_u +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   plot.title   = ggplot2::element_blank())
  
  # plot on the bottom
  p_rus_pip_u <- base_rus_plot(
    df2 = pip_list$df2,
    legend_name = "Region",
    x_breaks = pip_list$x_breaks,
    x_labels = pip_list$x_labels,
    show_top_years = FALSE,
    angle_bottom = 45
  )
  p_rus_pip_u <- p_rus_pip_u + ggplot2::theme(plot.title = ggplot2::element_blank())
  
  # strip on the right
  w_bar <- 0.05
  p_rus_lng_u_strip <- add_right_strip(p_rus_lng_u,  "LNG",      w_bar)
  p_rus_pip_u_strip <- add_right_strip(p_rus_pip_u,  "Pipeline", w_bar)
  
  # combine
  p_combined <- (p_rus_lng_u_strip / p_rus_pip_u_strip) +
    patchwork::plot_layout(heights = c(1, 1)) &
    ggplot2::theme(legend.position = "right")
  
  p_combined <- p_combined + patchwork::plot_annotation(
    title = "Russia traded LNG and pipeline",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0, vjust = 0.5)
    )
  )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "IAMC_Russia_traded_LNG_and_pipeline_combined.png"),
    plot = p_combined, width = 7.2, height = 6.0, dpi = 300, bg = "white"
  )
  
  
  ######################################################################################
  ########################## Russia natural gas production (slide 1 right) ##############
  ######################################################################################

  scen_levels <- c("NetZero_default", "NetZero_core", "NetZero_CH4_CN", "NetZero_CH4_group")
  first_scen  <- scen_levels[1]
  yrs_use     <- if (exists("yrs_use")) yrs_use else yrs_keep   # 保持与上文一致
  
  .to_num <- function(x) suppressWarnings(as.numeric(x))

  .compute_centers <- function(yrs_keep, year_gap = 0.30) {
    centers <- numeric(length(yrs_keep))
    centers[1] <- year_gap
    if (length(yrs_keep) >= 2) {
      for (i in 2:length(yrs_keep)) {
        add <- if (yrs_keep[i - 1] == "2021" && yrs_keep[i] == "2025") year_gap / 1.5 else year_gap
        centers[i] <- centers[i - 1] + add
      }
    }
    names(centers) <- yrs_keep
    centers
  }
  
  .build_layout <- function(yrs_keep, scen_levels, first_scen, eps) {
    year_to_scen <- function(y) if (y == "2021") first_scen else scen_levels
    centers <- .compute_centers(yrs_keep)
    purrr::map_dfr(yrs_keep, function(y) {
      scs  <- year_to_scen(y)
      k    <- length(scs)
      offs <- (seq_len(k) - (k + 1) / 2) * eps   
      tibble::tibble(
        year   = factor(y, levels = yrs_keep),
        Source = factor(scs, levels = scen_levels),
        x      = centers[[y]] + offs
      )
    }) %>%
      dplyr::mutate(year = factor(year, levels = yrs_keep))
  }
  
  # ========= A Domestic =========
  df_domestic <- df_all %>%
    dplyr::filter(
      Variable == "regional natural gas by tech (nest)",
      trimws(region) == "Russia"
    ) %>%
    dplyr::select(Source, technology, dplyr::all_of(yrs_use)) %>%
    tidyr::pivot_longer(dplyr::all_of(yrs_use), names_to = "year", values_to = "value") %>%
    dplyr::mutate(
      year       = factor(year, levels = yrs_keep),
      Source     = factor(Source, levels = scen_levels),
      value      = .to_num(value),
      tech_clean = tolower(trimws(technology))
    ) %>%
    dplyr::filter(tech_clean == "domestic natural gas") %>%
    dplyr::transmute(
      Source, year, value,
      cat = factor("Domestic natural gas",
                   levels = c("Domestic natural gas","Exported LNG","Exported pipeline gas"))
    )
  
  # ========= B Exported =========
  df_export <- df_all %>%
    dplyr::filter(
      Variable == "exported gas by tech",
      trimws(region) == "Russia"
    ) %>%
    dplyr::select(Source, technology, dplyr::all_of(yrs_use)) %>%
    tidyr::pivot_longer(dplyr::all_of(yrs_use), names_to = "year", values_to = "value") %>%
    dplyr::mutate(
      year       = factor(year, levels = yrs_keep),
      Source     = factor(Source, levels = scen_levels),
      value      = .to_num(value),
      tech_clean = tolower(trimws(technology))
    ) %>%
    dplyr::filter(tech_clean %in% c("exported lng","exported pipeline gas")) %>%
    dplyr::mutate(
      cat = dplyr::case_when(
        tech_clean == "exported lng"          ~ "Exported LNG",
        tech_clean == "exported pipeline gas" ~ "Exported pipeline gas"
      ),
      cat = factor(cat, levels = c("Domestic natural gas","Exported LNG","Exported pipeline gas"))
    ) %>%
    dplyr::transmute(Source, year, value, cat)
  
  # ========= combine =========
  df_both <- dplyr::bind_rows(df_domestic, df_export) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(Source, year, cat) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      Source = factor(Source, levels = scen_levels),
      year   = factor(year, levels = yrs_keep)
    )
  
  # ========= 2021 only the first one =========
  df_sum <- dplyr::bind_rows(
    df_both %>% dplyr::filter(year == "2021", Source == first_scen),
    df_both %>% dplyr::filter(year != "2021")
  )

  eps      <- 0.05   
  year_gap <- 0.30   
  bar_w    <- 0.04   
  
  centers     <- .compute_centers(yrs_keep, year_gap)
  layout_df   <- .build_layout(yrs_keep, scen_levels, first_scen, eps)
  df_plot     <- df_sum %>% dplyr::left_join(layout_df, by = c("year","Source"))
  sec_centers <- as.numeric(centers)
  sec_labels  <- names(centers)
  
  x_breaks <- layout_df$x
  x_labels <- ifelse(as.character(layout_df$year) == "2021", "",
                     as.character(layout_df$Source))
  
  pal_cat <- c(
    "Domestic natural gas"   = "#1f77b4",
    "Exported LNG"           = "#F38400",
    "Exported pipeline gas"  = "#2ca02c"
  )
  p1 <- ggplot(df_plot, aes(x = x, y = value, fill = cat)) +
    geom_col(width = bar_w, position = "stack") +
    scale_fill_manual(values = pal_cat, name = "Category") +
    scale_x_continuous(
      breaks = x_breaks, labels = x_labels,
      expand = expansion(mult = c(0.01, 0.02)),
      sec.axis = dup_axis(name = NULL, breaks = sec_centers, labels = sec_labels)
    ) +
    labs(x = NULL, y = "EJ", title = "Russia natural gas production") +
    theme_bw(base_size = 12) +
    theme(
      legend.position    = "right",
      legend.key.height  = unit(10, "pt"),
      legend.key.width   = unit(14, "pt"),
      axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.x.top    = element_text(angle = 0, hjust = 0.5, vjust = 0.5,
                                        margin = margin(b = 4)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin        = margin(t = 18, r = 30, b = 12, l = 10, unit = "pt")
    ) +
    coord_cartesian(clip = "off")

  ggplot2::ggsave(
    file.path(output_dir, "IAMC_RUS_domestic_plus_exported.png"),
    plot = p1, width = 7.6, height = 4.8, bg = "white", dpi = 300
  )
  
  
  ######################################################################################
  ########################## China traded LNG and pipeline (slide 8) ###################
  ######################################################################################


  stack_order <- c("Australia",
                   "Middle East",
                   "Other low-methane producers",
                   "Russia",
                   "Other high-methane producers")
  
  # grouping
  group_lng_source <- function(tech) {
    base <- sub("\\s*traded\\s*LNG\\s*$", "", trimws(tech))
    if (base %in% c("Australia_NZ","Australia")) return("Australia")
    if (base %in% c("Middle East"))              return("Middle East")
    if (base %in% c("Russia"))                   return("Russia")
    other_high <- c(
      "Ukraine","South Korea","USA","South America_Southern","Indonesia",
      "Central Asia","Africa_Northern","Africa_Western","Central America and Caribbean",
      "India","EU-15","Brazil","Africa_Southern","Colombia"
    )
    if (base %in% other_high) return("Other high-methane producers")
    other_low  <- c("Canada","European Free Trade Association","Southeast Asia","Japan")
    if (base %in% other_low)  return("Other low-methane producers")
    return(base)
  }
  
  # color
  pal_group <- c(
    "Australia" = "#222222",
    "Middle East" = "#F3C300",
    "Russia" = "#F38400",
    "Other high-methane producers" = "#9E9E9E",
    "Other low-methane producers"  = "#C7C7C7"
  )
  yrs_keep_6 <- c("2021","2025","2030","2035","2050","2060")
  yrs_use_6  <- intersect(yrs_keep_6, names(df_all))
  
  if (!exists("scen_levels")) {
    scen_levels <- c("NetZero_default", "NetZero_core", "NetZero_CH4_CN", "NetZero_CH4_group")
  }
  
  .to_num <- function(x) suppressWarnings(as.numeric(x))
  
  compute_centers <- function(yrs_keep, year_gap = 0.35) {
    centers <- numeric(length(yrs_keep))
    centers[1] <- year_gap
    if (length(yrs_keep) >= 2) {
      for (i in 2:length(yrs_keep)) {
        add <- if (yrs_keep[i-1] == "2021" && yrs_keep[i] == "2025") year_gap/1.5 else year_gap
        centers[i] <- centers[i-1] + add
      }
    }
    names(centers) <- yrs_keep
    as.numeric(centers)
  }
  
  add_x_positions <- function(df, yrs_keep, scen_levels, eps, centers_vec) {
    scen_index <- match(df$Source, scen_levels)
    df2 <- df %>%
      dplyr::mutate(
        grp_center = centers_vec[match(as.character(year), yrs_keep)],
        x = grp_center + ifelse(year == "2021", 0,
                                (scen_index - mean(seq_len(length(scen_levels)))) * eps)
      )
    x_ticks  <- df2 %>% dplyr::distinct(x, year, Source) %>% dplyr::arrange(year, x)
    list(
      df2 = df2,
      x_breaks = x_ticks$x,
      x_labels = ifelse(x_ticks$year == "2021", "", as.character(x_ticks$Source))
    )
  }
  
  add_right_strip <- function(p, label, w_bar_data = 0.05) {
    gb <- ggplot2::ggplot_build(p)
    panel_params <- gb$layout$panel_params[[1]]
    xr <- if (!is.null(panel_params$x.range)) panel_params$x.range else {
      c(panel_params$x$range$range[1], panel_params$x$range$range[2])
    }
    x_span <- diff(xr)
    w_npc  <- w_bar_data / x_span
    w_npc  <- max(min(w_npc, 0.12), 0.01)
    p +
      ggplot2::annotation_custom(
        grob = grid::rectGrob(
          x = grid::unit(1, "npc"), y = grid::unit(0.5, "npc"),
          width = grid::unit(w_npc, "npc"), height = grid::unit(1, "npc"),
          just = c("right", "center"),
          gp = grid::gpar(fill = "#E5E5E5", col = "grey50", lwd = 0.6)
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) +
      ggplot2::annotation_custom(
        grob = grid::textGrob(
          label,
          x = grid::unit(1, "npc") - grid::unit(w_npc/2, "npc"),
          y = grid::unit(0.5, "npc"),
          rot = 90,
          gp  = grid::gpar(fontface = "bold", cex = 0.9)
        ),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
  }
  
  plot_df_lng_grouped <- df_all %>%
    dplyr::filter(
      Variable == "traded gas by tech",
      trimws(region) == "China",
      stringr::str_detect(technology, "LNG")
    ) %>%
    dplyr::select(Source, technology, dplyr::all_of(yrs_use)) %>%
    tidyr::pivot_longer(dplyr::all_of(yrs_use), names_to = "year", values_to = "value") %>%
    dplyr::mutate(
      year   = factor(year, levels = yrs_keep),
      Source = factor(Source, levels = scen_levels),
      value  = .to_num(value),
      group  = factor(vapply(technology, group_lng_source, FUN.VALUE = character(1)),
                      levels = stack_order)
    ) %>%
    dplyr::group_by(Source, year, group) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  eps_lng      <- 0.06
  year_gap_lng <- 0.35
  centers2     <- compute_centers(yrs_keep, year_gap_lng)
  
  plot_df_lng_grouped2 <- plot_df_lng_grouped %>%
    dplyr::filter(year %in% yrs_keep) %>%
    dplyr::filter((year == "2021" & Source == scen_levels[1]) | year != "2021")
  
  lng_pos <- add_x_positions(plot_df_lng_grouped2, yrs_keep, scen_levels, eps_lng, centers2)
  x_breaks <- lng_pos$x_breaks
  x_labels <- lng_pos$x_labels
  
  p_lng_grouped <- ggplot2::ggplot(
    lng_pos$df2, ggplot2::aes(x = x, y = value, fill = group)
  ) +
    ggplot2::geom_col(width = 0.05, position = "stack") +
    ggplot2::scale_fill_manual(
      values = pal_group,
      breaks = stack_order,
      name   = "LNG Source"
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      expand = ggplot2::expansion(mult = c(0.01, 0.02)),
      sec.axis = ggplot2::dup_axis(
        name   = NULL,
        breaks = centers2,
        labels = yrs_keep
      )
    ) +
    ggplot2::labs(x = NULL, y = "EJ", title = "Traded LNG – China") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position    = "right",
      legend.key.height  = grid::unit(10, "pt"),
      legend.key.width   = grid::unit(14, "pt"),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.x.top    = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 0.5,
                                                 margin = ggplot2::margin(b = 4)),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(t = 18, r = 35, b = 12, l = 8, unit = "pt")
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  ggplot2::ggsave(
    file.path(output_dir, "IAMC_Traded_LNG_China.png"),
    plot = p_lng_grouped, width = 6.2, height = 4.8, bg = "white", dpi = 300
  )
  
  ################### Traded pipeline - China ##################################
  
  stack_order_pipe <- c("Russia", "Central Asia", "Southeast Asia")
  pal_group_pipe <- c(
    "Russia"        = "#F38400",
    "Central Asia"  = "#bcbd22",
    "Southeast Asia"= "#CC79A7"
  )
  
  extract_region_from_pipe <- function(tech) {
    gsub("\\s*traded\\s*pipeline(\\s*gas)?\\s*$", "", trimws(tech), ignore.case = TRUE)
  }
  
  plot_df_pipe_grouped <- df_all %>%
    dplyr::filter(
      Variable == "traded gas by tech",
      trimws(region) == "China",
      stringr::str_detect(technology, regex("pipeline", ignore_case = TRUE))
    ) %>%
    dplyr::select(Source, technology, dplyr::all_of(yrs_use)) %>%
    tidyr::pivot_longer(dplyr::all_of(yrs_use), names_to = "year", values_to = "value") %>%
    dplyr::mutate(
      year   = factor(year, levels = yrs_keep),
      Source = factor(Source, levels = scen_levels),
      value  = .to_num(value),
      group0 = extract_region_from_pipe(technology)
    ) %>%
    dplyr::filter(group0 %in% stack_order_pipe) %>%
    dplyr::group_by(Source, year, group0) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group = factor(group0, levels = stack_order_pipe))
  
  eps_pipe      <- 0.06
  year_gap_pipe <- 0.35
  centers_pipe  <- compute_centers(yrs_keep, year_gap_pipe)
  
  plot_df_pipe_grouped2 <- plot_df_pipe_grouped %>%
    dplyr::filter(year %in% yrs_keep) %>%
    dplyr::filter((year == "2021" & Source == scen_levels[1]) | year != "2021")
  
  pipe_pos <- add_x_positions(plot_df_pipe_grouped2, yrs_keep, scen_levels, eps_pipe, centers_pipe)
  
  p_pipe_grouped <- ggplot2::ggplot(
    pipe_pos$df2, ggplot2::aes(x = x, y = value, fill = group)
  ) +
    ggplot2::geom_col(width = 0.05, position = "stack") +
    ggplot2::scale_fill_manual(
      values = pal_group_pipe,
      breaks = stack_order_pipe,
      limits = stack_order_pipe,
      name   = "Pipeline Source"
    ) +
    ggplot2::scale_x_continuous(
      breaks = pipe_pos$x_breaks,
      labels = pipe_pos$x_labels,
      expand = ggplot2::expansion(mult = c(0.01, 0.02)),
      sec.axis = ggplot2::dup_axis(
        name   = NULL,
        breaks = centers_pipe,
        labels = yrs_keep
      )
    ) +
    ggplot2::labs(x = NULL, y = "EJ", title = "Traded Pipeline Gas – China") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position    = "right",
      legend.key.height  = grid::unit(10, "pt"),
      legend.key.width   = grid::unit(14, "pt"),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.x.top    = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 0.5,
                                                 margin = ggplot2::margin(b = 4)),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(t = 18, r = 35, b = 12, l = 8, unit = "pt")
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  ggplot2::ggsave(
    file.path(output_dir, "IAMC_Traded_pipeline_China.png"),
    plot = p_pipe_grouped, width = 6.2, height = 4.8, bg = "white", dpi = 300
  )
  
  #################################### combine ##################################
  
  stack_order_lng  <- stack_order
  stack_order_pipe <- c("Russia", "Central Asia", "Southeast Asia")
  ordered_union    <- c(stack_order_lng, setdiff(stack_order_pipe, stack_order_lng))
  
  present_lng  <- unique(as.character(lng_pos$df2$group))
  present_pipe <- unique(as.character(pipe_pos$df2$group))
  present_all  <- unique(c(present_lng, present_pipe))
  
  legend_breaks <- c(
    ordered_union[ordered_union %in% present_all],
    setdiff(sort(present_all), ordered_union)
  )
  
  pal_all <- c(
    pal_group,
    pal_group_pipe[ setdiff(names(pal_group_pipe), names(pal_group)) ]
  )
  missing_all <- setdiff(legend_breaks, names(pal_all))
  if (length(missing_all) > 0) {
    add_cols <- grDevices::hcl.colors(length(missing_all), palette = "Dark 3")
    names(add_cols) <- missing_all
    pal_all <- c(pal_all, add_cols)
  }
  pal_legend <- pal_all[legend_breaks]
  
  # LNG
  p_lng_u <- ggplot2::ggplot(
    lng_pos$df2, ggplot2::aes(x = x, y = value, fill = group)
  ) +
    ggplot2::geom_col(width = 0.05) +
    ggplot2::scale_fill_manual(values = pal_legend, breaks = present_lng, limits = present_lng,
                               drop = TRUE, name = "Source") +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = rep("", length(x_breaks)),
      sec.axis = ggplot2::dup_axis(name = NULL, breaks = centers2, labels = yrs_keep),
      expand = ggplot2::expansion(mult = c(0.01, 0.10))
    ) +
    ggplot2::labs(x = NULL, y = "Natural Gas (EJ)") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position    = "right",
      legend.key.height  = grid::unit(10, "pt"),
      legend.key.width   = grid::unit(14, "pt"),
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(),
      axis.text.x.top    = ggplot2::element_text(size = 9, margin = ggplot2::margin(b = 4)),
      plot.title         = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  # PIPElINE
  p_pipe_u <- ggplot2::ggplot(
    pipe_pos$df2, ggplot2::aes(x = x, y = value, fill = group)
  ) +
    ggplot2::geom_col(width = 0.05) +
    ggplot2::scale_fill_manual(values = pal_legend, breaks = present_pipe, limits = present_pipe,
                               drop = TRUE, name = "Source") +
    ggplot2::scale_x_continuous(
      breaks = pipe_pos$x_breaks,
      labels = pipe_pos$x_labels,
      expand = ggplot2::expansion(mult = c(0.01, 0.10))
    ) +
    ggplot2::labs(x = NULL, y = "Natural Gas (EJ)") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position    = "right",
      legend.key.height  = grid::unit(10, "pt"),
      legend.key.width   = grid::unit(14, "pt"),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      plot.title         = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  p_lng_u_strip  <- add_right_strip(p_lng_u,  "LNG",      w_bar_data = 0.05)
  p_pipe_u_strip <- add_right_strip(p_pipe_u, "Pipeline", w_bar_data = 0.05)
  
  library(patchwork)
  p_combined_aligned <- (p_lng_u_strip / p_pipe_u_strip) +
    patchwork::plot_layout(heights = c(1, 1)) +
    patchwork::plot_annotation(
      title = "Traded LNG and pipeline - China",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0)
      )
    )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "IAMC_China_traded_LNG_and_pipeline.png"),
    plot = p_combined_aligned, width = 7.2, height = 6.0, dpi = 300, bg = "white"
  )
  
  
  ########################################################################## 
  ################### Traded LNG - Japan & South Korea (slide 8) ###########
  #########################################################################

  scen_levels <- c("NetZero_default", "NetZero_core", "NetZero_CH4_CN", "NetZero_CH4_group")
  scen_labs   <- scen_levels
  
  stack_order <- c("Australia",
                   "Middle East",
                   "Other low-methane producers",
                   "Russia",
                   "Other high-methane producers")
  
  group_lng_source <- function(tech) {
    base <- sub("\\s*traded\\s*LNG\\s*$", "", trimws(tech))
    if (base %in% c("Australia_NZ","Australia")) return("Australia")
    if (base %in% c("Middle East"))              return("Middle East")
    if (base %in% c("Russia"))                   return("Russia")
    other_high <- c(
      "Ukraine","South Korea","USA","South America_Southern","Indonesia",
      "Central Asia","Africa_Northern","Africa_Western","Central America and Caribbean",
      "India","EU-15","Brazil","Africa_Southern","Colombia"
    )
    if (base %in% other_high) return("Other high-methane producers")
    other_low  <- c("Canada","European Free Trade Association","Southeast Asia","Japan")
    if (base %in% other_low)  return("Other low-methane producers")
    return(base)
  }
  
  pal_group <- c(
    "Australia" = "#222222",
    "Middle East" = "#F3C300",
    "Russia" = "#F38400",
    "Other high-methane producers" = "#9E9E9E",
    "Other low-methane producers"  = "#C7C7C7"
  )
  
  yrs_keep_6 <- c("2021","2025","2030","2035","2050","2060")
  yrs_use_6  <- intersect(yrs_keep_6, names(df_all))
  
  if (!exists("yrs_keep"))  yrs_keep  <- c("2021","2025","2035","2060")
  if (!exists("yrs_use"))   yrs_use   <- yrs_keep
  
  .to_num <- function(x) suppressWarnings(as.numeric(x))
  
  compute_centers <- function(yrs_keep, year_gap = 0.35) {
    centers <- numeric(length(yrs_keep))
    centers[1] <- year_gap
    if (length(yrs_keep) >= 2) {
      for (i in 2:length(yrs_keep)) {
        add <- if (yrs_keep[i-1] == "2021" && yrs_keep[i] == "2025") year_gap/1.5 else year_gap
        centers[i] <- centers[i-1] + add
      }
    }
    names(centers) <- yrs_keep
    centers
  }
  
  add_x_positions <- function(df, yrs_keep, scen_levels, eps, centers_named) {
    scen_index <- match(df$Source, scen_levels)
    df2 <- df %>%
      dplyr::mutate(
        grp_center = as.numeric(centers_named[as.character(year)]),
        x = grp_center + ifelse(year == "2021", 0,
                                (scen_index - mean(seq_len(length(scen_levels)))) * eps)
      )
    x_ticks  <- df2 %>% dplyr::distinct(x, year, Source) %>% dplyr::arrange(year, x)
    list(
      df2 = df2,
      x_breaks = x_ticks$x,
      x_labels = ifelse(x_ticks$year == "2021", "", as.character(x_ticks$Source))
    )
  }
  

  make_traded_lng_single <- function(target_region, plot_title, outfile_png) {

    plot_df_lng_grouped <- df_all %>%
      dplyr::filter(
        Variable == "traded gas by tech",
        trimws(region) == target_region,
        stringr::str_detect(technology, "LNG")
      ) %>%
      dplyr::select(Source, technology, dplyr::all_of(yrs_use)) %>%
      tidyr::pivot_longer(dplyr::all_of(yrs_use), names_to = "year", values_to = "value") %>%
      dplyr::mutate(
        year   = factor(year, levels = yrs_keep),
        Source = factor(Source, levels = scen_levels),
        value  = .to_num(value),
        group  = factor(vapply(technology, group_lng_source, FUN.VALUE = character(1)),
                        levels = stack_order)  
      ) %>%
      dplyr::group_by(Source, year, group) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    eps      <- 0.06
    year_gap <- 0.35
    centers2 <- compute_centers(yrs_keep, year_gap)
    
    plot_df_lng_grouped2 <- plot_df_lng_grouped %>%
      dplyr::filter(year %in% yrs_keep) %>%
      dplyr::filter((year == "2021" & Source == scen_levels[1]) | year != "2021")
    
    pos <- add_x_positions(plot_df_lng_grouped2, yrs_keep, scen_levels, eps, centers2)
    
    p <- ggplot2::ggplot(
      pos$df2, ggplot2::aes(x = x, y = value, fill = group)
    ) +
      ggplot2::geom_col(width = 0.05, position = "stack") +
      ggplot2::scale_fill_manual(values = pal_group, breaks = stack_order, name = "LNG Source") +
      ggplot2::scale_x_continuous(
        breaks = pos$x_breaks,
        labels = pos$x_labels,
        expand = ggplot2::expansion(mult = c(0.01, 0.02)),
        sec.axis = ggplot2::dup_axis(name = NULL, breaks = as.numeric(centers2), labels = names(centers2))
      ) +
      ggplot2::labs(x = NULL, y = "EJ", title = plot_title) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        legend.position    = "right",
        legend.key.height  = grid::unit(10, "pt"),
        legend.key.width   = grid::unit(14, "pt"),
        axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.x.top    = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 0.5,
                                                   margin = ggplot2::margin(b = 4)),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        plot.margin        = ggplot2::margin(t = 18, r = 35, b = 12, l = 8, unit = "pt")
      ) +
      ggplot2::coord_cartesian(clip = "off")
    
    ggplot2::ggsave(
      file.path(output_dir, outfile_png),
      plot = p, width = 7.2, height = 4.8, bg = "white", dpi = 300
    )
  }
  
  make_traded_lng_single(
    target_region = "Japan",
    plot_title    = "Traded LNG – Japan",
    outfile_png   = "IAMC_Traded_LNG_Japan_together.png"
  )
  
  make_traded_lng_single(
    target_region = "South Korea",
    plot_title    = "Traded LNG – South Korea",
    outfile_png   = "IAMC_Traded_LNG_South_Korea_together.png"  
  )
  
  
  ############################################################################
  ################### Traded LNG - Europe  (slide 8) #########################
  ############################################################################
  
  scen_levels <- c("NetZero_default", "NetZero_core", "NetZero_CH4_CN", "NetZero_CH4_group")
  scen_labs   <- scen_levels
  
  europe_parts <- c("EU-12","EU-15","Europe_Non_EU","European Free Trade Association","EFTA")
  
  if (!exists("output_dir")) {
    output_dir <- if (exists("dir_output")) dir_output else getwd()
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (!exists("yrs_keep")) {
    yrs_keep <- names(df_all)[grepl("^\\d{4}$", names(df_all))]
    if (length(yrs_keep) == 0) stop("yrs_keep 未定义且未能自动识别年份列。")
  }
  if (!exists("yrs_use"))  yrs_use  <- intersect(yrs_keep, names(df_all))
  
  # ---------- grouping ----------
  stack_order_lng <- c(stack_order <- c("Australia",
                                          "Middle East",
                                          "Other low-methane producers",
                                          "Russia",
                                          "Other high-methane producers"))
  pal_group_lng <- c(
    "Australia" = "#222222",
    "Middle East" = "#F3C300",
    "Russia" = "#F38400",
    "Other high-methane producers" = "#9E9E9E",
    "Other low-methane producers"  = "#C7C7C7"
  )
  
  .to_num <- function(x) suppressWarnings(as.numeric(x))
  
  normalize_name <- function(x) {
    x <- trimws(x)
    x <- gsub("(?i)\\s*traded\\s*LNG\\s*$", "", x, perl = TRUE)
    x <- gsub("\\s+", " ", x)
    x
  }
  
  group_lng_source <- function(tech) {
    base <- normalize_name(tech)
    if (base %in% c("Australia_NZ","Australia")) return("Australia")
    if (base %in% c("Middle East"))              return("Middle East")
    if (base %in% c("Russia"))                   return("Russia")
    other_high <- c(
      "Ukraine","South Korea","USA","South America_Southern","Indonesia",
      "Central Asia","Africa_Northern","Africa_Western","Africa_Eastern","Central America and Caribbean",
      "India","EU-15","Brazil","Africa_Southern","Colombia","Argentina","Taiwan","EU-12","Europe_Non_EU"
    )
    if (base %in% other_high) return("Other high-methane producers")
    other_low  <- c("Canada","European Free Trade Association","Southeast Asia","Japan","China","South Africa","Mexico")
    if (base %in% other_low)  return("Other low-methane producers")
    base
  }
  
  extract_region_from_pipe <- function(tech) {
    base <- gsub("(?i)\\s*traded\\s*pipeline(\\s*gas)?\\s*$", "", trimws(tech), perl = TRUE)
    gsub("\\s+", " ", base)
  }
  
  compute_centers <- function(yrs, year_gap = 0.35) {
    centers <- numeric(length(yrs)); centers[1] <- year_gap
    if (length(yrs) >= 2) {
      for (i in 2:length(yrs)) {
        add <- if (yrs[i-1] == "2021" && yrs[i] == "2025") year_gap/1.5 else year_gap
        centers[i] <- centers[i-1] + add
      }
    }
    names(centers) <- yrs
    centers
  }
  
  add_x_positions <- function(df, yrs, scen_levels, eps, centers_named) {
    scen_index <- match(df$Source, scen_levels)
    df2 <- df %>%
      dplyr::mutate(
        grp_center = as.numeric(centers_named[as.character(year)]),
        x = grp_center + ifelse(year == "2021", 0,
                                (scen_index - mean(seq_len(length(scen_levels)))) * eps)
      )
    x_ticks <- df2 %>% dplyr::distinct(x, year, Source) %>% dplyr::arrange(year, x)
    list(df2 = df2, x_breaks = x_ticks$x,
         x_labels = ifelse(x_ticks$year == "2021", "", as.character(x_ticks$Source)))
  }
  
  auto_palette_for_pipe <- function(present_pipe) {
    known_cols <- c("Central Asia" = "#bcbd22","Middle East" = "#F3C300","Russia" = "#F38400")
    unknowns <- setdiff(present_pipe, names(known_cols))
    add_cols <- if (length(unknowns) > 0) {
      cols <- grDevices::hcl.colors(length(unknowns), palette = "Dark 3")
      names(cols) <- unknowns; cols
    } else c()
    c(known_cols[names(known_cols) %in% present_pipe], add_cols)
  }
  
  add_right_strip <- function(p, label, w_bar_data = 0.05) {
    gb <- ggplot2::ggplot_build(p)
    panel_params <- gb$layout$panel_params[[1]]
    xr <- if (!is.null(panel_params$x.range)) panel_params$x.range else {
      c(panel_params$x$range$range[1], panel_params$x$range$range[2])
    }
    x_span <- diff(xr); w_npc <- max(min(w_bar_data / x_span, 0.10), 0.008)
    p +
      ggplot2::annotation_custom(
        grob = grid::rectGrob(x = grid::unit(1, "npc"), y = grid::unit(0.5, "npc"),
                              width = grid::unit(w_npc, "npc"), height = grid::unit(1, "npc"),
                              just = c("right","center"),
                              gp = grid::gpar(fill = "#E5E5E5", col = "grey50", lwd = 0.6)),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      ) +
      ggplot2::annotation_custom(
        grob = grid::textGrob(label,
                              x = grid::unit(1, "npc") - grid::unit(w_npc/2, "npc"),
                              y = grid::unit(0.5, "npc"), rot = 90,
                              gp = grid::gpar(fontface = "bold", cex = 0.9)),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
  }
  
  eps       <- 0.06
  year_gap  <- 0.35
  centers   <- compute_centers(yrs_keep, year_gap)
  sec_ticks <- as.numeric(centers)
  
  # =========================== A LNG ===========================
  plot_df_lng_grouped_eu <- df_all %>%
    dplyr::filter(
      Variable == "traded gas by tech",
      trimws(region) %in% europe_parts,
      stringr::str_detect(technology, stringr::regex("LNG", ignore_case = TRUE))
    ) %>%
    dplyr::mutate(src_base = normalize_name(technology)) %>%
    dplyr::filter(!(src_base %in% europe_parts)) %>%
    dplyr::select(Source, technology, src_base, dplyr::all_of(yrs_use)) %>%
    tidyr::pivot_longer(dplyr::all_of(yrs_use), names_to = "year", values_to = "value") %>%
    dplyr::mutate(
      year   = factor(year, levels = yrs_keep),
      Source = factor(Source, levels = scen_levels),
      value  = .to_num(value),
      group0 = vapply(technology, group_lng_source, FUN.VALUE = character(1))
    ) %>%
    dplyr::group_by(Source, year, group0) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(group = factor(group0, levels = stack_order_lng))
  
  plot_df_lng_grouped2_eu <- plot_df_lng_grouped_eu %>%
    dplyr::filter(year %in% yrs_keep) %>%
    dplyr::filter((year == "2021" & Source == scen_levels[1]) | year != "2021")
  
  lng_pos <- add_x_positions(plot_df_lng_grouped2_eu, yrs_keep, scen_levels, eps, centers)
  
  present_lng_eu <- lng_pos$df2 %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(tot > 0) %>%
    dplyr::pull(group) %>%
    as.character() %>%
    intersect(stack_order_lng)
  pal_legend_lng <- pal_group_lng[present_lng_eu]
  
  p_lng_eu <- ggplot2::ggplot(lng_pos$df2, ggplot2::aes(x = x, y = value, fill = group)) +
    ggplot2::geom_col(width = 0.05) +
    ggplot2::scale_fill_manual(values = pal_legend_lng, breaks = present_lng_eu, limits = present_lng_eu,
                               drop = TRUE, name = "Source") +
    ggplot2::scale_x_continuous(
      breaks = lng_pos$x_breaks, labels = lng_pos$x_labels,
      sec.axis = ggplot2::dup_axis(name = NULL, breaks = sec_ticks, labels = yrs_keep),
      expand  = ggplot2::expansion(mult = c(0.01, 0.10))
    ) +
    ggplot2::labs(x = NULL, y = "Natural Gas (EJ)") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      legend.position    = "right",
      legend.key.height  = grid::unit(10, "pt"),
      legend.key.width   = grid::unit(14, "pt"),
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(),
      axis.text.x.top    = ggplot2::element_text(size = 9, margin = ggplot2::margin(b = 4)),
      plot.title         = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(clip = "off")
  
  
  invisible(list(nest = p1, 
                 traded_pipe = p_pipe, 
                 rus_exported_lng = p_rus_lng, 
                 rus_exported_pipe = p_rus_pip,
                 rus_supply_ERU = p_rus_supply_EUR,
                 rus_supply_default = p_rus_supply_default,
                 rus_export = p_rus_exp
                 ))
}