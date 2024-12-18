#' Country Summary Table
#'
#' @param d List object containing data
#' @param filter_values List of values for appropriate filters
#'
#' @return Returns a table summarizing data for the country at the indicator
#' and fiscal year level
#' @export
#'
country_summary <- function(d, filter_values) {

  if (is.null(d) || is.null(d$combined_data)) {
    return(NULL)
  }

  df <- d %>%
    purrr::pluck("combined_data") %>%
    dplyr::filter(reported_by == "Both") %>%
    dplyr::group_by(OU, indicator, period) %>%
    dplyr::summarise(
      MOH_total = sum(moh),
      PEPFAR_total = sum(pepfar),
      MOH_aligned = dplyr::n(),
      Concordance = sum(OU_Concordance),
      .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(x = .,
                     y = d$import_history %>%
                       dplyr::filter(OU == d$ou_name),
                     by = c("OU", "period", "indicator")) %>%
    table_filter(de_filter = filter_values$vz_de_filter,
                 pe_filter = filter_values$vz_pe_filter)

  return(df)
}

#' Summary of matched sites
#'
#' @description Summary table of country data
#'
#' @param d List object containing data
#' @param filter_values List of values for appropriate filters
#'
#' @return
#' @export
#'
matching_summary <- function(d, filter_values) {

  if (is.null(d) || is.null(d$combined_data)) {
    return(NULL)
  }

  df <- d %>%
    purrr::pluck("combined_data") %>%
    dplyr::group_by(OU, indicator, period, reported_by) %>%
    dplyr::summarise(
      site_count = dplyr::n(),
      MOH_total = sum(moh),
      PEPFAR_total = sum(pepfar),
      Concordance = sum(OU_Concordance),
      .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::full_join(x = .,
                     y = d$import_history %>%
                       dplyr::filter(OU == d$ou_name),
                     by = c("OU", "period", "indicator")) %>%
    table_filter(de_filter = filter_values$vz_de_filter,
                 pe_filter = filter_values$vz_pe_filter)

  return(df)
}

#' Site Reporting Graph
#'
#' @param df Dataframe that has been prepared using the `matching_summary` function.
#'
#' @return ggplot2 object
#' @export
#'
site_reporting_graph <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }

  graph_fy <- max(df$period)

  # Check if there is at least one row of data for "PEPFAR" in the specified year
  pepfar_data <- df %>%
    filter(reported_by == "PEPFAR", period == graph_fy)

  if (nrow(pepfar_data) == 0 || length(unique(df$period)) > 1) {
    plot.new()
    message1 <- expression(bold("Note: Please filter to a single fiscal year (panel to the left)."))
    message2 <- "The graph will only be rendered when selecting years for which Ministry data was provided."
    text(0.5, 0.8, message1, col = "#2D481E", cex = 1.5)
    text(0.5, 0.7, message2, col = "#2D481E", cex = 1.5)
    par(mar = c(5, 0, 0, 0))
  } else {
      gg_rprt <- df %>%
        dplyr::select(OU, indicator, period,
                      reported_by, site_count) %>%
        dplyr::filter(reported_by %in% c("Both", "PEPFAR")) %>%
        dplyr::group_by(indicator, reported_by) %>%
        dplyr::summarise(site_count = sum(site_count, na.rm = TRUE))  %>%
        dplyr::ungroup() %>%
        dplyr::filter(reported_by %in% c("Both", "PEPFAR")) %>%
        dplyr::mutate(site_type = ifelse(reported_by == "Both",
                                         "Facilities reporting to both MOH and PEPFAR",
                                         "Facilities reporting to PEPFAR only")) %>%
        ggplot2::ggplot(aes(x = indicator, y = site_count,
                            fill = site_type, group = site_type)) +
        geom_col() +
        geom_text(aes(label = site_count), position = position_stack(vjust = 0.5)) +
        coord_flip() +
        facet_wrap(~ site_type, nrow = 1) +
        labs(title = "Are all facilities reporting to PEPFAR also accounted for in MOH reporting?",
             subtitle =  glue::glue("Facilities reporting in {graph_fy}")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.15, size = 22),
              plot.subtitle = element_text(hjust = 0.35, size = 18),
              axis.title = element_blank(),
              text = element_text(size = 18),
              axis.line.y = element_blank(),
              panel.grid = element_blank(),
              legend.position = "none")

      return(gg_rprt)
    }
  }

#' Interactive Scatter Plot
#'
#' @description Creates an interactive scatterplot showing
#' unweighted concordance value by facility, with indicators shown
#' in different colors.
#'
#' @param d List object containing data
#' @param filter_values List of values for appropriate filters
#'
#' @return ggplotly object
#' @export
#'

interactive_scatter <- function(d, filter_values) {
  if (is.null(d) || is.null(d$combined_data)) {
    return(NULL)
  }
  if (length(filter_values$vz_pe_filter) != 1) {
    warning_plot <- ggplot2::ggplot() +
      ggplot2::labs(
        title = "Note: Please filter to a single fiscal year (panel to the left)."
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          color = "#2D481E", size = 16, face = "bold", hjust = 0.1, vjust = -2
        ),
        axis.line = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        plot.margin = grid::unit(c(2, 1, 1, 1), "lines")
      )

    return(plotly::ggplotly(warning_plot, tooltip = NULL) %>%
             plotly::config(displayModeBar = F))
  }
  # Generate the scatterplot
  concordance_distributions <- d %>%
    purrr::pluck("combined_data") %>%
    dplyr::filter(reported_by == "Both") %>%
    dplyr::mutate(unweighted_concordance =
                    OU_Concordance / OU_weighting) %>%
    dplyr::select(Facility, indicator, period, pepfar, moh,
                  unweighted_concordance) %>%
    table_filter(de_filter = filter_values$vz_de_filter,
                 pe_filter = filter_values$vz_pe_filter)

  unweighted_scatter <- concordance_distributions %>%
    dplyr::filter(unweighted_concordance != 1) %>%
    ggplot2::ggplot(
      aes(x = pepfar, y = unweighted_concordance, color = indicator,
          text =
            paste(
              "Facility: ", Facility,
              "<br>Indicator: ", indicator,
              "<br>Period: ", period,
              "<br>PEPFAR Reported Total: ", pepfar,
              "<br>MOH Reported Total: ", moh,
              "<br>Unweighted Concordance: ",
              scales::percent(unweighted_concordance, accuracy = 0.01)
            ))) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(trans = "log10",
                       breaks = scales::trans_breaks("log10",
                                                     function(x) 10 ^ x,
                                                     n = 5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_hline(yintercept = 0.90) +
    geom_vline(xintercept = 100) +
    theme(
      axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5,
                                 size = 14, color = "black",
                                 margin = margin(l = 20, r = 20))) +
    #plot.margin = unit(c(1, 1, 1, 3), "cm")) +
    labs(title = "How well are sites aligning?",
         subtitle = "Unweighted concordance",
         alt = "",
         x = "Number of patients reported by PEPFAR",
         y = "Percent Concordance \u21E8") +
    theme_minimal() +
    labs(color = "Indicators") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 22),
      plot.subtitle = element_text(hjust = 0.5, size = 18),
      text = element_text(size = 14),
      axis.title.x = element_text(size = 12, vjust = 1, color = "black"),
      panel.grid = element_blank(),
      plot.margin = unit(c(1, 1, 1, 2), "cm"),
      axis.title.y = element_text(angle = -90, vjust = 1, hjust = -0.2)
    )
  # Convert scatterplot to plotly
scatterplot <- ggplotly(unweighted_scatter, tooltip = "text") %>%
plotly::layout(
  annotations = list(
    list(
      x = 0.5,
      y = -0.8,
      text = paste(
        #"<b>Number of patients reported by PEPFAR</b><br><br>",
        "<b>The scatterplot is based on data reported by PEPFAR on the number of patients across sites and their corresponding concordance levels.</b>",
        "<br><br>Quality Improvement Focus: Quadrant 4 sites are identified as priority areas for quality improvement interventions.<br>",
        "This is due to their combination of high patient volume and suboptimal concordance.",
        "<br><br><b>Quadrants:</b>",
        "<br>Quadrant 1. (top-right) Sites with high patient volume and high concordance (>90%).",
        "<br>Quadrant 2. (top-left) Sites with low patient volume and high concordance (>90%).",
        "<br>Quadrant 3. (bottom-left) Sites with low patient volume and low concordance (<90%).",
        "<br>Quadrant 4. (bottom-right) Sites with high patient volume and low concordance (<90%)."
      ),
      showarrow = FALSE,
      xref = "paper",
      yref = "paper",
      align = "center",
      font = list(size = 14)
    )
    ),
    margin = list(t = 70, b = 350, l = 120, r = 50)
  ) %>%
  plotly::config(displayModeBar = FALSE)
  return(scatterplot)
}

#' Generate Indicator Table
#'
#' @param df Dataframe that has been prepared using the `country_summary` function.
#'
#' @return A `gt` table object
#' @export
#'
indicator_table_rendering <- function(df) {

  if (is.null(df)) {
    return(NULL)
  }

  t <- df %>%
    dplyr::select(indicator, Period = period, MappingData = has_disag_mapping,
                  Mapping = has_mapping_result_data,
                  PEPFAR = PEPFAR_total, MOH = MOH_total,
                  MOH_aligned, OU, Concordance) %>%

    dplyr::mutate(Mapping = ifelse((is.na(MOH) | MOH == "None") & MappingData == "Coarse" & Period < 2022, "Mapping Coarse",
                                ifelse((is.na(MOH) | MOH == "None") & MappingData == "Fine" & Period < 2022, "Mapping Fine",
                                ifelse((is.na(MappingData) | MappingData == "None") & Period < 2022, "No Mapping",
                                ifelse(!is.na(MOH) & MappingData == "Fine" & Period < 2022, "Data Fine",
                                ifelse(!is.na(MOH) & MappingData == "Coarse" & Period < 2022, "Data Coarse", Mapping)))))) %>%

    dplyr::filter(!(indicator %in% c("TX_PVLS_NUM", "TX_PVLS_DEN") & Period < 2022)) %>%

    dplyr::select(indicator, Period, Mapping,
                  PEPFAR, MOH,
                  MOH_aligned, OU, Concordance) %>%
    apply_levels() %>%
    dplyr::arrange(indicator, Period) %>%
    dplyr::group_by(indicator) %>%
    gt::gt(rowname_col = "Period") %>%
    gt::tab_spanner(
      label = "Reported figures",
      columns = c(PEPFAR, MOH)
    ) %>%
    gt::cols_align(
      align = "center",
      columns = c(MOH_aligned, PEPFAR, MOH)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = px(12)),
      locations = gt::cells_body(
        columns = c(Period, Mapping, MOH_aligned,
                    PEPFAR, MOH, OU))
    ) %>%
    gt::fmt_number(columns = c(PEPFAR, MOH),
                   decimals = 0) %>%
    gt::fmt_percent(columns = c(Concordance, Concordance),
                    decimals = 2) %>%
    gt::fmt_missing(columns = everything(), missing_text = "—") %>%
    gt::cols_label(MOH_aligned = gt::md("No. of sites<br>reported by both")) %>%
    gt::tab_options(
      column_labels.font.size = px(16),
      table.font.size = px(14),
      data_row.padding = px(5)
    ) %>%
    gt::tab_footnote(
      footnote = paste0("PEPFAR and MoH totals only include sites where ",
                        "both sources reported figures."),
      locations = cells_column_spanners(spanners = "Reported figures")
    ) %>%
    gt::tab_footnote(
      footnote = paste0("Military figures are not included in PEPFAR total."),
      locations = cells_column_labels(columns = c(PEPFAR))
    )

  return(t)
}

#' Generate Concordance Chart
#'
#' @param df A dataframe prepared using the `country_summary` function.
#'
#' @return A ggplot2 object
#' @export
#'
concordance_chart <- function(df) {

  if (is.null(df)) {
    return(NULL)
  }
  min_period <- min(df$period)
  max_period <- max(df$period)

  df$period <- as.factor(df$period)

  y_min <- min(df$Concordance, na.rm = TRUE)
  y_min <- floor(y_min * 10) / 10

  g <- df %>%
    ggplot(aes(x = period, y = Concordance,
               group = indicator, color = indicator)) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    # geom_label_repel(data = subset(df, df$period == max(df$period)),
    #                  aes(label = indicator),
    #                  size = 5,
    #                  show.legend = FALSE,
    #                  nudge_x = 1,
    #                  na.rm = TRUE) +
    scale_x_discrete(name = "Reporting Year") +
    scale_y_continuous(breaks = seq(0, 1, .05),
                       labels = scales::percent_format(accuracy = 1)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    coord_cartesian(ylim = c(y_min, 1)) +
    labs(title = "Progress Towards System Alignment",
         subtitle = glue::glue("Weighted Average Concordance, FY{min_period} - FY{max_period}")) +
    theme_minimal() +
    # scale_color_viridis_d(name = "Indicator") +
    theme(plot.title = element_text(hjust = 0.15, size = 22),
          plot.subtitle = element_text(hjust = 0.15, size = 18),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          text = element_text(size = 18),
          legend.position = "bottom")

  return(g)
}

#' Prepare site table data
#'
#' @param d List object containing data
#' @param filter_values List of values for appropriate filters
#'
#' @return A `DT` table object
#' @export
#'
#'

site_table_data <- function(d, filter_values) {

  if (is.null(d) || is.null(d$combined_data)) {
    return(NULL)
  }

  t <- d %>%
    purrr::pluck("combined_data") %>%
    table_filter(de_filter = filter_values$vz_de_filter,
                 pe_filter = filter_values$vz_pe_filter) %>%
    dplyr::filter(reported_by == "Both") %>%
    dplyr::mutate(
      Facility_hierarchy = ifelse(is.na(SNU3),
                                  paste(OU, SNU1, SNU2, Facility, sep = "/"),
                                  paste(OU, SNU1, SNU2, SNU3, Facility, sep = "/")),
      difference = abs(pepfar - moh)) %>%
    dplyr::select(Facility = Facility_hierarchy,
                  Indicator = indicator,
                  Period = period,
                  MOH = moh,
                  PEPFAR = pepfar,
                  `Difference (absolute)` = difference) %>%
    DT::datatable(options = list(pageLength = 20,
                                 order = list(list(5, 'desc'))
    ),
    rownames = FALSE)

  return(t)
}

new_analysis_table_rendering <- function(d, filter_values) {
  if (is.null(d) || is.null(d$combined_data)) {
    return(NULL)
  }

  # Process the data
  processed_df <- d %>%
    purrr::pluck("combined_data") %>%
    table_filter(de_filter = filter_values$vz_de_filter,
                 pe_filter = filter_values$vz_pe_filter) %>%
    dplyr::group_by(OU, indicator, period) %>%
    dplyr::mutate(
      MOH = length(unique(Facility_UID[reported_by %in% c("Both", "MOH")])),
      'Both(MOH & PEPFAR)' = length(unique(Facility_UID[reported_by == "Both"])),
      PEPFAR = length(unique(Facility_UID[reported_by %in% c("Both", "PEPFAR")])),
      MOH_Facilities_SupportedBy_PEPFAR = round((length(Facility_UID[reported_by == "Both"]) /
                                                   length(Facility_UID[reported_by %in% c("Both", "MOH")])) * 100, 2),
      PEPFAR_Reported_Facilities_ReportedByMOH = round((length(Facility_UID[reported_by == "Both"]) /
                                                          length(Facility_UID[reported_by %in% c("Both", "PEPFAR")])) * 100, 2),
      MOH_Supported_By_pepfar = dplyr::case_when(
        sum(pepfar[reported_by %in% c("Both", "PEPFAR")], na.rm = TRUE) > 0 &
          sum(moh[reported_by %in% c("Both", "MOH")], na.rm = TRUE) > 0 ~
          round((sum(pepfar[reported_by %in% c("Both", "PEPFAR")], na.rm = TRUE) /
                   sum(moh[reported_by %in% c("Both", "MOH")], na.rm = TRUE)) * 100, 2),
        TRUE ~ NA_real_
      ),
      weighted_concordance = dplyr::case_when(
        sum(OU_Concordance[reported_by == "Both"], na.rm = TRUE) > 0 ~
          round((sum(OU_Concordance[reported_by == "Both"], na.rm = TRUE)) * 100, 2)
      ),
      absolute_difference = ifelse(
        sum(absolute_difference[reported_by == "Both"], na.rm = TRUE) > 0,
        sum(absolute_difference[reported_by == "Both"], na.rm = TRUE),
        NA_real_
      ),
      absolute_diff_mean = round(abs(absolute_difference / length(unique(Facility_UID[reported_by == "Both"]))), 0),
    ) %>%
    dplyr::mutate(
      # Apply comma formatting for display
      absolute_difference_formatted = scales::comma(absolute_difference)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(PEPFAR_Reported_Facilities_ReportedByMOH, MOH_Facilities_SupportedBy_PEPFAR),
        ~ ifelse(PEPFAR == 0 | MOH == 0, NA, .x)
      ),
      PEPFAR_facilities_not_reported_by_MOH = round(abs((PEPFAR_Reported_Facilities_ReportedByMOH / 100) - 1) * 100, 2)
    )  %>%
    dplyr::ungroup() %>%
    dplyr::distinct(OU, indicator, period, .keep_all = TRUE) %>%
    dplyr::select(-Facility, -Facility_UID, -reported_by, -OU_UID, -OU_Concordance, -OU_weighting, -SNU1, -SNU1_UID, -SNU2,
                  -SNU2_UID, -SNU3, -SNU3_UID, -SNU1_Concordance, -SNU2_Concordance, -EMR_Concordance, -emr_present, -moh_id,
                  -longitude, -latitude, -moh, -pepfar) %>%
    dplyr::left_join(., d$import_history,
                     by = c("OU", "period", "indicator")) %>%
    dplyr::mutate('MOH Indicator Disaggregation' = dplyr::case_when(
      !is.na(has_disag_mapping) & has_disag_mapping != "None" ~ has_disag_mapping,
      is.na(has_disag_mapping) | has_disag_mapping == "None" ~ has_mapping_result_data,
      TRUE ~ NA_character_  # Catch-all for any other cases
    ),
    'MOH Indicator Disaggregation' = stringr::str_remove(
      `MOH Indicator Disaggregation`, "^(Data |Mapping )"
    )
    ) %>%
    dplyr::select(-has_disag_mapping, -has_mapping_result_data, -has_results_data)

  # Sort by year in descending order, assuming 'period' contains the year
  processed_df <- processed_df %>%
    dplyr::arrange(desc(period))

  processed_df <- processed_df %>%
    dplyr::select(-OU, -PEPFAR_Reported_Facilities_ReportedByMOH)

  processed_df <- processed_df %>%
    dplyr::select(
      indicator,
      period,
      'MOH Indicator Disaggregation',
      MOH,
      PEPFAR,
      'Both(MOH & PEPFAR)',
      MOH_Facilities_SupportedBy_PEPFAR,
      PEPFAR_facilities_not_reported_by_MOH,
      MOH_Supported_By_pepfar,
      weighted_concordance,
      absolute_difference_formatted,
      absolute_diff_mean
    )
  processed_df$SpacerColumn <- ""
  processed_df$SpacerColumn2 <- ""
  rendered_table <- processed_df %>%
    gt::gt() %>%

    gt::tab_header(
      title = gt::md("The DAA dashboard provides national level analysis of your country’s reporting across the DAA indicators. This is a useful starting point for a general comparison of data sources and to identify potential areas for deeper investigation.\n
It is recommended to filter the dashboard to a single year to simplify the view.")
    ) %>%

    gt::tab_options(
      table.border.top.width = px(0),
      heading.border.bottom.width = px(0),
      heading.padding = px(15)
    ) %>%

    #labs(title = gt::md("**This is the Main Dashboard**")) %>%

    # Move the desired column (for spacing) to its desired position
    gt::cols_move(SpacerColumn, after = 'Both(MOH & PEPFAR)') %>%
    gt::cols_move(SpacerColumn2, after = PEPFAR_facilities_not_reported_by_MOH) %>%

    # Label columns
    gt::cols_label(
      MOH_Facilities_SupportedBy_PEPFAR = "% MOH Facilities Supported By PEPFAR",
      PEPFAR_facilities_not_reported_by_MOH = "% PEPFAR Facilities Not Reported By MOH",
      MOH_Supported_By_pepfar = '% MOH Results Supported By PEPFAR',
      weighted_concordance = 'Weighted Concordance',
      absolute_difference_formatted = 'Abs Difference',
      indicator = 'Indicator',
      absolute_diff_mean = 'Abs Diff Mean',
      SpacerColumn = "",
      SpacerColumn2 = ""
    ) %>%

    # Group columns under a spanner
    gt::tab_spanner(
      label = md("**Nbr facilities reported by:**"),
      columns = c(MOH, PEPFAR, 'Both(MOH & PEPFAR)')
    ) %>%
    gt::data_color(
      columns = MOH_Supported_By_pepfar,
      colors = scales::col_bin(
        palette = c("white", "lightcoral"),
        bins = c(0, 100, Inf)
      )
    ) %>%
    gt::data_color(
      columns = 'MOH Indicator Disaggregation',
      colors = scales::col_factor(
      palette = c("#BBDBA9", "#D2E7C6", "#FFF0C2", "#FFC2C9"),
        levels = c("Fine", "Fine (65+)", "Fine (50+)", "Coarse", "No Mapping"),
        domain = c("Fine", "Fine (65+)", "Fine (50+)", "Coarse", "No Mapping")
      )
    ) %>%

    # Format numbers and percentages
    gt::fmt_number(
      columns = c(MOH, PEPFAR, 'Both(MOH & PEPFAR)'),
      sep_mark = ",",
      decimals = 0
    ) %>%
    gt::fmt_number(
      columns = c(MOH_Facilities_SupportedBy_PEPFAR, PEPFAR_facilities_not_reported_by_MOH, MOH_Supported_By_pepfar, weighted_concordance),
      decimals = 2,
      pattern = "{x}%"
    ) %>%

    # Center align all columns
    gt::cols_align(
      align = "center",
      columns = everything()
    ) %>%

    # Add borders to the body (rows) with BLACK color, excluding SpacerColumns
    gt::tab_style(
      style = gt::cell_borders(
        sides = "all",
        color = "black",
        weight = px(1)
      ),
      locations = gt::cells_body(columns = -SpacerColumn)
    ) %>%

    # Remove top and bottom borders from the SpacerColumn
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom"),
        color = "transparent",
        weight = px(0)
      ),
      locations = gt::cells_body(columns = SpacerColumn)
    ) %>%

    # Add borders to left and right sides of SpacerColumn
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("left", "right"),
        color = "black",
        weight = px(1)
      ),
      locations = gt::cells_body(columns = SpacerColumn)
    ) %>%

    # Apply grey shading to SpacerColumn
    gt::tab_style(
      style = gt::cell_fill(
        color = "darkgrey"
      ),
      locations = gt::cells_body(columns = SpacerColumn)
    ) %>%

    # Remove top and bottom borders from the SpacerColumn2
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom"),
        color = "transparent",
        weight = px(0)
      ),
      locations = gt::cells_body(columns = SpacerColumn2)
    ) %>%

    # Add borders to left and right sides of SpacerColumn2
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("left", "right"),
        color = "black",
        weight = px(1)
      ),
      locations = gt::cells_body(columns = SpacerColumn2)
    ) %>%

    # Apply grey shading to SpacerColumn2
    gt::tab_style(
      style = gt::cell_fill(
        color = "darkgrey"
      ),
      locations = gt::cells_body(columns = SpacerColumn2)
    ) %>%

    # Apply borders to column labels
    gt::tab_style(
      style = gt::cell_borders(
        sides = "all",
        color = "black",
        weight = px(1)
      ),
      locations = gt::cells_column_labels(columns = everything())
    ) %>%

    # Make column labels bold
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%

    gt::tab_options(
      table.font.size = px(14),
      column_labels.font.size = px(16),
      data_row.padding = px(5)
    )

  return(rendered_table)
}

#' Generate Integrity Check Tables
#'
#' @description Creates the data integrity  check tables.
#'
#' @param d List object containing data
#' @param filter_values List of values for appropriate filters
#'
#' @return A `DT` table object
#' @export
#'
generate_integrity_table <- function(d, filter_values) {

  if (is.null(d) || is.null(filter_values$integrity_radio)) {
    return(NULL)
  }

  if ("integrity_nulls" %in% filter_values$integrity_radio &&
      !is.null(d$integrity_nulls)) {
    t <- d %>%
      purrr::pluck("integrity_nulls") %>%
      dplyr::filter(level3 == d$ou_name)
    if (nrow(t) > 0) {
      t %<>%
        dplyr::mutate(
          facility = ifelse(facility_level == 6,
                            paste(level3, level4, level5, level6, sep = "/"),
                            paste(level3, level4, level5,
                                  level6, level7, sep = "/"))
        ) %>%
        dplyr::select(facility_uid, facility, data_element, disagg, value) %>%
        DT::datatable(options = list(pageLength = 20), rownames = FALSE)
    } else {
      t <-
        data.frame(message= glue::glue("Congratulations! You do not have \\
                                     any nulls.")) %>%
        DT::datatable(options = list(pageLength = 20), rownames = FALSE)
    }
  } else  if ("integrity_duplicates" %in% filter_values$integrity_radio &&
              !is.null(d$integrity_nulls)) {
    t <- d %>%
      purrr::pluck("integrity_duplicates") %>%
      dplyr::filter(country_name == d$ou_name)
    if (nrow(t) > 0) {
      t %<>%
        dplyr::select(where(~ any(!is.na(.))), -country_name) %>%
        DT::datatable(options = list(pageLength = 20), rownames = FALSE)
    } else {
      t <-
        data.frame(message= glue::glue("Congratulations! You do not have \\
                                     any duplicates.")) %>%
        DT::datatable(options = list(pageLength = 20), rownames = FALSE)
    }
  } else {
    t <-
      data.frame(message= glue::glue("Uh oh! Something has gone wrong \\
                                     and the integrity check data could not \\
                                     be retrieved.")) %>%
      DT::datatable(rownames = FALSE)
  }

  return(t)
}

#' Apply filters to table
#'
#' @param df A dataframe to be filtered
#' @param de_filter A list of indicators (data elements) to be included in the filter
#' @param pe_filter A list of fiscal years (periods) to be included in the filter
#'
#' @return A filtered dataframe
#' @export
#'
table_filter <- function(df, de_filter, pe_filter) {
  if (!is.null(de_filter)) {
    df %<>%
      dplyr::filter(indicator %in% de_filter)
  }

  if (!is.null(pe_filter)) {
    df %<>%
      dplyr::filter(period %in% pe_filter)
  }

  return(df)
}

#' Order Indicators by Levels
#'
#' @param df A dataframe object to be ordered by indicators
#'
#' @return A dataframe that has had indicators converted to a factor
#' @export
#'
apply_levels <- function(df) {
  df$indicator <- factor(df$indicator,
                         levels = c("HTS_TST", "PMTCT_ART", "PMTCT_STAT",
                                    "TB_PREV_LEGACY", "TB_PREV",
                                    "TX_NEW", "TX_CURR", "TX_PVLS_NUM", "TX_PVLS_DEN"))
  return(df)
}

#' Extract Data from Pivot Table
#'
#' @param d A list object containing data
#'
#' @return A dataset plucked from the Pivot table current view
#' @export
#'
moh_pivot <- function(d) {

  pivot <- d %>%
    purrr::pluck("combined_data")

  if (is.null(d) || is.null(d$combined_data)) {
    return(NULL)
  }

  js_code <- "function(config) {Shiny.onInputChange('pivot_data',
              document.getElementById('pivot').innerHTML);
              }"

  rpivotTable(data = pivot,
              rows = c("indicator"),
              cols = c("period"),
              vals = "OU_Concordance",
              aggregatorName = "Sum",
              rendererName = "Table",
              width = "70%",
              height = "700px",
              onRefresh = htmlwidgets::JS(js_code)
  )

}

#' @export
#' @title Extract data from Pivot Table
#'
#' @description This code is based on the below GitHub comment
#' in the rpivotTable repository. It allows the user to access the data
#' in the pivot table so that it can be exported as a CSV.
#'
#' @param x Pivot table data from `input$pivot_data`
#'
#' @returns Data frame containing data from current pivot table view.
#'
extract_pivot_data <- function(x) {
  x %<>%
    xml2::read_html(.) %>%
    rvest::html_table(fill = TRUE) %>%
    .[[2]]
  return(x)
}
