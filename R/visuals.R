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

  gg_rprt <- df %>%
    dplyr::select(OU, indicator, period,
                  reported_by, site_count) %>%
    dplyr::filter(period == max(period),
                  reported_by %in% c("Both", "PEPFAR")) %>%
    dplyr::mutate(site_type = ifelse(reported_by == "Both",
                                     "Matched sites",
                                     "Unmatched sites")) %>%
    ggplot2::ggplot(aes(x = indicator, y = site_count,
                        fill = site_type, group = site_type)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ site_type, nrow = 1) +
    labs(title = "How many PEPFAR sites have matching MoH sites?",
         subtitle = glue::glue("Totals for matched and unmatched \\
                               sites in {graph_fy}")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.15, size = 22),
          plot.subtitle = element_text(hjust = 0.15, size = 18),
          axis.title = element_blank(),
          text = element_text(size = 18),
          axis.line.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none")

  return(gg_rprt)
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
    labs(title = "How are sites alinging?",
         subtitle = "Unweighted concordance",
         alt = "",
         x = "Number of patients reported by PEPFAR\n Note: Only facilities that are not 100% concordant are shown in this scatterplot."
    ) +
    theme_minimal() +
    # scale_color_viridis_d(name = "Indicator") +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          plot.subtitle = element_text(hjust = 0.5, size = 18),
          text = element_text(size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_blank(),
          panel.grid = element_blank())

  fig <- ggplotly(unweighted_scatter, tooltip = "text") %>%
    config(displayModeBar = F)

  return(fig)
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
         subtitle = "Weighted Average Concordance, FY2018-FY2022") +
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
      difference = pepfar - moh) %>%
    dplyr::select(Facility = Facility_hierarchy,
                  Indicator = indicator,
                  Period = period,
                  MOH = moh,
                  PEPFAR = pepfar,
                  Difference = difference,
                  `Weighted concordance (OU Level)` = OU_Concordance) %>%
    DT::datatable(options = list(pageLength = 20,
                                 order = list(list(6, 'desc'))
                                 ),
                  rownames = FALSE) %>%
    DT::formatPercentage(
      columns = c("Weighted concordance (OU Level)"), digits = 5
    )

  return(t)
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
