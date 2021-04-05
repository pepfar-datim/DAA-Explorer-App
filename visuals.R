
indicator_table_data <- function(df) {

  df %<>%
    dplyr::filter(`Reported on by both` == "both") %>%
    dplyr::group_by(indicator, period) %>%
    dplyr::summarise(Sites = n(),
                     PEPFAR = sum(PEPFAR),
                     MOH = sum(MOH),
                     Difference = sum(Difference),
                     `Weighted difference` = sum(`Weighted difference`)) %>%
    dplyr::ungroup()

  return(df)

}

indicator_table_rendering <- function(df) {

  df %<>%
    gt::gt(rowname_col = "period") %>%
    gt::tab_spanner(
      label = "Reported figures",
      columns = vars(PEPFAR, MOH, Difference)
    ) %>%
    gt::cols_align(
      align = "center",
      columns = vars(Sites,
                     MOH,
                     PEPFAR,
                     Difference)
    ) %>%
    gt::tab_style(
      style = cell_text(size = px(12)),
      locations = cells_body(
        columns = vars(period,
                       Sites,
                       MOH,
                       PEPFAR,
                       Difference,
                       `Weighted difference`))
    ) %>%
    gt::fmt_number(columns = vars(MOH,
                                  PEPFAR,
                                  Difference),
                   decimals = 0) %>%
    gt::fmt_percent(columns = vars(`Weighted difference`),
                    decimals = 2) %>%
    gt::cols_label(
      Sites = md("No. of sites<br>reported by both")
    ) %>%
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
      locations = cells_column_labels(
        columns = vars(PEPFAR))
    )

  return(df)

}

discordance_chart <- function(df) {

  x_min <- min(df$period)
  x_max <- max(df$period)
  y_max <- max(df$`Weighted difference`, na.rm = TRUE) %>%
    log10(.) %>%
    ceiling(.) %>%
    `^`(10, .) %>%
    max(., 100)

  g <- df %>%
    ggplot2::ggplot(aes(x = period,
                        y = `Weighted difference`,
                        group = indicator,
                        color = indicator)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_x_continuous(name = "Year",
                                limits = c(x_min, x_max),
                                breaks = x_min:x_max,
                                labels = scales::number_format(accuracy = 1,
                                                               big.mark = "")) +
    ggplot2::scale_y_continuous(name = "Weighted Average Discordance",
                                breaks = seq(from = 0, to = y_max, by = .1),
                                labels = scales::percent) +
    ggplot2::labs(title = "Overall Weighted Average Discordance",
                  subtitle = "Changes in Discordance by Fiscal Year") +
    ggplot2::theme_bw() +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(plot.title = element_text(hjust = 0.5, size = 22),
                   plot.subtitle = element_text(hjust = 0.5, size = 14),
                   legend.position = "bottom")

  return(g)

}

site_table_data <- function(df, ou_uid, facility_level) {

  if (facility_level == 7) {

    df %<>%
      dplyr::filter(`Reported on by both` == "both") %>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5,
                      namelevel6, namelevel7, indicator,
                      period) %>%
      dplyr::summarise(PEPFAR = sum(PEPFAR),
                       MOH = sum(MOH),
                       Difference = sum(Difference),
                       `Weighted difference` = sum(`Weighted difference`)) %>%
      dplyr::ungroup()

  } else {

    df %<>%
      dplyr::filter(`Reported on by both` == "both") %>%
      dplyr::group_by(namelevel3, namelevel4, namelevel5,
                      namelevel6, indicator,
                      period) %>%
      dplyr::summarise(PEPFAR = sum(PEPFAR),
                       MOH = sum(MOH),
                       Difference = sum(Difference),
                       `Weighted difference` = sum(`Weighted difference`)) %>%
      dplyr::ungroup()

  }

  return(df)

}

moh_pivot <- function(d) {

  pivot <- d %>%
    purrr::pluck("analytics")

  js_code <- "function(config) {Shiny.onInputChange('pivotData',
              document.getElementById('pivot').innerHTML);
              }"

  rpivotTable(data = pivot,
              rows = c("indicator"),
              vals = "Weighted difference",
              aggregatorName = "Sum",
              rendererName = "Table",
              width = "70%",
              height = "700px",
              onRefresh = htmlwidgets::JS(js_code)
              )

}
