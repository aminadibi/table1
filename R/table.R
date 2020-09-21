#' Takes a dataset and returns Table 1 with graphics for distibutions
#' @return table1
#' @export
table1 <- function(dataset, ContVarList, CatVarList, group = NULL) {

  # prepare content
  #statcope <- haven::read_sas("H:/SAS Projects/Exacerbation Three Trials/3DataSets/STATCOPE/statcope.sas7bdat", NULL)

  ## Covariates summary statistics
  DataSummary <- DataSummary_Fnc(dataset = AllTrials_Data_Table1, ContVarList, CatVarList, group)
  

  if (! is.null(group)) {
    n_group <- length(table(dataset[ , group]))
    DataSummary_temp <- t(DataSummary[ , - c(1 : 2)])
    colnames(DataSummary_temp) <- DataSummary[ , 1]
  }
  else {
    n_group <- 0
    DataSummary_temp <- matrix(DataSummary, ncol = 1)
    colnames(DataSummary_temp) <- "Statistics"
  }
  
  DataSummary_temp <- as.data.frame(DataSummary_temp)
  DataSummary_temp$Distribution <- NA
  
  
  dplyr::tibble(Covariate = rownames(DataSummary_temp), DataSummary_temp) %>% gt()  ->  gt_temp

  
  for (varTemp in ContVarList) {
    gt_temp %>%
    text_transform(locations = cells_body(columns = vars("Distribution"), rows = rownames(DataSummary_temp) == varTemp),
                   fn = function(x) ggplot_density(dataset = dataset, group = group, VarName = varTemp, Height = px(50))) -> gt_temp
  }
  for (varTemp in CatVarList) {
    gt_temp %>%
      text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == varTemp),
                     fn = function(x) ggplot_bar(dataset = dataset, group = group, VarName = varTemp, Height = px(50))) -> gt_temp
  }
  
  gt_temp %>%
    fmt_number(columns = c(((n_group > 0) + 1) : ((n_group > 0) * n_group + 1)), decimals = 2, suffixing = TRUE) %>%
    fmt_percent(columns = c(((n_group > 0) + 1) : ((n_group > 0) * n_group + 1)), rows = rownames(DataSummary_temp) %in% CatVarList,
                drop_trailing_zeros = TRUE)    ->   Table1_gt ##%>%
    # tab_style(locations = cells_body(vars("ECLIPSE")), style = cell_text(color = "red")) %>%
    # tab_style(locations = cells_body(vars("SUMMIT")), style = cell_text(color = "green")) %>%
    # tab_style(locations = cells_body(vars("TORCH")), style = cell_text(color = "blue"))   ->   Table1_gt
  
  Table1_gt
  
}


ggplot_density <- function(dataset, group, VarName, Height = px(50)) {
  
  gg_object <- ggplot(data = dataset,
                      aes(x = !!sym(VarName), fill = group)) +
    geom_density(alpha = 0.4) +
    scale_color_grey() + theme_classic() +
    scale_x_continuous(breaks = breaks_pretty(3)) +
    theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line.y = element_blank(), text = element_text(size = 80), legend.position="none")
  gg_object %>% ggplot_image(height = Height)
  
}


ggplot_bar <- function(dataset, group, VarName, Height = px(50)) {
  
  gg_object = ggplot(data = dataset,
                     aes(x = !!sym(VarName), fill = group)) +
    geom_bar(stat = "count", width = 0.5, position = position_dodge(width = 0.9)) +
    scale_color_grey() + theme_classic() +
    theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line.y = element_blank(), text = element_text(size = 65), legend.position="none")
  gg_object %>% ggplot_image(height = Height)
  
}


DataSummary_Fnc <- function(dataset, ContVarList, CatVarList, group = NULL) {
  
  if (! is.null(group)) dataset %>% group_by(!!sym(group)) -> dataset

  
  as.data.frame(dataset %>% summarise(Size = n()))   -> DataSummary
  
  for (varTemp in ContVarList) {
    DataSummary <- cbind(DataSummary,
                         as.data.frame(dataset %>% summarise(Temp = mean(!!sym(varTemp))))[ , ((!is.null(group)) + 1)])
  }
  for (varTemp in CatVarList) {
    level_temp <- levels(as.data.frame(dataset)[ , varTemp])[2]
    DataSummary <- cbind(DataSummary,
                         as.data.frame(dataset %>% summarise(Temp = mean(!!sym(varTemp) == level_temp)))[ , ((!is.null(group)) + 1)])
  }
  
  if (is.null(group)) colnames(DataSummary) <- c(colnames(DataSummary)[1], ContVarList, CatVarList)
  else colnames(DataSummary) <- c(colnames(DataSummary)[c(1, 2)], ContVarList, CatVarList)
  
  return(DataSummary)
}

