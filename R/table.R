#' Creates a sample table
#' @return sample table
#' @export
table1 <- function(dataset) {

  # prepare content
  #statcope <- haven::read_sas("H:/SAS Projects/Exacerbation Three Trials/3DataSets/STATCOPE/statcope.sas7bdat", NULL)

  
  dplyr::tibble(Covariate = rownames(DataSummary_temp),
                DataSummary_temp[ , ]) %>%
    gt() %>%
    text_transform(locations = cells_body(columns = vars("Distribution"), rows = rownames(DataSummary_temp) == "Follow-up time (year)"),
                   fn = function(x) ggplot_density(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "Follow-up time (year)", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars("Distribution"), rows = rownames(DataSummary_temp) == "Age (year)"),
                   fn = function(x) ggplot_density(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "Age (year)", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars("Distribution"), rows = rownames(DataSummary_temp) == "BMI"),
                   fn = function(x) ggplot_density(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "BMI", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "FEV1 % Predicted"),
                   fn = function(x) ggplot_density(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "FEV1 % Predicted", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "SGRQ"),
                   fn = function(x) ggplot_density(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "SGRQ", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "Sex (male)"),
                   fn = function(x) ggplot_bar(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "Sex (male)", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "O2 therapy"),
                   fn = function(x) ggplot_bar(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "O2 therapy", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "On statin"),
                   fn = function(x) ggplot_bar(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "On statin", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "On LAMA"),
                   fn = function(x) ggplot_bar(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "On LAMA", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "On LABA"),
                   fn = function(x) ggplot_bar(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "On LABA", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "On ICS"),
                   fn = function(x) ggplot_bar(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "On ICS", Height = px(50))) %>%
    text_transform(locations = cells_body(columns = vars('Distribution'), rows = rownames(DataSummary_temp) == "Current smoker"),
                   fn = function(x) ggplot_bar(Data = AllTrials_Data_Table1, Trial = Trials_List, VarName = "Current smoker", Height = px(50))) %>%
    fmt_number(columns = 2 : 4, decimals = 2, suffixing = TRUE) %>%
    fmt_percent(columns = 2 : 4, rows = rownames(DataSummary_temp) %in% c("Current smoker", "O2 therapy",
                                                                          "Sex (male)", "On statin", "On LAMA",
                                                                          "On LABA", "On ICS"),
                drop_trailing_zeros = TRUE) %>%
    tab_style(locations = cells_body(vars("ECLIPSE")), style = cell_text(color = "red")) %>%
    tab_style(locations = cells_body(vars("SUMMIT")), style = cell_text(color = "green")) %>%
    tab_style(locations = cells_body(vars("TORCH")), style = cell_text(color = "blue"))   ->   Table1_gt
  
  Table1_gt
  
}

