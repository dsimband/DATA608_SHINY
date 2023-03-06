

#' Helper function to load the aggregatd mortality data
#'
#' @return summary mortality data for the united states
#' @export
#'
#' @examples
loadUSMortalityData <- function(df) {

  us_df <-  df %>% group_by(Year, Disease_ID) %>%
    summarise(Deaths = sum(Deaths),
              Population = sum(Population))

  us_df$Crude.Rate <- us_df$Deaths / us_df$Population * 100000

  return(us_df)

}




#' Helper function to filter the mortality data by a specific disease
#'
#' @param us_df
#' @param dID
#'
#' @return
#' @export
#'
#' @examples
filterUSDiseaseDF <- function(us_df, dID) {

  us_df <- us_df %>% filter(Disease_ID == dID) %>% pivot_wider(id_cols = 'Year',
                                                              names_from = 'Disease_ID',
                                                              values_from = 'Crude.Rate')
  us_df <- us_df %>% replace(is.na(.), 0)
  us_df <- us_df %>% select(Year,dID) %>% rename(Crude.Rate = dID)

  return(us_df)

}





#' Helper function to load the mortality data
#'
#' @return data frame with the mortality data
#' @export
#'
#' @examples
loadStateMortalityData <- function() {

  # load data
  df <- read.csv('./data/cleaned-cdc-mortality-1999-2010-2.csv')

  # clean up some of the longer disease titles
  df['ICD'] <- df$ICD.Chapter
  df$ICD <- replace(df$ICD,df$ICD=="Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism","Diseases of the blood and blood-forming organ")
  df$ICD <- replace(df$ICD,df$ICD=="Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified"  ,"Symptoms not elsewhere classified")
  df$ICD <- replace(df$ICD,df$ICD=="Diseases of the musculoskeletal system and connective tissue" , "Diseases of the musculoskeletal system")
  df$ICD <- replace(df$ICD,df$ICD=="Certain conditions originating in the perinatal period" , "Conditions / perinatal period" )
  df$ICD <- replace(df$ICD,df$ICD=="Congenital malformations, deformations and chromosomal abnormalities" ,  "Congenital malformations, and chromosomal abnormalities")

  # create a factor for the levels
  df <- df %>%
    dplyr::mutate(ICD = factor(ICD, levels = unique(ICD))) %>%
    dplyr::group_by(ICD) %>%
    mutate(Disease_ID = dplyr::cur_group_id()) %>%
    ungroup()

  # remove na
  df$Disease_ID <- df$ICD
  df <- df %>% replace(is.na(.), 0)

  return(df)

}




#' Helper function to filter state data by disease ID and state ID
#'
#' @param df
#' @param stID
#' @param dID
#'
#' @return state disease data
filterStateDiseaseDF <- function(df, stID, dID) {

  s_df <- df %>% filter(State == stID & Disease_ID == dID) %>% select(Year, Crude.Rate, Disease_ID)
  if (nrow(s_df) > 0) {
    s_df <- s_df %>% pivot_wider(id_cols = 'Year', names_from = 'Disease_ID', values_from = 'Crude.Rate')
    s_df <- s_df %>% select(Year,dID) %>% rename(Crude.Rate = dID)
  }

  return(s_df)

}



