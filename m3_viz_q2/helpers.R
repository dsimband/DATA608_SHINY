

loadUSMortalityData <- function() {

  df <- loadStateMortalityData()

  # create us dataframe
  us_df <-  df %>% group_by(Year, Disease_ID) %>%
    summarise(Deaths = sum(Deaths),
              Population = sum(Population))

  us_df$Crude.Rate <- us_df$Deaths / us_df$Population * 100000

  return(us_df)

}


#us_df <- loadUSMortalityData()
#dID <- 'd14'
#d0 <- filterUSDiseaseDF(us_df, dID)

filterUSDiseaseDF <- function(us_df, dID) {

  #us_df <- loadUSMortalityData()

  us_df <- us_df %>% filter(Disease_ID == dID) %>% pivot_wider(id_cols = 'Year',
                                                              names_from = 'Disease_ID',
                                                              values_from = 'Crude.Rate')
  us_df <- us_df %>% replace(is.na(.), 0)
  us_df <- us_df %>% select(Year,dID) %>% rename(Crude.Rate = dID)

  return(us_df)

}





loadStateMortalityData <- function() {

  df <- read.csv('./data/cleaned-cdc-mortality-1999-2010-2.csv')
  #df <- read.csv('./m3_viz_q2/data/cleaned-cdc-mortality-1999-2010-2.csv')
  df['ICD'] <- df$ICD.Chapter
  df$ICD <- replace(df$ICD,df$ICD=="Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism","Diseases of the blood and blood-forming organ")
  df$ICD <- replace(df$ICD,df$ICD=="Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified"  ,"Symptoms not elsewhere classified")

  df <- df |>
    dplyr::mutate(ICD = factor(ICD, levels = unique(ICD))) |>
    dplyr::group_by(ICD) |>
    mutate(Disease_ID = dplyr::cur_group_id()) |>
    ungroup()

  df$Disease_ID <- paste0('d',df$Disease_ID)
  df <- df %>% replace(is.na(.), 0)

  return(df)

}



# df <- loadStateMortalityData()
# stID <- 'AL'
# dID <- 'd8'
# d0 <- filterStateDiseaseDF(df, stID, dID)

filterStateDiseaseDF <- function(df, stID, dID) {

  s_df <- df %>% filter(State == stID & Disease_ID == dID) %>% select(Year, Crude.Rate, Disease_ID)
  s_df <- s_df %>% pivot_wider(id_cols = 'Year', names_from = 'Disease_ID', values_from = 'Crude.Rate')
  s_df <- s_df %>% select(Year,dID) %>% rename(Crude.Rate = dID)

  return(s_df)

}



