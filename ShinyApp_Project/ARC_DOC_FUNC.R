
subset_data <- function(semester,column1, column2){
  file_path <- "https://docs.google.com/spreadsheets/d/1VchC1E68erXgaOUw4pIi4H3JSIfHFzqjpF3XCv5Xk84/edit?usp=sharing"
  cleaned_df <- gsheet2tbl(file_path)
  ##cleaned_df <- read.csv("C:/Users/aniishs/Desktop/Data/Cleaned_Data_Updated.csv")
  drop <- c("Date.1")
  cleaned_df<- cleaned_df[!(names(cleaned_df)%in%drop)]
  cleaned_df_sub <- cleaned_df[cleaned_df$Semester==semester,]
  cleaned_df_sub = cleaned_df_sub[c(column1, column2)]
  return (cleaned_df_sub)
}












