#This is ARC DASHBOARD"
library(shiny)
library(DT)
library(shinyWidgets)
library(shinymanager)
library(rmarkdown)
library(gsheet)
library(shinydashboard)
library(ggplot2)
library(igraph)
library(googleAuthR)
###file_path <- "C:/Users/aniishs/Downloads/ARC_FILES/"
###cleaned_df <- read.csv(paste0(file_path,"Cleaned_Data_Updated.csv"))
#-gar_set_client(scopes = "https://www.googleapis.com/auth/drive")
credentials <- data.frame(
  user = c("shiny", "shinymanager"), # mandatory
  password = c("shiny"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)


#gar_set_client(scopes = "https://www.googleapis.com/auth/drive")

file_path <- "enter google sheet path"
cleaned_df <- gsheet2tbl(file_path)

file_path_cal <- "google sheet calendar file"
cal <- gsheet2tbl(file_path_cal)

dictionary_year = c("fall_2012"="Year 2012-2013", "fall_2013"="Year 2013-2014","fall_2014"="Year 2014-2015",
                    "fall_2015"="Year 2015-2016","fall_2016"="Year 2016-2017","fall_2017"="Year 2017-2018",
                    "fall_2018"="Year 2018-2019", "fall_2019"="Year 2019-2020", "fall_2020"="Year 2020-2021",
                    "spring_2012"="Year 2011-2012", "spring_2013"="Year 2012-2013", "spring_2014"="Year 2013-2014",
                    "spring_2015"="Year 2014-2015", "spring_2016"= "Year 2015-2016", "spring_2017"="Year 2016-2017",
                    "spring_2018"="Year 2017-2018", "spring_2019"= "Year 2018-2019", "spring_2020"="Year 2019-2020",
                    "spring_2021"="Year 2020-2021"
                      ) 


cleaned_df$Academic.Year <- dictionary_year[cleaned_df$Semester]

ui <- fluidPage(
  useShinydashboard(),
  
  
  titlePanel("WELCOME TO THE ARC REPORTING DASHBOARD"),
  

  navbarPage(title = span( "", style = "background-color: ; color: black"),
             tabPanel("CA And Staff Analytics",

                      #Application title
                      titlePanel("ARC Staff And CA Support Overview"),
                      
                      # Sidebar with reactive inputs
                      sidebarPanel(
                        selectizeInput('Name', 'Select Name', choices = unique(cleaned_df$Name), multiple=FALSE, selected="Vanitha Prabhu"), width = 15),
                      
                      # a table of reactive outputs
                     mainPanel(
                        fluidRow(
                          valueBoxOutput("vbox_p1", width=4),
                          valueBoxOutput("vbox_p2", width=4),
                          valueBoxOutput("vbox_p3", width=4)
                        ),
                        h2(paste0("Overall Tutoring Support in Hours Segmented By CA's Supported Courses")),
                        plotOutput("experience_plot", height = "500px"),
                        h2(paste0("Overall Tutoring Support In Hours Segmented By Semester")),
                        plotOutput("semester_bar", height="500px"),
                        
                        h2(paste0("Social Network Analysis Of Tutor-Student Interactions By CA Name And Semester")),
                        sidebarPanel(selectizeInput('name_sny', 'Select Name', choices = unique(cleaned_df$Name), selected = "Vanitha Prabhu"),
                                     selectizeInput("semester_sny","Filter By Semester", choices = sort(unique(cleaned_df$Semester), decreasing=TRUE)), width=20),
                        plotOutput("SNYtutor"),
                        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      
                        
                        h2(paste0("Total Hours Worked For The ARC With Breakdown By Course For Chosen Semester")),
                        sidebarPanel(selectizeInput('name_p2', 'Select Name', choices = unique(cleaned_df$Name), selected = "Vanitha Prabhu"),
                                     selectizeInput("semester_p2","Filter By Semester", choices = sort(unique(cleaned_df$Semester), decreasing=TRUE)), width=20),
                        plotOutput("current_plot", height = "500px"),
                        h2(paste0("Time Series Plot Of Hours Worked For The Chosen Semester Based On Filters Above")),
                        
                        plotOutput("ts_plot_bar", height="500px")
                        


                      )
             ),tabPanel("ARC Tutoring Analytics",
                        #selectizeInput('semester_student', 'Semester', choices = unique(cleaned_df$Semester), selected=NULL), 
                        
                        #Application title
                        ###titlePanel("ARC Individual Student Worker Performance  Overview"),
                        
                        # Sidebar with reactive inputs
                        sidebarPanel(
                          selectizeInput('name_student', 'Select Name', choices = unique(cleaned_df$Name.of.Student.Tutored), selected = "Andrei-Horia Pacurar QCS (Sophomore)"), width = 15),
                        
                        # a table of reactive outputs
                        mainPanel(
                          
                          h2(paste0("ARC Visits By Students At A Glance")),
                          fluidRow(
                            valueBoxOutput("vbox_p4", width = 4),
                            valueBoxOutput("vbox_p5", width = 4),
                            valueBoxOutput("vbox_p6", width = 4),
            
                          ),
                          plotOutput("student_experience", height = "500px"),
                          plotOutput("student_course_visits", height="500px"),
                          
                          sidebarPanel(selectizeInput('name_sny_student', 'Select Name', choices = unique(cleaned_df$Name.of.Student.Tutored), selected = "Andrei-Horia Pacurar QCS (Sophomore)"),
                                       selectizeInput("semester_sny_student","Filter By Semester", choices = sort(unique(cleaned_df$Semester), decreasing=TRUE)), width=20),
                          
                          plotOutput("SNYstudent"),
                          br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                          sidebarPanel(selectizeInput('name_student_ts', 'Select Name', choices = unique(cleaned_df$Name.of.Student.Tutored), selected = "Andrei-Horia Pacurar QCS (Sophomore)"),
                                       selectizeInput("semester_student_ts","Filter By Semester", choices = sort(unique(cleaned_df$Semester), decreasing=TRUE)), width=20),
                          plotOutput("student_current_plot", height = "500px"),
                          plotOutput("student_ts_plot_bar", height="500px")
                        )
             ),
             tabPanel("ARC Services Analaytics",
                      
                      h2(paste0("Total ARC Support Extended Across Department And Courses By Year")),
                      plotOutput("chart_yoy", height="500px"),
                      
                      h1("Semester Level Analysis Of Course Support Offered By The ARC"),
                      
                      sidebarPanel(selectizeInput('semester_preview', 'Semester', 
                      choices = unique(cleaned_df$Semester), selected=NULL), width = 15), 
                      
                      mainPanel(
                        h2(paste0("Course Support At a Glance")),
                        fluidRow(
                          valueBoxOutput("vbox1", width=4),
                          valueBoxOutput("vbox2", width=4),
                          valueBoxOutput("vbox3", width=4)
                        ),
                        
                        h2(paste0("Hours of Course Support Offered Through the ARC By Major")),
                        fluidRow(
                          valueBoxOutput("vbox4", width=3),
                          valueBoxOutput("vbox5", width=3),
                          valueBoxOutput("vbox6", width=3),
                          valueBoxOutput("vbox7", width=3)
                          
                        ),
                        
                        h2(paste0("Hours of Course Support Offered Through the ARC For Arts And Sciences Courses")),
                        fluidRow(
                          valueBoxOutput("vbox8", width=4),
                          valueBoxOutput("vbox9", width=4),
                          valueBoxOutput("vbox10", width=4),
                        ),
                        
                    h2(paste0("CA Hiring By Department")),
                        
                    plotOutput("pie_ca", height = "500px"),
                      
                        
                    h2("ARC Utilization By Course And Department. Choose Department From Dropdown"),

                    sidebarPanel(selectizeInput('department', 'Department Name', choices = unique(cleaned_df$Department), 
                                   selected = "Computer Science"), 
                                 selectizeInput("semester_preview_p2","Semester", choices = unique(cleaned_df$Semester)),
                                 width = 20), 

                    plotOutput("pie_dept", height="500px"),
                    
                    h2(paste0("Time Series Plot Of Weekly Sessions And Tutor-Wise Session Count Respectively By Course Number And Semester")),
                    sidebarPanel(selectizeInput('course_ts', 'Course Number', choices = unique(cleaned_df$Course.Number), selected="21111"),
                                                selectizeInput("semester_ts","Semester Name", choices = unique(cleaned_df$Semester), selected = "fall_2015"), width=20),
                    plotOutput("dept_ts", height="500px"),
                    plotOutput("course_tutorwise_analysis", height="500px")
                    
                    
                    ),
             ),
             tabPanel("ARC Event Calendar",
                      sidebarPanel(selectizeInput('semester_calendar', 'Semester', 
                                                  choices = unique(cal$Semester), selected=NULL), width = 15), 
                      mainPanel(
                        h2(paste0("ARC Semester And Workshops At A Glance")),
                        plotOutput("cal_dept", height="500px")
                        ),
             ),
             
             tabPanel("Data Analysis & Derived Tables",
                      
                      titlePanel("Click Button To Generate Semester-Wide Analytics Report "),
                      selectizeInput('semester', 'Semester', choices = sort(unique(cleaned_df$Semester), decreasing = TRUE), selected=NULL), 
                      verbatimTextOutput("value"),
                      downloadButton("report", "Generate report"),
                      
                      titlePanel("Data Tables Derived From Raw Data With Filters"),
                      
                      
                      sidebarPanel(selectizeInput('course_analysis', 'Course Number', choices = unique(cleaned_df$Course.Number)),
                                   selectizeInput("semester_analysis","Semester Name", choices = unique(cleaned_df$Semester)), width=20),
                      mainPanel(
                        h2(paste0("ARC Semester Analysis")),
                        h3("ARC Data Preview"),
                        downloadButton("CSV", "Download"),
                        fluidRow(column(12, (dataTableOutput("data_preview")))),
                        h3("ARC CA Hiring By Department For Chosen Semester With Breakdown By Department"),

                        sidebarPanel(selectizeInput("semester_ca","Semester", choice = unique(cleaned_df$Semester)), width=20),
                        
                        fluidRow(column(12, (dataTableOutput("ca_dept_analysis")))),
                        
                        h3("Preview of Top 10 Or Less Courses Supported By Department And Semester"),
                        
                        sidebarPanel(selectizeInput('department_dt', 'Department Name', choices = unique(cleaned_df$Department), 
                                                    selected = "Biological Sciences"), 
                                     selectizeInput("semester_dt","Semester", choices = unique(cleaned_df$Semester)),
                                     width = 20),
                        

                        fluidRow(column(12, (dataTableOutput("course_dept_sem_analysis"))))
                      ))
))

ui <- secure_app(ui)

server = function(input, output, session) {

  data <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Name==input$Name,]
  cleaned_df_sub = cleaned_df_sub[c("Course.Number", "Session.Length")]
  cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Course.Number), FUN=sum)
  cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
  names(cleaned_df_agg) <- c("Course","Sessions")
  cleaned_df_agg})
  
  data_ts <- reactive({cleaned_df_ts <- cleaned_df[cleaned_df$Semester==input$semester_p2,]
  cleaned_df_ts <- cleaned_df_ts[cleaned_df_ts$Name==input$name_p2,]
  if (dim(cleaned_df_ts)[1] == 0){
    cleaned_df_ts_agg <- data.frame(matrix(ncol=2, nrow=0))
    colnames(cleaned_df_ts_agg) <- c("Week","Session")
  }
  else{
  cleaned_df_ts['week_number'] <- as.numeric(format(strptime(cleaned_df_ts$Date, '%m/%d/%Y'), "%W")) +1

  cleaned_df_ts = cleaned_df_ts[c("week_number","Session.Length")]
  
  cleaned_df_ts_agg = aggregate(cleaned_df_ts$Session.Length, by=list(Category=cleaned_df_ts$week_number), FUN=sum)
  cleaned_df_ts_agg = cleaned_df_ts_agg[order(cleaned_df_ts_agg$x, decreasing=TRUE),]
  names(cleaned_df_ts_agg) <- c("Week","Session")
  }
  cleaned_df_ts_agg
  })
  
  data_student_ts <- reactive({cleaned_df_ts <- cleaned_df[cleaned_df$Semester==input$semester_student_ts,]
  cleaned_df_ts <- cleaned_df_ts[cleaned_df_ts$Name.of.Student.Tutored==input$name_student_ts,]
  if (dim(cleaned_df_ts)[1] == 0){
    cleaned_df_ts_agg <- data.frame(matrix(ncol=2, nrow=0))
    colnames(cleaned_df_ts_agg) <- c("Week","Session")
  }
  else{
    cleaned_df_ts['week_number'] <- as.numeric(format(strptime(cleaned_df_ts$Date, '%m/%d/%Y'), "%W")) +1
    
    cleaned_df_ts = cleaned_df_ts[c("week_number","Session.Length")]
    
    cleaned_df_ts_agg = aggregate(cleaned_df_ts$Session.Length, by=list(Category=cleaned_df_ts$week_number), FUN=sum)
    cleaned_df_ts_agg = cleaned_df_ts_agg[order(cleaned_df_ts_agg$x, decreasing=TRUE),]
    names(cleaned_df_ts_agg) <- c("Week","Session")
  }
  cleaned_df_ts_agg
  })
  
  data_current_sem <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Semester==input$semester_p2,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Name==input$name_p2,]
  if (dim(cleaned_df_sub)[1]==0){
    cleaned_df_agg <- data.frame(matrix(ncol=2, nrow=0))
    colnames(cleaned_df_agg) <- c("Course", "Session")
  }
  
  else{
  cleaned_df_sub = cleaned_df_sub[c("Course.Number","Session.Length")]
  cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Course.Number), FUN=sum)
  cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
  names(cleaned_df_agg) <- c("Course","Session")}
  cleaned_df_agg})
  
  student_data_current_sem <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Semester==input$semester_student_ts,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Name.of.Student.Tutored==input$name_student_ts,]
  if (dim(cleaned_df_sub)[1]==0){
    cleaned_df_agg <- data.frame(matrix(ncol=2, nrow=0))
    colnames(cleaned_df_agg) <- c("Course", "Session")
  }
  
  else{
    cleaned_df_sub = cleaned_df_sub[c("Course.Number","Session.Length")]
    cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Course.Number), FUN=sum)
    cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
    names(cleaned_df_agg) <- c("Course","Session")}
  cleaned_df_agg})
  
  
  
  data_semester <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Name==input$Name,]
  cleaned_df_sub = cleaned_df_sub[c("Semester", "Session.Length")]
  cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Semester), FUN=sum)
  cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
  names(cleaned_df_agg) <- c("Semester","Session")
  cleaned_df_agg})
  
  data_tutor_sny <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Name == input$name_sny,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Semester == input$semester_sny,]
  grouped_data <- aggregate(cleaned_df_sub[c('Name', 'Name.of.Student.Tutored')], by=list(cleaned_df_sub$Name, cleaned_df_sub$Name.of.Student.Tutored), FUN=length)
  grouped_data_new <- grouped_data[grouped_data$Group.1==input$name_sny,]
  names(grouped_data_new) <- c("TA", "Student","1","2")
  
  grouped_data_filter <- grouped_data[grouped_data$Group.1==input$name_sny,]
  actorNetwork <- graph_from_data_frame(d=grouped_data_filter[c('Group.1', 'Group.2','Name')], vertices=rbind(grouped_data_new$TA[1],grouped_data_filter['Group.2']), directed=T)
  actorNetwork
  })
  
  
  data_student_history <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Name.of.Student.Tutored==input$name_student,]
  cleaned_df_sub = cleaned_df_sub[c("Course.Number", "Session.Length")]
  cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Course.Number), FUN=sum)
  cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
  names(cleaned_df_agg) <- c("Course","Session")
  cleaned_df_agg})
  
  data_student_course_visits <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Name.of.Student.Tutored==input$name_student,]
  cleaned_df_sub = cleaned_df_sub[c("Semester", "Session.Length")]
  cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Semester), FUN=sum)
  cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
  names(cleaned_df_agg) <- c("Semester","Session")
  cleaned_df_agg
  })
  
  data_student_sny <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Name.of.Student.Tutored == input$name_sny_student,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Semester == input$semester_sny_student,]
  grouped_data <- aggregate(cleaned_df_sub[c('Name', 'Name.of.Student.Tutored')], by=list(cleaned_df_sub$Name.of.Student.Tutored, cleaned_df_sub$Name), FUN=length)
  grouped_data_new <- grouped_data[grouped_data$Group.1==input$name_sny_student,]
  names(grouped_data_new) <- c("TA", "Student","1","2")
  
  grouped_data_filter <- grouped_data[grouped_data$Group.1==input$name_sny_student,]
  actorNetwork <- graph_from_data_frame(d=grouped_data_filter[c('Group.1', 'Group.2','Name')], vertices=rbind(grouped_data_new$TA[1],grouped_data_filter['Group.2']), directed=T)
  print(actorNetwork)
  actorNetwork
  
  })
  
  data_ca_by_dept <- reactive({
    cleaned_df_sub <- cleaned_df[cleaned_df$Semester==input$semester_preview,]
    cleaned_df_sub = cleaned_df_sub[cleaned_df_sub$Office_Hours_Length < 480,]
    cleaned_df_sub <- cleaned_df_sub[c('Department', 'Name')]
    print (unique(cleaned_df_sub$Name))
    cleaned_df_agg = aggregate(cleaned_df_sub$Name, by=list(Category=cleaned_df_sub$Department), FUN= function(x) length(unique(x)))
    cleaned_df_agg <- cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
    
    if (nrow(cleaned_df_agg) >= 11){
      sum_hours = sum(cleaned_df_agg$x) - sum(cleaned_df_agg$x[0:11])
      cleaned_df_agg = cleaned_df_agg[0:11,]
      de<-data.frame("Others",sum_hours)
      names(cleaned_df_agg) <- c("Department","CA_Count")
      names(de)<-c("Department","CA_Count")
      cleaned_df_agg = rbind(cleaned_df_agg, de)}
    else{
      names(cleaned_df_agg) <- c("Department","CA_Count")
    }
    
    cleaned_df_agg
  })
  
  data_ca_by_dept_alias <- reactive({
    cleaned_df_sub <- cleaned_df[cleaned_df$Semester==input$semester_ca,]
    cleaned_df_sub = cleaned_df_sub[cleaned_df_sub$Office_Hours_Length < 480,]
    cleaned_df_sub <- cleaned_df_sub[c('Department', 'Name')]
    print (unique(cleaned_df_sub$Name))
    cleaned_df_agg = aggregate(cleaned_df_sub$Name, by=list(Category=cleaned_df_sub$Department), FUN= function(x) length(unique(x)))

    if (nrow(cleaned_df_agg) >= 11){
      sum_hours = sum(cleaned_df_agg$x) - sum(cleaned_df_agg$x[0:11])
      cleaned_df_agg = cleaned_df_agg[0:11,]
      de<-data.frame("Others",sum_hours)
      names(cleaned_df_agg) <- c("Department","CA_Count")
      names(de)<-c("Department","CA_Count")
      cleaned_df_agg = rbind(cleaned_df_agg, de)}
    else{
      names(cleaned_df_agg) <- c("Department","CA_Count")
    }
    cleaned_df_agg
  })
  
  data_dept_dist <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Department==input$department,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Semester==input$semester_preview_p2,]
  cleaned_df_sub = cleaned_df_sub[c("Course.Number", "Session.Length")]
  cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Course.Number), FUN=sum)
  cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
  if (nrow(cleaned_df_agg) >= 11){
  sum_hours = sum(cleaned_df_agg$x) - sum(cleaned_df_agg$x[0:11])
  cleaned_df_agg = cleaned_df_agg[0:11,]
  de<-data.frame("Others",sum_hours)
  names(cleaned_df_agg) <- c("Course","Session")
  names(de)<-c("Course","Session")
  cleaned_df_agg = rbind(cleaned_df_agg, de)
  cleaned_df_agg["Session"] = round(cleaned_df_agg["Session"]/60,0)}
  
  else{
    names(cleaned_df_agg) <- c("Course","Session")
    cleaned_df_agg["Session"] = round(cleaned_df_agg["Session"]/60,0)
  }
  
  (cleaned_df_agg)
  })
  
  data_dept_dist_alias <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Department==input$department_dt,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Semester==input$semester_dt,]
  cleaned_df_sub = cleaned_df_sub[c("Course.Number", "Session.Length")]
  cleaned_df_agg = aggregate(cleaned_df_sub$Session.Length, by=list(Category=cleaned_df_sub$Course.Number), FUN=sum)
  cleaned_df_agg = cleaned_df_agg[order(cleaned_df_agg$x, decreasing=TRUE),]
  if (nrow(cleaned_df_agg) >= 11){
    sum_hours = sum(cleaned_df_agg$x) - sum(cleaned_df_agg$x[0:11])
    cleaned_df_agg = cleaned_df_agg[0:11,]
    de<-data.frame("Others",sum_hours)
    names(cleaned_df_agg) <- c("Course","Session")
    names(de)<-c("Course","Session")
    cleaned_df_agg = rbind(cleaned_df_agg, de)
    cleaned_df_agg["Session"] = round(cleaned_df_agg["Session"]/60,0)}
  
  else{
    names(cleaned_df_agg) <- c("Course","Session")
    cleaned_df_agg["Session"] = round(cleaned_df_agg["Session"]/60,0)
  }
  row.names(cleaned_df_agg) <- NULL
  names(cleaned_df_agg) <- c("Course", "Sessions in Hours")
  
  (cleaned_df_agg)
  })
  
  data_ts_academicyear <- reactive ({
    clean_df_agg = aggregate(cleaned_df$Session.Length, by=list(Category=cleaned_df$Academic.Year), FUN=sum)
    clean_df_agg$x = clean_df_agg$x/60.0
    names(clean_df_agg) <- c("Year", "Support")
    clean_df_agg
  })
  
  data_dept_ts_week <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Course.Number == input$course_ts,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Semester == input$semester_ts,]
  if (dim(cleaned_df_sub)[1]==0){
    cleaned_df_agg <- data.frame(matrix(ncol=2, nrow=0))
    colnames(cleaned_df_agg) <- c("Week","Session")
  }
  else{
  cleaned_df_agg <- aggregate(cleaned_df_sub$Session.Length, by=list(cleaned_df_sub$week_number), FUN=sum)
  names(cleaned_df_agg) <- c("Week", "Session")}
  cleaned_df_agg
  })
  
  
  data_dept_ts_analysis <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Course.Number == input$course_analysis,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Semester == input$semester_analysis,]
  if (dim(cleaned_df_sub)[1]==0){
    cleaned_df_agg <- data.frame(matrix(ncol=2, nrow=0))
    colnames(cleaned_df_agg) <- c("Week","Session")
  }
  else{
    cleaned_df_agg <- aggregate(cleaned_df_sub$Session.Length, by=list(cleaned_df_sub$week_number), FUN=sum)
    names(cleaned_df_agg) <- c("Week", "Session (In Minutes)")}
  cleaned_df_agg
  })
  
  data_sessions_course_bytutor <- reactive({cleaned_df_sub <- cleaned_df[cleaned_df$Course.Number == input$course_ts,]
  cleaned_df_sub <- cleaned_df_sub[cleaned_df_sub$Semester == input$semester_ts,]
  if (dim(cleaned_df_sub)[1]==0){
    cleaned_df_agg <- data.frame(matrix(ncol=2, nrow=0))
    colnames(cleaned_df_agg) <- c("Tutor", "Session_Minutes")
  }
  else{
    cleaned_df_agg <- aggregate(cleaned_df_sub$Session.Length, by=list(cleaned_df_sub$Name), FUN=sum)
    names(cleaned_df_agg) <- c("Tutor", "Session_Minutes")}
  cleaned_df_agg
  print(cleaned_df_agg)
  })
  
  
  data_cal<- reactive({cleaned_df_sub <- cal[cal$Semester==input$semester_calendar,]
  tasks <- cleaned_df_sub$Workshop
  dfr <- data.frame(
    name        = factor(tasks, levels = rev(unique(tasks)),ordered=TRUE),
    start.date  = as.Date(cleaned_df_sub$Start.Date, tryFormats = c("%m/%d/%Y")),
    end.date    = as.Date(cleaned_df_sub$End.Date, tryFormats = c("%m/%d/%Y")),
    event.presenter = cleaned_df_sub$Presenter)
  dfr
  })

  
  output$experience_plot <- renderPlot(ggplot(data(), aes(x=Course, y=Sessions, fill=Course)) +
                                         geom_bar(stat="identity")+
                                         scale_fill_brewer(palette="Paired")+
                                         theme_minimal()+
                                         geom_col(width = 0.8, height = 0.8))
  
  output$semester_bar <-  renderPlot({ggplot(data=data_semester(), aes(x=Semester, y=Session, fill=Semester)) +
      geom_bar(stat="identity", position=position_dodge(), las=1)+
      scale_fill_brewer()+
      theme_minimal()+
      coord_flip()})
  
  output$current_plot <-  renderPlot({ggplot(data_current_sem(), aes(x=Course, y=Session)) +
      geom_bar(stat="identity", fill="Black")})
  
  output$student_current_plot <-  renderPlot({ggplot(student_data_current_sem(), aes(x=Course, y=Session)) +
      geom_bar(stat="identity", fill="Black")})
  
  
  output$ts_plot_bar <-  renderPlot({

    ggplot(data_ts(), aes(x=Week, y=Session)) +
      geom_line() + 
      xlab("Week_Number") + ylab("Session")})
  
  
  output$student_ts_plot_bar <-  renderPlot({
    
    ggplot(data_student_ts(), aes(x=Week, y=Session)) +
      geom_line() + 
      xlab("Week_Number") + ylab("Session")})
  
  
  output$SNYtutor <- renderPlot({

    plot(data_tutor_sny(), vertex.arrow.size = 0.6)
  },  height = 800, width = 800)
  
  output$SNYstudent <- renderPlot({
    
    plot(data_student_sny(), vertex.arrow.size = 0.6)
  },  height = 800, width = 800)
  
  output$student_experience <-  renderPlot({ggplot(data=data_student_history(), aes(x=Course, y=Session, fill=Course)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()})
  
  
  output$student_course_visits <-  renderPlot({ggplot(data=data_student_course_visits(), aes(x=Semester, y=Session, fill=Semester)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer(palette="Paired")+
      theme_minimal()})
  

  raw_data <- reactive({cleaned_df})
  
  output$data_preview <- DT::renderDataTable(
    data_dept_ts_analysis()
  )
  
  output$ca_dept_analysis <- DT::renderDataTable(
    data_ca_by_dept_alias()
  )
  
  
  
  output$vbox1 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    d <- length(unique(cleaned_df_filter$Course.Number))
    shinydashboard::valueBox( d, "Number Of Courses Supported")
  })
  
  output$vbox2 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    d<-round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "Hours Of Support Reported")
  })
  
  output$vbox3 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Office_Hours_Length < 480,]
    d<-length(unique(cleaned_df_filter$Name))
    shinydashboard::valueBox( d, "Number of Hired CA's")
  })
  
  output$vbox4 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Department == "Business Administration",]
    
    d <- round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "BA", color="green")
  })
  
  output$vbox5 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Department == "Computer Science",]
    d <- round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "CS", color="green")

  })
  
  output$vbox6 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Department == "Information Science",]
    
    d <- round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "IS", color="green")
  })
  
  output$vbox7 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Department == "Biological Sciences",]
    d <- round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "BS", color="green")
  })
  
  output$vbox8 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Department == "Mathematical Sciences",]
    d <- round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "Math", color="black")
  })
  output$vbox9 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Department == "English",]
    d <- round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "English", color="black")
  })
  output$vbox10 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester_preview,]
    cleaned_df_filter = cleaned_df_filter[cleaned_df_filter$Department == "History",]
    d <- round(sum(cleaned_df_filter$Session.Length)/60,0)
    shinydashboard::valueBox( d, "History", color="black")
  })
  
  output$vbox_p1 <- shinydashboard::renderValueBox({
  ###  cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester,]
    cleaned_df_filter = cleaned_df[cleaned_df$Name == input$Name,]
    d <- length(unique(cleaned_df_filter$Course.Number))
    shinydashboard::valueBox( d, "Number Of Courses Supported", color="blue")
  })
  
  output$vbox_p2 <- shinydashboard::renderValueBox({
   ### cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester,]
    cleaned_df_filter = cleaned_df[cleaned_df$Name == input$Name,]
    d <- nrow(cleaned_df_filter)
    shinydashboard::valueBox( d, "Total Number Of Sessions Held", color="red")
  })
  
  output$vbox_p3 <- shinydashboard::renderValueBox({
   ### cleaned_df_filter = cleaned_df[cleaned_df$Semester == input$semester,]
    cleaned_df_filter = cleaned_df[cleaned_df$Name == input$Name,]
    d <- length(unique(cleaned_df_filter$Name.of.Student.Tutored))
    shinydashboard::valueBox( d, "Number Of Students Assisted", color="black")
  })
  
  output$vbox_p4 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Name.of.Student.Tutored == input$name_student,]
    d <- sum(cleaned_df_filter$Session.Length)
    shinydashboard::valueBox( round(d/60.0,2), "Hours Of Support Sought", color="blue")
  })
  output$vbox_p5 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Name.of.Student.Tutored == input$name_student,]
    d <- length(unique(cleaned_df_filter$Course.Number))
    shinydashboard::valueBox( d, "Number Of Courses", color="red")
  })
  output$vbox_p6 <- shinydashboard::renderValueBox({
    cleaned_df_filter = cleaned_df[cleaned_df$Name.of.Student.Tutored == input$name_student,]
    d <- nrow(cleaned_df_filter)
    shinydashboard::valueBox( d, "Total Number of Sessions", color="black")
  })
  
  output$pie_ca <- renderPlot({pie(data_ca_by_dept()$CA_Count, labels = data_ca_by_dept()$Department, 
                                   main = "Pie Chart Showing the Support By Course And Department", 
                                   radius=1)
    })
  

  output$pie_dept <-  renderPlot({pie(data_dept_dist()$Session, data_dept_dist()$Course, 
                                      main = "Pie Chart Showing the Support By Course And Department", 
                                      radius=1)
    })
  output$course_dept_sem_analysis <- DT::renderDataTable(data_dept_dist_alias())
  
  output$cal_dept <-  renderPlot({
    ggplot(data_cal(), aes(x =start.date, xend= end.date, y=name, yend = name, color=event.presenter)) +
      geom_segment(size = 6) +
      theme(panel.grid = element_line(color = "red",
                                      size = 0.75,
                                      linetype = 2))+
      xlab(NULL) + ylab(NULL)+
      geom_text(aes(label = paste0(substr(months(as.Date(data_cal()$start.date, format="%m/%d/%Y")),0,3),'-',format(as.POSIXct(data_cal()$start.date, format="%m/%d/%Y"), "%d")), vjust = 1.5))
      
     # geom_text(aes(label = paste0(months(format(as.POSIXct(data_cal()$start.date,format="%m/%d/%Y"), "%m")),'-',format(as.POSIXct(data_cal()$start.date, format="%m/%d/%Y"), "%d")), vjust = 1.5, color = "black"))
  }, height = 600, width = 1000)
  
  output$chart_yoy <- renderPlot({ggplot(data=data_ts_academicyear(), aes(x=Year, y=Support, fill=Year)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer()+
      theme_minimal()+
        coord_flip()
    
  })
  
  output$dept_ts <-  renderPlot({
  ggplot(data_dept_ts_week(), aes(x=Week, y=Session)) +
      geom_line() + 
      xlab("Week_Number") + ylab("Session In Hours")})
  

  output$course_tutorwise_analysis <- renderPlot({ggplot(data=data_sessions_course_bytutor(), aes(x=Tutor, y=Session_Minutes)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer()+
      theme_minimal()
  })
  

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "ARCSTATS.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      
      tempReport <- file.path(getwd(), "ARC_MD.Rmd")
      ##file.copy("ARC_MD.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(semester = input$semester)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$CSV <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_dept_ts_analysis(), file)
    }
  )
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
}

shinyApp(ui= ui, server = server)