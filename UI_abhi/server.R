library(shiny)
library(datasets)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

mytemp <- arfd[which(!is.na(arfd$county)),]

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  output$choose_state <- renderUI({
    list_states <- sort(unique(mytemp$state))
    selectInput("chosen_state","Choose State",
                as.list(list_states))
    # selectInput("chosen_state","Choose State",
    #             list("Florida" = "FL",
    #                  "Illinois" = "IL",
    #                  "Texas" = "TX",
    #                  "Virginia" = "VA"))
  })
  
  output$choose_county <- renderUI({
    if(is.null(input$chosen_state))
      return()
    this_state <- input$chosen_state
    mytemp <- mytemp[which(mytemp$state==this_state),]
    list_counties <- sort(unique(mytemp$county))
    selectInput("chosen_county","Choose County",
                as.list(list_counties))
  })

  output$choose_city <- renderUI({
    if(is.null(input$chosen_state) || is.null(input$chosen_county))
      return()
    this_state <- input$chosen_state
    this_county <- input$chosen_county
    mytemp <- mytemp[which(mytemp$state==this_state &
                              mytemp$county==this_county),]
    list_cities <- sort(unique(mytemp$city))
    selectInput("chosen_city","Choose City",
                as.list(list_cities))
  })
  
  salary_values <- reactive({
    
  })
  
  observeEvent(input$add_roles, {
    list_roles <- sort(as.character(all_roles))
    insertUI(
      selector = "#add_roles",
      where = "afterEnd",
      ui = selectInput("chosen_role1","Primary Role",
                       as.list(list_roles))
    )
  })
  
  mytemp_editor <- reactive({
    this_state <- input$chosen_state
    this_county <- input$chosen_county
    this_city <- input$chosen_city
    min_salary <- input$salary_range[1]
    max_salary <- input$salary_range[2]
    mytemp_current <- mytemp[which(mytemp$salary>=min_salary &
                                     mytemp$salary<=max_salary),]
    if(!is.null(this_state) &
       !is.null(this_county) &
       !is.null(this_city)){
      mytemp_current <- mytemp_current[
        which(mytemp_current$state==this_state &
              mytemp_current$county==this_county &
              mytemp_current$city==this_city),]
    }
    # mytemp_current <- mytemp[which(mytemp$state==this_state &
    #                mytemp$county==this_county &
    #                mytemp$city==this_city &
    #                mytemp$salary>=min_salary &
    #                mytemp$salary<=max_salary),]
    if(!is.null(input$chosen_role1)){
      this_role1 <- input$chosen_role1
      mytemp_current <- mytemp_current[
        which(sapply(lapply(mytemp_current$role,as.character),
                                FUN=function(X) this_role1  %in% X)),]
    }
    mytemp_current #output
  })
  
  output$caption <- renderText({
    mytemp <- mytemp_editor()
    num_jobs <- as.character(length(mytemp$job_id))
    paste(num_jobs,"jobs found")
  })
  
  # output$view_jobs <- renderTable({
  #   #mytemp
  #   #head(mytemp, n = length(mytemp$job_id))
  #   mytemp[1,]
  # })
  
  output$view_jobs <- renderDataTable({
    mytemp <- mytemp_editor()
    mytemp[,c("company","post_date","time_to_fill",
                       "salary","role","skill")]
  })
  
})


