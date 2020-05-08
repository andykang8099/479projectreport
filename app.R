library(shiny)
library(tidyverse)
library(shinydashboard)
library(jsonlite)
library(cdlTools)
library(usmap)
library(ggplot2)
library(totalcensus)

# specify date breaks to generate nice-looking plot
customizedDateBreak <- function(x) {
  num_breaks <- 10
  round(seq(min(x), max(x), by=(max(x)-min(x))/num_breaks))
}

# plotting function to be used in renderPlot()
plotRequiredData <- function(data, log_scale, ylab, title, lwd, cap) {
  
  names(data)[3] <- "ts_data"
  un_states <- unique(data$state_name)
  
  if (log_scale) {
    data$ts_data[data$ts_data == 0] <- 1
    data$ts_data <- log(data$ts_data)
  }
  
  ggplot(data, aes(date, ts_data, group=state_name, colour=state_name)) + 
    geom_line(size=lwd) + 
    labs(x=NULL, y=ylab, title=title, caption=cap) +
    scale_color_manual(name="State",
                       labels=un_states,
                       values=1:length(un_states)) +
    scale_x_date(date_labels="%b, %d", breaks=customizedDateBreak) +
    theme_bw() +
    theme(text=element_text(size=15),
          plot.title=element_text(size=20, margin=margin(0,0,15,0)),
          plot.margin=unit(c(2,2,2,2), "cm"))
}

# read data
dat_url <- "https://covidtracking.com/api/v1/states/daily.json"

covid <- merge(fromJSON(dat_url), read.csv("./data/StateName.csv")) %>%
  mutate(date=as.character(date),
         date=as.Date(date, format="%Y%m%d"), 
         daily_positive=positiveIncrease,
         daily_tested=totalTestResultsIncrease,
         daily_pos_rate=positiveIncrease/totalTestResultsIncrease*100,
         death_rate=death/positive*100)

# NA <- 0 before cumsum()
covid[is.na(covid$positiveIncrease), "positiveIncrease"] <- 0
covid[is.na(covid$totalTestResultsIncrease), "positiveIncrease"] <- 0

covid <- covid %>%
  group_by(state_name) %>% arrange(date) %>%
  mutate(Cumulative_positive=positive,
         Cumulative_tested=totalTestResults) %>%
  mutate(cum_pos_rate=Cumulative_positive/Cumulative_tested*100) %>%
  ungroup()

current_time <- Sys.time()

# age
age=read.csv("./data/agedata1.csv")
age=age%>%select(STATEFIP,old)
age$STATEFIP<- sprintf("%02d", age$STATEFIP)
age1=age%>%rename(fips=STATEFIP)


# variable-name mapping
dis_names <- c(
  "Cumulative positive diagnosis rate",
  "Daily positive diagnosis rate",
  "Cumulative Death number",
  "Daily Death number",
  "Cumulative Death rate",
  "Total recovered",
  "Total positive case",
  "Total negative case",
  "Current hospitalized",
  "Current in ICU",
  "Current on ventillator"
)
var_names <- c(
  "cum_pos_rate",
  "daily_pos_rate",
  "death",
  "deathIncrease",
  "death_rate",
  "recovered",
  "positive",
  "negative",
  "hospitalizedCurrently",
  "inIcuCurrently",
  "onVentilatorCurrently"
)

# specify color palette

ui <- dashboardPage(
  
  dashboardHeader(title="COVID-19 Data Visualization", titleWidth=300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Info", tabName="state_chart",icon = icon("line-chart")),
      menuItem("Age distribution Info", tabName="age_chart",icon = icon("line-chart"))
    ),
    
    width=300, 
    
    selectizeInput("states", 
                   label = "Select state(s):",
                   choices = sort(unique(covid$state_name)),
                   multiple=TRUE,
                   options=list(placeholder='Click to see available states...')),
    
    radioButtons("metric",
                 label = "Choose topic:",
                 choices = dis_names),
    
    radioButtons("scale", 
                 label = "Choose scale:",
                 choices = c("Standard"=F, "Log"=T)),
    
    sliderInput("range",
                label = "Choose time range:",
                min = min(covid$date), 
                max = max(covid$date),
                value = range(covid$date),
                timeFormat = "%b, %d")
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem("state_chart",
              tableOutput("corona_table"),
              plotOutput("vis",height="800px")
              
      ),
      tabItem("age_chart",
              tableOutput("age_table"),
              plotOutput("ageplot",height="600px")
      )
    )
  )
)




# Server logic ----
server <- function(input, output) {
  
  output$vis <- renderPlot({
    
    if (! is.null(input$states)) {
      
      data <- covid %>%
        select(date, state_name, 
               var_names[dis_names == input$metric]) %>%
        filter(covid$state_name %in% input$states,
               between(covid$date, input$range[1], input$range[2]))
      
      title <- paste(
        input$metric, 
        ifelse(
          length(input$states) == 1, 
          "of selected state over time", 
          "of selected states over time"
        )
      )
      
      ylab <- ifelse(
        grepl("rate", input$metric), 
        paste(input$metric, "(%)"), 
        input$metric
      )
      
      plotRequiredData(
        data, input$scale, ylab, title, lwd=1.2, 
        cap=paste("Updated at", format(current_time, format="%X %A, %B %d, %Y"))
      )
    }
    
  })
  output$age_table <- renderTable({
    if(length(input$states)==0) {
      ("Please select states")
    }
    else { 
      datanow=age[which(age$STATEFIP %in% fips(c(input$states))),]
      colnames(datanow)=c("State","Proportion of aged 65 or older")
      for (i in 1:nrow(datanow)) {
        datanow[i,1]=convert_fips_to_names(as.character(datanow[i,1]))
      }
      datanow
    }
  })
  output$ageplot <- renderPlot({
    plot_usmap(data = age1, values = "old", color = "red") + 
      scale_fill_continuous(
        low = "white", high = "red", name = "Proportion of aged 65 or older", label = scales::comma) + theme(legend.position = "right")
  })
}

# Run app ----
shinyApp(ui, server)

