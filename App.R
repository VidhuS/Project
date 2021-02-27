#install packages
devtools::install_github('thomasp85/gganimate')
install.packages("tidyr")
install.packages("ggplot2")
install.packages(shinydashboard)
install.packages(shinycssloaders)
install.packages(shiny)
install.packages("fpp2")
install.packages("dplyr")
install.packages('rworldmap')
#Load packages
library("tidyr")
library('reprex')
library(ggplot2) 
library(gganimate)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(fpp2)
library("plyr")
library("dplyr")
library('rworldmap')

#Shiny APP
# Server module


s2<-shinyServer(function(input,output){
  #loading data
  GTemp_data<-as.data.frame(read.csv(file = 'GlobalLandTemperaturesByCountry.csv'))
  CityTemp_data<-as.data.frame(read.csv(file = 'city_temperature.csv'))
  M1_data<-as.data.frame(read.csv(file = 'Metadata_Indicator_API_19_DS2_en_csv_v2_1125137.csv'))
  c_data<-as.data.frame(read.csv(file = 'countries.csv'))
  
  d1<-as.data.frame(read.csv(file='data.csv'))
  d2_temp$Latitude<-as.numeric(gsub("[a-zA-Z ]", "", d2_temp$Latitude))
  d2_temp$Longitude<-as.numeric(gsub("[a-zA-Z ]", "", d2_temp$Longitude))
  
  GreenHouse_data<-as.data.frame(read.csv(file = 'datasets_4736_7235_greenhouse_gas_inventory_data_data.csv',check.names = FALSE))
  UN_data<-as.data.frame(read.csv(file = 'UN_DATA_T.csv'))
  d2_temp<-as.data.frame(read.csv(file='temperature.csv'))
  
  #data wrangling and cleaning
  
  d2_temp1<-d2_temp %>% filter(d2_temp$day==1&d2_temp$year==1995)
  d2_tempL<-d2_temp %>% filter(d2_temp$country_id=="NEW" & d2_temp$month==1 & d2_temp$day==1)
  x<-d2_tempL$year
  y<-d2_tempL$AverageTemperatureFahr
  
  #Data modeling and prediciton
  model <- lm(formula= y ~ x, data = d2_tempL)
  newx = seq(min(x),max(x),by = 0.05)
  newy=c(2020,2030,2040,2050,2060,2070,2080,2090,2100)
  new_values<-data.frame(x=newx)
  new_values2<-data.frame(x=newy)
  
  cf <- predict(model, newdata =new_values,interval = "confidence",level = 0.95)
  prediction<-predict(model,newdata = new_values2,interval = "predict")
  prediction<-data.frame(prediction)
  prediction<-cbind(prediction,newy)
  
  #Contry Map plot
  
  waterstress<-data.frame(country= c("BHR","KWT","QAT","SMR","SGP","ARE","PSE","ISR","SAU","OMN","LBN","KGZ","IRN","JOR","LBY","YEM","MKD","AZE","MAR","KAZ","IRQ","ARM","PAK","CHL","SYR","TKM","TUR","GRC","UZB","DZA"),scarcity=c("Extremely Low waterlevel","Extremely Low waterlevel","Extremely Low waterlevel","Extremely Low waterlevel","Extremely Low waterlevel","Extremely Low waterlevel","Extremely Low waterlevel","Extremely Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel","Low waterlevel"))

  malMap <- joinCountryData2Map(waterstress, joinCode = "ISO3",
                                nameJoinColumn = "country")
  
  GreenHouse_data2<-GreenHouse_data %>% filter(GreenHouse_data$country_or_area %in% c("Australia","Canada","Japan","European Union","United States of America") & GreenHouse_data$year %in% c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014"))
  
  disaster<-data.frame(countryz=c("CHN","VNM","JPN","IND","BGD","IDN","THA","NLD","PHL","MMR","USA","GBR","BRA","DEU","FRA","MYS","TWN","KOR","NGA","ITA"),
                       threat=c("Extreme Risk","High Risk","High Risk","High Risk","High Risk","High Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Moderate Risk","Low Risk","Low Risk"))
  
  malMap2 <- joinCountryData2Map(disaster, joinCode = "ISO3",
                                 nameJoinColumn = "countryz")
  
  d2_temp1<-d2_temp %>% filter(d2_temp$day==1&d2_temp$year==1995)
  
  
  d2_temp2<-d2_temp %>% filter(d2_temp$day==1&d2_temp$year==2010)
  
  d2_tempx<-d2_temp %>% filter(d2_temp$day==1)
  
  
  
  
  
  
  output$mapCountryData <- renderPlot({
    mapCountryData(malMap, nameColumnToPlot="scarcity", catMethod = "categorical",
                   missingCountryCol = grey(0.5), colourPalette = c("red", "orange"))
  })
  
  output$mapCountryData2 <- renderPlot({
    mapCountryData(malMap2, nameColumnToPlot="threat", catMethod = "categorical",
                   missingCountryCol = grey(0.5), colourPalette = c("red", "orange","yellow","green"))
  
  #Temprature change plot animation
    
  })
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  output$Tempplotx<- renderImage({
    outfile2 <- tempfile(fileext='.gif')
    px=ggplot(d2_tempx ,aes(`month`,`AverageTemperatureFahr`,size=`AverageTemperatureUncertaintyFahr`,color=`Country`))+
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=4)+labs(x = "Month", y = "Temperature") + ggtitle("Average Temprature over the years 1995-2014")+
      theme(legend.position = "top")
    
    anim1 <- px +
      transition_states(d2_temp$year,
                        transition_length = 4,
                        state_length = 2)
    
    anim1
    
    
    
    anim_save("outfile2.gif", animate(anim1))
    
    list(src = "outfile2.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  
  #Temprature plot
  
  output$temp1 <- renderPlot({
    p<-d2_temp1  %>% ggplot(aes(`month`,`AverageTemperatureFahr`,size=`AverageTemperatureUncertaintyFahr`,color=`Country`))+
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=4)+labs(fill="AverageTemperatureFahr")+labs(x = "Month", y = "Temperature") +ggtitle("Average Temprature 1995")+
      theme(legend.position = "top")
    
    p
    
    
  })
  output$temp2 <- renderPlot({
    p1<-d2_temp2  %>% ggplot(aes(`month`,`AverageTemperatureFahr`,size=`AverageTemperatureUncertaintyFahr`,color=`Country`))+
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=4)+labs(fill="AverageTemperatureFahr")+labs(x = "Month", y = "Temperature") +ggtitle("Average Temprature 2010")+
      theme(legend.position = "top")
    
    p1
    
  })
  
  #Prediction Plot 
  
  output$pred1 <- renderPlot({
    pm<-ggplot(prediction,aes(`newy`,`fit`))+geom_line()+
      geom_ribbon(data = prediction,aes(ymin=`lwr`,ymax=`upr`),alpha=0.3)+labs(x = "Month", y = "Temperature") +ggtitle("Future Temprature of NEW ZELAND over the years 2020-2100")
    theme(legend.position = "top")
    
    pm
    
  })
  
  #Prediction Plot using ggplot
  
  output$predR <- renderPlot({
    ggplot(d2_tempL, aes(x = x, y = y)) +
      
      geom_point() + 
      stat_smooth(method = "lm", col = "red")
    
  })
  
  data <- eventReactive(input$plot,{
    rnorm(1:100000)
  })
  
  
  
  # Pollution animation
  output$plot1 <- renderImage({
    outfile1 <- tempfile(fileext='.gif')
    p = ggplot(GreenHouse_data2,aes(`year`, `value`, group = `category`, color = factor(`category`))) +
      geom_line() +scale_color_viridis_d() +labs(x = "Month", y = "Temperature") +ggtitle("Emission from years")+
      theme(legend.position = "top")+  geom_point() +facet_wrap(~country_or_area)+ transition_reveal(`year`)
    anim_save("outfile1.gif", animate(p))
    
    list(src = "outfile1.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  
  
  
  
  
  
})

#UI module




u2<-shinyUI(
  dashboardPage( #dash board page
    dashboardHeader(title = "Climate Change"),
    
    dashboardSidebar( #side bar meny
      sidebarMenu(
      menuItem("Home", tabName = "Home"),
      menuItem("Pollution",tabName = "Gases"),
      menuItem("Temprature",tabName = "Temprature"),
      menuItem("Detailed analysis and Projections (Global)",tabName = "A_P"),
      menuItem("the-end",tabName = "the-end")
      
      )),
    dashboardBody( #Entire body
       tabItems( #list of items
      
         tabItem(tabName = "Home", #First item 
                 headerPanel("Effect of Climate chanege "),
                 mainPanel( #Content 
                  
                   h4("Mars asks Moon- Hey Moon, you live close to eath.Why is there so much trouble"),
                   h4("Moon replies - Well you know humans ... They have brought half their planet to endangered land mass area and also some of them are facing WATER crisis.What an irony"),                  
                 ),
                 

                 fluidRow( #Plot in the fluid page
                 
                 box(plotOutput("mapCountryData2",width ="auto")),
                 box(plotOutput("mapCountryData",width ="auto" )))),
                 
                 
                
                  

         tabItem(tabName = "Gases", #Second item 
                 headerPanel("Emission Around the world"),
                 mainPanel( #Content 
                   h4("Worried Mars asks- Well why is this all happening? Well ofcourse because of Humans but how??"),
                   h4("Moon Replies- Well their industrial activities have made Fossil Fuel Enission to skyrocket all over the planet...Thats why earth looks so sick"),
                   h5("The Graphical animation below describes the trend of how the fossil fuel emission increases year by year."),
                   h4("Moon continues- Not only that look at this...this is their temprature in Timge Gap and in Time Laps..Get it"),
                   
                 ),
                 fluidPage( #Plot in the fluid page
                   
                   p("This might take longer than expected , but its worth it :p"),
                   box(plotOutput("plot1",width ="auto") %>% withSpinner(color="#0dc5c2")),
                   box(plotOutput("Tempplotx",width ="auto")%>% withSpinner(color="#0dc5c1"))
                
                   )),
         
         tabItem(tabName = "Temprature", #Third item 
                 headerPanel("Temprature"),
                 mainPanel( #Content 
                   h4("Moon continues enthusiastically-Let do a detailed analysis on next page"),
                   h4("Mars thinks -Compared to 95 their temprature looks surely a lot more uncertain than before")
                 ),
                 
                 
                 fluidPage( #Plot in the fluid page
                   box(plotOutput("temp1",width ="auto")%>% withSpinner(color="#0dc5c1")),
                   box(plotOutput("temp2",width ="auto")%>% withSpinner(color="#0dc5c1")),

                   
                   )),
         
         tabItem(tabName = "A_P", #Fourth item 
                 headerPanel("Detailed Analysis"),
                 mainPanel(#Content 
                   h4("Moon says- As you can see earth is gradually getting hotter and hotter ...like in 2100 its going to get real hard to survive"),
                   h4("Mars asks - Temprature on one hand and floods on the other hand what are humans going to do?"),
                   h4("Moon laughs- Dont you know?")
                   
                 ),
                
                 fluidPage( #Plot in the fluid page
                   box(plotOutput("predR",width ="auto")%>% withSpinner(color="#0dc5c1")),
                   box(plotOutput("pred1",width ="auto")%>% withSpinner(color="#0dc5c1")))),
         tabItem(tabName = "the-end", #Fifth item 
                 mainPanel( #Content 
                   h3("Marks Ask--What???"),
                   h3("Moon smiles-- They are coming for you now"),
                   h1("                         THE END"),
                 ),
                 )
                 
        
         ))
  ))

         
 # Running the application
shinyApp(ui = u2, server = s2)

    
      
     
  














