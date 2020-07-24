server <- function(input, output, session) {
    ####################
    ### tab 1: Intro ###
    ####################
    
    
    #######################
    ### tab 2: BarChart ###
    #######################
    data_fil = reactive({
        return(data %>% filter(as.numeric(format(Date,'%m')) == input$month) %>%
                   group_by(State) %>%  
                   summarise(Deathrate_avg = mean(CumDeathRate),
                             Infectedrate_avg = mean(CumInfRate),
                             Growthrate_avg = mean(CurInfRate)))
    })
    
    
    output$plot1 = renderPlotly({
        P_deathrate = data_fil() %>% 
            arrange(-Deathrate_avg) %>%
            head(10) %>%
            ggplot(aes(x = reorder(State, Deathrate_avg), y = Deathrate_avg, fill="blue")) +
            geom_bar(stat = "identity")+
            coord_flip()+
            labs(y = "Average Death Rate (Monthly)", 
                 title = "Death Rate Ranking")+
            geom_text(size =12, x=3.5, y=0.003, color = "grey80", label = input$month)+
            scale_y_continuous(breaks =seq(0,0.0035, by=0.0005),  limits = c(0,0.0035), labels = percent)+
            theme(legend.position = "none", axis.ticks.y = element_blank(), 
                  plot.title = element_text( hjust=0.5, face='bold', size = 16),
                  axis.title.x = element_text(size = 8), axis.title.y = element_blank(),
                  axis.text.x =  element_text(size = 7, face = "bold-italic"),
                  axis.text.y = element_text(size = 8, face = "bold",color = "black"),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), panel.background = element_blank())
        
        ggplotly(P_deathrate)
    })
    
    output$plot2 = renderPlotly({
        P_infectedrate = data_fil() %>% 
            arrange(-Infectedrate_avg) %>%
            head(10) %>%
            ggplot(aes(x = reorder(State, Infectedrate_avg), y = Infectedrate_avg)) +
            geom_bar(stat = "identity", fill = "#56B4E9", width = 0.9)+
            coord_flip()+
            labs(y = "Average Infected Rate (Monthly)", 
                 title = "Infected Rate Ranking")+
            geom_text(size =12, x=3.5, y=0.32, color = "grey80", label = input$month)+
            scale_y_continuous(breaks =seq(0,0.3500, by=0.0500),  limits = c(0,0.3500), labels = percent)+
            theme(legend.position = "none", axis.ticks.y = element_blank(), 
                  plot.title = element_text( hjust=0.5, face='bold', size = 16),
                  axis.title.x = element_text(size = 8), axis.title.y = element_blank(),
                  axis.text.x =  element_text(size = 7, face = "bold-italic"),
                  axis.text.y = element_text(size = 8, face = "bold",color = "black"),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), panel.background = element_blank())
        
        ggplotly(P_infectedrate)
    }) 
    
    output$plot3 = renderPlotly({
        P_Growthrate = data_fil() %>% 
            arrange(-Growthrate_avg) %>%
            head(10) %>%
            ggplot(aes(x = reorder(State, Growthrate_avg), y = Growthrate_avg)) +
            geom_bar(stat = "identity", fill = "orange", width = 0.9)+
            coord_flip()+
            labs(y = "Average Growth Rate (Monthly)", 
                 title = "Growth Rate Ranking")+
            geom_text(size =12, x=3.5, y=0.027, color = "grey80", label = input$month)+
            scale_y_continuous(breaks =seq(0,0.0300, by= 0.0050 ),  limits = c(0,0.0300),labels = percent)+
            theme(legend.position = "none", axis.ticks.y = element_blank(), 
                  plot.title = element_text( hjust=0.5, face='bold', size = 16),
                  axis.title.x = element_text(size = 8), axis.title.y = element_blank(),
                  axis.text.x =  element_text(size = 7, face = "bold-italic"),
                  axis.text.y = element_text(size = 8, face = "bold",color = "black"),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), panel.background = element_blank())
        
        ggplotly(P_Growthrate)
    }) 
    
    output$text1 <- renderUI({
        P_D = data_fil() %>% 
            arrange(-Deathrate_avg) %>%
            head(10)
        D = data.frame(P_D$State)
        colnames(D)=c("state")
        
        P_I= data_fil() %>% 
            arrange(-Infectedrate_avg) %>%
            head(10)
        I = data.frame(P_I$State)
        colnames(I)=c("state")
        
        P_G = data_fil() %>% 
            arrange(-Growthrate_avg) %>%
            head(10)
        G = data.frame(P_G$State)
        colnames(G)=c("state")
        
        P_alert = rbind(D,I,G)
        P_alert = P_alert %>%
            group_by(state) %>%
            count() %>%
            arrange(-n,state)
        alert1 = as.vector(P_alert$state)
        div(
            h2("Alert State", style = "color:white", align = "center"),
            em("( Top 5 frequent shown states in three charts each month )",style = "color:white", align = "center"),
            br(),
            h4(alert1[1], style = "color:white", align = "center"),
            h4(alert1[2], style = "color:white", align = "center"),
            h4(alert1[3], style = "color:white", align = "center"),
            h4(alert1[4], style = "color:white", align = "center"),
            h4(alert1[5], style = "color:white", align = "center")
        )
        
    })  
    
    ##################
    ### tab 3: Map ###
    ##################
    states<-st_read("states.shp")
    
    data01=url("https://data.covidactnow.org/latest/us/states.OBSERVED_INTERVENTION.timeseries.csv")
    data01=read.csv(data01)
    
    output$dateText  <- renderText({
        paste("input$date is", as.character(input$date))
    })
    
    output$plot4 <- renderPlotly({
        
        df = data01%>%
            filter(data01$stateName==input$singlestate)
        df$date<-as.Date(df$date)
        fig <- plot_ly( x=df$date, y=df$cumulativeDeaths,type="scatter",mode="line",name="cumulativeDeath")%>%
            rangeslider()%>%add_trace(y=df$currentInfected,name="currentInfected")%>%
            layout(hovermode="x")
        fig
        
        
        
    })
    
    output$myMap = renderLeaflet({
        
        data02=data01%>%
            filter(as.character(format(date))==input$date)
        
        datajoin <- left_join(data02,states,by=c("stateName"="name")) %>%
            st_as_sf() 
        num_popup <- paste0("<strong> </strong>", 
                            datajoin$iso_3166_2, 
                            "<br><strong> Date: </strong>", 
                            input$date,
                            "<br><strong> Current Infected: </strong>", 
                            datajoin$currentInfected,
                            "<br><strong> Cumulative Infected: </strong>", 
                            datajoin$cumulativeInfected,
                            "<br><strong> Cumulative Death: </strong>", 
                            datajoin$cumulativeDeaths
                            
        )
        if(input$Statecharacter =="cumulativeInfected"){
            pal<-colorNumeric("YlOrRd", as.numeric(datajoin$cumulativeInfected))
            
            leaflet(datajoin) %>%
                addProviderTiles(providers$Stamen.Toner) %>%
                addPolygons(fillColor = ~pal(cumulativeInfected),popup = num_popup,
                            weight=2,fillOpacity = 0.8,color = "white",dashArray="3",opacity = 1,
                            highlight = highlightOptions(
                                fillColor = "yellow",
                                opacity = 1, weight = 2,
                                fillOpacity = 1,
                                #bringToFront = FALSE
                                bringToFront = TRUE))%>%
                addLegend("bottomleft", pal = pal, values = ~as.numeric(datajoin$cumulativeInfected),
                          title = "Cumulative Infected",
                          opacity = 1
                )
        }else {
            if(input$Statecharacter =="currentInfected"){
                
                
                
                pal<-colorNumeric("YlOrRd", as.numeric(datajoin$currentInfected))
                
                leaflet(datajoin) %>%
                    addProviderTiles(providers$Stamen.Toner) %>%
                    addPolygons(fillColor = ~pal(currentInfected),popup = num_popup,
                                fillOpacity = 0.8,color="white",weight = 2,opacity = 1,
                                highlight = highlightOptions(
                                    fillColor = "orange",
                                    opacity = 1, weight = 2,
                                    fillOpacity = 1,
                                    #bringToFront = FALSE
                                    bringToFront = TRUE, sendToBack = TRUE))%>%
                    addLegend("bottomleft", pal = pal,values =~datajoin$currentInfected,
                              title = "Current Infected",
                              opacity = 1)
                
            }else {
                if(input$Statecharacter =="cumulativeDeaths"){
                    
                    
                    
                    pal<-colorNumeric("YlGnBu", as.numeric(datajoin$cumulativeDeaths))
                    
                    leaflet(datajoin) %>%
                        addProviderTiles(providers$Stamen.Toner) %>%
                        # setView(lng=-75.010613,lat=39.981634,zoom=11) %>%
                        addPolygons(fillColor = ~pal((cumulativeDeaths)),popup = num_popup,
                                    fillOpacity = 0.8,opacity = 1,color = "white",weight = 2,
                                    highlight = highlightOptions(
                                        fillColor = "yellow",
                                        opacity = 1, weight = 2,
                                        fillOpacity = 1,
                                        #bringToFront = FALSE
                                        bringToFront = TRUE, sendToBack = TRUE))%>%
                        addLegend("bottomleft", pal = pal,values =~datajoin$cumulativeDeaths,
                                  title = "Cumulative Death",
                                  opacity = 1)
                    
                }
                
            }
            
        }
        
    })
    
    ########################
    ### tab 4: LineChart ###
    ########################
    
    ### subtab1: all states
    updateRadioButtons(session = session, inputId = "radio1")
    output$plotl1 <- renderPlot({
        if (input$radio1 == "Current infection rate") {
            Line1 <- ggplot(data = data, mapping = aes(x = Date, y = CurInfRate)) +
                labs(x = "Date", y = "Current infection rate")
        } else if (input$radio1 == "Cummulative infection rate") {
            Line1 <- ggplot(data = data, mapping = aes(x = Date, y = CumInfRate)) +
                labs(x = "Date", y = "Cummulative infection rate")
        } else if (input$radio1 == "Cummulative death rate") {
            Line1 <- ggplot(data = data, mapping = aes(x = Date, y = CumDeathRate)) +
                labs(x = "Date", y = "Cummulative death rate")
        } else if (input$radio1 == "Hospital bed usage rate") {
            Line1 <- ggplot(data = data, mapping = aes(x = Date, y = BedRate)) +
                labs(x = "Date", y = "Hospital bed usage rate")
        } else {
            Line1 <- ggplot(data = data, mapping = aes(x = Date, y = ICURate)) +
                labs(x = "Date", y = "ICU bed usage rate")
        }
        Line1 <- Line1 + geom_line(aes(group = State, color = Type)) + facet_wrap(~ State) +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette1) +
            scale_y_continuous(labels = scales::percent)
        return(Line1)
    })
    
    ### subtab2: three states
    updateRadioButtons(session = session, inputId = "radio2")
    updateSelectInput(session = session, inputId = "select1", choices = unique(data$State))
    updateSelectInput(session = session, inputId = "select2", choices = unique(data$State))
    updateSelectInput(session = session, inputId = "select3", choices = unique(data$State))
    updateActionButton(session = session, inputId = "action1")
    
    show1 <- eventReactive(input$action1, { input$select1 })
    show2 <- eventReactive(input$action1, { input$select2 })
    show3 <- eventReactive(input$action1, { input$select3 })
    
    output$plotl2 <- renderPlot({
        data1 <- data %>% filter(State == show1())
        data2 <- data %>% filter(State == show2())
        data3 <- data %>% filter(State == show3())
        if (input$radio2 == "Current infection rate") {
            Line2 <- ggplot(data = data1, mapping = aes(x = Date, y = CurInfRate)) +
                labs(x = "Date", y = "Current infection rate")
            m <- max(data1$CurInfRate, data2$CurInfRate, data3$CurInfRate)
        } else if (input$radio2 == "Cummulative infection rate") {
            Line2 <- ggplot(data = data1, mapping = aes(x = Date, y = CumInfRate)) +
                labs(x = "Date", y = "Cummulative infection rate")
            m <- max(data1$CumInfRate, data2$CumInfRate, data3$CumInfRate)
        } else if (input$radio2 == "Cummulative death rate") {
            Line2 <- ggplot(data = data1, mapping = aes(x = Date, y = CumDeathRate)) +
                labs(x = "Date", y = "Cummulative death rate")
            m <- max(data1$CumDeathRate, data2$CumDeathRate, data3$CumDeathRate)
        } else if (input$radio2 == "Hospital bed usage rate") {
            Line2 <- ggplot(data = data1, mapping = aes(x = Date, y = BedRate)) +
                labs(x = "Date", y = "Hospital bed usage rate")
            m <- max(data1$BedRate, data2$BedRate, data3$BedRate)
        } else {
            Line2 <- ggplot(data = data1, mapping = aes(x = Date, y = ICURate)) +
                labs(x = "Date", y = "ICU bed usage rate")
            m <- max(data1$ICURate, data2$ICURate, data3$ICURate)
        }
        Line2 <- Line2 + geom_line(aes(group = State, color = Type), size = 1) +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette1) +
            scale_y_continuous(labels = scales::percent, limits = c(0,m)) #+
        #transition_reveal(Date)
        return(Line2)
    })
    
    output$plotl3 <- renderPlot({
        data1 <- data %>% filter(State == show1())
        data2 <- data %>% filter(State == show2())
        data3 <- data %>% filter(State == show3())
        if (input$radio2 == "Current infection rate") {
            Line3 <- ggplot(data = data2, mapping = aes(x = Date, y = CurInfRate)) +
                labs(x = "Date", y = "Current infection rate")
            m <- max(data1$CurInfRate, data2$CurInfRate, data3$CurInfRate)
        } else if (input$radio2 == "Cummulative infection rate") {
            Line3 <- ggplot(data = data2, mapping = aes(x = Date, y = CumInfRate)) +
                labs(x = "Date", y = "Cummulative infection rate")
            m <- max(data1$CumInfRate, data2$CumInfRate, data3$CumInfRate)
        } else if (input$radio2 == "Cummulative death rate") {
            Line3 <- ggplot(data = data2, mapping = aes(x = Date, y = CumDeathRate)) +
                labs(x = "Date", y = "Cummulative death rate")
            m <- max(data1$CumDeathRate, data2$CumDeathRate, data3$CumDeathRate)
        } else if (input$radio2 == "Hospital bed usage rate") {
            Line3 <- ggplot(data = data2, mapping = aes(x = Date, y = BedRate)) +
                labs(x = "Date", y = "Hospital bed usage rate")
            m <- max(data1$BedRate, data2$BedRate, data3$BedRate)
        } else {
            Line3 <- ggplot(data = data2, mapping = aes(x = Date, y = ICURate)) +
                labs(x = "Date", y = "ICU bed usage rate")
            m <- max(data1$ICURate, data2$ICURate, data3$ICURate)
        }
        Line3 <- Line3 + geom_line(aes(group = State, color = Type), size = 1) +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette1) +
            scale_y_continuous(labels = scales::percent, limits = c(0,m))
        return(Line3)
    })
    
    output$plotl4 <- renderPlot({
        data1 <- data %>% filter(State == show1())
        data2 <- data %>% filter(State == show2())
        data3 <- data %>% filter(State == show3())
        if (input$radio2 == "Current infection rate") {
            Line4 <- ggplot(data = data3, mapping = aes(x = Date, y = CurInfRate)) +
                labs(x = "Date", y = "Current infection rate")
            m <- max(data1$CurInfRate, data2$CurInfRate, data3$CurInfRate)
        } else if (input$radio2 == "Cummulative infection rate") {
            Line4 <- ggplot(data = data3, mapping = aes(x = Date, y = CumInfRate)) +
                labs(x = "Date", y = "Cummulative infection rate")
            m <- max(data1$CumInfRate, data2$CumInfRate, data3$CumInfRate)
        } else if (input$radio2 == "Cummulative death rate") {
            Line4 <- ggplot(data = data3, mapping = aes(x = Date, y = CumDeathRate)) +
                labs(x = "Date", y = "Cummulative death rate")
            m <- max(data1$CumDeathRate, data2$CumDeathRate, data3$CumDeathRate)
        } else if (input$radio2 == "Hospital bed usage rate") {
            Line4 <- ggplot(data = data3, mapping = aes(x = Date, y = BedRate)) +
                labs(x = "Date", y = "Hospital bed usage rate")
            m <- max(data1$BedRate, data2$BedRate, data3$BedRate)
        } else {
            Line4 <- ggplot(data = data3, mapping = aes(x = Date, y = ICURate)) +
                labs(x = "Date", y = "ICU bed usage rate")
            m <- max(data1$ICURate, data2$ICURate, data3$ICURate)
        }
        Line4 <- Line4 + geom_line(aes(group = State, color = Type), size = 1) +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette1) +
            scale_y_continuous(labels = scales::percent, limits = c(0,m))
        return(Line4)
    })
    
    ### subtab3: one state
    updateSelectInput(session = session, inputId = "select4", choices = unique(data$State))
    updateActionButton(session = session, inputId = "action2")
    
    show <- eventReactive(input$action2, { input$select4 })
    
    output$plotl5 <- renderPlotly({
        data4 <- data %>% filter(State == show())
        Line5 <- ggplot(data = data4, mapping = aes(x = Date, y = CurInfRate)) +
            geom_line(aes(group = State, color = Type)) +
            labs(x = "Date", y = "Current infection rate") +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette2) +
            scale_y_continuous(labels = scales::percent)
        return(Line5)
    })
    output$plotl6 <- renderPlotly({
        data4 <- data %>% filter(State == show())
        Line6 <- ggplot(data = data4, mapping = aes(x = Date, y = CumInf)) +
            geom_line(aes(group = State, color = Type)) +
            labs(x = "Date", y = "Cummulative infection number") +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette2) +
            scale_y_continuous(labels = scales::comma)
        return(Line6)
    })
    output$plotl7 <- renderPlotly({
        data4 <- data %>% filter(State == show())
        Line7 <- ggplot(data = data4, mapping = aes(x = Date, y = CumInfRate)) +
            geom_line(aes(group = State, color = Type)) +
            labs(x = "Date", y = "Cummulative infection rate") +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette2) +
            scale_y_continuous(labels = scales::percent)
        return(Line7)
    })
    output$plotl8 <- renderPlotly({
        data4 <- data %>% filter(State == show())
        Line8 <- ggplot(data = data4, mapping = aes(x = Date, y = CumDeathRate)) +
            geom_line(aes(group = State, color = Type)) +
            labs(x = "Date", y = "Cummulative death rate") +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette2) +
            scale_y_continuous(labels = scales::percent)
        return(Line8)
    })
    output$plotl9 <- renderPlotly({
        data4 <- data %>% filter(State == show())
        Line9 <- ggplot(data = data4, mapping = aes(x = Date, y = BedRate)) +
            geom_line(aes(group = State, color = Type)) +
            labs(x = "Date", y = "Hospital bed usage rate") +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette2) +
            scale_y_continuous(labels = scales::percent)
        return(Line9)
    })
    
    output$plotl10 <- renderPlotly({
        data4 <- data %>% filter(State == show())
        Line10 <- ggplot(data = data4, mapping = aes(x = Date, y = ICURate)) +
            geom_line(aes(group = State, color = Type)) +
            labs(x = "Date", y = "ICU bed usage rate") +
            theme(legend.position = "none", axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            scale_color_manual(values = palette2) +
            scale_y_continuous(labels = scales::percent)
        return(Line10)
    })
    
}


