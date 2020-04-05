library(shiny)
library(ggplot2)
library(dplyr)
library(maptools)
library(rgdal)
library(usmap)

ui <- shinyUI(
    navbarPage("AP Exams Results",
        tabPanel("Intro", 
            fluidPage( 
            titlePanel("AP Exam Results: A data visualization"),
            br(),
            h4("Advanced Placement Exams are standardized exams created by the College Board
            to measure level of content mastery of a specific subject."),
            h4("This tool was designed to compare test results among demographic groups and to visualize volume per year and region."),
            br(),
            br(),
            br(),
            br(),
            h5("Data sources:"),
            uiOutput("kaggle"),
            uiOutput("CB"),
            br(),   
            h5("Created by:"),
            uiOutput("Github"),
            )
        ),

        tabPanel("Compare results by demographic group (AP 2016)",
            fluidPage(
                titlePanel("2016 Advanced Placement Exam Scores"),
                br(),
                sidebarLayout(	

                    sidebarPanel(
                        selectInput("subject","Subject",
                        c("Art history" = "ART HISTORY",
                        "Biology" = "BIOLOGY",
                        "Calculus AB" = "CALCULUS AB",
                        "Calculus BC" = "CALCULUS BC",
                        "Chemistry" = "CHEMISTRY",
                        "Chinese Language and Culture" = "CHINESE LANGUAGE & CULTURE",
                        "Computer Science A" = "COMPUTER SCIENCE A",
                        "Macroeconomics" = "MACROECONOMICS",
                        "Microeconomics" = "MICROECONOMICS",
                        "English Language and Composition" = "ENGLISH LANGUAGE & COMPOSITION",
                        "English Literature and Composition" = "ENGLISH LITERATURE & COMPOSITION",
                        "Environmental Science" = "ENVIRONMENTAL SCIENCE",
                        "European History" = "EUROPEAN HISTORY",
                        "French Language and Culture" = "FRENCH LANGUAGE & CULTURE",
                        "German Language and Culure" = "GERMAN LANGUAGE & CULTURE",
                        "Government and Politics: Comparative" = "GOVERNMENT & POLITICS: COMPARATIVE",
                        "Government and Politics: U.S" = "GOVERNMENT & POLITICS: U.S.",
                        "Human Geography" = "HUMAN GEOGRAPHY",
                        "Italian Language and Culture" = "ITALIAN LANGUAGE & CULTURE",
                        "Japanese Language and Culture" = "JAPANESE LANGUAGE & CULTURE",
                        "Latin" = "LATIN",
                        "Music Theory" = "MUSIC THEORY",
                        "Physics C: Electricity and Magnetism" = "PHYSICS C: ELECTRICITY & MAGNETISM",
                        "Physics C: Mechanics" = "PHYSICS C: MECHANICS",
                        "Physics 1" = "PHYSICS 1",
                        "Physics 2" = "PHYSICS 2",
                        "Psychology" = "PSYCHOLOGY",
                        "Research" = "RESEARCH",
                        "Seminar" = "SEMINAR",
                        "Spanish Language" = "SPANISH LANGUAGE",
                        "Spanish Literature" = "SPANISH LITERATURE",
                        "Statistics" = "STATISTICS",
                        "Studio Art: Drawing" = "STUDIO ART: DRAWING",
                        "Studio Art: 2-D Design" = "STUDIO ART: 2-D DESIGN",
                        "Studio Art: 3-D Design" = "STUDIO ART: 3-D DESIGN",
                        "U.S History" = "U.S. HISTORY",
                        "World History" = "WORLD HISTORY"
                        )
                        ),
                        # To do: Alaska native and pacific islander
                        selectInput("group", "Group 1:", 
                        c('11th grade',
                        '12th grade',
                        'male',
                        'female',
                        'white',
                        'black',
                        'hispanic',
                        'asian',
                        #'american indian alaska native',
                        #'native hawaiian pacific islander',
                        'two or more races', 
                        'all students 2016')
                        ),

                        selectInput("group2", "Group 2:", 
                        c('11th grade',
                        '12th grade',
                        'male',
                        'female',
                        'white',
                        'black',
                        'hispanic',
                        'asian',
                        #'american indian alaska native',
                        #'native hawaiian pacific islander',
                        'two or more races', 
                        'all students 2016')
                        ),
                        plotOutput("totals"),
                        width = 3 
                        ),

                    mainPanel(

                        splitLayout(
                            plotOutput("subjectsPlot"),

                            plotOutput("subjectsPlot2")
                            )
                    )
                )
            )
        ),
        tabPanel("AP Exam volume by region", 
            fluidPage(
                titlePanel("AP Exam volume by region"),
                br(),
                sidebarPanel(
                    sliderInput("year", "Year:",
                    min = 2009, max = 2019,
                    value = 2016),
                    width = 3 ),
                mainPanel(  
                    plotOutput("volumePlot")
                )	
            )
        )
    )
)

# Load data for exams
examsData <- read.csv('./data/exams.csv')
examsData[is.na(examsData)] <- 0

names(examsData)<-c('subject', 'score', '11th grade', '12th grade','male','female',
'white', 'black','hispanic','asian',
'american indian alaska native','native hawaiian pacific islander',
'two or more races', 'all students 2016')

#Load data for volume plot
volume<-read.csv("./data/2019-Exam-Volume-by-Region.csv", na.strings = c("", "NA"))

regions<-read.csv("./data/statelatlong.csv")
regions_lat_long<-subset(regions, select = c("City", "Latitude", "Longitude"))



server <- function(input, output) {

    urlKaggle <- a("2016 Advanced Placement Exam Scores|Kaggle", 
        href="https://www.kaggle.com/collegeboard/ap-scores")
    output$kaggle <- renderUI({
      tagList("", urlKaggle)
    })
    urlCB <- a("Exam Volume by Region", 
        href="https://secure-media.collegeboard.org/digitalServices/pdf/research/2019/2019-Exam-Volume-by-Region.xls")
    output$CB <- renderUI({
      tagList("", urlCB)
    })
    urlGithub <- a("Lizette Lemus", 
        href="https://github.com/Lizette-Lemus")
    output$Github <- renderUI({
      tagList("", urlGithub)
    })
    

    plot_scores_group <- function(df, group){
        p<- ggplot(data=df, aes(x=score, y=eval(as.symbol(group))/sum(eval(as.symbol(group))), fill = score)) +
        geom_bar(stat="identity")+theme_minimal()+ggtitle(tolower(group))+
        theme(plot.title = element_text(size = 20, hjust = 0.5),
                axis.title=element_text(size=15), axis.text=element_text(size=15)) + 
        ylab("Percentage of students") + ylim(0, .75)
        return(p)
    }


    output$subjectsPlot <- renderPlot({
        examsSubject <-examsData[which(examsData$subject == input$subject & examsData$score%in%c(1:5)),]
        plot_scores_group(examsSubject, input$group)
        })

    output$subjectsPlot2 <- renderPlot({
        examsSubject <-examsData[which(examsData$subject == input$subject & examsData$score %in% c(1:5)),]
        plot_scores_group(examsSubject,input$group2)
        })

    output$totals <- renderPlot({
        totalsSubject<-examsData[which(examsData$subject== input$subject & examsData$score == "All"),]
        groups <- c(input$group, input$group2)
        totals<-as.numeric(totalsSubject[, groups])
        df<-data.frame(groups, totals)
        p <-ggplot(df, aes( y = totals, x = groups, fill = as.factor(totals))) +
        labs(fill = "Totals")+
        theme_minimal()+
        scale_fill_brewer(palette="Set2")+
        geom_bar(position="dodge", stat="identity") +
        ggtitle("Number of students that took the test")+ 
        theme(plot.title = element_text(hjust = 0.5)) 
        p
        })

    output$volumePlot <- renderPlot({
        vol_year<-subset(volume, select = c("Region",paste("X", input$year, sep = "")))
        names(vol_year) <-c("Region", "volume")
        vol_year$volume <- gsub(",","",vol_year$volume)
        vol_year$volume <-as.numeric(as.character(vol_year$volume))

        vol_year_latlong <-inner_join(vol_year, regions_lat_long, by = c("Region"="City"))
        vol_year_latlong <-subset(vol_year_latlong, select = c("Longitude","Latitude", "volume"))
        vol_transformed <-usmap_transform(vol_year_latlong)

        plot_usmap(fill = "gray", alpha = 0.2) +
        geom_point(data = vol_transformed, aes(x = Longitude.1, y = Latitude.1, size = volume),
        shape = 21, colour = "black", fill = "steelblue", stroke = 1) +
        scale_size_continuous(range = c(1, 20), limits = c(1, 1000000)) +
        labs(title = "Exam volume by region", size = "Volume") +
        theme(legend.position = "right", plot.title = element_text(size = 20, hjust = 0.5), 
            legend.title = element_text(size=15), legend.text = element_text(size=15))
        },
        height = 800, width = 900 
    )
}

shinyApp(ui, server)
