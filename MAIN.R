if (!require ("tidyverse")) install.packages("tidyverse")
if (!require ("plotly")) install.packages("plotly")
if (!require ("shinydashboard")) install.packages("shinydashboard")
if (!require ("shiny")) install.packages("shiny")
if (!require ("leaflet")) install.packages("leaflet")
if (!require ("DT")) install.packages("DT")
if (!require ("forcats")) install.packages("forcats")
if (!require ("lubridate")) install.packages("lubridate")
if (!require ("hrbrthemes")) install.packages("hrbrthemes")
if (!require ("gcookbook")) install.packages("gcookbook")
if (!require ("reshape")) install.packages("reshape")
if (!require ("reshape2")) install.packages("reshape2")
if (!require ("wordcloud")) install.packages("wordcloud")
if (!require ("rtweet")) install.packages("rtweet")
if (!require ("wordcloud2")) install.packages("wordcloud2")
if (!require ("corpus")) install.packages("corpus")
if (!require ("SnowballC")) install.packages("SnowballC")
if (!require ("tm")) install.packages("tm")
if (!require ("tidytext")) install.packages("tidytext")
if (!require ("topicmodels")) install.packages("topicmodels")
if (!require ("shinythemes")) install.packages("shinythemes")
if (!require ("dashboardthemes")) install.packages("dashboardthemes")

# Load packages ----
library('tidyverse')
library('shinydashboard')
library('shiny')
library('leaflet')
library('lubridate')
library('forcats')
library('hrbrthemes')
library('gcookbook')
library("reshape2")
library("wordcloud")
library("rtweet")
library("wordcloud2")
library("corpus")
library("SnowballC")
library("tm") 
library("tidytext")
library("topicmodels")
library("plotly")
library("shinythemes")
library("dashboardthemes")
library("reshape")
library("DT")

# Hospitality data ----
# By gender in total industry
hospitality <- read.csv('2021Tourism_Hospitality.csv') %>%
  gather(key = "gender", value = "N", 3:4) %>%
  mutate(gender = factor(gender, levels = c("Male", "Female")),
         sector = factor(sector, levels = c("Accommodation", "FoodBeverage", "RecreationEnter", "Tourism", "Transportation & Travel"))) %>%
  ungroup()



hospitality$N = as.numeric(str_replace_all(hospitality$N, pattern = fixed(","), replacement = ""))
hospitality$Total = as.numeric(str_replace_all(hospitality$Total, pattern = fixed(","), replacement = ""))
names(hospitality)[1] <- paste("Region")

hospitality <-  hospitality %>%
  group_by(gender, sector) %>%
  mutate(percent = round((N/Total), digits = 3))

hospitality %>%
  filter(gender %in% "Male") %>%
  summarize(n = percent)


hospitality_plot <- ggplot(hospitality, aes(x = sector, y = percent, fill = gender)) +
  geom_col(position = "dodge", width=0.55) +
  scale_fill_manual(values=c("#64B8CE","#315B94")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_ipsum(grid=FALSE, axis=FALSE,
              base_family = "") +
  theme(legend.key.size = unit(0.8,"line"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.margin=margin(t = -0.25, unit='cm')) +
  labs(x = "",
       y = "",
       title = "Hospitality Industry by Gender",
       subtitle = "Workforce composition by gender in sectors of hospitality industry, %")

# Distribution of genders in hospitality industry overall
hosp_gender_dist <- hospitality %>%
  group_by(gender) %>%
  summarize(mean = mean(percent))

# Median percent by gender
hospitality %>%
  group_by(gender) %>%
  summarize(median = median(percent))

# Remove Transportation and Travel which is skewing mean
hosp_gender_dist2 <- hospitality %>%
  filter(!sector %in% "Transportation & Travel") %>%
  group_by(gender) %>%
  mutate(mean = mean(percent)) %>%
  summarize(mean = min(mean))


# By age in total industry ----
hospitality2 <- read.csv('2021Tourism_Hospitality2.csv') %>%
  gather(key = age, value = "N", 3:8) 


# Clean strings in age variable
hospitality2$N <- as.numeric(str_replace_all(hospitality2$N, pattern = fixed(","), replacement = ""))
hospitality2$Total <- as.numeric(str_replace_all(hospitality2$Total, pattern = fixed(","), replacement = ""))
hospitality2$age <- str_remove_all(hospitality2$age, pattern = fixed("X"))
hospitality2$age <- str_replace_all(hospitality2$age, pattern = fixed("."), replacement = " ")
hospitality2$age <- str_replace_all(hospitality2$age, pattern = " to ", replacement = "-")

hospitality2 <- hospitality2 %>%
  group_by(sector, age) %>%
  mutate(percent = round((N/Total), digits = 5))

names(hospitality2)[1] <- paste("Region")

hospitality2 %>%
  filter(sector %in% "Accommodation") %>%
  summarize(n = percent)

# Hospitality Age Plot
hosp_age_plot <- ggplot(hospitality2, aes(x = sector, y = percent, fill = fct_rev(age))) +
  geom_col(position = "stack", width = 0.5) +
  scale_fill_brewer() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_ipsum(base_family = "", grid=FALSE, axis=FALSE) +
  theme(legend.key.size = unit(0.8,"line"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.margin=margin(t = -0.25, unit='cm')) +
  labs(x = "",
       y = "",
       title = "Hospitality Industry by Age",
       subtitle = "Workforce composition by age in sectors of hospitality industry, %")

# Hospitality Industry by Education ----
hospitality3 <- read.csv('2021Tourism_Hospitality3.csv')

hospitality3$Below.High.School <- as.numeric(gsub(",","", hospitality3$Below.High.School))
hospitality3$High.School.or.Some.post.secondary.education <- as.numeric(gsub(",","", hospitality3$High.School.or.Some.post.secondary.education))
hospitality3$Degree.Below.Bachelor.s <- as.numeric(gsub(",","", hospitality3$Degree.Below.Bachelor.s))
hospitality3$Bachelor.s.degree.or.Above <- as.numeric(gsub(",","", hospitality3$Bachelor.s.degree.or.Above))

hospitality3 <- hospitality3 %>%
  pivot_longer(3:6, names_to = "edu level", values_to = "count") 

hospitality3 <- hospitality3 %>%
  group_by(sector, `edu level`) %>%
  mutate(percent = round((count/Total), digits = 3)) %>%
  mutate(`edu level` = factor(`edu level`, levels = c("Below.High.School",
                                                      "High.School.or.Some.post.secondary.education",
                                                      "Degree.Below.Bachelor.s",
                                                      "Bachelor.s.degree.or.Above"))) %>%
  mutate(`edu level` = recode(`edu level`, "Below.High.School" = "Below High School",
                              "High.School.or.Some.post.secondary.education" = "High School or Some Post-Secondary",
                              "Degree.Below.Bachelor.s" = "Degree Below Bachelors",
                              "Bachelor.s.degree.or.Above" = "Bachelors Degree or Above"))

names(hospitality3)[1] <- paste("Region")

hosp_edu_plot <- ggplot(hospitality3, aes(x = `edu level`, y = percent)) +
  geom_col(width = 0.5, fill="#315B94") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_ipsum(base_family = "", grid=FALSE, axis=FALSE) +
  theme(legend.key.size = unit(0.8,"line"),
        legend.title = element_blank(),
        legend.position = "top",
        legend.margin=margin(t = -0.25, unit='cm')) +
  labs(x = "",
       y = "",
       title = "Tourism Sector by Highest\nEducation Level Attained",
       subtitle = "Workforce composition by education in tourism sector, %")

# Hospitality Industry by # of Employees 2017-2022 ----
hosp_nemp <- read_csv("bc_tourism_employment_2017-2022.csv") %>%
  gather("month", "sum", "Jan":"Dec") %>%
  na.omit() %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                          "Oct", "Nov", "Dec")))

# CERB BC Estimate Cleaning ----
cerb <- read.csv("CERB.csv") 
  
cerb <- data.frame(t(cerb)) # Transpose the dataset to get our variables
names(cerb) <- cerb[1,] # Make first row the variable names
cerb <- cerb[-1,] %>% #[row, column]
  filter(!Age %in% "15 and over") # Remove observations in 'Age' variable that refer to 15 and over | Redundant obs messing with plots
rownames(cerb) <- NULL # Remove row names

# Stack the columns with gender to avoid faceting for age

cerb$`Total CERB recipients ` <- as.numeric(gsub(",","", cerb$`Total CERB recipients `)) #Strip the commas to transform into numeric-type
cerb$Gender <- as.factor(cerb$Gender) # Change to factors

# Remove the Total for all genders
cerb <- cerb %>%
  filter(!Gender %in% "Total") %>% 
  group_by(Gender) %>%
  mutate(percent = (`Total CERB recipients ` / sum(`Total CERB recipients `))*100)

# Gather Cerb Recipients and Dates 
cerb_ent_ts <- cerb %>%
  select(Age, Gender, "Entitled for week starting 2020-03-15 ":"Entitled for week starting 2020-09-20 ") %>%
  gather(key = "Date", value = "num", "Entitled for week starting 2020-03-15 ":"Entitled for week starting 2020-09-20 ") 

# Gather CERB recipients making more than 5k+
cerb_5k_ts <- cerb %>%
  select(Age, Gender, "Percent of workers with earnings of $5,000 or more in 2019 who received CERB in week starting 2020-03-15 ":"Percent of workers with earnings of $5,000 or more in 2019 who received CERB in week starting 2020-09-20 ") %>%
  gather(key = "Date", value = "percent", 
         "Percent of workers with earnings of $5,000 or more in 2019 who received CERB in week starting 2020-03-15 ":"Percent of workers with earnings of $5,000 or more in 2019 who received CERB in week starting 2020-09-20 ")

# Turn Dates into Date-class variable type and remove extra strings
cerb_ent_ts$Date <- ymd(str_remove_all(cerb_ent_ts$Date, pattern = fixed("Entitled for week starting ")))
cerb_5k_ts$Date <- ymd(str_remove_all(cerb_5k_ts$Date, 
                                                      pattern = fixed("Percent of workers with earnings of $5,000 or more in 2019 who received CERB in week starting ")))
cerb_ent_ts$num <- as.numeric(gsub(",","", cerb_ent_ts$num))
cerb_5k_ts$percent <- as.numeric(str_replace_all(cerb_5k_ts$percent, pattern = fixed("%"), replacement = ""))

# CERB Recipients by age PLOT ----

cerb_5k_ts <- cerb_5k_ts %>%
  mutate(percent = percent/100)

cerb_recip <- ggplot(cerb_5k_ts, aes(x = as.factor(Age), y = percent, fill = Gender)) + # Transform the data by logging so we can visualize better
  geom_col(width = 0.5) + # Style points 
  theme_ipsum(base_family = "", grid = FALSE, axis = "x + y") + # Maintain theme
  scale_fill_brewer() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "Total CERB Recepients (%)",
       title = "Cerb Recepients by Age",
       fill = NULL,
       subtitle = "Breakdown of CERB Recipients by Age and Gender"
  ) +
  theme(legend.position = "top") +
  coord_flip() # Easier to see bar graphs this way

# Time-Series Graphs of COVID-19 recipients and key legislation dates
ent_lm <- lm(log(num) ~ Age + Gender, cerb_ent_ts)

cerb_ent_tsgraph <- ggplot(cerb_ent_ts, aes(x = Date, y = num, col = Age, shape = Gender)) +
  geom_point() +
  geom_line() +
  scale_fill_brewer() +
  geom_vline(xintercept = ymd("2020-03-16")) +
  geom_vline(xintercept = ymd("2020-03-18")) +
  geom_vline(xintercept = ymd("2020-03-20")) +
  geom_vline(xintercept = ymd("2020-03-21")) +
  geom_vline(xintercept = ymd("2020-05-19")) +
  geom_vline(xintercept = ymd("2020-06-24")) +
  theme_ipsum(base_family = "", grid="Y", axis="X") +
  labs(x = "",
       y = "",
       title = "Number of Entitled for CERB",
       subtitle = "BC  |  March 2020 - September 2020") +
  theme(legend.position = "top")



# CERB 5k+ previous year timeseries graph
cerb_5k_tsgraph <- ggplot(cerb_5k_ts, aes(x = Date, y = percent, col = Age, shape = Gender)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = ymd("2020-03-16")) +
  geom_vline(xintercept = ymd("2020-03-18")) +
  geom_vline(xintercept = ymd("2020-03-20")) +
  geom_vline(xintercept = ymd("2020-03-21")) +
  geom_vline(xintercept = ymd("2020-05-19")) +
  geom_vline(xintercept = ymd("2020-06-24")) +
  theme_ipsum(base_family = "", grid="Y", axis="X") +
  labs(x = "",
       y = "",
       title = "Percent of Workers who Recieved CERB",
       subtitle = "BC  |  Earnings of $5,000+ in 2019") +
  theme(legend.position = "top")

# Residual Plot
residplot1 <- ggplot(ent_lm, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(y = "Residuals",
       x = "Fitted Values",
       title = "Residuals versus Fitted"
       )

# Key Legislation Dates ----
# Found this one Wikipedia
key_dates <- data.frame(
  Date = as.Date(c("2020-03-16", "2020-03-16", "2020-03-20", "2020-03-21", "2020-05-19", "2020-06-24", "2020-10-19")),
  leg = c("Health officials ban all events with more than 50 people in an effort to curb the spread of COVID-19. The ban includes indoor and outdoor sporting events, conferences, meetings and religious gatherings. All bars and nightclubs are ordered to close",
          "B.C. declares a provincial state of emergency over the COVID-19 pandemic. As of Jan. 28, 2021, the province is still in a state of emergency after being renewed nearly two dozen times.",
          "Dr. Bonnie Henry orders the closure of all dine-in establishments, with takeout and delivery options still allowed. Playgrounds are also ordered to close.",
          "All \"personal service\" establishments are ordered to close in B.C., including salons and spas. As of this date, 10 people have died from COVID-19 in the province.",
          "B.C. announces it's moving to Phase 2 of its restart plan, allowing all stores, salons, restaurants, libraries, museums, child-care facilities and parks to reopen. Medical services like physiotherapy, dentistry and massage therapy are allowed to resume",
          "B.C. enters Phase 3 of its restart plan, allowing non-essential travel throughout the province.",
          "B.C. announces 2nd wave of COVID-19, as it confirms 499 new cases and 2 more deaths over the weekend. Provincial Health Officer Dr. Bonnie Henry said Monday that B.C. is in the second wave of the conronavirus pandemic, as she confirmed 499 new cases of COVID-19 and two more deaths over the weekend.")
)


# Topic Modelling ----
data <- read_csv("Hospitality.csv")
ndata <- data[which(data$textlabel <= 30), ]

# Create a document-term-matrix
news_covid_dtm <- ndata %>%
  unnest_tokens(word, text, token = "regex", pattern = "\\s+|[[:punct:]]+") %>% 
  anti_join(stop_words) %>% 
  mutate(stem = wordStem(word)) %>%
  count(textlabel, word) %>%
  cast_dtm(textlabel, word, n)

news_covid_lda <- LDA(news_covid_dtm, k = 5, control = list(seed = 11037))

# the topic-word probability vector
news_covid_topics <- tidy(news_covid_lda, matrix = "beta")
news_covid_topics

tm_covid_graph <- news_covid_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  scale_y_reordered()
##BETA (END)##


###########
## Shiny ##
###########

header <- dashboardHeader(
  title = "COVID-19 Impacts on Hospitality and Tourism Industry", 
  titleWidth = 450
)

CSS <- "
          /* Makes the drop down list go upwards*/
          .selectize-dropdown {
           bottom: 100% !important; 
           top: auto !important; 
           } 

          /* Remove horizontal scroll for data tables */
          .dataTables_scrollBody {
           overflow-x:hidden !important; 
           
           #tabBox { height:90vh !important; } 
           }
"

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  tags$head(
    tags$style(HTML(CSS))),
  tabItems(
    tabItem(tabName = 'HI',
            fluidRow(
               column(width = 6,
             tabBox(
             width = NULL,
             title = NULL,
             tabPanel("Breakdown by Age",
                      plotOutput("hosp_age_plot")
             ),
             tabPanel("Breakdown by Gender",
              plotOutput("hospitality_plot")
               
              
             ),
             tabPanel("Tourism by Education",
              plotOutput("hosp_edu_plot"))),
             
           box(
             width = NULL,
             subtitle = NULL,
             color = "blue",
             strong("The transportation and travel sector is heavily dominated by men."),
             p("- 70% of the sector is Male while only 30% are Female."),
             p("The data does not contain values for the age group 15-24 but all other groups have a fairly normal age distribution"),
             p("For tracking the changes in employment we choose February 2020 - end of April 2020 because it was before the March 16 legislation of closing bars, night clubs and events with more than 50 people and May 19th - The beginning of BC's reopening plan"),
             br(),
             strong("Employment Changes per sector | February 2020 - End of Apr 2020:"),
              p("Accomodation: 59.3% decrease"),
              p("Food & Beverage: 53.73% Decrease"),
              p("Recreation and Entertainment: 32.42% Decrease"),
              p("Tourism: 47.04% Decrease"),
              p("Transport & Travel: 42.12% Decrease"),
              p("46.9% Total Industry average in decrease of number of employees"),
             br(),
              strong("In terms of region:"),
              p("The lower mainland was most impacted while the other regions remained relatively stable. Except for the Accommodation sector - where Thompson Okanagan & the Kootenay's experienced a 50% decrease in employment, Vancouver Island/Coast experienced 62.86% decrease and the lower mainland a 74.47% decrease. Lots of seniors were impacted 55-64 yrs old and mainly women (Over 60% of sector).")

             )
        ),
        column(width = 6,
               fluidRow(
                 column(width = 3,
                        valueBox(width = NULL,
                                 subtitle = NULL,
                                 color = "teal",
                                 value = tags$p("All Sectors", style = "font-size: 40%")
                          
                        ),
               valueBox(width = NULL,
                        subtitle = "Male",
                        color = "blue",
                        paste0(formatC(min(hosp_gender_dist$mean)*100, digits = 0, big.mark = ",", big.interval = 3, format = "f"), "%")
                 
               ),
               valueBox(width = NULL,
                        subtitle = "Female",
                        color = "red",
                        paste0(formatC(max(hosp_gender_dist$mean)*100, digits = 0, big.mark = ",", big.interval = 3, format = "f"), "%")
                
                  )
               ),
               column(width = 3,
                      valueBox(width = NULL,
                               subtitle = NULL,
                               color = "teal",
                               value = tags$p("w/o Transport & Travel", style = "font-size: 35%")
                        
                      ),
                      valueBox(width = NULL,
                               subtitle = "Male",
                               color = "blue",
                               paste0(formatC(min(hosp_gender_dist2$mean)*100, digits = 0, big.mark = ",", big.interval = 3, format = "f"), "%")
                        
                      ),
                      valueBox(width = NULL,
                               subtitle = "Female",
                               color = "red",
                               paste0(formatC(max(hosp_gender_dist2$mean)*100, digits = 0, big.mark = ",", big.interval = 3, format = "f"), "%")
                  
                      )
                 
               )
            )
          ),
        column(width = 4,
               tabBox(width = NULL,
                      title = "Data Tables for Hospitality Industry",
                      tabPanel("Age",
                               DTOutput("hosp_age")),
                      tabPanel("Gender",
                               DTOutput("hosp_gender")),
                      tabPanel("Education",
                               DTOutput("hosp_edu"))
               )
           )
            )
    ),
    tabItem(tabName = "CERB",
        fluidRow(
          column(width = 6,
            tabBox(
              width = NULL,
              title = NULL,
              tabPanel("Entitled for CERB",
                       plotlyOutput("cerb_ent_ts")),
              tabPanel("% of Workers Who Recieved CERB",
                       plotlyOutput("cerb_5k_tsgraph"))),
            
            box(selectInput("v_select", label = "Gender", choices = unique(cerb$Gender), selected = "Female"), width = NULL),
            
            tabBox(width = NULL,
                   title = "Total # of Employees",
                   tabPanel("Line Graph",
                            plotlyOutput("hosp_nemp_plot"), width = NULL),
                   tabPanel("Bar Graph",
                            plotlyOutput("hosp_nemp_bar"), width = NULL))
          ),
          column(width = 6,
                 box(width = NULL,
                     title = strong("KEY LEGISLATION DATES"),
                     strong("MARCH 16, 2020"),
                     p("Health officials ban all events with more than 50 people in an effort to curb the spread of COVID-19. The ban includes indoor and outdoor sporting events, conferences, meetings and religious gatherings. All bars and nightclubs are ordered to close"),
                     strong("March 18, 2020"),
                     p("B.C. declares a provincial state of emergency over the COVID-19 pandemic. As of Jan. 28, 2021, the province is still in a state of emergency after being renewed nearly two dozen times.  "),
                     strong("March 20, 2020"),
                     p("Dr. Bonnie Henry orders the closure of all dine-in establishments, with takeout and delivery options still allowed. Playgrounds are also ordered to close. "),
                     strong("March 21, 2020"),
                     p("All \"personal service\" establishments are ordered to close in B.C., including salons and spas. As of this date, 10 people have died from COVID-19 in the province. "),
                     strong("May 19, 2020"),
                     p("B.C. announces it's moving to Phase 2 of its restart plan, allowing all stores, salons, restaurants, libraries, museums, child-care facilities and parks to reopen. Medical services like physiotherapy, dentistry and massage therapy are allowed to resume"),
                     strong("June 24, 2020"),
                     p("B.C. enters Phase 3 of its restart plan, allowing non-essential travel throughout the province. "),
                     strong("Oct. 19, 2020"),
                     p("B.C. announces 2nd wave of COVID-19, as it confirms 499 new cases and 2 more deaths over the weekend. Provincial Health Officer Dr. Bonnie Henry said Monday that B.C. is in the second wave of the conronavirus pandemic, as she confirmed 499 new cases of COVID-19 and two more deaths over the weekend.")
                   
                 ),
                 br(),
                 box(width = 6,
                   title = strong("Total # of Employees 2017-2022"),
                     checkboxGroupInput("r_sel", label = "Region", choices = sort(unique(hosp_nemp$region)), selected = "BC", width = NULL),
                     
                     selectInput("y_sel", label = "Year", choices = unique(hosp_nemp$year), selected = 2020, width = NULL),
                     
                     selectInput("sec_sel", label = "Sector", choices = sort(unique(hosp_nemp$sector)), selected = "Accommodation", width = NULL))
              )
        )
            ),
    tabItem(tabName = "TM",
          fluidRow(
            column(width = 6,
                tabBox(
                  width = NULL,
                  title = NULL,
                  tabPanel("Topic Mod (5)",
                           plotOutput("tm_covid_graph")),
                  tabPanel("Topic Mod (8)",
                    plotOutput("tm2")
                  ),
                  tabPanel("Table",
                           box(tableOutput("covidnews_lda"))
                )
               )
              )
             )
            )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Hospitality Industry',
             tabName = 'HI'
    ),
    menuItem('CERB',
             tabName = 'CERB'
    ),
    menuItem("Topic Modelling",
             tabName = "TM"
    )
  ),
  collapsed = TRUE)


ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)

server <- function(input, output, session){
  output$hospitality_plot <- renderPlot({hospitality_plot})
  output$stock_plot <- renderPlot({stock_market})
  output$hosp_age_plot <- renderPlot({hosp_age_plot})
  output$hosp_edu_plot <- renderPlot({hosp_edu_plot})
  output$tm_covid_graph <- renderPlot({tm_covid_graph})
  
  output$tm2 <- renderPlot({news_covid_topics %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      group_by(topic) %>%
      top_n(8, beta) %>% # Top 8 words and beta
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free", ncol = 3) + 
      scale_y_reordered()}) 
  
  output$covidnews_lda <- renderTable({terms(news_covid_lda, 5)})
  
  output$hosp_age <- renderDT({hospitality2 %>%
      select(-"Region", -"percent")}, # Don't need
                                     options = list(scrollY = '245px',
                                                    scrollX = FALSE, # Doesn't work | Use CSS instead
                                                    searching = FALSE, # Remove search bar
                                                    lengthChange = FALSE, # Remove user ability
                                                    pageLength = nrow(hospitality2), # Rows displayed per page (1 because removed pagination)
                                                    bPaginate = FALSE, # Remove pagination navigation from bottom
                                                    bInfo = FALSE, # Remove showing entries
                                                    columnDefs = list(list(className = 'dt-left', # Pad columns to left
                                                                           targets = 0:1)))) # Target columns (Only 1 col so 0:1)))
  # CERB SUM filtering gender based on input                                                                       
  output$hosp_gender <- renderDT({hospitality %>%
      ungroup() %>%
      select("sector", "percentMale", "percentFemale", "Total")  %>%
      distinct(percentFemale, .keep_all = TRUE) %>%
      dplyr::rename(`% Male` = percentMale, `% Female` = percentFemale)},
    options = list(scrollY = '175px',
                 scrollX = FALSE, # Doesn't work | Use CSS instead
                 searching = FALSE, # Remove search bar
                 lengthChange = FALSE, # Remove user ability
                 pageLength = nrow(hospitality), # Rows displayed per page (1 because removed pagination)
                 bPaginate = FALSE, # Remove pagination navigation from bottom
                 bInfo = FALSE, # Remove showing entries
                 columnDefs = list(list(className = 'dt-left', # Pad columns to left
                                        targets = 0:1)))) # Target columns (Only 1 col so 0:1))

  output$hosp_edu <- renderDT({hospitality3 %>%
      select(-"Region") %>%
      mutate(percent = paste0(formatC(min(percent)*100, digits = 0, big.mark = ",", big.interval = 3, format = "f"), "%"))},
                                     options = list(scrollY = '175px',
                                                    scrollX = FALSE, # Doesn't work | Use CSS instead
                                                    searching = FALSE, # Remove search bar
                                                    lengthChange = FALSE, # Remove user ability
                                                    pageLength = nrow(hospitality3), # Rows displayed per page (1 because removed pagination)
                                                    bPaginate = FALSE, # Remove pagination navigation from bottom
                                                    bInfo = FALSE, # Remove showing entries
                                                    columnDefs = list(list(className = 'dt-left', # Pad columns to left
                                                                           targets = 0:1)))) # Target columns (Only 1 col so 0:1)))
  # CERB SUM filtering gender based on input
  output$cerb_ent_ts <- renderPlotly({
    
  cerb_ent <- cerb_ent_ts %>%
      filter(Gender %in% input$v_select) %>%
      ggplot(aes(x = Date, y = num, col = Age)) +
      geom_point() +
      geom_line() +
      geom_vline(xintercept = ymd("2020-03-16")) +
      geom_vline(xintercept = ymd("2020-03-18")) +
      geom_vline(xintercept = ymd("2020-03-20")) +
      geom_vline(xintercept = ymd("2020-03-21")) +
      geom_vline(xintercept = ymd("2020-05-19")) +
      geom_vline(xintercept = ymd("2020-06-24"))+
    theme_ipsum(base_family = "", grid="Y", axis="X") +
    labs(x = "",
         y = "") +
    theme(legend.position = "top")
  
  ggplotly(cerb_ent) %>%
    layout(title = list(text = paste0("Number of Entitled for CERB in 2020",
                                 "<br>",
                                 "<sup>",
                                 "BC  |  March 2020 - September 2020",
                                 "</sup>")))
    
  })
  
  # CERB % Graph filtering out gender based on user input
  output$cerb_5k_tsgraph <- renderPlotly({
    
    cerb_5k <- cerb_5k_ts %>% 
      filter(Gender %in% input$v_select) %>%
      ggplot(aes(x = Date, y = percent*100, col = Age)) +
      geom_point() +
      geom_line() +
      geom_vline(xintercept = ymd("2020-03-16")) + # vertical lines don't show up on plotly but they do for normal ggplot
      geom_vline(xintercept = ymd("2020-03-18")) +
      geom_vline(xintercept = ymd("2020-03-20")) +
      geom_vline(xintercept = ymd("2020-03-21")) +
      geom_vline(xintercept = ymd("2020-05-19")) +
      geom_vline(xintercept = ymd("2020-06-24")) +
      theme_ipsum(base_family = "", grid="Y", axis="X") +
      labs(x = "",
           y = "") +
      theme(legend.position = "top")
    
    ggplotly(cerb_5k) %>%
      layout(title = list(text = paste0("Percent of Workers who Recieved CERB in 2020",
             "<br>",
             "<sup>",
             "BC  |  Earnings of $5,000+ in 2019",
             "</sup>")))
    
    })
  
  output$hosp_nemp_plot <- renderPlotly({
    
    # Plot will not output unless a minimum number of regions is selected
    
    req(between(length(input$r_sel), 1,6))
    
    # Plot object so we can render as interactive plotly
    hnemp <- hosp_nemp %>%
      filter(region %in% input$r_sel, year %in% input$y_sel, sector %in% input$sec_sel) %>% # Filter by user conditions
      ggplot(aes(x = month, y = sum, group = region, col = region)) +
      geom_point() +
      geom_line(size = 0.8) +
      theme_ipsum(base_family = "", grid = "X + Y", axis = "Y") +
      labs(x = "Month",
           y = "# of Employees",
           title = "Number of Employees for 2017-2022",
           subtitle = "BC  |  Sums broken down by Sector",
           col = "Region")
    
    # Add vertical lines for legislation only for 2020
    if (input$y_sel %in% 2020) {
      hnemp <- hnemp + geom_vline(xintercept = which(levels(hosp_nemp$month) == "Mar")) + # Gives position of factor on the x-axis
                       geom_vline(xintercept = which(levels(hosp_nemp$month) == "May")) +
                       geom_vline(xintercept = which(levels(hosp_nemp$month) == "Jun")) +
                       geom_vline(xintercept = which(levels(hosp_nemp$month) == "Oct"))}
    
    # Render as plotly object
    ggplotly(hnemp, tooltip = c("x", "y")) # Hover display will only show (x,y) values
    
    })
  
  
  # Bar graph
  output$hosp_nemp_bar <- renderPlotly({
    
    req(between(length(input$r_sel), 1,6))
    
    g <- hosp_nemp %>%
      filter(region %in% input$r_sel, year %in% input$y_sel, sector %in% input$sec_sel) %>%
      ggplot(aes(x = month, y = sum, fill = region)) +
      geom_col(position = "dodge", width = 0.5) +
      theme_ipsum(base_family = "", grid = "X", axis = "Y") +
      scale_fill_brewer()+
      labs(x = "Month",
           y = "# of Employees",
           title = "Number of Employees for 2017-2022",
           subtitle = "BC  |  Sums broken down by Sector",
           fill = "Region") 
    
    # If the input year is 2020, the indicators for the key legislation will be "annotated" by vertical bars
    if (input$y_sel %in% 2020) {
      
      g <- g + 
        geom_vline(xintercept = which(levels(hosp_nemp$month) == "Mar")) + # Gives position of factor on the x-axis
        geom_vline(xintercept = which(levels(hosp_nemp$month) == "May")) +
        geom_vline(xintercept = which(levels(hosp_nemp$month) == "Jun")) +
        geom_vline(xintercept = which(levels(hosp_nemp$month) == "Oct"))
    }
    
    # Output plotly object
    ggplotly(g, tooltip = c("x", "y"))
  }) 
  
}

shinyApp(ui, server)
