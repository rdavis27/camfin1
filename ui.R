shinyUI(pageWithSidebar(
    headerPanel("Campaign Finance Contributions for 2015-2016 Election Cycle through June 2016"),
    sidebarPanel(
        width = 2,
        textInput('name',       'Search NAME',       value = '') ,
        textInput('city',       'Search CITY',       value = '') ,
        textInput('state',      'Search STATE',      value = '') ,
        textInput('employer',   'Search EMPLOYER',   value = '') ,
        textInput('cmte_nm',    'Search CMTE_NM',    value = '') ,
        textInput('prty',       'Search PRTY',       value = '') ,
        textInput('candidate',  'Search CANDIDATE',  value = 'CLINTON') ,
        textInput('occupation', 'Search OCCUPATION', value = '') ,
        selectInput("xsort", "Sort by",
                    choice   = c("LAST_DATE","TOTAL_CONTRIB","N_CONTRIB"),
                    selected =               "TOTAL_CONTRIB"), 
        radioButtons("xsortdir", NULL, c("Ascending","Descending"), "Descending", inline = TRUE),
        checkboxGroupInput("xshow", "Show",
            choice   = c("NAME","CITY","STATE","EMPLOYER","CMTE_NM","PRTY","CANDIDATE","OCCUPATION","LAST_DATE","TOTAL_CONTRIB","N_CONTRIB"),
            selected = c("NAME","CITY","STATE",           "CMTE_NM","PRTY","CANDIDATE",             "LAST_DATE","TOTAL_CONTRIB","N_CONTRIB"),
            inline = TRUE),
        numericInput("colwidth", "Maximum Column Width", value = "40") ,
        numericInput("totwidth", "Maximum Total Width",  value = "240") ,
        numericInput("totrows",  "Maximum Total Rows",   value = "900") ,
        
        helpText('GRAPH PARAMETERS') ,
        #textInput('grfnum',   'Number of Entries', value = '50') ,
        #textInput('grfminx',  'Minimum X (thousands)', value = '') ,
        #textInput('grfmidx',  'Middle X (thousands)', value = '') ,
        #textInput('grfmaxx',  'Maximum X (thousands)', value = '') ,
        numericInput("grfnum",  "Number of Entries",     value = "50") ,
        numericInput("grfminx", "Minimum X (thousands)", value = "") ,
        numericInput("grfmidx", "Middle X (thousands)",  value = "") ,
        numericInput("grfmaxx", "Maximum X (thousands)", value = "") ,
        selectInput("grfsfld", "Subfield",
                    choice   = c("NAME","CITY","STATE","EMPLOYER","CMTE_NM","PRTY","CANDIDATE","OCCUPATION","LAST_DATE","TOTAL_CONTRIB","N_CONTRIB"),
                    selected =                                                     "CANDIDATE"), 
        radioButtons("xrankdir", "Rank Order", c("Ascending","Descending"), "Descending", inline = TRUE) ),
    mainPanel(
        div(
            tabsetPanel(id = "tabs",
                tabPanel("Output",
                    width = 10,
                    verbatimTextOutput('myText')
                ),
                tabPanel("Plot",
                         width = 10,
                         #imageOutput('myImage')
                         plotOutput("myPlot", width = "840", height = "840")
                ),
                tabPanel("Usage",
                    width = 10,
                    includeMarkdown("camfin.Rmd")
                )
            )
        ),
        width = 10)
    )
)