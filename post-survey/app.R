# http://rstudio-pubs-static.s3.amazonaws.com/302891_3a8f5170171545248977bbb7b015f546.html
# http://rpubs.com/msteiner/ShinyPsych_TextfileTutorial

# Example of a Survey using the ShinyPsych package
#
# Code sections:
#   - Section 0: Load Libraries
#   - Section A: assign external values
#   - Section B: Define overall layout
#   - Section C: Define reactive values
#   - Section D: Page layouts
#   - Section F: Event (e.g. button) actions
#       - Section F1: Page navigation button
#       - Section F2: Event Control
#   - Section G: Save Data

# Section 0: Load Libraries ====================================================

library(shiny)
library(shinyjs)
library(ShinyPsych)

# Section A: assign external values ============================================

# Directory to save data
outputDir <- "./post-survey-data/"

# Vector with page ids used to later access objects
idsVec <- c("Instructions", "Survey_one", "Survey_two", "Goodbye")

# create page lists for the instructions and the last page
instructions.list <- createPageList(fileName = "Instructions.txt",
                                    globId = "Instructions",
                                    defaulttxt = FALSE)
survey.list <- createPageList(fileName = "Survey_one.txt",
                              globId = "Survey_one",
                              defaulttxt = FALSE)
survey_two.list <- createPageList(fileName = "Survey_two.txt",
                              globId = "Survey_two",
                              defaulttxt = FALSE)
goodbye.list <- createPageList(fileName = "Goodbye.txt",
                               defaulttxt = FALSE)


# Section B: Define overall layout =============================================

ui <- fixedPage(

  # App title
  title = "2019 R Bootcamp Survey",
  uiOutput("MainAction"),

  # For Shinyjs functions
  useShinyjs(),

  # include appropriate css and js scripts
  includeScriptFiles()

)

server <- function(input, output, session) {

  output$MainAction <- renderUI( {
    PageLayouts()

  })

  # Section C: Define Reactive Values ==========================================

  # CurrentValues controls page setting such as which page to display
  CurrentValues <- createCtrlList(firstPage = "instructions", # id of the first page
                                  globIds = idsVec,           # ids of pages for createPage
                                  complCode = TRUE,           # create a completion code
                                  complName = "2019_R_Bootcamp_Survey")    # first element of completion code

  # Section D: Page Layouts ====================================================

  PageLayouts <- reactive({

    # insert created completion code that it can later be displayed
    goodbye.list <- changePageVariable(pageList = goodbye.list, variable = "text",
                                       oldLabel = "completion.code",
                                       newLabel = CurrentValues$completion.code)

    # display instructions page
    if (CurrentValues$page == "instructions") {

      return(
        # create html logic of instructions page
        createPage(pageList = instructions.list,
                   pageNumber = CurrentValues$Instructions.num,
                   globId = "Instructions", ctrlVals = CurrentValues)
      )}

    # display survey page
    if (CurrentValues$page == "survey_one") {

      return(
        # create html logic of instructions page
        createPage(pageList = survey.list,
                   pageNumber = CurrentValues$Survey_one.num,
                   globId = "Survey_one", ctrlVals = CurrentValues)
      )}


    if (CurrentValues$page == "survey_two"){

      return(
        createPage(pageList = survey_two.list, pageNumber = CurrentValues$Survey_two.num,
                   globId = "Survey_two", ctrlVals = CurrentValues)
      )}


    # P5) Goodbye
    if (CurrentValues$page == "goodbye") {

      return(
        createPage(pageList = goodbye.list, pageNumber = CurrentValues$Goodbye.num,
                   globId = "Goodbye", ctrlVals = CurrentValues, continueButton = FALSE)
      )}

  })


  # Section F: Event (e.g.; button) actions ======================================

  # Section F1: Page Navigation Buttons ----------------------


  observeEvent(input[["Instructions_next"]],{
    nextPage(pageId = "instructions", ctrlVals = CurrentValues, nextPageId = "survey_one",
             pageList = instructions.list, globId = "Instructions")
  })

  observeEvent(input[["Survey_one_next"]],{
    nextPage(pageId = "survey_one", ctrlVals = CurrentValues,
             nextPageId = "survey_two", pageList = survey.list,
             globId = "Survey_one")
  })


  # Section F2: Event Control ----------------------

  # Make sure answers are selected
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = "instructions", ctrlVals = CurrentValues,
                  pageList = instructions.list, globId = "Instructions",
                  inputList = input)

    onInputEnable(pageId = "survey_one", ctrlVals = CurrentValues,
                  pageList = survey.list, globId = "Survey_one",
                  inputList = input, charNum = 4)

    onInputEnable(pageId = "survey_two", ctrlVals = CurrentValues,
                  pageList = survey_two.list, globId = "Survey_two",
                  inputList = input)

  })

  # Section G: Save data =========================================================

  observeEvent(input[["Survey_two_next"]], {(

    # Create progress message
    withProgress(message = "Saving data...", value = 0, {

      incProgress(.25)

      # Create a list to save data
      data.list <- list(  "title" = input$Survey_one_title,
                          "area" = input$Survey_one_area,
                          "tracks" = input$Survey_one_tracks,
                          "slowr11_intro" = input$Survey_one_slowr11_intro,
                          "slowr11_diff" = input$Survey_one_slowr11_diff,
                          "slowr21_index" = input$Survey_one_slowr21_index,
                          "slowr21_diff" = input$Survey_one_slowr21_diff,
                          "slowr22_funcs" = input$Survey_one_slowr22_funcs,
                          "slow22_diff" = input$Survey_one_slow22_diff,
                          "fastr11_mlm" = input$Survey_one_fastr11_mlm,
                          "fastr11_diff" = input$Survey_one_fastr11_diff,
                          "fastr21_laavan" = input$Survey_one_fastr21_laavan,
                          "fastr21_diff" = input$Survey_one_fastr21_diff,
                          "fastr22_dynr" = input$Survey_one_fastr22_dynr,
                          "fastr22_diff" = input$Survey_one_fastr22_diff,
                          "day31_wrangle" = input$Survey_one_day31_wrangle,
                          "day31_diff" = input$Survey_one_day31_diff,
                          "day32_ggplot" = input$Survey_one_day32_ggplot,
                          "day32_diff" = input$Survey_one_day32_diff,
                          "day33_rr" = input$Survey_one_day33_rr,
                          "day33_diff" = input$Survey_one_day33_diff,
                          "day34_data" = input$Survey_one_day34_data,
                          "day34_diff" = input$Survey_one_day34_diff,
                          "gen1_handson" = input$Survey_one_gen1_handson,
                          "gen2_iprac" = input$Surbey_one_gen2_iprac,

                          ## Survey Two
                          "most_helpful" = input$Survey_two_most_helpful,
                          "schedule_changes" = input$Survey_two_schedule_changes,
                          "fav_comp" = input$Survey_two_fav_comp,
                          "other_topics" = input$Survey_two_other_topics,
                          "other_feedback" = input$Survey_two_other_feedback
                          )

      # save Data
      saveData(data.list, location = "local", outputDir = outputDir,
               partId = data.list$id, suffix = "_s")

      CurrentValues$page <- "goodbye"

    })

  )})

}

# Create app!
shinyApp(ui = ui, server = server)
