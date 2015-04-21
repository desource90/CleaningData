library(shiny)
library(dplyr)
library(jsonlite)
library(stringr)

shinyUI(fluidPage(

  titlePanel("Image Search"),

  sidebarPanel(
      textInput(inputId = 'first', label = 'First Name', value = "Katherine"),
      textInput(inputId = 'last', label = 'Last Name', value = "August-deWilde"),
      textInput(inputId = 'full', label = 'Full Name', value = "Katherine-August-deWilde"),
      helpText("Note: Separate different parts of the name with a dash
               Ex. John-A.-Smith"),      
      textInput(inputId = 'affiliated_orgs', label = 'Affiliated Organizations',
                value = "Equilar First-Republic-Bank TriNet"),
      helpText("Note: Separate different organizations with a space.",
               "If an organization name has multiple words, separate with",
               "a dash"),
      submitButton("Update Search")
  ),

  mainPanel(
    htmlOutput('image'),
    textOutput('image_link'),
    textOutput('source_link')
  )
))