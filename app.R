library(shiny)
library(rsconnect)
library(rdrop2)

input_fields <- c("name", "email", "date", "vendor", "item",
                 "cat", "price", "quantity", "rec_date", "note")

outputDir <- "https://www.dropbox.com/home/ordering"


# user interface to interact with form
shinyApp(
    ui = fluidPage(theme = "bootstrap.css", 
                   shinyjs::useShinyjs(),
    div(id = "overall_form",
    h2(strong("Ordering Form")),
    shinyjs::hidden(div(id = "reset_msg", h3(em("Form Reset")))),
    div(id = "form gen info",
        div(textInput("name", "Name", placeholder = "First Name"),
            textInput("email", "Email Adress", 
                      placeholder = "eid@colostate.edu"),
            dateInput("date", "Date", value = Sys.Date())
        )
        ),
    div(id = "form"),
    shinyjs::hidden(
        div(
            id = "thankyou_msg",
            h3(strong("Item submitted, would you like to input another?"))
        )
    ),
    div(h4((strong("Item Information"))),
    div(id = "form_order_info",
            textInput("vendor", "Vendor", placeholder = "ex. VWR"),
            textInput("item", "Item", placeholder = "ex. 1000 ul tips"),
            textInput("cat", "Catalog #", placeholder = "83007-382"),
            numericInput("price", "Price $", value = ""),
            numericInput("quantity", "Quantity", value = 1),
            dateRangeInput("rec_date", "Desired Receiving Date",
                           start = Sys.Date()+2,
                           end = Sys.Date()+5),
            textInput("note", "Notes", placeholder = "ex. need one case")
        )),
    div(id = "buttons",
        actionButton("submit", "Submit Item", class = "btn-primary"),
        actionButton("order_complete", "Order Complete?", 
                     class = "btn-primary"),
        actionButton("reset", "Reset Form", class = "btn-primary")
    ),
    div(h2(strong("SPACE")), style = "color: white;")
    ),
    shinyjs::hidden(
        div(
            id = "quit_msg",
            h3(strong("Thanks for the order!"), style = "text-align: center")
        )
        )
),

    server = function(input, output, session) {
    
        # Drop box connection
        token <- drop_auth()
        saveRDS(token, "droptoken.rds")
        token <- readRDS("droptoken.rds")
        drop_acc(dtoken = token)
    
        formData <- reactive({
            data <- sapply(input_fields, function(x) input[[x]])
            data <- t(data)
            data
        })
        
        outputDir <- "ordering"
        
        saveData <- function(data) {
            data <- t(data)
            # Create a unique file name
            fileName <- paste0(format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv")
            # Write the data to a temporary file locally
            filePath <- file.path(tempdir(), fileName)
            write.csv(data, filePath, row.names = TRUE, quote = TRUE)
            # Upload the file to Dropbox
            drop_upload(filePath, path = outputDir)
        }
        
        observeEvent(input$submit, {
            saveData(formData())
            shinyjs::hide("form gen info")
            shinyjs::reset("form_order_info")
            shinyjs::show("thankyou_msg")
        })
        
        observeEvent(input$order_complete, {
            saveData(formData())
            shinyjs::hide("overall_form")
            shinyjs::show("quit_msg")
        })
        
        observeEvent(input$reset, {
            shinyjs::reset("overall_form")
            shinyjs::show("reset_msg")
        })
        
        loadData <- function() {
            # Read all the files into a list
            filesInfo <- drop_dir(outputDir)
            filePaths <- filesInfo$path
            data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
            # Concatenate all data together into one data.frame
            data <- do.call(rbind, data)
            data
        }
}
)