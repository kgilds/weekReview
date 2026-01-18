library(shiny)
library(bslib)
library(DT)
library(quarto)

# UI
ui <- page_sidebar(
  title = "Invoice Generator",
  sidebar = sidebar(
    h4("Company Information"),
    textInput("company_name", "Company Name", value = "Your Company Name"),
    textAreaInput("company_address", "Company Address", 
                  value = "123 Business St\nCity, State 12345\nPhone: (555) 123-4567"),
    
    hr(),
    
    h4("Client Information"),
    textInput("client_name", "Client Name", value = ""),
    textAreaInput("client_address", "Client Address", value = ""),
    
    hr(),
    
    h4("Invoice Details"),
    textInput("invoice_number", "Invoice Number", value = paste0("INV-", format(Sys.Date(), "%Y%m%d"))),
    dateInput("invoice_date", "Invoice Date", value = Sys.Date()),
    dateInput("due_date", "Due Date", value = Sys.Date() + 30),
    
    hr(),
    
    actionButton("generate_invoice", "Generate Invoice", class = "btn-primary")
  ),
  
  card(
    card_header("Invoice Items"),
    fluidRow(
      column(4, textInput("item_description", "Description", placeholder = "Service/Product")),
      column(2, numericInput("item_quantity", "Qty", value = 1, min = 0, step = 0.1)),
      column(2, numericInput("item_rate", "Rate", value = 0, min = 0, step = 0.01)),
      column(2, textOutput("item_total")),
      column(2, actionButton("add_item", "Add Item", class = "btn-success"))
    ),
    br(),
    DTOutput("invoice_table"),
    br(),
    fluidRow(
      column(8),
      column(4,
             wellPanel(
               h5("Invoice Summary"),
               textOutput("subtotal"),
               numericInput("tax_rate", "Tax Rate (%)", value = 0, min = 0, max = 100, step = 0.1),
               textOutput("tax_amount"),
               hr(),
               h4(textOutput("total_amount"))
             )
      )
    )
  ),
  
  card(
    card_header("Generated Invoice"),
    downloadButton("download_invoice", "Download PDF Invoice", class = "btn-info"),
    br(),br(),
    verbatimTextOutput("generation_status")
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store invoice items
  invoice_items <- reactiveVal(data.frame(
    Description = character(0),
    Quantity = numeric(0),
    Rate = numeric(0),
    Total = numeric(0),
    stringsAsFactors = FALSE
  ))
  
  # Calculate item total
  output$item_total <- renderText({
    total <- input$item_quantity * input$item_rate
    paste("Total: $", sprintf("%.2f", total))
  })
  
  # Add item to invoice
  observeEvent(input$add_item, {
    if(input$item_description != "" && input$item_quantity > 0 && input$item_rate >= 0) {
      current_items <- invoice_items()
      new_item <- data.frame(
        Description = input$item_description,
        Quantity = input$item_quantity,
        Rate = input$item_rate,
        Total = input$item_quantity * input$item_rate,
        stringsAsFactors = FALSE
      )
      invoice_items(rbind(current_items, new_item))
      
      # Clear inputs
      updateTextInput(session, "item_description", value = "")
      updateNumericInput(session, "item_quantity", value = 1)
      updateNumericInput(session, "item_rate", value = 0)
    }
  })
  
  # Display invoice table
  output$invoice_table <- renderDT({
    items <- invoice_items()
    if(nrow(items) > 0) {
      items$Rate <- sprintf("$%.2f", items$Rate)
      items$Total <- sprintf("$%.2f", items$Total)
    }
    datatable(items, options = list(pageLength = 10, dom = 't'), 
              selection = 'single', rownames = FALSE)
  })
  
  # Calculate subtotal
  subtotal <- reactive({
    sum(invoice_items()$Total, na.rm = TRUE)
  })
  
  # Display subtotal
  output$subtotal <- renderText({
    paste("Subtotal: $", sprintf("%.2f", subtotal()))
  })
  
  # Calculate tax
  tax_amount <- reactive({
    subtotal() * (input$tax_rate / 100)
  })
  
  # Display tax amount
  output$tax_amount <- renderText({
    paste("Tax (", input$tax_rate, "%): $", sprintf("%.2f", tax_amount()), sep = "")
  })
  
  # Calculate total
  total <- reactive({
    subtotal() + tax_amount()
  })
  
  # Display total
  output$total_amount <- renderText({
    paste("Total: $", sprintf("%.2f", total()))
  })
  
  # Generate invoice status
  generation_status <- reactiveVal("")
  
  output$generation_status <- renderText({
    generation_status()
  })
  
  # Generate and download invoice
  observeEvent(input$generate_invoice, {
    if(input$client_name == "" || nrow(invoice_items()) == 0) {
      generation_status("Please fill in client information and add at least one item.")
      return()
    }
    
    generation_status("Generating invoice...")
    
    tryCatch({
      # Create Quarto document content
      qmd_content <- generate_invoice_qmd(
        company_name = input$company_name,
        company_address = input$company_address,
        client_name = input$client_name,
        client_address = input$client_address,
        invoice_number = input$invoice_number,
        invoice_date = input$invoice_date,
        due_date = input$due_date,
        items = invoice_items(),
        subtotal = subtotal(),
        tax_rate = input$tax_rate,
        tax_amount = tax_amount(),
        total = total()
      )
      
      # Write QMD file
      qmd_file <- tempfile(fileext = ".qmd")
      writeLines(qmd_content, qmd_file)
      
      # Render to PDF
      pdf_file <- gsub("\\.qmd$", ".pdf", qmd_file)
      quarto_render(qmd_file, output_format = "pdf")
      
      # Copy to a permanent location for download
      final_pdf <- file.path(tempdir(), paste0("invoice_", input$invoice_number, ".pdf"))
      file.copy(pdf_file, final_pdf, overwrite = TRUE)
      
      generation_status(paste("Invoice generated successfully! File:", basename(final_pdf)))
      
      # Enable download
      output$download_invoice <- downloadHandler(
        filename = function() {
          paste0("invoice_", input$invoice_number, ".pdf")
        },
        content = function(file) {
          file.copy(final_pdf, file)
        },
        contentType = "application/pdf"
      )
      
    }, error = function(e) {
      generation_status(paste("Error generating invoice:", e$message))
    })
  })
}

# Function to generate Quarto markdown content
generate_invoice_qmd <- function(company_name, company_address, client_name, client_address,
                                 invoice_number, invoice_date, due_date, items,
                                 subtotal, tax_rate, tax_amount, total) {
  
  # Format addresses
  company_addr_lines <- strsplit(company_address, "\n")[[1]]
  client_addr_lines <- strsplit(client_address, "\n")[[1]]
  
  # Create items table
  items_table <- ""
  for(i in 1:nrow(items)) {
    items_table <- paste0(items_table, 
                          "| ", items$Description[i], " | ", 
                          items$Quantity[i], " | $", 
                          sprintf("%.2f", items$Rate[i]), " | $", 
                          sprintf("%.2f", items$Total[i]), " |\n")
  }
  
  qmd_content <- paste0(
    "---\n",
    "title: \"INVOICE\"\n",
    "format: \n",
    "  pdf:\n",
    "    documentclass: article\n",
    "    geometry: margin=1in\n",
    "    fontsize: 11pt\n",
    "---\n\n",
    
    "\\begin{center}\n",
    "\\Large\\textbf{", company_name, "}\n",
    "\\end{center}\n\n",
    
    paste(paste0("\\begin{center}\n", paste(company_addr_lines, collapse = "\\\\\n"), "\n\\end{center}\n\n")),
    
    "---\n\n",
    
    "\\begin{tabular}{ll}\n",
    "\\textbf{Invoice Number:} & ", invoice_number, " \\\\\n",
    "\\textbf{Invoice Date:} & ", format(invoice_date, "%B %d, %Y"), " \\\\\n",
    "\\textbf{Due Date:} & ", format(due_date, "%B %d, %Y"), " \\\\\n",
    "\\end{tabular}\n\n",
    
    "\\vspace{1cm}\n\n",
    
    "**Bill To:**\n\n",
    "**", client_name, "**\n\n",
    paste(client_addr_lines, collapse = "  \n"), "\n\n",
    
    "\\vspace{1cm}\n\n",
    
    "| Description | Quantity | Rate | Total |\n",
    "|-------------|----------|------|-------|\n",
    items_table, "\n",
    
    "\\vspace{0.5cm}\n\n",
    
    "\\begin{flushright}\n",
    "\\begin{tabular}{lr}\n",
    "\\textbf{Subtotal:} & \\$", sprintf("%.2f", subtotal), " \\\\\n",
    "\\textbf{Tax (", tax_rate, "\\%):} & \\$", sprintf("%.2f", tax_amount), " \\\\\n",
    "\\hline\n",
    "\\textbf{Total:} & \\textbf{\\$", sprintf("%.2f", total), "} \\\\\n",
    "\\end{tabular}\n",
    "\\end{flushright}\n\n",
    
    "\\vspace{2cm}\n\n",
    
    "\\textit{Thank you for your business!}\n\n",
    
    "\\textit{Payment is due by ", format(due_date, "%B %d, %Y"), ".}\n"
  )
  
  return(qmd_content)
}

# Run the app
shinyApp(ui = ui, server = server)
