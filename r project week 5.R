
# ============================================================
#       SMART SUPERMARKET SALES ANALYSIS SYSTEM
#             ADVANCED R SHINY DASHBOARD
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)

# ============================================================
# 1. INITIAL SALES DATA
# ============================================================

sales_data <- data.frame(
  
  InvoiceID = paste0("INV", sprintf("%03d", 1:12)),
  
  Date = as.Date(c(
    "2026-09-01", "2026-09-02", "2026-09-03",
    "2026-09-04", "2026-09-05", "2026-09-06",
    "2026-09-07", "2026-09-08", "2026-09-09",
    "2026-09-10", "2026-09-11", "2026-09-12"
  )),
  
  Product = c(
    "Rice", "Milk", "Bread", "Apple",
    "Shampoo", "Rice", "Biscuits", "Milk",
    "Soap", "Apple", "Coffee", "Bread"
  ),
  
  Category = c(
    "Groceries", "Dairy", "Bakery", "Fruits",
    "Personal Care", "Groceries", "Snacks",
    "Dairy", "Personal Care", "Fruits",
    "Beverages", "Bakery"
  ),
  
  Price = c(
    60, 30, 40, 120, 180, 60,
    25, 30, 50, 120, 150, 40
  ),
  
  Quantity = c(
    5, 10, 4, 3, 2, 8,
    12, 6, 5, 4, 3, 7
  ),
  
  Payment = c(
    "Cash", "UPI", "Card", "UPI",
    "Cash", "Card", "UPI", "Cash",
    "Card", "UPI", "Cash", "Card"
  ),
  
  Stock = c(
    40, 25, 30, 20, 15, 32,
    18, 19, 22, 16, 12, 23
  ),
  
  stringsAsFactors = FALSE
)

# Calculate sales and profit

sales_data$TotalSales <-
  sales_data$Price * sales_data$Quantity

sales_data$Profit <-
  round(sales_data$TotalSales * 0.10, 2)

# ============================================================
# 2. UI DESIGN
# ============================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "SUPERMARKET PRO"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Sales Records",
        tabName = "records",
        icon = icon("table")
      ),
      
      menuItem(
        "Add Sale",
        tabName = "add",
        icon = icon("plus-circle")
      ),
      
      menuItem(
        "Inventory",
        tabName = "inventory",
        icon = icon("cubes")
      ),
      
      menuItem(
        "Product Analysis",
        tabName = "analysis",
        icon = icon("bar-chart")
      ),
      
      menuItem(
        "Reports",
        tabName = "reports",
        icon = icon("download")
      ),
      
      menuItem(
        "About",
        tabName = "about",
        icon = icon("info-circle")
      )
      
    )
    
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(
        HTML(
          "
          body {
            font-family: Arial, sans-serif;
          }

          .small-box {
            border-radius: 14px;
          }

          .box {
            border-radius: 12px;
          }

          .main-header .logo {
            font-weight: bold;
          }

          .content-wrapper {
            background-color: #f4f6f9;
          }
          "
        )
      )
    ),
    
    tabItems(
      
      # ======================================================
      # DASHBOARD PAGE
      # ======================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        h2("Smart Supermarket Dashboard"),
        
        fluidRow(
          
          valueBoxOutput("totalSales", width = 3),
          
          valueBoxOutput("totalProfit", width = 3),
          
          valueBoxOutput("totalQuantity", width = 3),
          
          valueBoxOutput("totalTransactions", width = 3)
          
        ),
        
        fluidRow(
          
          box(
            width = 4,
            title = "Dashboard Filters",
            status = "primary",
            solidHeader = TRUE,
            
            dateRangeInput(
              "dateFilter",
              "Select Date Range",
              start = min(sales_data$Date),
              end = max(sales_data$Date)
            ),
            
            selectInput(
              "dashCategory",
              "Select Category",
              choices = c(
                "All",
                sort(unique(sales_data$Category))
              ),
              selected = "All"
            )
            
          ),
          
          box(
            width = 8,
            title = "Sales Trend",
            status = "info",
            solidHeader = TRUE,
            
            plotOutput(
              "trendPlot",
              height = "270px"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Category-wise Sales",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "categoryPlot",
              height = "280px"
            )
            
          ),
          
          box(
            width = 6,
            title = "Payment Method Analysis",
            status = "success",
            solidHeader = TRUE,
            
            plotOutput(
              "paymentPlot",
              height = "280px"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Quick Summary",
            status = "info",
            solidHeader = TRUE,
            
            verbatimTextOutput("summary")
            
          )
          
        )
        
      ),
      
      # ======================================================
      # SALES RECORDS PAGE
      # ======================================================
      
      tabItem(
        
        tabName = "records",
        
        h2("Sales Records"),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Search and Filter Records",
            status = "primary",
            solidHeader = TRUE,
            
            textInput(
              "searchProduct",
              "Search Product",
              value = ""
            ),
            
            selectInput(
              "filterCategory",
              "Select Category",
              choices = c(
                "All",
                sort(unique(sales_data$Category))
              ),
              selected = "All"
            ),
            
            downloadButton(
              "downloadCSV",
              "Download Filtered CSV"
            ),
            
            br(),
            br(),
            
            actionButton(
              "deleteSale",
              "Delete Selected Row",
              class = "btn-danger"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Sales Data Table",
            status = "info",
            solidHeader = TRUE,
            
            DTOutput("salesTable")
            
          )
          
        )
        
      ),
      
      # ======================================================
      # ADD SALE PAGE
      # ======================================================
      
      tabItem(
        
        tabName = "add",
        
        h2("Add New Sale"),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Enter Sale Details",
            status = "primary",
            solidHeader = TRUE,
            
            textInput(
              "productName",
              "Product Name",
              value = ""
            ),
            
            dateInput(
              "saleDate",
              "Sale Date",
              value = Sys.Date()
            ),
            
            selectInput(
              "productCategory",
              "Category",
              choices = sort(unique(sales_data$Category))
            ),
            
            numericInput(
              "productPrice",
              "Price (Rs.)",
              value = 10,
              min = 0.01
            ),
            
            numericInput(
              "productQuantity",
              "Quantity",
              value = 1,
              min = 1,
              step = 1
            ),
            
            selectInput(
              "paymentMethod",
              "Payment Method",
              choices = c(
                "Cash",
                "UPI",
                "Card"
              )
            ),
            
            numericInput(
              "stockValue",
              "Available Stock",
              value = 20,
              min = 0
            ),
            
            actionButton(
              "addSale",
              "Add Sale",
              class = "btn-success"
            ),
            
            br(),
            br(),
            
            textOutput("addMessage")
            
          )
          
        )
        
      ),
      
      # ======================================================
      # INVENTORY PAGE
      # ======================================================
      
      tabItem(
        
        tabName = "inventory",
        
        h2("Inventory Monitoring"),
        
        fluidRow(
          
          valueBoxOutput("lowStockCount", width = 4),
          
          valueBoxOutput("totalStock", width = 4),
          
          valueBoxOutput("stockProducts", width = 4)
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Low Stock Items",
            status = "danger",
            solidHeader = TRUE,
            
            DTOutput("inventoryTable")
            
          )
          
        )
        
      ),
      
      # ======================================================
      # PRODUCT ANALYSIS PAGE
      # ======================================================
      
      tabItem(
        
        tabName = "analysis",
        
        h2("Product Sales Analysis"),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Product-wise Sales",
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput(
              "productPlot",
              height = "350px"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Profit by Category",
            status = "success",
            solidHeader = TRUE,
            
            plotOutput(
              "profitPlot",
              height = "350px"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Product Statistics",
            status = "warning",
            solidHeader = TRUE,
            
            verbatimTextOutput("statistics")
            
          )
          
        )
        
      ),
      
      # ======================================================
      # REPORTS PAGE
      # ======================================================
      
      tabItem(
        
        tabName = "reports",
        
        h2("Reports and Downloads"),
        
        box(
          width = 12,
          title = "Export Reports",
          status = "info",
          solidHeader = TRUE,
          
          p(
            "Download the current sales data and inventory report."
          ),
          
          downloadButton(
            "downloadReport",
            "Download Sales Report"
          ),
          
          br(),
          br(),
          
          downloadButton(
            "downloadInventory",
            "Download Inventory Report"
          )
          
        )
        
      ),
      
      # ======================================================
      # ABOUT PAGE
      # ======================================================
      
      tabItem(
        
        tabName = "about",
        
        h2("About the Project"),
        
        box(
          width = 12,
          title = "Smart Supermarket Sales Analysis System",
          status = "info",
          solidHeader = TRUE,
          
          p(
            "This project is developed using R Programming, ",
            "Data Frames, Conditions, Loops, Functions, ",
            "Shiny, DT and ggplot2."
          ),
          
          h3("Project Objectives"),
          
          tags$ul(
            tags$li("Monitor supermarket sales"),
            tags$li("Calculate total sales and profit"),
            tags$li("Analyze product-wise performance"),
            tags$li("Monitor inventory and low stock"),
            tags$li("Add new sales records"),
            tags$li("Delete selected sales records"),
            tags$li("Download sales and inventory reports")
          ),
          
          h3("Technology Stack"),
          
          p(
            "R Programming, Shiny, shinydashboard, ",
            "DT, ggplot2 and dplyr."
          )
          
        )
        
      )
      
    )
    
  )
  
)

# ============================================================
# 3. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Reactive sales data frame
  
  sales <- reactiveVal(sales_data)
  
  # ==========================================================
  # FILTERED DASHBOARD DATA
  # ==========================================================
  
  selectedData <- reactive({
    
    data <- sales()
    
    if (!is.null(input$dateFilter)) {
      
      data <- data[
        data$Date >= input$dateFilter[1] &
          data$Date <= input$dateFilter[2],
        ,
        drop = FALSE
      ]
      
    }
    
    if (
      !is.null(input$dashCategory) &&
      input$dashCategory != "All"
    ) {
      
      data <- data[
        data$Category == input$dashCategory,
        ,
        drop = FALSE
      ]
      
    }
    
    data
    
  })
  
  # ==========================================================
  # FILTERED SALES RECORDS
  # ==========================================================
  
  filteredRecords <- reactive({
    
    data <- sales()
    
    if (
      !is.null(input$filterCategory) &&
      input$filterCategory != "All"
    ) {
      
      data <- data[
        data$Category == input$filterCategory,
        ,
        drop = FALSE
      ]
      
    }
    
    if (
      !is.null(input$searchProduct) &&
      trimws(input$searchProduct) != ""
    ) {
      
      data <- data[
        grepl(
          input$searchProduct,
          data$Product,
          ignore.case = TRUE
        ),
        ,
        drop = FALSE
      ]
      
    }
    
    data
    
  })
  
  # ==========================================================
  # DASHBOARD VALUE BOXES
  # ==========================================================
  
  output$totalSales <- renderValueBox({
    
    total <- sum(selectedData()$TotalSales)
    
    valueBox(
      paste0(
        "Rs. ",
        format(
          total,
          big.mark = ",",
          scientific = FALSE
        )
      ),
      "Total Sales",
      icon = icon("shopping-cart"),
      color = "blue"
    )
    
  })
  
  output$totalProfit <- renderValueBox({
    
    profit <- sum(selectedData()$Profit)
    
    valueBox(
      paste0(
        "Rs. ",
        format(
          profit,
          big.mark = ",",
          scientific = FALSE
        )
      ),
      "Total Profit",
      icon = icon("money"),
      color = "green"
    )
    
  })
  
  output$totalQuantity <- renderValueBox({
    
    quantity <- sum(selectedData()$Quantity)
    
    valueBox(
      quantity,
      "Items Sold",
      icon = icon("cube"),
      color = "orange"
    )
    
  })
  
  output$totalTransactions <- renderValueBox({
    
    transactions <- nrow(selectedData())
    
    valueBox(
      transactions,
      "Transactions",
      icon = icon("receipt"),
      color = "purple"
    )
    
  })
  
  # ==========================================================
  # SALES TREND PLOT
  # ==========================================================
  
  output$trendPlot <- renderPlot({
    
    data <- selectedData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    trend <- aggregate(
      TotalSales ~ Date,
      data = data,
      FUN = sum
    )
    
    ggplot(
      trend,
      aes(
        x = Date,
        y = TotalSales
      )
    ) +
      geom_line(
        linewidth = 1.2,
        color = "steelblue"
      ) +
      geom_point(
        size = 3,
        color = "darkblue"
      ) +
      theme_minimal() +
      labs(
        title = "Date-wise Sales Trend",
        x = "Date",
        y = "Sales (Rs.)"
      )
    
  })
  
  # ==========================================================
  # CATEGORY SALES PLOT
  # ==========================================================
  
  output$categoryPlot <- renderPlot({
    
    data <- selectedData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    categorySales <- aggregate(
      TotalSales ~ Category,
      data = data,
      FUN = sum
    )
    
    ggplot(
      categorySales,
      aes(
        x = reorder(Category, TotalSales),
        y = TotalSales
      )
    ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Category-wise Sales",
        x = "Category",
        y = "Sales (Rs.)"
      )
    
  })
  
  # ==========================================================
  # PAYMENT ANALYSIS PLOT
  # ==========================================================
  
  output$paymentPlot <- renderPlot({
    
    data <- selectedData()
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    paymentCount <- as.data.frame(
      table(data$Payment)
    )
    
    names(paymentCount) <- c(
      "Payment",
      "Count"
    )
    
    ggplot(
      paymentCount,
      aes(
        x = Payment,
        y = Count,
        fill = Payment
      )
    ) +
      geom_col() +
      theme_minimal() +
      labs(
        title = "Payment Method Analysis",
        x = "Payment Method",
        y = "Transactions"
      )
    
  })
  
  # ==========================================================
  # SUMMARY
  # ==========================================================
  
  output$summary <- renderPrint({
    
    data <- selectedData()
    
    if (nrow(data) == 0) {
      
      cat("No records available for selected filters.")
      return()
      
    }
    
    cat("========== SALES SUMMARY ==========\n")
    
    cat(
      "Transactions:",
      nrow(data),
      "\n"
    )
    
    cat(
      "Total Sales: Rs.",
      sum(data$TotalSales),
      "\n"
    )
    
    cat(
      "Total Profit: Rs.",
      sum(data$Profit),
      "\n"
    )
    
    cat(
      "Total Items Sold:",
      sum(data$Quantity),
      "\n"
    )
    
    cat(
      "Average Sale: Rs.",
      round(mean(data$TotalSales), 2),
      "\n"
    )
    
    cat(
      "Highest Sale: Rs.",
      max(data$TotalSales),
      "\n"
    )
    
    cat(
      "Lowest Sale: Rs.",
      min(data$TotalSales),
      "\n"
    )
    
  })
  
  # ==========================================================
  # SALES DATA TABLE
  # ==========================================================
  
  output$salesTable <- renderDT({
    
    datatable(
      filteredRecords(),
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
    
  })
  
  # ==========================================================
  # DELETE SELECTED SALE
  # ==========================================================
  
  observeEvent(input$deleteSale, {
    
    selectedRow <- input$salesTable_rows_selected
    
    if (length(selectedRow) != 1) {
      
      showNotification(
        "Please select one row to delete.",
        type = "error"
      )
      
      return()
      
    }
    
    data <- filteredRecords()
    
    invoiceID <- data$InvoiceID[selectedRow]
    
    updatedData <- sales()[
      sales()$InvoiceID != invoiceID,
      ,
      drop = FALSE
    ]
    
    sales(updatedData)
    
    showNotification(
      paste(invoiceID, "deleted successfully."),
      type = "warning"
    )
    
  })
  
  # ==========================================================
  # ADD NEW SALE
  # ==========================================================
  
  observeEvent(input$addSale, {
    
    product <- trimws(input$productName)
    
    if (product == "") {
      
      output$addMessage <- renderText({
        "Please enter a product name."
      })
      
      return()
      
    }
    
    if (
      is.null(input$productPrice) ||
      input$productPrice <= 0
    ) {
      
      output$addMessage <- renderText({
        "Price must be greater than zero."
      })
      
      return()
      
    }
    
    if (
      is.null(input$productQuantity) ||
      input$productQuantity < 1 ||
      input$productQuantity != floor(input$productQuantity)
    ) {
      
      output$addMessage <- renderText({
        "Quantity must be a positive whole number."
      })
      
      return()
      
    }
    
    currentData <- sales()
    
    nextNumber <- nrow(currentData) + 1
    
    newID <- paste0(
      "INV",
      sprintf("%03d", nextNumber)
    )
    
    newSale <- data.frame(
      
      InvoiceID = newID,
      Date = as.Date(input$saleDate),
      Product = product,
      Category = input$productCategory,
      Price = input$productPrice,
      Quantity = input$productQuantity,
      Payment = input$paymentMethod,
      Stock = input$stockValue,
      TotalSales =
        input$productPrice * input$productQuantity,
      Profit = round(
        input$productPrice *
          input$productQuantity *
          0.10,
        2
      ),
      
      stringsAsFactors = FALSE
      
    )
    
    sales(
      rbind(
        currentData,
        newSale
      )
    )
    
    output$addMessage <- renderText({
      
      paste(
        "Sale added successfully!",
        "Invoice ID:",
        newID
      )
      
    })
    
    showNotification(
      "New sale added successfully!",
      type = "message"
    )
    
  })
  
  # ==========================================================
  # INVENTORY TABLE
  # ==========================================================
  
  output$inventoryTable <- renderDT({
    
    data <- sales()
    
    lowStockData <- data[
      data$Stock <= 20,
      ,
      drop = FALSE
    ]
    
    datatable(
      lowStockData,
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX = TRUE
      )
    )
    
  })
  
  # ==========================================================
  # INVENTORY VALUE BOXES
  # ==========================================================
  
  output$lowStockCount <- renderValueBox({
    
    lowStock <- sum(sales()$Stock <= 20)
    
    valueBox(
      lowStock,
      "Low Stock Items",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
    
  })
  
  output$totalStock <- renderValueBox({
    
    totalStock <- sum(sales()$Stock)
    
    valueBox(
      totalStock,
      "Total Stock",
      icon = icon("cubes"),
      color = "blue"
    )
    
  })
  
  output$stockProducts <- renderValueBox({
    
    products <- length(
      unique(sales()$Product)
    )
    
    valueBox(
      products,
      "Products",
      icon = icon("shopping-basket"),
      color = "green"
    )
    
  })
  
  # ==========================================================
  # PRODUCT SALES ANALYSIS
  # ==========================================================
  
  output$productPlot <- renderPlot({
    
    data <- sales()
    
    productSales <- aggregate(
      TotalSales ~ Product,
      data = data,
      FUN = sum
    )
    
    ggplot(
      productSales,
      aes(
        x = reorder(Product, TotalSales),
        y = TotalSales
      )
    ) +
      geom_col(fill = "orange") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Product-wise Sales",
        x = "Product",
        y = "Sales (Rs.)"
      )
    
  })
  
  # ==========================================================
  # PROFIT BY CATEGORY
  # ==========================================================
  
  output$profitPlot <- renderPlot({
    
    data <- sales()
    
    categoryProfit <- aggregate(
      Profit ~ Category,
      data = data,
      FUN = sum
    )
    
    ggplot(
      categoryProfit,
      aes(
        x = Category,
        y = Profit,
        fill = Category
      )
    ) +
      geom_col() +
      theme_minimal() +
      labs(
        title = "Profit by Category",
        x = "Category",
        y = "Profit (Rs.)"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 30,
          hjust = 1
        )
      )
    
  })
  
  # ==========================================================
  # STATISTICS
  # ==========================================================
  
  output$statistics <- renderPrint({
    
    data <- sales()
    
    productSales <- aggregate(
      TotalSales ~ Product,
      data = data,
      FUN = sum
    )
    
    topProduct <- productSales$Product[
      which.max(productSales$TotalSales)
    ]
    
    categorySales <- aggregate(
      TotalSales ~ Category,
      data = data,
      FUN = sum
    )
    
    topCategory <- categorySales$Category[
      which.max(categorySales$TotalSales)
    ]
    
    cat("========== PRODUCT ANALYSIS ==========\n")
    
    cat(
      "Top Product:",
      topProduct,
      "\n"
    )
    
    cat(
      "Top Category:",
      topCategory,
      "\n"
    )
    
    cat(
      "Total Products:",
      length(unique(data$Product)),
      "\n"
    )
    
    cat(
      "Average Product Price: Rs.",
      round(mean(data$Price), 2),
      "\n"
    )
    
    cat(
      "Total Sales: Rs.",
      sum(data$TotalSales),
      "\n"
    )
    
    cat(
      "Total Profit: Rs.",
      sum(data$Profit),
      "\n"
    )
    
  })
  
  # ==========================================================
  # DOWNLOAD FILTERED CSV
  # ==========================================================
  
  output$downloadCSV <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "filtered_sales_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        filteredRecords(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
  # ==========================================================
  # DOWNLOAD SALES REPORT
  # ==========================================================
  
  output$downloadReport <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "sales_report_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        sales(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
  # ==========================================================
  # DOWNLOAD INVENTORY REPORT
  # ==========================================================
  
  output$downloadInventory <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "inventory_report_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      inventoryData <- sales()[
        ,
        c(
          "InvoiceID",
          "Product",
          "Category",
          "Stock"
        )
      ]
      
      write.csv(
        inventoryData,
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}

# ============================================================
# 4. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)