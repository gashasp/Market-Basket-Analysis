function(input, output, session){
  
#=====BACKGROUND=====  
#PlotBackground
  output$plotbackground <- renderPlotly({
    background_plot})
  
#=====EXPLORATORY DATA=====
#==Raw Data==
output$tableonlineretail <- renderDataTable({
  datatable(data = online_retail,rownames = F,
            options = list(scrollX = T,"pageLength" = 10,
                           lengthChange = F))})

#==Visualization Data==
#Value Box (Total Sales)
output$totalsales <- renderValueBox({
  value_total_sales  <-
    sum(online_retail$UnitPrice) %>%
    dollar()
  valueBox(
    value = value_total_sales,
    subtitle = "Total Sales",
    icon = icon("dollar-sign"),
    color = "orange")})

#Value Box (Total Customer)
output$totalcustomer <- renderValueBox({
  value_total_customer  <-
    online_retail$CustomerID %>%
    unique() %>%
    length()
  valueBox(
    value = value_total_customer,
    subtitle = "Total Customer",
    icon = icon("user"),
    color = "orange")})

#Value Box (Total Transaction)
output$totaltransaction <- renderValueBox({
  value_total_transaction  <-
    online_retail$InvoiceNo %>%
    unique() %>%
    length()
  valueBox(
    value = value_total_transaction,
    subtitle = "Total Transaction",
    icon = icon("shopping-cart"),
    color = "orange")})

#Value Box (Total Country)
output$totalcountry <- renderValueBox({
  value_total_country  <-
    online_retail$Country %>%
    unique() %>%
    length()
  valueBox(
    value = value_total_country,
    subtitle = "Total Country",
    icon = icon("flag"),
    color = "orange"
  )})

#Plot0
output$plotdata0 <- renderPlotly({
  data0_plot})

#Plot1
output$plotdata1 <- renderPlotly({
  data1_plot})

#Plot2
output$plotdata2 <- renderPlotly({
  data2_plot})

#Plot3
output$plotdata3 <- renderPlotly({
  data3_plot})

#Plot4
output$plotdata4 <- renderPlotly({
  data4_plot})

#Plot5
output$plotdata5 <- renderPlotly({
  data5_plot})

#=====MARKET BASKET ANALYSIS=====
#==Tabel==
output$tabelmba <- renderDataTable({
  datatable(data = rules_tabel,rownames = F,
            filter = "top",
            options = list(scrollX = T,"pageLength" = 10,
                           lengthChange = F, dom = "t", digits = 5))  %>% 
    formatRound(columns=c('Support', 'Confidence',"Coverage","Lift"), digits=5)})

#PlotMBA1
output$plotmba1 <- renderPlotly({
  scater_plot_apriori})

#PlotMBA2
output$plotmba2 <- renderVisNetwork({
  graph_plot_apriori})



#=====PRODUCT RECOMENDATION======
barang_filter <- reactive({
  #Jika user hanya memilih hanya 1 barang
if(input$selectinputproduct2 == "NOTHING" &
   input$selectinputproduct3 == "NOTHING" &
   input$selectinputproduct4 == "NOTHING" &
   input$selectinputproduct5 == "NOTHING" &
   input$selectinputproduct6 == "NOTHING" &
   input$selectinputproduct7 == "NOTHING")
{
  barang_filter <- 1
}
  #Jika user hanya memilih 2 barang
else if(input$selectinputproduct2 != "NOTHING" &
        input$selectinputproduct3 == "NOTHING" &
        input$selectinputproduct4 == "NOTHING" &
        input$selectinputproduct5 == "NOTHING" & 
        input$selectinputproduct6 == "NOTHING" &
        input$selectinputproduct7 == "NOTHING")
{
  barang_filter <- 2
}
  #Jika user hanya memilih 3 barang
else if(input$selectinputproduct2 != "NOTHING" &
        input$selectinputproduct3 != "NOTHING" &
        input$selectinputproduct4 == "NOTHING" &
        input$selectinputproduct5 == "NOTHING" &
        input$selectinputproduct6 == "NOTHING" &
        input$selectinputproduct7 == "NOTHING")
{
  barang_filter <- 3
}
  #Jika user hanya memilih 4 barang
else if(input$selectinputproduct2 != "NOTHING" &
        input$selectinputproduct3 != "NOTHING" &
        input$selectinputproduct4 != "NOTHING" &
        input$selectinputproduct5 == "NOTHING" &
        input$selectinputproduct6 == "NOTHING" &
        input$selectinputproduct7 == "NOTHING")
{
  barang_filter <- 4
}
  #Jika user hanya memilih 5 barang
else if(input$selectinputproduct2 != "NOTHING" &
        input$selectinputproduct3 != "NOTHING" &
        input$selectinputproduct4 != "NOTHING" &
        input$selectinputproduct5 != "NOTHING" &
        input$selectinputproduct6 == "NOTHING" &
        input$selectinputproduct7 == "NOTHING")
{
  barang_filter <- 5
}
  #Jika user hanya memilih 6 barang
else if(input$selectinputproduct2 != "NOTHING" &
        input$selectinputproduct3 != "NOTHING" & 
        input$selectinputproduct4 != "NOTHING" &
        input$selectinputproduct5 != "NOTHING" &
        input$selectinputproduct6 != "NOTHING" &
        input$selectinputproduct7 == "NOTHING")
{
  barang_filter <- 6
}
  #Jika user hanya memilih 7 barang
  else if(input$selectinputproduct2 != "NOTHING" &
          input$selectinputproduct3 != "NOTHING" & 
          input$selectinputproduct4 != "NOTHING" &
          input$selectinputproduct5 != "NOTHING" &
          input$selectinputproduct6 != "NOTHING" &
          input$selectinputproduct7 != "NOTHING")
{
  barang_filter <- 7
  }
})

###==FIRST VALUEBOX==###

output$vbox <- renderValueBox({
  if (barang_filter() == 1){
    value_vbox <-
      rules_tabel_df %>%
      filter (Product_Selection_1 == input$selectinputproduct1) %>%
      pull(Product_Recommendation) %>% 
      head(1) 
    valueBox(
          value = value_vbox,
          subtitle = "Product Recommendation 1",
          icon = icon("award"),
          color = "orange")
  }
  else
    if (barang_filter() == 2){
      value_vbox <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        pull(Product_Recommendation) %>% 
        head(1)
      valueBox(
        value = value_vbox,
        subtitle = "Product Recommendation 1",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 3){
    value_vbox <-
      rules_tabel_df %>%
      filter (Product_Selection_1 == input$selectinputproduct1) %>%
      filter (Product_Selection_2 == input$selectinputproduct2) %>% 
      filter (Product_Selection_3 == input$selectinputproduct3) %>%   
      pull(Product_Recommendation) %>% 
      head(1)
    valueBox(
      value = value_vbox,
      subtitle = "Product Recommendation 1",
      icon = icon("award"),
      color = "orange")
    }
  else
    if (barang_filter() == 4){
      value_vbox <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>% 
        pull(Product_Recommendation) %>% 
        head(1)
      valueBox(
        value = value_vbox,
        subtitle = "Product Recommendation 1",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 5){
      value_vbox <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>%   
        filter (Product_Selection_5 == input$selectinputproduct5) %>% 
        pull(Product_Recommendation) %>% 
        head(1)
      valueBox(
        value = value_vbox,
        subtitle = "Product Recommendation 1",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 6){
      value_vbox <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>%   
        filter (Product_Selection_5 == input$selectinputproduct5) %>% 
        filter (Product_Selection_6 == input$selectinputproduct6) %>% 
        pull(Product_Recommendation) %>% 
        head(1)
      valueBox(
        value = value_vbox,
        subtitle = "Product Recommendation 1",
        icon = icon("award"),
        color = "orange")
    }
  else
    {value_vbox <-
      rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>%   
        filter (Product_Selection_5 == input$selectinputproduct5) %>% 
        filter (Product_Selection_6 == input$selectinputproduct6) %>% 
        filter (Product_Selection_7 == input$selectinputproduct7) %>% 
        pull(Product_Recommendation) %>% 
        head(1)
      valueBox(
        value = value_vbox,
        subtitle = "Product Recommendation 1",
        icon = icon("award"),
        color = "orange")
    }
})

###==SECOND VALUEBOX==###

output$vbox2 <- renderValueBox({
  if (barang_filter() == 1){
    value_vbox2 <-
      rules_tabel_df %>%
      filter (Product_Selection_1 == input$selectinputproduct1) %>%
      pull(Product_Recommendation) %>%
      head(2) %>%
      tail(1)
    valueBox(
      value = value_vbox2,
      subtitle = "Product Recommendation 2",
      icon = icon("award"),
      color = "orange")
  }
  else
    if (barang_filter() == 2){
      value_vbox2 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        pull(Product_Recommendation) %>%
        head(2) %>%
        tail(1)
      valueBox(
        value = value_vbox2,
        subtitle = "Product Recommendation 2",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 3){
      value_vbox2 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>% 
        pull(Product_Recommendation) %>%
        head(2) %>%
        tail(1)
      valueBox(
        value = value_vbox2,
        subtitle = "Product Recommendation 2",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 4){
      value_vbox2 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>% 
        pull(Product_Recommendation) %>%
        head(2) %>%
        tail(1)
      valueBox(
        value = value_vbox2,
        subtitle = "Product Recommendation 2",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 5){
      value_vbox2 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>%   
        filter (Product_Selection_5 == input$selectinputproduct5) %>% 
        pull(Product_Recommendation) %>%
        head(2) %>%
        tail(1)
      valueBox(
        value = value_vbox2,
        subtitle = "Product Recommendation 2",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 6){
      value_vbox2 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>%   
        filter (Product_Selection_5 == input$selectinputproduct5) %>% 
        filter (Product_Selection_6 == input$selectinputproduct6) %>% 
        pull(Product_Recommendation) %>%
        head(2) %>%
        tail(1)
      valueBox(
        value = value_vbox2,
        subtitle = "Product Recommendation 2",
        icon = icon("award"),
        color = "orange")
    }
  else
  {value_vbox2 <-
    rules_tabel_df %>%
    filter (Product_Selection_1 == input$selectinputproduct1) %>%
    filter (Product_Selection_2 == input$selectinputproduct2) %>% 
    filter (Product_Selection_3 == input$selectinputproduct3) %>%  
    filter (Product_Selection_4 == input$selectinputproduct4) %>%   
    filter (Product_Selection_5 == input$selectinputproduct5) %>% 
    filter (Product_Selection_6 == input$selectinputproduct6) %>% 
    filter (Product_Selection_7 == input$selectinputproduct7) %>%
    pull(Product_Recommendation) %>%
    head(2) %>%
    tail(1)
  valueBox(
    value = value_vbox2,
    subtitle = "Product Recommendation 2",
    icon = icon("award"),
    color = "orange")
  }
})

###==THIRD VALUEBOX==###

output$vbox3 <- renderValueBox({
  if (barang_filter() == 1){
    value_vbox3 <-
      rules_tabel_df %>%
      filter (Product_Selection_1 == input$selectinputproduct1) %>%
      pull(Product_Recommendation) %>%
      head(3) %>%
      tail(1)
    valueBox(
      value = value_vbox3,
      subtitle = "Product Recommendation 3",
      icon = icon("award"),
      color = "orange")
  }
  else
    if (barang_filter() == 2){
      value_vbox3 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>%
        pull(Product_Recommendation) %>%
        head(3) %>%
        tail(1)
      valueBox(
        value = value_vbox3,
        subtitle = "Product Recommendation 3",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 3){
      value_vbox3 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%
        pull(Product_Recommendation) %>%
        head(3) %>%
        tail(1)
      valueBox(
        value = value_vbox3,
        subtitle = "Product Recommendation 3",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 4){
      value_vbox3 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>% 
        pull(Product_Recommendation) %>%
        head(3) %>%
        tail(1)
      valueBox(
        value = value_vbox3,
        subtitle = "Product Recommendation 3",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 5){
      value_vbox3 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>%   
        filter (Product_Selection_5 == input$selectinputproduct5) %>% 
        pull(Product_Recommendation) %>%
        head(3) %>%
        tail(1)
      valueBox(
        value = value_vbox3,
        subtitle = "Product Recommendation 3",
        icon = icon("award"),
        color = "orange")
    }
  else
    if (barang_filter() == 6){
      value_vbox3 <-
        rules_tabel_df %>%
        filter (Product_Selection_1 == input$selectinputproduct1) %>%
        filter (Product_Selection_2 == input$selectinputproduct2) %>% 
        filter (Product_Selection_3 == input$selectinputproduct3) %>%  
        filter (Product_Selection_4 == input$selectinputproduct4) %>%   
        filter (Product_Selection_5 == input$selectinputproduct5) %>% 
        filter (Product_Selection_6 == input$selectinputproduct6) %>%
        pull(Product_Recommendation) %>%
        head(3) %>%
        tail(1)
      valueBox(
        value = value_vbox3,
        subtitle = "Product Recommendation 3",
        icon = icon("award"),
        color = "orange")
    }
  else
  {value_vbox3 <-
    rules_tabel_df %>%
    filter (Product_Selection_1 == input$selectinputproduct1) %>%
    filter (Product_Selection_2 == input$selectinputproduct2) %>% 
    filter (Product_Selection_3 == input$selectinputproduct3) %>%  
    filter (Product_Selection_4 == input$selectinputproduct4) %>%   
    filter (Product_Selection_5 == input$selectinputproduct5) %>% 
    filter (Product_Selection_6 == input$selectinputproduct6) %>% 
    filter (Product_Selection_7 == input$selectinputproduct7) %>%
    pull(Product_Recommendation) %>%
    head(3) %>%
    tail(1)
  valueBox(
    value = value_vbox3,
    subtitle = "Product Recommendation 3",
    icon = icon("award"),
    color = "orange")
  }
})


# observeEvent(input$pilih,{
#   output$vbox <- DT::renderDataTable({
#     data3()})
# })




}