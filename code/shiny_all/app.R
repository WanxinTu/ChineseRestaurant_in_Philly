rm(list = ls())
#install.packages("rsconnect")

# library(rsconnect)
# 
# rsconnect::setAccountInfo(name='nicoletu',
#                           token='61C01E366F5C7501645E23A442453C7C',
#                           secret='<SECRET>')

# rsconnect::deployApp('~/Desktop/2023/628/project3/yelp_Fall2023/shiny_all')
library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("shiny.csv")
df_sentiment_block <- read.csv("df_sentiment_block.csv")
sentiment_rank_by_block <-read.csv("sentiment_rank_by_block.csv")
service_reviews<-read.csv("service_reviews.csv")
price_reviews<-read.csv("price_reviews.csv")

ui <- fluidPage(
  titlePanel("Find Your Bussiness Report (Chinese Restaurant in Philly)"),
  fluidRow(
    column(width = 2,
           selectInput("postalCode", "Postal Code:", choices = unique(data$postal_code)),
           selectInput("restaurantName", "Restaurant Name:", choices = NULL),
           actionButton("result", "Result")
    ),
    column(width = 10,
           tabsetPanel(
             tabPanel("Overview", 
                      uiOutput("avg_star_output"),
                      uiOutput("total_reviews_output"),
                      uiOutput("type_of_restaurant_output"),
                      plotOutput("star_trend_plot")),
             tabPanel("Neighbors and Competitive", 
                      
                      htmlOutput("compare_output"),
                      plotOutput("sentiment_hist_plot"),
                      htmlOutput('service_rank'),
                      plotOutput("sentiment_hist_plot2")),
             tabPanel("Suggestions", 
                      htmlOutput("suggest_output")
                      )
             ),style = "margin-bottom: 20px;"
           )))


server <- function(input, output, session) {
  observeEvent(input$postalCode, {
    updateSelectInput(session, "restaurantName",
                      choices = data %>%
                        filter(postal_code == input$postalCode) %>%
                        .$name)
  })
  
  resultData <- eventReactive(input$result, {
    selectedData <- data %>%
      filter(postal_code == input$postalCode, name == input$restaurantName)
    return(selectedData)
  })
  
  # TU_sentiment_hist
  sub_df_sentiment_by_block <- eventReactive(input$result, {
    sub_df <- df_sentiment_block %>%
      filter(postal_code_x == input$postalCode, name_x == input$restaurantName)
    return(sub_df)
  })
  #plot hist
  output$sentiment_hist_plot <- renderPlot({
    sub_df = sub_df_sentiment_by_block()
    if(nrow(sub_df) > 0){
      # Assuming df_sentiment_block is your data frame
      rest_block <- sub_df$block[1]
      df_rest_block <- df_sentiment_block[df_sentiment_block$block == rest_block, ]
      
      # Combine data into one data frame
      combined_data <- rbind(
        data.frame(sentiment = sub_df$sentiment, group = "Your Business"),
        data.frame(sentiment = df_rest_block$sentiment, group = paste0("Business in ", rest_block, " Block")),
        data.frame(sentiment = df_sentiment_block$sentiment, group = "Business in Philly")
      )
      
      # Create a ggplot with density plots and distinguishable colors
      ggplot(combined_data, aes(x = sentiment, colour = group)) +
        geom_line(stat = "density", size = 1.5) +
        labs(title = "Sentiment", x = "Values", y = "Density") +
        scale_colour_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
        theme_minimal() +
        theme(legend.position = "bottom",legend.text = element_text(size = 12),
              plot.title = element_text(size = 15, hjust = 0.5),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15))
      }
  }, width = 450, height = 250)
  
  
  #plot hist
  output$sentiment_hist_plot2 <- renderPlot({
    sub_df <- df_sentiment_block %>%
      filter(postal_code_x == input$postalCode, name_x == input$restaurantName)
    bus_id = sub_df$business_id[1]
    if(nrow(sub_df) > 0){
      # Assuming df_sentiment_block is your data frame
      sub_service_reviews=service_reviews[service_reviews$business_id == bus_id,]
      sub_price_reviews = price_reviews[price_reviews$business_id == bus_id,]
      
      if(nrow(sub_service_reviews) > 0){
        combined_data <- rbind(
          data.frame(sentiment = sub_service_reviews$sentiment, group = "Mentioned Service"),
          data.frame(sentiment = sub_df$sentiment, group = "All")
        )
      }
      if(nrow(sub_price_reviews) > 0){
        combined_data <- rbind(
          data.frame(combined_data),
          data.frame(sentiment = sub_price_reviews$sentiment, group = "Mentioned Price")
        )
      }
      # # Combine data into one data frame
      # combined_data <- rbind(
      #   data.frame(sentiment = sub_service_reviews$sentiment, group = "Mentioned Service"),
      #   data.frame(sentiment = sub_price_reviews$sentiment, group ="Mentioned Price"),
      #   data.frame(sentiment = sub_df$sentiment, group = "All")
      # )

      if(nrow(sub_price_reviews) > 0|nrow(sub_service_reviews) > 0){
        # Create a ggplot with density plots and distinguishable colors
         ggplot(combined_data, aes(x = sentiment, colour = group)) +
          geom_line(stat = "density", size = 1.5) +
          labs(title = "Reviews Sentiment of Your Business", x = "Values", y = "Density") +
          scale_colour_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
          theme_minimal() +
          theme(legend.position = "bottom",legend.text = element_text(size = 12),
                plot.title = element_text(size = 15, hjust = 0.5),
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15))
      }
      
    }
  }, width = 450, height = 250)
  
  
  output$avg_star_output <- renderUI({
    current_data <- resultData()
    if (nrow(current_data) > 0) {
      HTML(paste0("<h3 style='font-weight: bold;'>Ratings received on Yelp: ", current_data$avg_star[1], " Star</h3>"))
    } else {
      HTML("<h3 style='font-weight: bold;'>No data available</h3>")
    }
  })
  
  output$total_reviews_output <- renderUI({
    current_data <- resultData()
    if (nrow(current_data) > 0) {
      HTML(paste0("<h3 style='font-weight: bold;'>Total number of comments: ", current_data$total_reviews[1], "</h3>"))
    } else {
      HTML("<h3 style='font-weight: bold;'>No data available</h3>")
    }
  })
  
  output$type_of_restaurant_output <- renderUI({
    current_data <- resultData()
    if (nrow(current_data) > 0) {
      if (current_data$categories[1] == 'Other') {
        HTML("<h3 style='font-weight: bold;'>Not categorized at this time</h3>")
      } else {
        HTML(paste0("<h3 style='font-weight: bold;'>Type of restaurant: ", current_data$categories[1], "</h3>"))
      }
    } else {
      HTML("<h3 style='font-weight: bold;'>No data available</h3>")
    }
  })
  
  output$star_trend_plot <- renderPlot({
    current_data <- resultData()
    if (nrow(current_data) > 0) {
      overall_avg_star <- mean(current_data$avg_star, na.rm = TRUE)
      ggplot(current_data, aes(x = month, y = mean_star)) +
        geom_point(color = "blue") + 
        geom_hline(yintercept = overall_avg_star, linetype = "dashed", color = "red") + 
        theme_minimal() +
        labs(x = "Month", y = "Mean Star Rating", title = "Monthly Mean of Star Rating (2019-2021)") +
        theme(plot.title = element_text(size = 25, hjust = 0.5),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20))
    }
  }, width = 1000, height = 300)
  
  

  output$compare_output <- renderText({
    req(input$result)
    selected_restaurant <- data[data$name == input$restaurantName, ]
    
    if(nrow(selected_restaurant) > 0){
      selected_restaurant <- selected_restaurant[1, ]
      postal_code <- selected_restaurant$postal_code
      block <- selected_restaurant$block
      income_block <- selected_restaurant$income_block
      asian_pro_block <- selected_restaurant$asian_pro_block
      income_rank <- selected_restaurant$income_rank
      asian_rank <- selected_restaurant$asian_rank
      star_rank <- selected_restaurant$star_rank
      review <- selected_restaurant$review
      comment_rank <- selected_restaurant$comment_rank
      GoodForGroups <- ifelse(selected_restaurant$GoodForGroups, "Yes", "No")
      T_GoodForGroups <- selected_restaurant$T_GoodForGroups
      OutdoorSeating <- ifelse(selected_restaurant$OutdoorSeating, "Yes", "No")
      T_OutdoorSeating <- selected_restaurant$T_OutdoorSeating
      AcceptsCreditCards <- ifelse(selected_restaurant$AcceptsCreditCards, "Yes", "No")
      T_AcceptsCreditCards <- selected_restaurant$T_AcceptsCreditCards
      TakeOut <- ifelse(selected_restaurant$TakeOut, "Yes", "No")
      T_TakeOut <- selected_restaurant$T_TakeOut
      Reservations <- ifelse(selected_restaurant$Reservations, "Yes", "No")
      T_Reservations <- selected_restaurant$T_Reservations
      Delivery <- ifelse(selected_restaurant$Delivery, "Yes", "No")
      T_Delivery <- selected_restaurant$T_Delivery
      PriceRange <- switch(as.character(selected_restaurant$PriceRange),
                           "1" = "under $10",
                           "2" = "$11-$30",
                           "3" = "$31-$60",
                           "4" = "over $60",
                           "inconclusive")
      PriceRanges <- c(PriceRange_1.0 = selected_restaurant$PriceRange_1.0,
                       PriceRange_2.0 = selected_restaurant$PriceRange_2.0,
                       PriceRange_3.0 = selected_restaurant$PriceRange_3.0,
                       PriceRange_4.0 = selected_restaurant$PriceRange_4.0)
      max_index <- which.max(PriceRanges)
      max_value <- PriceRanges[max_index]
      max_name <- names(PriceRanges)[max_index]
      if (max_name == "PriceRange_1.0") {
        max_name <- "under $10"
      } else if (max_name == "PriceRange_2.0") {
        max_name <- "$11-$30"
      } else if (max_name == "PriceRange_3.0") {
        max_name <- "$31-$60"
      } else if (max_name == "PriceRange_4.0") {
        max_name <- "over $60"
      }
      
      HTML(paste("<div style='font-size: 16px; line-height: 1.5;'>",
                 "The <b>zip code</b> of your restaurant is <b>", postal_code, "</b>, which belongs to the <span style='color: blue;'>", block, "</span> area of <b>Philadelphia</b>.<br/>",
                 "The <b>average annual income</b> in this area is <span style='color: blue;'>$", income_block, "</span>, and <b>ranked</b> <span style='color: blue;'>No.", income_rank, "</span>.<br/>",
                 "This region has a<span style='color: blue;'> ", asian_pro_block, " % </span><b>Asian population</b>, and <b>ranked</b> <span style='color: blue;'>No.", asian_rank, "</span>.<br/>",
                 "The <b>rating</b> your restaurant receives is the <span style='color: blue;'>", star_rank, "</span> in the ", block, "</span> area.<br/>",
                 "<b>From 2019 to 2021</b>, the restaurant received a total of <span style='color: blue;'>", review, "</span> <b>reviews</b> and <b>ranked</b> <span style='color: blue;'>No.", comment_rank, "</span> in this area.<br/>",
                 "Is your restaurant <b>group-friendly</b>? <span style='color: blue;'>", GoodForGroups, "</span>. <b>", T_GoodForGroups, "%</b> of restaurants in this area group-friendly.<br/>",
                 "Does your restaurant have <b>outdoor seating</b>? <span style='color: blue;'>", OutdoorSeating, "</span>. <b>", T_OutdoorSeating, "%</b> of restaurants in this area have outdoor seating.<br/>",
                 "Does your restaurant <b>accept credit card payments</b>? <span style='color: blue;'>", AcceptsCreditCards, "</span>. <b>", T_AcceptsCreditCards, "%</b> of restaurants accept credit card payments.<br/>",
                 "Is your restaurant open for <b>takeout</b>? <span style='color: blue;'>", TakeOut, "</span>. <b>", T_TakeOut, "%</b> of restaurants in this area open takeout.<br/>",
                 "Can <b>reservations</b> be made at your restaurant? <span style='color: blue;'>", Reservations, "</span>. <b>", T_Reservations, "%</b> of restaurants in this area share this attribute.<br/>",
                 "Does your restaurant <b>deliver food</b>? <span style='color: blue;'>", Delivery, "</span>. <b>", T_Delivery, "%</b> of restaurants in this area provide delivery.<br/>",
                 "Approximate cost of a meal per person: <span style='color: blue;'>", PriceRange, "</span>. ",
                 "For the restaurants in this area, the <b>largest proportion is ", max_name, "</b> (the percentage is <b>", max_value, "</b> %).<br/>",
                 sep = ""))
    } else {
      "Please select a restaurant."}
  })
  
  output$suggest_output <- renderText({
    req(input$result)
    selected_restaurant <- data[data$name == input$restaurantName, ]
    
    #TU
    sub_df <- df_sentiment_block %>%
      filter(postal_code_x == input$postalCode, name_x == input$restaurantName)
    bus_id = sub_df$business_id[1]
    sub_rank_df <- sentiment_rank_by_block[sentiment_rank_by_block$business_id == bus_id, ]
    suggest_price='<br/>'
    if(!is.na(sub_rank_df$Rank_sentiment_price_reviews)){
      
      suggest_price=paste0(suggest_price,"<h4 style='font-weight: bold;'>Based on the review analysis: <br/></h4>")
      if(sub_rank_df$Rank_sentiment_price_reviews/sub_rank_df$price_count_by_block<0.25){
        suggest_price=paste0(suggest_price,'Customers are satisfied with the pricing at your restaurant. <br/>The price is competitive among you area. Well done!')
      }
      else if(sub_rank_df$Rank_sentiment_price_reviews/sub_rank_df$price_count_by_block<0.5){
        suggest_price=paste0(suggest_price,'There is still room for improvement in your pricing.','<br/> Consider 
        (1)introducing affordable options. 
        (2)tenhancing perceived value. 
        (3)analyzing competitor pricing. 
        (4)launching loyalty programs')
      }
      else{suggest_price=paste0(suggest_price,'Customers are dissatisfied with your pricing. <br/> You could consider 
(1) Revise the menu with budget-friendly options.
(2) Provide special promotions or discounts.
(3) Actively seek customer feedback on pricing concerns.
For further assistance, please contact us to conduct a pricing analysis.')}
    }
    
    suggest_service='<br/>'
    if(!is.na(sub_rank_df$Rank_sentiment_service_reviews)){
      suggest_service=paste0(suggest_service,"<h4 style='font-weight: bold;'>Based on the review analysis: <br/></h4>")
      if(sub_rank_df$Rank_sentiment_service_reviews/sub_rank_df$service_count_by_block<0.25){
        suggest_service=paste0(suggest_service,'Customers are satisfied with the service at your restaurant, the service is competitive among you area. Well done!')
      }
      else if(sub_rank_df$Rank_sentiment_service_reviews/sub_rank_df$service_count_by_block<0.5){
        suggest_service=paste0(suggest_service,'There is still room for improvement in your service.','Consider 
(1) Enhancing Staff Training, (2) Implementing Customer Feedback Systems, (3) Reward staff for delivering exceptional service')
      }
      else{suggest_service=paste0(suggest_service,'Customers are dissatisfied with your service. You could consider 
(1) Enhancing Staff Training, (2) Implementing Customer Feedback Systems, (3) Reward staff for delivering exceptional service')}
    }
    

    if(nrow(selected_restaurant) > 0){
      selected_restaurant <- selected_restaurant[1, ]
      avg_star <- selected_restaurant$avg_star
      review <- selected_restaurant$review
      GoodForGroups <- ifelse(selected_restaurant$GoodForGroups, "Yes", "No")
      AcceptsCreditCards <- ifelse(selected_restaurant$AcceptsCreditCards, "Yes", "No")
      TakeOut <- ifelse(selected_restaurant$TakeOut, "Yes", "No")
      Reservations <- ifelse(selected_restaurant$Reservations, "Yes", "No")
      Delivery <- ifelse(selected_restaurant$Delivery, "Yes", "No")
      PriceRange <- selected_restaurant$PriceRange
      
      suggestions <- ""
      
      if (avg_star >= 4 && review >= 30) {
        message <- "<h4 style='font-weight: bold;'>Congratulations!<br/>Your restaurant is a great success so far, keep up the good work.<br/>Consider collecting customer feedback at the end of their meal to maintain a solid business model and identify any potential gaps in time.</h4>"
      } else if (avg_star >= 4 && review < 30) {
        message <- "<h4 style='font-weight: bold;'>Your restaurant is receiving positive reviews, but it seems to lack sufficient traffic.<br/>You might want to increase exposure and customer inflow through advertising and promotions.</h4>"
      } else if (avg_star >= 3 && avg_star < 4 && review >= 30) {
        message <- "<h4 style='font-weight: bold;'>Your restaurant has decent traffic and reviews, but there seems to be room for improvement in your business model.<br/>Consider adjusting your strategies to further enhance customer satisfaction.</h4>"
      } else if (avg_star >= 3 && avg_star < 4 && review < 30) {
        message <- "<h4 style='font-weight: bold;'>Your restaurant is receiving moderate ratings, but the traffic is not as high as it could be.<br/>Boosting marketing efforts and customer engagement might help increase your visibility and customer flow.</h4>"
      } else {
        message <- "<h4 style='font-weight: bold;'>It appears that your restaurant is currently facing some challenges.<br/>Please contact us as soon as possible and we will provide a more in-depth analysis and comprehensive recommendations to help you improve your business.</h4>"
      }
      
      if (avg_star < 4) {
        if (GoodForGroups == "No") {
          suggestions <- paste(suggestions, "<br/>A dedicated group dining area provides large tables or tables that can be joined together to accommodate large groups. Offer set menus designed for groups, both for quick service and for different tastes.")
        }
        if (AcceptsCreditCards == "No") {
          suggestions <- paste(suggestions, "<br/>Provide POS machine.")
        }
        if (TakeOut == "No") {
          suggestions <- paste(suggestions, "<br/>Open take-out service.")
        }
        if (Reservations == "No") {
          suggestions <- paste(suggestions, "<br/>Opening of reservation services (e.g., telephone reservation and online reservation).")
        }
        if (Delivery == "No") {
          suggestions <- paste(suggestions, "<br/>Opened a delivery service.")
        }
        if (!is.na(PriceRange) && (PriceRange == 3 || PriceRange == 4)) {
          suggestions <- paste(suggestions, "<br/>Consider lowering the pricing of your dishes or offering a discount.")
        }
        if (suggestions != "") {
          message <- paste(message, "<br/><b>Some Suggestions:</b><br/>", "<b>", suggestions, "</b>")
        } else {
          message <- paste(message, "<br/><h4 style='font-weight: bold;'>Please provide us with more information to get business advice.</h4>")
        }
      }
      HTML(paste(message,suggest_price,suggest_service, sep = ""))
    } else {
      "Please select a restaurant."}
  })
  
  
  # TU_rank_sentiment
  output$service_rank <- renderText({
    req(input$result)
    sub_df <- df_sentiment_block %>%
      filter(postal_code_x == input$postalCode, name_x == input$restaurantName)
    bus_id = sub_df$business_id[1]
    sub_rank_df <- sentiment_rank_by_block[sentiment_rank_by_block$business_id == bus_id, ]
    # print(sub_rank_df)
    if (!is.na(sub_rank_df$business_id)) {
      HTML(paste("<div style='font-size: 16px; line-height: 1.5;'>",
                 "Sentiment review mean <b>ranked</b>: <span style='color: blue;'>", ifelse(is.na(sub_rank_df$Rank_sentiment_all_reviews), "Not available", sub_rank_df$Rank_sentiment_all_reviews), "</span> out of ", ifelse(is.na(sub_rank_df$all_count_by_block), "Not available", sub_rank_df$all_count_by_block), " in ", ifelse(is.na(sub_rank_df$block), "Not available", sub_rank_df$block), ".<br>",
                 "Service satisfaction <b>ranked</b>: <span style='color: blue;'>", ifelse(is.na(sub_rank_df$Rank_sentiment_service_reviews), "Not available", sub_rank_df$Rank_sentiment_service_reviews), "</span> out of ", ifelse(is.na(sub_rank_df$service_count_by_block), "Not available", sub_rank_df$service_count_by_block), " in ", ifelse(is.na(sub_rank_df$block), "Not available", sub_rank_df$block), ".<br>",
                 "Price satisfaction <b>ranked</b>: <span style='color: blue;'>", ifelse(is.na(sub_rank_df$Rank_sentiment_price_reviews), "Not available", sub_rank_df$Rank_sentiment_price_reviews), "</span> out of ", ifelse(is.na(sub_rank_df$price_count_by_block), "Not available", sub_rank_df$price_count_by_block), " in ", ifelse(is.na(sub_rank_df$block), "Not available", sub_rank_df$block), ".<br>",
                 "</div>"
      ))
    } else {
      "Your business doesn't have customers reviews in 2019-2021"
    }
  }) 
  
}

shinyApp(ui, server)
