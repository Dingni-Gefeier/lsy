library(shiny)
library(bslib)
library(ggplot2)
library(mlr)
library(HDclassif)
library(MASS)
library(tidyverse)

# Define UI ----
ui <- fluidPage(
  theme = bs_theme(preset = "simplex"),
  titlePanel("DFU-Clin"),
  layout_columns(
    card(
      h4("Feature:"),
      numericInput("age", "Age",value = NA),
      radioButtons(
        "sex",
        "Sex",
        choices = list("Male" = 1, "Female" = 0),
        selected = 1,
        inline=TRUE
      ),
      numericInput("w", "Weight(kg)",value = NA),
      numericInput("h", "Height(m)",value = NA),
      radioButtons(
        "race",
        "Mixed Ethnicity",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "smoking",
        "Current Smoker",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      numericInput("HbA1c", "Glycated Haemoglobin (HbA1c) (mmol/mol)",value = NA),
      numericInput("crp", "C-reactive Protein (mg/L)",value = NA)),
    card(
      numericInput("vd", "Vitamin D (nmol/L)",value = NA),
      radioButtons(
        "diabete",
        "Type I Diabetes Mellitus ",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "DM",
        "Long Term Diabetes Mellitus (> 10 years)",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "Cardiovascula",
        "Cardiovascular Disease",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "Chronic",
        "Chronic Kidney Disease (Stage < 5)",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "Renal",
        "End Stage Renal Disease",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "retinopathy",
        "Retinopathy",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "aterial_disease",
        "Peripheral Arterial Disease",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "neuropathy",
        "Peripheral Neuropathy",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      radioButtons(
        "depression",
        "Depression",
        choices = list("Yes" = 1, "No" = 0),
        selected = 1,
        inline=TRUE
      ),
      actionButton("predict", "Predict")
      
    ),
    card(
      h4("Outcome:"),
      card(
        max_height="400px",
        plotOutput("plot")),
      card(
        textOutput('ans_5'),
        textOutput('ans_10'),
        textOutput('ans_all')),
      
    )),
  card(
    textOutput('detail1'),
    textOutput('detail2'),
    textOutput("detail3")
  )
)

# Define server logic ----
server <- function(input, output,session) {
  judge = -1
  pred1 = NA
  pred2 = NA
  pred3 = NA
  output$detail1 <- renderText("The DFU-Clin tool was established based on UK Biobank study cohort by linear discriminant analysis. ")
  output$detail2 <- renderText("The tool was developed on research purpose and cannot be used as clinical evidence.")
  output$detail3 <- renderText("Private questions: dfupred@gmail.com")
  output$ans_5 <- renderText({
    if(input$predict != judge){
      bmi <- input$w / input$h / input$h
      age <- (input$age - 59.11827397070055) / 7.359274871392511
      bmi <- (bmi - 31.197306993849125) / 5.776894564654322
      HbA1c <- (input$HbA1c - 52.23636691529952) / 14.574635182949262
      crp <- (input$crp - 3.499418093268373) / 4.9438303319512
      vd <- (input$vd - 43.175448817807265) / 19.497003368332273
      loaded_model <- readRDS("m_5.rds")
      new_data <- data.frame(
        "Age" = age,"Sex" = as.numeric(input$sex),"BMI" = bmi,"HbA_1c" = HbA1c,"C-reactive_Protein" = crp,"Vitamin_D" = vd,"Diabetes_Mellitus" = as.numeric(input$diabete),"Cardiovascular_Disease" = as.numeric(input$Cardiovascula),"Chronic_Kidney_Disease" = as.numeric(input$Chronic),"Retinopathy" = as.numeric(input$retinopathy),"Peripheral_Aterial_Disease" = as.numeric(input$aterial_disease),"Peripheral_Neuropathy" = as.numeric(input$neuropathy),"End_stage_Renal_Disease" = as.numeric(input$Renal),"Depression" = as.numeric(input$depression),"Long_Term_DM" = as.numeric(input$DM),"Race" = as.numeric(input$race),"Smoking" = as.numeric(input$smoking))
      predictions <- predict(loaded_model,newdata=new_data)
      pred1 <<- 100 * predictions$posterior[,2]
      sprintf("Based on feature values, predicted possibility of DFU in 5 years : %1.1f%%",100 * predictions$posterior[,2])
    }else{
      sprintf("Based on feature values, predicted possibility of DFU in 5 years : %1.1f%%",pred1)
      }})
  output$ans_10 <- renderText({
    if(input$predict != judge){
      bmi <- input$w / input$h / input$h
      age <- (input$age - 59.131420095891826) / 7.355678649649147
      bmi <- (bmi - 31.214925424357702) / 5.779466246926634
      HbA1c <- (input$HbA1c - 52.28773361947218) / 14.604433781434464
      crp <- (input$crp - 3.492870704971261) / 4.895841907754212
      vd <- (input$vd - 43.16181991876967) / 19.510933466682822
      loaded_model <- readRDS("m_10.rds")
      new_data <- data.frame(
        "Age" = age,"Sex" = as.numeric(input$sex),"BMI" = bmi,"HbA_1c" = HbA1c,"C-reactive_Protein" = crp,"Vitamin_D" = vd,"Diabetes_Mellitus" = as.numeric(input$diabete),"Cardiovascular_Disease" = as.numeric(input$Cardiovascula),"Chronic_Kidney_Disease" = as.numeric(input$Chronic),"Retinopathy" = as.numeric(input$retinopathy),"Peripheral_Aterial_Disease" = as.numeric(input$aterial_disease),"Peripheral_Neuropathy" = as.numeric(input$neuropathy),"End_stage_Renal_Disease" = as.numeric(input$Renal),"Depression" = as.numeric(input$depression),"Long_Term_DM" = as.numeric(input$DM),"Race" = as.numeric(input$race),"Smoking" = as.numeric(input$smoking))
      predictions <- predict(loaded_model,newdata=new_data)
      pred2 <<- 100 * predictions$posterior[,2]
      sprintf("Based on feature values, predicted possibility of DFU in 10 years : %1.1f%%",100 * predictions$posterior[,2])
    }else{
      sprintf("Based on feature values, predicted possibility of DFU in 10 years : %1.1f%%",pred2)
    }})
  output$ans_all <- renderText({
    if(input$predict != judge){
      judge <<- judge + 1
      bmi <- input$w / input$h / input$h
      age <- (input$age - 59.17059732580797) / 7.34923200104327
      bmi <- (bmi - 31.272657924212805) / 5.81856998417168
      HbA1c <- (input$HbA1c - 52.48691561519947) / 14.737859978346254
      crp <- (input$crp - 3.5334026266144254) / 4.9821802201722845
      vd <- (input$vd - 43.073769882562814) / 19.4731225127317
      loaded_model <- readRDS("m_all.rds")
      new_data <- data.frame(
        "Age" = age,"Sex" = as.numeric(input$sex),"BMI" = bmi,"HbA_1c" = HbA1c,"C-reactive_Protein" = crp,"Vitamin_D" = vd,"Diabetes_Mellitus" = as.numeric(input$diabete),"Cardiovascular_Disease" = as.numeric(input$Cardiovascula),"Chronic_Kidney_Disease" = as.numeric(input$Chronic),"Retinopathy" = as.numeric(input$retinopathy),"Peripheral_Aterial_Disease" = as.numeric(input$aterial_disease),"Peripheral_Neuropathy" = as.numeric(input$neuropathy),"End_stage_Renal_Disease" = as.numeric(input$Renal),"Depression" = as.numeric(input$depression),"Long_Term_DM" = as.numeric(input$DM),"Race" = as.numeric(input$race),"Smoking" = as.numeric(input$smoking))
      predictions <- predict(loaded_model,newdata=new_data)
      pred3 <<- 100 * predictions$posterior[,2]
      sprintf("Based on feature values, predicted possibility of DFU in all time : %1.1f%%",100 * predictions$posterior[,2])
    }else{
      sprintf("Based on feature values, predicted possibility of DFU in all time : %1.1f%%",pred3)
    }})
  output$plot <- renderPlot({
    if(input$predict != judge){
      bmi <- input$w / input$h / input$h
      age <- (input$age - 59.11827397070055) / 7.359274871392511
      bmi <- (bmi - 31.197306993849125) / 5.776894564654322
      HbA1c <- (input$HbA1c - 52.23636691529952) / 14.574635182949262
      crp <- (input$crp - 3.499418093268373) / 4.9438303319512
      vd <- (input$vd - 43.175448817807265) / 19.497003368332273
      loaded_model <- readRDS("m_5.rds")
      new_data <- data.frame(
        "Age" = age,"Sex" = as.numeric(input$sex),"BMI" = bmi,"HbA_1c" = HbA1c,"C-reactive_Protein" = crp,"Vitamin_D" = vd,"Diabetes_Mellitus" = as.numeric(input$diabete),"Cardiovascular_Disease" = as.numeric(input$Cardiovascula),"Chronic_Kidney_Disease" = as.numeric(input$Chronic),"Retinopathy" = as.numeric(input$retinopathy),"Peripheral_Aterial_Disease" = as.numeric(input$aterial_disease),"Peripheral_Neuropathy" = as.numeric(input$neuropathy),"End_stage_Renal_Disease" = as.numeric(input$Renal),"Depression" = as.numeric(input$depression),"Long_Term_DM" = as.numeric(input$DM),"Race" = as.numeric(input$race),"Smoking" = as.numeric(input$smoking))
      pred1 <<- predict(loaded_model,newdata=new_data)$posterior[,2] * 100
      
      bmi <- input$w / input$h / input$h
      age <- (input$age - 59.131420095891826) / 7.355678649649147
      bmi <- (bmi - 31.214925424357702) / 5.779466246926634
      HbA1c <- (input$HbA1c - 52.28773361947218) / 14.604433781434464
      crp <- (input$crp - 3.492870704971261) / 4.895841907754212
      vd <- (input$vd - 43.16181991876967) / 19.510933466682822
      loaded_model <- readRDS("m_10.rds")
      new_data <- data.frame(
        "Age" = age,"Sex" = as.numeric(input$sex),"BMI" = bmi,"HbA_1c" = HbA1c,"C-reactive_Protein" = crp,"Vitamin_D" = vd,"Diabetes_Mellitus" = as.numeric(input$diabete),"Cardiovascular_Disease" = as.numeric(input$Cardiovascula),"Chronic_Kidney_Disease" = as.numeric(input$Chronic),"Retinopathy" = as.numeric(input$retinopathy),"Peripheral_Aterial_Disease" = as.numeric(input$aterial_disease),"Peripheral_Neuropathy" = as.numeric(input$neuropathy),"End_stage_Renal_Disease" = as.numeric(input$Renal),"Depression" = as.numeric(input$depression),"Long_Term_DM" = as.numeric(input$DM),"Race" = as.numeric(input$race),"Smoking" = as.numeric(input$smoking))
      pred2 <<- predict(loaded_model,newdata=new_data)$posterior[,2] * 100
      
      bmi <- input$w / input$h / input$h
      age <- (input$age - 59.17059732580797) / 7.34923200104327
      bmi <- (bmi - 31.272657924212805) / 5.81856998417168
      HbA1c <- (input$HbA1c - 52.48691561519947) / 14.737859978346254
      crp <- (input$crp - 3.5334026266144254) / 4.9821802201722845
      vd <- (input$vd - 43.073769882562814) / 19.4731225127317
      loaded_model <- readRDS("m_all.rds")
      new_data <- data.frame(
        "Age" = age,"Sex" = as.numeric(input$sex),"BMI" = bmi,"HbA_1c" = HbA1c,"C-reactive_Protein" = crp,"Vitamin_D" = vd,"Diabetes_Mellitus" = as.numeric(input$diabete),"Cardiovascular_Disease" = as.numeric(input$Cardiovascula),"Chronic_Kidney_Disease" = as.numeric(input$Chronic),"Retinopathy" = as.numeric(input$retinopathy),"Peripheral_Aterial_Disease" = as.numeric(input$aterial_disease),"Peripheral_Neuropathy" = as.numeric(input$neuropathy),"End_stage_Renal_Disease" = as.numeric(input$Renal),"Depression" = as.numeric(input$depression),"Long_Term_DM" = as.numeric(input$DM),"Race" = as.numeric(input$race),"Smoking" = as.numeric(input$smoking))
      pred3 <<- predict(loaded_model,newdata=new_data)$posterior[,2] * 100

      
      # 创建数据框
      df <- data.frame(x = c("5-year", "10-year", "All"), y = c(pred1, pred2, pred3), group = 1)
      
      # 将 x 列转换为因子，并指定水平顺序
      df$x <- factor(df$x, levels = c("5-year", "10-year", "All"))
      
      l = list()
      for (i in seq(from=0,to=3,by=1)) {
        l[i] = round(df$y[i],1)
      }
      
      # 绘制折线图并设置线条颜色为红色
      ggplot(df, aes(x = x, y = y, group = group)) +
        geom_bar(stat="identity",width = .5,lwd = 1,fill =  "#C73824") +
        geom_text(aes(label=l, vjust=-0.5))+
        labs(x = "Incidence Times", y = "Risk(%)") +
        scale_y_continuous(expand = c(0,0),limits = c(0, 100)) +
        theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
              panel.background = element_rect(fill='transparent'),
              panel.grid.major = element_line(color="grey90"), 
              axis.text.x = element_text(size=10), 
              axis.text.y = element_text(size=10))  # 添加细边框
    }
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)