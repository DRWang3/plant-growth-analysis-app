# server.R

library(shiny)
source("source_HTP.R")

### read in CEPF dataset
df <- read.csv("data/CEPF_RGB_compiled_for_app.csv")

Subpop <- levels(as.factor(df$Subpop))
Species <- levels(as.factor(df$Species))
growth_var <- colnames(df)[10:length(colnames(df))]

# Source the code

function(input, output){
  
  ##### tab 1 (Introduction ) ######
  output$tab1_download = downloadHandler(
    filename = "CEPF_dataset_for_R_Shiny.csv",
    content = function(file){
      write.csv(df, file, row.names = F, quote = F)
    }
  )

  ##### tab 2 (Relative growth rate) ######
  
  output$tab2_plot = renderPlot({
    df.tab2 = df[which(df$Species == input$tab2_species),]
    idx = which(names(df.tab2)== input$tab2_variable)
    plot(df.tab2$PlantAge, df.tab2[,idx], main = paste(input$tab2_species, input$tab2_variable, sep = " "), cex = 2,
         ylab = input$tab2_variable, xlab = "PlantAge (days)", pch = 16, col=rgb(red=1, green=0.6, blue=0.2, alpha=0.6))
  })
  
  output$tab2_text = renderText({
    paste("Definition:", defDict$description[which(defDict$var.name == input$tab2_variable)], sep=" ")
  })
  
  ##### tab 3 (Interval approach) ######
  df.tab3 = df[which(df$Species == "Papaya"),]
  
  tab3_idx = reactive({
    which(names(df.tab3)== input$tab3_variable)
  })
  
  tab3_data = reactive({
    t1 = input$tab3_TP1
    t2 = input$tab3_TP2
    
    w1 = mean(df.tab3[which(df.tab3$PlantAge == t1), tab3_idx()], na.rm=T)
    w2 = mean(df.tab3[which(df.tab3$PlantAge == t2), tab3_idx()], na.rm=T)
    
    c(t1, t2, w1, w2)
  })
  
  output$tab3_plot = renderPlot({
    w1 = tab3_data()[3]
    w2 = tab3_data()[4]
    
    plot(df.tab3$PlantAge, df.tab3[,tab3_idx()], main = paste("Papaya", input$tab3_variable, sep = " "), cex =2,
         ylab = input$tab3_variable, xlab = "PlantAge (days)", pch = 16, col=rgb(red=1, green=0.6, blue=0.2, alpha=0.6))
    abline(v= c(input$tab3_TP1, input$tab3_TP2 ), lty = 3, lwd = 2, col=rgb(red=0, green=0.4, blue=0.8, alpha=0.8))
    
    tab3_expression = bquote(paste('W'[1]*' = ', .(w1), '; W'[2]*' = ', .(w2) ) )
      
    if(input$tab3_variable == "TopPlantSurface"){
      text(28, 300000, tab3_expression, pos = 4, col = "blue", cex = 1.25)
    } else {
      text(28, 1100, tab3_expression, pos = 4, col = "blue", cex = 1.25 )
    }
  })
  
  output$tab3_RGR = renderText({
    t1 = tab3_data()[1]
    t2 = tab3_data()[2]
    
    ## check if the timepoints selected are within the dataset
    if(length(which(df.tab3$PlantAge == input$tab3_TP1)) != 0 ){
      if(length(which(df.tab3$PlantAge == input$tab3_TP2)) != 0 ){
        ## if timepoints selected are OK, then compute RGR
        
        w1 = tab3_data()[3]
        w2 = tab3_data()[4]
        
        rgr = round( RGR_interval(w1, w2, t1, t2), digits = 4)
        
        res.tab3 = paste("RGR = ", rgr, "days^-1")
        
        
      } else {
        res.tab3 = "Please select timepoints that have data"
      }
    } else {
      res.tab3 = "Please select timepoints that have data"
    }

    res.tab3
  })
  
  ##### tab 4 (Integral approach) ######
  df.tab4 = df[which(df$Species == "Papaya"),]
  
  tab4_idx = reactive({
    which(names(df.tab4)== input$tab4_variable)
  })
  
  tab4_data = reactive({
    t1 = input$tab4_TP1
    t2 = input$tab4_TP2
    
    w1 = mean(df.tab4[which(df.tab4$PlantAge == t1), tab4_idx()], na.rm=T)
    w2 = mean(df.tab4[which(df.tab4$PlantAge == t2), tab4_idx()], na.rm=T)
    
    c(t1, t2, w1, w2)
  })
  
  output$tab4_plot = renderPlot({
    t1 = tab4_data()[1]
    t2 = tab4_data()[2]
    w1 = tab4_data()[3]
    w2 = tab4_data()[4]
    
    plot(df.tab4$PlantAge, df.tab4[,tab4_idx()], main = "Papaya TopPlantSurface",
         ylab = "Papaya TopPlantSurface", xlab = "PlantAge (days)", cex = 2,
         pch = 16, col=rgb(red=1, green=0.6, blue=0.2, alpha=0.6))
        abline(v= c(input$tab4_TP1, input$tab4_TP2 ), lty = 3, lwd = 2, col=rgb(red=0, green=0.4, blue=0.8, alpha=0.8) )
        
        ## check if the timepoints selected are within the dataset, add polygon if OK
        if(length(which(df.tab4$PlantAge == tab4_data()[1])) != 0 ){
          if(length(which(df.tab4$PlantAge == tab4_data()[2])) != 0 ){
            
            x = c(input$tab4_TP1, input$tab4_TP2 , input$tab4_TP2 , input$tab4_TP1)
            y = c(w1, w2, 0 , 0)
            polygon(x, y, col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.2), border = NA)
            
          }
        }
        
        tab4_expression = bquote(paste('W'[1]*' = ', .(w1), '; W'[2]*' = ', .(w2) ) )
        
        if(input$tab4_variable == "TopPlantSurface"){
          text(28, 300000, tab4_expression, pos = 4, col = "blue", cex = 1.25)
        } else {
          text(28, 1100, tab4_expression, pos = 4, col = "blue", cex = 1.25 )
        }
        
  })
  
  output$tab4_D = renderText({
    t1 = tab4_data()[1]
    t2 = tab4_data()[2]
    
    ## check if the timepoints selected are within the dataset
    if(length(which(df.tab4$PlantAge == input$tab4_TP1)) != 0 ){
      if(length(which(df.tab4$PlantAge == input$tab4_TP2)) != 0 ){
        ## if timepoints selected are OK, then compute RGR
        
        w1 = mean(df.tab4[which(df.tab4$PlantAge == t1), tab4_idx()], na.rm=T)
        w2 = mean(df.tab4[which(df.tab4$PlantAge == t2), tab4_idx()], na.rm=T)
        
        d = D_integral(w1, w2, t1, t2)
        rgr = round(RGR_integral(w1, w2, d), digits = 4)
        
        res.tab4 = paste("RGR = ", rgr, " days^-1", sep ="")
        
      } else {
        res.tab4 = "Please select timepoints that have data"
      }
    } else {
      res.tab4 = "Please select timepoints that have data"
    }
    res.tab4
  })
  
  ##### tab 5 (Functional approach) ######
  
  df.tab5 = df[which(df$Species == "Rice"),]
  idx = which(names(df) == "TopPlantSurface")
  
  # call data() to access in Outputs below, e.g. outputPlot
  data = reactive({  
    idx.geno = which(df.tab5$Genotype == input$tab5_geno)
    
    # t (time) and A (area) for nls
    t = df.tab5$PlantAge[idx.geno]
    A = df.tab5[idx.geno, idx]
    
    # initial values
    K.init = max(A)
    N0.init = min(A[A>0]) # default to something small
    
    d= data.frame(cbind(t, A))
    names(d) = c("t", "A")
    
    # make an initial estimate for r
    set.seed(5)
    glm_mod <- stats::glm(A / K.init ~ t,
                          family = stats::quasibinomial("logit"),
                          data = d)
    
    r_init <- stats::coef(glm_mod)[[2]]   # slope
    
    mod.logistic = nls(A ~ K/(1+(((K - N0)/N0) * exp(-r*(t)))), 
                       start = list(K = K.init, r = r_init, N0= N0.init), data= data.frame(A, t),
                       trace = TRUE)
    params = coef(mod.logistic)
    
    list("t" = t, "A" = A, "params" = params)
  })
  
  output$tab5_text = renderText({
    rgr = round(RGR_functional(data()$params[1], data()$params[3] , data()$params[2], input$tab5_TP), digits = 4)
    paste("RGR = ", rgr, " days^-1", sep="")
  })
  
  output$tab5_plot = renderPlot({
    
    plot(data()$t, data()$A, main = "Rice TopPlantSurface", cex = 2, 
         ylab = "Rice TopPlantSurface", xlab = "PlantAge (days)", ylim = c(0, 180000), xlim= c(0, 65), 
         pch = 16,  col=rgb(red=1, green=0.6, blue=0.2, alpha=0.6) )
    plot.fitted.log(data()$params[1], data()$params[3] , data()$params[2] , 1:65, color = rgb(red=0.2, green=0.2, blue=1.0, alpha=0.2),
                    line.width = 8)
    abline(v= c(input$tab5_TP ), lty = 3, lwd = 2, col=rgb(red=0, green=0.4, blue=0.8, alpha=0.8) )
  
    expression = paste("K = ", round(data()$params[1], digits=2), "; N = ", round(data()$params[3], digits=2), 
                       "; r = ", round(data()$params[2], digits=2), sep="")
    text(0, 160000, "Parameter estimates:", pos = 4, col = "blue", cex = 1.25)
    text(0, 140000, expression, pos = 4, col = "blue", cex = 1.25)
  
  })
  
  
  
} # Close the server
