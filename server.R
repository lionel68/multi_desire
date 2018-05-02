############################################
### A shiny app to explore desirability  ###
############################################

#Below is the server code

library(shiny)
library(plyr)
library(ggplot2)
library(ggtern)
library(viridis)


#setwd("Documents/PostDoc_Ghent/Synthesis_stuff/Shiny/multi_desire/")
#helper functions
revv <- function(vec){
  return(- vec + (max(vec) + min(vec)))
}

desirab <- function(mat,direction,importance){
  for(i in 1:ncol(mat)){
    if(direction[i] == -1){
      mat[,i] <- revv(mat[,i])
    }
  }
  out <- apply(mat,1,function(x) sum(importance * x) / sum(importance))
  return(out)
}

dd_mat <- function(ypred,direction,importance,probs=c(0.25,0.5,0.75),type){
  tmp <- apply(ypred,1,function(mat) desirab(mat,direction,importance))
  out <- adply(tmp,1,quantile,probs,na.rm=TRUE)
  names(out)[2:4] <- c("LCI","Med","UCI")
  if(type=="div"){
    out <- cbind(out,X_div)
    out$divF <- factor(out$specrich,labels=c(1,2,3))
  }
  if(type=="comp"){
    out$speccomb <- rep(comp,each=3)
    out$fragm <- rep(c(-2.42,0,1.69),7)
  }
  if(type=="kir"){
    out <- cbind(out,X_kir)
  }
  return(out)
}

#load data
#the model prediction
pred_div <- readRDS("data/prediction_diversity.rds")
pred_comp <- readRDS("data/prediction_comp.rds")
pred_kir <- readRDS("data/prediction_kirwan.rds")
#the newdata matrix
X_div <- read.table("data/X_div.csv",head=TRUE)
X_comp <- read.table("data/X_comp.csv",head=TRUE)
X_kir <- read.table("data/X_kir.csv",head=TRUE)

#load the standard direction
dirr <- read.table("data/dirr.csv",head=TRUE)
dirr <- dirr[,1]

comp <- c("qrob_qrub","fsyl","fsyl_qrub","all","fsyl_qrob","qrub","qrob")

# Define server 
shinyServer(function(input, output) {
  #define the importance scores based on the choice of the user (default, all equal or manual)
  importance <- reactive({
    if(input$type == "default"){
      tmp <- c(1,1,1,1,4,3,7,3.66666666666667,5,3.5,2,3,
        7.33333333333333,6,6,6,6,4,6.33333333333333,
        6.66666666666667,9.5,9.5,9.33333333333333,7)
    }
    
  
  if(input$type == "equal"){
    tmp <- rep(1,24)
  }
  if(input$type == "manual"){
    tmp <- c(input$Egg_bact,input$Egg_IgY,input$Egg_vol,input$Fle_IgY,input$Breed_succ,input$Bird_smi,input$Herbivory,
      input$Predation,input$Decomp,input$Arth_div,input$Fit_spider,input$Spider_diet,input$C_stock,input$BS,
      input$pH,input$CN_ratio,input$Phosph,input$Veg_div,input$GLI,input$LAI,input$P_germ,input$Seed_biom,
      input$Biomass,input$cover)
  }
    return(tmp)
  })


  
  #compute desirability for each model
  d_div <- reactive({dd_mat(pred_div,dirr,importance(),type="div")})
  d_comp <- reactive({dd_mat(pred_comp,dirr,importance(),type="comp")})
  d_kir <- reactive({dd_mat(pred_kir,dirr,importance(),type="kir")})
  #the plots
  output$div_f_plot <- renderPlot({
    
    gg <- ggplot(d_div(),aes(x=fragm,y=Med,group=divF)) +
      geom_ribbon(aes(ymin=LCI,ymax=UCI,fill=divF),alpha=0.2) +
      geom_path(aes(color=divF)) +
      labs(fill="Species richness",color="Species richness",x="Fragmentation intensity (from high to low)",y="Desirability score")
    
    gg
    
  })
  
  
  output$kir_f_plot <- renderPlot({
    
    gg <- ggtern(d_kir(),aes(fsyl,qrob,qrub))+
      theme_bw()+
      geom_tri_tern(bins=4,fun=mean,aes(value=Med,fill=..stat..)) +
      stat_tri_tern(bins=4,fun=mean,geom="text",aes(value=Med,
                                                    label=sprintf("%.2f",..stat..)),
                    color="lightgrey",centroid=TRUE) +
      scale_fill_viridis(option="C") +
      theme(legend.position = "none")
    
    gg
    
    
  })
  
  
  output$comp_f_plot <- renderPlot({
    
    gg <- ggplot(d_comp(),aes(x=fragm,y=Med,group=speccomb)) +
      geom_ribbon(aes(ymin=LCI,ymax=UCI,fill=speccomb),alpha=0.1) +
      geom_path(aes(color=speccomb),size=2) +
      labs(fill="Species\ncomposition",color="Species\ncomposition",x="Fragmentation intensity (from high to low)",y="Desirability score")
    gg
    
  })
  
})
