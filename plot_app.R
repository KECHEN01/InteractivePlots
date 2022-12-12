library(shiny)
library(fCopulae)
ui<-fluidPage(
  titlePanel("Copula Slider"),
  
  sidebarLayout(
    sidebarPanel(
      #Type of Copula
      selectInput("types", 
                  h4("Choose the type of Copula"),
                  choices = list("Archimedean Copula",
                                 "Elliptical Copula",
                                 "Extreme Value Copula"),
                  selected=1),
      
      #Number of Archimedean Copula   
      
      conditionalPanel(condition="input.types=='Archimedean Copula'",
                       numericInput("no",label="No. of Copula",value=1,min=1,max=22),
                       selectInput("names",label="Names of Copula",choices=list("Clayton"=1,
                                                                                "Ali-Mikhail-Hag"=3,
                                                                                "Grumbel-Hongard"=4,
                                                                                "Frank"=5,
                                                                                "Joe-Fran"=6,
                                                                                "Gumbel-Barnett"=9,
                                                                                "Genest-Ghoudi"=15),selected=1)),                                      
      #Except the number of Copula outside 1-22
      conditionalPanel(condition= "input.no<1 || input.no>22",
                       p("There is no Copula for that number!")),
      
      #Number of Elliptical Copula                           
      conditionalPanel(condition="input.types=='Elliptical Copula'&& input.plots=='1'",
                       selectInput("no2",label="No. and Name of Copula",
                                   choices=list("1-Normal"="norm",
                                                "2-Cauchy"="cauchy",
                                                "3-Student t"="t"),selected=1)),
      
      conditionalPanel(condition="input.types=='Elliptical Copula'&& input.plots=='2'",
                       selectInput("no4",label="No. and Name of Copula",
                                   choices=list("1-Normal"="norm",
                                                "2-Student t"="t",
                                                "3-Logistic"="logistic",
                                                "4-Exponential Power [Laplace|Kotz]"='epower'),selected=1)),
      
      
      #Number of Extreme Value Copula                           
      conditionalPanel(condition="input.types=='Extreme Value Copula'",
                       selectInput("no3",label="No.and Name of Copula",
                                   choices=list("1-Gumbel"="gumbel",
                                                "2-Galambos"="galambos",
                                                "3-Husler-Reiss"="husler.reiss",
                                                "4-Tawn"="tawn",
                                                "5-BB5"="bb5"),selected=1)),
      
      #Type of Plot
      radioButtons("plots", label = h4("Choose the type of Plots"),
                   choices = list("Variate" = 1, "Probability" = 2,"Density"=3), 
                   selected = 1),
      
      conditionalPanel(condition="input.plots=='2'",
                       radioButtons("plot2",h4("Choose the type of Probability Plot"),
                                    choices=list("Contour"=1,"Perspective"=2),
                                    selected=1)),
      
      
      #Different ranges for the corresponding groups of Archimedean Copula
      sliderInput("alpha","alpha value",value=4.5,min=-1,max=10,step=0.1),
      
      
      conditionalPanel(condition="input.types=='Extreme Value Copula'&& input.no3=='tawn'",
                       sliderInput("beta","beta value",min=0,max=1,value=0.5),
                       sliderInput("r","r value",min=1,max=10,value=5.5)),
      conditionalPanel(condition="input.types=='Extreme Value Copula'&& input.no3=='bb5'",
                       sliderInput("theta","theta value",min=1,max=10,value=5.5)),
      conditionalPanel(condition="input.types=='Elliptical Copula'&& input.plots=='2'&& input.no4=='t'",
                       sliderInput("nu","nu value",min=1,max=20,value=10.5)),
      conditionalPanel(condition="input.types=='Elliptical Copula'&& input.plots=='2'&& input.no4=='epower'",
                       sliderInput("s","s value",min=0.1,max=5,value=2.5)),
      
      conditionalPanel(condition="input.types=='Elliptical Copula'&&input.plots=='2'&& input.plot2=='2'",
                       sliderInput("theta_plot","theta value",min=-180,max=180,value=0),
                       sliderInput("phi","phi value",min=0,max=360,value=180))
      
      
    ),
    
    
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_max") ,
      textOutput("alpha_value"),
      plotOutput("plots_output"))
  ))


server<-function(input,output,session){
  
  ############################Variates#################################
  #Change the range and value of alpha according to the No of Archimedean Copula
  observe({
    new_no<-input$no
    new_plot<-input$plots
    types<-input$types
    if(new_no== 1 )
      updateSliderInput(session,"alpha","alpha value",min=-1,max=10,value=4.5)
    
    if((new_no== 2 || new_no==4 || new_no==6 || new_no==8 
        || new_no==12 || new_no==14 || new_no==15 || new_no==21) )
      updateSliderInput(session,"alpha","alpha value",
                        min=1,max=10,step=0.1,value=5)
    
    if(new_no== 3)
      updateSliderInput(session,"alpha",min=-1,max=1,step=0.1,value=0)
    
    if((new_no== 7 || new_no==9 || new_no==10 || new_no==22 ) )
      updateSliderInput(session,"alpha",min=0,max=1,step=0.1,value=0.5)
    
    if((new_no== 13 || new_no==16 || new_no==19 || new_no==20 ))
      updateSliderInput(session,"alpha",min=0,max=10,step=0.1,value=5)
    
    if(new_no== 11 )
      updateSliderInput(session,"alpha",min=0,max=0.5,step=0.1,value=0.2)
    
    if(new_no== 18 )
      updateSliderInput(session,"alpha",min=2,max=10,step=0.1,value=6)
    
    if ((new_no== 5 || new_no==17 ))
      updateSliderInput(session,"alpha",min=-10,max=10,step=0.1,value=0)
    
    if (types=="Elliptical Copula")
      updateSliderInput(session,"alpha",label="rho value",min=-1,max=1,value=0)
    
    if ((types=="Extreme Value Copula") && (input$no3=="gumbel"))
      updateSliderInput(session,"alpha","delta value",min=1,max=10,value=5.5)
    
    if ((types=="Extreme Value Copula")&& (input$no3=="galambos" || input$no3=="husler.reiss"))
      updateSliderInput(session,"alpha","delta value",min=0,max=10,value=5)
    
    if ((types=="Extreme Value Copula") && (input$no3=="tawn") )
      updateSliderInput(session,"alpha","alpha value", min=0,max=1,value=0.5)
    
    if ((types=="Extreme Value Copula" )&&(input$no3=="bb5"))
      updateSliderInput(session,"alpha","delta value",min=0,max=10,value=5)
    
    ##########################Probability###############################################
    
    if( types=="Elliptical Copula"&& new_plot=="2")
      updateSliderInput(session,"alpha","rho value",min=-0.95,max=0.95,step=0.05,value=0)
  })
  
  
  
  
  
  
  
  
  
  ######################################Different Plots#############################################
  output$plots_output<-renderPlot({
    
    #################################Variates##################################
    
    ##Archimedean
    
    if (input$types=="Archimedean Copula"&&input$plots=="1"){
      R<-rarchmCopula(n = 1000, alpha = input$alpha,type=as.character(input$no))
      plot(R, xlab = "U", ylab = "V", pch = 19, col = "steelblue")
      grid()}
    
    ##Elliptical
    
    if (input$types=="Elliptical Copula"&&input$plots=="1"){ 
      if ( input$no2=="norm" || input$no2=="cauchy"){
        R = rellipticalCopula(n = 3000, rho = input$alpha, param = NULL, type =input$no2)
        plot(x = R[, 1], y = R[, 2], xlim = c(0, 1), ylim = c(0, 1),
             xlab = "u", ylab = "v", pch = 19,col="steelblue")
        grid()}
      
      else {
        R = rellipticalCopula(n = 3000, rho = input$alpha, param = 4, type =input$no2)
        plot(x = R[, 1], y = R[, 2], xlim = c(0, 1), ylim = c(0, 1),
             xlab = "u", ylab = "v", pch = 19,col="steelblue")
        grid()}
    }
    
    ##Extreme Value
    
    if (input$types=="Extreme Value Copula"&&input$plots=="1"){
      if (input$no3=="gumbel" || input$no3=="galambos" || input$no3=="husler.reiss"){
        R = revCopula(1000, param = input$alpha, type = input$no3)
        plot(R, pch = 19, col = "steelblue")
        grid()}
      
      else if (input$no3=="tawn"){
        R = revCopula(1000, param =c(input$alpha,input$beta,input$r), type ="tawn")
        plot(R, pch = 19, col = "steelblue")
        grid()}
      
      else {
        R = revCopula(1000, param =c(input$alpha,input$theta), type = "bb5")
        plot(R, pch = 19, col = "steelblue")
        grid()}
    }
    
    
    
    
    
    #######################################Probability###################################
    
    ###################Contour
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="2"&&input$plot2=="1" ){
        uv=grid2d(x = (0:100)/100)
        P=parchmCopula(u =uv, v = uv, alpha =input$alpha, type = as.character(input$no),
                 output = "list")
        image(P, col = heat.colors(16) )
        contour(P, xlab = "u", ylab = "v", nlevels =15, add = TRUE)
    }
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="2"&&input$plot2=="1" ){
      
      if (input$no4=="norm"||input$no4=="logistic"){
        P=pellipticalCopula(u = 100, v = 100, rho =input$alpha, param = NULL, type =input$no4,
                   output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =15, add = TRUE)}
      
      if (input$no4=="t"){
        P=pellipticalCopula(u = 100, v = 100, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =15, add = TRUE)}
      
      else {
        P=pellipticalCopula(u = 100, v = 100, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =15, add = TRUE)}
    }
    
    
    ##Extreme Value Copula
    
    if (input$types=="Extreme Value Copula"&& input$plots =="2"&&input$plot2=="1" ){
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (0:100)/100)
        P=pevCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                            output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels = 15, add = TRUE)}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (0:100)/100)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels = 15, add = TRUE)}
      
      else{
        uv = grid2d(x = (0:100)/100)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                  output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
      contour(P, nlevels = 15, add = TRUE)}
    
    }
    
  })
  
  
  
  
  
  
  
  #######################################TEXT######################################
  # Change the no of Copula according to the selected name of copula
  observeEvent(input$names, {
    x <- input$names
    updateNumericInput(session, "no", value = x)
  })
  
  #Change the name of copula according to the numeric input 
  observe({
    y <- input$no
    updateSelectInput(session, "names",
                      selected = y)
  })
  
  #Show the type of copula chosen
  output$selected_var<-renderText({
    paste("You have selected the ",input$types)})
  
  output$min_max<-renderText({
    
    if (input$types=="Archimedean Copula"){
      paste("You have chosen No.",input$no,"Copula")}
    
    else if (input$types=="Elliptical Copula"){
      paste("You have chosen", input$no2,"Copula")}
    
    else{ paste("You have chosen",input$no3,"Copula")}
  })
  
  
  #Show the value of alpha
  output$alpha_value<-renderText({
    paste("The alpha value is ", input$alpha)
  })
  
  
}
shinyApp(ui=ui,server=server)
