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
      
      conditionalPanel(condition="input.types=='Elliptical Copula'&& (input.plots=='2'||input.plots=='3')",
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
      
      conditionalPanel(condition="input.plots=='2'||input.plots=='3'",
                       radioButtons("plot2",h4("Choose the type of Plot"),
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
      
      conditionalPanel(condition="input.plot2=='2'",
                       sliderInput("theta_plot","Plot: theta value",min=-180,max=180,value=-40),
                       sliderInput("phi","Plot: phi value",min=0,max=360,value=30)),
      conditionalPanel(condition="input.plots=='2'||input.plots=='3'",
                       sliderInput("N","N Value",min=10,max=100,value=50)),
      conditionalPanel(condition="(input.plots=='2'||input.plots=='3')&&input.plot2=='1'",
                       sliderInput("nlevel","Level of Contour",min=5,max=100,value=15))
      
      
    ),
    
    
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("min_max") ,
      textOutput("alpha_value"),
      plotOutput("plots_output"))
  ))
