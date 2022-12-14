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
      updateSliderInput(session,"alpha",label="rho value",min=-1,max=1,value=0.5)
    
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
      uv=grid2d(x = (0:input$N)/input$N)
      P=parchmCopula(u =uv, v = uv, alpha =input$alpha, type = as.character(input$no),
                     output = "list")
      image(P, col = heat.colors(16) )
      contour(P, xlab = "u", ylab = "v", nlevels =input$nlevel, add = TRUE)
    }
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="2"&&input$plot2=="1" ){
      
      if (input$no4=="norm"||input$no4=="logistic"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =input$nlevel, add = TRUE)}
      
      if (input$no4=="t"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =input$nlevel, add = TRUE)}
      
      else {
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =input$nlevel, add = TRUE)}
    }
    
    
    ##Extreme Value Copula
    
    if (input$types=="Extreme Value Copula"&& input$plots =="2"&&input$plot2=="1" ){
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels =input$nlevel, add = TRUE)}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels = input$nlevel, add = TRUE)}
      
      else{
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels = input$nlevel, add = TRUE)}
      
    }
    
    
    ######################Perspective
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="2"&&input$plot2=="2" ){
      uv=grid2d(x = (0:input$N)/input$N)
      P=parchmCopula(u =uv, v = uv, alpha =input$alpha, type = as.character(input$no),
                     output = "list")
      persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
            zlab = "C(u,v)" )}
    
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="2"&&input$plot2=="2" ){
      if (input$no4=="norm"||input$no4=="logistic"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        persp(P, theta = input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
              ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
              zlab = "C(u, v)", xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1) )}
      
      if (input$no4=="t"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        persp(P, theta = input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
              ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
              zlab = "C(u, v)", xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1) )}
      
      else {
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        persp(P, theta = input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
              ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
              zlab = "C(u, v)", xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1) )}
    }
    
    
    ##Extreme Value Copula
    
    if (input$types=="Extreme Value Copula"&& input$plots =="2"&&input$plot2=="2" ){
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)}
      
      else{
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)}
      
    }
    
    
    
    #######################################Density###################################
    #################################Contour
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="3"&&input$plot2=="1" ){
      uv = grid2d(x = (1:(input$N-1)/input$N))
      D=darchmCopula(u = uv, v = uv, alpha = input$alpha, type =as.character(input$no),
                     output ="list", alternative = FALSE ) 
      image(D, xlim = c(0, 1), ylim = c(0,1), col = heat.colors(16) )
      contour(D, xlab = "u", ylab = "v", nlevels = input$nlevel, add = TRUE)
    }
    
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="3"&&input$plot2=="1" ){
      
      if (input$no4=="norm"||input$no4=="logistic"){
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        image(D, col = heat.colors(16), ylab = "v", xlim = c(0,1), ylim = c(0,1) )
        mtext("u", side = 1, line = 2)
        contour(D, nlevels = input$nlevel, add = TRUE)}
      
      if (input$no4=="t"){
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        image(D, col = heat.colors(16), ylab = "v",xlim = c(0,1), ylim = c(0,1) )
        mtext("u", side = 1, line = 2)
        contour(D, nlevels =input$nlevel, add = TRUE)}
      
      else {
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        image(D, col = heat.colors(16), ylab = "v", xlim = c(0,1), ylim = c(0,1) )
        mtext("u", side = 1, line = 2)
        contour(D, nlevels = input$nlevel, add = TRUE)}
    }
    
    
    ##Extreme Value Copula
    if (input$types=="Extreme Value Copula"&& input$plots =="3"&&input$plot2=="1" ){
      
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (1:input$N-1)/input$N)
        D=devCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        image(D, col = heat.colors(16) )
        contour(D, nlevels = input$nlevel, add = TRUE)}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (1:input$N-1)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        image(D, col = heat.colors(16) )
        contour(D, nlevels = input$nlevel, add = TRUE)}
      
      else{
        uv = grid2d(x = (1:input$N-1)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        image(D, col = heat.colors(16) )
        contour(D, nlevels = input$nlevel, add = TRUE)}
    }
    
    
    
    
    #################################Perspective
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="3"&&input$plot2=="2" ){
      uv = grid2d(x = (1:(input$N-1))/input$N)
      D=darchmCopula(u =uv, v = uv, alpha =input$alpha, type = as.character(input$no),
                     output = "list")
      persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
            zlab = "C(u,v)" )
    }
    
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="3"&&input$plot2=="2" ){
      if (input$no4=="norm"||input$no4=="logistic"){
        
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        Var = var(as.vector(D$z), na.rm = TRUE)
        if (Var < 1.0e-6) {
          Mean = round(1.5*mean(as.vector(D$z), na.rm = TRUE), 2)
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlim = c(0, Mean), zlab = "C(u,v)" )
        } else {
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlab = "C(u,v)" )
        }
      }
      
      if (input$no4=="t"){
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        Var = var(as.vector(D$z), na.rm = TRUE)
        if (Var < 1.0e-6) {
          Mean = round(1.5*mean(as.vector(D$z), na.rm = TRUE), 2)
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlim = c(0, Mean), zlab = "C(u,v)" )
        } else {
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlab = "C(u,v)" )
        }}
      
      else {
        D=dellipticalCopula(u = input$N, v =input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        Var = var(as.vector(D$z), na.rm = TRUE)
        if (Var < 1.0e-6) {
          Mean = round(1.5*mean(as.vector(D$z), na.rm = TRUE), 2)
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlim = c(0, Mean), zlab = "C(u,v)" )
        } else {
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlab = "C(u,v)" )
        }}
    }
    
    
    ##Extreme Value Copula
    if (input$types=="Extreme Value Copula"&& input$plots =="3"&&input$plot2=="2" ){
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (0:input$N)/input$N)
        D=devCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (0:input$N)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)}
      
      else{
        uv = grid2d(x = (0:input$N)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)}
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
