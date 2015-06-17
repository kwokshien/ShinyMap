#server.R

library(shiny)
require(rCharts)
require(devtools)
shinyServer(function(input,output){
  
  map1 <- Leaflet$new()
  JOHOR.latlong = c(1.968543, 103.427241)
  KEDAH.latlong = c(5.918662, 100.652949)
  KELANTAN.latlong = c(5.408780, 102.060372)
  LABUAN.latlong = c(5.318625, 115.219707)
  MELAKA.latlong = c(2.310956, 102.310971)
  N.SEMBILAN.latlong = c(2.778099, 102.171897)
  PAHANG.latlong = c(3.720752, 102.668101)
  PERAK.latlong = c(4.792993, 101.122553)
  PERLIS.latlong = c(6.487922, 100.246505)
  PULAU_PINANG.latlong = c(5.390236, 100.254387)
  SABAH.latlong = c(5.380094, 116.952980)
  SARAWAK.latlong = c(2.851508, 113.479144)
  SELANGOR.latlong = c(3.273800, 101.541154)
  TERENGGANU.latlong = c(4.930664, 102.990655)
  WPKL_PUTRAJAYA.latlong = c(3.151687, 101.696468)
  ALL.latlong = c(3.943535, 109.765039)
 
  
  output$map2 <- renderMap({
    if(input$negeri == "ALL"){
    map1$setView(c(3.943535, 109.765039), zoom = 5)
    
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "JOHOR")    
    map1$marker(JOHOR.latlong, bindPopup = paste0("JOHOR ",as.character(new_df2[a,3])," cases"))
    
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "KEDAH")
    map1$marker(KEDAH.latlong, bindPopup = paste0("KEDAH ",as.character(new_df2[a,3])," cases"))
    
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "KELANTAN")
    map1$marker(KELANTAN.latlong, bindPopup = paste0("KELANTAN ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "LABUAN")              
    map1$marker(LABUAN.latlong, bindPopup = paste0("LABUAN ",as.character(new_df2[a,3])," cases"))
    
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "MELAKA")
    map1$marker(MELAKA.latlong, bindPopup = paste0("MELAKA ",as.character(new_df2[a,3])," cases"))
    
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "N.SEMBILAN")
    map1$marker(N.SEMBILAN.latlong, bindPopup = paste0("N.SEMBILAN ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PAHANG")              
    map1$marker(PAHANG.latlong, bindPopup = paste0("PAHANG ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PERAK")              
    map1$marker(PERAK.latlong, bindPopup = paste0("PERAK ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PERLIS")                 
    map1$marker(PERLIS.latlong, bindPopup = paste0("PERLIS ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PULAU_PINANG")                  
    map1$marker(PULAU_PINANG.latlong, bindPopup = paste0("PINANG ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "SABAH")                  
    map1$marker(SABAH.latlong, bindPopup = paste0("SABAH ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "SARAWAK")                  
    map1$marker(SARAWAK.latlong, bindPopup = paste0("SARAWAK ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "SELANGOR")                 
    map1$marker(SELANGOR.latlong, bindPopup = paste0("SELANGOR ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "TERRENGGANU")                
    map1$marker(TERENGGANU.latlong, bindPopup = paste0("TERRENGGANU ",as.character(new_df2[a,3])," cases"))
                  
    a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "WPKL_PUTRAJAYA")               
    map1$marker(WPKL_PUTRAJAYA.latlong, bindPopup = paste0("WPKL ",as.character(new_df2[a,3])," cases"))
    }
    else if(input$negeri == "JOHOR")
    {
      map1$setView(JOHOR.latlong, zoom = 8)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "JOHOR")    
      map1$marker(JOHOR.latlong, bindPopup = paste0("JOHOR ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "KEDAH")
    {
      map1$setView(KEDAH.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "KEDAH")    
      map1$marker(KEDAH.latlong, bindPopup = paste0("KEDAH ",as.character(new_df2[a,3])," cases"))
    }
     
    else if(input$negeri == "KELANTAN")
    {
      map1$setView(KELANTAN.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "KELANTAN")    
      map1$marker(KELANTAN.latlong, bindPopup = paste0("KELANTAN ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "LABUAN")
    {
      map1$setView(LABUAN.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "LABUAN")    
      map1$marker(LABUAN.latlong, bindPopup = paste0("LABUAN ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "MELAKA")
    {
      map1$setView(MELAKA.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "MALAKA")    
      map1$marker(MELAKA.latlong, bindPopup = paste0("MELAKA ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "N.SEMBILAN")
    {
      map1$setView(N.SEMBILAN.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "N.SEMBILAN")    
      map1$marker(N.SEMBILAN.latlong, bindPopup = paste0("N.SEMBILAN ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "PAHANG")
    {
      map1$setView(PAHANG.latlong, zoom = 8)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PAHANG")    
      map1$marker(PAHANG.latlong, bindPopup = paste0("PAHANG ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "PERAK")
    {
      map1$setView(PERAK.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PERAK")    
      map1$marker(PERAK.latlong, bindPopup = paste0("PERAK ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "PERLIS")
    {
      map1$setView(PERLIS.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PERLIS")    
      map1$marker(PERLIS.latlong, bindPopup = paste0("PERLIS ",as.character(new_df2[a,3])," cases"))
    }
    else if(input$negeri == "PULAU PINANG")
    {
      map1$setView(PULAU_PINANG.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "PULAU PINANG")    
      map1$marker(PULAU_PINANG.latlong, bindPopup = paste0("PENANG ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "SABAH")
    {
      map1$setView(SABAH.latlong, zoom = 8)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "SABAH")    
      map1$marker(SABAH.latlong, bindPopup = paste0("SABAH ",as.character(new_df2[a,3])," cases"))
    }
    else if(input$negeri == "SARAWAK")
    {
      map1$setView(SARAWAK.latlong, zoom = 8)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "SARAWAK")    
      map1$marker(SARAWAK.latlong, bindPopup = paste0("SARAWAK ",as.character(new_df2[a,3])," cases"))
    }
    else if(input$negeri == "SELANGOR")
    {
      map1$setView(SELANGOR.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "SELANGOR")    
      map1$marker(SELANGOR.latlong, bindPopup = paste0("SELANGOR ",as.character(new_df2[a,3])," cases"))
    }
    else if(input$negeri == "TERRENGGANU")
    {
      map1$setView(TERRENGGANU.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "TERRENGGANU")    
      map1$marker(TERRENGGANU.latlong, bindPopup = paste0("TERRENGGANU ",as.character(new_df2[a,3])," cases"))
    }
    
    else if(input$negeri == "WPKL/PUTRAJAYA")
    {
      map1$setView(WPKL_PUTRAJAYA.latlong, zoom = 10)
      a<-which(new_df2$Year==input$Year & new_df2$NEGERI == "WPKL/PUTRAJAYA")    
      map1$marker(WPKL_PUTRAJAYA.latlong, bindPopup = paste0("WPKL ",as.character(new_df2[a,3])," cases"))
    }

    map1
    
  })
  
})

