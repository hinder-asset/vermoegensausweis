# As of 2016-10-01 Robert Leitner

##################################################################################################################
library(reshape2)
library(shiny)
library(shinyBS)

##################################################################################################################
##################################################################################################################

##################################################################################################################
#                                                                                                                #
#                                        Main Body                                                               #
#                                                                                                                #
##################################################################################################################

##################################################################################################################
##################################################################################################################
shinyUI(fluidPage(title="HAM Vermoegensausweis",
                  
                  
              # Appearance:
              
              #################################################################
              #   Header with Logo                                            #
              #                                                               #
              #################################################################
              #         #                                                 #   #
              # left    #       mainPanel:                                #  <- "export_print"
              # panel:  #        "main_question_panel"                    #   #
              #"sidebar"#        "comment_panel"                          #   #
              #         #                                                 #   #
              #         #                                                 #   #
              #         #                                                 #   #
              #################################################################
              #   Footer                                                      #
              #                                                               #
              #################################################################
                                
                

                  
##################################################################################################################
# header:
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "style_mozilla_win.css"),
                    tags$style("#container-fluid{font-family: serif; font-size: 13px; margin: 0;
                    }"),
                    tags$meta(name="viewport", content="width=device-width, initial-scale=.25, maximum-scale=2.0, minimum-scale=.01, user-scalable=yes"),
                    tags$style(HTML("

                                    .row {
                                    padding: 0px;
                                    }
                                    
                                    #row {
                                    padding: 0px;
                                    }
                                    
                                   #space1 {
                                   height: 15px;
                                   }
                                    
                                   #broadview .btn:hover {
                                    background: #708090;
                                    text-decoration: none;
                                    border: solid #708090 1px;
                                    }
                                    
                                    
                                    .tab-pane{
                                    
                                    background: #FFFFFF;
                                    
                                    }
                                    
                                    #tab-pane{
                                    
                                    background: #FFFFFF;
                                    
                                    }

                                    #start_button_initial{
                                    font-family: verdana;
                                    font: 18px verdana, sans-serif;
                                    }
                                    
                                    #broadview .btn {
                                    
                                    font-family: verdana;
                                    font: 11px verdana, sans-serif;
                                    color: #ffffff;
                                    font-size: 11px;
                                    font-weight: bold;
                                    background: #708090;
                                    padding: 10px 20px 10px 20px;
                                    text-decoration: none;
                                    border: solid #708090 2px;
                                    }


                                    #wrapper2{
                                    align:center;
                                    width: 100%;
                                    }


                                    .btn {
                                    
                                    font-family: verdana;
                                    font: 13px verdana, sans-serif;
                                    color: #004790;
                                    font-size: 13px;
                                    font-weight: bold;
                                    text-decoration: none;
                                    }

                                    #broadview .btn:focus {outline:0;}
                                    #broadview .p { font-color: white; color: white;}
                                    #broadview { font-color: white; color: white;
                                    width: 1000px;
                                    font-size: 12px;
                                    font-weight: bold;
                                    padding: 10px 10px 10px 10px;}
                                    
                                    #broadview2 .btn:focus {outline:0;}
                                    #broadview2 .p { font-color: white; color: white;}
                                    #broadview2 { font-color: white; color: white;
                                    width: 1000px;
                                      position: absolute; 
                                      bottom: 0;
                                    font-size: 12px;
                                    font-weight: bold;
                                    padding: 10px 10px 10px 10px;}

                                    #wrapper {
                                    align: center;
                                    min-width: 960px;
                                    max-width: 1600px;
                                    padding: 10px 10px 10px 10px;
                                    }

                                    #mainpanels{
                                    position: relative;
                                    float: right; width: 80%; margin-right: 10%; margin-left: -90%;
                                    align: left;
                                    min-width: 400px;
                                    max-width: 1200px;
                                    min-height: 600px;
                                    width: 80%
                                    float: left;
                                    border-left: 0.5px solid;
                                    border-right: 0.5px solid;
                                    border-color: #d3d3d3;
                                    }

                                    "))
                            ),

# Logo has the property on click -> Restart app
                       HTML('
                       <div id="HAMHeader">
                       <!-- Logo -->
                       <div id="HAMLogo">
                       <a href="http://www.hinder-asset.ch/">
                       <img src="hinder_asset_management_logo2.gif" width="250" height="78" alt="Zur Startseite" title="Zur Startseite">
                       </a>
                       </div>
                       <!--End logo -->
                       '),
                  htmlOutput("PageTitle"),
                  HTML('
                       </div>
                       '),

##################################################################################################################
# Main body:
div(id="wrapper", class="container",

div(id="broadview", class="container",

      
      bsButton("backwards_button","Zurueck",  style="link", size="small"),
      bsButton("start_button","Weiter",  style="link", size="small"),
             style="background-color: #708090; font-color: white; border-radius: 5px;", align="right"),

  div(id="mainpanels", class="container",
        uiOutput("main_question_panel"),
        uiOutput("comment_panel1"),
      style="background-color: #FFFFFF;"
  )
# div(id="broadview2",
#     p("Hinder Asset Management AG   I   Beethovenstrasse 3   I   8002 Zürich   I   Schweiz   I   Telefon: +41 44 208 24 24  I  E-Mail: info@hinder-asset.ch"),
#     style="background-color: #708090; font-color: #708090; border-radius: 5px;", align="center")
)

)
)
