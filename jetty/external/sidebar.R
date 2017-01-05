
sidebarPanel(

	wellPanel( # add parcels
        strong(div("Add Parcels", style="color:#f94f40; font-size:16px")), br(),
        
        selectInput("parcelName", 
                    "Select Products", 
                    choices = sort(c("", unique(d$ProductName))) #?? ""
					),
					
        numericInput("quant","Quantity", value = 0),
        
		actionButton("Add", "Add"),
        actionButton("Delete","Delete")
      ),
      
	wellPanel(
        strong(div("Setting", style="color:#f94f40; font-size:16px")), br(),
        
        numericInput("nsamp","Number of Simulations", value = 500),
        selectInput("method",
                    "Select method",
                    choices = c("statistical",
                                "anticipated")
					),
					
        uiOutput("ui4min"),
        uiOutput("ui4max"),
        uiOutput("ui17min"),
        uiOutput("ui17max"),
        uiOutput("ui18min"),
        uiOutput("ui18max"),
        uiOutput("ui35min"),
        uiOutput("ui35max"),
        uiOutput("ui35Amin"),
        uiOutput("ui35Amax")
      ), 
      
    actionButton("Evaluate",
				 "evaluate", 
				 icon("paper-plane"), 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
				)  	
)


