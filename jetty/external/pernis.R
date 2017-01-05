
tabPanel('Pernis', br(),
		
		strong(div("Port Time Schema", style="color:#f94f40; font-size:20px")), br(),
		p(span(img(src = "jetty_t.PNG"))), br(), br(),  
		
		strong(div("Parcel List", style="color:#f94f40; font-size:20px")), br(),
      
		DT::dataTableOutput("table"),
		downloadButton('downloadparcel', 'Download'), br(), br(),
      
		wellPanel(
			verbatimTextOutput("text0"),
			verbatimTextOutput("text1")
		),
      
		br(),
		strong(div("Distribution Plot", style="color:#f94f40; font-size:20px")), br(),br(),
		plotlyOutput("histplot", height = "400px")
		
)

