# ui.R

library(shiny)

# Initialize the layout with a navbar
fluidPage(
  withMathJax(),

  navbarPage(title = "Plant Growth Analysis",

             #### First tab ####
             tabPanel(title = "Introduction",

                      titlePanel("Introduction"),

                            mainPanel(

                              p("Characterizing growth and development is an important component of plant ecophysiology. It requires collecting information over time on size, form, number, and/or mass, depending on the specific questions addressed. Of important plant growth parameters, ",
                              strong("relative growth rate "), "is among the most informative and can be computed using interval, integral and functional approaches. This learning module will walk through each of these approaches and is geared towards undergraduate students of plant physiology as a supplement to their foundational coursework."),
                              br(),
                        
                              h4("The Data"),
                               
                              "The data used in this module come from the ", tags$a(href="https://ag.purdue.edu/cepf/", "Purdue University Controlled Environment Phenotyping Facility (CEPF).") , " The CEPF is a plant growth facility that employs an automated system for irrigation, fertigation, and imaging.",
                              "There are three camera systems at the facility: red-green-blue (RGB), hyperspectral, and computed tomography root scanner.",
                              
                              "We will be examining data from the RGB imaging system. Plant images of three papaya",
                              em("(Carica papaya,"), "cv. Sunrise) and 31 rice ", em("(Oryza sativa,"), "16 diverse cultivars) were taken at 12 angles several times per week. These images were processed through an analytical pipeline, and two of the resulting computed metrics will be used in this lesson: SideAverageHeight (Figure 1) and TopPlantSurface (Figure 2). SideAverageHeight is the computed distance from the bottom of the green box to the top of the green box in Figure 1. TopPlantSurface is the computed area in ",
                              HTML(paste0("mm",tags$sup("2"))), "occupied by grey pixels bounded by the red outline in Figure 2. The full dataset can also be downloaded here for educators or researchers to re-purpose for other analyses.",
                              br(),
                              br(),
                              
                              # Download data button
                              downloadButton(outputId = "tab1_download", label = "Download data"),
                              br(),
                              br(),
                              
                              img(src='figures_HTP_app.png', align = "center", height = "100%", width = "100%"),
                              br(),
                              
                              h4("Learning Objectives"),
                              
                              p("After completing this module, you should be able to:"),
                              
                              tags$li("  Compare and contrast the differences between interval, integral and functional approaches"),
                              tags$li("  Discuss the benefits and trade-offs of high-throughput phenotyping data"),
                              br(),
                              
                              h3("Click on the Relative Growth Rate tab to begin"),
                              
                              br(),
                              br()

                            )
                   ),


             #### Second tab ####
             tabPanel("Relative Growth Rate",

                      titlePanel("Relative Growth Rate"),
                      
                      # Create a sidebar panel
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          p("Instantaneous RGR of a plant growth trait (e.g., biomass, leaf area) is defined by the following equation:"),
                          br(),
                          
                          p("$$RGR = \\frac{dW}{dt} \\frac{1}{W} = \\frac{d(lnW)}{dt}$$"),
                          br(),
                          
                          p("where W represents the plant trait of interest."),
                          
                          HTML(paste0("RGR is the rate of increase or decrease in the trait relative to its previous value; for this reason, RGR is in units of inverse time, e.g. days",tags$sup("-1"), " or weeks", tags$sup("-1"), ".")),
                          "What would be the units of growth rate?",
                          br(),
                          br(),
                          
                          
                          p("In this learning module, we will be using the CEPF metrics, SideAverageHeight (mm) and ",
                          HTML(paste0("TopPlantSurface (mm",tags$sup("2"))), "), however, many other variables are part of this dataset."),
                          br(),
                          
                          p("To plot these traits and others that are part of the dataset, you can make selections in the drop-down menus to the right."),
                          br(),
                          
                          p("Do you notice common shapes to growth trajectories across various traits?")
                          
                    
                          ), 
                        
                        # output scatterplot
                        
                        mainPanel(
                          # dropdown menus to select species and growth variable
                          
                          selectInput("tab2_species", "Select species", choices = c("Papaya", "Rice")),
                          selectInput("tab2_variable", "Select growth variable", selected = "TopPlantSurface",
                                      choices = c("PlantAge","SideAverageCenterOfMassDistance", "SideAverageCenterOfMassX",
                                                  "SideAverageCenterOfMassY" ,"SideAverageConvexHull", "SideAverageHeight", 
                                                  "SideAverageHue","SideAverageRoundness","SideAverageSaturation","SideAverageSurface",
                                                  "SideAverageValue", "TopAverageHue","TopAverageSaturation", 
                                                  "TopAverageValue", "TopCenterOfMassDistance", "TopCenterOfMassX","TopCenterOfMassY",
                                                  "TopConvexHull","TopPlantSurface", "TopRoundness", "TopSurface")),
                       
                          textOutput("tab2_text"),
                          plotOutput("tab2_plot")
                        )
                        )
             ),

             #### Third tab ####
             tabPanel("Interval Approach",

                      titlePanel("Interval Approach"),

                      # Create a sidebar panel
                      sidebarLayout(

                        sidebarPanel(
                          p("Collecting plant growth data was traditionally destructive, which limited the number of timepoints during which a researcher could measure. The interval approach works with as few as two timepoints. Here, we will use SideAverageHeight and TopPlantSurface to examine the RGR across any two timepoints (i.e. an interval) for papaya."),
                          br(),
                          
                          p("RGR using the interval approach is computed"),
                          br(),
                          p("$$RGR = \\frac{ln({W_2}) - ln({W_1})} {({t_2}-{t_1})}$$"),
                          br(),
                          
                          p("where W is either SideAverageHeight (mm) or ", HTML(paste0("TopPlantSurface (mm",tags$sup("2"))), ") and t is time (days)."),
                          br(),
                          
                          p("Using the papaya dataset, find RGR at three different pairs of timepoints (during the early, middle and end of the experiment) for each trait. How does RGR change throughout the experiment?"),
                          br(),
                          
                          # Dropdown menu for trait selection
                          selectInput("tab3_variable", "Select growth variable", choices = c("TopPlantSurface", "SideAverageHeight"))
                          
                        ),
                        
                        mainPanel(
                          
                          # Slider for timepoint 1
                          sliderInput(inputId = "tab3_TP1",
                                      "Select timepoint 1:",
                                      value = 80,
                                      min = 25,
                                      max = 140 ),
                          
                          # Slider for timepoint 2
                          sliderInput(inputId = "tab3_TP2",
                                      "Select timepoint 2:",
                                      value = 99,
                                      min = 25,
                                      max = 140 ),
                          
                          span(textOutput("tab3_RGR"), style="color:blue"),
                          plotOutput("tab3_plot")
                          
                          
                        )
                        
                      )

              # Close the tab
             ),

             
             #### Fourth tab ####
             tabPanel("Integral Approach",

                      titlePanel("Integral Approach"),

                      # Create a sidebar panel
                      sidebarLayout(
                        
                        sidebarPanel(
                          p("The integral approach can also be used to compute RGR. It entails calculating an additional duration parameter, which represents the area under the curve when the variable of interest is plotted against time."),
                          br(),
                          
                          p("$$ RGR = \\frac{W_2 - W_1} {D} $$"),
                          
                          p("where"),
                          
                          p("$$D=\\int_{t_1}^{t_2} W \\,dt $$"),
                          br(),
                          
                          p("Again using the papaya dataset, we will now use the trapezoidal rule at two timepoints to approximate D (shaded in purple). Select pairs of timepoints to approximate D and the resulting RGR. Do this for at least three timepoint pairs."),
                          
                          p("What are the units of D for each trait? How do the results of this approach compare with the interval approach to computing RGR?"),
                          br(),
                          
                          # Dropdown menu for trait selection
                          selectInput("tab4_variable", "Select growth variable", choices = c("TopPlantSurface", "SideAverageHeight"))
                        ),

                        mainPanel(
                          
                          # Slider for timepoint 1
                          sliderInput(inputId = "tab4_TP1",
                                      "Select timepoint 1:",
                                      value = 80,
                                      min = 25,
                                      max = 140 ),
                          
                          # Slider for timepoint 2
                          sliderInput(inputId = "tab4_TP2",
                                      "Select timepoint 2:",
                                      value = 99,
                                      min = 25,
                                      max = 140 ),
                          
                          span(textOutput("tab4_D"), style="color:blue"),
                          plotOutput("tab4_plot")
                          
                        )

                      )
             ),
             
             #### Fifth tab ####
             tabPanel(title = "Functional Approach",
                      titlePanel("Functional Approach"),

                      # Create a sidebar panel
                      sidebarLayout(
                        
                        sidebarPanel(
                          p("The functional approach to plant growth analysis involves fitting a curve to a time-series of observations and estimating the parameters of the curve."),
                          br(),
                          
                          "Sigmoidal and exponential growth patterns are commonly found in biology. In this exercise, you will fit the three-parameter logistic function (with parameters K, N and r) on diverse rice genotypes for TopPlantSurface. There are 16 different genotypes in this evaluation with all but one genotype having two replicates.",
                          br(),
                          br(), 
                          br(),
                          
                          p("The logistic growth curve is"),
                          
                          p("$$ W = \\frac{K} {1+ \\frac{K - N} {N} exp(-r t)} $$"),
                          
                          p("where W is the growth trait of interest and t is time. RGR is computed as the logarithmic derivative of this function."),
                          br(), 
                          
                          # dropdown menus to select genotype and timepoint
                          selectInput("tab5_geno", "Select genotype", selected = "Nipponbare",
                                      choices = c("Bico Branco","Dhala Shaitta" ,"DV85" ,"BJ 1", "Jaya" ,"Binulawan" , "IR64-21",
                                                  "Rathuwee","Chiem Chanh","Agostano","Nipponbare" ,"Geumobyeo" , "Moroberekan", "Trembese",
                                                  "Asse Y Pung","Jefferson") ),
                          
                          p("What can you tell about the minimum data requirement of the functional approach versus the former two? What are a few possible advantages that HTP phenotyping provides in this context?")
                          
                          
                        ),
                        
                        mainPanel(
                          
                          # Slider for timepoint
                          sliderInput(inputId = "tab5_TP",
                                      "Select single timepoint:",
                                      value = 25,
                                      min = 1,
                                      max = 65 ),
                          
                          span(textOutput("tab5_text"), style="color:blue"),
                          plotOutput("tab5_plot"),
                          
                          p("A note about the three parameters: "),
                          tags$li("  K - the horizontal asymptote as time approaches infinity. It can be thought of as a maximum value of the plant growth trait."),
                          tags$li("  N - the starting value at time of 0. This is where the curve intersects the y-axis. "),
                          tags$li("  r - the rate parameter."),
                          br()
                        )
                        
                      )
             ),
             
             
             #### Sixth tab ####
             tabPanel(title = "Summary",
                      
                      titlePanel("Summary"),
                      
                        mainPanel(
                          
                          p("In this lesson, we discussed interval, integral and functional approaches to analyzing relative growth rate:"),
                          br(),
                          
                          tags$li(strong("Interval:"), " Computes an average relative growth rate across two timepoints."),
                          tags$li(strong("Integral:"), " Computes relative growth rate using an additional duration parameter."),
                          tags$li(strong("Functional:"), " Computes instantaneous RGR using the logarithmic derivative of a fitted function of growth over time."),
                          br(),
                          
                          p("The questions presented in this learning module were meant to generate discussion, and below are some possible responses."),
                          br(),
                          
                          p(strong("RGR tab: Do you notice common shapes to growth trajectories across
                            various traits?")),
                            
                          p("Some look exponential (e.g., Papaya TopPlantSurface and Papaya TopConvexHull), while others look logistic (e.g., Rice TopPlantSurface).
                            Other variables have no well-defined shape at all or have extreme
                            outliers that make it difficult to define (e.g. Papaya TopCenterOfMassDistance)."),
                          br(),
                          
                          p(strong("Interval tab: How does RGR change throughout the experiment?
                            RGR decreases throughout the experiment.")),
                          
                          p("RGR decreases throughout the experiment."),
                          br(),
                          
                          p(strong("Integral tab: What are the units of D for each trait? How do the results of
                            this approach compare with the interval approach to computing RGR?")),
                          
                          p("The units of D for TopPlantSurface is mm^2 days and for SideAverageHeight is mm days.
                            The response to the second question depends on the specific timepoints selected.
                            For some timepoint pairs, the interval approach gives a greater RGR value, whereas for others,
                            the integral approach gives a greater value."),
                          br(),
                          
                          p(strong("Functional tab: What can you tell about the minimum data requirement of the functional approach versus the former two? What are a few possible
                            advantages that HTP phenotyping provides in this context?")),
                          
                          p("We saw how the interval and integral approaches can be used to estimate
                            RGR with as little as two timepoints of data. The functional approach,
                            on the other hand, requires sampling across the entirety of a growth
                            trajectory. With the advancement of HTP, the functional approach can be
                            more easily applied across many genotypes with relatively little effort.
                            However, as HTP technologies are based on indirect means of measurement,
                            it is also important to collect ‘ground-truth’ datasets to check that
                            there is high correlation of HTP variables with target traits. This is
                            especially important when applying such approaches to untested systems
                            (new species, new treatments, new environments etc)."),
                          br(),
                          
                          h4("Other Resources"),
                          
                          p("Much of the content for this learning module is developed from the following textbook. Students are encouraged to consult this resource for more details on plant growth analysis and other ecophysiological methods."),
                          br(),
                          p(em("Chiariello, Nona R.; Mooney, Harold A; Williams, Kimberlyn. Growth, carbon allocation and cost of plant tissues. Plant Physiological Ecology: Field methods and instrumentation, Eds: Pearcy, RW; Ehleringer, J; Mooney, HA; Rundel, PW, Volume 1, Chapman and Hall Ltd, 1989, New York, NY (pgs 327-365).")),
                          br(),
                          
                          p("For primary studies on how plant growth analysis have been employed in research, students can search using Google Scholar or start with the suggested papers below:"),
                          br(),
                          p(em("Granier C, Tardieu F. Is thermal time adequate for expressing the effects of temperature on sunflower leaf development?. Plant, Cell & Environment. 1998 Jul;21(7):695-703.")), 
                          
                          p(em("Baker RL, Leong WF, Welch S, Weinig C. Mapping and predicting non-linear Brassica rapa growth phenotypes based on bayesian and frequentist complex trait estimation. G3: Genes, Genomes, Genetics. 2018 Apr 1;8(4):1247-58.")),
                          br(),
                          
                          h4("Citation"),
                          "Wang, DR; Imel RK; Paull RE; Kantar, MK. An online learning module for plant growth analysis using high-throughput phenotyping data.",
                          "In prep. ", em("Natural Sciences Education"),
                          br(), br()
                          
                          
                        )
                      
             )
             
             

   # End the navigation bar
  )

) # End the UI

