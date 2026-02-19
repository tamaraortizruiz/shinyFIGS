coreCollectionUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("group"), "Select a group", list("")),
    selectInput(ns("uid"), "Select the unique identifier", list("")),
    selectInput(ns("allocMeth"), "Select allocation method", choices = c("Propotional" = "Pro","Logarythmic" = "Log","D2 allocation method" = "D2","D3 allocation method" = "D3"), selected = "Propotional"),
    selectInput(ns("clustMeth"), "Select cluster analysis method", choices = c( "average", "single", "complete", "ward"), selected = "ward"),
    numericInput(ns("fraction"), "Select fraction", 0.1, min = 0, max = 1, step = 0.1)
  )
}

coreCollectionMod <- function(input, output, session, rv){

  observe({
    req(rv$data4core)
    updateSelectInput(session, "group", label = "Select a group", choices = colnames(rv$data4core))
    updateSelectInput(session, "uid", label = "Select the unique identifier", choices = colnames(rv$data4core))
  })
  
  core <- reactive({
    
    req(rv$data4core, input$group, input$uid, 
        input$group != "", input$uid != "")
    
    alloc <- input$allocMeth
    cluster_method <- input$clustMeth
    fraction <- input$fraction
    group <- input$group
    uid <- input$uid
    
    data4core <- rv$data4core
    
    if(group=="PopulationType"){
      data4core <- rv$data4core %>% dplyr::filter(PopulationType!="Unknown")
    }
    
    req(nrow(data4core) > 0)
    rownames(data4core) <- as.character(data4core[[uid]])
    
    climate_columns <- search4pattern(c('tavg*', 'tmin*', 'tmax*', 
                                        'prec*', 'bio*', 'srad*', 
                                        'vapr*', 'wind*'), names(data4core))
    
    select_columns <- c(group, climate_columns)
    data4core_sub <- data4core[select_columns]
    
    data4core_sub_na_omit <- data4core_sub[complete.cases(data4core_sub[[group]]), ]
    req(nrow(data4core_sub_na_omit) > 0)
    
    original_levels <- as.factor(data4core_sub_na_omit[[group]])
    new_levels <- levels(original_levels)
    data4core_sub_na_omit[[group]] <- as.integer(original_levels)
    
    withProgress(message = "Developing Core Collection ...", {
      core <- ccChooser::stratcc(x = data4core_sub_na_omit, 
                                 groups = data4core_sub_na_omit[[group]], 
                                 alloc = alloc, 
                                 fraction = fraction, 
                                 clustering = TRUE, 
                                 cluster_method = cluster_method)
    })
    
    req(nrow(core) > 0)
    
    core[[group]] <- factor(core[[group]], 
                               levels = seq_along(new_levels), 
                               labels = new_levels)
    
    core[[uid]] <- rownames(core)
    
    core <- merge(x = core, y = rv$data4core)
    core <- core %>% relocate(climate_columns, .after = last_col())
    
    list(core, group)
   })
 
  return(core)
}