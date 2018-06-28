#Set Parameters
subData = F
standalone = T

checkPackage = function(pack) {
  if (!is.element(pack, installed.packages()[,1])) {
    install.packages(pack, dependencies = TRUE,
                     repos = "http://cran.us.r-project.org")
  }
}

#Import Libraries
checkPackage("htmlwidgets")
library(htmlwidgets)
checkPackage("shiny")
library(shiny)
checkPackage("visNetwork")
library(visNetwork)
checkPackage("shinydashboard")
library(shinydashboard)
checkPackage("rhandsontable")
library(rhandsontable)
checkPackage("bnlearn")
library(bnlearn)
checkPackage("LearnBayes")
library(LearnBayes)



#### Import Data ####
setwd(getwd())

if (standalone) {
  source(paste(getwd(),"/fsoPath.dat", sep = ""))
  mainData = read.csv(filePath)
} else {
  mainData = read.csv("hcp_dataset.csv",
                      fileEncoding="UTF-8-BOM")
}

#mainData = read.csv("HCP Household Heads Prop Sample.V2.csv")
#mainData = read.csv("afsun_dataset.csv")
#mainData = read.csv("pseudodata.csv")
#mainData = coronary
#coronary
#asia
#learning.test


#Force to bnlearn supported data type
for (i in 1:ncol(mainData)) {
  mainData[,i] = as.factor(mainData[,i])
}

#Force complete case analysis
if (length(which(is.na(mainData))) > 0) {
  print(paste("Removed", length(which(is.na(mainData))), "incomplete cases."))
  mainData = mainData[complete.cases(mainData),]
}

set.seed(963522132)

#Get Subset
if (subData) {
  subata <- sample.int(n = nrow(mainData),
                       size = floor(0.05*nrow(mainData)),
                       replace = F)
  mainData <- mainData[subata, ]
}



#### Functions ####

valid = function(obj) {
  #a variant to the (!is.null) argument
  if (is.null(obj)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

getClickType = function(input) {
  if (valid(input$selectNode)) {
    return("node")
  } else if (valid(input$selectEdge)) {
    return("edge")
  } else {
    return(NULL)
  }
}

nameNodes = function(rawData) {
  n = length(rawData)
  nodeName = names(rawData[1])
  nodes <- data.frame(id = nodeName, 
                      name = nodeName, 
                      label = nodeName,
                      title = nodeName)
  
  #coerce data type
  nodes$id = as.character((nodes$id))
  nodes$name = as.character((nodes$name))
  nodes$label = as.character((nodes$label))
  nodes$title = as.character((nodes$title))
  
  for (i in 2:n) {
    nodeName = names(rawData[i])
    nodes = rbind(nodes, c(id = nodeName,
                           name = nodeName,
                           label = nodeName,
                           title = nodeName))
  }
  return(nodes)
}

getModString = function(rawData) {
  n = length(rawData)
  modNet = ""
  for (i in 1:n) {
    modNet = paste(modNet, "[", names(rawData[i]), "]", sep = "")
  }
  return(modNet)
}

getEdgeList = function(tempDag, rawData) {
  rawArcStrength = arc.strength(tempDag, rawData)
  n = nrow(arc.strength(tempDag, rawData))
  if (n == 0) {
    edgeList = data.frame(from = character(),
                          to = character(),
                          arrows = character(),
                          id = integer())
  } else {
    edgeList = data.frame(from = unlist(rawArcStrength[1]),
                          to = unlist(rawArcStrength[2]),
                          arrows = "to",
                          id = c(1:n),
                          row.names = NULL)
  }
  
  edgeList$from = as.character((edgeList$from))
  edgeList$to = as.character((edgeList$to))
  edgeList$arrows = as.character((edgeList$arrows))
  
  return(edgeList)
}

getNewEdge = function(graph, edgeList) {
  fromId = graph$from
  toId = graph$to
  
  
  fromLabel = nodeStruc[[fromId]]["label"]
  toLabel = nodeStruc[[toId]]["label"]
  
  g = data.frame(from = unlist(fromLabel),
                 to = unlist(toLabel),
                 arrows = "to",
                 id = graph$id,
                 row.names = NULL)
  
  g$from = as.character(g$from)
  g$to = as.character(g$to)
  g$arrows = as.character(g$arrows)
  
  return(g)
}

addEdge = function(input, output, edgeDf) {
  #create a new dataframe to append to edgelist
  newEdge = getNewEdge(input$myNetId_graphChange, edgeDf)
  
  if (isDuplicate(edgeDf, newEdge)) {
    visNetworkProxy("myNetId") %>%
      visRemoveEdges(id = input$myNetId_graphChange$id)
    output$shiny_return <- renderPrint({
      print("Cannot create duplicate edge.")
    })
  } else if (length(nodeStruc[[newEdge$to]][["myParent"]]) == 5) {
    visNetworkProxy("myNetId") %>%
      visRemoveEdges(id = input$myNetId_graphChange$id)
    output$shiny_return <- renderPrint({
      print("Maximum number of parents reached for this node")
    })
  } else {
    #update dag
    arcError = tryCatch({
      dag <<- set.arc(dag,
                      newEdge$from,
                      newEdge$to,
                      debug = F)
      edgeDf <<- rbind(edgeDf, newEdge)
      
      #update nodeStruc
      addChildParent(newEdge)
      
    }, error = function(e) {
      #Cyclical error
      visNetworkProxy("myNetId") %>%
        visRemoveEdges(id = input$myNetId_graphChange$id)
      output$shiny_return <- renderPrint({
        print("Error: The resulting graph contains cycles.")
      })
    })
  }
}

deleteEdge = function(input, edgeDf) {
  deleteType = getDeleteType(input$myNetId_graphChange)
  
  if (deleteType == "edge") {
    #Remove edge from db
    deleteId = input$myNetId_graphChange$edges
    deleteIndex = which(edgeDf$id == deleteId)
    
    f = edgeDf[deleteIndex,1]
    t = edgeDf[deleteIndex,2]
    
    dag <<- drop.arc(dag,
                     from = f,
                     to = t)
    
    #remove deleted edges from nodeStruc
    rmvIndex = which(nodeStruc[[f]][["myChild"]] == t)
    nodeStruc[[f]][["myChild"]] <<- nodeStruc[[f]][["myChild"]][-rmvIndex]
    
    rmvIndex = which(nodeStruc[[t]][["myParent"]] == f)
    nodeStruc[[t]][["myParent"]] <<- nodeStruc[[t]][["myParent"]][-rmvIndex]
    
    #Remove from edgeDf
    edgeDf <<- edgeDf[-c(deleteIndex), ]
  }
}

getDeleteType = function(inGraph) {
  if (length(inGraph$nodes) > 0) {
    #TODO: change ui to only allow edge deletion
    ##else record which edges were deleted along with node
    if (length(inGraph$edges) > 0) {
      warning("Connected Node Deleted")
      return("both")
    } else {
      print("Node Deleted")
      return("node")
    }
  } else if (length(inGraph$edges) > 0) {
    print("Edge Deleted")
    if (length(inGraph$edges) > 1) {
      stop("Error: Multiple edges deleted without node deletion")
    } else {
      return("edge")
    }
  }
}

addChildParent = function(edge) {
  t = edge$to
  f = edge$from
  nodeStruc[[f]][["myChild"]] <<- c(nodeStruc[[f]][["myChild"]], t)
  nodeStruc[[t]][["myParent"]] <<- c(nodeStruc[[t]][["myParent"]], f)
}

clearChildParent = function(){
  for (i in 1:length(nodeStruc)) {
    nodeStruc[[i]][["myChild"]] <<- character()
    nodeStruc[[i]][["myParent"]] <<- character()
  }
}

removeAllEdges = function(edgeList, dag) {
  for (i in 1:nrow(edgeList)) {
    dag <<- drop.arc(dag,
                     from = as.character(edgeList[i,1]),
                     to = as.character(edgeList[i,2]),
                     debug = F)
    
    visNetworkProxy("myNetId") %>%
      visRemoveEdges(id = edgeList[i,4])
  }
}

isDuplicate = function(database, observation) {
  k = nrow(database)
  
  if (k == 0) {
    return(FALSE)
  }
  
  for (i in 1:k) {
    obj = database[i,]
    if (as.character(obj$from) == observation$from) {
      if (as.character(obj$to) == observation$to) {
        return(TRUE)
      }
    }
    #Check inverse
    if (as.character(obj$from) == observation$to) {
      if (as.character(obj$to) == observation$from) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

idToLabel = function(input) {
  tempId = input$current_node_id
  #Uses the global variable "nodeStruc
  label = nodeStruc[[tempId]]["label"]
  label = as.character(label)
  return(label)
}

printNodeProb = function(input, output) {
  observe({
    if(valid(input$myNetId_nodes)) {
      if (valid(input$current_node_id)) {
        nodeLabel = idToLabel(input)
        #Check if making this private breaks anything
        fit <- bn.fit(dag, mainData)
        
        #Post node values in sidebar
        output$shiny_return <- renderPrint({
          fit[as.character(nodeLabel)]
        })
        updateSpreadsheet(nodeLabel, output)
      }
    }
  })
  
}

updateSpreadsheet = function(nodeLabel, output) {
  if (length(nodeLabel) == 0) {
    warning("nodeLabel has a length of zero")
    return()
  }
  
  nodeLabel = as.character(nodeLabel)
  freqTable = prop.table(table(mainData[[nodeLabel]]))
  
  #Catch catagorical variables
  if (length(freqTable) > 2) {
    output$hot <- renderRHandsontable({
      #Blank
    })
    warning("Please ensure each variable has up to two possible responses")
    return()
  }
  
  freqTable[1] = round(freqTable[1], 2)
  freqTable[2] = round(freqTable[2], 2)
  
  #Convert to matrix to preserve structure
  m = matrix(freqTable)
  
  #Convert to matrix for naming
  df = as.data.frame(m)
  df = t(df)
  rownames(df) = nodeLabel
  colnames(df) = names(freqTable)
  
  if (is.null(nodeStruc[[nodeLabel]][["prior"]])) {
    df[1] = 1
    df[2] = 1
  } else {
    df[1] = nodeStruc[[nodeLabel]][["prior"]][1]
    df[2] = nodeStruc[[nodeLabel]][["prior"]][2]
  }
  
  
  values <- reactiveValues()
  values[["df"]] = df
  
  if (valid(df)) {
    output$hot <- renderRHandsontable({
      rhandsontable(df, rowHeaderWidth = 150)
    })
  }
}

getPostMean = function(success, failure, a, b) {
  totalSuccess = success+a-1
  totalFailure = failure+b-1
  return(totalSuccess/(totalSuccess+totalFailure))
}

getPosterior = function(a, b, response, prior, data) {
  if (length(levels(b)) == 2) {
    if (length(levels(a)) == 2) {
      if (!missing(data)) {
        attach(data)
        a = a
        b = b
        detach(data)
      }
      
      aResponse = levels(a)[response]
      bResponse = levels(b)[response]
      na = length(a)
      nb = length(b)
      
      if (na != nb) {
        stop("dataset has an unequal number of observations per column")
      }
      n = na
      
      if (missing(prior)) {
        pab1 = length(which(a == aResponse & b == bResponse)) / n
        pb = length(which(b == bResponse)) / n
        pab = pab1 / pb
        return(pab)
      } else {
        if(is.null(prior)) {
          stop("NULL prior value supplied")
        }
        if (!is.numeric(prior)) {
          stop("Non-numeric prior")
        }
        if (prior > 1) {
          stop("Prior greater than 1.0")
        }
      }
      pa = prior
      
      pba1 = length(which(b == bResponse & a == aResponse)) / n
      paOriginal = length(which(a == aResponse)) / n
      pba = pba1 / paOriginal
      
      paNot = 1-pa
      pbaNot1 = length(which(b == bResponse & a != aResponse)) / n
      pbaNot = pbaNot1 / (1 - paOriginal)
      
      normConst = (pba*pa)+(pbaNot*paNot)
      pab = (pba*pa)/ normConst
      return(pab)
    }
  }
  stop("No code written for catagorical variables")
}

makePrior = function(node, newArray) {
  if (length(coef(node)) == length(newArray)) {
    #Get k dimensions
    k = length(dimnames(coef(node)))
    names = dimnames(coef(node))
    frame = vector()
    for (i in 1:k) {
      frame = c(frame, length(unlist(names[i])))
    }
    
    #1: colName, 2: Row Name, 3: table name, etc
    dim(newArray) = frame
    dimnames(newArray) = dimnames(coef(node))
    return(newArray)
  } else {
    print("Number of priors does not match existing data frame")
  }
  #finally
  print("Could not parse belief propagation")
  return(coef(node))
}

plotPost = function(input) {
  if(valid(input$current_node_id)) {
    nodeLabel = idToLabel(input)
    
    freqTable = table(mainData[[nodeLabel]])
    success = freqTable[1]
    failure = freqTable[2]
    x = seq(0,1,0.01)
    
    likDist = dbeta(x, success + 1, failure + 1)
    
    if (valid(nodeStruc[[nodeLabel]][["prior"]])) {
      beta = nodeStruc[[nodeLabel]][["prior"]]
      for (i in 1:2) {
        if (beta[i] == 0) {
          beta[i] = 1
        }
      }
      
      priorDist = dbeta(x, beta[1], beta[2])
      postDist = dbeta(x, success + beta[1], failure + beta[2])
      contenders = c(priorDist, likDist, postDist)
      contenders = contenders[is.finite(contenders)]
      top = max(contenders)
      
      #Get ess ratio
      priorPercent = (beta[1] + beta[2]) / (success + failure + beta[1] + beta[2])
      priorPercent = round(priorPercent,2) * 100
      priorPercent = ifelse(priorPercent == 0, "<1", priorPercent)
      iss = as.character(paste(priorPercent, "% of this model is elicited from the prior*", sep = ""))
      
      plot(x, likDist, type = "l", col = "red",
           ylim = c(0, top),
           ylab = "Density",
           xlab = c("Percent",iss),
           main = nodeLabel)
      lines(x, priorDist, col = "blue",
            lty = "dotted")
      lines(x, postDist, col = rgb(0,0.75,0),
            lty = "longdash")
      legend(x = 0, y = top,
             c("Likelihood", "Prior", "Posterior"),
             col = c("red", "blue", rgb(0,0.75,0)),
             lty = c("solid", "dotted", "longdash"))
    } else {
      plot(x, likDist, type = "l", col = "red",
           ylab = "Density",
           xlab = "Percent",
           main = nodeLabel)
    }
  }
}

getMultiPosterior = function(nameList, resp, db) {
  n = length(nameList)
  
  if (n != length(resp)) {
    warning("nameList and response lengths do not match")
    return()
  }
  
  #Loop for Either
  eitherIndex = vector()
  
  v = vector()
  respIndex = vector()
  p = logical()
  for (i in 1:n) {
    #(variable)
    v[i] = list(db[[nameList[i]]])
    
    #Check for catagorical variables
    if (length(levels(v[[i]])) > 2) {
      output$shiny_return <- renderPrint({
        print("Please ensure each variable has up to two possible responses")
      })
      return()
    }
    
    #catch all input = 'either'
    if (resp[i] == "Either") {
      eitherIndex = c(eitherIndex, i)
    } else {
      #(response for variable)
      respIndex[i] = which(levels(v[[i]]) == resp[i])
      p[i] = list(v[[i]] == levels(v[[i]])[respIndex[[i]]])
    }
  }
  
  #Permutate all 'either' responses up to 3
  nEither = length(eitherIndex)
  cpt = vector()
  if (nEither > 3) {
    print("CP tables must be limited to three dimensions")
    return()
    
    ##3##
  } else if (nEither == 3) {
    for (ni in 1:2) {
      p[eitherIndex[3]] = list(v[[eitherIndex[3]]] == levels(v[[eitherIndex[3]]])[ni])
      for (nii in 1:2) {
        p[eitherIndex[2]] = list(v[[eitherIndex[2]]] == levels(v[[eitherIndex[2]]])[nii])
        for (niii in 1:2) {
          p[eitherIndex[1]] = list(v[[eitherIndex[1]]] == levels(v[[eitherIndex[1]]])[niii])
          
          #This info should be stored somewhere
          if(resp[1] == "Either") {
            pNot = v[[1]] != levels(v[[1]])[niii]
            childState = niii
          } else {
            pNot = v[[1]] != levels(v[[1]])[respIndex[1]]
            childState = respIndex[1]
          }
          
          cpt = c(cpt, getInteractionProb(n, p, pNot, v, nameList, childState))
        } 
      }
    }
    cpt = as.matrix(cpt)
    dim(cpt) = c(2, 2, 2)
    dimnames(cpt) = list(levels(v[[eitherIndex[1]]]), levels(v[[eitherIndex[2]]]), levels(v[[eitherIndex[3]]]))
    names(dimnames(cpt)) = list(nameList[eitherIndex[1]], nameList[eitherIndex[2]], nameList[eitherIndex[3]]) 
    
    ##2##
  } else if (nEither == 2) {
    for (ni in 1:2) {
      p[eitherIndex[2]] = list(v[[eitherIndex[2]]] == levels(v[[eitherIndex[2]]])[ni])
      for (nii in 1:2) {
        p[eitherIndex[1]] = list(v[[eitherIndex[1]]] == levels(v[[eitherIndex[1]]])[nii])
        
        if(resp[1] == "Either") {
          pNot = v[[1]] != levels(v[[1]])[nii]
          childState = nii
        } else {
          pNot = v[[1]] != levels(v[[1]])[respIndex[1]]
          childState = respIndex[1]
        }
        
        cpt = c(cpt, getInteractionProb(n, p, pNot, v, nameList, childState))
      }
    }
    cpt = as.matrix(cpt)
    dim(cpt) = c(2, 2)
    dimnames(cpt) = list(levels(v[[eitherIndex[1]]]), levels(v[[eitherIndex[2]]]))
    names(dimnames(cpt)) = list(nameList[eitherIndex[1]], nameList[eitherIndex[2]])
    
    ##1##
  } else if (nEither == 1) {
    for (ni in 1:2) {
      p[eitherIndex[1]] = list(v[[eitherIndex[1]]] == levels(v[[eitherIndex[1]]])[ni])
      
      if(resp[1] == "Either") {
        pNot = v[[1]] != levels(v[[1]])[ni]
        childState = ni
      } else {
        pNot = v[[1]] != levels(v[[1]])[respIndex[1]]
        childState = respIndex[1]
      }
      
      cpt = c(cpt, getInteractionProb(n, p, pNot, v, nameList, childState))
    }
    cpt = as.matrix(cpt)
    dim(cpt) = 2
    dimnames(cpt) = list(levels(v[[eitherIndex]]))
    names(dimnames(cpt)) = list(nameList[eitherIndex[1]])
    
    ##0##
  } else if (nEither < 1) {
    respIndexNot = which(levels(v[[1]]) == resp[1])
    pNot = v[[1]] != levels(v[[1]])[respIndexNot]
    
    cpt = c(cpt, getInteractionProb(n, p, pNot, v, nameList, respIndexNot))
    cpt = as.matrix(cpt)
    #Assume the selected node is the one they want
    dimnames(cpt) = list(resp[1], nameList[1])
  }
  
  #Print contingencies
  given = character()
  for (i in 1:n) {
    if (resp[i] != "Either") {
      given = c(given, paste(nameList[i], "=", resp[i], "\n"))
    }
  }
  cat("Conditional Probability Table\n")
  cat("\n")
  print(cpt)
  
  if (nEither < n) {
    cat("\n\ngiven that:\n", given)
  }
  
  prior = nodeStruc[[nameList[1]]][["postMean"]]
  if (valid(prior)) {
    cat("Used a prior for", nameList[1], "of", round(prior, 4), '\n')
  }
}

getInteractionProb = function(n, p, pNot, v, nameList, responseIndex) {
  prior = nodeStruc[[nameList[1]]][["postMean"]]
  
  if (n > 1) {
    if (valid(prior)) {
      if (responseIndex == 2) {
        prior = 1 - prior
      }
      
      #With Prior
      pa = length(which(p[[1]])) / length(p[[1]])
      top = length(which(Reduce("&", p))) / length(p[[1]])
      top = top / pa * prior
      
      #attach all logical vectors since Reduce() is finicky
      intersection = list()
      intersection[[1]] = pNot
      for (i in 2:n) {
        intersection[i] = p[i]
      }
      
      bottom = length(which(Reduce("&", intersection))) / length(p[[1]])
      bottom = bottom / (1-pa) * (1-prior)
      
      return(top/(top+bottom))
    } else {
      #No Prior
      top = length(which(Reduce("&", p)))
      bottom =length(which(Reduce("&", p[-1])))
      return(top/bottom)
    }
  } else {
    if (valid(prior)) {
      if (responseIndex == 2) {
        prior = 1 - prior
      }
      return(prior)
    } else {
      top = length(which(p[[1]]))
      bottom =length(v[[1]])
      return(top/bottom)
    }
  }
}

getSelectState = function(input, output) {
  if(valid(input$current_node_id)) {
    nodeLabel = idToLabel(input)
    
    parent = nodeStruc[[nodeLabel]][["myParent"]]
    
    #Parents and self
    nodesList = c(nodeLabel, parent)
    
    if (length(nodesList) > 6) {
      output$selectState <- renderUI({
        print("Too many parents connected to a single node (>5)")
      })
      warning("Cannot process a node with more than 5 parents")
      return()
    }
    
    output$selectState <- renderUI({
      if(valid(input$current_node_id)) {
        lapply(1:length(nodesList), function(i) {
          response = levels(mainData[[nodesList[i]]])
          selectInput(paste("select", as.character(i), sep = ""),
                      nodesList[i],
                      width = "25%",
                      c(response[1], response[2], "Either"),
                      selected = ifelse(i <= 3, "Either", response[1]))
        })
      }
    })
  } else {
    output$selectState <- renderUI({
      print("Please select a node to generate conditional probability table (CPT)")
    })
  }
}

getSaveState = function(input, output) {
  output$saveState <- renderUI({
    if(valid(input$current_node_id)) {
      actionButton("saveStateButton", "Apply Changes")
    }
  })
}

getSavePrior = function(input, output) {
  output$savePrior <- renderUI({
    actionButton("savePriorButton", "Save Priors")
  })
}

getScore = function(graph, data, output) {
  output$bnScoreTextBox <- renderPrint({
    print(paste("Bayesian Network Score:", round(score(graph, data), 4)))
    print(graph)
  })
}

updateSidbarUi = function(input, output, dag, mainData) {
  if (input$useType == 'CP Table') {
    printNodeProb(input, output)
  } else if (input$useType == 'BN Score') {
    getScore(dag, mainData, output)
  }
}



#### CREATE DAG ####

modString = getModString(mainData)

# create and plot the network structure.
dag = model2network(modString)

#Declare Global Variable
edgeDf <- getEdgeList(dag, mainData)


########## #
#### UI ####
########## #

header = dashboardHeader(title = "Bayes Belief Network",
                         titleWidth = "400px")

sidebar = dashboardSidebar(width = "400px",
                           box(width = 12, height = 400, status = "info",
                               conditionalPanel(
                                 condition = "input.useType == 'CP Table'",
                                 verbatimTextOutput("shiny_return"),
                                 tags$head(tags$style(HTML("#shiny_return {
                                                           font-size: 12px;
                                                           overflow-y:scroll;
                                                           max-height: 375px;
                                                           background: ghostwhite;
                                                           }")))
    ),
    conditionalPanel(
      condition = "input.useType == 'BN Score'",
      verbatimTextOutput("bnScoreTextBox"),
      tags$head(tags$style(HTML("#bnScoreTextBox {
                                font-size: 12px;
                                overflow-y:scroll;
                                max-height: 375px;
                                background: ghostwhite;
                                }")))
    )
      ),
    #h3("Actions"),
    radioButtons("useType", "Select Output", c("CP Table", "BN Score")),
    actionButton("debugButton", "Debug"),
    actionButton("learnNetButton", "Learn Network")
    #,actionButton("savePriorButton", "Save Priors")
      )

body = dashboardBody(
  tabBox(
    side = "right", height = "485px", width = "200px",
    id = "bodyTab",
    selected = "Network",
    tabPanel("Set CPT",
             id = "cptTab",
             uiOutput("selectState"),
             uiOutput("saveState")
    ),
    tabPanel("Graph",
             id = "graphTab",
             plotOutput("priorPlot")
    ),
    tabPanel("Network",
             visNetworkOutput("myNetId",height = "450px", width = "480"))
  ),
  h4("Prior Beta Distribution"),
  # sliderInput(inputId = "ciSlider",
  #           label = "Confidence Interval Slider",
  #          post = "%",
  #         min = 0, max = 100, value = c(0, 100),
  #        step = 0.5, ticks = FALSE),
  div(style="display:inline-block", rHandsontableOutput("hot")),
  div(style="display:inline-block", uiOutput("savePrior"))
)

ui <- fluidPage(
  dashboardPage(header, sidebar, body)
)



############## #
#### SERVER ####
############## #

server <- function(input, output, session) {
  values <- reactiveValues()
  
  #setup network
  output$myNetId <- renderVisNetwork({
    visNetwork(nameNodes(mainData), edgeDf) %>%
      visPhysics(solver = "barnesHut",
                 minVelocity = 0.1,
                 forceAtlas2Based = list(gravitationalConstant = -150)) %>%
      visOptions(manipulation = TRUE, highlightNearest = FALSE) %>%
      visEdges(arrows = 'to') %>%
      visEvents(type = "once", beforeDrawing = "function(init) {
                Shiny.onInputChange('getNodeStruc','init');
  }") %>%
      visEvents(selectNode = "function(n) {
                Shiny.onInputChange('current_node_id', n.nodes);
      }",
                dragging = "function(n) {
                Shiny.onInputChange('current_node_id', n.nodes);
                }",
                deselectNode = "function(n) {
                Shiny.onInputChange('current_node_id', n.nodes);
                }",
                selectEdge = "function(e) {
                Shiny.onInputChange('current_edge_id', e.edges);
                }") %>%
      visEvents(click = "function(click) {
                Shiny.onInputChange('selectNode', click.nodes)
                Shiny.onInputChange('selectEdge', click.edges)
      }")
  })
  
  
  
  #update graph changes
  observe({
    if (valid(input$getNodeStruc)) {
      print("Initializing node structure...")
      visNetworkProxy("myNetId") %>%
        visGetNodes()
      #Returns: id, name, label, title, x, y
      nodeStruc <<- input$myNetId_nodes
    }
  })
  
  
  
  #On new tab click
  observeEvent(input$bodyTab, {
    if (input$bodyTab == "Graph") {
      output$priorPlot <- renderPlot({
        plotPost(input)
      })
    } else if (input$bodyTab == "Set CPT") {
      observe({
        getSelectState(input, output)
        getSaveState(input, output)
      })
    }
  })
  
  
  #on visNet Click
  observe({
    clickType = getClickType(input)
    
    if (is.null(clickType)) {
      output$shiny_return <- renderPrint({
        print(paste("Bayesian Network Score:", round(score(dag,  mainData), 4)))
      })
      output$hot <- renderRHandsontable({})
      output$savePrior <- renderUI({})
    } else if (clickType == "node") {
      printNodeProb(input, output)
      getSavePrior(input, output)
    } else if (clickType == "edge") {
      
      visNetworkProxy("myNetId") %>%
        visGetSelectedEdges()
      
      edgeIndex = which(edgeDf$id == input$myNetId_selectedEdges)
      
      #CPT radio selected
      output$shiny_return <- renderPrint({
        print(arc.strength(dag, mainData)[edgeIndex,])
      })
    }
  })
  
  
  
  #BN Score radio selected
  observeEvent(input$useType == 'BN Score', {
    getScore(dag, mainData, output)
  })
  
  
  
  ################## #
  #### MANIPULATE ####
  ################## #
  
  #Display manipulate data
  observeEvent (input$myNetId_graphChange, {
    cmd = input$myNetId_graphChange$cmd
    if (valid(cmd)) {
      if (cmd == "addEdge") {
        addEdge(input, output, edgeDf)
      } else if (cmd == "deleteElements") {
        deleteEdge(input, edgeDf)
      }
      updateSidbarUi(input, output, dag, mainData)
    }
  })
  
  
  
  #### HandsOnTable ####
  
  observeEvent(input$hot, {
    if (valid(input$hot)) {
      df = hot_to_r(input$hot)
    } else {
      if (is.null(values[["df"]])) {
        df <- df
      } else {
        #df <- values[["df"]]
      }
    }
    
    values[["df"]] <- df
  })
  
  
  ############### #
  #### BUTTONS ####
  ############### #
  
  observeEvent(input$debugButton, {
    #print(arc.strength(dag, mainData))
    #print(score(dag, mainData))
    #print(edgeDf)
    print(nodeStruc)
  })
  
  observeEvent(input$learnNetButton, {
    #remove all edges
    
    if (nrow(edgeDf) > 0) {
      clearChildParent()
      removeAllEdges(edgeDf, dag)
    }
    
    #add new (ML) edges
    dag <<- hc(mainData, score = "bic")
    
    edgeDf <<- getEdgeList(dag, mainData)
    
    #update NodeStruc
    if (nrow(edgeDf) > 0) {
      for (i in 1:nrow(edgeDf)) {
        addChildParent(edgeDf[i,])
      }
    }
    
    print(edgeDf)
    
    visNetworkProxy("myNetId") %>%
      visUpdateEdges(edges = edgeDf)
    
    updateSidbarUi(input, output, dag, mainData)
  })
  
  observeEvent(input$savePriorButton, {
    if (is.null(input$current_node_id)) {
      output$shiny_return <- renderPrint({
        print("No node selected")
      })
      return()
    }
    
    nodeLabel = idToLabel(input)
    beta = c(values[["df"]])
    
    #Update prior list (memory)
    nodeStruc[[nodeLabel]][["prior"]] <<- c(beta[1], beta[2])
    
    for (i in 1:2) {
      if (!is.numeric(beta[i])) {
        warning("Please set a number for the alpha/beta priors")
        return()
      }
      
      if (is.na(beta[i])) {
        warning("Please ensure alpha/beta priors are valid numbers")
        return()
      }
      
      if (beta[i] < 0) {
        warning("Please set nonnegative alpha/beta priors")
        return()
      }
    }
    
    #get alpha beta values from handsontable
    freqTable = table(mainData[[nodeLabel]])
    success = freqTable[1]
    failure = freqTable[2]
    
    prior = getPostMean(success, failure, beta[1], beta[2])
    prior = as.numeric(prior)
    
    nodeStruc[[nodeLabel]][["postMean"]] <<- prior
    
    nParent = which(edgeDf$to == nodeLabel)
    if (length(nParent) == 0) {
      cpt = c(prior, 1-prior)
    } else if (length(nParent) == 1) {
      parent = edgeDf$from[nParent]
      parent = as.character(parent)
      
      cpt = vector()
      for (i in 1:2) {
        post = getPosterior(a = mainData[[nodeLabel]],
                            b = mainData[[parent]],
                            response = i)
        
        #Append new p(a|b)
        #TODO: make sure the list inverts at second itteration
        ##DEPRECIATED##
        cpt = c(cpt, post, 1-post)
      }
    } else {
      warning("No code for parents > 1")
      return()
    }
    
    #depreciated since we manually calc cpt
    #Check if making this private breaks anything
    #fit[[nodeLabel]] <- makePrior(fit[[nodeLabel]], cpt)
    
    
    #Prior info box
    #incorporate into updateSidbarUi()
    # output$shiny_return <- renderPrint({
    #   #TODO: Make this a seperate datastructure in a seperate window
    #   print(fit[[nodeLabel]])
    # })
    output$priorPlot <- renderPlot({
      plotPost(input)
    })
  })
  
  observeEvent(input$saveStateButton, {
    #Shouldn't occur from user input (button hides when no node selected)
    if (is.null(input$current_node_id)) {
      warning("No node selected")
      return()
    }
    
    nodeLabel = idToLabel(input)
    parent = nodeStruc[[nodeLabel]][["myParent"]]
    nodesList = c(nodeLabel, parent)
    
    responseList = vector()
    
    #compile responseList (from parents)
    for (i in 1:length(nodesList)) {
      inputName = paste("select", as.character(i), sep = "")
      responseList = c(responseList, input[[inputName]])
    }
    
    output$shiny_return <- renderPrint({
      getMultiPosterior(nodesList, responseList, mainData)
    })
  })
  
  if (standalone) {
    # close the R session when Chrome closes
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}

shinyApp(ui = ui, server = server)