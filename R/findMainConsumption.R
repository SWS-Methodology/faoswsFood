##' Find main consumption commodity
##' 
##' This function finds the main commodity (for a particular commodity tree) 
##' where the commodity is consumed.
##' 
##' @param tree A data.table containing the commodity tree.  In particular, it 
##'   needs to specify the parent/child relationship.
##' @param suaData A data.table containing the SUA data.  The important elements
##'   are the production, trade, stock changes and food supply.  This dataset 
##'   should contain data from only one year and country.
##' @param threshold1 Numeric.  For what value should we consider the food to 
##'   supply ratio to indicate that a commodity is eaten?  This should be a 
##'   numeric value between 0 and 1, and defaults to 0.8.
##' @param threshold2 Numeric.  If Imports > Exports, no production is required.
##'   But, if Prod > (I-X) * threshold2, we assign this commodity to a main 
##'   source of food.  Likewise, if Imports < Exports, production must be at 
##'   least Exports - Imports.  However, if Prod > (X-I) * (1 + threshold2), we 
##'   assign this commodity to a main food.  Note that this is only used after 
##'   the primary commodity has been identified as a non-food item (or at least
##'   not the main source of food).
##'   
##' @return A data.table with CPC codes and a logical value indicating whether 
##'   or not that commodity is the main commodity eaten.
##'   
##' @example
items = GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")[, code]
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD", dimensions = list(
  Dimension(name = "geographicAreaFS", keys = "79"),
  Dimension(name = "measuredElementFS",
            keys = c("51", "61", "71", "91", "141")),
  Dimension(name = "timePointYears", keys = "2011"),
  Dimension(name = "measuredItemFS", keys = items)
)
suaData = GetData(key)
tree = getCommodityTree(timePointYears = "2011")
tree = unique(tree[, c("measuredItemParentCPC", "measuredItemChildCPC"),
                   with = FALSE])
tree[, measuredItemParentFS := cpc2fcl(measuredItemParentCPC,
                                       returnFirst = TRUE)]
tree[, measuredItemChildFS := cpc2fcl(measuredItemChildCPC,
                                      returnFirst = TRUE)]
tree = tree[, c("measuredItemParentFS", "measuredItemChildFS"), with = FALSE]
findMainConsumption(tree, suaData)
##' 
##' @export
##' 

findMainConsumption = function(tree, suaData, threshold = 0.8){
  ## Data Quality Checks
  stopifnot(length(unique(suaData$geographicAreaFS)) == 1)
  stopifnot(length(unique(suaData$timePointYears)) == 1)
  stopifnot(threshold > 0)
  stopifnot(threshold < 1)
  
  ## Convert all commodity codes to numeric
  tree[, measuredItemParentFS := as.numeric(measuredItemParentFS)]
  tree[, measuredItemChildFS := as.numeric(measuredItemChildFS)]
  suaData[, measuredItemFS := as.numeric(measuredItemFS)]
  
  ## Find levels so we can start at all parents and iterate down
  levels = getCommodityLevel(tree, parentColname = "measuredItemParentFS",
                             childColname = "measuredItemChildFS")
  topNodes = levels[level == 0, unique(node)]
  
  ## Restructure SUA data so elements become columns
  suaData = dcast.data.table(data = suaData, value.var = "Value",
                             formula = measuredItemFS ~ measuredElementFS)
  setnames(suaData, old=c("141", "51", "61", "71", "91"),
           new=c("food", "production", "imports", "stockVariation", "exports"))
  suaData[, supply := ifelse(is.na(production), 0, production) +
                      ifelse(is.na(imports), 0, imports) -
                      ifelse(is.na(exports), 0, exports) +
                      ifelse(is.na(stockVariation), 0, stockVariation)]
  suaData[, foodToSupply := ifelse(is.na(food), 0, food) / supply]

  ## Initialize the output
  out = data.table(measuredItemFS = unique(suaData$measuredItemFS),
                   mainFoodFlag = FALSE)
  
  ## Start loop down the tree
  candidates = topNodes
  selected = suaData[measuredItemFS %in% candidates & foodToSupply > threshold,
                     measuredItemFS]
  out[measuredItemFS %in% selected, mainFoodFlag := TRUE]
  notSelected = setdiff(candidates, selected)
  newCandidates = tree[as.numeric(measuredItemParentFS) %in% notSelected, ]
  childProduction = suaData[, c("measuredItemFS", "production"), with = FALSE]
  setnames(childProduction, c("measuredItemChildFS", "production"))
  newCandidates = merge(newCandidates, childProduction,
                        by = "measuredItemChildFS")
  newCandidates[, maxProduction := max(production, na.rm = TRUE),
                by = "measuredItemParentFS"]
  newCandidates[production == maxProduction, ]
  
  ## Return the final data.table
}