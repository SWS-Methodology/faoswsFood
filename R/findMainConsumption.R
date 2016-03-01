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
##' supply ratio to indicate that a commodity is eaten?  This should be a 
##' numeric value between 0 and 1, and defaults to 0.8.
##' @param threshold2 Numeric.  If Imports > Exports, no production is required.
##'   But, if Prod > (I-X) * threshold2, we assign this commodity to a main 
##'   source of food.  Likewise, if Imports < Exports, production must be at 
##'   least Exports - Imports.  However, if Prod > (X-I) * (1 + threshold2), we 
##'   assign this commodity to a main food.  Note that this is only used after 
##'   the primary commodity has been identified as a non-food item (or at least 
##'   not the main source of food).  In the code, we calculate the ratio Prod / 
##'   (I-X) if Imports > Exports and Prod / (Exports - Imports) - 1 if Imports <
##'   Exports.  In this way, we can compare both ratios directly to threshold2.
##'   
##' @return A data.table with CPC codes and a logical value indicating whether 
##'   or not that commodity is the main commodity eaten.
##'   
##' @examples
##' \dontrun{
##' items = GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")[, code]
##' key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD", dimensions = list(
##'   Dimension(name = "geographicAreaFS", keys = "79"),
##'   Dimension(name = "measuredElementFS",
##'             keys = c("51", "61", "71", "91", "141")),
##'   Dimension(name = "timePointYears", keys = "2011"),
##'   Dimension(name = "measuredItemFS", keys = items)
##' ))
##' suaData = GetData(key)
##' tree = getCommodityTree(timePointYears = "2011")
##' tree = unique(tree[, c("measuredItemParentCPC", "measuredItemChildCPC"),
##'                    with = FALSE])
##' tree[, measuredItemParentFS := cpc2fcl(measuredItemParentCPC,
##'                                        returnFirst = TRUE)]
##' tree[, measuredItemChildFS := cpc2fcl(measuredItemChildCPC,
##'                                       returnFirst = TRUE)]
##' tree = tree[, c("measuredItemParentFS", "measuredItemChildFS"), with = FALSE]
##' findMainConsumption(tree, suaData)
##' }
##' 
##' @export
##' 

findMainConsumption = function(tree, suaData, threshold1 = 0.8, threshold2 = 0.2){
  ## Data Quality Checks Note: "<= 1" used instead of "== 1" because there are 
  ## cases where no geographicAreaFS or timePointYears columns will exist (for 
  ## example, when using those columns in a data.table "by" argument).  Using
  ## "<= 1" avoids the error because the length of this variable will be 0.
  stopifnot(length(unique(suaData$geographicAreaFS)) <= 1)
  stopifnot(length(unique(suaData$timePointYears)) <= 1)
  stopifnot(threshold1 > 0)
  stopifnot(threshold1 < 1)
  stopifnot(threshold2 > 0)
  if(nrow(suaData) == 0)
    return(NULL)

  ## Convert all commodity codes to numeric
  suaData = copy(suaData)
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
  for(code in c("141", "51", "61", "71", "91")){
    if(!code %in% colnames(suaData))
      suaData[, c(code) := 0]
  }
  setnames(suaData, old=c("141", "51", "61", "71", "91"),
           new=c("food", "production", "imports", "stockVariation", "exports"))
  ## If a value is missing, that means the true value can be assumed to be zero.
  suaData[, production := ifelse(is.na(production), 0, production)]
  suaData[, imports := ifelse(is.na(imports), 0, imports)]
  suaData[, exports := ifelse(is.na(exports), 0, exports)]
  suaData[, stockVariation := ifelse(is.na(stockVariation), 0, stockVariation)]
  suaData[, food := ifelse(is.na(food), 0, food)]
    
  ## Calculate supply and some ratios
  suaData[, supply := production + imports - exports + stockVariation]
  suaData[, foodToSupply := ifelse(is.na(food), 0, food) / supply]
  ## See documentation of threshold2 for the logic of this ratio
  suaData[, productionRatio := ifelse(imports > exports,
                                      production / (imports - exports),
                                      production / (exports - imports) - 1)]
  
  ## If no food or supply is available, we can't use that observation for
  ## anything.
  suaData = suaData[!(food == 0 & supply == 0), ]
  if(nrow(suaData) == 0)
      return(NULL)

  ## Initialize the output
  out = data.table(measuredItemFS = unique(suaData$measuredItemFS),
                   mainFoodFlag = FALSE)
  
  ## Find the top level commodities which are allocated to food
  candidates = topNodes
  selected = suaData[measuredItemFS %in% candidates & foodToSupply > threshold1,
                     measuredItemFS]
  out[measuredItemFS %in% selected, mainFoodFlag := TRUE]
  ## For all the commodities not seleceted, look at all their descendants.
  notSelected = setdiff(candidates, selected)
  newCandidates = getDescendants(tree = tree, parentColname = "measuredItemParentFS",
                                 childColname = "measuredItemChildFS")
  newCandidates = newCandidates[measuredItemParentFS %in% notSelected,
                                unique(measuredItemChildFS)]
  childProduction = suaData[measuredItemFS %in% newCandidates,
                            c("measuredItemFS", "productionRatio"),
                            with = FALSE]
  selectedChildren = childProduction[productionRatio > threshold2,
                                     measuredItemFS]
  out[measuredItemFS %in% selectedChildren, mainFoodFlag := TRUE]

  ## Return the final data.table
  return(out)
}