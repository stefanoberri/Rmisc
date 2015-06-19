# this function retrieves, from a working R installation, all the packages names and
# versions in depends and import. The packages must be installed on the running
# R session

getPkgsTree <- function(listOfPkgs,
  repos="http://mirrors.ebi.ac.uk/CRAN/src/contrib"){
  # listOfPkgs is a vector with package name (i.e. c('ggplot2', 'RMySQL'))
  # repos is one or more repositories used by packageStatus() to retrieve
  # information on available packages

  # helping functions
  namesOnly <- function(string){
    # split on ',\s'
    messyPkgs <- strsplit(string, ',\\s*', perl=TRUE)
    pkgsNames <- lapply(messyPkgs[[1]], function(x){return(strsplit(x, '\\s+')[[1]][1])})
    return(unlist(pkgsNames))
  }

  getDependencies <- function (packageName, PS){
    # recursive function
    # packageName is name of ONE package
    # PS is the object returned by packageStatus()
    # returns a vecotr of package names
    pkgIndex <-  match(packageName, names(PS$inst$Package))  
    if (is.na(pkgIndex)){
      stop(paste("Package", packageName, "is not installed. Aborting"))
    }
    dependencies <- c(namesOnly(PS$inst$Depends[pkgIndex]), namesOnly(PS$inst$Imports[pkgIndex]))
    dependencies <- dependencies[!is.na(dependencies)] 
    
    # don't need the base packages
    pkgID <- match(dependencies, PS$inst$Package)
    isBase <- PS$inst$Priority[pkgID] == "base"
    isBase[is.na(isBase)] <- FALSE
    
    # take out 'R'
    cleanDeps <- dependencies[!isBase & dependencies != 'R']
    
    # let's recurse 
    if (length(cleanDeps) == 0){
      # no more dependencies. We terminate returning package name
      return(packageName)
    } else {
      # recurse
      deps <- unlist(lapply(cleanDeps, getDependencies, PS))
      allDeps <- unique(c(deps, packageName))
      return (allDeps)
    }
  }

  # main
  message("retrieving package status...")
  PS <- packageStatus(repositories = repos)
  
  # recursively find packages (names only)
  allPakgs <- unique(unlist(lapply(listOfPkgs, getDependencies, PS)))
  allPakgsID = match(allPakgs, PS$inst$Package)
  colsOfInterest <- c("Package", "Version")
  colID <- match(colsOfInterest, names(PS$inst))
  df <- PS$inst[allPakgsID, colID]

  return(df)
}





