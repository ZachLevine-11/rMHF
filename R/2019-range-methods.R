#Iterate over the Met value and return TRUE if there is a dash in the string - indicating that we are dealing with a range
check_ifRange <- function(metValue){
  rangeCounter <- 1
  while (rangeCounter <= stringr::str_length(metValue)){
    letter <- substr(metValue, start = rangeCounter, stop = rangeCounter)
    if (letter == "-"){
      #Make sure that a single value followed by a "met-mins" unit is not interpreted as a range by checking the class of characters around the dash. If the characters can't be converted to integers, then the dash is part of a "met-min" tag and should be treated as a value.
      theDashIndex <- gregexpr(pattern = "-", metValue)[[1]][1]
      letterBefore <- substr(metValue, start = theDashIndex - 1, stop = theDashIndex - 1)
      letterAfter <- substr(metValue, start = theDashIndex + 1, stop = theDashIndex + 1)
      #Add a secondary check, because on a few occasions, the range values are put in with a space around the dash. Let's make sure those ranges aren't interpreted as values.
      twoLettersBefore <- substr(metValue, start = theDashIndex - 2, stop = theDashIndex - 2)
      twoLettersAfter <- substr(metValue, start = theDashIndex + 2, stop = theDashIndex + 2)
      primaryCheck <- !is.na(as.integer(letterBefore)) & !is.na(as.integer(letterAfter))
      secondaryCheck <- !is.na(as.integer(twoLettersBefore)) & !is.na(as.integer(twoLettersAfter))
      #If either the characters immediately around the dash are an integer, or the values two characters away are integers (can be converted to an integer without producing a NA value), this value is a range, and we should treat it accordingly.
      IsaTrueRange <- primaryCheck | secondaryCheck
      if (IsaTrueRange){
        return(TRUE)
      }
      else{
        return(FALSE)
      }
    }
    else{
      rangeCounter <- rangeCounter + 1
    }
  }
  return(FALSE)
}
#Extract the range from a MET range column, and return it as an integer atomic vector of the form c(upper, lower).
extractRange <- function(metValue){
  dashIndex <- gregexpr(pattern = "-", metValue)[[1]][1]
  rangeLower <- substr(metValue, start = 1, stop = dashIndex - 1)
  rangeUpper <- substr(metValue, start = dashIndex + 1, stop = str_length(metValue))
  #The upper range will include the "Met" unit we do not want, so let's get rid of that part.
  rangeUpper <- justValue(rangeUpper)
  #Just to be safe, we'll call justValue on the lower range too.
  rangeLower <- justValue(rangeLower)
  return(c(rangeLower, rangeUpper))
}
