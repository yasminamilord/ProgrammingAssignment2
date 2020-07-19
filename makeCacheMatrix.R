makeCacheMatrix <- function(x = matrix()){
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverseMatrix <<- NULL
      }
      getValue <- function() {x}
      setInverseValue <- function(inverse2){inverse <<- inverse2}
      InverseValue <- function() {inverse}
      list(set = set, getValue = getValue, setInverseValue = setInverseValue, InverseValue = InverseValue)
      
}

cacheSolve <- function(x, ...){
      inverse <- x$InverseValue()
      if(!is.null(inverse)){
            message("Getting cached data!")
            return(inverse)
      }
      matrixData <- x$getValue()
      inverse <- solve(matrixData, ...)
      x$setInverseValue(inverse)
      inverse
      
}