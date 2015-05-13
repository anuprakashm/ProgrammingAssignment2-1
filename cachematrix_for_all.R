makeCacheMatrix <- function(mat = matrix()) {
  invmat <- NULL
  det <- NULL
  rowcol <- NULL
  
  setmat <- function(temp) {
    mat <<- temp
    invmat <<- NULL
    det <<- NULL
    rowcol <<- NULL
  }
  
  getmat <- function() mat
  
  setrowcol <- function(temp) rowcol <<- temp
  
  getrowcol <- function() rowcol
  
  setdet <- function(temp) det <<- temp
  
  getdet <- function() det
  
  setinv <- function(inverse) invmat <<- inverse
  
  getinv <- function() invmat
  
  list(setrowcol = setrowcol, getrowcol = getrowcol ,setdet= setdet, getdet=getdet, setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
} 


cacheSolve <- function(mat) {
  
  detmat <- mat$getdet()
  if (!is.null(detmat)) {
    if (detmat == 0) {
      message("Cached message: The matrix is non invertible since determinant is zero")
      return(matrix())
    }
  }
  
  rowcolmat <- mat$getrowcol()
  if (!is.null(rowcolmat)) {
    if (rowcolmat == FALSE) {
      message("Cached message: The matrix is non invertible since row numbers not equal to column numbers")
      return(matrix())
    }
  }
  
  invmat <- mat$getinv()
  if (!is.null(invmat)) {
    message("Getting matrix inverse from cache")
    return(invmat)
  }
  
  
  matdata <- mat$getmat()
  
  if (nrow(matdata) != ncol(matdata)) {
    message("The matrix is non invertible since row numbers not equal to column numbers")
    mat$setrowcol(FALSE)
    return(matrix())
  }
  mat$setrowcol(TRUE)
  
  detdata <- det(matdata)
  mat$setdet(detdata)
  if (detdata == 0) {
    message("The matrix is non invertible since determinant is zero")
    return(matrix())
  }
  
  invmat <- solve(matdata)
  mat$setinv(invmat)
  return(invmat)
}
