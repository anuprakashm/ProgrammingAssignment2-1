## There are two functions created to cache the inverse of the matrix. The first function (makeCacheMatrix()) is to create a 
## vector of functions to be used to set a matrix and other functions to be used by the second function(cacheSolve()) to 
## create and cache the inverse of the matrix. The result for matrix inverse will be stored in cache when run first and from 
## the next run the inverse will be pulled from cache until the matrix to be inversed is updated using the setmat function. 
## Appropriate message will be displayed if the matrix is not invertible. 

## The functions can be used as follows:
## 1 ) Source the functions R script containing the functions makeCacheMatrix() and cacheSolve(). For example, If cachematrix.R
##     is the R scirpt containing the functions, then execute source("cachematrix.R") proveded the script is in working 
##     directory.
## 2 ) Assign the makeCacheMatrix() function to a vector, say matrix. matrix <- makeCacheMatrix()
## 3 ) Now the functions in makeCacheMatrix() will be stored in the list called matrix
## 4 ) We need to set the matrix to be inverted using the function matrix$setmat() with the matrix as argument
## 5 ) Now cacheSolve() function can be used to find the inverse with argument as matrix, cacheSolve(matrix)
## 6 ) For the first run of cacheSolve() the inverse of the matrix will be calculated and from the next run onwards the 
##     inverse will be taken from cache until a new matrix is set.
## 7 ) If the matrix is not invertible then appropriate message will be displayed whether it is due to non-square matrix or
##     due to zero determinant matrix.  These verification of the matrix will also be cached until the a new matrix is set.

## makeCacheMatrix() creates the list of following functions:
## 1 ) setmat() -> This function will set the matrix
## 2 ) getmat() -> This function will get the matrix
## 3 ) setrowcol() -> This will set the logical value whether the number of rows and columns in matrix are matching or not
## 4 ) getrowcol() -> This will get the logical value whether the number of rows and columns in matrix are matching or not
## 5 ) setdet() -> This function will set the determinant of the matrix
## 6 ) getdet() -> This function will get the determinant of the matrix
## 7 ) setinv() -> This function will set the inverse of the matrix using the function solve()
## 8 ) getinv() -> This function will get the inverse of the matrix

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

## cacheSolve() function does the following:
## Get the cached determinant using the getdet() function in makeCacheMatrix().  If the deteminant is zero then display
## cahced message and retun matrix()
## Get the getrowcol() function in makeCacheMatrix() which says the matrix is a square matrix or not. If not square matrix
## then display the cached message and return matrix()
## Get the cached inverse matrix using getinv() function in makeCacheMatrix(). If the cahced inverse is not NULL then 
## return the inverse matrix
## Get the matrix using the getmat() function in makeCacheMatrix(). Check whether it is a square matrix, if yes set 
## setrowcol(TRUE) else set setrowcol(FALSE) and diplay appropriate message and return matrix()
## Check the determinant of the matrix using det() function. Set the determinant using setdet() function in makeCacheMatrix()
## If determinant is zero then diplay appropriate message and return matrix()
## Find the inverse of the matrix using solve(). Set inverse using setinv() function in makeCacheMatrix() and retun the 
## matrix inverse

cacheSolve <- function(mat) {
  
  detmat <- mat$getdet()
  if (detmat == 0) {
      message("Cached message: The matrix is non invertible since determinant is zero")
      return(matrix())
  }
  
  rowcolmat <- mat$getrowcol()
  if (rowcolmat == FALSE) {
      message("Cached message: The matrix is non invertible since it is not a square matrix")
      return(matrix())
  }
  
  invmat <- mat$getinv()
  if (!is.null(invmat)) {
    message("Getting matrix inverse from cache")
    return(invmat)
  }
  
  matdata <- mat$getmat()
  
  if (nrow(matdata) != ncol(matdata)) {
    message("The matrix is non invertible since it is not a square matrix")
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
