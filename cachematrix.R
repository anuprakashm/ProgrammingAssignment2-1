## There are two functions created to cache the inverse of the matrix. The first function (makeCacheMatrix()) is to create a 
## vector of functions to be used to set a matrix and other functions to be used by the second function(cacheSolve()) to 
## create, cache the inverse and return the inverse of the matrix. The result for matrix inverse will be stored in cache when 
## run for first time and from the next run the inverse will be pulled from cache until the matrix to be inversed is updated 
## using the setmat function. 
## This functions will work only for invertible matrix. Invertible matrix check is included in the R source
## "cachematrix_advanced.R" present in the same github folder.

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

## makeCacheMatrix() creates the list of following functions:
## 1 ) setmat() -> This function will set the matrix
## 2 ) getmat() -> This function will get the matrix
## 3 ) setinv() -> This function will set the inverse of the matrix using the function solve()
## 4 ) getinv() -> This function will get the inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
  invmat <- NULL
  
  setmat <- function(temp) {
    mat <<- temp
    invmat <<- NULL
  }
  getmat <- function() mat
  setinv <- function(inverse) invmat <<- inverse
  getinv <- function() invmat
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
} 

## cacheSolve() function does the following:
## Get the cached inverse matrix using getinv() function in makeCacheMatrix(). If the cahced inverse is not NULL then 
## return the inverse matrix
## Find the inverse of the matrix using solve(). Cache inverse using setinv() function in makeCacheMatrix() and retun the 
## matrix inverse

cacheSolve <- function(mat, ...) {
  
  invmat <- mat$getinv()
  if (!is.null(invmat)) {
    message("Getting matrix inverse from cache")
    return(invmat)
  }
  matdata <- mat$getmat()
  invmat <- solve(matdata, ...)
  mat$setinv(invmat)
  return(invmat)
}
