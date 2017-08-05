## 
## The cachematrix.R file contains two functions, makeCacheMatrix() and cacheSolve(). 
## They are used to calculate the inverse of a matrix and cache it so that
## the next time the matrix inverse is needed the cached value is returned instead of 
## calculating in again.
##
## Example of using both functions:
##
## > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## > m1
##       [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
##
## > myMatrixObject <- makeCacheMatrix(m1)  
##      
##  myMatrixObject contains the matrix m1 and its inverse after the first call to cacheSolve
## > cacheSolve(myMatrixObject)  # calculate the inverse of m1 and store it 
##                                 in the environment of myMatrixObject
##       [,1] [,2]
## [1,]    6    8
## [2,]    2    4 
## 
## If we call cacheSolve again the cached value of the inverse matrix is returned
## > cacheSolve(myMatrix_object)
## getting cached data
##       [,1] [,2]
## [1,]    6    8
## [2,]    2    4
##
##
## The function makeCacheMatrix() creates an R object that stores a matrix and its inverse
## In addition, it returns a list of four functions (getters and setters)
## that allow to set and get the values of the matrix and its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseMatrixToBeCached) inverseMatrix  <<- inverseMatrixToBeCached
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

##
## The function cacheSolve() requires an argument that is returned by makeCacheMatrix(). 
## It is used to populate and retrieve the matrix inverse from an object of type makeCacheMatrix()
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve retrieves the inverse from the makeCacheMatrix() object. 
## Otherwise it calculates the inverse of the matrix and stores it in the makeCacheMatrix() object.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverseMatrix()
    if (!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
