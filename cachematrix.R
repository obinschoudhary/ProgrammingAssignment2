## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function (m = matrix()) {
    
    ## Initialize the inverse property
    i <- NULL
    
    ## Method to set the matrix
    set <- function(matrix){
        m <<- matrix
        i <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() m
    
    ## Method to set the inverse of the matrix
    setMatInv <- function(MatInverse) i <<- MatInverse
    
    ## Method to get the inverse of the matrix
    getMatInv <- function() i
    
    ## Return a list of the methods
    mypOptMatrix <<- list(get=get,setMatInv=setMatInv,getMatInv=getMatInv)
    mypOptMatrix
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x = myOptMatrix, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    MatInverse<- x$getMatInv()
    
    ## Return the inverse if its already set    
    if(!is.null(MatInverse)){
        message("getting cached matrix inverse")
        return(MatInverse)    
    }
    
    ## Get the matrix from our object
    DataMatrix<-x$get()
    
    ## Calculate the inverse using matrix multiplication
    MatInverse<-solve(DataMatrix,...)
    
    ## Set the inverse to the object
    x$setMatInv(MatInverse)
    MatInverse}
