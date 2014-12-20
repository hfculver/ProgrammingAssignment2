## The R script "cachematrix.R" was built to complete the programming assignment
## for R programming course, ProgrammingAssignment2.
## Created: 12/20/2014, by H. Culver

## Function: makeCacheMatrix
## Purpose: Create a list of functions to store and retrieve
##          a matrix and its inverse. Use the global envirtonment
##          to persist the matrices.

makeCacheMatrix <- function(xm = matrix()) {
    si <- NULL     # si - some inverse...
    # Save (set) a matrix...
    set <- function(y) {
        xm <<- y
        si <<- NULL
    }

    # Retrieve (get) a stored matrix...
    get <- function() xm
    
    # Save (set) the inverse of a stored matrix...
    setinv <- function(inv) si <<- inv
    
    # Retrieve (get) the inverse of a stored matrix...
    getinv <- function() si

    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function: cacheSolve
## Purpose: Either retrieve a cached version of the inverse of a matrix
##          or generate and store the inverse of a matrix. The inverse
##          of the matrix is returned in either case.
## Depends on: an object created by the "makeCacheMatrix" function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    si <- x$getinv()   # attempt to get matrix inverse...

if(!is.null(si)) {   # if matrix inverse exists...
        message("getting cached data")
        return(si)
    }

## When matrix inverse does not already exist...
    data <- x$get()
    si <- solve(data)
    x$setinv(si)
    si
}

# Test run procedure:
#> xm1=matrix(c(1:3,5),nrow=2,ncol=2)
#> zm<-makeCacheMatrix(xm1)
#> zm$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    5
#> cacheSolve(zm)
#[,1] [,2]
#[1,]   -5    3
#[2,]    2   -1
#> cacheSolve(zm)
#getting cached data
#[,1] [,2]
#[1,]   -5    3
#[2,]    2   -1
#>