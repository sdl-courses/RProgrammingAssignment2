## author: stefano de luca
## date: 2016-02-17

## this source file is for the assignment #2 of the Coursera course "R Programming"
## assignment page: https://goo.gl/Qe5Qnl 


## this function computes the inverse of a matrix
## using the R function "solve"
## because "solve" require time-consuming computation, 
## our function uses a caching mechanism to evitate computation duplication



## makeCacheMatrix
## create a special vector containing the functions needed to cache computation
## it is a sort of simple object oriented programming
## but it is really a "wrapper" around the x matrix

makeCacheMatrix <- function(x = matrix()) {
    
    solved <- NULL  #  this is the cached solution
    
    set <- function(y) {
        x <<- y         # assing y to the outer environment, i.e., that of makeCacheMatrix
        solved <<- NULL # reset a not synchronized cached result, because x changed
    }
    
    get <- function() x  # return the original matrix
    
    setSolved <- function(x) solved <<- x # put the computation result in the cached solution
    
    getSolved <- function() solved # return the cached solution
    
    list(set = set,   # list of function to access these functions
         get = get,   # sort of publishing methods in object oriented programming
         setSolved = setSolved,
         getSolved = getSolved
    )
}


## cacheSolve 
## Return a matrix that is the inverse of 'x'v

## this function compute the inverse matrix with cache
## the first time you call cacheSolve() with a matrix, it compute the inverse
## and store the results in a private variable
## the next calls will reuse the first calculation just reading the private variable

cacheSolve <- function(x, ...) {
    
    invertedMatrix <- x$getSolved()  # try to get the cached solution
    
    if(!is.null(invertedMatrix)) {    
        # yes, the cached solution is not null, then it yet exists!!!
        message("getting cached data")
        return(invertedMatrix)
    }
    
    # if we are here, no cached solution existed
    
    data <- x$get()   # get the original data wrapped by x-matrix
    invertedMatrix <- solve(data, ...)  # generate the solution
    x$setSolve(invertedMatrix)  # put the solution inside the wrapped x-matrix 
    invertedMatrix  # return the solution
}


## test
## test function to verify that cacheSolve is correct
## to be correct, cacheSolve must inverse a matrix using caching


test <- function() {
    # function to create an invertible matrix
    hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
    
    # create a matrix 8x8 
    h8 <- hilbert(8)
    
    # create the caching version of the matrix
    
    cached_h8 <- makeCacheMatrix(h8)

    # call 3 times cacheSolve
    for (i in 1:3) {   
        sh8 <- cacheSolve(cached_h8)
    }
    
    # verify that sh8 is the inverse of h8
    # multiplying sh with h8 (%*% is the operator for matrix multiplication)
    # round() is used to remove the minimal differences
    
    
    result <- round(sh8 %*% h8)
    
    # if it is all ok, the result is a matrix with all 0s and 1s on the diagonal
    print(result)
}

# execute the test
test()

# end
