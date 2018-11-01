## Hi Everyone, I'm currently taking the R Programming course in the John's
## Hopkins Data Science Specialization on Coursera.
## This is the 3rd week's course assignment and is due on Oct 28th, 2018.
## Unfortunately, life happened, and I was late in submitting this assignment.
## The goal of the assingment is to cache time-consuming computations.

## Write a short comment describing this function - The goal of the first function
## is to create a special matrix object that can cache its invesrse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(a) {
        x <<- a
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    
    ## This is where I set up the inverse of matrix after I initialize the inverse and get the matrix above
    setinverse <- function(inverse) {
        inverse <<- inverse
    }
    
    ## This gets the inverse of the matrix
    getinverse <- function() {
        inverse
    }
    
    list(set = set
    , get = get
    , setinverse = setinverse
    , getinverse = getinverse)
}


## Write a short comment describing this function - The goal of the second
## function is to compute the inverse of the special Cache matrix from the
## above function with a condition on when to retrieve the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
