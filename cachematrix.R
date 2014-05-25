## The functions here allow caching the matrix inverse computed, thus avoiding 
## unnecessary resrouce-intensive matrix inversion calculations if the original 
## input/matrix has not changed

## makeCacheMatrix creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        M <- NULL # sets default inverse matrix value (solution) to NULL
        set <- function(y) { # function to change matrix to be solved
                x <<- y
                M <<- NULL # when applicable, clears previously cached solution
        }
        get <- function() x # function that returns matrix inputted to be solved
        setsolve <- function(solve) M <<- solve # sets value of matrix to solve
        getsolve <- function() M
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve takes as input a list 'x' created by makeCacheMatrix with a matrix 
## to be solved and, if previously solved, the inverse of the matrix; cacheSolve
## checks whether the inverse of the matrix has been stored in the list before
## recalculating, if there is, it returns cached value, otherwise it calculates
## and caches the solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        M <- x$getsolve() # retrieves stored matrix solution for matrix
        if(!is.null(M)) { # checkes if retrieved solution value is empty
                message("getting cached data")
                
                return(M) #returns cached matrix inverse if applicable
        }
        data <- x$get() # sets matrix value to be solved by using x$get()
        M <- solve(data, ...) # calculates matrix x inverse and stores in M
        x$setsolve(M) # caches M (matrix x inverse)
        M #returns matrix x inverse
}