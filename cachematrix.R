# R-PROGRAMMING #2, Coursera Johns Hopkins
# Jeff Muday
# jefmud@gmail.com
# rprog-035
#
# Given that inverting matrices [R solve() function] is computationally expensive,
# Lexical scoping in R can be leveraged to 
# create a matrix object which can
# cache an inverse when it is first called upon...
# and returns the cached value of the "solve" on subsequent calls
#
# Programming Task: create 2 functions
# makeCacheMatrix(<user_defined_numeric_matrix>) - returns a CacheMatrix object, but does not use solve()
#
# cacheSolve(<user_defined_CacheMatrix_type>) - returns a regular inverted solution
#   cacheSolve STORES the first inverted result, and returns a CACHED matrix on subsequent calls
#
# example usage:
# test_matrix <- matrix(rnorm(9),3,3)
# my_cached_matrix <- makeCacheMatrix(test_matrix)
# my_cached_inverted_matrix <- cacheSolve(my_cached_matrix)
# testing funtion
# my_unit_test() - creates a well-known 3x3 matrix and then a CacheMatrix object, and then "solves"
#                  a first-pass, and checks the value, then a second-pass which is CACHED


# makeVector(<user_defined_numeric_vector) - returns a cacheVector object
# example code given to us by instructor
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# cachemean(<user_defined_cacheVector>) - returns a numeric mean from cacheVector object
# example code given to us by an instructor
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

makeCacheMatrix <- function(x=matrix()) {
  # makeCacheMatrix(<user_defined_numeric_matrix>) - returns a CacheMatix
  # loosely based on the ideas of makeVector of R. Peng
  # member functions set(), get(), getinverse(), setinverse()
  m <- NULL # initialize local 'm' matrix to NULL
  set <- function(y) {
    # member function SET
    x <<- y # x set from 'y' in parent environment
    m <<- NULL # set m in parent environment to NULL
  }
  get <- function() {
    # member function GET simply returns current data (matrix type)
    x
  }
  setinverse <- function(solve) {
    # computes the inverse, called when m is NULL (which means to solve and CACHE)
    m <<- solve
  }
  getinverse <- function() {
    # if m is NULL, solve is not cached
    # if m is NOT NULL, then solve is cached
    m
  }
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # cacheSolve(<user_defined_cacheMatrix>) - returns a SOLVE (matrix inversion)
  # data - the original matrix from the cacheMatrix
  # m - member data which is NULL, requires a SOLVE be computed on data, otherwise returns value from cache
  # get the inverse from the object
  m <- x$getinverse()
  if (!is.null(m)) {
    # if not null SOLVE results are already cached
    message("getting cached matrix")
    return(m)
  }
  # GET data from the object x
  data <- x$get()
  # compute the inverse of the matrix via SOLVE function
  m <- solve(data, ...)
  # object x now sets (caches) the inverse
  x$setinverse(m)
  # return the SOLVE that was computed above
  m
}

my_unit_test <- function() {
  # this function simply tests if we can invert a
  # well-known integer-based matrix, proven invertable
  mat_orig <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)
  mat_inverse <- solve(mat_orig)
  c_mat_orig <- makeCacheMatrix(mat_orig) # cached version
  c_mat_inverse1 <- cacheSolve(c_mat_orig)
  if (all.equal(c_mat_inverse1,mat_inverse)) {
    print("Passes 3x3 test, UNCACHED PASS")
  } else {
    print("FAILS first PASS")
  }
  # check if "getting cached matrix" prints
  c_mat_inverse2 <- cacheSolve(c_mat_orig)
  if (all.equal(c_mat_inverse2,mat_inverse)) {
    print("Passes 3x3 test, CACHED PASS")
  } else {
    print("FAILS CACHED PASS")
  }
}

# run the test
my_unit_test()

