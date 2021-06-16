makeCacheMatrix <- function(a= matrix()){
  inv <- NULL
  set <- function(b){
    a<<-b
    inv<<-NULL
  }
  get <- function() {a}
  setinv <-function(inverse){inv <<- inverse}
  getinv <-function(inv)
    list(set= set, get = get,setinv = setinv,getinv = getinv)
}
cacheSolve <- function(a,...){
  inv<-a$getinv()
  if(!is.null(inv)){
    message("Retreving cached data ")
    return(inv)
  }
  mat <- a$get()
  inv <- solve(mat,...)
  a$setinv(inv)
  inv
}