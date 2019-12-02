makeCacheMatrix <- function(m=matrix()) {
  i<- NULL
  get<- function() m
  set<- function(y){
    m<<- y
    i<<-NULL
  } 
  setInverse<- function(inverse) i<<- inverse
  getInverse<- function () i
  list(get=get,set=set,getInverse=getInverse,setInverse=setInverse)
}

cacheSolve <- function(x,...){
  i<-x$getInverse()
  if(!is.null(i)){
    print("getting cache data")
    return(i)
  }
  data<- x$get()
  i<- solve(data)
  x$setInverse(i)
  i
}

