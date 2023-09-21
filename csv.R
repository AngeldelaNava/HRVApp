csvSum <- function(A, B){
  return (c(A, B))
}

csvDownload <- function(df, fileName){
  write.csv(df, fileName)
}