stats <- function(value, stat){
  
  if (stat == "mean"){
    return(list(SE_gl = unlist(lapply(lapply(value, '[[', 1L), mean)), SE_mb = unlist(lapply(lapply(value, '[[', 2L), mean)),
                SPRING = unlist(lapply(lapply(value, '[[', 3L), mean)), HARMONIES = unlist(lapply(lapply(value, '[[', 4L), mean)),
                gCoda = unlist(lapply(lapply(value, '[[', 5L), mean)), COZINE = unlist(lapply(lapply(value[-1], '[[', 6L), mean))))
  }
  
  if (stat == "sd"){
    return(list(SE_gl = unlist(lapply(lapply(value, '[[', 1L), sd)), SE_mb = unlist(lapply(lapply(value, '[[', 2L), sd)),
                SPRING = unlist(lapply(lapply(value, '[[', 3L), sd)), HARMONIES = unlist(lapply(lapply(value, '[[', 4L), sd)),
                gCoda = unlist(lapply(lapply(value, '[[', 5L), sd)), COZINE = unlist(lapply(lapply(value[-1], '[[', 6L), sd))))
  }
  
  if (stat == "median"){
    return(list(SE_gl = unlist(lapply(lapply(value, '[[', 1L), median)), SE_mb = unlist(lapply(lapply(value, '[[', 2L), median)),
                SPRING = unlist(lapply(lapply(value, '[[', 3L), median)), HARMONIES = unlist(lapply(lapply(value, '[[', 4L), median)),
                gCoda = unlist(lapply(lapply(value, '[[', 5L), median)), COZINE = unlist(lapply(lapply(value[-1], '[[', 6L), median))))
  }
  
}