# findCritDepths ----
## Find the interpolated depths at critical DO and Temp
findCritDepths <- function(d,depth,xDO,xT,critDO=6,critT=22.8) {
  # Fancy code so that variabales don't have to be quoted when calling function
  depth <- eval(substitute(depth),envir=d)
  xDO <- eval(substitute(xDO),envir=d)
  xT <- eval(substitute(xT),envir=d)

  # Find depth at critical DO (depth where DO is first less than critDO)
  if (all(xDO>=critDO)) depthDO <- max(depth)
  else {
    # Position of shallowest depth where DO is less than critDO ...
    cl <- which.max(xDO<=critDO)
    # ... and position of depth just above (shallower) cl
    cl <- c(cl,cl-1)
    # Linearly interpolate between the two points
    cfs <- coef(lm(depth[cl]~xDO[cl]))
    depthDO <- cfs[["(Intercept)"]]+cfs[[2]]*critDO
  }

  ## Find depth at critical temp
  if (all(xT<=critT)) depthT <- min(depth)
  else {
    # Position of shallowest depth where temp is more than critTemp ...
    cl <- which.min(xT>=critT)
    # ... and position of depth just below (deeper) cl
    cl <- c(cl,cl-1)
    # Linearly interpolate between the two points
    cfs <- coef(lm(depth[cl]~xT[cl]))
    depthT <- cfs[["(Intercept)"]]+cfs[[2]]*critT
  }
  ## Put it all together in a data.frame to return
  tibble(critDO,depthDO,critT,depthT,CLT=abs(depthDO-depthT))
}


# cltByDay
## Get CLT "width" for each lake and date
cltByDay <- function(d) {
  # Must get unique lakes and dates in vectors for map2_df below
  #   see http://www.rebeccabarter.com/blog/2019-08-19_purrr/
  lake_dates <- d %>% distinct(Lake,Date)
  lakes <- lake_dates$Lake
  dates <- lake_dates$Date
  # purr to get the results by lake and date
  tmp<- map2_df(.x=lakes,.y=dates,
                .f=~{
                  d %>%
                    filter(Lake==.x,Date==.y) %>%
                    findCritDepths(Depth,mnDO,mntemp) %>%
                    mutate(Lake=.x,Date=.y) %>%
                    select(Lake,Date,critDO,depthDO,critT,depthT,CLT)
                }
  )
  # return the result
  tmp
}
