# Code adapted from a blogpost by Hadley Wickham

whitelist.examples = function() {
  cat.funs(list.base.computation.funs())
  cat.pkg.funs("stats")

  cat.pkg.funs("base")
  cat.pkg.funs("dplyr")

  wl.funs = parse.whitelist.yaml("D:/libraries/WhitelistEval/WhitelistEval/lists/StratTournWhiteList.yaml")

  txt = paste0("c(",paste0('"',wl.funs,'"',collapse=","),")")
  writeClipboard(txt)

  call = quote({
    eval(1+1)
    base::print
    y <<- 5
    print("Hi")
    filter(group_by(x,y), a==5)
    ggplot()
  })
  check.whitelist(call, wl.funs = wl.funs)


  # Let us perform some speed tests
  funs =find.funs(call)
  funs = rep(funs, times=10)

  library(microbenchmark)
  library(data.table)

  wl.funs = c(1:10000, wl.funs)

  funs[1] = "dfndgfzfirnfne"
  dt = data.table(wl.funs=wl.funs, ind=seq_along(wl.funs), key="wl.funs")
  microbenchmark(
    match(funs, wl.funs),
    funs %in% wl.funs,
    funs %chin% wl.funs,
    find.funs(call),
    dt[funs, ind],
    which(is.na(dt[funs, ind]))

  )
}



examples.check.whitelist = function() {


  dir = path.package("WhitelistEval")
  wl.funs = parse.whitelist.yaml(file.path(dir,"lists/StratTournWhiteList.yaml"))
  call = quote({
    eval(1+1)
    base::print
    y <<- 5
   # print("Hi")
    print("Ok")
    filter(group_by(x,y), a==5)
    ggplot()
  })



  check.whitelist(call, wl.funs = wl.funs)
  check.whitelist(call, bl.funs="print", wl.calls=alist(print=print("Ok")))

  li = as.expression(list(call, call))
  check.whitelist(li, wl.funs = wl.funs)
  check.whitelist(li, bl.funs="print", wl.calls=alist(print=print("Ok")))
}

#' Check whether a call only calls functions or uses variables that satify a whitelist of allowed symbols
#'
#' The function extracts in a nested fashion all names of called functions and used variables
#' in a call object. It can look up in whitelists whether these symbols are allowed or in
#' blacklists whether these symbols are forbidden. Whitelists and blacklists are simple
#' character vectors with the function / variable names.
#'
#' Returns a list (ok, fb.funs, fb.vars, msg) where ok is FALSE if a forbidden symbol
#' has been found and otherwise TRUE. fb.funs and fb.vars list the forbidden function calls
#' or variable names that have been found. msg is a default error message that can be shown
#' if the call did not pass the check.
#'
#' @param call the call object
#' @param wl.funs a character vector of the function names that are allowed (whitelisted). If NULL ignored.
#' @param wl.vars a character vector of the variable names that are allowed (whitelisted). If NULL ignored.
#' @param wl.calls a named list of quoted calls that are allowed.
#' The list names should be the call names, i.e. call[[1]].
#' For example, one may not generally whitelist the function 'library'
#' (who knows what can happen if a library has functions with the
#' same name than some whitelisted function but different behavior)
#' Yet one may allow the explicit call
#' `library(dplyr)`. In this case, we could set
#'  wl.calls = alist(library=library(dplyr)).
#' @param bl.funs a character vector of the function names that are forbidden (blacklisted). If NULL ignored.
#' @param bl.vars a character vector of the variable names that are forbidden  (blacklisted). If NULL ignored.
#' @export
check.whitelist = function(call, wl.funs=NULL,wl.vars=NULL, wl.calls=NULL, bl.funs=NULL, bl.vars=NULL) {
  #restore.point("check.whitelist")

  funs = robust.find.funs.except(call,ignore.calls=wl.calls)
  vars = robust.find.variables(call)


  if (!is.null(wl.calls)) {
    if (is.null(names(wl.calls))) stop("wl.calls must be a named list with the names equal to the function names of the calls. Call set.call.list.names on it first.")
  }

  fb.funs = NULL
  fb.vars = NULL
  msg = ""
  ok = TRUE

  if (!is.null(wl.funs)) {
    fb.funs = c(fb.funs,funs[which(! funs %in% wl.funs)])
  }
  if (!is.null(bl.funs)) {
    fb.funs = c(fb.funs,funs[which(funs %in% bl.funs)])
  }
  if (!is.null(wl.vars)) {
    fb.vars = c(fb.vars,vars[which(! funs %in% wl.vars)])
  }
  if (!is.null(bl.vars)) {
    fb.vars = c(fb.vars,vars[which(funs %in% bl.vars)])
  }

  if (length(fb.funs)>0) {
    ok = FALSE
    in.wl.calls = fb.funs %in% names(wl.calls)

    if (any(!in.wl.calls)) {
      msg = paste0(msg,"For security reasons, it is not allowed to call the following functions:\n",paste0(fb.funs[!in.wl.calls],collapse=", "),"\n")
    }
    if (any(in.wl.calls)) {
      msg = paste0(msg,"For security reasons, the following functions can only be called in a restriced way. You called them in a forbidden form:\n",paste0(fb.funs[in.wl.calls],collapse=", "),"\n")
    }
  }
  if (length(fb.vars)>0) {
    ok = FALSE
    msg = paste0(msg,"For security reasons, it is forbidden to access the following variable(s):\n",paste0(fb.vars,collapse=", "),"\n")
  }
  return(list(ok=ok, fb.funs = fb.funs, fb.vars=fb.vars, msg=msg))
}



#' A synoym for check.whitelist with different order of the arguments
check.blacklist = function(call, bl.funs=NULL,bl.vars=NULL, wl.funs=NULL, wl.vars=NULL, wl.calls=NULL) {
  check.whitelist(call=call, bl.funs=bl.funs, bl.vars=bl.vars, wl.funs=wl.funs, wl.vars=wl.var, wl.calls=wl.calls)
}

examples.find.forbidden.calls = function() {
  call = quote({
    print(abs(-2*5*log(abs(-1))))
  })
  find.forbidden.calls(call, bl.funs=c("abs"), wl.calls = alist(abs=abs(-1)))

}


#' Returns a list of forbidden calls
#'
#' Returns a list of all subcalls of call that are forbidden given the provided
#' whitelists and blacklists. Usually, it suffices to simply call check.whitelist.
#' This function is slower, put may be helpful if the exact sources of the violations
#' want to be known.
#'
#' @param call the call object
#' @param wl.funs a character vector of the function names that are allowed (whitelisted). If NULL ignored.
#' @param wl.vars a character vector of the variable names that are allowed (whitelisted). If NULL ignored.
#' @param wl.calls a named list of quoted calls that are allowed.
#' The list names should be the call names, i.e. call[[1]].
#' For example, one may not generally whitelist the function 'library'
#' (who knows what can happen if a library has functions with the
#' same name than some whitelisted function but different behavior)
#' Yet one may allow the explicit call
#' `library(dplyr)`. In this case, we could set
#'  wl.calls = alist(library=library(dplyr)).
#' @param bl.funs a character vector of the function names that are forbidden (blacklisted). If NULL ignored.
#' @param bl.vars a character vector of the variable names that are forbidden  (blacklisted). If NULL ignored.
#' @param check.white.list.res optional the returned object of a call to check.white.list. Can speed up find.forbidden.calls
#' @export

find.forbidden.calls = function(call, wl.funs=NULL, wl.vars=NULL, wl.calls=NULL,bl.funs=NULL,bl.vars=NULL, check.white.list.res=NULL) {

  # First find forbidden function names or variables in call
  if (is.null(check.white.list.res)) {
    res=check.whitelist(call, wl.funs=wl.funs, wl.vars=wl.vars, wl.calls = NULL, bl.funs=bl.funs, bl.vars=bl.vars)
  } else {
    res=check.whitelist.res
  }

  if (res$ok) return(NULL)

  calls = find.calls(call,fun.names = res$fb.funs,var.names = res$fb.vars)
  fb.calls = setdiff(calls, wl.calls)
  fb.calls
}