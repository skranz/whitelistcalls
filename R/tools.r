wbl.to.yaml =  function(funs, package) {
  paste0(package,": ",vector.to.yaml(funs))

}


vector.to.yaml = function(vec, width=80, indent = 4, quote.char=if (is.character(vec)) "'" else "") {


  res = stringi::stri_wrap(paste0(quote.char,vec,quote.char, collapse=", "),width = 70,indent = indent,exdent = indent)
  mline = ifelse(length(res)>2,"\n","")
  paste0("[",mline,paste0(res, collapse="\n"),mline,"]")
}



examples.set.call.list.names = function() {
  set.call.list.names(alist(print(5), x[5], x))

}

set.call.list.names = function(call.list) {
  #restore.point("set.call.list.names")
  names = lapply(call.list, function(call) {
    if (length(call)>1) return(call[[1]])
    as.character(call)
  })
  names(call.list) = names
  call.list
}


call.list.to.call = function(li) {
  names(li) = NULL
  do.call("call", c("{",li))

}


#' Find function calls inside a call object but ignore subcalls listed in ignore.calls
#'
#' @param call the call to be analysed
#' @param ignore.calls a named list of quoted calls. The names must be the function names of the call. Alternatively, ignore.names can be set
#' @param ignore.names just the function names of the ignore.calls
find.funs.except = function(call, ignore.calls=NULL, ignore.names=names(ignore.calls)) {
  if (is.null(ignore.calls)) return(find.funs(call))


  if (!is.call(call)) return(NULL)
  fun.name = as.character(call[1])

  rows = ignore.names == fun.name
  ignore = any(sapply(ignore.calls[ignore.names == fun.name], identical,y=call))
  if (ignore) return(NULL)

  sub.names = lapply(call[-1], function(e1) {
    find.funs.except(e1, ignore.calls=ignore.calls, ignore.names=ignore.names)
  })
  names = unique(c(fun.name,unlist(sub.names, use.names=FALSE)))
  names
}

#' Like find.funs.except but also works if call is a (nested) list of calls or an expression
robust.find.funs.except = function(call, ignore.calls=NULL, ignore.names=names(ignore.calls)) {
  if (is.expression(call))
    call = as.list(call)
  if (is.list(call)) {
    return(unique(unlist(lapply(call,robust.find.funs.except,ignore.calls=ignore.calls, ignore.names=names(ignore.calls) ))))
  }

  find.funs.except(call, ignore.calls=ignore.calls,ignore.names = ignore.names)
}

#' Like find.funs but also works if call is a (nested) list of calls or an expression
robust.find.funs = function(call) {
  if (is.expression(call))
    call = as.list(call)
  if (is.list(call)) {
    return(unique(unlist(lapply(call,robust.find.funs ))))
  }
  find.funs(call)
}

#' Like find.variables but also works if call is a (nested) list of calls or an expression
robust.find.variables = function(call) {
  if (is.expression(call))
    call = as.list(call)
  if (is.list(call)) {
    return(unique(unlist(lapply(call,robust.find.variables ))))
  }
  find.variables(call)
}
