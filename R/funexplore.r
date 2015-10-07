nested.find.fun.funs = function(fun.name, env=new.env(), existing = ls(env), as.list=TRUE) {
  #restore.point("nested.add.fun.funs")

  if (length(fun.name)>1) {
    for (fn in fun.name) {
      nested.find.fun.funs(fn, env, as.list=FALSE)
    }
   if (as.list) return(as.list(env))
   return(env)

  }

  funs = find.fun.funs(fun.name)
  env[[fun.name]] = funs
  new = setdiff(funs, c(existing, fun.name))
  for (child.fun in new) {
    existing = c(ls(env),new)
    if (exists(child.fun)) {
      nested.find.fun.funs(child.fun, env, existing=existing, as.list=FALSE)
    } else {
      # Internal function that is not available
      env[[child.fun]] = NA_character_
    }
  }
  if (as.list) return(as.list(env))
  env
}

funs.call.graph = function(fun.name, env=new.env()) {
  funs.li = nested.find.fun.funs(fun.name)

  library(igraph)
  funs = names(funs.li)

  g <- graph.empty(directed=TRUE) + vertices(funs)
  i = 1
  for (i in seq_along(funs)) {
    called = funs.li[[i]]
    if (length(called)==0) next
    if (any(is.na(called))) next
    called.ind = match(called,funs)

    # need better treatment
    called.ind = na.omit(called.ind)

    g[from=rep(i, length(called.ind)),to=called.ind] <- TRUE
  }
  list(g=g, li = funs.li)
}

find.fun.funs = function(fun, lib = NULL, penv = parent.env(.GlobalEnv), truncate.pkg=FALSE) {
  #restore.point("find.fun.funs")

  if (is.character(fun)) {
    if (!exists(fun, penv)) return(NULL)

    fun=get(fun,envir = penv)

    if (!is.function(fun)) {
      return(NULL)
    }
  }
  li = c(formals(fun), list(body(fun)))
  res = robust.find.funs(li)
  if (is.null(res)) res = character(0)
  if (truncate.pkg) res = stringtools::str.right.of(res,"::")
  res
}

find.function.packages = function(fun.name) {
  require(stringtools)
  libs = sapply(findFunction(fun.name), function(el) attr(el,"name"))
  rows = str.starts.with(libs,"package:")
  str.right.of(libs[row],"package:")
}
