find.fun.pkg.from.pkg = function(fun.name, pkg, pkg.env = asNamespace(pkg)) {
  library(stringtools)
  if (has.substr(fun.name,"::")) {
    if (has.substr(fun.name,":::")) {
      return(c(
        fun.name=str.right.of(fun.name,":::"),
        pkg=str.right.of(fun.name,":::")
      ))
    }
    return(c(
      fun.name=str.right.of(fun.name,"::"),
      pkg=str.left.of(fun.name,"::")
    ))
  }

  where.env = try(pryr::where(fun.name, pkg.env), silent=TRUE)
  if (is(where.env,"try-error")) {
    return(c(
      fun.name=fun.name,
      pkg="?"
    ))
  }

  #where.env[[fun]]
  where.name = environmentName(where.env)
  c(fun.name=fun.name,pkg=where.name)
  #list(long.name=paste0(fun.name,":::", where.name), fun.name=fun.name, pkg=where.name)
}

collect.pkg.calls = function(pkg) {
  pkg = "base"
  library(pkg,character.only = TRUE)

  pkg.env = asNamespace(pkg)
  ls(pkg.env,all.names = TRUE)
  funs = get.pkg.funs(pkg)

  fun.li = lapply(funs,find.fun.funs, penv=pkg.env)



  all.funs = unique(c(funs,unlist(fun.li)))
  ext.funs = setdiff(all.funs,funs )

  # Exported function calls from the package
  int.li = lapply(funs, function(fun) {
    c(fun.name=fun, pkg=pkg)
  })

  # External or not exported function calls
  pkg.env = asNamespace(pkg)
  ext.li = lapply(ext.funs, function(fun) {
    find.fun.pkg.from.pkg(fun,pkg = pkg, pkg.env=pkg.env)
  })

  fp.li = c(int.li, ext.li)
  names(fp.li) = c(funs, ext.funs)


  long.names = sapply(fp.li, function(el) {
    if (el$pkg == "?") return(el$fun.name)
    paste0(el$pkg,":::",el$fun.name)
  })
  names(fp.li) = long.names
  fp.li
}

pkg.risks = function(pkg,wl=NULL,risk.funs=NULL) {
  pkg = "base"
  setwd("D:/libraries/whitelistcalls/whitelistcalls/lists/risklists")
  risk.funs = unlist(yaml.load_file("risklist.yaml"))

  funs = get.pkg.funs(pkg)
  fun.li = lapply(funs,find.fun.funs, penv=pkg.env)
  names(fun.li) = funs

  risks = lapply(fun.li, function(sub.funs) {
    intersect(sub.funs, risk.funs)
  })
  risks = risks[!sapply(risks,function(el) length(el)==0)]

  risk.names = setdiff(names(risks), risk.funs)

  yaml = wbl.to.yaml(risk.names, pkg)
  writeClipboard(yaml)
  cat(yaml)
}

examples.listbuilder = function() {
  dir = setwd("D:/libraries/whitelistcalls/whitelistcalls/lists/pkglists")

  fun = "readLines"

  fun = "rnorm"
  pkg = "stats"

  pkg = "base"
  wl.file = paste0("white__",pkg,".yaml")
  bl.file = paste0("black__",pkg,".yaml")

  wl = bl = NULL
  funs = get.pkg.funs(pkg)
  if (file.exists(wl.file))
    wl = parse.whitelist.yaml(wl.file)
  if (file.exists(bl.file))
    bl = parse.whitelist.yaml(bl.file)

  if (is.null(wl)) wl = setdiff(funs,bl)
  if (is.null(bl)) bl = setdiff(funs,wl)

  gl = setdiff(funs,c(bl,wl))

  #wl.yaml = wbl.to.yaml(wl, "base")
  #writeLines(wl.yaml, wl.file)

  #bl.yaml = wbl.to.yaml(bl, "base")
  #writeLines(bl.yaml, bl.file)

  #gl.yaml = wbl.to.yaml(gl, "base")
  #writeLines(wl.yaml, wl.file)

  res = funs.call.graph(funs)
  g = res$g; funs.li = res$li
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


  fun = "colMeans"

  nh = neighborhood(g, nodes=wl,  mode="out", order=100)


  df.li = lapply(seq_along(wl), function(ind) {
    desc= funs[as.numeric(nh[[ind]])]
    bl.desc = intersect(desc, bl)
    list(name = wl[ind],num.bl = length(bl.desc), bl.desc=paste0(bl.desc, collapse=", "))
  })
  df = rbindlist(df.li)

  risk.df =df[df$num.bl >0,]

  list(g=g, li = funs.li)

}


#' Parse a whitelist yaml file and return it as a character vector
#'
#' For a structure of the yaml file,
#' see the example whitelists in the lists folder
#' @param yaml.file the file name of the whitelist
#' @param yaml.text yaml text as a single character variable
#' @param yaml.list an already imported yaml file returned from yaml.load
#' @return a character vector with function names or variable names that are forbidden
parse.whitelist.yaml = function(yaml.file=NULL,yaml.text=NULL, yaml.list=NULL) {
  if (is.null(yaml.list)) {
    if (is.null(yaml.text)) {
      yaml.list = yaml.load_file(yaml.file)
    } else {
      yaml.list = yaml.load(yaml.text)
    }
  }
  txt = unlist(yaml.list,use.names = FALSE)
  txt
}


examples.cat.pkg.funs = function() {
  cat.pkg.funs("stats")
  cat.pkg.funs("base")
  cat.pkg.funs("dplyr")
  cat.pkg.funs("ggplot2")
}


get.pkg.bl = function(pkg="base", wl) {
  funs = get.pkg.funs(pkg)
  setdiff(funs, wl)
}


get.pkg.wl = function(pkg="base", bl) {
  funs = get.pkg.funs(pkg)
  setdiff(funs, bl)
}



get.pkg.funs = function(pkg="base") {
  ls(paste0("package:",pkg))
}


#' Helper function to create whitelists
#'
#' Cats all functions in a package
#' as a yaml vector
#' @param pkg the name of the package
cat.pkg.funs = function(pkg="base") {
  library(pkg,character.only = TRUE)
  funs = ls(paste0("package:",pkg))
  txt = wbl.to.yaml(funs, pkg)
  writeClipboard(txt)
  cat(txt)
}

cat.funs = function(funs) {
  txt = paste0("- '",funs,collapse="'\n")
  writeClipboard(txt)
  cat(txt)
}

