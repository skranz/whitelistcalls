# Package whitelistcalls


** Very preliminary version, don't use it yet for your webapps**

The package can check whether the function calls and accessed variables in parsed R code satifsfy a given whitelist of allowed function calls (or accessed variables)
and / or do not violate a blacklist. The main function is `check.whitelist`.

### Example



```r
library(whitelistcalls)
# Some code as a string
code = "print(readLines('myfile.csv'))"

# Parse the code in the usual way
# At this point the code is not yet executed
expr = parse(text=code)

# Specify whitelist or blacklist of function calls
# (the whitelist package also has predefined lists 
#  and helper functions to create such lists)
wl.funs = c("print","cat")

# Check whether all function calls in expr satisfy the whitelist
check.whitelist(expr, wl.funs=wl.funs)
```

```
## $ok
## [1] FALSE
## 
## $fb.funs
## [1] "readLines"
## 
## $fb.vars
## NULL
## 
## $msg
## [1] "For security reasons, it is not allowed to call the following functions:\nreadLines\n"
```

```r
# The same result using a (much too short) blacklist
check.whitelist(expr, bl.funs=c("readLines","writeLines"))
```

```
## $ok
## [1] FALSE
## 
## $fb.funs
## [1] "readLines"
## 
## $fb.vars
## NULL
## 
## $msg
## [1] "For security reasons, it is not allowed to call the following functions:\nreadLines\n"
```

See the examples and details further below to see how the package works and which function calls can be detected and which not.

In general, I would prefer using whitelists that explicitly state which functions are allowed to be called instead of blacklists. For example, you may have generated blacklists based on the standard R packages, but in the application another library is loaded that has additional security risks.

### Securing R based webapps

The main motivation for the package is to help securing web apps that allow users to enter more or less free R code and run it. Examples are the shiny based  [RTutor](https://github.com/skranz/RTutor) problem sets. The key idea is to avoid running commands that can potentially be harmful, e.g. commands that allow system or file access.

#### Warning: Never rely on whitelistcalls alone for security of webapps!

However, I would strongly discourage you to rely only on whitelistcalls for security. Who knows what loopholes have been overlooked? If you run a shiny app
on your on linux machine with shiny server, you should definitely also use the
[RAppArmor](https://github.com/jeroenooms/RAppArmor) package to restrict
file access on a system level. Alternatively, you may host your app on a 
service like shinyapps.io that ensures that your app has no access outside
the app directory.

## How does check.whitelist work?

The function check.whitelist works in a simple fashion. It traverses the unevaluated call object and stores all function calls. Then it compares whether all these calls are in the provided whitelist of allowed function calls and / or, if blacklist is provided, not in the blacklist. Similar, also access to variables can be whitelisted / blacklisted. (It ususally makes sense to blacklist access to global variables like .GlobalEnv).  

### check.whitelist does not look inside existing functions

check.whitelist does not check whether a called function internally makes a forbidden function call. For example, in the following example, check.whitelist will not state a violation:


```r
myReadLines = function(...) readLines(...)

expr = quote({
  myReadLines("myfile.csv")
})
# Does not look inside myReadLines
check.whitelist(expr, bl.funs=c("readLines"))
```

```
## $ok
## [1] TRUE
## 
## $fb.funs
## character(0)
## 
## $fb.vars
## NULL
## 
## $msg
## [1] ""
```

Yet, it does look inside functions that are generated in
the code:

```r
expr = quote({
  myReadLines = function(...) readLines(...)
  myReadLines("myfile.csv")
})
# Now does look inside myReadLines
check.whitelist(expr, bl.funs=c("readLines"))
```

```
## $ok
## [1] FALSE
## 
## $fb.funs
## [1] "readLines"
## 
## $fb.vars
## NULL
## 
## $msg
## [1] "For security reasons, it is not allowed to call the following functions:\nreadLines\n"
```


## Which sort of functions do you want to forbid?

There are different categories of functions that one typically wants to forbid.

### 1. Functions that convert strings to calls or evaluate calls

If users can parse and evaluate arbitrary strings that contain code,
`check.whitelist` can be easily cirumvented. Here are some examples that
circumvent the detection of a call to `list.files`.

```r
expr = quote({
  eval(parse(text="list.files()"))

  # indirect parsing and evaluating via mutate_ in dplyr
  library(dplyr)
  mutate_(data_frame(dir=c("./")), files = "list.files()[1]")
  
  # Indirect parsing via as.formula
  eval(as.formula("y~I(list.files())")[[3]][[2]])

})
# Don't detect circumvented call to list.files
check.whitelist(expr, bl.funs=c("list.files"))
```

```
## $ok
## [1] TRUE
## 
## $fb.funs
## character(0)
## 
## $fb.vars
## NULL
## 
## $msg
## [1] ""
```
You see that there is actually quite a bunch of functions like `parse`, `eval`, `mutate_` or even `as.formula` that are quit risky and should not be allowed.

### 2. Functions that take other functions as arguments

Loopholes are also created by functions take other functions or function names as argument...

```r
expr = quote({
  do.call("list.files", args=list())
  # Unfortunately, also the popular lapply function is risky...  
  lapply("./","list.files")
})
# Don't detect circumvented call to list.files
check.whitelist(expr, bl.funs=c("list.files"))
```

```
## $ok
## [1] TRUE
## 
## $fb.funs
## character(0)
## 
## $fb.vars
## NULL
## 
## $msg
## [1] ""
```

Currently, check.whitelist does also not detect a function argument
when it is not in form of a string. Hopefully, this issue will be closed in a future version.

```r
expr = quote({
  do.call(list.files, args=list())
  # Unfortunately, also the popular lapply function is risky...  
  lapply("./",list.files)
})
# Don't detect circumvented call to list.files
check.whitelist(expr, bl.funs=c("list.files"))
```

```
## $ok
## [1] TRUE
## 
## $fb.funs
## character(0)
## 
## $fb.vars
## NULL
## 
## $msg
## [1] ""
```

### 3. Functions or global variables that allow access to environments

If you can access environments, you can also access and call their functions
in a way that check.whitelist does not detect.

```r
expr = quote({
  baseenv()[["list.files"]]()
  .BaseNamespaceEnv[["list.files"]]()
})
# Don't detect circumvented call to list.files
check.whitelist(expr, bl.funs=c("list.files"))
```

```
## $ok
## [1] TRUE
## 
## $fb.funs
## character(0)
## 
## $fb.vars
## NULL
## 
## $msg
## [1] ""
```

### 4. Functions that access or load package

If user code can access packages like `base:::list.files` or can load packages with `library` or `require`, bad things may happen. In particular, if the loaded
packages contains a function with the same name as a whitelisted function from another package, but dangerous behavior. `check.whitelist` only checks function names, not the package a function is called from.

### 5. Function that can call system commands, access files, open internet connections

These are the functions that may possibly generate the worst damage. So they should not be allowed.

## Discussion

You see
