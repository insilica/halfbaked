#' throttle
#' a bad way of throttling a function
#' @param fn the function to run with a throttle
#' @param speed seconds between calls
#' @export
throttle = function(fn,speed=0.5){
  lasttime = as.numeric(Sys.time())
  function(...){
    sleeptime = max(0,speed - (as.numeric(Sys.time()) - lasttime))
    Sys.sleep(speed)
    res = fn(...)
    lasttime <<- as.numeric(Sys.time())
    res
  }
}

#' runjob
#' run a job in the background
#' @import rstudioapi
#' @import readr
#' @param expr an expression to run as a job
#' @param libs a library expression to run in the job
#' @param jobname a name for the job
#' @param jobfile the file to write the jobscript
#' @export
runjob = function(expr,libs,jobname,jobfile){
  dir.create("job")
  filename = sprintf("./job/%s.R",jobfile)
  readr::write_lines(deparse(libs),filename,append = F)
  readr::write_lines(deparse(expr),filename,append = T)
  rstudioapi::jobRunScript(filename,jobname,workingDir = getwd(),importEnv = T)
}

#' xml.tagnames
#' @concept maybe we add this to some xmltools package
#' @param node an xml_document or xml_node
#' @param prefix (optional) add a prefix "prefix/name/..." to the resulting names
#' @export
xml.tagnames = function(node,prefix=""){
  tag      = sprintf("%s/%s",prefix,xml_name(node))
  children = sapply(xml2::xml_children(node),function(x){xml.tagnames(x,prefix = tag)},simplify = T) |> unlist()
  c(tag,children)
}

##' resolve.id
##' @concept how about an rendnote library?
##' TODO this needs to be rewritten without the XML package.
##' resolve the bibliographic identifiers from an endnote record
##' @export
# resolve.id = function(xmlrecord,type="endnote.xml"){
#   result = if(type == "endnote.xml"){
#
#     x.list = XML::xmlParse(xmlrecord) |> XML::xmlToList() |> .[c("urls","electronic-resource-num")]
#     x = c(x.list |> toString(),URLdecode(x.list |> toString())) |> toupper()
#
#     pmids = stringr::str_match(x,pattern = "PMID:([0-9]+)") |> .[!is.na(.[,1]),2] |> unique()
#     dois  = stringr::str_match(x,pattern = "(10.\\d{4,9}/[-._;()/:A-Z0-9]+)") |> .[!is.na(.[,1]),2] |> unique()
#
#     data.frame(id=pmids) |> mutate(type = "pmid") |> bind_rows(data.frame(id=dois) |> mutate(type="doi"))
#   }
#
#   if(result |> group_by(type) |> count() |> pull(n) |> max() > 1){
#     warning("ambiguous resolution")
#   }else if(nrow(result) == 0){
#     warning("no resolution")
#   }
#   return(result)
# }

#' regex.email
#' a regex for extracting emails
#' @concept how about a big library of useful regexes?
#' @export
regex.email = function(){
  "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
}

#' regex.doi
#' a regex for extracting dois
#' @concept how about a big library of useful regexes?
#' @export
regex.doi = "(10.\\d{4,9}/[-._;()/:A-Z0-9]+)"

##' resolve.abstract
##' TODO rewrite without the XML library
##' request an abstract for a given bibliographic id from crossref and pubmed
##' @import reutils
##' @import rcrossref
##' @import xml2
##' @export
# resolve.abstract = function(id,id.type){
#   if(is.na(id)){
#     NA_character_
#   }else if(id.type=="doi"){
#     tryCatch({rcrossref::cr_abstract(id)},error=function(e){NA_character_})
#   }else if(id.type=="pmid"){
#     res = efetch(id,"pubmed")$content |> xmlParse() |> xmlToList()
#     a   = res$PubmedArticle$MedlineCitation$Article$Abstract |> .[names(.)=="AbstractText"]
#     res = paste(unlist(a),collapse="\n\n")
#     if(nchar(res)==0){NA_character_}else{res}
#   }
# }


#' add.lib
#' modifies the current notebook to add a library to the top chunk
#' looks for a chunk called ```{r, setup and adds lines directly below
#' @import glue
#' @param lib the library to add
#' @param path path to an rmarkdown notebook (defaults to this current open notebook)
#' @param eval whether or not to load the given library (default true)
#' @examples
#' # add.lib(library(readr))
#' @export
add.lib = function(lib,path=rstudioapi::getSourceEditorContext()$path,eval=T){
  nbtop = readr::read_lines(path)
  schun = which(startsWith(tolower(nbtop),"```{r, setup"))
  if(length(schun)==0){stop('no setup chunk, is there a line starting with "```{r, setup"?')}
  newlines = c(nbtop[1:schun],glue("library({lib})"),nbtop[-(1:schun)])
  readr::write_lines(newlines,path,append = F)
  if(eval){requireNamespace(lib)}
}

#' mpipe.singleton
#' @import memoise
#' @import dplyr
mpipe.singleton = {

  evalfn   = memoise::memoise(function(egstring,invalidate.obj){
    cat("memoing");eval(parse(text = egstring))
  })

  forgetfn = function(){forget(evalfn)}
  list(evalfn=evalfn, forget=forgetfn)
}

#' mpipe
#' memoizes the results of a pipe and invalidates on the deparsed code.
#' @param expr a piped expression
#' @param iv an object to invalidate cache, can be a name or any other hashable object. If it changes, the function will be re-memoized.
#' @examples
#' data.frame(a=2) |> dplyr::mutate(b=3) |> mpipe() # memorizes the long chain
#' @export
mpipe = function(expr,iv=NA){
  egstring = substitute(expr) |> deparse()
  mpipe.singleton$evalfn(egstring,iv)
}

#' mpipe.forget
#' forget the mpipe cached values
#' @examples
#' mpipe.forget()
#' @export
mpipe.forget = function(){mpipe.singleton$forget()}

#' xpluck
#' allows plucking from a list based on expression rather than string. which works with autocomplete better
#' @param input_list a list to pluck from
#' @param ... some code that deparses to a symbol that can be plucked
#' @import purrr
#' @examples
#' xpluck(list(a=2),a)
#' @export
xpluck = function(input_list,...){
  pluck(input_list,deparse(expr(...)))
}

#' @title Try
#' a wrapper around R try.
#' Try an expression and getOrElse the result
#' @param expr an expression to try
#' @param silent whether to suppress error messages
#' @examples
#' Try({log("a")})$get(5)
#' @export
Try = function(expr,silent=T){
  res = try({expr},silent = T)
  list(get = function(.default=NA){
    if(class(res) == "try-error"){ .default }else{ res }
  })
}

#' an inline version of if{...}else{...}
#' @param boolexpr an expression to evalu
#' @param ifexpr an expression if boolexpr is true
#' @param elexpr an expression if boolexpr is false
#' @examples
#' ifel({5==5},1,{1/5})
#' ifel({5==6},1,{1/5})
#' @export
ifel = function(boolexpr,ifexpr,elexpr){
  if(eval(boolexpr)){
    eval(ifexpr)
  }else{
    eval(elexpr)
  }
}

#' chunks input into list of vectors w/ max size chunksize
#' @param v the vector to chunk
#' @param chunksize size of the batches
#' @examples
#' chunk(1:1000,28)
#' @export
chunk = function(v,chunksize){split(v, ceiling(seq_along(v)/28))}

#' batch make a function into a batch function
#' @description
#' it is often useful to run a function in batches.
#' Particularly side effect functions.
#' @importFrom pbapply pblapply
#' @param batchsize size of the batches
#' @param fn the function to run in batches
#' @examples
#' fn = function(x){ sum(x) }
#' batchify(100,fn)(1:1000)
#' @export
batchify = function(batchsize=100,fn){ function(v,...){ pblapply(chunk(v,batchsize),fn) } }

