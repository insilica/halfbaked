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
  evalfn   = memoise::memoise(function(egstring){cat("memoing");eval(parse(text = egstring))})
  memopipe = function(...){expr(...) |> deparse() |> evalfn()}
  forgetfn = function(){forget(evalfn)}
  list(evalfn=evalfn, memopipe=memopipe, forget=forgetfn)
}

#' mpipe
#' @import dplyr
#' @param ... a pipe, see example
#' @examples
#' data.frame(a=2) |> mutate(b=3) |> mpipe() # memorizes the long chain
#' @export
mpipe = function(...){mpipe.singleton$memopipe(...)}

#' mpipe.forget
#' forget the mpipe cached values
#' @examples
#' mpipe.forget()
#' @export
mpipe.forget = function(){mpipe.singleton$forget()}
