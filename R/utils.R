#' Null coalescing operator
#'
#'
#' @name null-coalesce
#' @rdname nullcoalesce
#' @keywords internal
#' @usage lhs \%otherwise\% rhs
#'
`%otherwise%` <- function(lhs, rhs) {
  if (!is.null(lhs) && length(lhs) > 0) lhs else rhs
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

is_scrapable <- function(bow){
  is_scrapable_rt(bow$robotstxt, bow$url, bow$user_agent)
}

#' @importFrom httr parse_url
is_scrapable_rt <- function(rtxt, url, user_agent){
  url_parsed <- httr::parse_url(url)
  rtxt$check(paths=url_parsed$path, bot=user_agent)
}

#' @importFrom httr GET
httr_get <- function(url, config, handle, times, verbose){
  httr::RETRY(verb="GET",
    url = url,
    config = config,
    handle = handle,
    times = times,
    pause_base = 5,
    quiet=!verbose
  )
}

#' @importFrom ratelimitr limit_rate rate
httr_get_ltd <- ratelimitr::limit_rate(httr_get,
                                       ratelimitr::rate(n = 1, period=5))

#' @importFrom ratelimitr limit_rate rate
#' @importFrom utils download.file
download_file_ltd <- ratelimitr::limit_rate(utils::download.file,
                                       ratelimitr::rate(n = 1, period=5))

#' Host key for a URL
#'
#' Builds the `scheme://hostname[:port]` key used to isolate per-host session
#' state. The port is included when present so that two servers living on
#' different ports of the same machine are treated as different hosts.
#'
#' @param url URL
#' @return character host key
#' @keywords internal
#' @importFrom httr parse_url
url_host_key <- function(url){
  url_parsed <- httr::parse_url(url)
  key <- paste0(url_parsed$scheme, "://", url_parsed$hostname)
  if(!is.null(url_parsed$port) && nzchar(url_parsed$port))
    key <- paste0(key, ":", url_parsed$port)
  key
}

## Per-host session state ----------------------------------------------------

# environment mapping host keys (scheme://hostname[:port]) to per-host state.
# Each state carries its own rate-limited `get`/`rip` functions and its own
# memoise cache, so that hosts never overwrite each other's throttles and
# cached results are only shared within the same host.
.polite_hosts <- new.env(parent = emptyenv(), hash = TRUE)

#' Get (or create) the polite session state for a host
#'
#' @param domain host key as produced by `url_host_key()`
#' @param delay delay (in seconds) between subsequent requests, used only when
#' the state for this host is created for the first time
#'
#' @return environment with per-host rate limiters and cache
#' @keywords internal
#' @importFrom ratelimitr limit_rate rate
#' @importFrom memoise cache_memory
#' @importFrom utils download.file
polite_host <- function(domain, delay = 5){
  st <- .polite_hosts[[domain]]
  if(is.null(st)){
    st <- new.env(parent = emptyenv(), hash = FALSE)
    st$domain <- domain
    st$delay  <- delay
    st$get    <- ratelimitr::limit_rate(httr_get,
                                        ratelimitr::rate(n = 1, period = delay))
    st$rip    <- ratelimitr::limit_rate(utils::download.file,
                                        ratelimitr::rate(n = 1, period = delay))
    st$cache  <- memoise::cache_memory()
    st$scrape <- NULL
    .polite_hosts[[domain]] <- st
  }
  st
}

#' Merge a desired delay into the rate limit of a host
#'
#' Reuses the existing per-host limiters and merges the new delay with the one
#' already in effect: the most conservative (largest) value wins, so a host
#' never becomes less polite as a result of a new `bow()`. Other hosts are
#' not affected.
#'
#' @param domain host key as produced by `url_host_key()`
#' @param delay desired delay in seconds
#'
#' @return the delay now in effect for this host
#' @keywords internal
#' @importFrom ratelimitr UPDATE_RATE rate
merge_host_delay <- function(domain, delay){
  st <- polite_host(domain, delay = delay)
  new_delay <- max(st$delay, delay)
  if(!isTRUE(all.equal(new_delay, st$delay))){
    ratelimitr::UPDATE_RATE(st$get, ratelimitr::rate(n = 1, period = new_delay))
    ratelimitr::UPDATE_RATE(st$rip, ratelimitr::rate(n = 1, period = new_delay))
    st$delay <- new_delay
  }
  st$delay
}

#' Clear all per-host scrape caches
#'
#' @return `TRUE`, invisibly
#' @keywords internal
polite_forget <- function(){
  for(nm in ls(.polite_hosts, all.names = TRUE)){
    st <- .polite_hosts[[nm]]
    if(!is.null(st$cache)) st$cache$reset()
  }
  invisible(TRUE)
}



#' Guess download file name from the URL
#'
#' @param x url to guess basename from
#'
#' @return guessed file name
#' @export
#'
#' @examples
#' guess_basename("https://bit.ly/polite_sticker")
#'
#' @importFrom tools file_ext
#' @importFrom httr HEAD headers
guess_basename <- function(x) {
  destfile <- basename(x)
  if(tools::file_ext(destfile)==""){
    hh <- httr::HEAD(x)
    destfile <- basename(hh$url)
    if(tools::file_ext(destfile)==""){
    cds <- httr::headers(hh)$`content-disposition`
    destfile <- gsub('.*filename=', '', gsub('\\\"','', cds))
  }}
  destfile %otherwise% basename(x)
}


is_url <- function(x){
  # from Jim Hester's rex vignette https://cran.r-project.org/web/packages/rex/vignettes/url_parsing.html
  re <- "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9\u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9\u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$"
  grepl(re, x)
}
