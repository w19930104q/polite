context("Per-host throttling and cache isolation")
library(webmockr)

# Two local robots.txt fixtures with different crawl-delays.
# Host A allows one request every 0.5s, host B one every 1.2s.

webmockr::enable(adapter = "httr")
webmockr::httr_mock()
webmockr::stub_registry_clear()
webmockr::request_registry()$reset()

ua <- "testthat fixture bot"

webmockr::stub_request("get", "https://host-a.test/robots.txt") %>%
  webmockr::to_return(body = "User-agent: *\nCrawl-delay: 0.5\n", status = 200)

webmockr::stub_request("get", "https://host-b.test/robots.txt") %>%
  webmockr::to_return(body = "User-agent: *\nCrawl-delay: 1.2\n", status = 200)

pages_a <- c("https://host-a.test/p1", "https://host-a.test/p2",
             "https://host-a.test/p1?q=1", "https://host-a.test/p1?q=2")
pages_b <- c("https://host-b.test/p1", "https://host-b.test/p2")

for (u in c(pages_a, pages_b))
  webmockr::stub_request("get", u) %>%
    webmockr::to_return(body = paste0("body of ", u), status = 200)

# count actual HTTP requests to a URL, across all request signatures
# (webmockr registers a separate signature per header set, e.g. per user-agent)
times_executed <- function(u) {
  sigs <- webmockr::request_registry()$request_signatures$hash
  counts <- vapply(sigs, function(x)
    if (identical(x$sig$uri, u)) x$count else 0, numeric(1))
  sum(counts)
}

sessA <- polite::bow("https://host-a.test/p1", user_agent = ua, delay = 0.5)
sessB <- polite::bow("https://host-b.test/p1", user_agent = ua, delay = 1.2)

test_that("each host keeps its own crawl-delay", {
  expect_equal(sessA$delay, 0.5)
  expect_equal(sessB$delay, 1.2)
})

test_that("interleaved scraping follows each host's own clock", {
  t <- rep(NA_real_, 4)
  scrape(polite::nod(sessA, "p1"), accept = "txt"); t[1] <- as.numeric(Sys.time())
  scrape(polite::nod(sessB, "p1"), accept = "txt"); t[2] <- as.numeric(Sys.time())
  scrape(polite::nod(sessA, "p2"), accept = "txt"); t[3] <- as.numeric(Sys.time())
  scrape(polite::nod(sessB, "p2"), accept = "txt"); t[4] <- as.numeric(Sys.time())

  # host B is not blocked by the request just made to host A
  expect_lt(t[2] - t[1], 0.4)
  # host A still uses its own 0.5s delay, not the 1.2s of host B
  expect_gte(t[3] - t[1], 0.5)
  expect_lt(t[3] - t[1], 1.2)
  # host B still uses its own 1.2s delay, not the 0.5s of host A
  expect_gte(t[4] - t[2], 1.2)
  # independent clocks: well below the serialized 0.5 + 1.2 + 1.2 + ...
  expect_lt(t[4] - t[1], 2.2)

  # every page was requested exactly once
  for (u in c("https://host-a.test/p1", "https://host-a.test/p2",
              "https://host-b.test/p1", "https://host-b.test/p2"))
    expect_equal(times_executed(u), 1)
})

test_that("nod to another domain switches to that host's policy", {
  sessX <- polite::nod(sessA, "https://host-b.test/p2")
  expect_equal(sessX$domain, "https://host-b.test")
  expect_equal(sessX$delay, 1.2)
  # host A's own throttle is untouched
  expect_equal(sessA$delay, 0.5)
})

test_that("cache hits require same host, user-agent and query", {
  s1 <- polite::bow("https://host-a.test/p1", user_agent = ua, delay = 0.5)

  scrape(s1, query = list(q = 1), accept = "txt")
  scrape(s1, query = list(q = 1), accept = "txt") # identical -> cached
  expect_equal(times_executed("https://host-a.test/p1?q=1"), 1)

  # different request parameters -> not a cache hit
  scrape(s1, query = list(q = 2), accept = "txt")
  expect_equal(times_executed("https://host-a.test/p1?q=2"), 1)

  # different user-agent -> not a cache hit
  s2 <- polite::bow("https://host-a.test/p1", user_agent = "other fixture bot",
                    delay = 0.5)
  scrape(s2, query = list(q = 1), accept = "txt")
  expect_equal(times_executed("https://host-a.test/p1?q=1"), 2)

  # a fresh session with identical semantics -> served from the host cache
  s3 <- polite::bow("https://host-a.test/p1", user_agent = ua, delay = 0.5)
  scrape(s3, query = list(q = 1), accept = "txt")
  expect_equal(times_executed("https://host-a.test/p1?q=1"), 2)
})

webmockr::disable(adapter = "httr")
