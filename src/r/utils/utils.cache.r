CACHE.DIR <- '.'

#' Cache object into specified directory
#'
#' @param args Prefereably alist (i.e. list of symbols,
#' where the arguments are not evaluated.)
#'
#' @keywords internal
#'
as.file <- function(fun, args, key = lapply(args, eval.parent, n = 2), prefix = 'cache',
                    use.cache = TRUE, cache.dir = CACHE.DIR) {

  if (!dir.exists(cache.dir)) dir.create(cache.dir, recursive = TRUE)

  hash <- digest::digest(key, algo = 'xxhash64')
  cache.path <- tolower(paste0(cache.dir, '/', prefix, '_', hash, '.rds'))

  if (use.cache && !file.exists(cache.path)) {
    lock.path <- paste0(cache.path, '.lock')
    wlog('[CACHE] Locking %s... (computing)', cache.path)
    lock <- filelock::lock(lock.path)
    on.exit(filelock::unlock(lock))
    on.exit(try(file.remove(lock.path), silent = TRUE), add = TRUE)
    on.exit(wlog('[CACHE] Unlocked %s...', cache.path), add = TRUE)
  }

  if (use.cache && file.exists(cache.path)) {
    wlog('[CACHE] Using cache for %s...', prefix)
  } else {
    result <- do.call(fun, args, envir = parent.frame(n = 1))
    base::saveRDS(result, file = cache.path)
  }

  base::readRDS(cache.path)
}
