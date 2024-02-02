# .onLoad <- function(libname, pkgname)
# {
#   library.dynam("r4fish", pkgname, libname)
# }

r4fishStartup <- function()
{
  msg <- c(paste0("🆁4🅵🅸🆂🅷 🌊🦈 version ",
                  packageVersion(".r4fish")), "\nType 'citation(\"r4fish\")' for citing this R package in publications.",
           "\nLAssessment for Marine Resources Toolkit",
           "\nqselmer@gmail.com")
  return(msg)
}

r4fishStartupMessage <- function()
{
  msg <- c(paste0(
    "🆁4🅵🅸🆂🅷 🌊🦈 version ",
packageVersion("r4fish")),
"\nType 'citation(\"r4fish\")' for citing this R package in publications.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # unlock .r4fish variable allowing its modification
  unlockBinding(".r4fish", asNamespace("r4fish"))
  # startup message
  msg <- r4fishStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'r4fish' version", packageVersion("r4fish"))
  packageStartupMessage(msg)
  invisible()
}

