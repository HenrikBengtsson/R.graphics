# Allows conflicts. For more information, see library() and
# conflicts() in [R] base.
.conflicts.OK <- TRUE

## .First.lib <- function(libname, pkgname) {
.onAttach <- function(libname, pkgname) {
  # Set the default width of a PS/EPS image to 5 inches. To support "50%".
  # From R v2.0.0 this must be in .First.lib.
  grDevices::ps.options(width=5.0);

  pkg <- Package(pkgname);
  assign(pkgname, pkg, pos=getPosition(pkg));

  envir <- globalenv();
  assign(".Bitmap.Options", .Bitmap.Options, envir=envir);
  assign(".PicTeX.Options", .PicTeX.Options, envir=envir);

  packageStartupMessage(getName(pkg), " v", getVersion(pkg), " (", 
    getDate(pkg), ") successfully loaded. See ?", pkgname, " for help.");
}




