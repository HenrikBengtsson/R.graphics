# Added '...' to some base functions. These will later be
# turned into default functions by setMethodS3().

choose <- appendVarArgs(choose)
isOpen <- appendVarArgs(isOpen)
if (exists("bringToTop", mode="function"))
  bringToTop <- appendVarArgs(bringToTop)


############################################################################
# HISTORY:
# 2005-02-20
# o BUG FIX: "bringToTop" on pre-exists on R for Windows.
# o Created to please R CMD check.
############################################################################
