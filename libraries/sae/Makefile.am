## Process this file with automake to produce Makefile.in

include_HEADERS = SAE_PAR sae_par.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

include_MESSAGES = SAI_ERR sai_err.h
# The facility code within sae_err.msg is SAI, not the default.
SAI_ERR_SOURCES = sae_err.msg
sai_err.h_SOURCES = sae_err.msg

dist_pkgdata_DATAT = LICENCE
EXTRA_DIST = sae_err.msg

CLEANFILES = SAI_ERR sai_err.h

