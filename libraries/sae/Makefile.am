## Process this file with automake to produce Makefile.in

include_HEADERS = SAE_PAR sae_par.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Note that, although sae_err.msg exists, we only generate, install
# and distribute, the fac_nnn_err file.  This is done as a side-effect
# of the declaration of *_MESSAGES, and the SAE_ERR file which is thus
# generated is not used not installed.
noinst_MESSAGES = SAI_ERR
# The facility code within sae_err.msg is SAI, not the default.
SAI_ERR_SOURCES = sae_err.msg

EXTRA_DIST = sae_err.msg

CLEANFILES = SAI_ERR

