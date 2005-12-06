
noinst_LTLIBRARIES = libsmf.la

libsmf_la_SOURCES = $(C_ROUTINES)

C_ROUTINES = smf_correct_extinction.c smf_open_file.c smf_fits_rdhead.c\
smf_fits_crchan.c \
smf_close_file.c \
smf_fits_getI.c smf_fits_getD.c \
smf_tslice_ast.c \
smf_flatfield.c \
smf_check_flat.c

noinst_INCLUDES = smf.h smf_typ.h

# fac files must be installed
noinst_MESSAGES = smf_err.h

BUILT_ROUTINES = smf_err.h
