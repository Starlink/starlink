#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.
#  Original created by the SDT newdev command on:
#
#           Wed Jan  3 16:40:22 GMT 1996


#  Define new groups here...
C_ROUTINES = \
axis.c \
c2f77.c \
channel.c \
cmpframe.c \
cmpmap.c \
dssmap.c \
err_ems.c \
err_null.c \
error.c \
fchannel.c \
fcmpframe.c \
fcmpmap.c \
fdssmap.c \
ferror.c \
ffitschan.c \
fframe.c \
fframeset.c \
fintramap.c \
fitschan.c \
flutmap.c \
fmapping.c \
fmatrixmap.c \
fobject.c \
fpermmap.c \
fplot.c \
frame.c \
frameset.c \
fskyframe.c \
fslamap.c \
fsphmap.c \
funitmap.c \
fwcsmap.c \
fwinmap.c \
fzoommap.c \
grf_null.c \
grf_pgplot.c \
intramap.c \
loader.c \
lutmap.c \
mapping.c \
matrixmap.c \
memory.c \
object.c \
permmap.c \
plot.c \
pointset.c \
proj.c \
skyaxis.c \
skyframe.c \
sla.c \
slamap.c \
sphmap.c \
unitmap.c \
wcsmap.c \
wcstrig.c \
winmap.c \
zoommap.c

F_ROUTINES = \
object.f \
frame.f

DERIVED_FILES = \
ast.h \
ast_err.h \
ast_err \
fac_1521_err

C_INCLUDE_FILES = \
axis.h \
c2f77.h \
channel.h \
cmpframe.h \
cmpmap.h \
dssmap.h \
ems.h \
err.h \
error.h \
f77.h \
fitschan.h \
frame.h \
frameset.h \
grf.h \
intramap.h \
loader.h \
lutmap.h \
mapping.h \
matrixmap.h \
memory.h \
object.h \
permmap.h \
plot.h \
pointset.h \
proj.h \
skyaxis.h \
skyframe.h \
slalib.h \
slamap.h \
sphmap.h \
unitmap.h \
wcsmap.h \
wcstrig.h \
winmap.h \
zoommap.h

F_INCLUDE_FILES = \
ast_par.source

LATEX_DOCUMENTATION_FILES = \
sun210.tex \
sun211.tex

POSTSCRIPT_FIGURES = \
cmpframe.eps \
complex.eps \
frames.eps \
frameset.eps \
fronta.eps \
fronta_bw.eps \
frontb.eps \
frontb_bw.eps \
frontc.eps \
frontc_bw.eps \
fsalign.eps \
fsconvert.eps \
fsexample.eps \
fsmerge.eps \
fsremap.eps \
gridplot.eps \
gridplot_bw.eps \
mapping.eps \
parallel.eps \
series.eps \
simpexamp.eps

WCSLIB_FILES = \
COPYING.LIB \
proj.c \
proj.h \
wcstrig.c \
wcstrig.h

UNIX_LINK_FILES = \
ast_link \
ast_link_adam

UNIX_STARTUP_FILES = \
ast_dev

VERSION_FILES = \
makefile \
sun_master.tex

MESSAGE_SYSTEM_FILES = \
ast_err.msg

UTILITY_PROGRAMS = \
astbad.c

TEST_PROGRAMS = \
ast_test.c

DEVELOPMENT_TOOLS = \
makeh

#  Target for use by the grp command.
$(action)

#  Keywords for use by SCCS.
#%Z%%M%   %I%   %E% %U%   %D% %T%
