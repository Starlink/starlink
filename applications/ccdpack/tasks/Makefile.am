noinst_LTLIBRARIES = libccdpack_tasks.la
libccdpack_tasks_la_SOURCES = $(TASKSRC)

AM_FCFLAGS = -I../

#  List of applications/tasks in monoliths.

REDTASKS = makebias debias makecal calcor makeflat flatcor
REGTASKS = findcent idicurs findobj register tranlist plotlist findoff \
 pairndf ccdedit makemos tranndf drizzle astimp astexp wcsreg wcsedit drawndf
RESTASKS = ccdsetup ccdclear ccdnote ccdshow ccdndfac ccdgenerate \
 picinfo import present schedule ccdimp ccdexp makeset showset
SCRTASKS = reduce ccdalign xreduce

TASKSRC = $(REDTASKS:=.f) $(REGTASKS:=.f) $(RESTASKS:=.f) $(SCRTASKS:=.f)

