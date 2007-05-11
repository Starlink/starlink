
## The monolith for all local atasks.
bin_MONOLITHS = gaiaMon

gaiaMon_TASKS = ardspectra ardstat autocrop cat2tab tab2cat asc2tab tab2asc

gaiaMon_SOURCES = \
	gaiaMon.c \
	$(gaiaMon_TASKS:=.f) \
	$(F_ROUTINES) \
	$(F_G_ROUTINES)

gaiaMon_LDADD = $(LDADD) \
	`fio_link_adam` `ard_link_adam` `ndf_link_adam` `cat_link_adam`

F_ROUTINES = gai1_nxtab.f gai1_s2ang.f gai1_t2cg.f gai1_t2cn.f gai1_wrtab.f \
	     gai_a2tab.f gai_c2tab.f gai_t2asc.f gai_t2cat.f rtd1_ardst.f \
	     rtd1_prsh2.f

G_ROUTINES = gai1_ardsp.gen rtd1_statx.gen rtd1_gbndx.gen

F_G_ROUTINES = $(G_ROUTINES:.gen=.f)

SUFFIXES: .c .o .f .gen 

FGENERIC = @FGENERIC@

.gen.f:
	$(FGENERIC) -t n $<
