## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libjcmtapplic.la

libjcmtapplic_la_SOURCES = $(F_ROUTINES)

# Need include files from the subs directory
# and from FIGARO
AM_FCFLAGS = -I../subs -I../../../figaro/dta -I../../../figaro/include

F_ROUTINES = ae2rd1.f fake.f jcmtextc.f map2mem.f restore.f ae2rd2.f \
iras_tag.f makemap.f map2ts.f ts2map.f
