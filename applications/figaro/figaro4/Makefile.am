## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro4

figaro4_SOURCES = figaro4.f \
                  $(figaro4_TASKS:=.f)

figaro4_TASKS = arcdisp arcgendb arcident arclocat ascin ascout bbody \
correl editext evalfit fillcube fitbb fitgauss fitpoly fittri grow    \
moments movie resamp speccont specgrid specplot subset xtract

figaro4_LDADD = $(LDADD) \
../appcb/libappcb.la \
../fitres/libfitres.la \
../spext/libspext.la \
../work/libwork.la \
../util/libutil.la \
../sploop/libsploop.la \
`pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` `fio_link_adam`

