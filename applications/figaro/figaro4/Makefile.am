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

IFC_FILES = arcdisp.ifc arcgendb.ifc arcident.ifc arclocat.ifc ascin.ifc
ascout.ifc bbody.ifc correl.ifc editext.ifc evalfit.ifc fillcube.ifc
fitbb.ifc fitgauss.ifc fitpoly.ifc fittri.ifc grow.ifc moments.ifc
movie.ifc resamp.ifc speccont.ifc specgrid.ifc specplot.ifc subset.ifc
xtract.ifc figaro4.ifc

MISC_BUILT_FILES = version.dat figaro4.ifl figaro4

CLEANFILES = $(IFC_FILES) $(MISC_BUILT_FILES)


