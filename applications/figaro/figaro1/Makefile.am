## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro1

figaro1_SOURCES = figaro1.f $(F_ROUTINES)

# Sometimes multiple tasks refer to a single fortran file
# Note that SURFIT is not included due to NAG dependency.
# It can be reintroduced conditionally once a test for NAG
# is in place
figaro1_TASKS = \
adjoin bclean ccdlin ccur cfit clean clip coadd colour combine copobj \
cosrej creobj cset delobj dvdplot elsplot errcon esplot extlist \
extract figinfo flag2qual fscrunch goodvar growx growxt growxy \
growy growyt hard hcross hist hopt iadd ialog icadd icdiv icmult icont \
iconv3 icor16 icset icsub icur idev idiff idiv igconv igcur igrey \
ilist ilog image imult iplots ipower irevx irevy irot90 isedit ishift \
ismooth isplot istat istretch isub isubset isuper isxadd isxdiv isxmul \
isxsub isyadd isydiv isymul isysub ixsmooth lsplot lxset lyset medfilt \
medfiltr medsky msplot ncset optextract polysky profile q2bad \
qual2flag rembad renobj resample rescale retype rotx sclean scnsky \
scross scrunch setobj sfit slice soft splot sqrterr tippex \
trimfile twodspec_mon xcadd xcdiv xcmult xcsub xcur xtplane xyplane \
ycadd ycdiv ycmult ycsub ystract ytplane

F_ROUTINES = adjoin.f colour.f errcon.f grow.f iconv3.f ilog.f	       \
ismooth.f lset.f polysky.f retype.f slice.f ystract.f bclean.f	       \
combine.f extlist.f grow3.f icor16.f image.f isops.f lsplot.f	       \
profile.f rotx.f soft.f ccdlin.f copobj.f extract.f hcross.f icset.f   \
iplots.f isplot.f medfilt.f q2bad.f sclean.f splot.f ccur.f cosrej.f   \
hist.f icur.f ipower.f istat.f medsky.f qual2flag.f scnsky.f \
sqrterr.f cfit.f creobj.f figinfo.f hopt.f idiff.f irev.f istretch.f   \
msplot.f rembad.f scross.f clean.f cset.f flag2qual.f	       \
iadsub.f igconv.f irot90.f isubset.f ncset.f renobj.f scrunch.f	       \
tippex.f clip.f delobj.f fscrunch.f iconst.f igcur.f isedit.f isuper.f \
optextract.f resample.f setobj.f trimfile.f coadd.f dvdplot.f	       \
goodvar.f icont.f ilist.f ishift.f ixsmooth.f plane.f rescale.f sfit.f \
xcur.f





figaro1_LDADD = $(LDADD) \
../appsub/libappsub.la \
../fig/libfig.la \
../fda/libfda.la \
../jty/libjty.la \
../dta/libdta.la \
../fit/libfit.la \
../tio/libtio.la \
../gen/libgen.la \
../gkd/libgkd.la \
../par/libfigpar.la \
../ich/libich.la \
`pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` `fio_link_adam`

