## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro1

figaro1_SOURCES = figaro1.f \
                  $(figaro1_TASKS:=.f)

figaro1_TASKS = \
adjoin bclean ccdlin ccur cfit clean clip coadd colour combine copobj \
cosrej creobj cset delobj dvdplot elsplot errcon esplot extlist \
extract figaro1 figinfo flag2qual fscrunch goodvar growx growxt growxy \
growy growyt hard hcross hist hopt iadd ialog icadd icdiv icmult icont \
iconv3 icor16 icset icsub icur idev idiff idiv igconv igcur igrey \
ilist ilog image imult iplots ipower irevx irevy irot90 isedit ishift \
ismooth isplot istat istretch isub isubset isuper isxadd isxdiv isxmul \
isxsub isyadd isydiv isymul isysub ixsmooth lsplot lxset lyset medfilt \
medfiltr medsky msplot ncset optextract polysky profile q2bad \
qual2flag rembad renobj resample rescale retype rotx sclean scnsky \
scross scrunch setobj sfit slice soft splot sqrterr surfit tippex \
trimfile twodspec_mon xcadd xcdiv xcmult xcsub xcur xtplane xyplane \
ycadd ycdiv ycmult ycsub ystract ytplane

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

