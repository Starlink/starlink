## Process this file with automake to produce Makefile.in

# Use common set of include files.
AM_FCFLAGS = -I../include

bin_MONOLITHS = figaro1

MULTISOURCES = grow3.f grow.f iadsub.f iconst.f irev.f isops.f \
               lset.f plane.f 

figaro1_SOURCES = figaro1.f \
                  $(NORMALTASKS:=.f) \
                  $(MULTISOURCES)

figaro1_TASKS = $(NORMALTASKS) $(CODELESSTASKS)

# Note that SURFIT is not included due to a NAG dependency.  It was
# already commented out in the monolith, but not from the Classic
# style makefile.  It also clashes with KAPPA:SURFIT.
NORMALTASKS = \
adjoin bclean ccdlin ccur cfit clean clip coadd colour combine copobj \
cosrej creobj cset delobj dvdplot errcon extlist extract figinfo \
flag2qual fscrunch goodvar hcross hist hopt icont iconv3 icor16 icset \
icur idiff igconv igcur ilist ilog image iplots ipower irot90 isedit \
ishift ismooth isplot istat istretch isubset isuper ixsmooth lsplot \
medfilt medsky msplot ncset optextract polysky profile q2bad \
qual2flag rembad renobj resample rescale retype rotx sclean scnsky \
scross scrunch setobj sfit slice soft splot sqrterr tippex trimfile \
xcur ystract

# Sometimes a single source file applies to many tasks.  This is
# a list of those tasks which do not have a corresponding <task>.f.
CODELESSTASKS = elsplot esplot growx growxt growxy growy growyt hard \
iadd ialog icadd icdiv icmult icsub idev idiv igrey imult irevx irevy \
isub isxadd isxdiv isxmul isxsub isyadd isydiv isymul isysub lxset \
lyset medfiltr xcadd xcdiv xcmult xcsub xtplane xyplane ycadd \
ycdiv ycmult ycsub ytplane

figaro1_LDADD = $(LDADD) \
../appsub/libappsub.la \
../fig/libfig.la \
../fda/libfda.la \
../jty/libjty.la \
../tio/libfigtio.la \
../gen/libfiggen.la \
../gkd/libgkd.la \
../par/libfigpar.la \
../ich/libfigich.la \
`pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` `fio_link_adam`
