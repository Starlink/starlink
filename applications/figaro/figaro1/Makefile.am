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
../jty/libfigjty.la \
../tio/libfigtio.la \
../gen/libfiggen.la \
../gkd/libgkd.la \
../par/libfigpar.la \
../ich/libfigich.la \
`pda_link` `agi_link_adam` `grp_link_adam` `ndf_link_adam` `fio_link_adam`


MISC_BUILT_FILES = version.dat figaro1 figaro1.ifl

IFC_FILES = xtplane.ifc xyplane.ifc ycadd.ifc ycdiv.ifc ycmult.ifc ycsub.ifc \
ytplane.ifc growx.ifc growxt.ifc growxy.ifc growy.ifc growyt.ifc \
hard.ifc iadd.ifc ialog.ifc icadd.ifc icdiv.ifc icmult.ifc icsub.ifc \
idev.ifc idiv.ifc igrey.ifc imult.ifc irevx.ifc irevy.ifc isub.ifc \
isxadd.ifc isxdiv.ifc isxmul.ifc isxsub.ifc isyadd.ifc isydiv.ifc \
isymul.ifc isysub.ifc lxset.ifc lyset.ifc medfiltr.ifc xcadd.ifc \
xcdiv.ifc xcmult.ifc xcsub.ifc elsplot.ifc esplot.ifc lsplot.ifc \
medfilt.ifc medsky.ifc msplot.ifc ncset.ifc optextract.ifc polysky.ifc \
profile.ifc q2bad.ifc qual2flag.ifc rembad.ifc renobj.ifc resample.ifc \
rescale.ifc retype.ifc rotx.ifc sclean.ifc scnsky.ifc scross.ifc scrunch.ifc \
setobj.ifc sfit.ifc slice.ifc soft.ifc splot.ifc sqrterr.ifc tippex.ifc \
trimfile.ifc xcur.ifc ystract.ifc adjoin.ifc bclean.ifc ccdlin.ifc ccur.ifc \
cfit.ifc clean.ifc clip.ifc coadd.ifc colour.ifc combine.ifc copobj.ifc \
cosrej.ifc creobj.ifc cset.ifc delobj.ifc dvdplot.ifc errcon.ifc extlist.ifc \
extract.ifc figinfo.ifc flag2qual.ifc fscrunch.ifc goodvar.ifc hcross.ifc \
hist.ifc hopt.ifc icont.ifc iconv3.ifc icor16.ifc icset.ifc icur.ifc idiff.ifc \
igconv.ifc igcur.ifc ilist.ifc ilog.ifc image.ifc iplots.ifc ipower.ifc \
irot90.ifc isedit.ifc ishift.ifc ismooth.ifc isplot.ifc istat.ifc istretch.ifc \
isubset.ifc isuper.ifc ixsmooth.ifc figaro1.ifc

CLEANFILES = $(IFC_FILES) $(MISC_BUILT_FILES)
