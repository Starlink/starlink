# Starlink M4 macros for autoconf
# This file is part of starconf version 1.0
# DO NOT EDIT: it may be overwritten when starconf is next run



# STAR_DEFAULTS
# -------------
# Defaults for Starlink configure.ac files
AC_DEFUN([STAR_DEFAULTS],
[AC_REQUIRE([_STAR_BOILERPLATE])dnl
# Everything depends on where /star is.  Declare STARLINK as a
# `precious variable'.  Amongst other things, this will make
# ./configure squeal if the package is re-configured with an
# inconsistent value of this variable.
AC_ARG_VAR(STARLINK, [Where all the Starlink software lives])dnl

# AC_SUBST the STARLINK variable.  Macro AC_ARG_VAR does this anyway,
# but automake doesn't know that (in 1.6 at least): however any
# variable that automake finds has been AC_SUBSTed, it includes in
# Makefile.in, and that is useful to us.
AC_SUBST(STARLINK)

# Ensure that STARLINK has a value, defaulting to /export3/sun.  
# Note that this directory will be different from /star, if the
# `starconf' component was configured with a non-default value of STARLINK
test -n "$STARLINK" || STARLINK=/export3/sun

# Install software in /export3/sun by default
AC_PREFIX_DEFAULT(/export3/sun)dnl
dnl    Don't include a dependency of acinclude.m4 on this .m4 file, or on
dnl    starconf.status: we can't have it depending on starconf.status,
dnl    since that isn't distributed (only its products are
dnl    distributed), and we can't just have an empty dependency with
dnl    ./starconf.status as the rule, since this file must exist
dnl    before any Makefile is run.  Another way of putting this is
dnl    that, in order for this rule to be discovered and included in a
dnl    Makefile, we must _already_ have an up-to-date acinclude.m4,
dnl    which means that the rule is completely redundant.
#
# Always create a manifest-install target
_STAR_MAKE_MANIFEST

FFLAGS="$FFLAGS -I$STARLINK/include"
CFLAGS="$CFLAGS -I$STARLINK/include"

# Dependencies declarations and checks (see macros
# [STAR_DECLARE_BUILD_DEPENDENCIES] and [STAR_DECLARE_USE_DEPENDENCIES])
STAR_BUILD_DEPENDENCIES='starconf'
AC_SUBST(STAR_BUILD_DEPENDENCIES)dnl
STAR_USE_DEPENDENCIES=''
AC_SUBST(STAR_USE_DEPENDENCIES)dnl

# List of documentation.  See [STAR_LATEX_DOCUMENTATION].
# STAR_DOCUMENTATION is a list of document codes,
STAR_DOCUMENTATION=
AC_SUBST([STAR_DOCUMENTATION])
dnl    Note: the following adds an EXTRA_DIST variable to the
dnl    generated Makefile.  automake generates Makefile.in in such a
dnl    way that that the contents of the $(EXTRA_DIST) variable is
dnl    added to the distribution; thus we are making use of
dnl    undocumented behaviour here.  However it's safe, since automake
dnl    is documented to support the EXTRA_DIST variable being used, in
dnl    such a way that (I think) it would always be present as a
dnl    Makefile.in variable.
dnl
dnl    Create a shell variable EXTRA_DIST, and call AC_SUBST on it.
dnl    This means that automake will create a Make variable in
dnl    Makefile.in, of the form `EXTRA_DIST = @EXTRA_DIST@', _unless_
dnl    the variable EXTRA_DIST is declared in Makefile.am, in which
dnl    case it will have that value.  Therefore in this case, the
dnl    Makefile.am should declare `EXTRA_DIST = XXX @EXTRA_DIST@' so
dnl    that the extra files built up here are appended to the list.
dnl    Needs to be prominently documented!
EXTRA_DIST=
AC_SUBST(EXTRA_DIST)
BUILT_SOURCES=
AC_SUBST(BUILT_SOURCES)

dnl NO -- CHANGED -- now mandate that users use STAR_EXTRA_DIST instead
dnl _STAR_EXTRA_DIST_TARGETS=
dnl AC_SUBST([_STAR_EXTRA_DIST_TARGETS])
dnl _STAR_BUILT_SOURCES_TARGETS=
dnl AC_SUBST([_STAR_BUILT_SOURCES_TARGETS])
dnl cat >>$STAR_BOILERPLATE <<\_EOD
dnl EXTRA_DIST = $(_STAR_EXTRA_DIST_TARGETS) $(STAR_EXTRA_DIST)
dnl BUILT_SOURCES = $(_STAR_BUILT_SOURCES_TARGETS) $(STAR_BUILT_SOURCES)
dnl _EOD
dnl    The following are hooks in Makefile.starconf

_STAR_INSTALL_DATA_LOCAL=
AC_SUBST(_STAR_INSTALL_DATA_LOCAL)
_STAR_INSTALL_DATA_HOOK=
AC_SUBST(_STAR_INSTALL_DATA_HOOK)
])# STAR_DEFAULTS



# STAR_MESSGEN([msgfile=$PACKAGE_err.msg])
# ----------------------------------------
#
# Handle generating message, error, and facility files.
#
# The argument is the name of the file with the message declarations
# in it, in the format prescribed by the messgen utility.  This
# defaults to ${PACKAGE}_err.msg.  The macro may be called more than
# once if you have more than one .msg file in the directory.
#
# This adds Makefile rules to create the appropriate _err, _ERR,
# _err.h and 
# fac_nnn_err files, and handles installing the latter file to the
# help directory, and including it in the distribution.
#
# The files which this generates should be declared as sources in
# the Makefile.am if they are to be used in code, or are to be
# installed.  If they are used in building the package, you will
# probably need to declare them additionally as `BUILT_SOURCES'.  The
# package also implicitly declares a build-time dependency on the
# messgen package.
#
AC_DEFUN([STAR_MESSGEN],
         [AC_REQUIRE([_STAR_BOILERPLATE])dnl
          _s_facfile=m4_ifval([$1], [$1], [${PACKAGE}_err.msg])
          if test -r $_s_facfile; then
              : OK
          else
              echo "Facility file $_s_facfile missing or not readable"
              exit 1
          fi
          # Extract the facility name and code from the .msg file
          # Format is `.FACILITY name,code/PREFIX=xxx'
          eval [`sed -n '/\.FACILITY/{s/.*\.FACILITY *\([a-zA-Z]*\) *, *\([0-9]*\).*/_s_facname=\1; _s_faccode=\2;/p;q;}' $_s_facfile`]
          _s_facname=`echo $_s_facname | tr '[A-Z]' '[a-z]'`
          _s_FACNAME=`echo $_s_facname | tr '[a-z]' '[A-Z]'`
          cat >>$STAR_BOILERPLATE <<EOD
#  Rules for generating files built by the error message system.
#  ------------------------------------------------------------
${_s_FACNAME}_ERR:     $_s_facfile; \$(STARLINK)/bin/messgen -F $_s_facfile
${_s_facname}_err:     $_s_facfile; \$(STARLINK)/bin/messgen -f $_s_facfile
${_s_facname}_err.h:   $_s_facfile; \$(STARLINK)/bin/messgen -c $_s_facfile
fac_${_s_faccode}_err: $_s_facfile; \$(STARLINK)/bin/messgen -e $_s_facfile
EOD
          STAR_DECLARE_BUILD_DEPENDENCIES(messgen)
          STAR_HELP_FILES(fac_${_s_faccode}_err)
          _STAR_EXTRA_DIST(fac_${_s_faccode}_err)
])# STAR_MESSGEN



## # STAR_FACILITY_CODE(code, [fac=$PACKAGE], [msgfile=$fac_err.msg])
## # ----------------------------------------------------------------
## # Declare the facility code corresponding to this package.
## #
## # Arg 1 is numeric facility code, set to ERR_FACILITY_CODE and AC_SUBSTed
## #
## # Arg 2 is the facility name, set to ERR_FACILITY_NAME and
## # AC_SUBSTed.  This defaults to the package name.
## #
## # Arg 3 is the name of the source .msg file, which defaults to
## # $fac_err.msg, and thus defaults to $PACKAGE_err.msg if argument 2 is
## # unspecified.
## #
## # About the only time when you need to use non-default arguments here
## # is when you have more than one facility file in a directory, at
## # least one of which (necessarily) has a facility name different from
## # the package.
## #
## # In either case, the message file should be made available in a
## # message source file (default $PACKAGE_err.msg.in) which has the
## # facility code as @ERR_FACILITY_CODE@, and this should be listed in a
## # AC_CONFIG_FILES line.
## #
## # The generated _err files should be listed as sources in the
## # Makefile.am if they are used in code, or are to be installed.
## #
## # As a side-effect of this macro, the file fac_code_err is installed
## # in the help directory, and added to the distribution.
## #
## dnl AC_DEFUN([STAR_FACILITY_CODE],
## dnl 	 [AC_REQUIRE([_STAR_BOILERPLATE])dnl
## dnl ERR_FACILITY_CODE=$1
## dnl AC_SUBST(ERR_FACILITY_CODE)dnl
## dnl ERR_FACILITY_FILES="${PACKAGE}_err ${PACKAGE}_err.h fac_$1_err"
## dnl AC_SUBST(ERR_FACILITY_FILES)dnl
## dnl _star_tmp=m4_ifval([$2], [$2], [${PACKAGE}_err.msg])
## dnl _star_tmp2=`echo ${PACKAGE}_err | tr '[a-z]' '[A-Z]'`
## dnl cat >>$STAR_BOILERPLATE <<EOD
## dnl #  Rules for generating files built by the error message system.
## dnl #  ------------------------------------------------------------
## dnl $_star_tmp2:      $_star_tmp; \$(STARLINK)/bin/messgen -F $_star_tmp
## dnl ${PACKAGE}_err:   $_star_tmp; \$(STARLINK)/bin/messgen -f $_star_tmp
## dnl ${PACKAGE}_err.h: $_star_tmp; \$(STARLINK)/bin/messgen -c $_star_tmp
## dnl fac_$1_err:       $_star_tmp; \$(STARLINK)/bin/messgen -e $_star_tmp
## dnl EOD
## dnl STAR_DECLARE_BUILD_DEPENDENCIES(messgen)
## dnl STAR_HELP_FILES(fac_$1_err)
## dnl _STAR_EXTRA_DIST(fac_$1_err)
## dnl ])# STAR_FACILITY_CODE



# _STAR_MAKE_MANIFEST
# ------------------
#
# At install time, also make a manifest, by adding the install target
# `manifest-install'.  This wraps the autodiscovered install command
# using the script install-with-manifest.  Internal macro, called from
# within STAR_DEFAULTS.
#
# This isn't completely satisfactory, since the install-with-manifest
# script (currently) simply appends names to a file called MANIFEST,
# and it has no way of either clearing that file before the
# installation starts or renaming it away after it completes (automake
# has install hooks, but the configuration work here is being done on
# the Makefile.in _after_ automake completes).  This problem is dealt
# with to some extent by creating the `manifest-install' target.
#
# As a separate problem, there's nothing to stop a parallelised make
# setting off two copies of the script, so that two processes try to
# append to the file simultaneously.  A fix to this would probably
# need to be sophisticated enough that it could fix the other problem
# as well.
#
# Dependency: the makefile fragment below relies on starconf correctly
# including install-with-manifest.in
AC_DEFUN([_STAR_MAKE_MANIFEST],
[#
AC_REQUIRE([_STAR_BOILERPLATE])
# Declare where the manifests should live.  This is slightly
# complicated: we want to use this at configure time to check that
# dependencies have in fact been installed, but it ought to depend on
# $(prefix), since the user has the freedom to install things
# elsewhere, and will expect the manifests to go in the same place.
# We can't do anything other than accept, and document, that the user
# can confuse things here.  It doesn't matter too much, really, as the
# dependency check isn't crucial.  XXX revisit this.
#
# Ooooh, bad, this logic is probably mildly implementation dependent
if test x$prefix = xNONE; then
    current_MANIFESTS=$ac_default_prefix/manifests
else
    current_MANIFESTS=$prefix/manifests
fi
STAR_MANIFESTDIR='$(prefix)/manifests'
AC_SUBST(STAR_MANIFESTDIR)
# Although PACKAGE_VERSION is a default output variable, it isn't
# added as a Makefile variable by default.  We need it below, however,
# so add it now.
AC_SUBST(PACKAGE_VERSION)
dnl   The $[]@ quoting below is because $@ means rest-of-args to m4
cat >>$STAR_BOILERPLATE <<\_EOD
# Create an install manifest, and install it in the required manifest location
# Create install-manifest.xml file, using the MANIFEST file generated
# by install-with-manifest
install-manifest.xml: install-with-manifest
	rm -f MANIFEST; touch MANIFEST
	$(MAKE) INSTALL=./install-with-manifest install
	rm -f $[]@
	echo "<?xml version='1.0'?>"                     >$[]@
	echo "<!DOCTYPE manifest SYSTEM 'componentinfo.dtd'>" >>$[]@
	echo "<manifest component='$(PACKAGE)'>"         >>$[]@
	echo "<version>$(PACKAGE_VERSION)</version>"     >>$[]@
	echo "<files>"                                   >>$[]@
	cat MANIFEST                                     >>$[]@
	echo "</files>"                                  >>$[]@
	echo "</manifest>"                               >>$[]@
	rm -f MANIFEST
# Install the install-manifest.xml into the $(STAR_MANIFESTDIR) directory.
# We should respect $(DESTDIR), but needn't worry about $(srcdir),
# since we know that the file was created in the current directory.
install-manifest: install-manifest.xml
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(STAR_MANIFESTDIR)
	$(INSTALL_DATA) install-manifest.xml \
		$(DESTDIR)$(STAR_MANIFESTDIR)/$(PACKAGE)
# The following rule is redundant (install-with-manifest.in must exist
# in order for ./configure to run without error), but leave it in for
# its documentation value.
install-with-manifest.in:
	./starconf.status
_EOD
AC_CONFIG_FILES(install-with-manifest, [chmod +x install-with-manifest])
])# _STAR_MAKE_MANIFEST



# STAR_LATEX_DOCUMENTATION(documentcode, [targets])
# -------------------------------------------------
# Generate the standard makefile targets to handle LaTeX documentation
# source.  The parameter documentcode should be something like
# `sun123' -- it should not include any .tex extension.
#
# The second, optional, argument gives an explicit list of the targets
# which are build.  If this is _not_ specified, then a standard list
# is used (.tex, .ps and .tar_htx) and corresponding rules added to
# the generated makefile.  If it is specified (to any value, including
# ''), then its value is a list of files which are to be added to the
# distribution, and no extra Makefile rules are added.  Thus if users need
# anything complicated done, they should use this second argument and
# provide rules for satisfying the given targets.
AC_DEFUN([STAR_LATEX_DOCUMENTATION],
[AC_REQUIRE([_STAR_BOILERPLATE])dnl
m4_ifval([$1], [], [AC_FATAL([$0: called with no documentcode])])dnl
STAR_DOCUMENTATION="$STAR_DOCUMENTATION $1"
m4_ifval([$2],
         [dnl non-empty second argument -- just add to variable
	  _STAR_EXTRA_DIST($2)],
	 [dnl second arg empty -- use defaults
	  for d in $1; do
              docno=`basename $d .tex`
              cat >>$STAR_BOILERPLATE <<EOD
$docno.dvi: $docno.tex
	latex $docno
$docno.ps: $docno.dvi
	dvips -o $docno.ps $docno.dvi
$docno.htx: $docno.tex
	star2html $docno.tex
$docno.htx_tar: $docno.htx
	tar cf $docno.htx_tar $docno.htx
EOD
	  done
	  _STAR_EXTRA_DIST($docno.tex $docno.ps $docno.htx_tar)
	  STAR_DECLARE_BUILD_DEPENDENCIES(star2html)
])dnl
])# STAR_LATEX_DOCUMENTATION



# STAR_HELP_FILES(helpfiles)
# --------------------------
#
# Declare a list of files to be installed into the Starlink help
# directory.  This can be used both internally and in user
# configure.ac files.
#
AC_DEFUN([STAR_HELP_FILES],
[AC_REQUIRE([_STAR_BOILERPLATE])dnl
m4_ifval([$1], [], [AC_FATAL([$0: called with no helpfiles])])dnl
if test "X$_STAR_INSTALL_HELP" = X; then
    # First time
    STAR_HELPDIR='$(prefix)/help'
    AC_SUBST(STAR_HELPDIR)
    AC_SUBST(_STAR_INSTALL_HELP)
    cat >>$STAR_BOILERPLATE <<\_EOD
install-starhelp: $(_STAR_INSTALL_HELP) 
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(STAR_HELPDIR)
	@list='$(_STAR_INSTALL_HELP)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " $(INSTALL_DATA) $$d$$p $(DESTDIR)$(STAR_HELPDIR)/$$f"; \
	  $(INSTALL_DATA) $$d$$p $(DESTDIR)$(STAR_HELPDIR)/$$f; \
	done
_EOD
fi
_STAR_INSTALL_HELP="$_STAR_INSTALL_HELP $1"
_STAR_INSTALL_DATA_LOCAL="$_STAR_INSTALL_DATA_LOCAL install-starhelp"
])# STAR_HELP_FILES



# STAR_DECLARE_BUILD_DEPENDENCIES(packagelist)
# --------------------------------------------
#
# Declare a space-separated list of packages/components which the
# build of this package depends on.  The packages here are tested to
# ensure thay do exist (XXX possibly precarious, for the reasons
# mentioned in _STAR_MAKE_MANIFEST -- should revisit this), and the
# list is available for substitution in the AC_SUBST-ed variable
# [@STAR_BUILD_DEPENDENCIES@].
#
# The macro may be called several times.
AC_DEFUN([STAR_DECLARE_BUILD_DEPENDENCIES],
         [#
          m4_ifval([$1], [],
                         [AC_FATAL([$0: called with empty packagelist])])dnl
          STAR_BUILD_DEPENDENCIES="$STAR_BUILD_DEPENDENCIES $1"
          for p in $1
          do
              test -e $current_MANIFESTS/$p || \
                  echo "Package $PACKAGE depends on $p, but that doesn't appear to be installed"
          done
])# STAR_DECLARE_BUILD_DEPENDENCIES



# STAR_DECLARE_USE_DEPENDENCIES(packagelist)
# ------------------------------------------
#
# Declare a space-separated list of packages/components which the use
# of this package depends on.  The packages here are tested to ensure
# thay do exist (XXX not yet), and the list is available for
# substitution in the [AC_SUBST]-ed variable [@STAR_USE_DEPENDENCIES@].
#
# The macro may be called several times.
AC_DEFUN([STAR_DECLARE_USE_DEPENDENCIES],
         [#
          m4_ifval([$1], [],
                         [AC_FATAL([$0: called with empty packagelist])])dnl
         STAR_USE_DEPENDENCIES="$STAR_USE_DEPENDENCIES $1"
])# STAR_DECLARE_USE_DEPENDENCIES



# STAR_PLATFORM_SOURCES(target-file, platform-list)
# -------------------------------------------------
#
# Generate the given target-file, by selecting the appropriate
# platform-list based on the value of [AC_CANONICAL_BUILD].
#
# For each of the platforms, p, in platform-list, there should be a
# file `<target-file>p'.  There should always be a file
# `<target-file>default', and if none of the platform-list strings
# matches, this is the file which is used.  If the `default' file is
# listed in the `platform-list', then it is matched in the normal run
# of things; if it is not listed, it still matches, but a warning is
# issued.
#
# If you wish no match _not_ to be an error -- perhaps because there
# is a platform-dependent file which is redundant on unlisted platforms
# -- then end the platform-list with `NONE'.  In this case, if no file
# matches, then no link is made, with no error or warning.
#
# This macro uses the results of ./config.guess to determine the
# current platform.  That returns a triple consisting of
# cpu-vendor-os, such as `i686-pc-linux-gnu' (OS=linux-gnu),
# `sparc-sun-solaris2.9', or `alphaev6-dec-osf5.1'
#
# The extensions in platform-list should all have the form
# `cpu_vendor[_os]', where each of the components `cpu', `vendor' and
# `os' may be blank.  If not blank, they are matched as a prefix of
# the corresponding part of the config.guess value.  Thus
# `_sun_solaris' would match `sparc-sun-solaris2.9' but not
# `sparc-sun-sunos', and `_sun' would match both.
#
AC_DEFUN([STAR_PLATFORM_SOURCES],
         [
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
m4_ifval([$1], [], [AC_FATAL([$0: no target-file given])])dnl
m4_ifval([$2], [], [AC_FATAL([$0: no platform-list given])])dnl
AC_MSG_CHECKING([platform-specific source for $1])
_star_tmp=default
for platform in $2
do
    if test $platform = NONE; then
        # Special case -- no file required
        _star_tmp=NONE
        break;
    fi
    if expr $build : `echo $platform | sed 's/_/.*-/g'` >/dev/null; then
        _star_tmp=$platform
        break;
    fi
done
if test $_star_tmp = NONE; then
    AC_MSG_RESULT([none required])
else
    if test -e $srcdir/$1$_star_tmp; then
        AC_MSG_RESULT([using $1$_star_tmp])
        (cd $srcdir; rm -f $1; $LN_S $1$_star_tmp $1)
    else
        AC_MSG_ERROR([platform $_star_tmp matched, but no file $1$_star_tmp found])
    fi
    if test $_star_tmp = default; then
        AC_MSG_WARN([build platform $build does not match any of ($2): using `default'])
    fi
fi
])# STAR_PLATFORM_SOURCES



# starconf internal macros



# _STAR_EXTRA_DIST(filelist)
# --------------------------
#
# Declare a space-separated list of files which should be included in
# the distribution.  This has the same function as automake's
# EXTRA_DIST rule, and the way it should be used is as follows
#
# If there is no EXTRA_DIST variable set in the Makefile.am, then a
# variable is created in Makefile.in, of the form `EXTRA_DIST =
# @EXTRA_DIST@', and it is the accumulated value of EXTRA_DIST which
# is substituted in.  If you use EXTRA_DIST in the Makefile.am, you
# should use it as `EXTRA_DIST = xxx @EXTRA_DIST@', so that the values
# here will be appended to it
#
AC_DEFUN([_STAR_EXTRA_DIST],
	 [m4_ifval([$1], [], 
                         [AC_FATAL([$0: called with empty filelist])])dnl
          EXTRA_DIST="$EXTRA_DIST $1"
])# _STAR_EXTRA_DIST



# _STAR_BUILT_SOURCES(filelist)
# ----------------------------
#
# Declare a space-separated list of files which are `generated
# sources', in automake parlance, and so should be built early.
#
# This has a similar status to, and is used in the same way as
# EXTRA_DIST -- see _STAR_EXTRA_DIST for details.
#
AC_DEFUN([_STAR_BUILT_SOURCES],
	 [m4_ifval([$1], [], 
                         [AC_FATAL([$0: called with empty filelist])])dnl
          BUILT_SOURCES="$BUILT_SOURCES $1"
])# _STAR_BUILT_SOURCES



# _STAR_BOILERPLATE
# -----------------------
# Utility macro.  If the variable STAR_BOILERPLATE is not already defined,
# define it to be a file which will contain boilerplate fragments of
# makefile, which will be appended to the Makefile at the end of the
# configuration run.
# 
# Any macros which need to write boilerplate should [AC_REQUIRE] this
# macro, then write their boilerplate to the file $STAR_BOILERPLATE
AC_DEFUN([_STAR_BOILERPLATE],
[if test -z "$STAR_BOILERPLATE"; then
  STAR_BOILERPLATE=Makefile.boilerplate.$$
  cat >$STAR_BOILERPLATE <<\_EOD
# Starlink boilerplate additions to Makefile
# Generated by ./configure and starconf
#
_EOD
  AC_CONFIG_COMMANDS([_star_boilerplate],
                     [#
    if test -n "$_star_boilerplate" -a -e "$_star_boilerplate"; then
        echo ""                                         >>Makefile
        cat $_star_boilerplate                          >>Makefile
        rm -f $_star_boilerplate
    else
        : This is possibly an error, but worry not
    fi], [_star_boilerplate=$STAR_BOILERPLATE])
fi])# _STAR_BOILERPLATE



# end of starconf macros
