# itkConfig.sh --
# 
# This shell script (for sh) is generated automatically by Itk's
# configure script.  It will create shell variables for most of
# the configuration options discovered by the configure script.
# This script is intended to be included by the configure scripts
# for Itk extensions so that they don't have to figure this all
# out for themselves.  This file does not duplicate information
# already provided by tclConfig.sh, tkConfig.sh or itclConfig.sh,
# so you may need to use those files in addition to this one.
#
# The information in this file is specific to a single platform.

# Itcl's version number.
ITCL_VERSION='3.0'
ITCL_MAJOR_VERSION='3'
ITCL_MINOR_VERSION='0'

# The name of the Itk library (may be either a .a file or a shared library):
ITK_LIB_FILE=libitk3.0.a

# String to pass to linker to pick up the Itk library from its
# build directory.
ITK_BUILD_LIB_SPEC='-L/loc/pwda/pdraper/gaiadev/top/tcl/itcl3.0.1/itk/unix -litk3.0'

# String to pass to linker to pick up the Itk library from its
# installed directory.
ITK_LIB_SPEC='-L/loc/pwda/pdraper/gaiadev/top/built/lib -litk3.0'

# Location of the top-level source directories from which [incr Tk]
# was built.  This is the directory that contains a README file as well
# as subdirectories such as generic, unix, etc.  If [incr Tk] was
# compiled in a different place than the directory containing the source
# files, this points to the location of the sources, not the location
# where [incr Tk] was compiled.
ITK_SRC_DIR='/loc/pwda/pdraper/gaiadev/top/tcl/itcl3.0.1/itk'
