$ VERIFY = F$VERIFY( 0 )
$!+
$! Name:
$!    MAKE_CONVERT
$!
$! Purpose:
$!    Builds the CONVERT M-task.
$!
$! Language:
$!    DCL
$!
$! Type of Module:
$!    Command procedure.
$!
$! Invocation:
$!    @MAKE_CONVERT
$!
$! Description:
$!    Generates the CONVERT executable module as an ADAM monolith.
$!    CONVERT.EXE is purged.
$!
$! Arguments:
$!    P1 --- Used to specify linking qualifiers, such as "/MAP/CROSS" or
$!           "/DEBUG".  Normally, it will be null.
$!
$! Output:
$!    -  The CONVERT executable.
$!
$! Notes:
$!    A monolith interface file should have been prepared (made and
$!    compiled) by use of the CONVERT_DIR:MAKE_CONVERT_IFL.COM before
$!    loading the new CONVERT monolith if any of the interface files have
$!    altered.
$!
$! Prior Requirements:
$!    It is assumed that you are logged in for ADAM and Figaro
$!    development (the procedure CONVERT_DIR:CON_DEV.COM will do
$!    this) and you are currently in the directory where CONVERT is to
$!    be built.  
$!
$! Authors:
$!    MJC: Malcolm J. Currie (STARLINK)
$!    {enter_new_authors_here}
$!
$! History:
$!    1992 September 7 (MJC):
$!       Original version based on MAKE_KAPPA.
$!    1992 September 29 (MJC):
$!       Added IRAF libraries and used released FIO.
$!    1992 September 30 (MJC):
$!       Used options file to reduce size of executables.
$!    1993 July 30 (MJC):
$!       Link with FIO explicitly, and released PAR library.
$!    1993 August 21 (MJC):
$!       Extract the monolith object module.
$!    {enter_further_changes_here}
$!
$!-
$!
$!  Extract the module for monolith.
$!
$ LIBRARY/EXTRACT=CONVERT/OUTPUT=CONVERT CONVERT_DIR:CONVERT
$!
$!  Just link the top-level routine into all the relevant libraries
$!  using the Adam command MLINK.
$!
$ MLINK CONVERT.OBJ,-
CONVERT_DIR:CONVERT/LIB,-
CONVERT_DIR:CONVERT/OPT,-
INTERIM/LIB/INCLUDE=(STL_DATA),-
PRM_LINK/OPT,-
FIGARO_LIBS:DTA/LIB,-
DYN/LIB,-
CNV/LIB,-
FIO_LINK_ADAM/OPT,-
KAPPA_DIR:AIF/LIB,-
CONVERT_DIR:LIBIMFORT/LIB,-
CONVERT_DIR:LIBSYS/LIB,-
CONVERT_DIR:LIBVOPS/LIB,-
CONVERT_DIR:LIBOS/LIB,- 
SYS$LIBRARY:VAXCRTL/L 'P1'
$!
$!  Purge old versions of the executable file.
$!
$    PURGE/NOLOG/NOCONFIRM CONVERT.EXE
$    DELETE CONVERT.OBJ;0
$!
$!  Exit the procedure.
$!
$    IF ( VERIFY ) THEN SET VERIFY
$!
$    EXIT
$!
