$ VERIFY = F$VERIFY( 0 )
$!+
$! Name:
$!    CON_DEV
$!
$! Purpose:
$!    Sets up symbols and logical names for public CONVERT development.
$!
$! Language:
$!    DCL
$!
$! Type of Module:
$!    Command procedure.
$!
$! Invocation:
$!    @CON_DEV
$!
$! Description :
$!    Defines the logical names and symbols for public CONVERT development. 
$!    This includes activating symbols and logicals defined for standard
$!    subroutine packages: CHR, FIO, NDF, PRIMDAT, PSX, and TRANSFORM; and 
$!    Figaro development.  There are also logical names for include files
$!    defined. The CONVERT CMS library is selected.
$!
$! Authors:
$!    MJC: Malcolm J. Currie (STARLINK)
$!    {enter_new_authors_here}
$! 
$! History :
$!    1992 September 23 (MJC):
$!       Original version.
$!    1993 August 20 (MJC):
$!       Renamed from CONVERTDEV for connsistency with other packages.
$!    {enter_changes_here}
$!
$!-
$!  Prepare to run ADAM applications if this has not been done already.
$!
$    IF F$TRNLNM( "ADAM$_INITDONE" ) .NES. "TRUE" THEN ADAMSTART
$!
$!  Set up ADAM development symbols and logical names.
$!
$    ADAMDEV
$!
$!  Start library maintenance.
$!
$    LIBMAINT
$!
$!  Set up for Figaro development.
$!
$    @FIGARO_PROG_N:FIGDEV
$!
$!  Define logicals for FIGARO include files.
$!
$    DEFINE DTACODES FIGPACK_DISK:[FIGPACK.NFIGARO.INC]DTACODES.INC
$    DEFINE DYNAMIC_MEMORY FIGPACK_DISK:[FIGPACK.NFIGARO.INC]DYNAMIC_MEMORY.INC
$!
$!  TRN definitions.
$!
$    @TRANSFORM_DIR:START
$!
$!  Develop CHR applications.
$!
$    CHR_DEV
$!
$!  Develop CNF applications.
$!
$    CNF_DEV
$!
$!  Develop FIO applications.
$!
$    FIO_DEV
$!
$!  Develop NDF applications.
$!
$    NDF_DEV
$!
$!  Develop PAR applications.
$!
$    PAR_DEV
$!
$!  Develop PRIMDAT applications.
$!
$    PRM_DEV
$!
$!  Develop PSX applications.
$!
$    PSX_DEV
$!
$!  Exit the procedure, restoring the verification status.
$!
$    IF ( VERIFY ) THEN SET VERIFY
$!
$    EXIT
