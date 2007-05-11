      SUBROUTINE HDIR( STATUS )
*+
* Name:
*    HDIR

* Purpose:
*    Produce a simple summary of an HDS object.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    hdir inp [dev]

* ADAM Parameters :
*    INP = UNIV (Read)
*       HDS data object object to be summarized. <GLOBAL.HDSOBJ>
*    DEV = _CHAR (Read)
*       Output device (TERMINAL, PRINTER, OLDFILE, NEWFILE etc.).
*       [TERMINAL]

* Description:
*    Produces a simple summary of a named HDS data object to a selected
*    output. For a primitive object the name, type and value are given if
*    scalar, or dimensions if non-scalar.  For a structure the components
*    are listed.

* Examples
*    % hdir file1
*       Produces a summary of file1.sdf on the terminal.
*
*    % hdir file1 summary.lis
*       Produces a summary of file1.sdf in text file summary.lis.
*
*    % hdir file2 '"O=summary.lis"'
*       Appends a summary of file2.sdf to text file summary.lis.
*
*    % hdir file1.array O
*       Appends a summary of component ARRAY of file1.sdf to text file
*       ast_print.lis, assuming environment variable OLDFILE is not set.

* Authors :
*    RJV: R.J.Vallance (Birmingham University)
*    DJA: D.J.Allan (Birmingham University)
*    AJC: A.J.Chipperfield (Starlink, RAL)
*    TIMJ: Tim Jenness (JAC, Hawaii)

* History :
*     ??-???-???? (RJV):
*        V1.0-0 Original
*     08-MAY-1992 (DJA):
*        V1.6-1 Tidied up. Device name lengthed.
*     04-MAY-1994 (DJA):
*        V1.7-0 Use AIO for proper UNIX output
*     24-NOV-1994 (DJA):
*        V1.8-0 Now use USI for user interface
*     28-FEB-1997 (RJV):
*        V2.1-0 Make seperator line correct length
*     06-SEP-2001 (AJC):
*        V3.0-0 Remove Asterix stuff
*               Improve prologue
*     09-MAY-2007 (TIMJ):
*        Bigger buffer for file name
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MSG_PAR'

*    Status :
      INTEGER 			STATUS

*    Local variables :
      CHARACTER*(DAT__SZLOC) 	OBJLOC            	! Data object locator
      INTEGER			OCH			! Output channel id
      INTEGER 			OUTWIDTH                ! Output page width

*    Version id :
      CHARACTER*30 		VERSION
        PARAMETER 		( VERSION = 'HDIR Version 3.0-0' )
*.

*    Set MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Version number
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*    Get parameter values
      CALL DAT_ASSOC( 'INP', 'READ', OBJLOC, STATUS )

*    Connect output device
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, OUTWIDTH, STATUS )

*    Write out information on data object
      CALL HDIR_OUT( OCH, OUTWIDTH, OBJLOC, STATUS )

*    Close output device
      CALL AIO_CANCL( 'DEV', STATUS )

*    Tidy up
      CALL DAT_ANNUL(OBJLOC,STATUS)

      END




*+
      SUBROUTINE HDIR_OUT(OCH,WID,OBJLOC,STATUS)
*    Description :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) OBJLOC        ! locator to data object
      INTEGER			OCH			! Output channel id
      INTEGER			WID			! Output width
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) COMPLOC    ! locator to structure component
      CHARACTER*512 PATH                ! full object name from top level
      CHARACTER*256 FILE                 ! VMS file name
      CHARACTER*132 LINE                 ! dashed line to make output pretty
      CHARACTER*256 STR                  ! string containing object info
      INTEGER L				! used length of string
      INTEGER NLEV                      ! levels of structure
      INTEGER NCOMP                     ! components in structure
      INTEGER ICOMP                     ! index to component
      INTEGER NDIMS                     ! dimensionality of object
      INTEGER DIMS(DAT__MXDIM)          ! actual dimensions
      LOGICAL STRUCT                    ! whether object a structure
*    Functions :
      INTEGER CHR_LEN
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get full file and object names
      CALL HDS_TRACE(OBJLOC,NLEV,PATH,FILE,STATUS)
      CALL DAT_STRUC(OBJLOC,STRUCT,STATUS)
      CALL DAT_SHAPE(OBJLOC,DAT__MXDIM,DIMS,NDIMS,STATUS)
      IF (STATUS.EQ.SAI__OK) THEN

*      Write out header
        CALL CHR_FILL('-',LINE)
        CALL AIO_WRITE( OCH, LINE(:WID), STATUS )
        L=CHR_LEN(FILE)
        CALL AIO_WRITE( OCH, 'File name===>  '//FILE(:L), STATUS )
        L=CHR_LEN(PATH)
        CALL AIO_WRITE( OCH, 'Object name=>  '//PATH(:L), STATUS )

*      Write out basic attributes
        CALL STR_OBDESC( OBJLOC, STR, STATUS )
        CALL AIO_BLNK( OCH, STATUS )
        CALL AIO_WRITE( OCH, STR, STATUS )
        CALL AIO_BLNK( OCH, STATUS )

*      If a scalar structure list components
        IF (STRUCT.AND.NDIMS.EQ.0) THEN
          CALL AIO_WRITE( OCH, 'Contents:', STATUS )
          CALL DAT_NCOMP(OBJLOC,NCOMP,STATUS)
          IF (NCOMP.EQ.0) THEN
            CALL AIO_WRITE( OCH, 'Contents undefined', STATUS )
          ENDIF
          DO ICOMP=1,NCOMP
            CALL DAT_INDEX(OBJLOC,ICOMP,COMPLOC,STATUS)
            CALL STR_OBDESC(COMPLOC,STR,STATUS)
            CALL AIO_WRITE( OCH, STR, STATUS )
            CALL DAT_ANNUL(COMPLOC,STATUS)
          ENDDO
        ENDIF

      CALL AIO_WRITE( OCH, LINE(:WID), STATUS )
      CALL AIO_BLNK( OCH, STATUS )

      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP( 'HDIR_OUT', 'HDIR_OUT', STATUS )
      ENDIF
      ENDIF

      END
