*+  HDIR - gives list (directory) of HDS data object
      SUBROUTINE HDIR( STATUS )
*
*    Description :
*
*      Produces a simple summary of a named HDS data object to a selected
*      output device which may be user CONSOLE, linePRINTER or text file
*      with logical name AST_LIST.  In latter case output
*      may be appended to an OLDFILE or written to a NEWFILE.  For a
*      primitive object the name, type and value are given if scalar or
*      dimensions if non scalar.  For a structure the components are listed
*      - primitives as above and structures named and dimensioned
*
*    Parameters :
*      INP = HDS data object
*             object to be HDIRd
*      DEVICE = CHAR
*             output device { C)ONSOLE,P)RINTER,O)LDFILE,N)EWFILE }
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     R.J.Vallance (BHVAD::RJV)
*
*    History :
*
*     ?? ??? ?? : V1.0-0  Original (BHVAD::RJV)
*      8 May 92 : V1.6-1  Tidied up. Device name lengthed. (BHVAD::DJA)
*      4 May 94 : V1.7-0  Use AIO for proper UNIX output (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) 	OBJLOC            	! Data object locator
      INTEGER			OCH			! Output channel id
      INTEGER 			OUTWIDTH                ! Output page width
*
*    Version id :
*
      CHARACTER*30 		VERSION
        PARAMETER 		( VERSION = 'HDIR Version 1.7-0' )
*-

*    Version number
      CALL MSG_PRNT( VERSION )

*    Get parameter values
      CALL DAT_ASSOC( 'INP', 'READ', OBJLOC, STATUS )

*    Connect output device
      CALL AIO_ASSOCO( 'DEVICE', 'LIST', OCH, OUTWIDTH, STATUS )

*    Write out information on data object
      CALL HDIR_OUT( OCH, OUTWIDTH, OBJLOC, STATUS )

*    Close output device
      CALL AIO_CANCL( 'DEVICE', STATUS )

*    Tidy up
 99   CALL AST_ERR( STATUS )

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
      CHARACTER*100 PATH                ! full object name from top level
      CHARACTER*80 FILE                 ! VMS file name
      CHARACTER*132 LINE                 ! dashed line to make output pretty
      CHARACTER*80 STR                  ! string containing object info
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
          ENDDO
        ENDIF

      CALL AIO_WRITE( OCH, LINE, STATUS )
      CALL AIO_BLNK( OCH, STATUS )

      IF (STATUS.NE.SAI__OK) THEN
        CALL AST_REXIT( 'HDIR_OUT', STATUS )
      ENDIF
      ENDIF

      END
