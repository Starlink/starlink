      SUBROUTINE HTAB( STATUS )
*+
* Name:
*    HTAB

* Purpose:
*    Display one or more vector objects in table form.

* Language:
*    Fortran 77

* Type of Module:
*    ADAM A-task

* Usage:
*    htab in1 in2 ... [dev=] [slice=] [width=]

* Description:
*    Provides the facility to display vector HDS data objects
*    simultaneously in a tabular form on a selected output. The range
*    to be output is selectable but by default the whole of each object
*    is output.
*
*    Note that the input object names must either be given on the
*    command line or the PROMPT keyword must be used.

* ADAM Parameters:
*    INPn = UNIV (Read)
*       nth object in table. Up to six objects may be specified,
*       <GLOBAL.HDSOBJ> for INP1, [! (no more objects)] for INP2 - 6.
*    DEV = _CHAR (Read)
*       Output device (TERMINAL, PRINTER, OLDFILE, NEWFILE etc.).
*       [TERMINAL]
*    SLICE = _CHAR (Read)
*       Range of data to be output in form "n1:n2" ":n2" or "n1:".
*       [* (whole object)]
*    WIDTH = _INTEGER (Read)
*       Output page width. A null (!) will result in a width appropriate
*       for the chosen text output device. [!]

* Method:

* Examples:
*    % htab vec1 vec2 vec3 dev=n=vec.lis
*       Tabulate the three vectors to file vec.lis
*
*    % htab vec1 vec2 vec3 dev=printer slice=1:10
*       Tabulate the first ten elements of the given vectors on printer,
*
*    % htab vec1 vec2 vec3 vec4 vec5 vec6 width=132
*       Tabulate six vectors to terminal in 132 column mode

* Authors:
*    RJV: R.J. Vallance (Birmingham University)
*    DJA: D.J. Allan (Birmingham University)
*    AJC: A.J.Chipperfield (Starlink, RAL)

* History:
*     ?-???-???? (RJV):
*       Original Version
*     5-DEC-1990 (DJA):
*       V1.0-0 Allows full 80 character device name
*    15-APR-1991 (DJA):
*       V1.4-0 6 digit array index rather than 5
*    22-MAY-1992 (DJA):
*       V1.6-0 ERR_ANNULs inserted.
*       Uses PRS_GETSLICE for slice parsing.
*       Use USI for object association
*    24-NOV-1994 (DJA):
*       V1.8-0 Now use USI for user interface
*    18-JAN-1996 (DJA):
*       V2.0-0 Updated USI routines
*     6-SEP-2001 (AJC):
*       V3.0-0 Remove Asterix stuff
*       Improve prologue
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MSG_PAR'

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC(7)      ! locators to objects in table
      CHARACTER*4 INP		         ! parameter name
      CHARACTER*20 SLICE

      INTEGER 			DEFWIDTH                ! width of output
      INTEGER 			DIMS(DAT__MXDIM)        ! dimensions of object
      INTEGER 			IOBJ                    ! index to object
      INTEGER LEN
      INTEGER			OCH			! Output channel id
      INTEGER NOBJ                       ! number of objects in table
      INTEGER NMAX			 ! maximum objects in table
      INTEGER NDIM                       ! dimensionality of object
      INTEGER RANGES(2,DAT__MXDIM)       ! ranges of data to be output
      INTEGER WIDTH

      LOGICAL PRIM                       ! whether object primitive
      LOGICAL MORE                       ! whether more objects to be included

*    Version id :
      CHARACTER*30       VERSION
        PARAMETER        ( VERSION = 'HTAB Version 3.0-0' )
*-

*    Set MSG environment
      CALL MSG_TUNE( 'ENVIRONMENT', 0, STATUS )

*    Version id
      CALL MSG_OUTIF( MSG__NORM, ' ', VERSION, STATUS )

*    Set up output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, DEFWIDTH, STATUS )
      CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
        WIDTH=DEFWIDTH
        CALL ERR_ANNUL( STATUS )
      END IF
      IF ( WIDTH .LE. 80 ) THEN
        NMAX=3
      ELSE
        NMAX=6
      END IF

*    Get input objects
      NOBJ = 0
      MORE = .TRUE.
      DO WHILE (NOBJ.LT.NMAX.AND.MORE.AND.STATUS.EQ.SAI__OK)

*      Construct parameter and associate with object
        WRITE(INP,'(A3,I1)') 'INP', NOBJ+1
        CALL DAT_ASSOC( INP, 'READ', LOC(NOBJ+1), STATUS )
        CALL DAT_PRIM( LOC(NOBJ+1), PRIM, STATUS )

*      No more objects?
        IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL( STATUS )
          MORE = .FALSE.

        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99

*      Must be primitive
        ELSE IF ( .NOT. PRIM ) THEN

          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Object is not primitive', STATUS )

*      Must be 1D
        ELSE IF ( STATUS .EQ. SAI__OK ) THEN

          CALL DAT_SHAPE(LOC(NOBJ+1),DAT__MXDIM,DIMS,NDIM,STATUS)
          IF ( NDIM .NE. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Object is not a vector', STATUS )
          ELSE IF ( NOBJ .EQ. 0 ) THEN
            LEN = DIMS(1)
          ELSE IF ( DIMS(1) .NE. LEN ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Vector is of incompatible length',
     :                                                    STATUS )
          END IF

          IF ( STATUS .EQ. SAI__OK ) NOBJ = NOBJ + 1

        END IF

      END DO

*    Get slice if required
      IF ( (NOBJ.GT.0) .AND. (STATUS.EQ.SAI__OK) ) THEN
        CALL PAR_GET0C( 'SLICE', SLICE, STATUS )
        CALL PRS_GETSLICE( 1, LEN, SLICE, RANGES, STATUS )
        CALL HTAB_OUT( LOC, NOBJ, RANGES, OCH, WIDTH, STATUS )
      END IF

*    Close output channel
      CALL AIO_CANCL( OCH, STATUS )

*    Free input objects. Do this inside new error context
      CALL ERR_MARK( STATUS )
      DO IOBJ = 1, NOBJ
        WRITE( INP, '(A3,I1)' ) 'INP', IOBJ
        CALL DAT_CANCL( INP, STATUS )
      END DO
      CALL ERR_RLSE( STATUS )

*    Tidy up
 99   CONTINUE

      END



*+  HTAB_OUT - Write data from specified objects to specified unit
      SUBROUTINE HTAB_OUT(LOC,NOBJ,RANGES,OCH,OUTWIDTH,STATUS)
*    Description :
*    Method :
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC) LOC(*)  ! locators to objects in table
      INTEGER NOBJ                   ! number of objects in table
      INTEGER 			RANGES(2)               ! range of data to be output
      INTEGER			OCH			! Output channel
      INTEGER  			OUTWIDTH               	! Width of output
*
*    Status :
*
      INTEGER STATUS
*    Local Constants :
      CHARACTER*80 FMT1,FMT2,FMT3,FMT4,FMT5
      PARAMETER (FMT1='(1X,A)',
     :           FMT2='(1X,A/1X,A)',
     :           FMT3='(1X,A/)',
     :           FMT4='(A1)',
     :           FMT5='(I6)')
      CHARACTER*1 BLANK,KET,DASH,VLIN
      PARAMETER (BLANK=' ',
     :           KET='>',
     :           DASH='-',
     :           VLIN='|')
      INTEGER    MAXLINE
        PARAMETER (MAXLINE = 4)

*    Local variables :
      CHARACTER*(DAT__SZLOC) ELOC  ! locator to element of vector
      CHARACTER*132 LINE           ! output buffer
      CHARACTER*132 OTXT(MAXLINE)
      CHARACTER*132 FILE           ! container file name
      CHARACTER*(DAT__SZNAM) PATH(10) ! component path components

      INTEGER BEG,END              ! substring pointers
      INTEGER IOBJ                 ! index to object
      INTEGER I                    ! index to vector component
      INTEGER NLINE                ! Lines use to display filename
      INTEGER NLEV                 ! Number of PATH levels
      INTEGER OPLEN                ! Length of filename line
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Table of contents
      CALL AIO_WRITE( OCH, 'Objects in table :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      DO IOBJ = 1, NOBJ
        NLINE = MAXLINE
        WRITE( OTXT(1), '(A,I1,A,I1,A)' ) 'Column ',IOBJ,
     :                                  ' ^FILE'
!        CALL USI_TEXT( 1, OTXT, NLINE, STATUS )
        CALL HDS_TRACE( LOC(IOBJ), NLEV, PATH, FILE, STATUS )
        CALL MSG_SETC( 'FILE', FILE )
        CALL MSG_LOAD( ' ', OTXT(1), OTXT(2), OPLEN, STATUS )
        CALL AIO_WRITE( OCH, OTXT(2)(:OPLEN), STATUS )
      END DO
      CALL AIO_BLNK( OCH, STATUS )

      LINE = BLANK
      CALL CHR_FILL(DASH,LINE(7:OUTWIDTH-1))
      CALL AIO_WRITE( OCH, LINE(:OUTWIDTH), STATUS )
      LINE(6:6)=VLIN
      LINE(OUTWIDTH:OUTWIDTH)=VLIN

      DO I = RANGES(1),RANGES(2)
        LINE=BLANK
        IF (MOD(I,5).EQ.0) THEN
          WRITE(LINE(:6),FMT5) I
          LINE(7:7)=KET
        ELSE
          LINE(6:6)=VLIN
        END IF
        LINE(OUTWIDTH:OUTWIDTH)=VLIN
        BEG=8
        DO IOBJ=1,NOBJ
          END=BEG+19
          CALL DAT_CELL(LOC(IOBJ),1,I,ELOC,STATUS)
          CALL STR_OBVAL(ELOC,LINE(BEG:END),STATUS)
          CALL DAT_ANNUL(ELOC,STATUS)
          BEG=END+1
        END DO
        CALL AIO_WRITE( OCH, LINE(:OUTWIDTH), STATUS )
      END DO
      LINE=BLANK
      CALL CHR_FILL(DASH,LINE(6:OUTWIDTH-1))
      CALL AIO_WRITE( OCH, LINE(:OUTWIDTH), STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HTAB_OUT', STATUS )
      END IF

      END
