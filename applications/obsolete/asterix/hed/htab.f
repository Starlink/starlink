*+  HTAB - displays one or more vector objects in table form
      SUBROUTINE HTAB( STATUS )
*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*
*      5 Dec 90 : V1.0-0 Allows full 80 character device name (DJA)
*     15 Apr 91 : V1.4-0 6 digit array index rather than 5 (DJA)
*     22 May 92 : V1.6-0 ERR_ANNULs inserted. Uses PRS_GETSLICE for slice
*                        parsing. Use USI for object association (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) LOC(7)      ! locators to objects in table
      CHARACTER*4 INP		         ! parameter name
      CHARACTER*20 SLICE

      INTEGER DEFWIDTH                   ! width of output
      INTEGER DIMS(DAT__MXDIM)           ! dimensions of object
      INTEGER IOBJ                       ! index to object
      INTEGER LEN
      INTEGER			OCH			! Output channel id
      INTEGER NOBJ                       ! number of objects in table
      INTEGER NMAX			 ! maximum objects in table
      INTEGER NDIM                       ! dimensionality of object
      INTEGER RANGES(2,DAT__MXDIM)       ! ranges of data to be output
      INTEGER WIDTH

      LOGICAL PRIM                       ! whether object primitive
      LOGICAL MORE                       ! whether more objects to be included
*
*    Version id :
*
      CHARACTER*30       VERSION
        PARAMETER        ( VERSION = 'HTAB Version 1.8-0' )
*-

*    Version id
      CALL MSG_PRNT(VERSION)

*    Start ASTERIX
      CALL AST_INIT()

*    Set up output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, DEFWIDTH, STATUS )
      CALL USI_GET0I('WIDTH',WIDTH,STATUS)
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
        CALL USI_ASSOCI( INP, 'READ', LOC(NOBJ+1), PRIM, STATUS )

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
        CALL USI_GET0C( 'SLICE', SLICE, STATUS )
        CALL PRS_GETSLICE( 1, LEN, SLICE, RANGES, STATUS )
        CALL HTAB_OUT( LOC, NOBJ, RANGES, OCH, WIDTH, STATUS )
      END IF

*    Close output channel
      CALL AIO_CANCL( OCH, STATUS )

*    Free input objects. Do this inside new error context
      CALL ERR_MARK( STATUS )
      DO IOBJ = 1, NOBJ
        CALL USI_ANNUL( LOC(IOBJ), STATUS )
      END DO
      CALL ERR_RLSE( STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

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

      INTEGER BEG,END              ! substring pointers
      INTEGER IOBJ                 ! index to object
      INTEGER I                    ! index to vector component
      INTEGER NLINE                ! Lines use to display filename
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Table of contents
      CALL AIO_WRITE( OCH, 'Objects in table :', STATUS )
      CALL AIO_BLNK( OCH, STATUS )
      DO IOBJ = 1, NOBJ
        NLINE = MAXLINE
        WRITE( OTXT(1), '(A,I1,A,I1,A)' ) 'Column ',IOBJ,
     :                                  ' {INP', IOBJ, '}'
        CALL USI_TEXT( 1, OTXT, NLINE, STATUS )
        DO I = 1, NLINE
          CALL AIO_WRITE( OCH, OTXT(I)(:OUTWIDTH), STATUS )
        END DO
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
