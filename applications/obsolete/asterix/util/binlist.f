*+  BINLIST - Lists data stored in a 1D binned dataset
      SUBROUTINE BINLIST( STATUS )
*    Description :
*    Parameters :
*
*     INP = UNIV(R)
*       NDF to be listed
*     DEV = CHAR(R)
*       Ascii output device name
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*
*      1 Jun 89 : V1.0-1 Now indicates where quantities are defaulted (RJV)
*     29 Oct 91 : V1.5-0 Uses PRS_GETSLICE for slice parse. Doesn't display
*                        data which doesn't exist. Handles asymmetric data
*                        and axis errors. (DJA)
*      3 Mar 94 : V1.7-0 Use sensible format for real numbers (DJA)
*      4 May 94 : V1.7-1 Use AIO to do i/o (DJA)
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
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) 	LOC	 		! Locator to top level

      CHARACTER*75           	NAME
      CHARACTER*20           	SLICE       		! Data slice specification

      INTEGER                	AEPTR(2)    		! Asymmetric data errors
      INTEGER                	AWPTR(2)    		! Asymmetric axis errors

      INTEGER L
      INTEGER			OCH			! Output channel
      INTEGER 			DEFWIDTH                ! Width of output
      INTEGER 			NDIM            	! Dimensionality of object
      INTEGER DIMS(DAT__MXDIM)           ! Dimensions of object
      INTEGER NDUM,DUMS(DAT__MXDIM)
      INTEGER RANGES(2,DAT__MXDIM)       ! Ranges of data to be output
      INTEGER ID
      INTEGER DPTR,VPTR,QPTR,APTR,WPTR

      LOGICAL                PRIM        ! Whether object primitive
      LOGICAL                DOK
      LOGICAL VOK,AOK,WOK,QOK,AWOK,AEOK
      LOGICAL DUM
*
*    Version id :
*
      CHARACTER*30 VERSION
        PARAMETER (VERSION='BINLIST Version 1.8-0')
*-

*    Version id
      CALL MSG_PRNT(VERSION)

*    Initialise
      CALL AST_INIT()
      DOK=.FALSE.

*    Input dataset
      CALL USI_ASSOCI( 'INP', 'READ', LOC, PRIM, STATUS )
      CALL BDA_FIND( LOC, ID, STATUS )

*    Set up output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, DEFWIDTH, STATUS )

      CALL BDA_CHKDATA_INT(ID,DOK,NDIM,DIMS,STATUS)

      IF ( DOK.AND. (NDIM.EQ.1) ) THEN

*      Write dataset name
        CALL STR_OBNAME(LOC,NAME,L,STATUS)
        CALL AIO_BLNK( OCH, STATUS )
        CALL AIO_WRITE( OCH, 'Dataset:-', STATUS )
        CALL AIO_BLNK( OCH, STATUS )
        CALL AIO_IWRITE( OCH, 2, NAME(:L), STATUS )
        CALL AIO_BLNK( OCH, STATUS )

*      Axis data?
        CALL BDA_CHKAXVAL_INT(ID,1,AOK,DUM,NDUM,STATUS)
        CALL BDA_MAPAXVAL_INT(ID,'R',1,APTR,STATUS)

*      Asymmetric axis errors?
        CALL BDA_CHKXERR_INT(ID,AWOK,DUM,STATUS)
        IF ( AWOK ) THEN
          WOK = .FALSE.
          CALL BDA_MAPXERR_INT(ID,'R',AWPTR(1),AWPTR(2),STATUS)
        ELSE
          CALL BDA_CHKAXWID_INT(ID,1,WOK,DUM,NDUM,STATUS)
          CALL BDA_MAPAXWID_INT(ID,'R',1,WPTR,STATUS)
        END IF

*      The data
        CALL BDA_MAPDATA_INT(ID,'R',DPTR,STATUS)

*      Asymmetric data errors?
        CALL BDA_CHKYERR_INT(ID,AEOK,DUM,STATUS)
        IF ( AEOK ) THEN
          VOK = .FALSE.
          CALL BDA_MAPYERR_INT(ID,'R',AEPTR(1),AEPTR(2),STATUS)

*      Otherwise look for variance
        ELSE
          CALL BDA_CHKVAR_INT(ID,VOK,NDUM,DUMS,STATUS)
          CALL BDA_MAPVAR_INT(ID,'R',VPTR,STATUS)

        END IF

*      Quality
        CALL BDA_CHKQUAL_INT(ID,QOK,NDUM,DUMS,STATUS)
        CALL BDA_MAPQUAL_INT(ID,'R',QPTR,STATUS)

*      Get slice of dataset
        CALL USI_GET0C( 'SLICE', SLICE, STATUS )
        CALL PRS_GETSLICE( NDIM, DIMS(1), SLICE, RANGES, STATUS )

*      Output the data
        CALL BINLIST_OUT( %VAL(DPTR), %VAL(VPTR), VOK, %VAL(AEPTR(1)),
     :                    %VAL(AEPTR(2)), AEOK, %VAL(QPTR), QOK,
     :                    %VAL(AWPTR(1)), %VAL(AWPTR(2)), AWOK,
     :                    %VAL(APTR), AOK, %VAL(WPTR), WOK, RANGES,
     :                                      OCH, DEFWIDTH, STATUS )


      ELSE IF (.NOT.DOK) THEN
        STATUS=SAI__ERROR
        CALL ERR_REP(' ','AST_ERR: no data present',STATUS)

      ELSE IF (DOK.AND.NDIM.NE.1) THEN
        STATUS=SAI__ERROR
        CALL ERR_REP(' ','AST_ERR: dataset not 1D',STATUS)

      END IF

*    Close output channel
      CALL AIO_CANCL( 'DEV', STATUS )

*    Close down ASTERIX
      CALL USI_ANNUL(LOC,STATUS)
      CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END




*+  BINLIST_OUT - Write data values to text file
      SUBROUTINE BINLIST_OUT(D,V,VOK,AEL,AEU,AEOK,Q,QOK,AWL,AWU,AWOK,A,
     :           AOK,W,WOK,RANGES,OCH, OUTWIDTH, STATUS )
*
*    Description :
*    Method :
*
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      REAL D(*),V(*), AEL(*), AEU(*)
      BYTE Q(*)
      REAL A(*),W(*),AWL(*),AWU(*)
      LOGICAL VOK,QOK,AOK,WOK,AEOK,AWOK
      INTEGER RANGES(2)              ! range of data to be output
      INTEGER 			OCH                    	! Output channel id
      INTEGER			OUTWIDTH		! Channel width
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*10 FMT2,FMT3
      PARAMETER (FMT2='(I6)',
     &           FMT3='(1PG14.6)')
      CHARACTER*1 BLANK,KET,DASH,VLIN
      PARAMETER (BLANK=' ',
     &           KET='>',
     &           DASH='-',
     &           VLIN='|')
*
*    Local variables :
*
      CHARACTER*80     LINE                          ! Output buffer

      INTEGER          COL                           ! Column counter
      INTEGER          ACOL,WCOL,DCOL,VCOL,QCOL      ! Columns for output
      INTEGER          I                             ! Index to vector component
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      LINE=BLANK
      CALL CHR_FILL(DASH,LINE(8:OUTWIDTH-1))
      CALL AIO_WRITE( OCH, LINE(:OUTWIDTH), STATUS )
      LINE=BLANK
      LINE(7:7)=VLIN
      LINE(OUTWIDTH:OUTWIDTH)=VLIN

*    Write out headers
      COL = 9
      IF ( AOK ) THEN
        ACOL = COL
        COL = COL + 15
        LINE(ACOL+5:) = 'AXIS'
      END IF
      IF ( AWOK ) THEN
        WCOL = COL
        COL = COL + 30
        LINE(WCOL+4:) = 'LOWIDTH'
        LINE(WCOL+19:) = 'UPWIDTH'
      ELSE IF ( WOK ) THEN
        WCOL = COL
        COL = COL + 15
        LINE(WCOL+5:) = 'WIDTH'
      END IF
      DCOL = COL
      COL = COL + 15
      LINE(DCOL+5:)='DATA'
      IF ( AEOK ) THEN
        VCOL = COL
        COL = COL + 30
        LINE(VCOL+4:) = 'LOERROR'
        LINE(VCOL+19:) = 'UPERROR'
      ELSE IF ( VOK ) THEN
        VCOL = COL
        COL = COL + 15
        LINE(VCOL+2:) = 'VARIANCE'
      END IF
      IF ( QOK ) THEN
        QCOL=COL
        LINE(QCOL+1:) = 'QUALITY'
      END IF
      CALL AIO_WRITE( OCH, LINE(:OUTWIDTH), STATUS )

*    Loop over data
      DO I = RANGES(1), RANGES(2)

*      Blank the buffer
        LINE=BLANK

*      The line number
        IF (MOD(I,5).EQ.0) THEN
          WRITE(LINE(:6),FMT2) I
          LINE(7:7)=KET
        ELSE
          LINE(7:7)=VLIN
        END IF

*      Write data to buffer
        IF ( AOK) WRITE(LINE(ACOL:),FMT3)  A(I)
        IF ( AWOK ) THEN
          WRITE(LINE(WCOL:),FMT3) AWL(I)
          WRITE(LINE(WCOL+15:),FMT3) AWU(I)
        ELSE IF ( WOK) THEN
          WRITE(LINE(WCOL:),FMT3) W(I)
        END IF

        WRITE(LINE(DCOL:),FMT3) D(I)
        IF ( AEOK ) THEN
          WRITE(LINE(VCOL:),FMT3) AEL(I)
          WRITE(LINE(VCOL+15:),FMT3) AEU(I)
        ELSE IF ( VOK) THEN
          WRITE(LINE(VCOL:),FMT3) V(I)
        END IF
        IF ( QOK) CALL STR_BTOC(Q(I),LINE(QCOL:),STATUS)

*      End vertical bar
        LINE(OUTWIDTH:OUTWIDTH) = VLIN

*      Write buffer
        CALL AIO_WRITE( OCH, LINE(:OUTWIDTH), STATUS )

      END DO

      LINE=BLANK
      CALL CHR_FILL(DASH,LINE(8:OUTWIDTH-1))
      CALL AIO_WRITE( OCH, LINE(:OUTWIDTH), STATUS )

      IF ( STATUS.NE.SAI__OK ) THEN
        CALL AST_REXIT( 'BINLIST_OUT', STATUS )
      END IF

      END
