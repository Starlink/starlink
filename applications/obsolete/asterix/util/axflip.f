*+  AXFLIP - Reverses specified axes of dataset
      SUBROUTINE AXFLIP( STATUS )
*    Description :
*     <description of what the application does - for user info>
*    Environment parameters :
*
*     INP = UNIV(R)
*           Input object - may be primitive
*     SELAX(7) = INT(R)
*           Axes to flip
*     OUT = UNIV(W)
*           Output dataset
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Dec 89 : V1.0-0  Original (DJA)
*      4 Jan 90 : V1.0-1  Bug where existing irregular widths were mapped
*                         incorrectly fixed. (DJA)
*     18 Jun 90 : V1.2-0  Bug with irregular axes fixed (DJA)
*     10 Apr 91 : V1.4-0  Copes with primitives and datasets with no axes (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*     28 Mar 95 : V1.8-1  New data interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
      INTEGER                 	CHR_LEN
*
*    Local variables :
*
      CHARACTER*80            	HTXT(5)    		! History text
      CHARACTER*10              NSTR              	!

      CHARACTER*(ADI__SZTYP)    OBTYPE            	! Object type

      INTEGER                   DIMS(ADI__MXDIM)  	! Input dimensions
      INTEGER                   IDPTR,IVPTR,IQPTR 	! Input data pointers
      INTEGER                   QDIMS(ADI__MXDIM) 	! Input quality dimensions
      INTEGER                   SELAX(ADI__MXDIM) 	! Axes to flip
      INTEGER                   VDIMS(ADI__MXDIM) 	! Input variance dimensions

      INTEGER                   HLEN              	! Length of history text
      INTEGER                   I                 	! Loop over dimensions
      INTEGER			IFID			! Input dataset id
      INTEGER                   NAX               	! # of AXIS objects
      INTEGER                   NDIGIT            	! Length of number in char
      INTEGER                   NDIM              	! Dimensionality
      INTEGER                   NLINES            	! History lines used
      INTEGER                   ODPTR,OVPTR,OQPTR 	! Output data pointers
      INTEGER			OFID			! Output dataset id
      INTEGER                   QNDIM             	! Quality dimensionality
      INTEGER                   VNDIM             	! Variance dimensionality
      INTEGER                   NSEL              	! Number of axes to flip

      LOGICAL                   FLIP(ADI__MXDIM)  	! Flip these axes?
      LOGICAL                   FLIP_OK           	! Carry on with flip?
      LOGICAL                   INPRIM            	! Input primitive?
      LOGICAL                   OK, VOK, QOK      	! Input objects ok?
*
*    Version :
*
      CHARACTER*30 		VERSION
        PARAMETER 		( VERSION = 'AXFLIP Version 1.8-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CALL AST_INIT

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get input object
      CALL USI_TASSOCI( 'INP', '*', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check and map the data
      CALL BDI_CHKDATA( IFID, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'FATAL ERROR : Invalid data', STATUS )
      ELSE IF ( NDIM .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Object is scalar - no axes to flip!',
     :                 STATUS )
      ELSE
         CALL BDI_MAPDATA( IFID, 'READ', IDPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Variance
      CALL BDI_CHKVAR( IFID, VOK, VNDIM, VDIMS, STATUS )
      IF ( VOK ) THEN
        CALL BDI_MAPVAR( IFID, 'READ', IVPTR, STATUS )
      END IF

*    Quality
      CALL BDI_CHKQUAL( IFID, QOK, QNDIM, QDIMS, STATUS )
      IF ( QOK ) THEN
        CALL BDI_MAPQUAL( IFID, 'READ', IQPTR, STATUS )
      END IF

*    Get number of input axes (ie axis structures)
      CALL BDI_PRIM( IFID, INPRIM, STATUS )
      IF ( INPRIM ) THEN
        NAX = 0
      ELSE
        CALL BDI_CHKAXES( IFID, NAX, STATUS )
      END IF

*    Tell user about different axes
      IF ( INPRIM ) THEN
        CALL MSG_PRNT( ' ' )
        CALL MSG_SETI( 'N', NDIM )
        CALL MSG_PRNT( 'There are ^N dimensions in this primitive'/
     :                                                 /' object' )
        CALL MSG_PRNT( ' ' )
      ELSE
        CALL MSG_PRNT( 'Axes present in dataset are:' )
        CALL MSG_PRNT( ' ' )
        CALL AXIS_TLIST( IFID, NDIM, STATUS )
      END IF

*    Select axes to flip if NDIM > 1
      IF ( NDIM .GT. 1 ) THEN
        NSEL = 0
        CALL USI_GET1I( 'SELAX', NDIM, SELAX, NSEL, STATUS )
        IF ( ( STATUS .NE. SAI__OK ) .OR. ( NSEL .EQ. 0 ) ) GOTO 99
      ELSE
        NSEL = 1
        SELAX(1) = 1
      END IF

*    Set up the FLIP array
      CALL ARR_INIT1L( .FALSE., ADI__MXDIM, FLIP, STATUS )
      FLIP_OK = .FALSE.
      IF ( ( NDIM .EQ. 1 ) .OR. ( NSEL .EQ. 1 ) ) THEN
        HTXT(1) = 'Swapped axis : '
      ELSE
        HTXT(1) = 'Swapped axes : '
      END IF
      HLEN = CHR_LEN(HTXT(1))+1
      DO I = 1, NSEL
        IF ( ( SELAX(I) .GT. 0 ) .AND. ( SELAX(I) .LE. NDIM ) ) THEN
          FLIP(SELAX(I)) = .TRUE.
          FLIP_OK = .TRUE.
          CALL CHR_ITOC( SELAX(I), NSTR, NDIGIT )
          HTXT(1) = HTXT(1)(:HLEN)//NSTR(:NDIGIT)//' '
          HLEN = HLEN + NDIGIT + 1
        ELSE
          CALL MSG_SETI( 'N', SELAX(I) )
          CALL MSG_PRNT( 'No such axis number ^N' )
        END IF
      END DO

*    Output dataset
      CALL USI_TASSOCO( 'OUT', 'BINDS', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy axes
      IF ( NAX .GT. 0 ) THEN

*      Create axis structure
        CALL BDI_CREAXES( OFID, NDIM, STATUS )

        DO I = 1, NDIM
          IF ( FLIP(I) ) THEN

*          Copy with flip
            CALL AXFLIP_COPAXIS( IFID, OFID, I, STATUS )

          ELSE

*          Simple axis copy
            CALL BDI_COPAXIS( IFID, OFID, I, I, STATUS )

          END IF
        END DO
      END IF

*    Copy output data, variance and quality
      CALL BDI_COPDATA( IFID, OFID, STATUS )
      IF ( VOK ) THEN
        CALL BDI_COPVAR( IFID, OFID, STATUS )
      END IF
      IF ( QOK ) THEN
        CALL BDI_COPQUAL( IFID, OFID, STATUS )
      END IF

*    Map output data
      CALL BDI_MAPDATA( OFID, 'WRITE', ODPTR, STATUS )
      IF ( VOK ) THEN
        CALL BDI_MAPVAR( OFID, 'WRITE', OVPTR, STATUS )
      END IF
      IF ( QOK ) THEN
        CALL BDI_MAPQUAL( OFID, 'WRITE', OQPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Pad out dimensions for 7D
      IF ( NDIM .LT. ADI__MXDIM ) THEN
        DO I = NDIM + 1, ADI__MXDIM
          DIMS(I) = 1
        END DO
      END IF

*    Flip the appropriate axes
      CALL AR7_AXFLIP_R( DIMS, %VAL(IDPTR), FLIP, %VAL(ODPTR), STATUS )
      IF ( VOK ) THEN
        CALL AR7_AXFLIP_R( DIMS, %VAL(IVPTR), FLIP, %VAL(OVPTR),
     :                                                  STATUS )
      END IF
      IF ( QOK ) THEN
        CALL AR7_AXFLIP_B( DIMS, %VAL(IQPTR), FLIP, %VAL(OQPTR),
     :                                                  STATUS )
      END IF

*    Copy ancillary stuff
      IF ( .NOT. INPRIM ) THEN
        CALL BDI_COPMORE( IFID, OFID, STATUS )
        CALL BDI_COPTEXT( IFID, OFID, STATUS )
      END IF

*    History
      IF ( INPRIM ) THEN
        CALL HSI_NEW( OFID, STATUS )
      ELSE
        CALL HSI_COPY( IFID, OFID, STATUS )
      END IF
      CALL HSI_ADD( OFID, VERSION, STATUS )

*    Get input file spec
      CALL USI_NAMEI( NLINES, HTXT(2), STATUS )
      CALL HSI_PTXT( OFID, NLINES+1, HTXT, STATUS )

*    Release files
      CALL BDI_RELEASE( IFID, STATUS )
      CALL BDI_RELEASE( OFID, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  AXFLIP_COPAXIS - Copy axis and flip direction
      SUBROUTINE AXFLIP_COPAXIS( IFID, OFID, AXN, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Dec 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER			IFID			! Input dataset
      INTEGER                 	AXN               	! Axis to flip
      INTEGER			OFID			! Output dataset
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                   	BASE, SCALE       	! Spaced parameters

      INTEGER                 	IAPTR             	! Input axis data pointer
      INTEGER                 	OAPTR             	! Output axis data pointer
      INTEGER                 	SIZE              	! Size of axis

      LOGICAL                 	OK                	! Validity check
      LOGICAL                 	REG               	! Regular values?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Validate input axis
      OK = .FALSE.
      CALL BDI_CHKAXVAL( IFID, AXN, OK, REG, SIZE, STATUS )
      IF ( OK ) THEN
         CALL BDI_CREAXVAL( OFID, AXN, REG, SIZE, STATUS )
         IF ( REG ) THEN
            CALL BDI_GETAXVAL( IFID, AXN, BASE, SCALE, SIZE, STATUS )
            CALL BDI_PUTAXVAL( OFID, AXN, BASE+(SIZE-1)*SCALE, -SCALE,
     :                                                  SIZE, STATUS )
         ELSE
            CALL BDI_MAPAXVAL( IFID, 'READ', AXN, IAPTR, STATUS )
            CALL BDI_MAPAXVAL( OFID, 'WRITE', AXN, OAPTR, STATUS )
            CALL ARR_FLIP1R( SIZE, %VAL(IAPTR), %VAL(OAPTR), STATUS )
            CALL BDI_UNMAPAXVAL( IFID, AXN, STATUS )
            CALL BDI_UNMAPAXVAL( OFID, AXN, STATUS )
         END IF
      END IF

*    now the widths
      OK = .FALSE.
      CALL BDI_CHKAXWID( IFID, AXN, OK, REG, SIZE, STATUS )
      IF ( OK ) THEN
         IF ( REG ) THEN
            CALL BDI_GETAXWID( IFID, AXN, BASE, SCALE, SIZE, STATUS )
            CALL BDI_PUTAXWID( OFID, AXN, BASE+(SIZE-1)*SCALE, -SCALE,
     :                                                  SIZE, STATUS )
         ELSE
            CALL BDI_MAPAXWID( IFID, 'READ', AXN, IAPTR, STATUS )
            CALL BDI_CREAXWID( OFID, AXN, .FALSE., SIZE, STATUS )
            CALL BDI_MAPAXWID( OFID, 'WRITE', AXN, OAPTR, STATUS )
            CALL ARR_FLIP1R( SIZE, %VAL(IAPTR), %VAL(OAPTR), STATUS )
            CALL BDI_UNMAPAXWID( IFID, AXN, STATUS )
            CALL BDI_UNMAPAXWID( OFID, AXN, STATUS )
         END IF
      END IF

*    and copy the text
      CALL BDI_COPAXTEXT( IFID, OFID, AXN, AXN, STATUS )

      END
