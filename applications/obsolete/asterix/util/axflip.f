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
*
*    Type definitions :
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
*    Function definitions :
*
      INTEGER                 	CHR_LEN
*
*    Local variables :
*
      CHARACTER*80            	HTXT(5)    		! History text
      CHARACTER*10              NSTR              	!

      CHARACTER*(DAT__SZLOC)    ILOC              	! Input dataset
      CHARACTER*(DAT__SZLOC)    OLOC              	! Output dataset
      CHARACTER*(DAT__SZTYP)    OBTYPE            	! Object type

      INTEGER                   DIMS(DAT__MXDIM)  	! Input dimensions
      INTEGER                   IDPTR,IVPTR,IQPTR 	! Input data pointers
      INTEGER                   QDIMS(DAT__MXDIM) 	! Input quality dimensions
      INTEGER                   SELAX(DAT__MXDIM) 	! Axes to flip
      INTEGER                   VDIMS(DAT__MXDIM) 	! Input variance dimensions

      INTEGER                   HLEN              	! Length of history text
      INTEGER                   I                 	! Loop over dimensions
      INTEGER                   NAX               	! # of AXIS objects
      INTEGER                   NDIGIT            	! Length of number in char
      INTEGER                   NDIM              	! Dimensionality
      INTEGER                   NLINES            	! History lines used
      INTEGER                   ODPTR,OVPTR,OQPTR 	! Output data pointers
      INTEGER                   QNDIM             	! Quality dimensionality
      INTEGER                   VNDIM             	! Variance dimensionality
      INTEGER                   NSEL              	! Number of axes to flip

      LOGICAL                   FLIP(DAT__MXDIM)  	! Flip these axes?
      LOGICAL                   FLIP_OK           	! Carry on with flip?
      LOGICAL                   INPRIM            	! Input primitive?
      LOGICAL                   OK, VOK, QOK      	! Input objects ok?
*
*    Version :
*
      CHARACTER*30 		VERSION
        PARAMETER 		( VERSION = 'AXFLIP Version 1.8-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CALL AST_INIT

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get input object
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, INPRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check and map the data
      CALL BDA_CHKDATA( ILOC, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
         CALL MSG_PRNT( 'FATAL ERROR : Invalid data' )
         STATUS = SAI__ERROR
      ELSE IF ( NDIM .EQ. 0 ) THEN
         CALL MSG_PRNT( 'Object is scalar - no axes to flip!' )
         STATUS = SAI__ERROR
      ELSE
         CALL BDA_MAPDATA( ILOC, 'READ', IDPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Variance
      CALL BDA_CHKVAR( ILOC, VOK, VNDIM, VDIMS, STATUS )
      IF ( VOK ) THEN
        CALL BDA_MAPVAR( ILOC, 'READ', IVPTR, STATUS )
      END IF

*    Quality
      CALL BDA_CHKQUAL( ILOC, QOK, QNDIM, QDIMS, STATUS )
      IF ( QOK ) THEN
        CALL BDA_MAPQUAL( ILOC, 'READ', IQPTR, STATUS )
      END IF

*    Get number of input axes (ie axis structures)
      IF ( INPRIM ) THEN
        NAX = 0
      ELSE
        CALL BDA_CHKAXES( ILOC, NAX, STATUS )
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
        CALL AXIS_LIST( ILOC, NDIM, STATUS )
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
      CALL ARR_INIT1L( .FALSE., DAT__MXDIM, FLIP, STATUS )
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
      IF ( INPRIM ) THEN
        OBTYPE = 'BINDS'
      ELSE
        CALL DAT_TYPE( ILOC, OBTYPE, STATUS )
      END IF
      CALL USI_ASSOCO( 'OUT', OBTYPE, OLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy axes
      IF ( NAX .GT. 0 ) THEN

*      Create axis structure
        CALL BDA_CREAXES( OLOC, NDIM, STATUS )

        DO I = 1, NDIM
          IF ( FLIP(I) ) THEN

*          Copy with flip
            CALL AXFLIP_COPAXIS( ILOC, OLOC, I, STATUS )

          ELSE

*          Simple axis copy
            CALL BDA_COPAXIS( ILOC, OLOC, I, I, STATUS )

          END IF
        END DO
      END IF

*    Copy output data, variance and quality
      CALL BDA_COPDATA( ILOC, OLOC, STATUS )
      IF ( VOK ) THEN
        CALL BDA_COPVAR( ILOC, OLOC, STATUS )
      END IF
      IF ( QOK ) THEN
        CALL BDA_COPQUAL( ILOC, OLOC, STATUS )
      END IF

*    Map output data
      CALL BDA_MAPDATA( OLOC, 'WRITE', ODPTR, STATUS )
      IF ( VOK ) THEN
        CALL BDA_MAPVAR( OLOC, 'WRITE', OVPTR, STATUS )
      END IF
      IF ( QOK ) THEN
        CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Pad out dimensions for 7D
      IF ( NDIM .LT. DAT__MXDIM ) THEN
        DO I = NDIM + 1, DAT__MXDIM
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
        CALL BDA_COPMORE( ILOC, OLOC, STATUS )
        CALL BDA_COPTEXT( ILOC, OLOC, STATUS )
      END IF

*    History
      IF ( INPRIM ) THEN
        CALL HIST_NEW( OLOC, STATUS )
      ELSE
        CALL HIST_COPY( ILOC, OLOC, STATUS )
      END IF
      CALL HIST_ADD( OLOC, VERSION, STATUS )

*    Get input file spec
      CALL USI_NAMEI( NLINES, HTXT(2), STATUS )
      CALL HIST_PTXT( OLOC, NLINES+1, HTXT, STATUS )

*    Release files
      CALL BDA_RELEASE( ILOC, STATUS )
      CALL BDA_RELEASE( OLOC, STATUS )
      CALL USI_ANNUL( ILOC, STATUS )
      CALL USI_ANNUL( OLOC, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  AXFLIP_COPAXIS - Copy axis and flip direction
      SUBROUTINE AXFLIP_COPAXIS( ILOC, OLOC, AXN, STATUS )
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
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)  	ILOC              	! Input dataset
      INTEGER                 	AXN               	! Axis to flip
*
*    Import-Export :
*
      CHARACTER*(DAT__SZLOC)  	OLOC              	! Output dataset
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
      CALL BDA_CHKAXVAL( ILOC, AXN, OK, REG, SIZE, STATUS )
      IF ( OK ) THEN
         CALL BDA_CREAXVAL( OLOC, AXN, REG, SIZE, STATUS )
         IF ( REG ) THEN
            CALL BDA_GETAXVAL( ILOC, AXN, BASE, SCALE, SIZE, STATUS )
            CALL BDA_PUTAXVAL( OLOC, AXN, BASE+(SIZE-1)*SCALE, -SCALE,
     :                                                  SIZE, STATUS )
         ELSE
            CALL BDA_MAPAXVAL( ILOC, 'READ', AXN, IAPTR, STATUS )
            CALL BDA_MAPAXVAL( OLOC, 'WRITE', AXN, OAPTR, STATUS )
            CALL ARR_FLIP1R( SIZE, %VAL(IAPTR), %VAL(OAPTR), STATUS )
            CALL BDA_UNMAPAXVAL( ILOC, AXN, STATUS )
            CALL BDA_UNMAPAXVAL( OLOC, AXN, STATUS )
         END IF
      END IF

*    now the widths
      OK = .FALSE.
      CALL BDA_CHKAXWID( ILOC, AXN, OK, REG, SIZE, STATUS )
      IF ( OK ) THEN
         IF ( REG ) THEN
            CALL BDA_GETAXWID( ILOC, AXN, BASE, SCALE, SIZE, STATUS )
            CALL BDA_PUTAXWID( OLOC, AXN, BASE+(SIZE-1)*SCALE, -SCALE,
     :                                                  SIZE, STATUS )
         ELSE
            CALL BDA_MAPAXWID( ILOC, 'READ', AXN, IAPTR, STATUS )
            CALL BDA_CREAXWID( OLOC, AXN, .FALSE., SIZE, STATUS )
            CALL BDA_MAPAXWID( OLOC, 'WRITE', AXN, OAPTR, STATUS )
            CALL ARR_FLIP1R( SIZE, %VAL(IAPTR), %VAL(OAPTR), STATUS )
            CALL BDA_UNMAPAXWID( ILOC, AXN, STATUS )
            CALL BDA_UNMAPAXWID( OLOC, AXN, STATUS )
         END IF
      END IF

*    and copy the text
      CALL BDA_COPAXTEXT( ILOC, OLOC, AXN, AXN, STATUS )

      END
