*+  AXORDER - Rearranges specified axes in a dataset
      SUBROUTINE AXORDER( STATUS )
*    Description :
*    Environment parameters :
*
*     INP = UNIV(R)
*           Input object - may be primitive
*     SELAX(7) = INT(R)
*           New order of axes
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
*     13 Dec 89 : V1.0-0  Original (DJA)
*     12 Mar 90 : V1.2-0  Was copying output axes incorrectly (DJA)
*      4 Jul 90 : V1.2-1  Swaps 1d data with axis values. Bit of a dodgy
*                         thing to do. (DJA)
*      8 May 91 : V1.4-0  Re-named from AXSWAP (DJA)
*      6 Mar 92 : V1.6-0  OUT prompt moved back to pre-processing (DJA)
*
*    Type definitions :
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
*    Function definitions :
*
      INTEGER                    CHR_LEN
*
*    Local variables :
*
      CHARACTER*80               LABEL, HTXT(5)    !
      CHARACTER*80               UNITS             !
      CHARACTER*10               NSTR              !

      CHARACTER*(DAT__SZLOC)     ILOC              ! Input dataset
      CHARACTER*(DAT__SZLOC)     OLOC              ! Output dataset

      INTEGER                    DIMS(DAT__MXDIM)  ! Input dimensions
      INTEGER                    IDPTR,IVPTR,IQPTR ! Input data pointers
      INTEGER                    ODIMS(DAT__MXDIM) ! Output dimensions
      INTEGER                    QDIMS(DAT__MXDIM) ! Input quality dimensions
      INTEGER                    SELAX(DAT__MXDIM) ! New axis order
      INTEGER                    VDIMS(DAT__MXDIM) ! Input variance dimensions

      INTEGER                    HLEN, NDIGIT      ! String lengths
      INTEGER                    I                 ! Loop over dimensions
      INTEGER                    IAPTR, OAPTR      ! Input & output axis data
      INTEGER                    IWPTR             ! Input axis widths
      INTEGER                    NDIM              ! Dimensionality
      INTEGER                    NLINES            ! Number of history text bits
      INTEGER                    ODPTR,OVPTR,OQPTR ! Output data pointers
      INTEGER                    OWPTR             ! Output axis widths
      INTEGER                    QNDIM             ! Quality dimensionality
      INTEGER                    VNDIM             ! Variance dimensionality
      INTEGER                    NSEL              ! Number of axes specified
      INTEGER                    NWID              ! Number of axis widths

      BYTE                       MASK              ! Input quality mask

      LOGICAL                    INPRIM            ! Input primitive?
      LOGICAL                    IWOK              ! Input widths ok?
      LOGICAL                    OK, VOK, QOK      ! Input objects ok?
      LOGICAL                    SPEC(DAT__MXDIM)  ! Axis specified in output?
      LOGICAL                    SWAP_1D           ! Special 1D swap?
      LOGICAL                    UNIF              ! Uniform axis widths?
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER  (VERSION = 'AXORDER Version 1.6-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CALL AST_INIT()

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get input object
      CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC, INPRIM,
     :                                                  STATUS )
      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'Primitive input object - not allowed' )
        STATUS = SAI__OK
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check and map the data
      SWAP_1D = .FALSE.
      CALL BDA_CHKDATA( ILOC, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT( 'FATAL ERROR : Invalid data' )
        STATUS = SAI__ERROR
      ELSE IF ( NDIM .EQ. 0 ) THEN
        CALL MSG_PRNT( 'Object is scalar - no axes to swap!' )
        STATUS = SAI__ERROR
      ELSE IF ( NDIM .EQ. 1 ) THEN
        SWAP_1D = .TRUE.
        CALL MSG_PRNT( 'Swapping data and axis values' )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL BDA_MAPDATA( ILOC, 'READ', IDPTR, STATUS )

*    Variance
      CALL BDA_CHKVAR( ILOC, VOK, VNDIM, VDIMS, STATUS )
      IF ( VOK ) THEN
        CALL BDA_MAPVAR( ILOC, 'READ', IVPTR, STATUS )
      END IF

*    Quality
      CALL BDA_CHKQUAL( ILOC, QOK, QNDIM, QDIMS, STATUS )
      IF ( QOK .AND. .NOT. SWAP_1D ) THEN
        CALL BDA_MAPQUAL( ILOC, 'READ', IQPTR, STATUS )
      END IF

*    Get new axis order if NDIM > 2
      IF ( NDIM .GT. 2 ) THEN

*      Tell user about different axes
        CALL MSG_PRNT('Axes in input object are :')
        CALL MSG_PRNT(' ')
        CALL AXIS_LIST( ILOC, NDIM, STATUS )

        NSEL = 0
        CALL PAR_GET1I( 'SELAX', NDIM, SELAX, NSEL, STATUS )
        IF ( ( STATUS .NE. SAI__OK ) .OR. ( NSEL .EQ. 0 ) ) GOTO 99

      ELSE IF ( NDIM .EQ. 2 ) THEN
        NSEL = 2
        SELAX(1) = 2
        SELAX(2) = 1
      ELSE
        NSEL = 1
      END IF

*    Check contents of swap array
      IF ( NSEL .NE. NDIM ) THEN
        CALL MSG_SETI( 'ND', NDIM )
        CALL MSG_PRNT( 'You must specify the positions of all'/
     :                                           /' ^ND axes' )
      ELSE IF ( SWAP_1D ) THEN
        HTXT(1) = 'Data array and axis values swapped'
        ODIMS(1) = DIMS(1)

      ELSE

*      Donor dimensions not set yet
        CALL ARR_INIT1L( .FALSE., DAT__MXDIM, SPEC, STATUS )

*      Loop over input and create output dimensions
        DO I =1, NSEL
          IF ( ( SELAX(I) .GT. NDIM ) .OR. ( SELAX(I) .LT. 0 )  ) THEN
            CALL MSG_PRNT( 'FATAL ERROR : No such axis in input'/
     :                                                /' file!' )
            STATUS = SAI__ERROR
          ELSE IF ( SPEC(I) ) THEN
            CALL MSG_SETI( 'AX', I )
            CALL MSG_PRNT( 'FATAL ERROR : Axis ^AX is multiply'/
     :                                 /' specified in output' )
            STATUS = SAI__ERROR
          ELSE
            SPEC(I) = .TRUE.
            ODIMS(I) = DIMS(SELAX(I))
          END IF
          IF ( STATUS .NE. SAI__OK ) GOTO 99
        END DO

*      Make history string
        HTXT(1) = 'New axis order '
        HLEN = CHR_LEN(HTXT) + 1
        DO I = 1, NSEL
          CALL CHR_ITOC( SELAX(I), NSTR, NDIGIT )
          HTXT(1)  = HTXT(1)(:HLEN)//NSTR(:NDIGIT)//' '
          HLEN = HLEN + NDIGIT + 1
        END DO

      END IF

*    Create axis structure
      CALL BDA_CREAXES( OLOC, NDIM, STATUS )

*    Create and map output data
      CALL BDA_CREDATA( OLOC, NDIM, ODIMS, STATUS )
      CALL BDA_MAPDATA( OLOC, 'WRITE', ODPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    One dimensional swap
      IF ( SWAP_1D ) THEN

*      Map input axis
        CALL BDA_MAPAXVAL( ILOC, 'READ', 1, IAPTR, STATUS )

*      Create irregular output axis and map
        CALL BDA_CREAXVAL( OLOC, 1, .FALSE., ODIMS(1), STATUS )
        CALL BDA_MAPAXVAL( OLOC, 'WRITE', 1, OAPTR, STATUS )

*      Copy input data to output axis, and input axis to output data
        CALL ARR_COP1R( ODIMS(1), %VAL(IDPTR), %VAL(OAPTR), STATUS )
        CALL ARR_COP1R( ODIMS(1), %VAL(IAPTR), %VAL(ODPTR), STATUS )

*      Variances become axis widths and vice versa
        IF ( VOK ) THEN
          CALL BDA_CREAXWID( OLOC, 1, .FALSE., ODIMS(1), STATUS )
          CALL BDA_MAPAXWID( OLOC, 'WRITE', 1, OWPTR, STATUS )

*        Copy and square root
          CALL ARR_COP1R( ODIMS(1), %VAL(IVPTR), %VAL(OWPTR), STATUS )
          CALL ARR_SQRT1R( %VAL(OWPTR), ODIMS(1), STATUS )

        END IF
        CALL BDA_CHKAXWID( ILOC, 1, IWOK, UNIF, NWID, STATUS )
        IF ( IWOK ) THEN

*        Create variance array, and map it
          CALL BDA_CREVAR( OLOC, 1, ODIMS, STATUS )
          CALL BDA_MAPVAR( OLOC, 'WRITE', OVPTR, STATUS )

*        Copy widths and square
          CALL ARR_COP1R( ODIMS(1), %VAL(IWPTR), %VAL(OVPTR), STATUS )
          CALL ARR_SQR1R( %VAL(OVPTR), ODIMS(1), STATUS )

        END IF

*      Just copy quality
        IF ( QOK ) THEN
          CALL BDA_COPQUAL( ILOC, OLOC, STATUS )
        END IF

*      Copy text strings
        CALL BDA_GETLABEL( ILOC, LABEL, STATUS )
        CALL BDA_GETUNITS( ILOC, UNITS, STATUS )
        CALL BDA_PUTAXLABEL( OLOC, 1, LABEL, STATUS )
        CALL BDA_PUTAXUNITS( OLOC, 1, UNITS, STATUS )
        CALL BDA_GETAXUNITS( ILOC, 1, UNITS, STATUS )
        CALL BDA_GETAXLABEL( ILOC, 1, LABEL, STATUS )
        CALL BDA_PUTLABEL( OLOC, LABEL, STATUS )
        CALL BDA_PUTUNITS( OLOC, UNITS, STATUS )

*    normal swap
      ELSE

*      Copy axes
        DO I = 1, NDIM
          CALL BDA_COPAXIS( ILOC, OLOC, SELAX(I), I, STATUS )
        END DO

*      Copy top-level text
        CALL BDA_COPTEXT( ILOC, OLOC, STATUS )

*      Create output variance and quality
        IF ( VOK ) THEN
          CALL BDA_CREVAR( OLOC, NDIM, ODIMS, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL BDA_CREQUAL( OLOC, NDIM, ODIMS, STATUS )
          CALL BDA_GETMASK( ILOC, MASK, STATUS )
          CALL BDA_PUTMASK( OLOC, MASK, STATUS )
        END IF

*      Map output variance and quality
        IF ( VOK ) THEN
          CALL BDA_MAPVAR( OLOC, 'WRITE', OVPTR, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL BDA_MAPQUAL( OLOC, 'WRITE', OQPTR, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Pad out dimensions for 7D
        IF ( NDIM .LT. DAT__MXDIM ) THEN
          DO I = NDIM + 1, DAT__MXDIM
            DIMS(I) = 1
            ODIMS(I) = 1
            SELAX(I) = I
          END DO
        END IF

*      Swap the appropriate axes
        CALL AR7_AXSWAP_R( DIMS, %VAL(IDPTR), SELAX, ODIMS,
     :                                %VAL(ODPTR), STATUS )
        IF ( VOK ) THEN
          CALL AR7_AXSWAP_R( DIMS, %VAL(IVPTR), SELAX, ODIMS,
     :                                  %VAL(OVPTR), STATUS )
        END IF
        IF ( QOK ) THEN
          CALL AR7_AXSWAP_B( DIMS, %VAL(IQPTR), SELAX, ODIMS,
     :                                  %VAL(OQPTR), STATUS )
        END IF

      END IF

*    Copy ancillary stuff
      CALL BDA_COPMORE( ILOC, OLOC, STATUS )

*    History
      CALL HIST_COPY( ILOC, OLOC, STATUS )
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      CALL USI_NAMEI( NLINES, HTXT(2), STATUS )
      CALL HIST_PTXT( OLOC, NLINES + 1, HTXT, STATUS )

*    Release datasets
      CALL BDA_RELEASE( ILOC, STATUS )
      CALL BDA_RELEASE( OLOC, STATUS )
      CALL USI_ANNUL( ILOC, STATUS )
      CALL USI_ANNUL( OLOC, STATUS )

*    Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END
