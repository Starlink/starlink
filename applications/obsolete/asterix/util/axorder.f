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
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*     26 Mar 95 : V1.8-1  Use new data interface (DJA)(
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
      INTEGER                    CHR_LEN
*
*    Local variables :
*
      CHARACTER*80               LABEL, HTXT(5)    !
      CHARACTER*80               UNITS             !
      CHARACTER*10               NSTR              !

      INTEGER                    DIMS(ADI__MXDIM)  ! Input dimensions
      INTEGER                    IDPTR,IVPTR,IQPTR ! Input data pointers
      INTEGER                    ODIMS(ADI__MXDIM) ! Output dimensions
      INTEGER                    QDIMS(ADI__MXDIM) ! Input quality dimensions
      INTEGER                    SELAX(ADI__MXDIM) ! New axis order
      INTEGER                    VDIMS(ADI__MXDIM) ! Input variance dimensions

      INTEGER                    HLEN, NDIGIT      ! String lengths
      INTEGER                    I                 ! Loop over dimensions
      INTEGER                    IAPTR, OAPTR      ! Input & output axis data
      INTEGER			IFID			! Input dataset id
      INTEGER                    IWPTR             ! Input axis widths
      INTEGER                    NDIM              ! Dimensionality
      INTEGER                    NLINES            ! Number of history text bits
      INTEGER                    ODPTR,OVPTR,OQPTR ! Output data pointers
      INTEGER			OFID			! Output dataset id
      INTEGER                    OWPTR             ! Output axis widths
      INTEGER                    QNDIM             ! Quality dimensionality
      INTEGER                    VNDIM             ! Variance dimensionality
      INTEGER                    NSEL              ! Number of axes specified
      INTEGER                    NWID              ! Number of axis widths

      BYTE                       MASK              ! Input quality mask

      LOGICAL                    INPRIM            ! Input primitive?
      LOGICAL                    IWOK              ! Input widths ok?
      LOGICAL                    OK, VOK, QOK      ! Input objects ok?
      LOGICAL                    SPEC(ADI__MXDIM)  ! Axis specified in output?
      LOGICAL                    SWAP_1D           ! Special 1D swap?
      LOGICAL                    UNIF              ! Uniform axis widths?
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER  (VERSION = 'AXORDER Version 1.8-1')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CALL AST_INIT()

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get input object
      CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )
      CALL BDI_PRIM( IFID, INPRIM, STATUS )
      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'Primitive input object - not allowed' )
        STATUS = SAI__OK
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check and map the data
      SWAP_1D = .FALSE.
      CALL BDI_CHKDATA( IFID, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'FATAL ERROR : Invalid data', STATUS )
      ELSE IF ( NDIM .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object is scalar - no axes to swap!',
     :                STATUS )
      ELSE IF ( NDIM .EQ. 1 ) THEN
        SWAP_1D = .TRUE.
        CALL MSG_PRNT( 'Swapping data and axis values' )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL BDI_MAPDATA( IFID, 'READ', IDPTR, STATUS )

*    Variance
      CALL BDI_CHKVAR( IFID, VOK, VNDIM, VDIMS, STATUS )
      IF ( VOK ) THEN
        CALL BDI_MAPVAR( IFID, 'READ', IVPTR, STATUS )
      END IF

*    Quality
      CALL BDI_CHKQUAL( IFID, QOK, QNDIM, QDIMS, STATUS )
      IF ( QOK .AND. .NOT. SWAP_1D ) THEN
        CALL BDI_MAPQUAL( IFID, 'READ', IQPTR, STATUS )
      END IF

*    Get new axis order if NDIM > 2
      IF ( NDIM .GT. 2 ) THEN

*      Tell user about different axes
        CALL MSG_PRNT('Axes in input object are :')
        CALL MSG_PRNT(' ')
        CALL AXIS_TLIST( IFID, NDIM, STATUS )

        NSEL = 0
        CALL USI_GET1I( 'SELAX', NDIM, SELAX, NSEL, STATUS )
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
        CALL ARR_INIT1L( .FALSE., ADI__MXDIM, SPEC, STATUS )

*      Loop over input and create output dimensions
        DO I =1, NSEL
          IF ( ( SELAX(I) .GT. NDIM ) .OR. ( SELAX(I) .LT. 0 )  ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'FATAL ERROR : No such axis in input'/
     :                                            /' file!', STATUS )
          ELSE IF ( SPEC(I) ) THEN
            CALL MSG_SETI( 'AX', I )
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'FATAL ERROR : Axis ^AX is multiply'/
     :                             /' specified in output', STATUS )
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
      CALL BDI_CREAXES( OFID, NDIM, STATUS )

*    Create and map output data
      CALL BDI_CREDATA( OFID, NDIM, ODIMS, STATUS )
      CALL BDI_MAPDATA( OFID, 'WRITE', ODPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    One dimensional swap
      IF ( SWAP_1D ) THEN

*      Map input axis
        CALL BDI_MAPAXVAL( IFID, 'READ', 1, IAPTR, STATUS )

*      Create irregular output axis and map
        CALL BDI_CREAXVAL( OFID, 1, .FALSE., ODIMS(1), STATUS )
        CALL BDI_MAPAXVAL( OFID, 'WRITE', 1, OAPTR, STATUS )

*      Copy input data to output axis, and input axis to output data
        CALL ARR_COP1R( ODIMS(1), %VAL(IDPTR), %VAL(OAPTR), STATUS )
        CALL ARR_COP1R( ODIMS(1), %VAL(IAPTR), %VAL(ODPTR), STATUS )

*      Variances become axis widths and vice versa
        IF ( VOK ) THEN
          CALL BDI_CREAXWID( OFID, 1, .FALSE., ODIMS(1), STATUS )
          CALL BDI_MAPAXWID( OFID, 'WRITE', 1, OWPTR, STATUS )

*        Copy and square root
          CALL ARR_COP1R( ODIMS(1), %VAL(IVPTR), %VAL(OWPTR), STATUS )
          CALL ARR_SQRT1R( %VAL(OWPTR), ODIMS(1), STATUS )

        END IF
        CALL BDI_CHKAXWID( IFID, 1, IWOK, UNIF, NWID, STATUS )
        IF ( IWOK ) THEN

*        Create variance array, and map it
          CALL BDI_CREVAR( OFID, 1, ODIMS, STATUS )
          CALL BDI_MAPVAR( OFID, 'WRITE', OVPTR, STATUS )

*        Copy widths and square
          CALL ARR_COP1R( ODIMS(1), %VAL(IWPTR), %VAL(OVPTR), STATUS )
          CALL ARR_SQR1R( %VAL(OVPTR), ODIMS(1), STATUS )

        END IF

*      Just copy quality
        IF ( QOK ) THEN
          CALL BDI_COPQUAL( IFID, OFID, STATUS )
        END IF

*      Copy text strings
        CALL BDI_GETLABEL( IFID, LABEL, STATUS )
        CALL BDI_GETUNITS( IFID, UNITS, STATUS )
        CALL BDI_PUTAXTEXT( OFID, 1, LABEL, UNITS, STATUS )
        CALL BDI_GETAXTEXT( IFID, 1, LABEL, UNITS, STATUS )
        CALL BDI_PUTLABEL( OFID, LABEL, STATUS )
        CALL BDI_PUTUNITS( OFID, UNITS, STATUS )

*    normal swap
      ELSE

*      Copy axes
        DO I = 1, NDIM
          CALL BDI_COPAXIS( IFID, OFID, SELAX(I), I, STATUS )
        END DO

*      Copy top-level text
        CALL BDI_COPTEXT( IFID, OFID, STATUS )

*      Create output variance and quality
        IF ( VOK ) THEN
          CALL BDI_CREVAR( OFID, NDIM, ODIMS, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL BDI_CREQUAL( OFID, NDIM, ODIMS, STATUS )
          CALL BDI_GETMASK( IFID, MASK, STATUS )
          CALL BDI_PUTMASK( OFID, MASK, STATUS )
        END IF

*      Map output variance and quality
        IF ( VOK ) THEN
          CALL BDI_MAPVAR( OFID, 'WRITE', OVPTR, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL BDI_MAPQUAL( OFID, 'WRITE', OQPTR, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Pad out dimensions for 7D
        IF ( NDIM .LT. ADI__MXDIM ) THEN
          DO I = NDIM + 1, ADI__MXDIM
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
      CALL BDI_COPMORE( IFID, OFID, STATUS )

*    History
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL USI_NAMEI( NLINES, HTXT(2), STATUS )
      CALL HSI_PTXT( OFID, NLINES + 1, HTXT, STATUS )

*    Release datasets
      CALL BDI_RELEASE( IFID, STATUS )
      CALL BDI_RELEASE( OFID, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
