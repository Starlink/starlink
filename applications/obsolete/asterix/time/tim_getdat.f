*+  TIM_GETDAT - Reads an input time series
      SUBROUTINE TIM_GETDAT( IFID, NTOT, NGOOD, TPNTR, DPNTR,
     :                                  LVAR, VPNTR, STATUS )
*    Description :
*      Gets an array of times,data values and variances from an
*      input datafile. Outputs the good values.
*    Environment parameters :
*     INPUT  = UNIV          Name of input datafile
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*
*      8 Jan 91 : Original (RDS)
*     10 Apr 93 : Removed UTIL_MOVEBYTE (for a change) (DJA)
*     11 Apr 95 : Use BDI rather than BDA (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Export :
*
      INTEGER			IFID			! Series dataset id
      INTEGER NTOT                      !Number of data points in input arrays
      INTEGER NGOOD                     !Number of data points in output arrays
      INTEGER TPNTR                     !Array of times of good points
      INTEGER DPNTR                     !Data array of good points
      LOGICAL LVAR                      !Were variances found in input file ?
      INTEGER VPNTR                     !Variance array of good points
*                                       !nb. if variances not present then
*                                       !the variance is set to the data array.
*    Status :
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER NDIM,DIMS(ADI__MXDIM)     ! Dimensions of input data array
      INTEGER ITPNTR                    ! Pointer to input times array
      INTEGER IDPNTR                    ! Pointer to input data array
      INTEGER IVPNTR                    ! Pointer to input variance array
      INTEGER IQPNTR                    ! Pointer to input quality array
      INTEGER DUM1,DUM2                 ! Pointers to workspace
      INTEGER NVAL,NBAD
      INTEGER QMIN,QMAX                 ! Quality min and max values

      BYTE			BADBITS			! Quality mask

      LOGICAL 			OK                      ! Data ok ?
      LOGICAL LQUAL                     ! Were quality values available ?
      LOGICAL REG                       ! Were axis values regularly spaced ?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get locator to input file
      CALL USI_TASSOCI( 'INP', '*', 'READ', IFID, STATUS )
*
      IF (STATUS .NE. SAI__OK) GOTO 99
*
*  Check data array is ok and find size
      CALL BDI_CHKDATA( IFID, OK, NDIM, DIMS, STATUS )
*
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error looking for input data array')
         IF (STATUS .EQ. SAI__OK) STATUS=SAI__ERROR
         GOTO 99
      ENDIF
*
*  Can only handle 1-d data
      IF (NDIM .GT. 1) THEN
         CALL MSG_PRNT('Error: Data must be 1 dimensional')
         STATUS = SAI__ERROR
         GOTO 99
      ENDIF
*
*  Set the number of data points
      NTOT = DIMS(1)
*
*  Map data array
      CALL BDI_MAPDATA( IFID, 'READ', IDPNTR, STATUS )
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping input data array')
         GOTO 99
      ENDIF
*
*  Check variance array is present
      CALL BDI_CHKVAR( IFID, OK, NDIM, DIMS, STATUS )
*
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('*Variance not present in input datafile*')
         LVAR=.FALSE.
      ELSE
         LVAR=.TRUE.
      ENDIF
*
*  Map variance array. NB: this returns the data array if the variance array
*  is missing.
      CALL BDI_MAPVAR( IFID, 'READ', IVPNTR, STATUS )
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error mapping input variance array')
         GOTO 99
      ENDIF
*
*  Check quality array is present
      CALL BDI_CHKQUAL( IFID, OK, NDIM, DIMS, STATUS )
*
      IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('*Quality not present in input datafile*')
         LQUAL=.FALSE.
      ELSE
         LQUAL=.TRUE.
      ENDIF
*
*  Map quality array if present
      IF (LQUAL) THEN
*
         CALL BDI_MAPQUAL( IFID, 'READ', IQPNTR, STATUS )
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error mapping input quality array')
            GOTO 99
         ENDIF

*    Get the mask value
         CALL BDI_GETMASK( IFID, BADBITS, STATUS )
         IF (STATUS .NE. SAI__OK) THEN
           BADBITS = QUAL__MASK
           CALL ERR_ANNUL( STATUS )
         ENDIF
*
*    If all quality values are good then dont bother quality checking.
         CALL ARR_RANGB(NTOT, %val(IQPNTR), QMIN, QMAX, STATUS )
*
         IF (QMIN .EQ. 0 .AND. QMAX .EQ. 0) LQUAL=.FALSE.
*
      ENDIF
*
*  Get the times of each data bin from the axis
      CALL BDI_CHKAXVAL( IFID, 1, OK, REG, NVAL, STATUS )
*
*    Exit if axis corrupt or incompatible
      IF (STATUS .NE. SAI__OK .OR. NVAL .NE. NTOT) THEN
         CALL MSG_PRNT('** Axis corrupted or incompatible with data '/
     :                /'array **')
         GOTO 99
      ENDIF
*
*    If the axis is missing tell user integers will be used.
      IF (.NOT. OK) THEN
         CALL MSG_PRNT('Axis values not available using 1.0,2.0,3.0...')
      ENDIF
*
*    Map the axis values. NB: if the axis is not present then this routine
*    returns integers i.e. 1.0,2.0,3.0...
      CALL BDI_MAPAXVAL( IFID, 'READ', 1, ITPNTR, STATUS )
*
      IF (STATUS .NE. SAI__OK)THEN
         CALL MSG_PRNT('Error mapping axis array')
         GOTO 99
      ENDIF
*
*  Map output time axis, data and variance arrays. NB: it doesn't matter
*  that these arrays have been mapped larger than they may be needed as
*  they will always be passed into subroutines which will declare them
*  to have NGOOD elements.
      CALL DYN_MAPR(1, NTOT, TPNTR, STATUS )
      CALL DYN_MAPR(1, NTOT, DPNTR, STATUS )
      CALL DYN_MAPR(1, NTOT, VPNTR, STATUS )
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining virtual memory')
         GOTO 99
      ENDIF
*
*  If quality values available and none zero then split off the good
*  data values
      IF (LQUAL) THEN
*
*    Map two temporary arrays
         CALL DYN_MAPI(1, NTOT, DUM1, STATUS )
         CALL DYN_MAPI(1, NTOT, DUM2, STATUS )
*
         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error obtaining virtual memory')
            GOTO 99
         ENDIF

*    Get the array of times of the good data points
         CALL UTIL_QUALSPLIT(1, 1, 1, NTOT, 1, 1, 1, %val(IQPNTR),
     :                   BADBITS, %val(ITPNTR), NGOOD, %val(DUM1),
     :                               %val(TPNTR), NBAD, %val(DUM2))

*    Get the array of data values of the good points
         CALL UTIL_QUALSPLIT(1, 1, 1, NTOT, 1, 1, 1, %val(IQPNTR),
     :                   BADBITS, %val(IDPNTR), NGOOD, %val(DUM1),
     :                               %val(DPNTR), NBAD, %val(DUM2))

*    Get the array of variances of the good data points
         CALL UTIL_QUALSPLIT(1, 1, 1, NTOT, 1, 1, 1, %val(IQPNTR),
     :                   BADBITS, %val(IVPNTR), NGOOD, %val(DUM1),
     :                               %val(VPNTR), NBAD, %val(DUM2))

*  If quality data is not available then copy the time,data and variance
*  input arrays into the output arrays
      ELSE
*
         CALL ARR_COP1R( NTOT, %VAL(ITPNTR), %VAL(TPNTR), STATUS )
         CALL ARR_COP1R( NTOT, %VAL(IDPNTR), %VAL(DPNTR), STATUS )
         CALL ARR_COP1R( NTOT, %VAL(IVPNTR), %VAL(VPNTR), STATUS )
         NGOOD = NTOT

      END IF

      CALL MSG_SETI('NGOOD', NGOOD)
      CALL MSG_SETI('NTOT', NTOT)
      CALL MSG_PRNT('Using ^NGOOD of the ^NTOT input data values')

*    Tidy up
      CALL DYN_UNMAP( DUM1,STATUS )
      CALL DYN_UNMAP( DUM2,STATUS )

 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'TIM_GETDAT', STATUS )
      END IF

      END
