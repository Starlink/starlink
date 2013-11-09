*+  FOLDAOV - Period folding application
      SUBROUTINE FOLDAOV(STATUS)
*    Description :
*     Finds the best period fit to a 1-dimensional dataset
*     Produces an output periodogram and a phase file folded at the
*     best frequency.
*    Environment parameters :
*    Method :
*     The initial data array is checked for quality. Only those points
*     with good quality are passed from TIM_GETDATA back to the main
*     program. The variances of these points are set to the data values
*     if no variance array is present in the datafile.
*     The data are passed into the period searching routine and the
*     best period displayed to the user.
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     date:  changes (institution::username)
*     17 Apr 95 : V1.8-0  Updated for new data interfaces (DJA)
*     11 Dec 1995 : V2.0-0 ADI port (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*    Local variables :
      INTEGER NTOT                          !Total no of points in input array
      INTEGER NGOOD                         !No of good points in data arrays
      INTEGER NFREQ                         !No of frequencies to use
      INTEGER NPBIN                         !No of phase bins to use
*
      REAL P0,PEND,DP                       !Start & end period and increment

      REAL 			F0                      ! Initial trial freq
      REAL 			DF                      ! Frequency increment
      REAL 			FEND                    ! Final frequency

      INTEGER			IFID			! Input dataset id
      INTEGER			OFID1			! O/p fold dataset id
      INTEGER			OFID2			! O/p phase dataset id

*    pointers:
      INTEGER TPNTR                         !Times of good data
      INTEGER DPNTR                         !Data with good quality
      INTEGER VPNTR                         !Variance of points with good qual.
      INTEGER FPNTR                         !Array of frequencies used
      INTEGER SPNTR                         !Array of fit statistics
      INTEGER PPNTR                         !Phase curve for best fit period
      INTEGER WPNTR                         !Wokspace array
*
      LOGICAL LVAR                          !Is variance present in data file ?

*    Version :
      CHARACTER*30	 	VERSION
        PARAMETER 		( VERSION = 'FOLDAOV Version 2.2-0' )
*-

*  Version
      CALL MSG_PRNT( VERSION )

*  Initialise
      CALL AST_INIT()

*  Get data from input file
      CALL TIM_GETDAT( IFID, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     :                                           VPNTR, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Get period searching parameters from the environment
      CALL USI_GET0R('PERIOD', P0, STATUS)
      CALL USI_GET0R('PINC', DP, STATUS)
      CALL USI_GET0I('NPER', NFREQ, STATUS)
      CALL USI_GET0I('NPBIN', NPBIN, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Convert start period to frequency
      PEND = P0 + DP * (NFREQ-1)
      F0 = 1.0 / PEND

*  Calculate frequency increment
      FEND = 1.0 / P0
      DF = (FEND - F0) / REAL (NFREQ-1)

*  Make sure values are within bounds
      IF (F0 .LE. 0.0) THEN
         CALL MSG_SETR('PSTART', DP)
         CALL MSG_PRNT('Setting start period to ^PSTART')
         F0 = DF
      ENDIF
*
      IF (NPBIN .GT. 49) THEN
         CALL MSG_PRNT('Setting number of phase bins to 49')
         NPBIN=49
      ENDIF

*  Map some dynamic space to hold the frequencies used, the significance at
*  each frequency, the phase curve at the best period and some workspace.
      CALL DYN_MAPR(1, NFREQ, FPNTR, STATUS)
      CALL DYN_MAPR(1, NFREQ, SPNTR, STATUS)
      CALL DYN_MAPR(1, NPBIN, PPNTR, STATUS)
      CALL DYN_MAPR(1, NGOOD, WPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining virtual memory')
         GOTO 99
      ENDIF

*  Call period searching routine
      CALL TIM_AOV(NGOOD, %val(TPNTR), %val(DPNTR), NFREQ, F0, DF,
     :               NPBIN, %val(WPNTR), %val(FPNTR), %val(SPNTR),
     :                                          %val(PPNTR), STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 99

*  Produce output periodogram
      CALL TIM_PUTOUT( 'PER', 'Periodogram', NFREQ, %VAL(SPNTR), F0, DF,
     :                                           OFID1, STATUS )

*  Add axis label
      CALL BDI_AXPUT0C( OFID1, 1, 'Label', 'Fold frequency', STATUS )
      CALL BDI_AXPUT0C( OFID1, 1, 'Units', 'Hz', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error writing periodogram file')
      END IF

*  Produce output phase file
      CALL TIM_PUTOUT('FOLD', 'PhaseFold', NPBIN, %val(PPNTR), 0.0,
     :                             1.0/REAL(NPBIN), OFID2, STATUS )

*  Add axis label
      CALL BDI_AXPUT0C( OFID2, 1, 'Label', 'Phase', STATUS )
      CALL BDI_AXPUT0C( OFID2, 1, 'Units', ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT('Error writing phase file')
      END IF

*  Release workspace
      CALL DYN_UNMAP( FPNTR, STATUS )
      CALL DYN_UNMAP( SPNTR, STATUS )
      CALL DYN_UNMAP( PPNTR, STATUS )
      CALL DYN_UNMAP( WPNTR, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
