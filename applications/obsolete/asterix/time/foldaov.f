*+FOLDAOV  -  Period folding application
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
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
      CHARACTER*30 TYPE1,TYPE2
         PARAMETER( TYPE1='PERIODOGRAM' , TYPE2='PHASE_FOLD')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC            !Locator to input file
      CHARACTER*(DAT__SZLOC) LOCO1          !Locator to output periodogram
      CHARACTER*(DAT__SZLOC) LOCO2          !Locator to output phase file
      INTEGER NTOT                          !Total no of points in input array
      INTEGER NGOOD                         !No of good points in data arrays
      INTEGER NFREQ                         !No of frequencies to use
      INTEGER NPBIN                         !No of phase bins to use
*
      REAL P0,PEND,DP                       !Start & end period and increment

      REAL F0                               !Initial trial frequency
      REAL DF                               !Frequency increment
      REAL FEND                             !Final frequency
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

*    Local data :
*     <any DATA initialisations for local variables>
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'FOLDAOV Version 1.5-1')
*-
      CALL MSG_PRNT(VERSION)
*
      CALL AST_INIT(STATUS)
*
* Get data from input file
      CALL TIM_GETDATA(LOC, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     &                                           VPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Get period searching parameters from the environment
      CALL USI_GET0R('PERIOD', P0, STATUS)
      CALL USI_GET0R('PINC', DP, STATUS)
      CALL USI_GET0I('NPER', NFREQ, STATUS)
      CALL USI_GET0I('NPBIN', NPBIN, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Convert start period to frequency
      PEND = P0 + DP * (NFREQ-1)
      F0 = 1.0 / PEND
*
* Calculate frequency increment
      FEND = 1.0 / P0
      DF = (FEND - F0) / REAL (NFREQ-1)
*
* Make sure values are within bounds
      IF (F0 .LE. 0.0) THEN
         CALL MSG_SETR('PSTART', DP)
         CALL MSG_PRNT('Setting start period to ^PSTART')
*
         F0 = DF
      ENDIF
*
      IF (NPBIN .GT. 49) THEN
         CALL MSG_PRNT('Setting number of phase bins to 49')
         NPBIN=49
      ENDIF
*
* Map some dynamic space to hold the frequencies used, the significance at
* each frequency, the phase curve at the best period and some workspace.
      CALL DYN_MAPR(1, NFREQ, FPNTR, STATUS)
      CALL DYN_MAPR(1, NFREQ, SPNTR, STATUS)
      CALL DYN_MAPR(1, NPBIN, PPNTR, STATUS)
      CALL DYN_MAPR(1, NGOOD, WPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining virtual memory')
         GOTO 999
      ENDIF
*
* Call period searching routine
      CALL TIM_AOV(NGOOD, %val(TPNTR), %val(DPNTR), NFREQ, F0, DF,
     &               NPBIN, %val(WPNTR), %val(FPNTR), %val(SPNTR),
     &                                          %val(PPNTR), STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Produce output periodogram
      CALL TIM_OUTPUT('PER', TYPE1, NFREQ, %val(SPNTR), F0, DF,
     &                                                LOCO1, STATUS)
*
* Add axis label
      CALL BDA_PUTAXLABEL(LOCO1, 1, 'Fold frequency', STATUS)
      CALL BDA_PUTAXUNITS(LOCO1, 1, 'Hz', STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing periodogram file')
      ENDIF
*
* Produce output phase file
      CALL TIM_OUTPUT('FOLD', TYPE2, NPBIN, %val(PPNTR), 0.0,
     &                                1.0/REAL(NPBIN), LOCO2, STATUS)
*
* Add axis label
      CALL BDA_PUTAXLABEL(LOCO2, 1, 'Phase', STATUS)
      CALL BDA_PUTAXUNITS(LOCO2, 1, ' ', STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error writing phase file')
      ENDIF
*
999   CONTINUE
*
      CALL AST_CLOSE(STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from FOLDAOV',STATUS)
      ENDIF
*
      END
