*+  LOMBSCAR  -  Calculates power spectrum with associated error estimate
      SUBROUTINE LOMBSCAR(STATUS)
*    Description :
*
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     The initial data array is checked for quality. Only those points
*     with good quality are passed from TIM_GETDATA back to the main
*     program. The variances of these points are set to the data values
*     if no variance array is present in the datafile.
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
      CHARACTER*30 TYPE
          PARAMETER(TYPE='Power_spectrum')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC            !Locator to input file
      CHARACTER*(DAT__SZLOC) LOCO           !Locator to output file
      CHARACTER*80 PATH(4)                  !Input data path
      INTEGER NTOT                          !Total no of points in input array
      INTEGER NGOOD                         !No of good points in data arrays
*    pointers:
      INTEGER TPNTR                         !Times of good data
      INTEGER DPNTR                         !Data with good quality
      INTEGER VPNTR                         !Variance of points with good qual.
      INTEGER W1PNTR,W2PNTR,W3PNTR          !Workspace arrays
      INTEGER PPNTR                         !Output power spectrum
      INTEGER WNPNTR                        !Output window spectrum

      INTEGER NWORK                         !Size of power spec. workspace array
      INTEGER NWIND                         !Size of window workspace arrays
      INTEGER NOUT                          !Number of output frequencies
      INTEGER NLINES                        !No of lines in input path

      REAL PROB                             !Error on highest peak frequency
      REAL F1                               !Window frequency to use
*                                           ! =0.0 if no window fn. wanted
      REAL DF                               !Frequency increment
      REAL OFAC                             !Oversampling ratio
      REAL HIFAC                            !Number of times Nyquist frequency
*                                           !to extend spectrum to.
      INTEGER VALUE,LP
      LOGICAL LVAR                          !Is variance present in data file ?

*    Local data :
*     <any DATA initialisations for local variables>
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'LOMBSCAR Version 1.5-1')
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
* Get input parameters
*   Oversampling function
      CALL USI_GET0R('OFAC', OFAC, STATUS)
*
*   Multiple of Nyquist frequency to extrapolate power spectrum to.
      CALL USI_GET0R('HIFAC', HIFAC, STATUS)
*
*   Window function frequency. This should be set to zero if window
*   function is not wanted
      CALL USI_GET0R('WFREQ', F1, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Calculate the maximum workspace requirement in TIM_FASPER
      VALUE = REAL(OFAC * HIFAC * NGOOD * 4.0)
*
      DO LP=1,100
         IF (2.0 ** LP .GT. VALUE) THEN
            NWORK = NINT( 2.0 ** (LP+1) ) + 2
            GOTO 10
         ENDIF
      ENDDO
*
10    CONTINUE
*
* Calculate the workspace needed for the window function calculation.
* This will be the same as NWORK if window function is being calculated
* otherwise just set to one.
      IF (F1 .GT. 0.0) THEN
         NWIND = NWORK
      ELSE
         NWIND = 1
      ENDIF
*
* Map dynamic arrays
*   Window workspace
      CALL DYN_MAPR(1, NGOOD, W1PNTR, STATUS)
      CALL DYN_MAPR(1, NWIND, W2PNTR, STATUS)
      CALL DYN_MAPR(1, NWIND, WNPNTR, STATUS)
*   Power spectrum workspace
      CALL DYN_MAPR(1, NWORK, W3PNTR, STATUS)
      CALL DYN_MAPR(1, NWORK, PPNTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error obtaining virtual memory')
         GOTO 999
      ENDIF
*
* Call time series application
      CALL TIM_FASPER(NGOOD, %val(TPNTR), %val(DPNTR), OFAC, HIFAC, F1,
     &         %val(W1PNTR), NWORK, NWIND, %val(W2PNTR), %val(WNPNTR),
     &         %val(W3PNTR), %val(PPNTR), DF, NOUT, PROB)
*
* Save the result
      CALL TIM_OUTPUT( 'OUT', TYPE, NOUT, %val(PPNTR), DF/2.0,
     &                                             DF, LOCO, STATUS)
*
* Add axis label and units
      CALL BDA_PUTAXLABEL(LOCO, 1, 'Frequency', STATUS)
      CALL BDA_PUTAXUNITS(LOCO, 1, 'Hz', STATUS)
*
* Add data label and units
      CALL BDA_PUTLABEL(LOCO, 'Power', STATUS)
      CALL BDA_PUTUNITS(LOCO, '(Counts/s)**2', STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error adding auxiliary info.')
      ENDIF
*
* Add history
      CALL HIST_COPY(LOC, LOCO, STATUS)
      CALL HIST_ADD(LOCO, VERSION, STATUS)
*
      CALL USI_NAMEI(NLINES, PATH, STATUS)
      CALL HIST_PTXT(LOCO, NLINES, PATH, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error adding history record')
      ENDIF
*
999   CONTINUE
*
      CALL AST_CLOSE(STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from LOMBSCAR',STATUS)
      ENDIF
*
      END
