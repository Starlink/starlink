*+  LOMBSCAR  -  Calculates power spectrum with associated error estimate
      SUBROUTINE LOMBSCAR(STATUS)
*    Description :
*
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     The initial data array is checked for quality. Only those points
*     with good quality are passed from TIM_GETDAT back to the main
*     program. The variances of these points are set to the data values
*     if no variance array is present in the datafile.
*    Authors :
*     Richard Saxton (LTVAD::RDS)
*    History :
*     date:  changes (institution::username)
*     ?? ??? ?? : V1.5-0 Original (RDS)
*     11 Apr 95 : V1.8-0 Updated data interfrace (DJA)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    Local constants :
      CHARACTER*30 TYPE
          PARAMETER(TYPE='Power_spectrum')
*    Local variables :
      CHARACTER*80 PATH(4)                  !Input data path
      INTEGER			IFID, OFID		! Dataset identifiers
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

*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'LOMBSCAR Version 1.8-0')
*-

      CALL MSG_PRNT(VERSION)
      CALL AST_INIT()

*  Get data from input file
      CALL TIM_GETDAT( IFID, NTOT, NGOOD, TPNTR, DPNTR, LVAR,
     :                                        VPNTR, STATUS )
      IF (STATUS .NE. SAI__OK) GOTO 999

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

*  Call time series application
      CALL TIM_FASPER(NGOOD, %val(TPNTR), %val(DPNTR), OFAC, HIFAC, F1,
     :         %val(W1PNTR), NWORK, NWIND, %val(W2PNTR), %val(WNPNTR),
     :         %val(W3PNTR), %val(PPNTR), DF, NOUT, PROB)

*  Save the result
      CALL TIM_PUTOUT( 'OUT', TYPE, NOUT, %val(PPNTR), DF/2.0,
     :                                      DF, OFID, STATUS )

*  Add axis label and units
      CALL BDI_PUTAXTEXT( OFID, 1, 'Frequency', 'Hz', STATUS )

*  Add data label and units
      CALL BDI_PUTLABEL(OFID, 'Power', STATUS)
      CALL BDI_PUTUNITS(OFID, '(Counts/s)**2', STATUS)

*  Add history
      CALL HSI_COPY( IFID, OFID, STATUS)
      CALL HSI_ADD(OFID, VERSION, STATUS)
      CALL USI_NAMEI(NLINES, PATH, STATUS)
      CALL HSI_PTXT(OFID, NLINES, PATH, STATUS)

*   Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
