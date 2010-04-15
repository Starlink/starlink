
      SUBROUTINE TSP_PHSXLIMITS(X,STRT,FIN,XN,STATUS)
*+
*
*  T S P _ P H S X L I M I T S
*
*  TSP PHASEPLOT command - set X limits
*
*  Determine the X (time axis) limits to use - The original range of
*  X values are used as defaults and the user is prompted for new
*  values. The phase of each point in the range is then determined and
*  returned in the output array XN.
*
*
*  The program assumes that data are in order of increasing time
*
*  (>) X       (Double)  Original array of X data (MJD values)
*  (!) STRT    (Integer) Start index
*  (!) FIN     (Integer) Finish index
*  (<) XN      (Double)  New array of X data (Binary Phases)
*  (!) STATUS  (Integer) status value
*
*   Jeremy Bailey   28/2/1988
*
*   Modified:
*       11/12/1991
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters

      INTEGER STRT,FIN,STATUS
      DOUBLE PRECISION X(*),XN(*)

*  Local variables

      DOUBLE PRECISION XS,XE,EPOCH,PERIOD
      INTEGER ISTRT,IFIN,I
      LOGICAL WHOLE

      IF (STATUS .EQ. SAI__OK) THEN

*  Get WHOLE parameter (use whole data set)

          CALL PAR_GET0L('WHOLE',WHOLE,STATUS)
          XS = 0.0D0
          XE = 0.0D0
          IF (.NOT. WHOLE) THEN

*  If WHOLE is not TRUE prompt the user for the range to use

              CALL PAR_DEF0D('XSTART',X(STRT),STATUS)
              CALL PAR_GET0D('XSTART',XS,STATUS)
              CALL PAR_DEF0D('XEND',X(FIN),STATUS)
              CALL PAR_GET0D('XEND',XE,STATUS)
          ELSE
              XS = X(STRT)
              XE = X(FIN)
          ENDIF

*  Look for a position in the time array corresponding to the requested
*  X values for start and finish

          ISTRT = STRT
          IFIN = FIN
          DO I=STRT,FIN-1
              IF (X(I) .LE. XS .AND. X(I+1) .GE. XS) THEN
                  ISTRT=I
              ENDIF
              IF (X(I) .LE. XE .AND. X(I+1) .GE. XE) THEN
                  IFIN=I+1
              ENDIF
          ENDDO
          STRT = ISTRT
          FIN = IFIN

*  Get the EPOCH and PERIOD for phasing

          CALL PAR_GET0D('EPOCH',EPOCH,STATUS)
          CALL PAR_GET0D('PERIOD',PERIOD,STATUS)

*  Put an array of phase values into the output array (XN)

          DO I=1,FIN-STRT+1
              XN(I)=(X(STRT+I-1)-EPOCH)/PERIOD
              XN(I)=XN(I)-INT(XN(I))
              IF (XN(I) .LT. 0.0) XN(I)=XN(I)+1.0
          ENDDO
      ENDIF
      END


