*+  MATH_LINTERP - Performs linear interpolation over 1D array
      SUBROUTINE MATH_LINTERP(NG,XG,YG,NR,XR,YR,STATUS)
*    Description :
*     Interpolates linearly between the given real data YG(XG), to find y
*     values at the required points XR.
*     A single interpolated value is obtained by putting NR=1 and giving a
*     scalar XR.
*     The given array is assumed to be ordered monotonically in x, but the
*     required array need not be (though processing will be quickest if it is).
*     If a given value lies outside the range of XG then its y value is returned
*     as VAL__BADR (i.e.HDS undefined).
*    Environment parameters :
*    Method :
*     Simple linear interpolation.
*    Deficiencies :
*    Bugs :
*    Authors :
*     Trevor Ponman (BHVAD::TJP)
*    History :
*     18 Aug 88: Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
      INTEGER NG			! No of given values
      REAL XG(NG)			! Given x values
      REAL YG(NG)			! Given y values
      INTEGER NR			! No of values required
      REAL XR(NR)			! x values at which y is required
*    Import-Export :
*    Export :
      REAL YR(NR)			! Interpolated y values
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J			! Indices
*    Local data :
*-----------------------------------------------------------------------------

* Status check
      IF(STATUS.NE.SAI__OK) RETURN

* Check
      IF(NG.LE.1)THEN
        CALL ERR_REP('TOOFEW','Insufficient values for interpolation',
     :  STATUS)
        STATUS=SAI__ERROR
        GO TO 9000
      ENDIF

* Loop through required array
      I=1
      DO J=1,NR

* Check that value lies within interpolation range
        IF((XR(J).LT.XG(1)).OR.(XR(J).GT.XG(NG)))THEN
          YR(J)=VAL__BADR
        ELSE
          IF(XR(J).LT.XG(I))THEN
            I=1				! Reset start position
          ENDIF
          DO WHILE (XG(I).LT.XR(J))
            I=I+1
          ENDDO

* Trap case where value falls on first array point
          IF(I.EQ.1)THEN
            YR(J)=YG(1)
          ELSEIF (XG(I).EQ.XR(J)) THEN
            YR(J)=YG(I)
          ELSE
* First point g.e. XR found - interpolate to determine y
            YR(J)=YG(I-1)+(XR(J)-XG(I-1))*(YG(I)-YG(I-1))/(XG(I)-
     :      XG(I-1))
            I=I-1			! Decrement 1 for start of next search
          ENDIF
        ENDIF
      ENDDO

* Exit
 9000 CONTINUE
      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP(' ','from MATH_LINTERP',STATUS)
      ENDIF

      END
