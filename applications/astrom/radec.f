      SUBROUTINE RADEC (FIELDS,JFLAGS,RA,DEC,J)
*+
*
*     - - - - - -
*      R A D E C
*     - - - - - -
*
*  Convert input fields to RA,Dec
*
*  Given:
*     FIELDS     d()     Hr,Min,Sec,Deg,Arcmin,Arcsec
*     JFLAGS     i()     sla_DFLTIN status flags
*
*  Returned:
*     RA,DEC     d       RA,Dec
*     J          i       status:  0 = OK
*
*  The RA,Dec is computed irrespective of the validity of the fields
*
*+

      IMPLICIT NONE

      DOUBLE PRECISION FIELDS(*)
      INTEGER JFLAGS(*)
      DOUBLE PRECISION RA,DEC
      INTEGER J

      DOUBLE PRECISION H,TM,TS,D,AM,AS,F
      INTEGER I,JD,JF


*  Preset status
      J=0

*  Copy fields, checking for dubious non-integers
      H=FIELDS(1)
      IF (MOD(H,1D0).NE.0D0) J=1
      TM=FIELDS(2)
      IF (MOD(TM,1D0).NE.0D0) J=2
      TS=FIELDS(3)
      D=FIELDS(4)
      IF (MOD(D,1D0).NE.0D0) J=4
      AM=FIELDS(5)
      IF (MOD(AM,1D0).NE.0D0) J=5
      AS=FIELDS(6)

*  Look for dubious negatives and handle negative Dec
      DO I=6,1,-1
         JD=JFLAGS(I)
         IF (I.EQ.4) THEN
            IF (JD.LT.0) THEN
               F=-1D0
               D=-D
            ELSE
               F=1D0
            END IF
         ELSE
            IF (JD.LT.0) J=I
         END IF
      END DO

*  Convert to angles
      CALL sla_DTF2R(INT(H),INT(TM),TS,RA,JF)
      IF (J.EQ.0) J=JF
      CALL sla_DAF2R(INT(D),INT(AM),AS,DEC,JF)
      IF (JF.NE.0.AND.J.EQ.0) J=JF+3
      DEC=DEC*F

*  Exit

      END
