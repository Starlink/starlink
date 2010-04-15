*+MAN_MOONC        Finds periods when moon constraint prevents viewing
      SUBROUTINE MAN_MOONC(SOP,EOP,DRA,DDEC,MANG,
     &    MAX_NPERIODS,MJDINT,NPERIODS)
      IMPLICIT NONE

*   Input :
      DOUBLE PRECISION SOP			!Start of period of interest
      DOUBLE PRECISION EOP			!End of period of interest
      DOUBLE PRECISION DRA 			!Right ascension of source, rad
      DOUBLE PRECISION DDEC			!Declination of source, rad.
      REAL MANG				!Maximum angle between moon and source vector that will cause
					!a violation of the constraint, radian
      INTEGER MAX_NPERIODS		! max no periods to get

*   Output :
      DOUBLE PRECISION MJDINT( 2, MAX_NPERIODS)	!MJD intervals, start and end
      INTEGER NPERIODS				!Occultation time interval count

* History
*     1986 Dec	M Bush		1st version for MAN
*     1989 Feb	M Ricketts	minor mods
*-
*   Internal :
      INTEGER IY,IM,ID,NY,ND          !Date variables
      INTEGER K
      DOUBLE PRECISION MJD            !Modified Julian Date
      DOUBLE PRECISION FD             !Fraction of day
      REAL FDR                        !Fraction of day
      REAL MNANG                      !Angle between source and Moon vector
      REAL POSVEL(6)                  !Moon position and velocity vector
      REAL MPOSN(3)                   !Normalised moon position vector
      REAL TV(3)                      !Target vector
      REAL RAR				!Right ascension of source
      REAL DECR				!Declination of source
      REAL D2R                        !Degrees to Radians
      REAL RV_DOT                       !Dot product between moon vector and
                                      !viewing vector

      LOGICAL BLOCK                   !TRUE      If source is blocked by Moon
                                      !FALSE     otherwise
      REAL ASTEP1, ASTEP2
      PARAMETER(D2R=0.0174532925, ASTEP1=1.5*D2R, ASTEP2=10.0*D2R)

* _________________________________ Executable Code ___________________________________

      BLOCK=.FALSE.
      NPERIODS = 0

      RAR =  DRA
      DECR = DDEC
      CALL RS_A2V(RAR,DECR,TV)
      MJD=SOP
      DO WHILE(MJD.LT.EOP)					! Repeat calculations until time is outside period of interest
         CALL SLA_DJCL(MJD,IY,IM,ID,FD,K)
         FDR=FD
         CALL SLA_CALYD(IY,IM,ID,NY,ND,K)
         CALL SLA_MOON(NY,ND,FDR,POSVEL)			! Find moon position
         CALL RV_NORM(POSVEL,MPOSN)				! COnvert posn in AU to unit vector
         MNANG=ACOS(RV_DOT(TV,MPOSN))				! Find angle between moon position vector and velocity vector

         IF(MNANG.LE.MANG)THEN
            IF(.NOT.BLOCK)THEN
               BLOCK=.TRUE.
               NPERIODS = NPERIODS + 1
               MJDINT(1, NPERIODS)=MJD					!Set time at which Moon occultation starts
            END IF
         ELSE
            IF(BLOCK)THEN
               BLOCK=.FALSE.
               MJDINT(2, NPERIODS)=MJD					!Set time at which Moon occultation ends
               IF (NPERIODS .EQ. MAX_NPERIODS) GOTO 100			! Exdit if got enough /array-full
            END IF
         END IF
         IF (ABS(MNANG) .LE. ASTEP1 ) THEN				! small time increment
            MJD=MJD+0.007
         ELSE IF (ABS(MNANG) .LE. ASTEP2 ) THEN				! large time increment
            MJD=MJD+0.05
         ELSE
            MJD=MJD+0.25
         END IF
      END DO

      IF(BLOCK)THEN
         MJDINT(2, NPERIODS) = EOP					!Close time interval if still open
      END IF
100   CONTINUE

      END
