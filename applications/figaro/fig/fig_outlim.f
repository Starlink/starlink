C+
      SUBROUTINE FIG_OUTLIM(XOUT,OUTELM,NOUT,LOGOUT,STOUT,ENOUT)
C
C     F I G _ O U T L I M
C
C     Calculates the wavelength limits of an element of the
C     output wavelength array.
C
C     Parameters -   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) XOUT      (Double precision array XOUT(NOUT)) The wavelength
C                   values of the centres of the output elements.
C                   This routine assumes that the values in XOUT are
C                   in ascending order.
C     (>) OUTELM    (Integer) Number of the element in the output array.
C     (>) NOUT      (Integer) The number of elements in the output arrays.
C     (>) LOGOUT    (Logical) If true, indicates that the values of the
C                   output wavelength array increase logarithmicaly.
C     (<) STOUT     (Double precision) The start wavelength of the element.
C     (<) ENOUT     (Double precision) The end wavelength of the element.
C
C     Common variables used: None
C
C     External subroutines / functions used: None
C
C                                           KS / AAO 17th June 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LOGOUT
      INTEGER OUTELM, NOUT
      DOUBLE PRECISION STOUT, ENOUT, XOUT(NOUT)
C
C     The end values are special cases
C
      IF (OUTELM.EQ.1) THEN
         IF (LOGOUT) THEN
            STOUT=EXP(0.5*(3.0*LOG(XOUT(1))-LOG(XOUT(2))))
            ENOUT=EXP(0.5*(LOG(XOUT(1))+LOG(XOUT(2))))
         ELSE
            STOUT=0.5*(3.0*XOUT(1)-XOUT(2))
            ENOUT=0.5*(XOUT(1)+XOUT(2))
         END IF
      ELSE IF (OUTELM.EQ.NOUT) THEN
         IF (LOGOUT) THEN
            ENOUT=EXP(0.5*(3.0*LOG(XOUT(NOUT))-LOG(XOUT(NOUT-1))))
            STOUT=EXP(0.5*(LOG(XOUT(NOUT))+LOG(XOUT(NOUT-1))))
         ELSE
            ENOUT=0.5*(3.0*XOUT(NOUT)-XOUT(NOUT-1))
            STOUT=0.5*(XOUT(NOUT)+XOUT(NOUT-1))
         END IF
      ELSE
C
C        Normal case is just linear interpolation between
C        adjacent values.
C
         IF (LOGOUT) THEN
            STOUT=EXP(0.5*(LOG(XOUT(OUTELM-1))+LOG(XOUT(OUTELM))))
            ENOUT=EXP(0.5*(LOG(XOUT(OUTELM))+LOG(XOUT(OUTELM+1))))
         ELSE
            STOUT=0.5*(XOUT(OUTELM-1)+XOUT(OUTELM))
            ENOUT=0.5*(XOUT(OUTELM)+XOUT(OUTELM+1))
         END IF
      END IF
C
      END
