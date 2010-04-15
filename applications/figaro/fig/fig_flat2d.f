      SUBROUTINE FIG_FLAT2D(DATA,VARIANCE,VEXIST,FLAT,FVARIANCE,
     :                      NPIX,NLINE,IORDER,SPECTRUM,X,Y)

*     Flattens a 2d spectrum.

*     Parameters -
*
*     (!) data      (real data(npix,nline)) data to be flattened
*     (!) variance  (real variance(npix,nline)) variance of data
*     (>) vexist    (logical vexist .true. if errors to be propagated
*     (>) flat      (real flat(npix,nline)) flat field data
*     (>) fvariance (real fvariance(npix,nline)) flat field variance
*     (>) npix      (integer) x-dimension of data
*     (>) nline     (integer) y-dimension of data
*     (>) iorder    (integer) order for fit to profile
*     (<) spectrum  (real spectrum(npix)) work area to hold profile
*     (<) x         (real x(npix)) work area for profile fit
*     (<) y         (real y(npix)) work area for profile fit

*     Original routine by JT, modified to run as a subroutine
*                                               KS /AAO  5th June 1983
*
*     26th Mar 1997  JJL / Starlink, Southampton. Error propagation added.
*
*     Uppercase code and no length specifiers (*4,*8), TABs removed.
*     FITLPOLY and LPOLY are now JTY_...
*                                    HME / UoE, Starlink, 6th Oct 1992

      INTEGER NPIX,NLINE,IORDER
      REAL DATA(NPIX,NLINE)
      REAL FLAT(NPIX,NLINE)
      REAL FVARIANCE(NPIX,NLINE)
      REAL VARIANCE(NPIX,NLINE)
      LOGICAL VEXIST
      REAL SPECTRUM(NPIX)
      REAL X(NPIX), Y(NPIX)

      REAL OLDDATA, SCALE(2), JTY_LPOLY
      DOUBLE PRECISION COEFF(8)

      DO 10 I=1,NPIX
10       SPECTRUM(I)=0.

      DO 20 J = 1,NLINE
         DO 20 I = 1,NPIX
20          SPECTRUM(I) = SPECTRUM(I) + FLAT(I,J)/FLOAT(NLINE)

      NCOEFF = IORDER + 1
      NPT = 0
      DO I = 1,NPIX
         IF(SPECTRUM(I).GT.0) THEN
            NPT = NPT + 1
            X(NPT) = I
            Y(NPT) = ALOG(SPECTRUM(I))
         ENDIF
      ENDDO
      SCALE(1) = X(1)
      SCALE(2) = X(NPT)
      CALL JTY_FITLPOLY(NPT,X,Y,SCALE,NCOEFF,COEFF)
      DO I = 1,NPIX
         SPECTRUM(I) = EXP(JTY_LPOLY(FLOAT(I),SCALE,NCOEFF,COEFF))
      ENDDO

      FRACMIN = 0.1
      DO J = 1,NLINE
         DO I = 1,NPIX
            IF(FLAT(I,J).GE.FRACMIN*SPECTRUM(I)) THEN
               TEMP = DATA(I,J)
               TEMP = TEMP / FLAT(I,J) * SPECTRUM(I)
               OLDDATA = DATA(I,J)
               DATA(I,J) = TEMP
               IF (VEXIST) THEN
C
C     Calculate the errors. Put in a trap to avoid either, or
C     both the flat field and the data being zero. Also check
C     the variance isn't zero.
C
               IF (VARIANCE(I,J).GT.0.AND.DATA(I,J).NE.0
     :            .AND.FVARIANCE(I,J).GT.0.AND.FLAT(I,J).GT.0) THEN
C
C     Neither are zero
C
                    TEMP = VARIANCE(I,J)/(OLDDATA**2.)
                    TEMP = TEMP + (FVARIANCE(I,J)/(FLAT(I,J)**2.))
                    TEMP = TEMP * (DATA(I,J)**2.)
                    VARIANCE(I,J) = TEMP
               ELSE
                    VARIANCE(I,J) = 0
                    DATA (I,J) = 0
               ENDIF

               ENDIF

            ENDIF

         ENDDO
      ENDDO

      END
