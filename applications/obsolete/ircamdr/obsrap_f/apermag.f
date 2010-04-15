      SUBROUTINE APERMAG(CLEANEDARRAY,CLEANSIZE,D,DOLD,SKY,
     >PA,AXRAT,FLUX,MEANFLUX,FLUXSE,MAGNITUDE,ERRORMAG,NO)

*     ARGUMENTS

*     CLEANEDARRAY (input)               The extracted, cleaned image
*     REAL*4 (CLEANSIZE,CLEANSIZE)

*     CLEANSIZE (input)                  The side length of the extracted image
*     INTEGER

*     D (input)                          The diameter in pixels of the aperture
*     REAL

*     DOLD (input)                       The diameter in pixels of the old
*     REAL                               aperture

*     SKY (input)                        Sky value - required for poisson errors
*     REAL

*     PA (input)
*     REAL

*     AXRAT (input)
*     REAL

*     FLUX (output)
*     REAL

*     MEANFLUX (output)
*     REAL

*     FLUXSE (output)
*     REAL

*     MAGNITUDE (output)
*     REAL

*     ERRORMAG (output)
*     REAL

*     NO (output)
*     INTEGER

C     Declare arrayS

      IMPLICIT NONE

      REAL*4 CLEANEDARRAY(45,45),ANULUS(3000),
!     >       NEWANULUS(3000),
     >       AN1(3000),AN2(3000)
C     Declare other stuff

      INTEGER CLEANSIZE,I,J,NO
      REAL*4 D,
     >       FLUX,MAGNITUDE,RADIUS,APERRAD,X,Y,XO,YO,
     >       ERRORMAG,SKY,POISSON,FLATFIELD,ERROR,UPFLUX,
     >       DOWNFLUX,UPMAG,DOWNMAG,APERRADOLD,DOLD,MEANFLUX,
     >       FLUXSD,FLUXSE,SUM,RESID,
!     >       CUT,
     >       XSQUARE,YSQUARE,
     >       XSQUAREOLD,YSQUAREOLD,APERRAD_THETA,
     >       APERRADOLD_THETA,
     >       PA,AXRAT,THETA
      REAL*4 MAG                ! Never set in this program!!!

      MAG = 0.0

C     Tidy up anulus array

      DO I=1,3000
         ANULUS(I)=0.0
      ENDDO

C     Set up object position and aperture radius

      XO=REAL((CLEANSIZE+1)/2)+0.1
      YO=XO
      APERRAD=D/2.0
      APERRADOLD=DOLD/2.0

C     Set initial flux to zero

      FLUX=0.0
      NO=0
      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            X=REAL(I)
            Y=REAL(J)
            THETA=ATAN((XO-X)/(Y-YO))
            THETA=THETA-(PA*3.14159/180.0)
            IF (ABS(THETA).LT.0.000000001) THEN
               THETA=0.000000001
            ENDIF
            RADIUS=SQRT((X-XO)*(X-XO)+(Y-YO)*(Y-YO))
            XSQUARE=1.0 / ((1.0/(APERRAD*APERRAD*TAN(THETA)*
     >             TAN(THETA))) + (1.0/(AXRAT*AXRAT*
     >             APERRAD*APERRAD)))
            YSQUARE=1.0 / ((1.0/(APERRAD*APERRAD)) +
     >             ((TAN(THETA)*TAN(THETA))/(AXRAT*AXRAT*
     >             APERRAD*APERRAD)))
            APERRAD_THETA=SQRT(XSQUARE+YSQUARE)

            XSQUAREOLD=1.0 / ((1.0/(APERRADOLD*APERRADOLD*TAN(THETA)*
     >             TAN(THETA))) + (1.0/(AXRAT*AXRAT*
     >             APERRADOLD*APERRADOLD)))
            YSQUAREOLD=1.0 / ((1.0/(APERRADOLD*APERRADOLD)) +
     >             ((TAN(THETA)*TAN(THETA))/(AXRAT*AXRAT*
     >             APERRADOLD*APERRADOLD)))
            APERRADOLD_THETA=SQRT(XSQUAREOLD+YSQUAREOLD)

            IF (RADIUS.LE.APERRAD_THETA.AND.RADIUS.GT.
     >      APERRADOLD_THETA) THEN
               NO=NO+1
               ANULUS(NO)=CLEANEDARRAY(I,J)
               FLUX=FLUX+CLEANEDARRAY(I,J)
               AN1(NO)=REAL(I)
               AN2(NO)=REAL(J)
            ENDIF
         ENDDO
      ENDDO

C        Statistics

      CALL PGPOINT(NO,AN1,AN2,16)
      MEANFLUX=FLUX/REAL(NO)
      SUM=0.0
      DO I=1,NO
         RESID=(ANULUS(I)-MEANFLUX)
         SUM=SUM+(RESID**2.0)
      ENDDO
      FLUXSD=(SUM/(REAL(NO)-1.0))**0.5
      FLUXSE=FLUXSD/((REAL(NO))**0.5)


c---------------------------------------------------------------------------
C     2 sigma cut -- if not wanted, comment out.

*      IF (APERRAD.GT.19.0) THEN
*         CUT=MEANFLUX+2.0*FLUXSD
*         J=0
*         DO I=1,NO
*            IF (ANULUS(I).LT.CUT) THEN
*               J=J+1
*               NEWANULUS(J)=ANULUS(I)
*            ENDIF
*         ENDDO

*         NO=J
*         SUM=0.0
*         FLUX=0.0

*         DO I=1,NO
*            FLUX=FLUX+NEWANULUS(I)
*         ENDDO

*         MEANFLUX=FLUX/REAL(NO)

*         DO I=1,NO
*            RESID=(NEWANULUS(I)-MEANFLUX)
*            SUM=SUM+(RESID**2.0)
*         ENDDO
*         FLUXSD=(SUM/(REAL(NO)-1.0))**0.5
*         FLUXSE=FLUXSD/((REAL(NO))**0.5)
*      ENDIF
c---------------------------------------------------------------------------


      IF (FLUX.GE.0.000001) THEN
         MAGNITUDE=-2.5*(LOG10(FLUX))

C        Errors-assuming 1% error in flatfielding and
C        photon-count conversion factor of 15.0 for AAT or
C        2.5 for UH 88inch

         POISSON=(SQRT(((REAL(NO)*SKY)+FLUX)*15.0))/15.0
         FLATFIELD=(REAL(NO)*SKY)/100.0
         ERROR=SQRT((POISSON*POISSON)+(FLATFIELD*FLATFIELD))
         UPFLUX=FLUX+ERROR
         DOWNFLUX=FLUX-ERROR
         IF (UPFLUX.GE.0.0) THEN
            UPMAG=MAG-((-2.5)*(LOG10(UPFLUX)))
         ELSE
            UPMAG=0.0
         ENDIF
         IF (DOWNFLUX.GE.0.0) THEN
            DOWNMAG=((-2.5)*(LOG10(DOWNFLUX)))-MAG
         ELSE
            DOWNMAG=0.0
         ENDIF
         ERRORMAG=(UPMAG+DOWNMAG)/2.0
      ELSE
         MAGNITUDE=0.0
      ENDIF

      RETURN
      END

