      SUBROUTINE BUSINESS(IMAGE,NPIX,NLINES,INVAL,SCALE,ZERO)

*     ARGUMENTS

*     IMAGE (input)                The image
*     INTEGER*2 (NPIX,NLINES)

*     NPIX,NLINES (input)          The dimensions of the image
*     INTEGER

*     INVAL (input)                Invalid pixel flag for IMAGE
*     INTEGER

*     SCALE,ZERO (input)           Scale and zero level for IMAGE
*     REAL


C     Dimension arrays

      REAL IMAGE(NPIX,NLINES)
      REAL*4 SKYARRAY(45,45),CLEANARRAY(45,45),CLEANEDARRAY(45,45),
     >       CLEANARRAYSTORE(45,45),DISPLAYARRAY(90,90),
!     >       SSKYARRAY(45,45),
     >       STARARRAY(45,45),SURFBRIGHT(35),
     >       RADIUS(35),RADQUART(35),SURFUP(35),SURFDOWN(35),
     >       SURFSIG(35)

C     Declare other stuff

      INTEGER NPIX,NLINES,INVAL,I,J,K,SKYRAD,SKYSIZE,CLEANRAD,
     >        CLEANSIZE,NO,
!     >        summysumsum,
     >        OLDNO,NIPPLE
      INTEGER LUN1, LUN2
      REAL SCALE,ZERO,X,Y,XO,YO,PIXSCALE,SKY,SKYSIGMA,FLUX,
     >     MAGNITUDE,D,DIA,FLUXCHECK1,FLUXCHECK2,
!     >     ERR,
!     >     XX,
!     >     YY,
!     >     XXO,
!     >     YYO,
     >     SSKY,
!     >     SSKYSIGMA,
     >     ERRORMAG,
!     >     summy,
     >     OLDFLUX,CHECK,
     >     DOLD,MEANFLUX,FLUXSE,CHECKUP,CHECKDOWN,PA,AXRAT,ZP

!      CHARACTER*3 ANS
!      CHARACTER*30 NAME
!     >             FILE


C     First we ask for the pixel scale in arcseconds

      WRITE(6,'(A)')'$PIXEL SCALE IN ARCSECONDS = '
      READ(5,*)PIXSCALE
      WRITE(6,'(A)')'         '

C     Now we set up the dimensions of the subimages.
C     Take sky square to have side of 89 pixels

      SKYRAD=44
      SKYSIZE=(2*SKYRAD)+1

C     Take cleaning square to have side of 45 pixels

      CLEANRAD=22
      CLEANSIZE=(2*CLEANRAD)+1

C     Read in seeing profile, inserting it into a 45x45 array for simplicity

      CALL FIO_OPEN( 'stararray.dat', 'READ', 'NONE', 0, LUN1, STATUS)
      DO J=1,45
         DO I=1,45
           READ(LUN1,*)STARARRAY(I,J)
         ENDDO
      ENDDO
      CALL FIO_CLOSE( LUN1, STATUS )

C     -------------------------------------------------------

C     Now ask for xy position of the object from the terminal

      WRITE(6,'(A)')'       '
      WRITE(6,'(A)')'$ACCURATE X COORD OF OBJECT = '
      READ(5,*)XO
      WRITE(6,'(A)')'$ACCURATE Y COORD OF OBJECT = '
      READ(5,*)YO
      WRITE(6,'(A)')'       '
      WRITE(6,'(A)')'       '

C     Find accurate centre of object profile

C     CALL IMFIT(IMAGE,NPIX,NLINES,INVAL,SCALE,ZERO,XO,YO,X,Y,
C     >4.0)
      X = XO
      Y = YO

C     Redefine pixel centering using nearest integers to the fitted centre

      XO=NINT(X)
      YO=NINT(Y)

C     Send info to the terminal clarifying the results of finding the object

      WRITE(6,20)X
 20   FORMAT(' FITTED XCOORD OF OBJECT = ',F8.3)
      WRITE(6,21)Y
 21   FORMAT(' FITTED YCOORD OF OBJECT = ',F8.3)
      WRITE(6,'(A)')'       '
      WRITE(6,22)XO
 22   FORMAT(' CHOSEN INTEGER XCOORD IS THEREFORE = ',F8.3)
      WRITE(6,23)YO
 23   FORMAT(' CHOSEN INTEGER YCOORD IS THEREFORE = ',F8.3)
      WRITE(6,'(A)')'       '

C     Now we wish to extract 2 separate subimages, a large square one from
C     which the sky parameters can be calculated and an intermediate square
C     one which will define the region to be cleaned.


      CALL EXTRACT(IMAGE,NPIX,NLINES,XO,YO,SKYRAD,CLEANRAD,
     >             SKYSIZE,CLEANSIZE,SKYARRAY,CLEANARRAY)

C     Call the subroutine to estimate the sky parameters

      CALL SKYPARA(SKYARRAY,SKYSIZE,SKY,SKYSIGMA)
      WRITE(6,'(A)')'$SKY VALUE CALCULATED = '
      WRITE(6,*)SKY
      WRITE(6,'(A)')'$TRUE SKY VALUE = '
      READ(5,*)SKY
      WRITE(6,'(A)')'$SKY SIGMA CALCULATED = '
      WRITE(6,*)SKYSIGMA
      WRITE(6,'(A)')'$TRUE SKY SIGMA = '
      READ(5,*)SKYSIGMA
      WRITE(6,'(A)')'$POSITION ANGLE = '
      READ(5,*)PA
      WRITE(6,'(A)')'$AXIAL RATIO = '
      READ(5,*)AXRAT
      WRITE(6,'(A)')'$ZERO POINT = '
      READ(5,*)ZP

C     ------------------------------------------------------------
C     Now produce a contour plot of the area to be cleaned

      CALL CONTPLOT(CLEANARRAY,CLEANSIZE,SKY,SKYSIGMA)
      CALL PGEND

C     Now perform the actual clean

      CALL STARCLEAN(CLEANARRAY,STARARRAY,CLEANSIZE,SKY,SSKY,
     >SKYSIGMA,XO,YO,PIXSCALE
     >,SCALE,INVAL,ZERO,CLEANEDARRAY,CLEANARRAYSTORE)

C     Check that flux has been conserved in the cleaning process

      FLUXCHECK1=0.0
      FLUXCHECK2=0.0

      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            FLUXCHECK1=FLUXCHECK1+CLEANARRAYSTORE(I,J)
            FLUXCHECK2=FLUXCHECK2+CLEANEDARRAY(I,J)
         ENDDO
      ENDDO

      WRITE(6,'(A)')'      '
      WRITE(6,521)FLUXCHECK1
 521  FORMAT(' FLUX PRIOR TO CLEANING = ',F11.3)
      WRITE(6,522)FLUXCHECK2
 522  FORMAT(' FLUX AFTER CLEANING = ',F11.3)
      WRITE(6,'(A)')'      '

C     Now produce a contour plot of the cleaned image

      CALL CONTPLOT(CLEANEDARRAY,CLEANSIZE,0.0,SKYSIGMA)

C     Work out the series of aperture magnitudes

 15   FLUX=0.0
      CALL PGSCH(2.0)
      NO=0
      D=1.0/PIXSCALE
      DO I=2,35
      DOLD=D
      DIA=REAL(I)
      D=DIA/PIXSCALE
      OLDFLUX=FLUX
      OLDNO=NO
      NIPPLE=I
      IF (I.GT.10) THEN
         NIPPLE=I-10
         IF (I.GT.20) THEN
            NIPPLE=I-20
         ENDIF
      ENDIF
      CALL PGSCI(NIPPLE)

      CALL APERMAG(CLEANEDARRAY,CLEANSIZE,D,DOLD,SKY,
     >PA,AXRAT,FLUX,MEANFLUX,FLUXSE,MAGNITUDE,ERRORMAG,NO)
      CHECK=MEANFLUX/(100.0*0.62*0.62)
      CHECKUP=(MEANFLUX+FLUXSE)/(100.0*0.62*0.62)
      CHECKDOWN=(MEANFLUX-FLUXSE)/(100.0*0.62*0.62)
      IF (CHECK.LE.0.000000000001) THEN
         CHECK=0.000000000001
      ENDIF
      SURFBRIGHT(I)=ZP-2.5*LOG10(CHECK)
      IF (CHECKUP.LE.0.000000000001) THEN
         CHECKUP=0.0000000000011
      ENDIF
      SURFUP(I)=ZP-2.5*LOG10(CHECKUP)
      IF (CHECKDOWN.LE.0.000000000001) THEN
         CHECKDOWN=0.0000000000009
      ENDIF
      SURFDOWN(I)=ZP-2.5*LOG10(CHECKDOWN)
      RADIUS(I)=(DIA/2.0)-0.25
      RADQUART(I)=RADIUS(I)**0.25

C     Output

      WRITE(9,399)DIA
      WRITE(6,399)DIA
 399  FORMAT(' APERTURE DIAMETER IN ARCSECONDS = ',F7.3)
      WRITE(9,400)FLUX
 400  FORMAT(' THE INTEGRATED FLUX IS ',F11.3)
      WRITE(9,401)MAGNITUDE,ERRORMAG
 401  FORMAT(' THE INSTRUMENTAL MAGNITUDE IS ',F7.3,'+ or -',F7.3)
      WRITE(9,402)NO
 402  FORMAT(' THE NUMBER OF PIXELS IS ',I5)
      WRITE(9,'(A)')'      '
      WRITE(9,'(A)')'      '

      ENDDO
      CALL PGEND

C     Write radial profiles

      CALL PGBEGIN(13,'?',1,1)
      CALL PGENV(0.0,16.0,23.0,13.0,0,0)
      CALL PGLABEL('arcsec','surface brightness',' ')
      CALL PGPOINT(35,RADIUS,SURFBRIGHT,17)
      CALL PGERRY(35,RADIUS,SURFDOWN,SURFUP,1.0)
      CALL PGENV(0.85,2.0,23.0,13.0,0,0)
      CALL PGLABEL('arcsec\U1/4\D','surface brightness',' ')
      CALL PGPOINT(35,RADQUART,SURFBRIGHT,17)
      CALL PGERRY(35,RADQUART,SURFDOWN,SURFUP,1.0)
      CALL PGEND

C     Write out bollocks to data file for straight line fitting
      CALL FIO_OPEN( 'profile.dat', 'WRITE', 'NONE', 0, LUN2, STATUS)

      DO I=2,35
         SURFSIG(I)=((SURFBRIGHT(I)-SURFUP(I))+
     >              (SURFDOWN(I)-SURFBRIGHT(I)))/2.0
         RADIUS(I)=(RADIUS(I)+AXRAT*RADIUS(I))/2.0
         WRITE(LUN2,*)RADIUS(I),SURFBRIGHT(I),SURFUP(I),
     >              SURFDOWN(I)
      ENDDO
      CALL FIO_CLOSE( LUN2, STATUS )

C     Write out cleaned and uncleaned images into two quadrants of a big frame

      DO J=1,45
         DO I=1,45
            DISPLAYARRAY(I,J)=CLEANARRAYSTORE(I,J)
         ENDDO
      ENDDO
      DO I=1,45
         DO J=46,90
            K=J-45
            DISPLAYARRAY(I,J)=CLEANEDARRAY(I,K)
         ENDDO
      ENDDO

C     At this point colin you might want to output DISPLAYARRAY into an
C     SDF or something

      RETURN
      END
