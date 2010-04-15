      SUBROUTINE STARCLEAN(CLEANARRAY,STARARRAY,CLEANSIZE,SKY,
     >SSKY,SKYSIGMA,XO,YO,
     >PIXSCALE,SCALE,INVAL,ZERO,CLEANEDARRAY,CLEANARRAYSTORE)

*     ARGUMENTS

*     CLEANARRAY (input)                 The extracted image to be cleaned
*     REAL*4 (45,45)

*     STARARRAY (input)                  The extracted cleaning star
*     REAL*4 (45,45)

*     CLEANSIZE (input)                  The side length of the extracted image
*     INTEGER

*     SKY,SKYSIGMA (input)               The median sky value and error as
*     REAL                               calculated by subroutine sky

*     SSKY (input)                       The median sky value at the cleaning
*     REAL                               star

*     XO,YO (input)                      The pixel nearest to fitted object
*     REAL                               centre

*     PIXSCALE (input)                   The pixel size in arcsecs
*     REAL

*     INVAL (input)                      Invalid pixel flag for IMAGE
*     INTEGER

*     SCALE,ZERO (input)                 Scale and zero level for IMAGE
*     REAL

*     CLEANEDARRAY (output)              The output cleaned array
*     REAL*4 (45,45)

*     CLEANARRAYSTORE (output)           The sky subtracted uncleaned array
*     REAL*4 (45,45)

C     Declare arrays

      REAL*4 CLEANARRAY(45,45),CLEANSTORE(45,45)
     >       ,CLEANEDARRAY(45,45),CLEANARRAYSTORE(45,45)
     >       ,STARARRAY(45,45)

C     Declare other stuff

      INTEGER CLEANSIZE,I,J,IMAX,JMAX,COUNTER,INVAL,K,L,
     >        IX,IY,WARNING,SPACER

      REAL*4 SKY,SKYSIGMA,XO,YO,X,Y,
!     >        SEEING,
     >        PIXSCALE,
!     >       GAMMA,
     >        NOISEPEAK,MAX,AMAX,OLDAMAX,FACTOR,STUFFY,
!     >         R,
!     >        RO,
     >       SIZE,
!     >       CHECKSUM1,CHECKSUM2,
     >       MAXY,MINY,TOP,TOPY,
     >       BOTY,SCALE,ZERO,SSKY,
!     >       NORM,
     >       PEAKHEIGHT

!      CHARACTER*30 FILE
      CHARACTER*3 ANS

C     Set up coords of object in the small array

      X=(REAL(CLEANSIZE)+1.0)/2.0
      Y=X
      IX=(CLEANSIZE+1)/2
      IY=IX

C     Set the Cleanstore array to zero

      DO J=1,45
         DO I=1,45
            CLEANSTORE(I,J)=0.0
         ENDDO
      ENDDO

C     Set the final output array to zero

      DO J=1,45
         DO I=1,45
            CLEANEDARRAY(I,J)=0.0
         ENDDO
      ENDDO

C     Cleaning profile should already be normalized

      WRITE(6,'(A)')' SLICES OF CLEANING STAR ABOUT TO BE PLOTTED.'
      CALL PLOTSLICE(STARARRAY,CLEANSIZE,1.2,-0.2,0.0)

C     Choose the level of noise to clean to

      NOISEPEAK=2.5*SKYSIGMA

C     Now subtract the sky value from the array and make a copy
C     for checking purposes

      TOP=0.0
      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            CLEANARRAY(I,J)=CLEANARRAY(I,J)-SKY
            CLEANARRAYSTORE(I,J)=CLEANARRAY(I,J)
            IF (CLEANARRAYSTORE(I,J).GT.TOP) THEN
               TOP=CLEANARRAYSTORE(I,J)
            ENDIF
         ENDDO
      ENDDO
      TOPY=1.2*TOP
      BOTY=-(0.2*TOP)

      WRITE(6,'(A)')' SLICES OF UNCLEANED OBJECT ABOUT TO BE PLOTTED.'

      CALL PLOTSLICE(CLEANARRAYSTORE,CLEANSIZE,
     >TOPY,BOTY,NOISEPEAK)

C     We must decide at this point if the object is bright enough
C     for a full blooded clean

      WRITE(6,'(A)')'$IS THE OBJECT BRIGHT (Y/N):'
      READ(5,'(A)')ANS
      IF ((ANS(1:1).EQ.'Y').OR.(ANS(1:1).EQ.'y')) THEN
        GOTO 11
      ELSE
        WRITE(6,'(A)')'   '
        WRITE(6,'(A)')' SINGLE CLEAN OPTION SELECTED '
        MAX=CLEANARRAY(IX,IY)
        DO J=1,CLEANSIZE
           DO I=1,CLEANSIZE
              STUFFY=(STARARRAY(I,J)*MAX)
              CLEANARRAY(I,J)=CLEANARRAY(I,J)-STUFFY
              CLEANSTORE(IX,IY)=CLEANSTORE(IX,IY)+STUFFY
           ENDDO
        ENDDO
        WRITE(6,'(A)')' NOW FORMING CLEANED IMAGE '
        DO J=1,CLEANSIZE
           DO I=1,CLEANSIZE
               cleanedarray(I,J)=cleanarray(I,J)
C              CLEANEDARRAY(I,J)=CLEANARRAY(I,J)+CLEANSTORE(I,J)
           ENDDO
        ENDDO
        GOTO 4
      ENDIF

C     Choose the factor that governs severity of the cleaning step

 11   WRITE(6,'(A)')'$INPUT CHOSEN LOOP GAIN : '
      READ(5,*)FACTOR

C     Search the array for the largest absolute peak


      COUNTER=0
      SPACER=0
      WARNING=0
      AMAX=1000000000.0
 1    MAX=0.0
      OLDAMAX=AMAX
      AMAX=0.0
      IMAX=0
      JMAX=0
      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            SIZE=ABS(CLEANARRAY(I,J))
            IF (SIZE.GT.AMAX) THEN
                AMAX=SIZE
                MAX=CLEANARRAY(I,J)
                IMAX=I
                JMAX=J
            ENDIF
         ENDDO
      ENDDO



C     Check that this peak is not below the noise
C     or that clean is starting to diverge

      IF (AMAX.GT.OLDAMAX) THEN
         WARNING=WARNING+1
      ENDIF

C     Arbitrarily choose 1000000 increases as divergence

      IF (WARNING.GT.1000000) THEN
         WRITE(9,'(A)')' CLEAN STOPPED BECAUSE OF DIVERGENCE '
         WRITE(6,'(A)')' CLEAN STOPPED BECAUSE OF DIVERGENCE '
         PEAKHEIGHT=AMAX/SKYSIGMA
         WRITE(9,1111)PEAKHEIGHT
 1111    FORMAT(' CLEANED TO ',F7.3,' SIGMA ')
         GOTO 3
      ENDIF
      IF (AMAX.LE.NOISEPEAK) THEN
         GOTO 2
      ENDIF

C     Take note of which clean this is

      COUNTER=COUNTER+1
      SPACER=SPACER+1
      IF (SPACER.EQ.1) THEN
      SPACER=0
      WRITE(6,90)COUNTER,IMAX,JMAX
 90   FORMAT(' CLEAN NO. ',I5,' AT ' ,I3,' , ',I3)
      WRITE(6,*)MAX,NOISEPEAK
      ENDIF

C     Do a clean, taking note of the position and the amount of flux removed

      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            STUFFY=0.0
            K=I-IMAX+IX
            L=J-JMAX+IY
            IF ((K.GE.1).AND.(K.LE.CLEANSIZE)) THEN
               IF ((L.GE.1).AND.(L.LE.CLEANSIZE)) THEN
                   STUFFY=(STARARRAY(K,L)*FACTOR*MAX)
               ENDIF
            ENDIF
            CLEANARRAY(I,J)=CLEANARRAY(I,J)-STUFFY
            CLEANSTORE(IMAX,JMAX)=CLEANSTORE(IMAX,JMAX)+STUFFY
         ENDDO
      ENDDO
      GOTO 1

C     Now form the cleaned image

 2    WRITE(6,'(A)')' CLEANED TO 2.5 SIGMA OF SKY '
 3    WRITE(6,'(A)')' NOW FORMING CLEANED IMAGE '

      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            CLEANEDARRAY(I,J)=CLEANARRAY(I,J)+CLEANSTORE(I,J)
         ENDDO
      ENDDO

C     We now enter a checking section, slices,counts,etc


 4    MAXY=0.0
      MINY=0.0
      DO J=1,CLEANSIZE
         DO I=1,CLEANSIZE
            IF (CLEANEDARRAY(I,J).GT.MAXY) THEN
               MAXY=CLEANEDARRAY(I,J)
            ENDIF
            IF (CLEANEDARRAY(I,J).LT.MINY) THEN
               MINY=CLEANEDARRAY(I,J)
            ENDIF
         ENDDO
      ENDDO

      WRITE(6,'(A)')' SLICES OF CLEANED OBJECT ABOUT TO BE PLOTTED '

      CALL PLOTSLICE(CLEANEDARRAY,CLEANSIZE,
     >MAXY,MINY,NOISEPEAK)

      RETURN
      END
