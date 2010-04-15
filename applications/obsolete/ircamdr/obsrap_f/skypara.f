      SUBROUTINE SKYPARA(SKYARRAY,SKYSIZE,SKY,SKYSIGMA)

*     ARGUMENTS

*     SKYARRAY (input)                     The extracted sky subimage
*     REAL (45,45)

*     SKYSIZE (input)                      Size of sky subimage containing
*     INTEGER                              info

*     SKY,SKYSIGMA (output)                Sky parameters output back to
*     REAL                                 subroutine business

C     Declare arrays

      REAL*4 SKYARRAY(45,45),X(100000),Y(100000)
!     >       ,XX(100000),YY(100000),BINSKYCOUNT(100000)
      INTEGER*2 SKYCOUNT(100000)

C     Declare other stuff

      REAL*4 MAX,MIN,TOPCOUNT,TOTCOUNT,TOTSIZE,FACTOR,
     >       LOWERQUART,UPPERQUART,MEDIAN,MODE,
!     >       BINSIZE,
!     >       ESTIMATE,
     >       STANDERROR,CUT,SKY,SKYSIGMA

      INTEGER I,J,IMAX,IMIN,PIG,COUNTMAX,NUMBER,SKYSIZE,
     >        CHECK1,CHECK2,CHECK3,MAXCOUNT,
!     >        IBINSIZE,
!     >        IHALFBIN,START,STOP,STEP,COUNTER,
!     >        STARTY,
!     >        STOPY,
     >        ICUT

C     Find MAX and MIN sky values

      MAX=1.0
      MIN=1000000.0
      DO J=1,SKYSIZE
         DO I=1,SKYSIZE
            IF (SKYARRAY(I,J).GT.MAX) THEN
               MAX=SKYARRAY(I,J)
            ENDIF
            IF (SKYARRAY(I,J).LT.MIN) THEN
               IF (SKYARRAY(I,J).LT.1.0) THEN
                  SKYARRAY(I,J)=MAX
               ELSE
                  MIN=SKYARRAY(I,J)
               ENDIF
            ENDIF
         ENDDO
      ENDDO


      IMAX=NINT(MAX)
      IMIN=NINT(MIN)


C     Find the frequency distribution for the sky

      DO I=1,100000
         SKYCOUNT(I)=0
      ENDDO


      DO J=1,SKYSIZE
         DO I=1,SKYSIZE
            PIG=NINT(SKYARRAY(I,J))
            SKYCOUNT(PIG)=SKYCOUNT(PIG)+1
         ENDDO
      ENDDO


C     Get organized to plot histogram of sky

      COUNTMAX=0
      DO I=IMIN,IMAX
         IF (SKYCOUNT(I).GT.COUNTMAX) THEN
            COUNTMAX=SKYCOUNT(I)
         ENDIF
      ENDDO


      TOPCOUNT=(REAL(COUNTMAX))*1.5
      NUMBER=IMAX-IMIN+1

      DO I=1,NUMBER
         X(I)=REAL(I)+MIN-1.0
         Y(I)=REAL(SKYCOUNT(I+IMIN-1))
      ENDDO

C     Now plot histogram

**      CALL PGBEGIN(10,'SKYDISTRIBUTION.DAT/VERSATEC',1,1)
**      CALL PGENV(MIN,MAX,0.0,TOPCOUNT,0,0)
**      CALL PGLABEL('SKY COUNTS','NO. OF PIXELS','SKY DIST.')
**      CALL PGLINE(NUMBER,X,Y)
**      CALL PGEND

**      WRITE(6,'(A)')' SKY HISTOGRAM PLOTTED TO
**     >SKYDISTRIBUTION.DAT'

C     Now find the mode,median and the upper and lower quartiles
C     which bound the same percentage of pixels as the one sigma
C     points would, ie  68.3%

C     Remember that the total no. of sky pixels is SKYSIZE*SKYSIZE

      TOTSIZE=REAL(SKYSIZE*SKYSIZE)
      TOTCOUNT=0.
      CHECK1=0
      CHECK2=0
      CHECK3=0
      MAXCOUNT=0

      DO I=IMIN,IMAX
         IF (SKYCOUNT(I).GT.MAXCOUNT) THEN
            MAXCOUNT=SKYCOUNT(I)
            MODE=REAL(I)
         ENDIF
         TOTCOUNT=TOTCOUNT+REAL(SKYCOUNT(I))
         FACTOR=TOTCOUNT/TOTSIZE
         IF (FACTOR.GE.0.1585) THEN
            IF (CHECK1.EQ.0) THEN
               CHECK1=1
               LOWERQUART=REAL(I)
            ENDIF
         ENDIF
         IF (FACTOR.GE.0.50) THEN
            IF (CHECK2.EQ.0) THEN
               CHECK2=1
               MEDIAN=REAL(I)
            ENDIF
         ENDIF
         IF (FACTOR.GE.0.8415) THEN
            IF (CHECK3.EQ.0) THEN
               CHECK3=1
               UPPERQUART=REAL(I)
            ENDIF
         ENDIF

      ENDDO

C     Now we rebin the frequency distribution to achieve a resolution
C     ~10 bins between the one sigma quartiles

**      BINSIZE=(UPPERQUART-LOWERQUART)/10.

C     We make the integer bin size the nearest odd number to binsize

**      IHALFBIN=INT(BINSIZE/2.0)
**      IBINSIZE=(2*IHALFBIN)+1

C     Do the rebinning

**      ESTIMATE=(MAX-MIN)/BINSIZE
**      START=IMIN+IHALFBIN
**      STOP=START+((NINT(ESTIMATE))*IBINSIZE*2)
**      STEP=IBINSIZE
**      COUNTER=0
**      DO I=START,STOP,STEP
**         BINSKYCOUNT(I)=0.
**         STARTY=I-IHALFBIN
**         STOPY=I+IHALFBIN
**         DO J=STARTY,STOPY
**            BINSKYCOUNT(I)=BINSKYCOUNT(I)+SKYCOUNT(J)
**         ENDDO
**         BINSKYCOUNT(I)=(BINSKYCOUNT(I))/(IBINSIZE)

C        Produce real arrays for plotting histogram

**         COUNTER=COUNTER+1

**         XX(COUNTER)=REAL(I)
**         YY(COUNTER)=BINSKYCOUNT(I)
**      ENDDO

C     Plot histogram

**      CALL PGBEGIN(20,'BINNEDSKY.DAT/VERSATEC',1,1)
**      CALL PGENV(MIN,MAX,0.0,TOPCOUNT,0,0)
**      CALL PGLABEL('SKY COUNTS','NO. OF PIXELS','BINNED SKY')
**      CALL PGLINE(COUNTER,XX,YY)
**      CALL PGEND

**      WRITE(6,'(A)')' BINNED SKY DISTRIBUTION HISTOGRAM
**     >PLOTTED TO BINNEDSKY.DAT'

C     We now write out the sky parameters from the unbinned data


      WRITE(6,'(A)')'     '
      WRITE(6,'(A)')' From the unbinned sky data we get '
      WRITE(6,'(A)')'     '
      WRITE(6,50)MODE
 50   FORMAT(' THE MODE OF THE SKY IS ',F8.3)
      WRITE(6,51)MEDIAN
 51   FORMAT(' THE MEDIAN OF THE SKY IS ',F8.3)
      WRITE(6,52)LOWERQUART
 52   FORMAT(' THE LOWER QUARTILE IS ',F8.3)
      WRITE(6,53)UPPERQUART
 53   FORMAT(' THE UPPER QUARTILE IS ',F8.3)

C     Work out the standard error and write it out

      STANDERROR=(MEDIAN-LOWERQUART)/SQRT(TOTSIZE)
      WRITE(6,54)STANDERROR
 54   FORMAT(' THE STANDARD ERROR IS ',F8.3)
      WRITE(6,55)TOTSIZE
 55   FORMAT(' NUMBER OF PIXELS USED = ',F10.3)

C     Now do a 2 sigma cut and reevaluate the sky parameters

      CUT=MEDIAN+(2.0*(UPPERQUART-MEDIAN))
      ICUT=NINT(CUT)
      TOTCOUNT=0.0

C     Work out how many pixels this leaves

      DO I=IMIN,ICUT
         TOTCOUNT=TOTCOUNT+REAL(SKYCOUNT(I))
      ENDDO

C     Now revaluate the sky parameters

      TOTSIZE=TOTCOUNT

      TOTCOUNT=0.
      CHECK1=0
      CHECK2=0
      CHECK3=0
      MAXCOUNT=0

      DO I=IMIN,ICUT
         IF (SKYCOUNT(I).GT.MAXCOUNT) THEN
            MAXCOUNT=SKYCOUNT(I)
            MODE=REAL(I)
         ENDIF
         TOTCOUNT=TOTCOUNT+REAL(SKYCOUNT(I))
         FACTOR=TOTCOUNT/TOTSIZE
         IF (FACTOR.GE.0.1585) THEN
            IF (CHECK1.EQ.0) THEN
               CHECK1=1
               LOWERQUART=REAL(I)
            ENDIF
         ENDIF
         IF (FACTOR.GE.0.50) THEN
            IF (CHECK2.EQ.0) THEN
               CHECK2=1
               MEDIAN=REAL(I)
            ENDIF
         ENDIF
         IF (FACTOR.GE.0.8415) THEN
            IF (CHECK3.EQ.0) THEN
               CHECK3=1
               UPPERQUART=REAL(I)
            ENDIF
         ENDIF
      ENDDO

C     We now write out the altered sky parameters from the unbinned data

      WRITE(6,'(A)')'     '
      WRITE(6,'(A)')' Having done the 2 sigma cut we get '
      WRITE(6,'(A)')'     '
      WRITE(6,60)MODE
 60   FORMAT(' THE MODE OF THE SKY IS ',F8.3)
      WRITE(6,61)MEDIAN
 61   FORMAT(' THE MEDIAN OF THE SKY IS ',F8.3)
      WRITE(6,62)LOWERQUART
 62   FORMAT(' THE LOWER QUARTILE IS ',F8.3)
      WRITE(6,63)UPPERQUART
 63   FORMAT(' THE UPPER QUARTILE IS ',F8.3)

C     Now work out standard error and write it out

      STANDERROR=(MEDIAN-LOWERQUART)/SQRT(TOTSIZE)
      WRITE(6,64)STANDERROR
 64   FORMAT(' THE STANDARD ERROR IS ',F8.3)
      WRITE(6,65)TOTSIZE
 65   FORMAT(' NUMBER OF PIXELS USED = ',F10.3)

C     Finally assign the output sky parameters

      SKY=MEDIAN
      SKYSIGMA=MEDIAN-LOWERQUART

      RETURN
      END
