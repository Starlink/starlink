C+
      SUBROUTINE XCUR
C
C     X C U R
C
C     Uses the graphics cursor to delimit a range in X,
C     setting the user variables XSTART and XEND accordingly.
C
C     Command variables / keywords - None.
C
C     User variables -     (">" input, "<" output)
C
C     (>) SOFT     (Character) Device / type for soft plots.
C                  See documentation on 'SOFT' command for
C                  details.
C     (>) TVXST    (Numeric) X-start value for current plot.
C     (>) TVXEN    (Numeric) X-end value for current plot.
C     (>) TVLOW    (Numeric) Lowest value of current plot.
C     (>) TVHIGH   (Numeric) Highest value of current plot.
C     (<) XSTART   (Numeric) X-start for next plot.
C     (<) XEND     (Numeric) X-end for next plot.
C
C                                  KS / CIT 26th July 1982
C     Modified:
C
C     5th Aug 1987 -  DJA/AAO. Revised DSA_ routines - some specs changed.
C                     All WRUSERS changed to PAR_WRUSERs.
C     17th MArch 1988 KS / AAO. Modified for GKS version of PGPLOT.
C+
      IMPLICIT NONE
C
C     Functions used
C
      INTEGER PGBEGIN,ICH_LEN
C
C     Local variables
C
      CHARACTER    CH           ! The current character being returned
      REAL         HIGH         ! The maximum brightness level
      INTEGER      IGNORE       ! Used to pass an ignorable status
      REAL         LOW          ! The minimum brightness level
      CHARACTER    PGDEV*32     ! The name of the plotting device being used
      INTEGER      STATUS       ! Running status for DSA_ routines
      REAL         X1           ! The first X position of the cursor
      REAL         X2           ! The second X position of the cursor
      REAL         XEN          ! The value of the right edge of the display
      REAL         XST          ! The value of the left edge of the display
      REAL         Y            ! The current Y position of the cursor
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of 'SOFT'
C
      CALL VAR_GETCHR('SOFT',0,0,PGDEV,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to get soft device name.',IGNORE)
         CALL PAR_WRUSER('Probably no plot has been made.',IGNORE)
         GO TO 500
      END IF
C
C     Get the user variables describing the plot limits
C
      CALL VAR_GETNUM('TVXST',0,0,XST,STATUS)
      IF (STATUS.EQ.0) THEN
         CALL VAR_GETNUM('TVXEN',0,0,XEN,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL VAR_GETNUM('TVLOW',0,0,LOW,STATUS)
            IF (STATUS.EQ.0) THEN
               CALL VAR_GETNUM('TVHIGH',0,0,HIGH,STATUS)
            END IF
         END IF
      END IF
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to obtain plot variables',IGNORE)
         CALL PAR_WRUSER('Probably no plot has been made.',IGNORE)
         GO TO 500
      END IF
C
C     Initiate PGPLOT - note use of /APPEND to prevent scrren erase.
C
      STATUS=PGBEGIN(0,PGDEV(:ICH_LEN(PGDEV))//'/APPEND',1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',STATUS)
         GO TO 500
      END IF
      STATUS=0
C
C     Define window and viewport
C
      CALL PGWINDOW(XST,XEN,LOW,HIGH)
      CALL PGVSTAND
C
C     Get first cursor position and indicate it on display
C
      X1=.5*(XST+XEN)
      Y=.5*(LOW+HIGH)
      CALL PGCURSE(X1,Y,CH)
      CALL PGPOINT(1,X1,Y,ICHAR('X'))
C
C     Get second cursor position and indicate it.
C
      X2=X1
      CALL PGCURSE(X2,Y,CH)
      CALL PGPOINT(1,X2,Y,ICHAR('X'))
C
C     Sort the two points into the correct order
C
      IF (XST.LT.XEN) THEN
         XST=MIN(X1,X2)
         XEN=MAX(X1,X2)
      ELSE
         XST=MAX(X1,X2)
         XEN=MIN(X1,X2)
      END IF
C
C     Close down PGPLOT
C
      CALL PGEND
C
C     Set the XSTART and XEND user variables.
C
      CALL VAR_SETNUM('XSTART',0,0,XST,STATUS)
      IF (STATUS.EQ.0) THEN
         CALL VAR_SETNUM('XEND',0,0,XEN,STATUS)
      END IF
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to set user variables',IGNORE)
         GO TO 500
      END IF
C
C     Tidy up
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
