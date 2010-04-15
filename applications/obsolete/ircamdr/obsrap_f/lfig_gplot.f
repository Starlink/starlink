
      SUBROUTINE LFIG_GPLOT( DATARR, NX, NY, IXST, IXEN, IYST, IYEN,
     :                       DEVICE, LABEL, XLABEL, YLABEL, ERASE,
     : 	                     AXES, ADJUST, HIGH, LOW, STATUS)
C
C     F I G _ C P L O T
C
C     Produces a grey scale plot of a 2D array.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) DATARR   (Real array DATARR(NX,NY)) The data to be contoured.
C     (>) NX       (Integer) The number of x-pixels in DATA.
C     (>) NY       (Integer) The number of y-pixels in DATA.
C     (>) IXST     (Integer) The first x-pixel to be used.
C     (>) IXEN     (Integer) The last x-pixel to be used.
C     (>) IYST     (Integer) The first y-pixel to be used.
C     (>) IYEN     (Integer) The last y-pixel to be used.
C     (>) DEVICE   (Character) The device/type to be used for the
C                  plot, in the form required by PGBEGIN.
C     (>) LABEL    (Character) A label for the plot.
C     (>) XLABEL   (Character) A label for X AXIS.
C     (>) YLABEL   (Character) A label for Y AXIS.
C     (>) ERASE    (Logical) Erase the screen before plotting, if true.
C     (>) AXES     (Logical) Plot and label axes, if true.
C     (>) ADJUST   (Logical) Adust scales so as to fil display.
C     (>) HIGH     (Real) Max data value to be used (the white level)
C     (>) LOW      (Real) Min data value to be used (the black level)
C     (<) STATUS   (Integer) Return status.  0 => OK, non-zero
C                  indicates an error code from PGBEGIN.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     PGBEGIN      (PGPLOT) Initialise PGPLOT routines
C     PGGRAY       (  "   ) Plot grey scale map of data
C     PGEND        (  "   ) Close PGPLOT routines
C     PGENV        (  "   ) Set plotting environment
C
C                                      KS / AAO 27th Nov 1985
C+
      IMPLICIT NONE

C     Parameters
      LOGICAL ADJUST, AXES, ERASE
      INTEGER NX, NY, IXST, IXEN, IYST, IYEN, STATUS
      REAL DATARR( NX, NY), HIGH, LOW
      CHARACTER*( *) DEVICE, LABEL, XLABEL, YLABEL
C
C     Functions
      INTEGER PGBEGIN
C
C     Local variables
      INTEGER IADJ
      REAL TR( 6)
      DATA TR/0.,1.,0.,0.,0.,1./
C
C     Perform the plot
      STATUS = PGBEGIN( 0, DEVICE, 1, 1)
      IF (STATUS.EQ.1) THEN
         STATUS=0
         IF (ADJUST) THEN
            IADJ=0
         ELSE
            IADJ=1
         END IF
         CALL PGENV( FLOAT( IXST), FLOAT( IXEN), FLOAT( IYST),
     :               FLOAT( IYEN), IADJ, 0)
	 CALL PGSLW( 3)
         CALL PGGRAY( DATARR, NX, NY, IXST, IXEN, IYST, IYEN, LOW,
     :                HIGH, TR)
	 CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0)
         CALL PGLABEL( XLABEL, YLABEL, LABEL)
	 CALL PGIDEN
         CALL PGEND
      END IF

      END
