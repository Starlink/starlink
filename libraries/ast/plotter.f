      PROGRAM PLOTTER

*  Usage:
*     PLOTTER <fits file> <attr> <ps file>

*  Description:
*     Plots a standard grid from the specified fits header file, using
*     the specified attributes, and sends postscript output to the
*     specified ps file.

*  Parameters:
*     file file
*        A text file containing fits headers.
*     attr
*        A string containg a comma-separated list of attribute
*        settings for the Plot.
*     ps file
*        The output postscript file
*


      IMPLICIT NONE
      INCLUDE 'AST_PAR'

      INTEGER STATUS, FC, FS, NAXIS1, NAXIS2, PL, PGBEG, IARGC
      CHARACTER FILE*80, CARD*80, DEVN*80, PSFILE*80, ATTR*80
      REAL GBOX(4), RANGE, DELTA, ASP
      DOUBLE PRECISION PBOX(4)

*
* Check command line arguments have been supplied.
*
      IF( IARGC() .LT. 3 ) THEN
         WRITE(*,*) 'Usage: plotter <file file> <attrs> <ps file>'
         RETURN
      END IF

* 
* Create a FitsChan to store the FITS headers.
*
      STATUS = 0
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )

*
* Open a text file containing a list of FITS headers.
*
      CALL GETARG( 1, FILE )
      OPEN( UNIT=10, FILE=FILE, STATUS='OLD' )

*
* Read each card out of the text file and store it in the FitsChan.
*
      DO WHILE( .TRUE. )
         READ( 10, '(A)', END = 10 ) CARD
         CALL AST_PUTFITS( FC, CARD, 0, STATUS )
      END DO

 10   CLOSE( 10 )

*
* See if values were supplied for NAXIS1 and NAXIS2. If not assume a value
* of 100 for each. The FitsChan is re-wound before calling AST_FINDFITS so
* that the search starts form the beginning.
*
      CALL AST_CLEAR( FC, 'CARD', STATUS )
      IF ( AST_FINDFITS( FC, 'NAXIS1', CARD, .TRUE., STATUS ) ) THEN
         READ(CARD(11:),*) NAXIS1
      ELSE
         NAXIS1 = 100
      END IF

      CALL AST_CLEAR( FC, 'CARD', STATUS )
      IF ( AST_FINDFITS( FC, 'NAXIS2', CARD, .TRUE., STATUS ) ) THEN
         READ(CARD(11:),*) NAXIS2
      ELSE
         NAXIS2 = 100
      END IF

* 
* Read an Object from the contents of the FitsChan. This should be a
* FrameSet (this should be tested really, and an error reported if any
* other type of Object is obtained). Re-wind the FitsChan first so that 
* its entire contents are read. Note, this is a destructive read, in that 
* the cards which are significant to the creation of the FrameSet are 
* removed from the FitsChan (this is why NAXIS1 and NAXIS2 are read out 
* earlier - just in case they are significant to the FrameSet). Any 
* insignificant cards are left as they are. 
*
      CALL AST_CLEAR( FC, 'CARD', STATUS )
      FS = AST_READ( FC, STATUS )

      IF( FS .EQ. AST__NULL ) THEN
         WRITE(*,*) '!!! No object read from FitsChan!!!'
         GO TO 999
      END IF

*
* If all is OK, start up PGPLOT.
*
      IF( STATUS .EQ. 0 ) THEN
         CALL GETARG( 3, PSFILE )
         CALL DELETEFILE( PSFILE )

         DEVN = 'pscol_l;'//PSFILE
	 IF( PGBEG( 0, DEVN, 1, 1 ) .EQ. 1 ) THEN
            CALL PGPAGE
            CALL PGWNAD( 0.0, 1.0, 0.0, 1.0 )

*
* Create the Plot. The pixel coordinates box (0,0) to (NAXIS1,NAXIS2) is 
* mapped onto a window which is 10% smaller than the full PGPLOT window. 
* This gives some space for things like tick marks with negative length 
* (which stick outside the pixel cooridnates box).
*
            CALL PGQWIN( GBOX(1), GBOX(3), GBOX(2), GBOX(4) )

            RANGE = GBOX(3) - GBOX(1)
            GBOX(1) = GBOX(1) + 0.05*RANGE
            GBOX(3) = GBOX(3) - 0.05*RANGE

            RANGE = GBOX(4) - GBOX(2)
            GBOX(2) = GBOX(2) + 0.05*RANGE
            GBOX(4) = GBOX(4) - 0.05*RANGE

            PBOX(3) = 0.0
            PBOX(2) = 0.0
            PBOX(1) = DBLE( NAXIS1 )
            PBOX(4) = DBLE( NAXIS2 )


            ASP = REAL( NAXIS2 )/REAL( NAXIS1 )
            IF( ASP .GT. 1.0 ) THEN
               DELTA = ( ASP - 1.0 )*( GBOX(3) - GBOX(1) )/( 2.0*ASP )
               GBOX(3) = GBOX(3) - DELTA
               GBOX(1) = GBOX(1) + DELTA
            ELSE
               DELTA = ( 1.0 - ASP )*( GBOX(4) - GBOX(2) )/2.0
               GBOX(4) = GBOX(4) - DELTA
               GBOX(2) = GBOX(2) + DELTA
            END IF

            CALL GETARG( 2, ATTR )
            PL = AST_PLOT( FS, GBOX, PBOX, 'title = A FITS test,'//
     :                     'tol=0.01', STATUS )
            CALL AST_SET( PL, ATTR, STATUS )

*
* Draw the grid.
*
            CALL AST_GRID( PL, STATUS )
                
*
* Annul the Plot, and close PGPLOT.
*
            CALL AST_ANNUL( PL, STATUS )
            CALL PGEND
         END IF
      END IF

*
* Annul the other objects.
*
      CALL AST_ANNUL( FS, STATUS )
 999  CALL AST_ANNUL( FC, STATUS )

      END


*
*  Delete a file if it exists.
*
      SUBROUTINE DELETEFILE( FILNAM )
      IMPLICIT NONE

      CHARACTER FILNAM*(*)
      LOGICAL EXISTS

      INQUIRE ( FILE   = FILNAM,
     :          EXIST  = EXISTS )

      IF( EXISTS ) THEN
         OPEN ( UNIT=10, FILE=FILNAM, STATUS='OLD' )
         CLOSE ( 10, STATUS='DELETE' )
      END IF

      END
