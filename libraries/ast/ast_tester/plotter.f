      PROGRAM PLOTTER

*  Usage:
*     PLOTTER <fits file> <attr1> <attr2> <ps file> [<xlo> <ylo> <xhi> <yhi>]

*  Description:
*     Plots a standard grid from the specified fits header file, using
*     the specified attributes, and sends postscript output to the
*     specified ps file.

*  Parameters:
*     file file
*        A text file containing fits headers.
*     attr1
*        A string containg a comma-separated list of attribute
*        settings for the Plot.
*     attr2
*        A string containg a comma-separated list of attribute
*        settings for the FitsChan.
*     ps file
*        The output postscript file
*     xlo ylo xhi yhi
*        The bounds within the GRID Frame of the required plot.
*        Taken from the FITS headers if not supplied


      IMPLICIT NONE
      INCLUDE 'AST_PAR'

      INTEGER STATUS, FC, FS, NAXIS1, NAXIS2, PL, PGBEG, IARGC, OC
      CHARACTER FILE*80, CARD*80, DEVN*80, PSFILE*80, ATTR*255, TEXT*80
      REAL GBOX(4), RANGE, DELTA, ASP
      DOUBLE PRECISION PBOX(4)

      STATUS = 0
*
* Check command line arguments have been supplied.
*
      IF( IARGC() .LT. 3 ) THEN
         WRITE(*,*) 'Usage: plotter <file file> <attrs> <fattrs> '//
     :              '<ps file> [<xlo> <ylo> <xhi> <yhi>]'
         RETURN
      END IF

*
*  Use object caching to minimise allocation of new memory
*
      OC = AST_TUNE( 'ObjectCaching', 1, STATUS )
      IF( OC .NE. 0 ) THEN
         WRITE(*,'(A,I6)') 'Default ObjectCaching VALUE is ',OC
      END IF

      IF( AST_TUNE( 'ObjectCaching', AST__TUNULL, STATUS ) .NE. 1 ) THEN
         WRITE(*,'(A,I6)') 'Set ObjectCaching VALUE is ',OC
      END IF

*
* Create a FitsChan to store the FITS headers.
*
      CALL GETARG( 3, ATTR )
      FC = AST_FITSCHAN( AST_NULL, AST_NULL, ATTR, STATUS )

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
*  If the base frame box was supplied on the command line, use it.
*
      IF( IARGC() .GT. 6 ) THEN
         CALL GETARG( 5, TEXT )
         READ( TEXT, * ) PBOX( 1 )

         CALL GETARG( 6, TEXT )
         READ( TEXT, * ) PBOX( 2 )

         CALL GETARG( 7, TEXT )
         READ( TEXT, * ) PBOX( 3 )

         CALL GETARG( 8, TEXT )
         READ( TEXT, * ) PBOX( 4 )

*  Otherwise use NAXISi keywords in the header.
      ELSE

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

         PBOX(1) = 0.5
         PBOX(2) = 0.5
         PBOX(3) = DBLE( NAXIS1 )+0.5
         PBOX(4) = DBLE( NAXIS2 )+0.5
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
         CALL GETARG( 4, PSFILE )
         CALL DELETEFILE( PSFILE )

         DEVN = 'pscol_l;'//PSFILE
c         IF( PGBEG( 0, '?', 1, 1 ) .EQ. 1 ) THEN
	 IF( PGBEG( 0, DEVN, 1, 1 ) .EQ. 1 ) THEN
            CALL PGPAGE
            CALL PGWNAD( 0.0, 1.0, 0.0, 1.0 )

*
* Create the Plot. The pixel coordinates box is
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

            ASP = REAL( PBOX(4) - PBOX(2) )/REAL( PBOX(3) - PBOX(1) )
            IF( ASP .LT. 0.05 .OR. ASP .GT. 20 ) ASP = 1.0

            IF( ASP .GT. 1.0 ) THEN
               DELTA = 0.5*( ( GBOX(3) - GBOX(1) ) -
     :                       ( GBOX(4) - GBOX(2) )/ASP )
               GBOX(3) = GBOX(3) - DELTA
               GBOX(1) = GBOX(1) + DELTA
            ELSE
               DELTA = 0.5*( ( GBOX(4) - GBOX(2) ) -
     :                       ASP*( GBOX(3) - GBOX(1) ) )
               GBOX(4) = GBOX(4) - DELTA
               GBOX(2) = GBOX(2) + DELTA
            END IF

            CALL GETARG( 2, ATTR )
            PL = AST_PLOT( FS, GBOX, PBOX, 'title = A FITS test',
     :                     STATUS )
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
