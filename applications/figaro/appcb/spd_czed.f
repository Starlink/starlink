      SUBROUTINE SPD_CZED( REASON, GRAPHI,
     :   VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :   MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :   COMP, FU, STATUS )
*+
*  Name:
*     SPD_CZED

*  Purpose:
*     Parameter-free FITGAUSS call back.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZED( REASON, GRAPHI,
*        VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
*        MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
*        COMP, FU, STATUS )

*  Description:
*     This is the call back for the FITGAUSS application common to the
*     Motif- and ADAM-based interfaces. All parameter handling takes
*     place outwith this routine, all data access and work takes place
*     under the control of this routine. Since NDF_BEGIN/END must be
*     outside NDF_ASSOC, they too are handled outwith this routine.

*  Arguments:
*     REASON = INTEGER (Given)
*        The call back reason:
*         0 Clean up,
*         1 start up,
*         2 access input cube and get work spaces,
*         3 choose a spectrum from the cube,
*         4 locate the chosen spectrum in the cube,
*         5 set the mask intervals,
*         6 apply the mask to the spectrum,
*         7 set the guess parameters
*         8 get the guess parameters from the existing fit,
*         9 get the guess parameters from existing fits to the
*           spectrum's neighbourhood,
*        10 perform the fit,
*        11 save the fit results in input file and to a log file,
*        12 perform a sensible complete sequence of the other call
*           backs.
*     GRAPHI = INTEGER (Given)
*        Non-zero if and only if graphical interaction is to be used.
*        This does not mean that the graphics device must have been
*        opened, but that it is available.
*     VARUSE = INTEGER (Given)
*        Non-zero if an existing variance array is to be used.
*     NDFCUB = INTEGER (Given and Returned)
*        The cube NDF identifier.
*     ROW( 2 : NDF__MXDIM ) = INTEGER (Given and Returned)
*        The row specification. ROW(2), ROW(3) ... ROW(NDF__MXDIM) are
*        taken to be the position of the row within the NDF bounds of
*        the second, third, ... NDF__MXDIM-th axis.
*     MSKDIM = INTEGER (Given)
*        Size of MASK array. Must be even.
*     MSKUSE = INTEGER (Given and Returned)
*        Number of valid mask intervals.
*     MASK( MSKDIM ) = REAL (Given and Returned)
*        The mask is put together from up to MSKDIM/2 intervals:
*           complex mask = [MASK(1);MASK(1+MSKDIM/2)]
*                        U [MASK(2);MASK(2+MSKDIM/2)]
*                        U ...
*                        U [MASK(MSKUSE);MASK(MSKUSE+MSKDIM/2)].
*        The elements of MASK are not checked for monotony. Thus
*        intervals may be empty or overlapping.
*     MXCOMP = INTEGER (Given)
*        The maximum number of line components.
*     NCOMP = INTEGER (Given and Returned)
*        The number of line components to be fitted.
*     CONT = REAL (Given and Returned)
*        The level of the continuum underlying the line components.
*     CENTRE( MXCOMP ) = REAL (Given and Returned)
*        Centre position for each component.
*     PEAK( MXCOMP ) = REAL (Given and Returned)
*        Peak height for each component.
*     FWHM( MXCOMP ) = REAL (Given and Returned)
*        Full width at half maximum for each component.
*     CF( MXCOMP ) = INTEGER (Given)
*        Fit flags for centres.
*     PF( MXCOMP ) = INTEGER (Given)
*        Fit flags for peaks.
*     WF( MXCOMP ) = INTEGER (Given)
*        Fit flags for widths.
*     COMP( MXCOMP ) = INTEGER (Given)
*        Component numbers to be used to store fit results.
*     FU = INTEGER (Given)
*        The Fortran unit number on which the log file was opened. Give
*        -1 if no file is open.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 May 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Arguments Given:
      INTEGER REASON
      INTEGER GRAPHI
      INTEGER VARUSE
      INTEGER MSKDIM
      INTEGER MXCOMP
      INTEGER CF(   MXCOMP )
      INTEGER PF(   MXCOMP )
      INTEGER WF(   MXCOMP )
      INTEGER COMP( MXCOMP )
      INTEGER FU

*  Arguments Given and Returned:
      INTEGER NDFCUB
      INTEGER ROW( 2 : NDF__MXDIM )
      INTEGER MSKUSE
      REAL MASK( MSKDIM )
      INTEGER NCOMP
      REAL CONT
      REAL CENTRE( MXCOMP )
      REAL PEAK(   MXCOMP )
      REAL FWHM(   MXCOMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER BUFSIZ             ! Position storage buffer size
      PARAMETER ( BUFSIZ = 12 )
      INTEGER LMXCMP             ! Should be .GE. MXCOMP
      PARAMETER ( LMXCMP = 6 )

*  Local Static Variables:
      LOGICAL IMVIS              ! True if finder image displayed
      LOGICAL SPVIS              ! True if spectrum displayed
      INTEGER VARXST             ! True if variance to be used and avail
      INTEGER SPVXST             ! True if cube x data are N-D
      INTEGER COVRSX             ! True if covariance row sums available
      INTEGER PTRCX              ! Pointer to cube x
      INTEGER PTRCD              ! Pointer to cube data
      INTEGER PTRCV              ! Pointer to cube variance
      INTEGER PTRCC              ! Pointer to cube covariance row sums
      INTEGER PTRSX              ! Pointer to spectrum x
      INTEGER PTRSD              ! Pointer to spectrum data
      INTEGER PTRSV              ! Pointer to spectrum variance
      INTEGER PTRSC              ! Pointer to spectr covariance row sums
      INTEGER PTRMSK             ! Pointer to masked arrays
      INTEGER PTRW1              ! Pointer to data - model
      INTEGER PTRW2              ! Pointer to data - model - error
      INTEGER PTRW3              ! Pointer to data - model + error
      INTEGER PTRW4              ! Pointer to work space
      INTEGER CUBDIM             ! Actual NDF dimensionality
      INTEGER FIRST              ! CUBE(FIRST) is SPECTRUM(1)
      INTEGER SNELM              ! Size of spectrum
      INTEGER MSKELM             ! Size of masked spectrum
      REAL XKEY, YKEY            ! PGCURS position

      SAVE IMVIS, SPVIS, VARXST, SPVXST, COVRSX,
     :   PTRCX, PTRCD, PTRCV, PTRCC, PTRSX, PTRSD, PTRSV, PTRSC,
     :   PTRMSK, PTRW1, PTRW2, PTRW3, PTRW4,
     :   CUBDIM, FIRST, SNELM, MSKELM,
     :   XKEY, YKEY

*  Local Volatile Variables:
      INTEGER I, J               ! Temporary integers
      INTEGER NPOS               ! So many positions read
      INTEGER FITPAR, FITDIM     ! So many free parameters in fit
      REAL MODPAR( 1 + 3 * LMXCMP ) ! Model parameters for SPLOOP
      REAL XPOS( BUFSIZ )        ! X position buffer
      REAL YPOS( BUFSIZ )        ! Y position buffer
      REAL CHISQR                ! Sort-of chi-squared
      DOUBLE PRECISION COVAR( (3*LMXCMP)**2 ) ! Covariance matrix
      CHARACTER * ( 1 ) CKEY     ! PGCURS key

*  Internal References:
      EXTERNAL SPD_UAAER         ! N-Gauss-plus-constant model function

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Clean up.
*  =========

      IF ( REASON .EQ. 0 ) THEN
         CONTINUE


*  Start up.
*  =========

      ELSE IF ( REASON .EQ. 1 ) THEN

*     Reset display flags.
         IMVIS = .FALSE.
         SPVIS = .FALSE.


*  Access.
*  =======

      ELSE IF ( REASON .EQ. 2 ) THEN

*     Access cube arrays. Get workspaces.
*     If graphics, work out and register the finder image.
         VARXST = VARUSE
         CALL SPD_CAAC( NDFCUB, GRAPHI, VARXST, SPVXST, COVRSX,
     :      PTRCX, PTRCD, PTRCV, PTRCC, PTRMSK,
     :      PTRW1, PTRW2, PTRW3, PTRW4, CUBDIM, STATUS )

*     If graphics, display finder image, set image display
*     flag, reset spectrum display flag.
         IF ( ( CUBDIM .EQ. 2 .OR. CUBDIM .EQ. 3 ) .AND.
     :          GRAPHI .NE. 0                            ) THEN
            CALL SPD_PDAB( STATUS )
            IMVIS = .TRUE.
            SPVIS = .FALSE.
         END IF


*  Spectrum.
*  =========

      ELSE IF ( REASON .EQ. 3 .AND. GRAPHI .NE. 0 .AND.
     :          ( CUBDIM .EQ. 2 .OR. CUBDIM .EQ. 3 )    ) THEN

*     If not displayed, display finder image, update display flags.
         IF ( .NOT. IMVIS ) THEN
            CALL SPD_PDAB( STATUS )
            IMVIS = .TRUE.
            SPVIS = .FALSE.
         END IF

*     Enquire new ROW parameter via finder image.
         IF ( CUBDIM .EQ. 2 ) THEN
            YKEY = FLOAT(ROW(2))
            CALL SPD_PDAC( XKEY, YKEY, CKEY, STATUS )
            ROW(2) = NINT(YKEY)
         ELSE IF ( CUBDIM .EQ. 3 ) THEN
            XKEY = FLOAT(ROW(2))
            YKEY = FLOAT(ROW(3))
            CALL SPD_PDAC( XKEY, YKEY, CKEY, STATUS )
            ROW(2) = NINT(XKEY)
            ROW(3) = NINT(YKEY)
         END IF


*  Read.
*  =====

      ELSE IF ( REASON .EQ. 4 ) THEN

*     Locate spectrum within the cube.
         CALL SPD_CAAA( NDFCUB, ROW, SPVXST, VARXST, COVRSX,
     :      PTRCX, PTRCD, PTRCV, PTRCC, PTRSX, PTRSD, PTRSV, PTRSC,
     :      SNELM, FIRST, STATUS )

*     If graphics.
         IF ( GRAPHI .NE. 0 ) THEN

*        Register spectrum and model.
            CALL SPD_PBAA( (VARXST.NE.0), SNELM, PTRSX, PTRSD, PTRSV,
     :         PTRW1, PTRW2, PTRW3, STATUS )

*        Sort model parameters for SPLOOP.
            MODPAR(1) = CONT
            DO 1001 I = 1, NCOMP
               MODPAR( 2 + 3*(I-1) ) = CENTRE(I)
               MODPAR( 3 + 3*(I-1) ) = PEAK(I)
               MODPAR( 4 + 3*(I-1) ) = FWHM(I)
               IF ( FWHM(I) .LE. 0. ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZED_E01', 'FITGAUSS: Error: ' //
     :               'A line width is given as zero.', STATUS )
                  GO TO 500
               END IF
 1001       CONTINUE

*        Display spectrum and model, update display flags.
            CALL SPD_PBAB( .TRUE., .FALSE., SPD_UAAER, SPD_UAAER,
     :         1+3*NCOMP, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )
            IMVIS = .FALSE.
            SPVIS = .TRUE.

         END IF


*  Mask.
*  =====

      ELSE IF ( REASON .EQ. 5 .AND. GRAPHI .NE. 0 ) THEN

*     Sort model parameters for SPLOOP.
         MODPAR(1) = CONT
         DO 1002 I = 1, NCOMP
            MODPAR( 2 + 3*(I-1) ) = CENTRE(I)
            MODPAR( 3 + 3*(I-1) ) = PEAK(I)
            MODPAR( 4 + 3*(I-1) ) = FWHM(I)
            IF ( FWHM(I) .LE. 0. ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CZED_E01', 'FITGAUSS: Error: ' //
     :            'A line width is given as zero.', STATUS )
               GO TO 500
            END IF
 1002    CONTINUE

*     Display spectrum and model, update display flags.
         CALL SPD_PBAB( .TRUE., .FALSE., SPD_UAAER, SPD_UAAER,
     :      1+3*NCOMP, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )
         IMVIS = .FALSE.
         SPVIS = .TRUE.

*     Get up to 12 x values.
*     Quitting forgets all positions entered.
*     The Return key signals last position to get.
         NPOS = MIN( BUFSIZ, MSKDIM )
         DO 1003 I = 1, MIN( BUFSIZ, MSKDIM )
            CALL SPD_PBAC( XKEY, YKEY, CKEY,
     :         .TRUE., .FALSE., SPD_UAAER, SPD_UAAER, 1+3*NCOMP, MODPAR,
     :         MSKDIM, MSKUSE, MASK, STATUS )
            CALL CHR_UCASE( CKEY )
            XPOS(I) = XKEY
            IF ( CKEY .EQ. 'Q' ) THEN
               NPOS = 0
               GO TO 1004
            ELSE IF ( ICHAR(CKEY) .EQ. 13 ) THEN
               NPOS = I
               GO TO 1004
            END IF
 1003    CONTINUE       ! End of DO loop
 1004    CONTINUE       ! Bail-out point for DO loop
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Unless we quit.
         IF ( NPOS .GT. 0 ) THEN

*        Update MASK etc.
            MSKUSE = NPOS / 2
            J = 1
            DO 1005 I = 1, MSKUSE
               MASK(I)          = XPOS(J)
               MASK(I+MSKDIM/2) = XPOS(J+1)
               J = J + 2
 1005       CONTINUE

*        Display spectrum and model.
            CALL SPD_PBAB( .TRUE., .FALSE., SPD_UAAER, SPD_UAAER,
     :         1+3*NCOMP, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )

         END IF

      ELSE IF ( REASON .EQ. 6 ) THEN  ! Apply

*     This is a non-action.


*  Guess via graph.
*  ================

      ELSE IF ( REASON .EQ. 7 .AND. GRAPHI .NE. 0 ) THEN

*     Sort model parameters for SPLOOP.
         MODPAR(1) = CONT
         DO 1006 I = 1, NCOMP
            MODPAR( 2 + 3*(I-1) ) = CENTRE(I)
            MODPAR( 3 + 3*(I-1) ) = PEAK(I)
            MODPAR( 4 + 3*(I-1) ) = FWHM(I)
            IF ( FWHM(I) .LE. 0. ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CZED_E01', 'FITGAUSS: Error: ' //
     :            'A line width is given as zero.', STATUS )
               GO TO 500
            END IF
 1006    CONTINUE

*     Display spectrum and model, update display flags.
         CALL SPD_PBAB( .TRUE., .FALSE., SPD_UAAER, SPD_UAAER,
     :      1+3*NCOMP, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )
         IMVIS = .FALSE.
         SPVIS = .TRUE.

*     Get up to 12 (x,y) pairs.
*     Quitting forgets all positions entered.
*     The Return key signals last position to get.
         NPOS = MIN( BUFSIZ, 2*MXCOMP )
         DO 1007 I = 1, MIN( BUFSIZ, 2*MXCOMP )
            CALL SPD_PBAC( XKEY, YKEY, CKEY,
     :         .TRUE., .FALSE., SPD_UAAER, SPD_UAAER, 1+3*NCOMP, MODPAR,
     :         MSKDIM, MSKUSE, MASK, STATUS )
            CALL CHR_UCASE( CKEY )
            XPOS(I) = XKEY
            YPOS(I) = YKEY
            IF ( CKEY .EQ. 'Q' ) THEN
               NPOS = 0
               GO TO 1008
            ELSE IF ( ICHAR(CKEY) .EQ. 13 ) THEN
               NPOS = I
               GO TO 1008
            END IF
 1007    CONTINUE       ! End of DO loop
 1008    CONTINUE       ! Bail-out point for DO loop
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Unless we quit.
         IF ( NPOS .GT. 0 ) THEN

*        Update guess.
            NCOMP = NPOS / 2
            IF ( NCOMP .LE. 0 ) THEN
               CONT = YPOS(1)
            ELSE
               CONT = 0.
               J = 1
               DO 1009 I = 1, NCOMP
                  CONT = CONT + ( 2.*YPOS(J+1)-YPOS(J) ) / FLOAT(NCOMP)
                  CENTRE(I) = XPOS(J)
                  PEAK(I) = 2. * ( YPOS(J) - YPOS(J+1) )
                  FWHM(I) = 2. * ABS( XPOS(J) - XPOS(J+1) )
                  CF(I) = 0
                  PF(I) = 0
                  WF(I) = 0
                  J = J + 2
 1009          CONTINUE
            END IF

*        Sort model parameters for SPLOOP.
            MODPAR(1) = CONT
            DO 1010 I = 1, NCOMP
               MODPAR( 2 + 3*(I-1) ) = CENTRE(I)
               MODPAR( 3 + 3*(I-1) ) = PEAK(I)
               MODPAR( 4 + 3*(I-1) ) = FWHM(I)
               IF ( FWHM(I) .LE. 0. ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'SPD_CZED_E01', 'FITGAUSS: Error: ' //
     :               'A line width is given as zero.', STATUS )
                  GO TO 500
               END IF
 1010       CONTINUE

*        Display spectrum and model.
            CALL SPD_PBAB( .TRUE., .FALSE., SPD_UAAER, SPD_UAAER,
     :         1+3*NCOMP, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )

         END IF

      ELSE IF ( REASON .EQ. 8 ) THEN  ! Guess using old fit

*     Get old fit.
*     If graphics.
*        Work out model related arrays.
*        Re-display spectrum and model, update display flags.

      ELSE IF ( REASON .EQ. 9 ) THEN  ! Guess using fit to neighbours

*     Get average fit of eight neighbours.
*     If graphics.
*        Work out model related arrays.
*        Re-display spectrum and model, update display flags.


*  Fit.
*  ====

      ELSE IF ( REASON .EQ. 10 ) THEN

*     Check number of components manageable.
         IF ( NCOMP .GT. LMXCMP ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZED_E02', 'FITGAUSS: Error: ' //
     :         'Cannot fit that many components at once.', STATUS )
            GO TO 500
         END IF

*     Apply mask.
         CALL SPD_WAAAR( VARXST, COVRSX, SNELM, MSKDIM,
     :      %VAL(PTRSX), %VAL(PTRSD), %VAL(PTRSV), %VAL(PTRSC),
     :      MSKUSE, MASK, MSKELM, %VAL(PTRMSK), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Complete the information about the guess.
         FITPAR = 0
         DO 1011 I = 1, NCOMP
            IF ( CF(I) .EQ. 0 ) FITPAR = FITPAR + 1
            IF ( PF(I) .EQ. 0 ) FITPAR = FITPAR + 1
            IF ( WF(I) .EQ. 0 ) FITPAR = FITPAR + 1
 1011    CONTINUE
         FITDIM = MAX( FITPAR, 1 )

*     Fit masked data.
         CALL SPD_WZEA( COVRSX, MSKELM, NCOMP, FITPAR, FITDIM,
     :      CONT, %VAL(PTRMSK), CF, PF, WF, CENTRE, PEAK, FWHM,
     :      CHISQR, COVAR, J, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     If fit successful.
         IF ( J .GE. 0 ) THEN

*        Report to screen.
            CALL SPD_WZEB( -1, 'Some row in some NDF',
     :         SNELM, (VARXST.NE.0),
     :         MSKDIM, MSKUSE, MASK, MSKELM,
     :         NCOMP, FITPAR, FITDIM,
     :         CONT, CF, PF, WF, CENTRE, PEAK, FWHM,
     :         CHISQR, COVAR, STATUS )

*        If graphics, update display.
            IF ( GRAPHI .NE. 0 ) THEN
               MODPAR(1) = CONT
               DO 1012 I = 1, NCOMP
                  MODPAR( 2 + 3*(I-1) ) = CENTRE(I)
                  MODPAR( 3 + 3*(I-1) ) = PEAK(I)
                  MODPAR( 4 + 3*(I-1) ) = FWHM(I)
                  IF ( FWHM(I) .LE. 0. ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'SPD_CZED_E01', 'FITGAUSS: ' //
     :                  'Error: A line width is given as zero.',
     :                  STATUS )
                     GO TO 500
                  END IF
 1012          CONTINUE
               CALL SPD_PBAB( .TRUE., .FALSE., SPD_UAAER, SPD_UAAER,
     :            1+3*NCOMP, MODPAR, MSKDIM, MSKUSE, MASK, STATUS )
            END IF

         END IF

      ELSE IF ( REASON .EQ. 11 ) THEN ! Save

*     Store results.
*     Report to file.

      ELSE IF ( REASON .EQ. 12 ) THEN ! Do all
      END IF


*  Return.
*  =======

 500  CONTINUE

      END
