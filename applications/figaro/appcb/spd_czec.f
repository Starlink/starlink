      SUBROUTINE SPD_CZEC( REASON, GRAPHI, STATUS )
*+
*  Name:
*     SPD_CZEC

*  Purpose:
*     ADAM-based FITGAUSS call back.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZEC( REASON, GRAPHI, STATUS )

*  Description:
*     This is the call back for the FITGAUSS application for the
*     ADAM-based interface.
*
*     In the case of the Motif-based interface we are free to use
*     the Motif widgets to store, display, and make editable, the
*     current values of a parameter. But with the ADAM-based interface
*     this is not so easy, due to the complex behaviour of the ADAM
*     parameter system, which is more or less impossible to control from
*     the programme. Thus the principal parameter repository are SAVE
*     variables in this routine and the ADAM parameter system is used
*     only when prompting should occur.

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
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     CLNUP = INTEGER
*        This flag is used to register the need for clean-up action
*        whence the application is de-selected. This flag is initially
*        zero, but is non-zero while clean up is necessary and pending.
*     FRSTT = INTEGER
*        This flag is used to register the need for additional action
*        necessary when the application performs its first real action.
*        Here for example, the flag is used to decide whether accessing
*        a new cube implies releasing any currently accessed cube: if
*        access occurs for the first time there is nothing to release.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 May 1994 (hme):
*        Original version.
*     19 May 1994 (hme):
*        Put the PAR_CANCL calls behind the PAR_GET* calls. Thus a value
*        given on the command line is used first time round. And still
*        the user will be prompted next time.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PAR_ERR'          ! Standard PAR error codes

*  Arguments Given:
      INTEGER GRAPHI
      INTEGER REASON

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MSKDIM             ! Twice max. number of mask intervals
      PARAMETER ( MSKDIM = 2 * 6 )
      INTEGER MXCOMP             ! Maximum number of line components
      PARAMETER ( MXCOMP = 6 )

*  Local Static Variables:
      LOGICAL ENABL( 12 )        ! True if call back reason enabled
      INTEGER CLNUP              ! See notes
      INTEGER FRSTT              ! See notes
      INTEGER VARUSE             ! True if cube variance to be used
      INTEGER NDFCUB             ! Cube NDF identifier
      INTEGER MASK( MSKDIM )     ! Mask intervals
      INTEGER MSKUSE             ! Actual number of mask intervals
      INTEGER ROW( 2 : NDF__MXDIM ) ! Row chosen from cube
      INTEGER NCOMP              ! Actual number of line components
      INTEGER CF( MXCOMP )       ! Centre fit flags
      INTEGER PF( MXCOMP )       ! Peak fit flags
      INTEGER WF( MXCOMP )       ! Width fit flags
      INTEGER COMP( MXCOMP )     ! Component numbers for storage
      INTEGER FD, FU             ! File descriptor and unit number
      REAL CONT                  ! Continuum level
      REAL CENTRE( MXCOMP )      ! Line centre positions
      REAL PEAK(   MXCOMP )      ! Line peak amplitudes
      REAL FWHM(   MXCOMP )      ! Line widths

      SAVE ENABL, CLNUP, FRSTT,
     :   VARUSE, NDFCUB, MASK, MSKUSE, ROW,
     :   NCOMP, CF, PF, WF, COMP, FD, FU, CONT, CENTRE, PEAK, FWHM

*  Local Volatile Variables:
      LOGICAL TEMPL              ! Temporary logical
      INTEGER I, J               ! Temporary integers

*  Local Data:
      DATA
     :   ENABL / .TRUE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE.,
     :      .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .TRUE. /,
     :   ROW / 1, 1, 1, 1, 1, 1 /,
     :   NCOMP / 1 /,
     :   CF / 0, 0, 0, 0, 0, 0 /,
     :   PF / 0, 0, 0, 0, 0, 0 /,
     :   WF / 0, 0, 0, 0, 0, 0 /,
     :   COMP / 1, 2, 3, 4, 5, 6 /,
     :   FU / -1 /,
     :   CONT / 0. /,
     :   CENTRE / 0., 0., 0., 0., 0., 0. /,
     :   PEAK / 1., 1., 1., 1., 1., 1. /,
     :   FWHM / 1., 1., 1., 1., 1., 1. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Change cursor (Motif only).
*  Defer ADAM error reporting (Motif only).


*  Clean up action.
*  ================

      IF ( REASON .EQ. 0 ) THEN

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )

*     If graphics, close device, saving current viewports as AGI pictures.
         IF ( GRAPHI .NE. 0 ) CALL SPD_PAAC( STATUS )

*     End NDF context.
         CALL NDF_END( STATUS )

*     Clear cleanup flag, set firsttime flag.
         CLNUP = 0
         FRSTT = 1

*     Disable spectrum, read, mask, apply, guess, fit, save.
         ENABL(3)  = .FALSE.
         ENABL(4)  = .FALSE.
         ENABL(5)  = .FALSE.
         ENABL(6)  = .FALSE.
         ENABL(7)  = .FALSE.
         ENABL(8)  = .FALSE.
         ENABL(9)  = .FALSE.
         ENABL(10) = .FALSE.
         ENABL(11) = .FALSE.


*  Start up action.
*  ================

      ELSE IF ( REASON .EQ. 1 ) THEN

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )


*  Access.
*  =======

      ELSE IF ( REASON .EQ. 2 ) THEN

*     If first time, open NDF context, else close and re-open NDF
*     context.
         IF ( FRSTT .EQ. 0 ) CALL NDF_END( STATUS )
         CALL NDF_BEGIN

*     Get VARUSE parameter, access input NDF.
         CALL PAR_GET0L( 'VARUSE', TEMPL, STATUS )
         CALL PAR_CANCL( 'VARUSE', STATUS )
         VARUSE = 0
         IF ( TEMPL ) VARUSE = 1
         CALL NDF_ASSOC( 'IN', 'UPDATE', NDFCUB, STATUS )
         CALL PAR_CANCL( 'IN', STATUS )

*     If first time and graphics interaction, access interactive
*     graphics device.
         IF ( FRSTT .NE. 0 .AND. GRAPHI .NE. 0 )
     :      CALL SPD_PAAB( 'DEVICE', STATUS )

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )

*     Set cleanup flag, clear firsttime flag.
         CLNUP = 1
         FRSTT = 0
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Enable spectrum, read.
*     Disable mask, apply, guess, fit, save.
         ENABL(3)  = .TRUE.
         ENABL(4)  = .TRUE.
         ENABL(5)  = .FALSE.
         ENABL(6)  = .FALSE.
         ENABL(7)  = .FALSE.
         ENABL(8)  = .FALSE.
         ENABL(9)  = .FALSE.
         ENABL(10) = .FALSE.
         ENABL(11) = .FALSE.


*  Spectrum.
*  =========

      ELSE IF ( REASON .EQ. 3 .AND. ENABL(3) ) THEN

         IF ( GRAPHI .NE. 0 ) THEN

*        Common call back.
            CALL SPD_CZED( REASON, GRAPHI,
     :         VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :         MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :         COMP, FU, STATUS )

         ELSE

*        Prompt for ROW parameter.
            CALL PAR_GET1I( 'ROW', NDF__MXDIM-1, ROW, I, STATUS )
            CALL PAR_CANCL( 'ROW', STATUS )
            DO 1002 J = I+2, NDF__MXDIM
               ROW(J) = 1
 1002       CONTINUE

         END IF


*  Read.
*  =====

      ELSE IF ( REASON .EQ. 4 .AND. ENABL(4) ) THEN

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Enable mask, apply, guess, fit.
         ENABL(5)  = .TRUE.
         ENABL(6)  = .TRUE.
         ENABL(7)  = .TRUE.
         ENABL(8)  = .TRUE.
         ENABL(9)  = .TRUE.
         ENABL(10) = .TRUE.


*  Mask.
*  =====

      ELSE IF ( REASON .EQ. 5 .AND. ENABL(5) ) THEN

         IF ( GRAPHI .NE. 0 ) THEN

*        Common call back.
            CALL SPD_CZED( REASON, GRAPHI,
     :         VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :         MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :         COMP, FU, STATUS )

         ELSE

*        Prompt for MASK parameter.
            J = MSKDIM / 2 + 1
            CALL PAR_GET1R( 'MASK1', MSKDIM/2, MASK(1), MSKUSE, STATUS )
            CALL PAR_GET1R( 'MASK2', MSKDIM/2, MASK(J), I,      STATUS )
            CALL PAR_CANCL( 'MASK1', STATUS )
            CALL PAR_CANCL( 'MASK2', STATUS )
            MSKUSE = MIN( MSKUSE, I )

         END IF


*  Apply.
*  ======

      ELSE IF ( REASON .EQ. 6 .AND. ENABL(6) ) THEN

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )


*  Guess via graphics.
*  ===================

      ELSE IF ( REASON .EQ. 7 .AND. ENABL(7) ) THEN

         IF ( GRAPHI .NE. 0 ) THEN

*        Common call back.
            CALL SPD_CZED( REASON, GRAPHI,
     :         VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :         MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :         COMP, FU, STATUS )

         ELSE

*        Prompt for guess parameters.
            CALL PAR_GET0R( 'CONT', CONT, STATUS )
            CALL PAR_GET1R( 'CENTRE', MXCOMP, CENTRE, NCOMP, STATUS )
            CALL PAR_GET1R( 'PEAK', MXCOMP, PEAK, I, STATUS )
            NCOMP = MIN( NCOMP, I )
            CALL PAR_GET1R( 'FWHM', MXCOMP, FWHM, I, STATUS )
            NCOMP = MIN( NCOMP, I )
            CALL PAR_GET1I( 'CF', MXCOMP, CF, I, STATUS )
            NCOMP = MIN( NCOMP, I )
            CALL PAR_GET1I( 'PF', MXCOMP, PF, I, STATUS )
            NCOMP = MIN( NCOMP, I )
            CALL PAR_GET1I( 'WF', MXCOMP, WF, I, STATUS )
            NCOMP = MIN( NCOMP, I )

         END IF
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Disable save.
         ENABL(11) = .FALSE.


*  Guess using old fit.
*  ====================

      ELSE IF ( REASON .EQ. 8 .AND. ENABL(8) ) THEN

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Disable save.
         ENABL(11) = .FALSE.


*  Guess using fit to neighbours.
*  ==============================

      ELSE IF ( REASON .EQ. 9 .AND. ENABL(9) ) THEN

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Disable save.
         ENABL(11) = .FALSE.


*  Fit.
*  ====

      ELSE IF ( REASON .EQ. 10 .AND. ENABL(10) ) THEN

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Enable save.
         ENABL(11) = .TRUE.


*  Save.
*  =====

      ELSE IF ( REASON .EQ. 11 .AND. ENABL(11) ) THEN

*     Get COMP parameter.
         CALL PAR_GET1I( 'COMP', MXCOMP, COMP, I, STATUS )
         CALL PAR_CANCL( 'COMP', STATUS )
         DO 1004 J = I+1, NCOMP
            COMP(J) = 0
 1004    CONTINUE
         DO 1001 J = 1, NCOMP
            IF ( COMP(J) .LT. 0 ) COMP(J) = 0
            IF ( J .GT. 1 ) THEN
               DO 1003 I = 1, J-1
                  IF ( COMP(J) .GT. 0 .AND. COMP(J) .EQ. COMP(I) )
     :               COMP(J) = 0
 1003          CONTINUE
            END IF
 1001    CONTINUE

*     Open log file.
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         CALL FIO_ASSOC( 'LOGFIL', 'APPEND', 'LIST', 0, FD, STATUS )
         CALL PAR_CANCL( 'LOGFIL', STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            FU = -1
         ELSE
            CALL FIO_UNIT( FD, FU, STATUS )
         END IF

*     Common call back.
         CALL SPD_CZED( REASON, GRAPHI,
     :      VARUSE, NDFCUB, ROW, MSKDIM, MSKUSE, MASK,
     :      MXCOMP, NCOMP, CONT, CENTRE, PEAK, FWHM, CF, PF, WF,
     :      COMP, FU, STATUS )

*     Close log file.
         IF ( FU .GE. 0 ) CALL FIO_CLOSE( FD, STATUS )
         FU = -1

      ELSE IF ( REASON .EQ. 12 ) THEN ! Do all
      END IF

*  Tidy up.
*  ========

 500  CONTINUE

*  Intercept or flush ADAM error reports (Motif only).

      END
