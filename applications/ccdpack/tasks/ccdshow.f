      SUBROUTINE CCDSHOW( STATUS )
*+
*  Name:
*     CCDSHOW

*  Purpose:
*     Displays the value of the CCDPACK global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDSHOW( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine shows the current value of any CCDPACK global
*     parameters.

*  Usage:
*     ccdshow

*  ADAM Parameters:
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter, then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]

*  Behaviour of parameters:
*     The parameters (LOGTO and LOGFILE ) have global values. These
*     global values will always take precedence, except when an
*     assignment is made on the command line.  Global values may be set
*     using the CCDSETUP command.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-AUG-1991 (PDRAPER):
*        Original version.
*     7-APR-1992 (PDRAPER):
*        Changed to use parameter system to get values rather than
*        directly accessing the global file.
*     20-JUL-1993 (PDRAPER):
*        Added LOG, LOGFILE and NDFNAMES parameters.
*     28-JAN-1994 (PDRAPER):
*        Added saturation extensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT constants
      INCLUDE 'PAR_ERR'          ! Parameter system constants
      INCLUDE 'MSG_PAR'          ! Message system constants
      INCLUDE 'FIO_PAR'          ! FIO parameters 

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Variables:
      CHARACTER * ( 8 ) LOGTO    ! Where information is logged to
      CHARACTER BIAS * ( FIO__SZFNM ) ! Name of mask file.
      CHARACTER CAL * ( FIO__SZFNM ) ! Name of mask file.
      CHARACTER FLAT * ( FIO__SZFNM ) ! Name of mask file.
      CHARACTER LOGNAM * ( FIO__SZFNM ) ! Name of logfile
      CHARACTER MSKNAM * ( FIO__SZFNM ) ! Name of mask file.
      DOUBLE PRECISION ADC       ! ADC factor
      DOUBLE PRECISION DEFER     ! Deferred charge value
      DOUBLE PRECISION RNOISE    ! Readout noise
      DOUBLE PRECISION SATVAL    ! Saturation value 
      INTEGER BOUNDS( 4 )        ! Bias strip bounds
      INTEGER DIRECT             ! Readout direction
      INTEGER EXTENT( 4 )        ! Useful CCD area
      INTEGER I                  ! Dummy
      INTEGER NBOUND             ! Number of bounds
      LOGICAL DUMMY              ! Dummy variable
      LOGICAL GENVAR             ! Whether variances are generated
      LOGICAL GOTADC             ! Flags showing which values have been
      LOGICAL GOTBDS             ! obtained
      LOGICAL GOTBIA             ! " "
      LOGICAL GOTCAL             ! " "
      LOGICAL GOTDEF             ! " "
      LOGICAL GOTDIR             ! " "
      LOGICAL GOTEXT             ! " "
      LOGICAL GOTFLA             ! " "
      LOGICAL GOTGEN             ! " "
      LOGICAL GOTLG2             ! " "
      LOGICAL GOTLGN             ! " "
      LOGICAL GOTMSK             ! " "
      LOGICAL GOTNAM             ! " "
      LOGICAL GOTNOI             ! " "
      LOGICAL GOTONE             ! True when have at least one value
      LOGICAL GOTPRE             ! " "
      LOGICAL GOTSAT             ! " "
      LOGICAL GOTSPR             ! " "
      LOGICAL GOTSVL             ! " "
      LOGICAL NDFS               ! INLIST prompts are NDFs
      LOGICAL PRESER             ! Whether to preserve data types.
      LOGICAL SATUR              ! Look for saturated pixels
      LOGICAL SETSAT             ! Set saturated pixels to saturation value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup CCDPACK logging system.
      CALL CCD1_START( 'CCDSHOW', STATUS )

*  Make sure that the LOGINT is greater than 0 (no output -
*  bit pointless)
      CALL CCD1_SILEV( 1, 3, I, STATUS )

*  Set GOTONE.
      GOTONE = .FALSE.

*  Access the values through GLOBAL associations one-by-one

*  Where is the logfile information coming from? The usual parameters
*  have already been used by this application so they should just
*  return the values assigned. To get round this problem we will access
*  the logfile and logto value via secondary parameters which return !
*  if no global value exists. These do not count as GOTONE.
      CALL PAR_GET0C( 'LOGTOD', LOGTO, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         GOTLG2 = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE
         GOTLG2 = .TRUE.
      END IF
      CALL PAR_GET0C( 'LOGFILED', LOGNAM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         GOTLGN = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE
         GOTLGN = .TRUE.
      END IF

*  ADC conversion factor.
      CALL PAR_GET0D( 'ADC', ADC, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         GOTADC = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE
         GOTADC = .TRUE.
         GOTONE = .TRUE.
      END IF

*  Extent of useful CCD area.
      CALL PAR_GET1I( 'EXTENT', 4, EXTENT, NBOUND, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         GOTEXT = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE
         GOTEXT = .TRUE.
         GOTONE = .TRUE.
      END IF

*  Readout noise estimate (may be none).
      CALL PAR_GET0D( 'RNOISE', RNOISE, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         GOTNOI = .FALSE.
         CALL ERR_ANNUL( STATUS )
      ELSE
         GOTNOI = .TRUE.
         GOTONE = .TRUE.
      END IF

*  Bounds of the bias strips. May default to none.
      CALL CCD1_GTBDS( .FALSE., I, -1, 1, 4, BOUNDS, NBOUND, DUMMY,
     :                 STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTBDS = .FALSE.
      ELSE
         GOTBDS = .TRUE.
         GOTONE = .TRUE.
      END IF

*  The readout direction.
      CALL CCD1_GTDIR( .FALSE., I, DIRECT, DUMMY, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTDIR = .FALSE.
      ELSE

*  Check the return for validity.
         GOTDIR = .TRUE.
         GOTONE = .TRUE.
      END IF

*  Deferred charge.
      CALL PAR_GET0D( 'DEFERRED', DEFER, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTDEF = .FALSE.
      ELSE
         GOTDEF = .TRUE.
         GOTONE = .TRUE.
      END IF

*  Mask file.
      CALL PAR_GET0C( 'MASK', MSKNAM, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTMSK = .FALSE.
      ELSE

*  Have a file.
         GOTONE = .TRUE.
         GOTMSK = .TRUE.
      END IF

*  Find out the users preferences for saturated pixel processing.
      GOTSVL = .FALSE.
      GOTSPR = .FALSE.
      CALL PAR_GET0L( 'SATURATE', SATUR, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTSAT = .FALSE.
      ELSE
         GOTSAT = .TRUE.
         GOTONE = .TRUE.
         IF ( SATUR ) THEN 

*  Need a saturation value and the method to use when applying it.
            CALL PAR_GET0D( 'SATURATION', SATVAL, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               GOTSVL = .FALSE.
            ELSE
               GOTSVL = .TRUE.
               GOTONE = .TRUE.
            END IF

*  Preference for saturatecd pixel flagging.
            CALL PAR_GET0L( 'SETSAT', SETSAT, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               GOTSPR = .FALSE.
            ELSE
               GOTSPR = .TRUE.
               GOTONE = .TRUE.
            END IF
         END IF
      END IF

*  Do we want to preserve data types through out the processing ?
      CALL PAR_GET0L( 'PRESERVE', PRESER, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTPRE = .FALSE.
      ELSE
         GOTONE = .TRUE.
         GOTPRE = .TRUE.
      END IF

*  Do we want to generate variances ?
      CALL PAR_GET0L( 'GENVAR', GENVAR, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTGEN = .FALSE.
      ELSE
         GOTONE = .TRUE.
         GOTGEN = .TRUE.
      END IF

*  Are reponses to INLIST prompts NDF names or position list names?
      CALL PAR_GET0L( 'NDFNAMES', NDFS, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTNAM = .FALSE.
      ELSE
         GOTONE = .TRUE.
         GOTNAM = .TRUE.
      END IF

*  What are the names of the GLOBAL calibration frames?
*  Master BIAS frame.
      CALL PAR_GET0C( 'BIAS', BIAS, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTBIA = .FALSE.
      ELSE
         GOTONE = .TRUE.
         GOTBIA = .TRUE.
      END IF

*  Master flatfield.
      CALL PAR_GET0C( 'FLAT', FLAT, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTFLA = .FALSE.
      ELSE
         GOTONE = .TRUE.
         GOTFLA = .TRUE.
      END IF
      
*  Master calibration frame.
      CALL PAR_GET0C( 'CAL', CAL, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GOTCAL = .FALSE.
      ELSE
         GOTONE = .TRUE.
         GOTCAL = .TRUE.
      END IF

*  Report global values if any have been found.
      IF ( GOTONE .OR. GOTLGN .OR. GOTLG2 ) THEN
         CALL CCD1_MSG( ' ', '  Listing of the current '//
     :                 'CCDPACK global parameters:',
     :                 STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_RGLO( GOTLG2, LOGTO, GOTLGN, LOGNAM, GOTADC,
     :                   ADC, GOTNOI, RNOISE, GOTEXT, EXTENT,
     :                   GOTBDS, BOUNDS, NBOUND, GOTDIR, DIRECT,
     :                   GOTDEF, DEFER, GOTMSK, MSKNAM, GOTSAT,
     :                   SATUR, GOTSPR, SETSAT, GOTSVL, SATVAL,
     :                   GOTPRE, PRESER, GOTGEN, GENVAR, GOTNAM,
     :                   NDFS, GOTFLA, FLAT, GOTBIA, BIAS,
     :                   GOTCAL, CAL, STATUS )
      END IF

      IF ( .NOT. GOTONE ) THEN 

*  No significant values
         IF ( GOTLGN .OR. GOTLG2 ) CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  No current settings exist for any'//
     :                      ' significant CCDPACK global parameters -'//
     :                      ' use CCDSETUP to set any required'//
     :                      ' values ', STATUS )
       END IF


*  Close down.
 99   CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'CCDSHOW_ERR',
     :   'CCDSHOW: Error showing global parameters...',
     :   STATUS )
      END IF

*  Terminate CCDPACK logging
      CALL CCD1_END( STATUS )

      END
* $Id$
