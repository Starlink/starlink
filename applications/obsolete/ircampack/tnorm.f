      SUBROUTINE TNORM( STATUS )
*+
*  Name:
*     TNORM

*  Purpose:
*     Normalise a group of frames to unit exposure time

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL TNORM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine normalises the input frames to an exposure time of
*     one second by dividing the DATA values by the total exposure time
*     (in seconds), and the VARIANCE values by the square of the total
*     exposure time.

*  Usage:
*     TNORM IN OUT

*  ADAM Parameters:
*     IN = NDF (Read)
*        A group of input NDFs. This should be in the form of a group
*        expression (see help on "Group_expressions").
*     OUT = NDF (Read)
*        A group of output NDFs corresponding one-for-one with the list
*        of input NDFs given by parameter IN. This should be in the
*        form of a group expression. Expressions such as "*_NEW" are
*        expanded by replacing the "*" character with each input NDF in
*        turn (see help on "Group_expressions").

*  Examples:
*     TNORM ^FILES.LIS *_N
*        This example normalises all the frames listed in text file
*        FILES.LIS, putting the output NDFs in files with the same
*        names, extended with the string "_N".

*  Notes:
*     - The total exposure time is determined from the information
*     stored in the IRCAM extensions of the input NDFs and is the
*     product of the number of coadds (NUMBER_COADDS) and the exposure
*     time per coadd (EXPOSURE_TIME).  The value of EXPOSURE_TIME is
*     divided by 1000 before use to convert it from milliseconds to
*     seconds. An error is reported if any input NDFs do not contain an
*     IRCAM extension.
*     - A new component called TNORM is written to the IRCAM extensions
*     of the output NDFs to indicate that the output frames are
*     normalised to an exposure time of one second. TNORM is a _REAL
*     value holding the total exposure time by which the frame has been
*     divided. An error is reported if any of the input NDFs already
*     contain this component (i.e. have already been normalised).
*     -  The TITLE, LABEL, QUALITY and AXIS components are propagated to
*     the output NDFs without change.
*     -  The UNITS components of the output NDFs are derived from the
*     input UNITS components by appending the string " per second".
*     -  The output NDFs have the same numeric type as the input NDFs.
*     However, all internal calculations are done in double precision.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}



*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      CHARACTER
     :        COMC*1,            ! Output group comment character
     :        OBSLOC*(DAT__SZLOC),! Locator to OBS component of IRCAM Xt
     :        OLDUN*80,          ! Input units.
     :        OUTNDF*(GRP__SZNAM),! Name of the current output NDF
     :        TEXT*(GRP__SZNAM), ! Text for output group list file
     :        UNITS*80,          ! Output units
     :        XLOC*(DAT__SZLOC)  ! Locator to the IRCAM extension

      INTEGER
     :        EL,                ! No. of elements in the NDF
     :        F,                 ! Index of first character in OLDUN
     :        I,                 ! Index into input and output groups
     :        IGRP1,             ! Identifier for input NDF group
     :        IGRP2,             ! Identifier for output NDF group
     :        INDF1,             ! Identifier for the input NDF
     :        INDF2,             ! Identifier for the output NDF
     :        IPIN,              ! Pointer to mapped input array
     :        IPOUT,             ! Pointer to mapped output array
     :        L,                 ! Index of last character in OLDUN
     :        NCOADD,            ! No. of coadds in the frame
     :        NOUT,              ! No. of good output NDFs
     :        SIZE,              ! Total size of the input group
     :        SIZEO,             ! Total size of the output group
     :        TLEN               ! Used length of text string

      LOGICAL
     :        BAD,               ! True if any bad values found
     :        THERE,             ! True if an object is found
     :        VAR                ! True if VARIANCE array is defined

      REAL
     :        EXPTIM,            ! Exposure time per coadd, in mS
     :        TIME               ! Total exposure time, in seconds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a group containing the names of the NDFs to be processed.
      CALL RDNDF( 'IN', 0, 1, '  Give more NDF names...', IGRP1, SIZE,
     :            STATUS )

*  Similarly, get a group containing the names of the output NDFs.
*  Base modification elements on the group containing the input NDFs.
      CALL WRNDF( 'OUT', IGRP1, SIZE, SIZE, '  Give more NDF names...',
     :            IGRP2, SIZEO, STATUS )

*  Get the character used to introduce comments into group expressions
*  relating to the output group.
      CALL GRP_GETCC( IGRP2, 'COMMENT', COMC, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the number of good output NDFs produced so far.
      NOUT = 0

*  Loop round each NDF to be processed.
      DO I = 1, SIZE
         CALL MSG_BLANK( STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'READ', INDF1, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_OUT( 'TNORM_MSG1', '  Processing ^NDF...', STATUS )

*  See if the input NDF contains an IRCAM extension.
         CALL NDF_XSTAT( INDF1, 'IRCAM', THERE, STATUS )

*  If not report an error.
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF1 )
            CALL ERR_REP( 'TNORM_ERR1', 'Input NDF ''^NDF'' does '//
     :                    'not contain an IRCAM extension.', STATUS )
         END IF

*  Get a locator to the IRCAM extension.
         CALL NDF_XLOC( INDF1, 'IRCAM', 'READ', XLOC, STATUS )

*  See if the extension contains a component called TNORM.
         CALL DAT_THERE( XLOC, 'TNORM', THERE, STATUS )

*  If it does, the NDF has already been normalised, so report an error.
         IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF1 )
            CALL ERR_REP( 'TNORM_ERR2', 'Input NDF ''^NDF'' has '//
     :                    'already been normalised to unit '//
     :                    'exposure time.', STATUS )
         END IF

*  Get a locator to the OBS component.
         CALL DAT_FIND( XLOC, 'OBS', OBSLOC, STATUS )

*  Get the value of the OBS.EXPOSURE_TIME component.
         CALL CMP_GET0R( OBSLOC, 'EXPOSURE_TIME', EXPTIM, STATUS )

*  Get the value of the OBS.NUMBER_COADDS component.
         CALL CMP_GET0I( OBSLOC, 'NUMBER_COADDS', NCOADD, STATUS )

*  Annul the locators.
         CALL DAT_ANNUL( OBSLOC, STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )

*  Calculate the total exposure time, in seconds and tell the user.
         TIME = REAL( NCOADD )*EXPTIM*0.001

*  Report an error if the total exposure time is zero.
         IF( TIME .EQ. 0.0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'TNORM_ERR', 'Total exposure time is zero.',
     :                    STATUS )

*  Otherwise, tell the user what the total exposure time is.
         ELSE
            CALL MSG_SETR( 'TIME', TIME )
            CALL MSG_OUT( 'TNORM_MSG2', '    Total exposure time: '//
     :                    '^TIME seconds', STATUS )
         END IF

*  Obtain an identifier for the output NDF, propagating all components
*  from the input to the output except DATA, VARIANCE and UNITS (the
*  HISTORY, LABEL, TITLE and all extensions are propagated by default).
         CALL NDG_NDFPR( INDF1, 'QUALITY,AXIS', IGRP2, I, INDF2,
     :                   STATUS )

*  Map the DATA components of the input and output NDFs, using double
*  precision.
         CALL NDF_MAP( INDF1, 'DATA', '_DOUBLE', 'READ', IPIN, EL,
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'DATA', '_DOUBLE', 'WRITE', IPOUT, EL,
     :                 STATUS )

*  Divide the input data values by the total time and write to the
*  output data arrays.
         CALL CDIVD( DBLE( TIME ), EL, %VAL( IPIN ), %VAL( IPOUT ),
     :               BAD, STATUS )

*  Set the bad pixel flag for the output DATA array.
         CALL NDF_SBAD( BAD, INDF2, 'DATA', STATUS )

*  Un-map the mapped arrays.
         CALL NDF_UNMAP( INDF1, 'DATA', STATUS )
         CALL NDF_UNMAP( INDF2, 'DATA', STATUS )

*  See if the input NDF has a defined VARIANCE component.
         CALL NDF_STATE( INDF1, 'VARIANCE', THERE, STATUS )

*  If so, map the input and output VARIANCE components.
         IF( THERE ) THEN
            CALL NDF_MAP( INDF1, 'VARIANCE', '_DOUBLE', 'READ', IPIN,
     :                    EL, STATUS )
            CALL NDF_MAP( INDF2, 'VARIANCE', '_DOUBLE', 'WRITE', IPOUT,
     :                    EL, STATUS )

*  Divide the input variance values by the square of the total time and
*  write to the output variance array.
            CALL CDIVD( DBLE( TIME**2 ), EL, %VAL( IPIN ),
     :                  %VAL( IPOUT ), BAD, STATUS )

*  Set the bad pixel flag for the output VARIANCE array.
            CALL NDF_SBAD( BAD, INDF2, 'VARIANCE', STATUS )

*  Un-map the mapped arrays.
            CALL NDF_UNMAP( INDF1, 'VARIANCE', STATUS )
            CALL NDF_UNMAP( INDF2, 'VARIANCE', STATUS )

         END IF

*  See if the input NDF has a defined UNITS component.
         CALL NDF_STATE( INDF1, 'UNITS', THERE, STATUS )

*  If so, get its value.
         IF( THERE ) THEN
            UNITS = ' '
            CALL NDF_CGET( INDF1, 'UNITS', OLDUN, STATUS )

*  Append " per second" to it and store it in the output NDF.
            CALL CHR_FANDL( OLDUN, F, L )
            IF( F .LE. L ) THEN
               UNITS = OLDUN( F : L )//' per second'
               CALL NDF_CPUT( UNITS, INDF2, 'UNITS', STATUS )

*  If the input UNITS component is blank, store a blank value in the
*  output.
            ELSE
               CALL NDF_CPUT( ' ', INDF2, 'UNITS', STATUS )
            END IF

         END IF

*  Store the total exposure time in the output NDF, in a component
*  called TNORM within the IRCAM extension.
         CALL NDF_XPT0R( TIME, INDF2, 'IRCAM', 'TNORM', STATUS )

*  Annul the input NDF identifier.
         CALL NDG_ANNUL( INDF1, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDG_DELET( INDF2, STATUS )
         ELSE
            CALL NDG_ANNUL( INDF2, STATUS )
         END IF

*  If an error occured processing the current input NDF, flush the
*  error.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_FLUSH( STATUS )

*  Give a warning telling the user that no output NDF will be created
*  for the current input NDF.
            CALL GRP_GET( IGRP2, I, 1, OUTNDF, STATUS )
            CALL MSG_SETC( 'NDF', OUTNDF )
            CALL MSG_OUTIF( MSG__QUIET, 'TNORM_MSG3',
     :                      'WARNING: ''^NDF'' cannot be produced',
     :                      STATUS )

*  Overwrite the current output NDF name with a string which will be
*  interpreted as a comment by later applications.
            TEXT = COMC//'          '//OUTNDF( : CHR_LEN( OUTNDF ) )//
     :             ' could not be produced'
            CALL GRP_PUT( IGRP2, 1, TEXT, I, STATUS )

*  If no error occurred, increment the number of good output NDFs.
         ELSE
            NOUT = NOUT + 1
         END IF

*  Process the next input NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANK( STATUS )

*  Assign a group expression to the output parameter NDFLIST which
*  specifies all the output NDFs. NDFLIST should normally be associated
*  with a suitable global parameter to cause its value to be passed on
*  to the next application.  The output parameter NDFLIST is not
*  advertised as a user parameter since users will normally not be
*  aware of the existence of global parameter, and so will not know
*  how to assign a value to it.
      IF( NOUT .GT. 0 ) CALL LISTN( 'NDFLIST', IGRP2, 'TNORM', STATUS )

*  Delete all groups.
 999  CONTINUE
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was requested,
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'TNORM_ERR3', 'TNORM: Unable to normalise '//
     :                    'IRCAM rames to unit exposure time.', STATUS )
         END IF

      END IF

      END
