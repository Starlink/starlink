      SUBROUTINE IRA_DRVAL( VALUE, SCS, NC, XPOS, YPOS, STYLE, ACC,
     :                      CONTXT, STATUS )
*+
*  Name:
*     IRA_DRVAL

*  Purpose:
*     Plot a text string holding a longitude or latitude value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_DRVAL( VALUE, SCS, NC, XPOS, YPOS, STYLE, ACC, CONTXT,
*                     STATUS )

*  Description:
*     This routine plots a text string holding a formatted version of a
*     longitude or latitude value, at a user specified location.
*     Plotting is performed with the current SGS zone.
*
*     A facility is available to suppress the display of leading fields
*     which have not changed since the last value was displayed (see
*     argument CONTXT).

*  Arguments:
*     VALUE = DOUBLE PRECISION (Given)
*        The longitude or latitude value, in radians.
*     SCS = CHARACTER * ( * ) (Given)
*        The name of the sky coordinate system to use. Any unambiguous
*        abbreviation will do. See ID/2 section "Sky Coordinates" for
*        more information.
*     NC = INTEGER (Given)
*        The axis index; 1 if VALUE is a longitude value and 2 if it is
*        a latitude value.
*     XPOS = REAL (Given)
*        The world coordinate X value at which to put the text. The
*        current SGS text justification and up vector are used to
*        position and orient the text.
*     YPOS = REAL (Given)
*        The world coordinate Y value at which to put the text.
*     STYLE = INTEGER (Given)
*        The style number for the text. The styles available are
*        described in IRA_DTOC. Note, styles 1 and 3 are not available
*        and style 5 has a superscript unit symbol added at the end of
*        the string. Superscripts (h, m, s, o, ' or " ) are used to
*        indicate units rather than the normal text characters h, m, s
*        and d.
*     ACC = DOUBLE PRECISION (Given)
*        Specifies the accuracy to which the value should be displayed,
*        in radians.  The displayed text is such that a change of 1 in
*        the least significant field corresponds to the largest value
*        which is smaller than (or equal to) the supplied value of ACC.
*        For instance, if an arc-seconds field is not required in the
*        displayed text, then ACC could be given the radian equivalent
*        of 1 arc-minute.
*     CONTXT = CHARACTER * ( * ) (Given and Returned)
*        The "context ". This returns information about the text fields
*        plotted by this routine. This value can be passed on to the
*        next call of this routine in order to suppress the display of
*        unchanged leading fields. If the supplied context is blank or
*        invalid, then all fields are displayed and the context is
*        returned holding information describing the displayed text
*        fields. If the supplied context is equal to "KEEP" then all
*        fields are displayed, and the value of CONTXT is left
*        unchanged. In normal practice CONTXT should be set blank
*        before the first call to this routine, and then left
*        un-altered between successive calls. It should have a declared
*        size equal to the symbolic constant IRA__SZCTX.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is effected by the COORD_SIZE and PEN4 options
*     set up by routine IRA_DROPT.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     9-NOV-1992 (DSB):
*        Graphics options introduced.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      DOUBLE PRECISION VALUE
      CHARACTER SCS*(*)
      INTEGER NC
      REAL XPOS
      REAL YPOS
      INTEGER STYLE
      DOUBLE PRECISION ACC

*  Arguments Given and Returned:
      CHARACTER CONTXT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External Routines:
      EXTERNAL IRA1_INIT         ! Initialise graphics options in common

*  Local Variables:
      CHARACTER        BJ*1      ! The type of epoch (Besselian or
                                 ! Julian) held by variable EQU.
      DOUBLE PRECISION EQU       ! The epoch of the reference equinox
                                 ! specified in argument SCS.
      INTEGER          LSTYLE    ! Local copy of style.
      CHARACTER        NAME*(IRA__SZSCS)! Full SCS name.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Abort if the supplied coordinate or accuracy value is bad.
      IF( VALUE .EQ. VAL__BADD .OR. ACC .EQ. VAL__BADD ) GO TO 999

*  Verify the argument NC.
      IF( NC .NE. 1 .AND. NC .NE. 2 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__BADNC
         CALL MSG_SETI( 'NC', NC )
         CALL ERR_REP( 'IRA_DRVAL_ERR1',
     :       'IRA_DRVAL: Invalid value supplied for argument NC: ^NC',
     :                 STATUS )
      END IF

*  Identify the SCS.
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )

*  Verify the argument STYLE.
      IF( ( STYLE .LT. 0 .OR. STYLE .GT. 5 .OR.
     :      STYLE .EQ. 1 .OR. STYLE .EQ. 3 )
     :     .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRA__BADST
         CALL MSG_SETI( 'ST', STYLE )
         CALL ERR_REP( 'IRA_DRVAL_ERR2',
     :     'IRA_DRVAL: Invalid value supplied for argument STYLE: ^ST',
     :                 STATUS )

*  If style zero supplied, set up the default style number for each SCS.
      ELSE IF( STYLE .EQ. 0 ) THEN

         IF( NAME(:10) .EQ. 'EQUATORIAL' ) THEN
            LSTYLE = 2

         ELSE IF( NAME(:8) .EQ. 'ECLIPTIC' ) THEN
            LSTYLE = 5

         ELSE IF( NAME(:8) .EQ. 'GALACTIC' ) THEN
            LSTYLE = 5

*  Report an error if this routine does not yet support the requested
*  sky coordinate system.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = IRA__BADSC
            CALL MSG_SETC( 'SCS', NAME )
            CALL ERR_REP( 'IRA_DRVAL_ERR3',
     : 'IRA_DRVAL: Sky coordinates ^SCS not yet supported.',
     :                     STATUS )
         END IF

*  Otherwise copy the supplied style to local storage.
      ELSE
         LSTYLE = STYLE

      END IF

*  Call a lower level routine to do the work.
      CALL IRA1_IDRVA( VALUE, NAME, NC, XPOS, YPOS, LSTYLE, ACC,
     :                 CONTXT, STATUS )

*  If an error occurred, give a contextual message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_DRVAL_ERR5',
     :               'IRA_DRVAL: Error plotting a sky coordinate value',
     :                 STATUS )
      END IF

      END
