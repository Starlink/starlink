      SUBROUTINE IRM_IATTR( ITEM, CURVE, IVAL, LVAL, STATUS )
*+
*  Name:
*     IRM_IATTR

*  Purpose:
*     Get an item of graphical information from IRM common blocks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_IATTR( ITEM, CURVE, IVAL, LVAL, STATUS )

*  Description:
*     Various IRM routines (written by Wei Gong) allow attributes of an
*     NCAR plot to be set up (eg colour, dashed line pattern, etc).
*     These routines store items of information in internal IRM common
*     blocks. However, NCAR applications such as TRACECRDD need to get
*     at these values stored in common. It is bad practice for an
*     application to attempt to directly access common blocks declared
*     internally within a (supposedly) independent subroutine package,
*     as it means that the implementation of IRM cannot be changed
*     without also changing the applications.  Also, IRM is usually
*     distributed in a sharable form, which makes it difficult for
*     applications to access its internal common blocks.
*
*     This routine gets round these problems by providing a subroutine
*     interface for accessing these common values.

*  Arguments:
*     ITEM = CHARACTER * ( * ) (Given)
*        The attribute required. These are listed in the "Notes:"
*        section below.
*     CURVE = INTEGER (Given)
*        The curve to which the returned value should refer. An error
*        is reported if this is out of bounds (1 to the value of the
*        NCURV attribute). It is ignored if the item does not have a
*        separate value for each curve.
*     IVAL = INTEGER (Returned)
*        The value of an integer-type attribute. The data type of each
*        attribute is listed in the "Notes:" section below.
*     LVAL = LOGICAL (Returned)
*        The value of an logical-type attribute.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  MXCURV: Maximum no. of curves (_INTEGER).
*     -  OLDPX: Original GKS polyline colour index (_INTEGER).
*     -  OLDTX: Original GKS polyline text index (_INTEGER).
*     -  SOCUR: True if polylines are drawn solid (_LOGICAL).
*     -  NCURV: The no. of curves with defined attributes. The
*     argument CURVE must be supplied with a value no greater than
*     NCURV.
*     -  CRPN: Pen number for each curve. (_INTEGER)
*     -  INCL: Colour indices of the in-line labels for each curve. (_INTEGER)
*     -  AXPN: Pen number of axis lines (_INTEGER)
*     -  TKPN: Pen number of tick marks (_INTEGER)
*     -  NLBCL: Colour index of numeric labels. (_INTEGER)
*     -  TITCL: Colour indices of title. (_INTEGER)
*     -  ALBCL: Colour indices of axes labels. (_INTEGER)
*     -  STDAT: True if values have already been assigned for these
*     attributes. (_LOGICAL)

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'IRM_COM'          ! IRM internal common blocks.
*        MCM_OLDPX = INTEGER (Read)
*           The original GKS polyline colour index
*        MCM_OLDTX = INTEGER (Read)
*           The original GKS text colour index
*        MCM_SOCUR = LOGICAL (Read)
*           The flag to show whether the curves are drawn in solid
*           lines.
*        MCM_NCURV = INTEGER (Read)
*          The number of curves which have been set pen numbers.
*        MCM_CRPN( MXCURV ) = INTEGER (Read)
*           The pen number of each curve.
*        MCM_INCL( MXCURV )  = INTEGER (Read)
*           Colour indices of the in-line labels.
*        MCM_AXPN = INTEGER (Read)
*           Pen number of axis lines
*        MCM_TKPN = INTEGER (Read)
*           Pen number of tick marks
*        MCM_NLBCL = INTEGER (Read)
*           Colour index of numeric labels.
*        MCM_TITCL = INTEGER (Read)
*           Colour indices of title
*        MCM_ALBCL = INTEGER (Read)
*           Colour indices of axes labels.
*        MCM_STDAT = LOGICAL (Read)
*           True if variables have already been set.

*  Arguments Given:
      CHARACTER ITEM*(*)
      INTEGER CURVE

*  Arguments Returned:
      INTEGER IVAL
      LOGICAL LVAL

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do each recognised item in turn.
      IF( ITEM .EQ. 'MXCURV' ) THEN
         IVAL = MXCURV

      ELSE IF( ITEM .EQ. 'OLDPX' ) THEN
         IVAL = MCM_OLDPX

      ELSE IF( ITEM .EQ. 'OLDTX' ) THEN
         IVAL = MCM_OLDTX

      ELSE IF( ITEM .EQ. 'SOCUR' ) THEN
         LVAL = MCM_SOCUR

      ELSE IF( ITEM .EQ. 'NCURV' ) THEN
         IVAL = MCM_NCURV

      ELSE IF( ITEM .EQ. 'CRPN' ) THEN

         IF( CURVE .GT. 0 .AND. CURVE .LE. MCM_NCURV ) THEN
            IVAL = MCM_CRPN( CURVE )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'C', CURVE )
            CALL MSG_SETI( 'MAX', MCM_NCURV )
            CALL ERR_REP( 'IRM_IATTR_ERR1',
     :    'IRM_IATTR: Curve no. (^C) is outside valid range [1,^MAX]',
     :                    STATUS )
         END IF

      ELSE IF( ITEM .EQ. 'INCL' ) THEN

         IF( CURVE .GT. 0 .AND. CURVE .LE. MCM_NCURV ) THEN
            IVAL = MCM_INCL( CURVE )

         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'C', CURVE )
            CALL MSG_SETI( 'MAX', MCM_NCURV )
            CALL ERR_REP( 'IRM_IATTR_ERR2',
     :    'IRM_IATTR: Curve no. (^C) is outside valid range [1,^MAX]',
     :                    STATUS )
         END IF

      ELSE IF( ITEM .EQ. 'AXPN' ) THEN
         IVAL = MCM_AXPN

      ELSE IF( ITEM .EQ. 'TKPN' ) THEN
         IVAL = MCM_TKPN

      ELSE IF( ITEM .EQ. 'NLBCL' ) THEN
         IVAL = MCM_NLBCL

      ELSE IF( ITEM .EQ. 'TITCL' ) THEN
         IVAL = MCM_TITCL

      ELSE IF( ITEM .EQ. 'ALBCL' ) THEN
         IVAL = MCM_ALBCL

      ELSE IF( ITEM .EQ. 'STDAT' ) THEN
         LVAL = MCM_STDAT

*  Report an error if an unknown attribute was requested.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ATT', ITEM )
         CALL ERR_REP( 'IRM_IATTR_ERR3',
     :                 'IRM_IATTR: Unknown graphical attribute - ^ATT',
     :                 STATUS )
      END IF

      END
