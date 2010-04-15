      SUBROUTINE DSA_SET_AXIS_INFO( DSAREF, AXIS,
     :   NCITEM, CITEMS, NNITEM, NITEMS, STATUS )
*+
*  Name:
*     DSA_SET_AXIS_INFO

*  Purpose:
*     Set some of the information held about an axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SET_AXIS_INFO( DSAREF, AXIS,
*        NCITEM, CITEMS, NNITEM, NITEMS, STATUS )

*  Description:
*     Sets a number of items in an axis structure.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis.
*     NCITEM = INTEGER (Given)
*        The number of character items to be set.
*     CITEMS( NCITEM ) = CHARACTER * ( * ) (Given)
*        The character items to be set.  CITEMS(1) is the axis units,
*        and CITEMS(2) is the axis label.  At present, only those are
*        supported.
*     NNITEM = INTEGER (Given)
*        The number of numeric items to be set.
*     NITEMS( NNITEM ) = DOUBLE PRECISION (Returned)
*        The numeric items returned.  At present only NITEMS(1) is
*        supported, this being the `log binning' flag (non-zero => `log
*        binning' flag set).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12 Aug 1987 (ks):
*        Original version.
*     15 Dec 1989 (ks):
*        `Log binning' flag added. Now uses DSA__ routines for structure
*        access.
*     26 Feb 1990 (ks):
*        Added call to DSA__CREATE_AXIS_EXTRA.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     10 Sep 1992 (hme):
*        There seems to be a problem for DAT_PUT (in DTA_WRVAR) to
*        convert a _DOUBLE into an _INTEGER. Here we try to circumvent
*        the problem by using DTA_WRVARI.
*     02 Feb 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     1996 July 9 (MJC):
*        Calls DSA2_ACRE for NDF_ACRE to allow default axis co-ordinates
*        to be pixel indices.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      INTEGER AXIS
      INTEGER NCITEM
      CHARACTER * ( * ) CITEMS( NCITEM )
      INTEGER NNITEM
      DOUBLE PRECISION  NITEMS( NNITEM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether an HDS component exists
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( DAT__SZLOC ) TLOC( 5 ) ! HDS locators

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Put the unit string.
      IF ( NCITEM .GE. 1 ) CALL NDF_ACPUT( CITEMS(1), DSA__REFID1(SLOT),
     :      'UNITS', AXIS, STATUS )

*  Put the label string.
      IF ( NCITEM .GE. 2 ) CALL NDF_ACPUT( CITEMS(2), DSA__REFID1(SLOT),
     :      'LABEL', AXIS, STATUS )

*  Put the log binning flag.
      IF ( NNITEM .GE. 1 ) THEN
         CALL DSA2_ACRE( DSA__REFID1(SLOT), STATUS )
         CALL NDF_LOC( DSA__REFID1(SLOT), 'UPDATE', TLOC(1), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ANNUL( TLOC(1), STATUS )
            GO TO 500
         END IF
         CALL DAT_FIND( TLOC(1), 'AXIS', TLOC(2), STATUS )
         CALL DAT_CELL( TLOC(2), 1, AXIS, TLOC(3), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ANNUL( TLOC(3), STATUS )
            CALL DAT_ANNUL( TLOC(2), STATUS )
            CALL DAT_ANNUL( TLOC(1), STATUS )
            GO TO 500
         END IF
         CALL DAT_THERE( TLOC(3), 'MORE', THERE, STATUS )
         IF ( .NOT. THERE ) CALL DAT_NEW( TLOC(3),
     :      'MORE', 'EXT', 0, 0, STATUS )
         CALL DAT_FIND( TLOC(3), 'MORE', TLOC(4), STATUS )
         CALL DAT_THERE( TLOC(4), 'FIGARO', THERE, STATUS )
         IF ( .NOT. THERE ) CALL DAT_NEW( TLOC(4),
     :      'FIGARO', 'FIGARO_EXT', 0, 0, STATUS )
         CALL DAT_FIND( TLOC(4), 'FIGARO', TLOC(5), STATUS )
         CALL DAT_THERE( TLOC(5), 'LOG', THERE, STATUS )
         IF ( .NOT. THERE ) CALL DAT_NEW( TLOC(5),
     :      'LOG', '_INTEGER', 0, 0, STATUS )
         CALL CMP_PUT0I( TLOC(5), 'LOG', INT(NITEMS(1)), STATUS )
         CALL DAT_ANNUL( TLOC(5), STATUS )
         CALL DAT_ANNUL( TLOC(4), STATUS )
         CALL DAT_ANNUL( TLOC(3), STATUS )
         CALL DAT_ANNUL( TLOC(2), STATUS )
         CALL DAT_ANNUL( TLOC(1), STATUS )
      END IF

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
