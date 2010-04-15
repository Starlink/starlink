      SUBROUTINE DSA_GET_AXIS_INFO( DSAREF, AXIS,
     :   NCITEM, CITEMS, NNITEM, NITEMS, STATUS )
*+
*  Name:
*     DSA_GET_AXIS_INFO

*  Purpose:
*     Obtain information held for an axis.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_AXIS_INFO( DSAREF, AXIS,
*        NCITEM, CITEMS, NNITEM, NITEMS, STATUS )

*  Description:
*     This routine obtains a number of items for an axis. If
*     these items do not in fact exist, then character items are
*     returned as blank, and numeric items are returned as zero.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis.
*     NCITEM = INTEGER (Given)
*        The number of character items to be returned.
*     CITEMS( NCITEM ) = CHARACTER * ( * ) (Returned)
*        The character items returned.  CITEMS(1) is the axis units,
*        and CITEMS(2) is the axis label.  At present, only those are
*        supported.
*     NNITEM = INTEGER (Given)
*        The number of numeric items to be returned.
*     NITEMS( NNITEM ) = DOUBLE PRECISION (Returned)
*        The numeric items returned.  At present only NITEMS(1) is
*        supported, this being the `log binning' flag (non-zero => `log
*        binning' flag set).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Jul 1987 (ks):
*        Original version.
*     15 Dec 1989 (ks):
*        Added log binning flag to NUM_ARRAY.  Recoded to use DSA__
*        routines to get data object names.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     12 Oct 1992 (hme):
*        There seems to be a problem for DAT_GET (in DTA_RDVAR) to get a
*        _DOUBLE from an _INTEGER. Here we try to circumvent the problem
*        by using DTA_RDVARI.
*     02 Feb 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
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
      INTEGER NNITEM

*  Arguments Returned:
      CHARACTER * ( * ) CITEMS( NCITEM )
      DOUBLE PRECISION  NITEMS( NNITEM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether an HDS component exists
      INTEGER I                  ! Loop index
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

*  Default values.
      DO 1 I = 1, NCITEM
         CITEMS(I) = ' '
 1    CONTINUE
      DO 2 I = 1, NNITEM
         NITEMS(I) = 0D0
 2    CONTINUE

*  Get the unit string.
      IF ( NCITEM .GE. 1 ) THEN
         CALL NDF_ASTAT( DSA__REFID1(SLOT), 'UNITS', AXIS,
     :      THERE, STATUS )
         IF ( THERE ) CALL NDF_ACGET( DSA__REFID1(SLOT),
     :      'UNITS', AXIS, CITEMS(1), STATUS )
      END IF

*  Get the label string.
      IF ( NCITEM .GE. 2 ) THEN
         CALL NDF_ASTAT( DSA__REFID1(SLOT), 'LABEL', AXIS,
     :      THERE, STATUS )
         IF ( THERE ) CALL NDF_ACGET( DSA__REFID1(SLOT),
     :      'LABEL', AXIS, CITEMS(2), STATUS )
      END IF

*  Get the log binning flag.
      IF ( NNITEM .GE. 1 ) THEN
         CALL NDF_STATE( DSA__REFID1(SLOT), 'AXIS', THERE, STATUS )
         CALL NDF_LOC( DSA__REFID1(SLOT), 'READ', TLOC(1), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL DAT_ANNUL( TLOC(1), STATUS )
            GO TO 500
         END IF
         IF ( THERE ) THEN
            CALL DAT_FIND( TLOC(1), 'AXIS', TLOC(2), STATUS )
            CALL DAT_CELL( TLOC(2), 1, AXIS, TLOC(3), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL DAT_ANNUL( TLOC(3), STATUS )
               CALL DAT_ANNUL( TLOC(2), STATUS )
               CALL DAT_ANNUL( TLOC(1), STATUS )
               GO TO 500
            END IF
            CALL DAT_THERE( TLOC(3), 'MORE', THERE, STATUS )
            IF ( THERE ) THEN
               CALL DAT_FIND( TLOC(3), 'MORE', TLOC(4), STATUS )
               CALL DAT_THERE( TLOC(4), 'FIGARO', THERE, STATUS )
               IF ( THERE ) THEN
                  CALL DAT_FIND( TLOC(4), 'FIGARO', TLOC(5), STATUS )
                  CALL DAT_THERE( TLOC(5), 'LOG', THERE, STATUS )
                  IF ( THERE ) THEN
                     CALL CMP_GET0I( TLOC(5), 'LOG', I, STATUS )
                     IF ( STATUS .EQ. SAI__OK ) NITEMS(1) = I
                  END IF
                  CALL DAT_ANNUL( TLOC(5), STATUS )
               END IF
               CALL DAT_ANNUL( TLOC(4), STATUS )
            END IF
            CALL DAT_ANNUL( TLOC(3), STATUS )
            CALL DAT_ANNUL( TLOC(2), STATUS )
         END IF
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
