      SUBROUTINE DSA_GET_DATA_INFO( DSAREF,
     :   NCITEM, CITEMS, NNITEM, NITEMS, STATUS )
*+
*  Name:
*     DSA_GET_DATA_INFO

*  Purpose:
*     Obtain information about the data array in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_DATA_INFO( DSAREF,
*        NCITEM, CITEMS, NNITEM, NITEMS, STATUS )

*  Description:
*     This routine obtains a number of items from an NDF that
*     relate directly to the data array itself. If these items do not in
*     fact exist, then character items are returned as blank, and
*     numeric items are returned as zero.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     NCITEM = INTEGER (Given)
*        The number of character items to be returned.
*     CITEMS( NCITEM ) = CHARACTER * ( * ) (Returned)
*        The character items returned.  CITEMS(1) is the data units,
*        and CITEMS(2) is the data label.  At present, only those are
*        supported.
*     NNITEM = INTEGER (Given)
*        The number of numeric items to be returned.
*     NITEMS( NNITEM ) = DOUBLE PRECISION (Returned)
*        The numeric items returned. At present only NITEMS(1) is
*        supported, this being the magnitude flag (non-zero => magnitude
*        data).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Jul 1987 (ks):
*        Original version.
*     02 May 1989 (ks):
*        Fixed bug in reading of magnitude flag - thanks, MSC.
*     08 Dec 1989 (ks):
*        Comments reformatted to avoid problems when processed using
*        MAN.
*     28 Feb 1990 (ks):
*        Modified to use DSA__ routines rather than assuming the
*        original Figaro data format.
*     12 Mar 1990 (ks):
*        Comments corrected - DSA__CREATE_EXTRA not used.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     10 Sep 1992 (hme):
*        There seems to be a problem for DAT_GET (in DTA_RDVAR) to get a
*        _DOUBLE from an _INTEGER. This shows up in CALDIV when it tries
*        to get the magnitude flag. Here we try to circumvent the
*        problem by using DTA_RDVARI.
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
      CHARACTER * ( DAT__SZLOC ) TLOC ! HDS locator

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
         CALL NDF_STATE( DSA__REFID1(SLOT), 'UNITS', THERE, STATUS )
         IF ( THERE )CALL NDF_CGET( DSA__REFID1(SLOT),
     :      'UNITS', CITEMS(1), STATUS )
      END IF

*  Get the label string.
      IF ( NCITEM .GE. 2 ) THEN
         CALL NDF_STATE( DSA__REFID1(SLOT), 'LABEL', THERE, STATUS )
         IF ( THERE ) CALL NDF_CGET( DSA__REFID1(SLOT),
     :      'LABEL', CITEMS(2), STATUS )
      END IF

*  Get the magnitude flag.
      IF ( NNITEM .GE. 1 ) THEN
         CALL NDF_XSTAT( DSA__REFID1(SLOT), 'FIGARO', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_XLOC( DSA__REFID1(SLOT), 'FIGARO', 'READ',
     :         TLOC, STATUS )
            CALL DAT_THERE( TLOC, 'MAGFLAG', THERE, STATUS )
            IF ( THERE ) THEN
               CALL CMP_GET0I( TLOC, 'MAGFLAG', I, STATUS )
               IF ( STATUS .EQ. SAI__OK ) NITEMS(1) = I
            END IF
            CALL DAT_ANNUL( TLOC, STATUS )
         END IF
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
