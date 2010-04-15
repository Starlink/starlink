      SUBROUTINE DSA_SET_DATA_INFO( DSAREF,
     :   NCITEM, CITEMS, NNITEM, NITEMS, STATUS )
*+
*  Name:
*     DSA_SET_DATA_INFO

*  Purpose:
*     Set information about the data array in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SET_DATA_INFO( DSAREF,
*        NCITEM, CITEMS, NNITEM, NITEMS, STATUS )

*  Description:
*     Sets a number of items in a data structure that relate directly to
*     the data array itself.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     NCITEM = INTEGER (Given)
*        The number of character items to be set.
*     CITEMS( NCITEM ) = CHARACTER * ( * ) (Given)
*        The character items to be set.  CITEMS(1) is the data units,
*        and CITEMS(2) is the data label. At present, only those are
*        supported.
*     NNITEM = INTEGER (Given)
*        The number of numeric items to be set.
*     NITEMS( NNITEM ) = DOUBLE PRECISION (Returned)
*        The numeric items returned. At present only NITEMS(1) is
*        supported, this being the magnitude flag (non-zero => magnitude
*        data).
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
*     08 Dec 1989 (ks):
*        Comments reformatted to avoid problems when processing them
*        using MAN.
*     27 Feb 1990 (ks):
*        Modified to use DSA__ routines rather than assuming the
*        original Figaro data format.
*     12 Mar 1990 (ks):
*        Now uses DSA__CREATE_DATA_EXTRA rather than using
*        DSA__CREATE_EXTRA.
*     05 Apr 1991 (ks):
*        Now makes sure existing string items are long enough for the
*        strings to be written to them.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     10 Sep 1992 (hme):
*        There seems to be a problem for DAT_PUT (in DTA_WRVAR) to
*        convert a _DOUBLE into an _INTEGER. This shows up in %%CONV
*        when it tries to set the magnitude flag. Here we try to
*        circumvent the problem by using DTA_WRVARI.
*     24 Jan 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     1996 July 19 (MJC):
*        Fixed bug where a locator to an existing FIGARO extension was
*        not obtained before being used.
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
      CHARACTER * ( * ) CITEMS( NCITEM )
      INTEGER NNITEM
      DOUBLE PRECISION  NITEMS( NNITEM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL THERE              ! Whether an HDS component exists
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( DAT__SZLOC ) TLOC ! HDS locators

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
      IF ( NCITEM .GE. 1 ) CALL NDF_CPUT( CITEMS(1), DSA__REFID1(SLOT),
     :      'UNITS', STATUS )

*  Put the label string.
      IF ( NCITEM .GE. 2 ) CALL NDF_CPUT( CITEMS(2), DSA__REFID1(SLOT),
     :      'LABEL', STATUS )

*  Put the magnitude flag.  Create a Figaro extension if one does not
*  exist.  Get an HDS locator to the extension.
      IF ( NNITEM .GE. 1 ) THEN
         CALL NDF_XSTAT( DSA__REFID1(SLOT), 'FIGARO', THERE, STATUS )
         IF ( THERE ) THEN
            CALL NDF_XLOC( DSA__REFID1(SLOT), 'FIGARO', 'UPDATE',
     :         TLOC, STATUS )
         ELSE
            CALL NDF_XNEW( DSA__REFID1(SLOT), 'FIGARO', 'FIGARO_EXT',
     :         0, 0, TLOC, STATUS )
         END IF
         CALL DAT_THERE( TLOC, 'MAGFLAG', THERE, STATUS )
         IF ( .NOT. THERE ) CALL DAT_NEW( TLOC,
     :      'MAGFLAG', '_INTEGER', 0, 0, STATUS )
         CALL CMP_PUT0I( TLOC, 'MAGFLAG', INT(NITEMS(1)), STATUS )
         CALL DAT_ANNUL( TLOC, STATUS )
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
