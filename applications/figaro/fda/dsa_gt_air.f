      SUBROUTINE DSA_GET_AIRMASS( DSAREF, WHEN, AIRMASS, STATUS )
*+
*  Name:
*     DSA_GET_AIRMASS

*  Purpose:
*     Return the airmass value associated with an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_AIRMASS( DSAREF, WHEN, AIRMASS, STATUS )

*  Description:
*     Some NDFs will have an airmass value associated
*     with them. This routine returns that value. In fact the data
*     structures will have the number sec(z) stored, where z is the
*     zenith distance. The returned airmass is calculated as
*
*        A = - 0.0002067
*            + 1.00147   * sec(z)
*            - 0.0004501 * sec(z) * sec(z)
*            - 0.0008083 * sec(z) * sec(z) * sec(z)
*
*     This polynomial conversion from sec(z) to airmass is lifted from
*     TYB's Forth code. It isn't clear where he got the coefficients,
*     but the results check with the table given in section 60 of Allen.
*     (It seems to be quibbling to include this when we ignore the Start
*     and End distinction!)

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     WHEN = CHARACTER * ( * ) (Given)
*        Ignored.
*     AIRMASS = REAL (Returned)
*        The airmass value associated with the structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1987 (ks):
*        Original version.
*     12 Mar 1990 (ks):
*        Now uses DSA__ routines rather than just assuming the original
*        Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     02 Feb 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     21 Dec 2000 (acd):
*        Removed unused variables.
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
      CHARACTER * ( * ) WHEN

*  Arguments Returned:
      REAL AIRMASS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot
      REAL SECZ                  ! The sec(z) value
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

*  Get the value, apply the polynomial.
*  (We leave the error reporting to NDF and HDS.)
*  (There used to be a check that sec(z) >= 0, but we leave that out.)
      CALL NDF_XLOC( DSA__REFID1(SLOT), 'FIGARO', 'READ',
     :   TLOC, STATUS )
      CALL CMP_GET0R( TLOC, 'SECZ', SECZ, STATUS )
      IF ( STATUS .EQ. SAI__OK ) AIRMASS = -2.067E-4
     :   + 1.00147 * SECZ - 4.501E-4 * SECZ * SECZ
     :   - 8.083E-4 * SECZ * SECZ * SECZ
      CALL DAT_ANNUL( TLOC, STATUS )

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
