      SUBROUTINE DSA_SET_RANGE( DSAREF, VMIN, VMAX, STATUS )
*+
*  Name:
*     DSA_SET_RANGE

*  Purpose:
*     Record the max and min values for the data array of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SET_RANGE( DSAREF, VMIN, VMAX, STATUS )

*  Description:
*     This routine would set information on the range of data
*     values in the NDF referred to by the DSA reference name. Since
*     there never is such information (in this implementation), the
*     routine ignores the given values and does nothing.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     VMIN = REAL (Given)
*        Ignored.
*     VMAX = REAL (Given)
*        Ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     30 Jul 1987 (ks):
*        Original version.
*     28 Feb 1990 (ks):
*        Modified to use DSA__ routines to remove assumption that file
*        is in original Figaro format.
*     12 Mar 1990 (ks):
*        Now uses DSA__CREATE_DATA_EXTRA rather than using DSA_CREATE_EXTRA.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     05 Oct 1992 (hme):
*        DSA_REF_SLOT was called with an empty and an extra argument
*        (...,,...).
*     21 Feb 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      REAL VMIN
      REAL VMAX

*  Status:
      INTEGER STATUS             ! Global status

*.

      END
