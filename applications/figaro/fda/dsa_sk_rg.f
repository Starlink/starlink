      SUBROUTINE DSA_SEEK_RANGE( DSAREF, EXIST, STATUS )
*+
*  Name:
*     DSA_SEEK_RANGE

*  Purpose:
*     Determine whether valid max and min values are known.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_RANGE( DSAREF, EXIST, STATUS )

*  Description:
*     This routine determines whether there is valid information
*     available on the range of data values in the NDF referred to by
*     the DSA reference name. Since there never is such information (in
*     this implementation), the reply is always false.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     EXIST = LOGICAL (Returned)
*        Always false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     30 Jul 1987 (ks):
*        Original version.
*     01 Mar 1990 (ks):
*        Modified to use DSA__ routines instead of assuming the original
*        Figaro data format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
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

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF ( STATUS .NE. 0 ) RETURN

      EXIST = .FALSE.

      END
