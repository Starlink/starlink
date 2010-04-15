      SUBROUTINE DSA_INPUT( DSAREF, PARAM, STATUS )
*+
*  Name:
*     DSA_INPUT

*  Purpose:
*     Open an existing NDF for read access, using a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_INPUT( DSAREF, PARAM, STATUS )

*  Description:
*     This routine takes the name of a parameter and gets its value
*     from the parameter system. The result should be the name of an
*     NDF. The NDF is then opened and associated with the given
*     reference name.
*
*     Contrary to earlier implementations, this routine gives only read
*     access to the NDF in question.
*
*     The NDF name can be a filename, a filename combined with an HDS
*     structure name within the file, and it can include an NDF section
*     specification. A full example might be
*
*        myfile.more.some.struct(5).data(:50,2.5:3.5,7,29.0)

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name to be associated with the opened NDF.
*        Only the first 16 characters are significant. The
*        name is case-insensitive.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter giving the name of the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     15 Jun 1987 (ks):
*        Original version.
*     05 Sep 1988 (ks):
*        Modified to use PAR_ABORT and the new '/NOCHECK' facility of
*        PAR_RDCHAR.
*     09 Sep 1988 (ks):
*        Now sets parameter value used in common.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     24 Nov 1995 (hme):
*        FDA library.
*     15 Dec 1995 (hme):
*        Use DSA1_RDNAM to get NDF name.
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

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) PARAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 256 ) NDFNAM ! Name of NDF to be opened

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Get the parameter value.
      CALL DSA1_RDNAM( PARAM, NDFNAM, STATUS )

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Open the named NDF.
      CALL DSA_NAMED_INPUT( DSAREF, NDFNAM, STATUS )

*  Return.
      END
