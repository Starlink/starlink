      SUBROUTINE DSA_OUTPUT( DSAREF, PARAM, BASREF,
     :   NOCOPY, NEW, STATUS )
*+
*  Name:
*     DSA_OUTPUT

*  Purpose:
*     Open a new NDF or overwrite an old NDF using a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_OUPUT( DSAREF, PARAM, BASREF, NOCOPY, NEW, STATUS )

*  Description:
*     This routine takes the name of a parameter and gets its value
*     from the parameter system. The result should be the name of an
*     NDF. The NDF is then opened and associated with the given
*     reference name. Access to the structure will be for write, i.e.
*     any previous content of the NDF is lost. If the structure has
*     been previously opened then a temporary NDF will be used until
*     the final NDF can be written.
*
*     The NDF name can be a filename, or a filename combined with an HDS
*     structure name within the file. A full example might be
*
*        myfile.more.some.struct(5).data
*
*     If a model NDF is specified via the base DSA reference and NOCOPY
*     is 0, then the new NDF will be a copy of the model. Otherwise the
*     output NDF will be initially undefined.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name to be associated with the opened NDF.
*        Only the first 16 characters are significant. The
*        name is case-insensitive.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter giving the name of the NDF.
*     BASREF = CHARACTER * ( * ) (Given)
*        The reference name of an already open NDF of which
*        the new NDF should be a copy. Ignored, if NOCOPY is 1.
*     NOCOPY = INTEGER (Given)
*        If zero and BASREF is not blank, then the model NDF is
*        copied to the new NDF.
*     NEW = INTEGER (Given)
*        If zero overwriting a currently open NDF is to be allowed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     16 Jul 1987 (ks):
*        Original version.
*     05 Sep 1988 (ks):
*        Modified to use PAR_ABORT and the new '/NOCHECK' facility of
*        PAR_RDCHAR.
*     09 Sep 1988 (ks):
*        Now sets default to parameter value used for BASIS_NAME.
*     14 Feb 1989 (ks):
*        Comments revised.
*     02 Jan 1990 (ks):
*        No longer defaults file name if data not copied.
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
      CHARACTER * ( * ) BASREF
      INTEGER NOCOPY
      INTEGER NEW

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
      CALL DSA_NAMED_OUTPUT( DSAREF, NDFNAM, BASREF,
     :   NOCOPY, NEW, STATUS )

*  Return.
      END
