      SUBROUTINE DSA_SIMPLE_OUTPUT( DSAREF, CLIST, TYPE,
     :   NDIM, DIMS, STATUS )
*+
*  Name:
*     DSA_SIMPLE_OUTPUT

*  Purpose:
*     Create a minimal output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SIMPLE_OUTPUT( DSAREF, CLIST, TYPE, NDIM, DIMS, STATUS )

*  Description:
*     This routine builds a minimal output NDF. It is passed the
*     reference name of what should be an empty NDF - that is one
*     created by DSA_OUTPUT (or equivalent), with no basis NDF
*     specified, and creates the NDF with the specified shape.
*
*     There is not any longer any more to this routine. Since NDF
*     components are created by accessing them, the item list is
*     ignored.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The DSA reference name for the output NDF.
*     CLIST = CHARACTER * ( * ) (Given)
*        Ignored.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the NDF, like 'FLOAT', 'DOUBLE' etc.
*     NDIM = INTEGER (Given)
*        The dimensionality of the NDF.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions of the NDF. The lower bounds of the NDF will
*        be set to 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     05 Jul 1988 (ks):
*        Original version.
*     25 Jul 1988 (ks):
*        Default type for QUALITY made BYTE.
*     01 Sep 1988 (ks):
*        Default axis dimensions now 1D, as intended.
*     16 Jan 1990 (ks):
*        Modified to use DSA__ routines to handle structure details,
*        rather than just assuming the original Figaro data format.
*        `VARIANCE' also allowed.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     25 Nov 1995 (hme):
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
      CHARACTER * ( * ) CLIST
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIMS( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot
      INTEGER ONE( NDF__MXDIM )  ! The NDF lower bound
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! TYPE in NDF speak

*  Local data:
      DATA ONE / NDF__MXDIM * 1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Process the type spec.
      CALL DSA1_NDFTYP( TYPE, NDFTYP, STATUS )

*  Find the reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Take the placeholder and create the new NDF with the given shape.
      CALL NDF_NEW( NDFTYP, NDIM, ONE, DIMS, DSA__REFPLC(SLOT),
     :   DSA__REFID1(SLOT), STATUS )

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
