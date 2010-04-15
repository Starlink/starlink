      SUBROUTINE DSA_DATA_SIZE( DSAREF, MAXDIM,
     :   NDIM, DIMS, NELM, STATUS )
*+
*  Name:
*     DSA_DATA_SIZE

*  Purpose:
*     Return the dimensions of the data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_DATA_SIZE( DSAREF, MAXDIM, NDIM, DIMS, NELM, STATUS )

*  Description:
*     This routine returns the dimensions and total number of elements
*     in the main data array of an NDF.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     MAXDIM = INTEGER (Given)
*        The maximum allowable dimensionality. The calling routine can
*        use this to force an error condition if the NDF has a higher
*        dimensionality than the calling routine can handle.
*     NDIM = INTEGER (Returned)
*        The actual dimensionality of the NDF.
*     DIMS( MAXDIM ) = INTEGER (Returned)
*        The dimensions of the NDF, i.e. the lengths along each axis
*        or dimension.
*     NELM = INTEGER (Returned)
*        The total size of the NDF, i.e. the product of all dimensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     15 Jun 1987 (ks):
*        Original version.
*     02 Feb 1989 (ks):
*        Now ensures unused DIMS elements are set to 1.
*     08 Dec 1989 (ks):
*        Comments reformatted to avoid problems when
*        processed using MAN.
*     15 Apr 1990 (ks):
*        Call to DSA_ARRAY_EXIST added to improve error
*        messages if structure is invalid.
*     23 Apr 1990 (ks):
*        Above call modified to allow for possibility that
*        the ref-name refers directly to an array.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     25 Nov 1995 (hme):
*        FDA library.
*     15 Dec 1995 (hme):
*        Safe return values if something goes wrong.
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
      INTEGER MAXDIM

*  Arguments Returned:
      INTEGER NDIM
      INTEGER DIMS( MAXDIM )
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER SLOT               ! The reference slot

*.

*  Set safe values.
      NELM = 1
      NDIM = 1
      DO 1 I = 1, MAXDIM
         DIMS(I) = 1
 1    CONTINUE

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find slot, NDF size, NDF dimensions.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL NDF_SIZE( DSA__REFID1(SLOT), NELM, STATUS )
      CALL NDF_DIM(  DSA__REFID1(SLOT), MAXDIM, DIMS, NDIM, STATUS )

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
