      SUBROUTINE DSA_COERCE_DATA_ARRAY( DSAREF, TYPE,
     :   NDIM, DIMS, STATUS )
*+
*  Name:
*     DSA_COERCE_DATA_ARRAY

*  Purpose:
*     Force the existence of a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_COERCE_DATA_ARRAY( DSAREF, TYPE, NDIM, DIMS, STATUS )

*  Description:
*     This routine creates a new data array of specified type and size.
*     Note that this can be used to create a data array in a structure
*     that does not at present have such an array. If the array does
*     exist, it should not be mapped at the time this call is made. The
*     data in an existing array are maintained.
*
*     Contrary to earlier implementations, the old data are re-ordered
*     according to the changes in shape. New areas are filled with the
*     bad value.
*
*     The lower NDF bounds are not changed to 1, but left at their old
*     values, which should make the re-ordering more useful. The
*     one-dimensional axis components are re-shaped in accordance with
*     the change of the data shape, but N-dimensional axis arrays are
*     not reshaped and require a call to DSA_COERCE_AXIS_DATA or
*     DSA_RESHAPE_AXIS after the call to this routine.
*
*     Other NDF array components (variance and quality) are re-shaped
*     along with the data component, but their types remain unchanged.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type that the data array is to have. This must be one of
*        the primitive types recognised by the DSA_ routines. (Usually,
*        this will be 'FLOAT' or 'DOUBLE').
*     NDIM = INTEGER (Given)
*        The number of dimensions the array is to have.
*     DIMS( NDIM ) = INTEGER (Given)
*        The dimensions the array is to have.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     24 Aug 1987 (ks):
*        Original version.
*     04 Oct 1989 (ks):
*        Now reworked as little more than a call to DSA_COERCE_ARRAY.
*        Comments revised to indicate that data not lost except on type
*        change.
*     18 Jan 1990 (ks):
*        Now uses DSA__ routines to get data structure details instead
*        of assuming the original Figaro format.
*     23 Apr 1990 (ks):
*        Dimensioning of DIMS changed to prevent adjustable array
*        exception if called with NDIM=0.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     23 Aug 1992 (ks):
*        Remove unused variable declarations.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     01 Feb 1996 (hme):
*        FDA library.
*     12 Feb 1996 (hme):
*        Mark reference as data-reshaped.
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
      CHARACTER * ( * ) TYPE
      INTEGER NDIM
      INTEGER DIMS( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER SLOT               ! The reference slot
      INTEGER ONE( NDF__MXDIM )  ! An NDF lower bound
      INTEGER LBND( NDF__MXDIM ) ! The NDF lower bound
      INTEGER UBND( NDF__MXDIM ) ! The NDF upper bound
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! TYPE in NDF speak

*  Local data:
      DATA ONE / NDF__MXDIM * 1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot and convert type specification.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_NDFTYP( TYPE, NDFTYP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If reference has only a placeholder.
      IF ( DSA__REFID1(SLOT) .EQ. NDF__NOID ) THEN

*     Create the NDF with given type and shape. Lower bounds are 1.
         CALL NDF_NEW( NDFTYP, NDIM, ONE, DIMS, DSA__REFPLC(SLOT),
     :      DSA__REFID1(SLOT), STATUS )

*  Else (reference has an NDF identifier).
      ELSE

*     Find the current lower bounds.
         CALL NDF_BOUND( DSA__REFID1(SLOT), NDF__MXDIM,
     :      LBND, UBND, I, STATUS )

*     Work out the new upper bounds.
         DO 1 I = 1, NDIM
            UBND(I) = LBND(I) - 1 + DIMS(I)
 1       CONTINUE

*     Set the type and bounds.
         CALL NDF_STYPE( NDFTYP, DSA__REFID1(SLOT), 'DATA', STATUS )
         CALL NDF_SBND( NDIM, LBND, UBND, DSA__REFID1(SLOT), STATUS )

      END IF

*  Set the flag indicating that the NDF has been re-shaped.
      DSA__REFRSD(SLOT) = .TRUE.

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
