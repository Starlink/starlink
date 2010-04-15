      SUBROUTINE DSA_AXIS_SIZE( DSAREF, AXIS, MAXDIM,
     :   NDIM, DIMS, NELM, STATUS )
*+
*  Name:
*     DSA_AXIS_SIZE

*  Purpose:
*     Return the dimensions of an axis centre array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_AXIS_SIZE( DSAREF, AXIS, MAXDIM,
*        NDIM, DIMS, NELM, STATUS )

*  Description:
*     This routine returns the dimensions and total number of elements
*     in a specified axis centre array. If the axis data does not in
*     fact exist, this routine returns as if it were a 1D array of the
*     same size as the corresponding dimension of the main NDF itself.
*     This behaviour is in line with that of DSA_MAP_AXIS_DATA.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis in question.
*     MAXDIM = INTEGER (Given)
*        The maximum number of dimensions for the data.
*     NDIM = INTEGER (Returned)
*        The actual number of dimensions in the data.
*     DIMS( MAXDIM ) = INTEGER (Returned)
*        The number of elements in each axis of the data. Elements
*        DIMS(NDIM+1) to DIMS(MAXDIM), if any, are set to 1.
*     NELM = INTEGER (Returned)
*        The total number of elements in the data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     15 Jun 1987 (ks):
*        Original version.
*     08 Jul 1988 (ks):
*        Add code to test for case AXIS>data dimensions.
*     08 Dec 1989 (ks):
*        Set unused elements of DIMS to 1.
*     19 Jan 1990 (ks):
*        Use DSA__ routines to get structure details rather than assume
*        original Figaro structure.
*     21 Dec 1990 (ks):
*        Wasn't setting high elements of DIMS to 1 if axis array doesn't
*        exist.  Fixed.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     31 Jan 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Move the real action to internal routine DSA1_AXSIZ.
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
      INTEGER AXIS
      INTEGER MAXDIM

*  Arguments Returned:
      INTEGER NDIM
      INTEGER DIMS( MAXDIM )
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! Reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up the reference name in the tables and call the working routine.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_AXSIZ( SLOT, AXIS, MAXDIM, NDIM, DIMS, NELM, STATUS )

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



      SUBROUTINE DSA1_AXSIZ( SLOT, AXIS, MAXDIM,
     :   NDIM, DIMS, NELM, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLOT
      INTEGER AXIS
      INTEGER MAXDIM

*  Arguments Returned:
      INTEGER NDIM
      INTEGER DIMS( MAXDIM )
      INTEGER NELM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL AXISND             ! True if N-D centre array
      INTEGER I                  ! Loop index
      INTEGER TNDIM              ! NDF dimensionality
      INTEGER TDIM( NDF__MXDIM ) ! Dimensions of NDF
      CHARACTER * ( DAT__SZNAM ) DELNAM ! Ignored
      CHARACTER * ( DAT__SZLOC ) DELLOC ! Ignored
      CHARACTER * ( DAT__SZLOC ) ARYLOC ! Array locator

*.

*  Return immediately on bad status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the validity of the value of AXIS.
      CALL NDF_DIM( DSA__REFID1(SLOT), NDF__MXDIM, TDIM, TNDIM, STATUS )
      IF ( AXIS .LT. 1 .OR. AXIS .GT. TNDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'DSA_AXNO', AXIS )
         CALL MSG_SETC( 'DSA_DSAREF', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'DSA_AXINV', 'DSA_AXIS_SIZE: Invalid axis ' //
     :      'number ^DSA_AXNO for reference ^DSA_DSAREF.', STATUS )
         GO TO 500
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Check for and locate N-D centre array.
      CALL DSA1_AXISND( SLOT, AXIS,
     :   AXISND, ARYLOC, DELLOC, DELNAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If N-D centre array exists.
      IF ( AXISND ) THEN

*     Enquire shape and size of N-D axis centre array.
         DO 1 I = 1, MAXDIM
            DIMS(I) = 1
 1       CONTINUE
         CALL DAT_SHAPE( ARYLOC, MAXDIM, DIMS, NDIM, STATUS )
         CALL DAT_SIZE(  ARYLOC, NELM, STATUS )
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )

*  Else (axis is 1-D).
      ELSE

*     Return relevant dimension of the main NDF as shape of the axis.
         NDIM = 1
         NELM = TDIM(AXIS)
         DIMS(1) = NELM
         DO 2 I = 2, MAXDIM
            DIMS(I) = 1
 2       CONTINUE

      END IF

*  Exit.
  500 CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( ARYLOC, STATUS )
         CALL DAT_ANNUL( DELLOC, STATUS )
      END IF
      END
