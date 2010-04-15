      SUBROUTINE DSA_SET_WIDTH( DSAREF, AXIS, WIDTH, STATUS )
*+
*  Name:
*     DSA_SET_WIDTH

*  Purpose:
*     Set an axis width array to a constant value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SET_WIDTH( DSAREF, AXIS, WIDTH, STATUS )

*  Description:
*     If a data structure contains, or is to contain, a constant width
*     value, as opposed to a width array, then this routine can be used
*     to set such a value. In fact, the width array will be filled with
*     the value.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        The number of the axis in question.
*     WIDTH = DOUBLE PRECISION (Given)
*        The value to be used to fill the width array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24 Mar 1991 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     20 Feb 1996 (hme):
*        FDA library.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      INTEGER AXIS
      DOUBLE PRECISION WIDTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot
      INTEGER MSLOT              ! A map slot
      INTEGER NELM               ! Array size
      INTEGER NDIM               ! Ignored
      INTEGER DIM( NDF__MXDIM )  ! Ignored
      INTEGER ADDRESS            ! Array pointer

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )

*  Find out axis array size.
      CALL DSA1_AXSIZ( SLOT, AXIS, NDF__MXDIM, NDIM, DIM, NELM, STATUS )

*  Map the width array for write access and with type DOUBLE.
      CALL DSA1_MAPWID( SLOT, AXIS, 'WRITE', 'DOUBLE',
     :   ADDRESS, MSLOT, STATUS )

*  Fill the array with the constant value.
      IF ( STATUS .EQ. SAI__OK )
     :   CALL DSA2_CFILLD( NELM, %VAL( CNF_PVAL(ADDRESS) ), WIDTH )

*  Unmap the width array.
      CALL DSA1_UNMAP( MSLOT, STATUS )

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



      SUBROUTINE DSA2_CFILLD( NELM, ARRAY, VALUE )

      IMPLICIT NONE

      INTEGER NELM
      DOUBLE PRECISION ARRAY( NELM )
      DOUBLE PRECISION VALUE

      INTEGER I

      DO 1 I = 1, NELM
         ARRAY(I) = VALUE
 1    CONTINUE

      END
