      SUBROUTINE SPD_WZUA( FILENO, THREEF, VXIST, NX,
     :   X1, X2, Y, V1, V2, V3, STATUS )
*+
*  Name:
*     SPD_WZUA

*  Purpose:
*     Write data for CORREL.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZUA( FILENO, THREEF, VXIST, NX, X1, X2, Y, V1, V2, V3,
*        STATUS )

*  Description:
*     This routine is designed to be called by CORREL and to write the
*     three data sets and their errors into a table with six columns.

*  Arguments:
*     FILENO = INTEGER (Given)
*        Fortran unit number for output file.
*     THREEF = LOGICAL (Given)
*        True if there are three data files, false if there are only
*        two.
*     VXIST( 3 ) = LOGICAL (Given)
*        True if V1, V2, V3 exist.
*     NX = INTEGER (Given)
*        Size of the arrays.
*     X1( NX ) = REAL (Given)
*        First data array.
*     X2( NX ) = REAL (Given)
*        Second data array.
*     Y( NX ) = REAL (Given)
*        Third data array.
*     V1( NX ) = REAL (Given)
*        First variance array.
*     V2( NX ) = REAL (Given)
*        Second variance array.
*     V3( NX ) = REAL (Given)
*        Third variance array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Status:
*     This routine does not skip data points with an existing variance
*     = 0. It skips only data points with any existing value bad.

*  Authors:
*     hme: Horst Meyerdierks (Starlink, UoE)
*     {enter_new_authors_here}

*  History:
*     13 Jul 1991 (hme):
*        Original version.
*     25 Jan 1995 (hme):
*        Renamed from CORRWR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER FILENO
      LOGICAL THREEF
      LOGICAL VXIST( 3 )
      INTEGER NX
      REAL X1( NX )
      REAL X2( NX )
      REAL Y( NX )
      REAL V1( NX )
      REAL V2( NX )
      REAL V3( NX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      REAL LX1
      REAL LX2
      REAL LY
      REAL LV1
      REAL LV2
      REAL LV3

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The format used.
 101  FORMAT ( '  ', 6(' ',G14.7E2) )

*  Default local copies.
      LY = 0.
      LV1 = 0.
      LV2 = 0.
      LV3 = 0.

*  Loop through data.
      DO 1 I = 1, NX

*     Local copy of array elements, obeying VXIST.
         LX1 = X1(I)
         LX2 = X2(I)
         IF ( THREEF )   LY  =  Y(I)
         IF ( VXIST(1) ) LV1 = V1(I)
         IF ( VXIST(2) ) LV2 = V2(I)
         IF ( VXIST(3) ) LV2 = V3(I)

*     Write values.
         IF ( LX1 .NE. VAL__BADR .AND. LX2 .NE. VAL__BADR .AND.
     :         LY .NE. VAL__BADR .AND. LV1 .NE. VAL__BADR .AND.
     :        LV2 .NE. VAL__BADR .AND. LV3 .NE. VAL__BADR )
     :      WRITE( FILENO, 101 ) LX1, LX2, LY,
     :         SQRT(LV1), SQRT(LV2), SQRT(LV3)
 1    CONTINUE

      END
