      SUBROUTINE NDF1_QITYP( DTYPE, ITYPE, OK, STATUS )
*+
*  Name:
*     NDF1_QITYP

*  Purpose:
*     Query if a data type matches an implemented algorithm.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QITYP( DTYPE, ITYPE, OK, STATUS )

*  Description:
*     The routine compares a numeric data type with the data type of an
*     algorithm which may be used to process it and returns a logical
*     value indicating if the data can be processed by the algorithm
*     without loss of precision.

*  Arguments:
*     DTYPE = INTEGER (Given)
*        An integer identifying the numeric type of the data to be
*        processed. One of the symbolic integers NDF__TYPx (where x is
*        UB, B, UW, W, I, K, R or D) should be used. These values are
*        defined in the NDF_CONST include file.
*     ITYPE = INTEGER (Given)
*        A similar symbolic integer identifying the data type which the
*        algorithm is designed to process.
*     OK = LOGICAL (Returned)
*        The value .TRUE. is returned if there will be no loss of
*        precision in converting the data type identified by DTYPE into
*        that identified by ITYPE and then processing the data.
*        Otherwise, the value .FALSE. is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The implementation of this routine depends on the collating
*     sequence of data types established by the symbolic constants used
*     to identify them.

*  Algorithm:
*     -  Compare the DTYPE data type code with each permitted value in
*     turn, performing the appropriate test on the ITYPE code.
*     -  If the DTYPE value was not recognised, then report an error.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-NOV-1989 (RFWS):
*        Original version.
*     17-JAN-1990 (RFWS):
*        Fixed bug in handling of unsigned word type.
*     1-MAR-1990 (RFWS):
*        Removed un-referenced include file.
*     2012-05-09 (TIMJ):
*        K type should not use _REAL as a fallback but go straight
*        to _DOUBLE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER DTYPE
      INTEGER ITYPE

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Compare the DTYPE data type code with each permitted value in turn,
*  performing the appropriate test on the ITYPE code.

*  ...unsigned byte.
      IF ( DTYPE .EQ. NDF__TYPUB ) THEN
         OK = ( ITYPE .EQ. NDF__TYPUB ) .OR. ( ITYPE .GE. NDF__TYPUW )

*  ...byte.
      ELSE IF ( DTYPE .EQ. NDF__TYPB ) THEN
         OK = ( ITYPE .EQ. NDF__TYPB ) .OR. ( ITYPE .GE. NDF__TYPW )

*  ...unsigned word.
      ELSE IF ( DTYPE .EQ. NDF__TYPUW ) THEN
         OK = ( ITYPE .EQ. NDF__TYPUW ) .OR. ( ITYPE .GE. NDF__TYPI )

*  ...word.
      ELSE IF ( DTYPE .EQ. NDF__TYPW ) THEN
         OK = ITYPE .GE. NDF__TYPW

*  ...integer.
      ELSE IF ( DTYPE .EQ. NDF__TYPI ) THEN
         OK = ITYPE .GE. NDF__TYPI

*  ...64-bit integer.
      ELSE IF ( DTYPE .EQ. NDF__TYPK ) THEN
         OK = (ITYPE .EQ. NDF__TYPK) .OR. ( ITYPE .GE. NDF__TYPD )

*  ...real.
      ELSE IF ( DTYPE .EQ. NDF__TYPR ) THEN
         OK = ITYPE .GE. NDF__TYPR

*  ...double precision.
      ELSE IF ( DTYPE .EQ. NDF__TYPD ) THEN
         OK = ITYPE .GE. NDF__TYPD

*  If the DTYPE data type code was not recognised, then report an error.
      ELSE
         STATUS = NDF__FATIN
         CALL MSG_SETC( 'ROUTINE', 'NDF1_QITYP' )
         CALL MSG_SETI( 'BADDTYPE', DTYPE )
         CALL ERR_REP( 'NDF1_QITYP_BAD',
     :   'Routine ^ROUTINE called with an invalid DTYPE argument of ' //
     :   '^BADDTYPE (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QITYP', STATUS )

      END
