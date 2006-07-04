      SUBROUTINE ARY_FTYPE( IARY, FTYPE, STATUS )
*+
*  Name:
*     ARY_FTYPE

*  Purpose:
*     Obtain the full data type of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_FTYPE( IARY, FTYPE, STATUS )

*  Description:
*     The routine returns the full data type of an array as an
*     upper-case character string (e.g. '_REAL' or 'COMPLEX_BYTE').

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     FTYPE = CHARACTER * ( * ) (Returned)
*        Full data type of the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The symbolic constant ARY__SZFTP may be used for declaring the
*     length of a character variable to hold the full data type of an
*     array. This constant is defined in the include file ARY_PAR.
*     - For "Scaled" arrays, the data type returned by this function is
*     the data type of the SCALE and ZERO terms, rather than the data
*     type of the stored array.

*  Algorithm:
*     -  Import the array identifier.
*     -  Obtain the Data Control Block index for the data object.
*     -  Ensure that data type information is available for the data
*     object.
*     _  Construct the full data type string and copy it to the output
*     argument.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
*     All Rights Reserved.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     14-JUN-1989 (RFWS):
*        Original version.
*     8-MAY-2006 (DSB):
*        Add support for scaled arrays.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_CPX( ACB_MXDCB ) = LOGICAL (Read)
*           Whether data object is complex.
*        DCB_TYP( ACB_MXDCB ) = CHARACTER * ( DAT__SZTYP ) (Read)
*           Numeric data type string for data object.
*        DCB_FRM( ACB_MXDCB ) = CHARACTER * ( ARY__SZFRM ) (Read)
*           Storage form for array.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY

*  Arguments Returned:
      CHARACTER * ( * ) FTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER TY*(DAT__SZTYP)  ! Intermediate data type
      INTEGER IACB               ! Index to ACB entry
      INTEGER IDCB               ! Index to data object entry in the DCB
    
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the DCB index for the data object.
         IDCB = ACB_IDCB( IACB )

*  Ensure that storage form, data type and scale information is available.
         CALL ARY1_DFRM( IDCB, STATUS )
         CALL ARY1_DTYP( IDCB, STATUS )
         CALL ARY1_DSCL( IDCB, STATUS )

*  For scaled arrays, return the data type of the scale and zero terms. 
*  For other storage forms, return the data type of the DCB entry.
         IF( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN
            CALL CMP_TYPE( DCB_SCLOC( IDCB ), 'SCALE', TY, STATUS )
         ELSE
            TY = DCB_TYP( IDCB )
         END IF

*  Construct the full data type string and copy it to the output
*  argument.
         IF ( DCB_CPX( IDCB ) ) THEN
            CALL ARY1_CCPY( 'COMPLEX' // TY, FTYPE,
     :      STATUS )
         ELSE
            CALL ARY1_CCPY( TY, FTYPE, STATUS )
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_FTYPE_ERR',
     :   'ARY_FTYPE: Error obtaining the full data type of an array.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_FTYPE', STATUS )
      END IF

      END
