************************************************************************

      SUBROUTINE AGI_1WCACH( WKNAME, PICNUM, PNAME, COMENT, DEVICE, NDC,
     :                       WORLD, MEMID, POINT, STATUS )

*+
*  Name:
*     AGI_1WCACH

*  Purpose:
*     Write the contents of a picture into the cache.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1WCACH( WKNAME, PICNUM, PNAME, COMENT, DEVICE, NDC,

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Write the contents of a picture into the dynamic cache

*  Algorithm:
*     Check status on entry.
*     Update the cache pointer.
*     Delete any compiled transformations at the new pointer.
*     Write the picture contents into the cache.
*     Indicate that the transformations have not been compiled.

*  Copyright:
*     Copyright (C) 1988, 1989, 1990 Science & Engineering Research Council.
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
*     Nick Eaton  ( DUVAD::NE )
*     {enter_new_authors_here}

*  History:
*     July 1988
*     June 1989  Amended to allow for the extra FIFO's
*     Sept 1989  Annul compiled transforms in the cache
*     June 1990  Added MEMID parameter
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'


*  Arguments Given:
*     Name of workstation
      CHARACTER * ( * ) WKNAME

*     Picture number
      INTEGER PICNUM

*     Picture name
      CHARACTER * ( * ) PNAME

*     Picture description
      CHARACTER * ( * ) COMENT

*     Device coordinates of picture
      REAL DEVICE( 4 )

*     Normalised device coordinates of picture
      REAL NDC( 4 )

*     World coordinates of picture
      REAL WORLD( 4 )

*     Memory identifier
      INTEGER MEMID


*  Arguments Returned:
*     Cache pointer to current fifo entry
      INTEGER POINT


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_cache'


*  Local Variables:
      INTEGER I, J, K

      CHARACTER * ( AGI__SZNAM ) TPNAME

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Obtain the FIFO number by hashing the picture number
         K = MOD( PICNUM, NFIFO )

*   Update the cache pointer
         J = MOD( PFIFO( K ) + 1, FIFLEN )
         PFIFO( K ) = J

*   Delete any compiled transformations
         IF ( CTRFOR( J, K ) .NE. 0 ) THEN
            CALL TRN_ANNUL( CTRFOR( J, K ), STATUS )
         ENDIF
         IF ( CTRINV( J, K ) .NE. 0 ) THEN
            CALL TRN_ANNUL( CTRINV( J, K ), STATUS )
         ENDIF

*   Convert the name to uppercase
         TPNAME = PNAME
         CALL CHR_UCASE( TPNAME )

*   Write the new picture into the cache
         FIFO( J, K ) = PICNUM
         CWKNAM( J, K ) = WKNAME
         CPNAME( J, K ) = TPNAME
         CCOM( J, K ) = COMENT
         CMEMID( J, K ) = MEMID

         DO I = 1, 4
            CDEV( I, J, K ) = DEVICE( I )
            CNDC( I, J, K ) = NDC( I )
            CWORLD( I, J, K ) = WORLD( I )
         ENDDO

*   Indicate that the transformations have not been compiled
         CTRFOR( J, K ) = 0
         CTRINV( J, K ) = 0

*   Return the current cache pointer
         POINT = J

      ENDIF

      END

