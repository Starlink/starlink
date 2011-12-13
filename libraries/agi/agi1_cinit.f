************************************************************************

      SUBROUTINE AGI_1CINIT ( STATUS )

*+
*  Name:
*     AGI_1CINIT

*  Purpose:
*     Initialise the AGI common blocks.

*  Language:
*     VAX Fortran

*  Type of Module:
*     Subroutine

*  Invocation:
*     CALL AGI_1CINIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Initialise the AGI common blocks which contain the references to
*     the picture identifiers. The definitions are held in the INCLUDE
*     file 'AGI_PFREE'.

*  Algorithm:
*     Check status on entry.
*     Set the variables to zero of blank strings.

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
*     Aug 1988
*     Dec 1989  Added CIDIID
*     Jul 1990  Added CROOT
*     Oct 1990  Added picture deferral
*     Nov 1990  Added nest level and clear flags
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


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'agi_pfree'


*  Local Variables:
      INTEGER I

*.


      IF ( STATUS .EQ. SAI__OK ) THEN

*   Clear out all the common block entries, but not the free list.
         DO I = 1, FRELEN
            CGRAWK( I ) = ' '
            CAGIWK( I ) = ' '
            PTNAME( I ) = ' '
            CPICNM( I ) = 0
            CIDIID( I ) = 0
            CLEVEL( I ) = 0
            CDEPS( I ) = 0
         ENDDO

*   Initialise the clear flags
         DO I = 1, CLRLEN
            CLEARF( I ) = 0
         ENDDO

*   Initialise other flags
         CURPID = 0
         CROOT = 0
         CNUMID = 0
         CISDEP = .FALSE.

      ENDIF

      END

