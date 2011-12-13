      SUBROUTINE SUBPAR_DATFIND ( TOPLOC, COMPONENT, BOTLOC, STATUS )
*+
*  Name:
*     SUBPAR_DATFIND

*  Purpose:
*     Find locator to an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DATFIND ( TOPLOC, COMPONENT, BOTLOC, STATUS )

*  Description:
*     Find the locator to an HDS component, including the case when it
*     is an array element.
*     If the routine fails, nullify BOTLOC.

*  Arguments:
*     TOPLOC=CHARACTER*(*) (given)
*        locator above required component
*     COMPONENT=CHARACTER*(*) (given)
*        name of the required component
*     BOTLOC=CHARACTER*(*) (returned)
*        locator to named component
*     STATUS=INTEGER

*  Algorithm:
*     Check whether an array component is specified. If it is, get a
*     locator to the array and then use DAT_CELL or DAT_SLICE.
*     Otherwise, get a locator.

*  Copyright:
*     Copyright (C) 1985, 1987, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     09-MAY-1985 (BDK):
*        Original
*     13-MAY-1987 (BDK):
*        handle slices of arrays
*     01-FEB-1990 (AJC):
*        nullify bad locators
*     22-JUL-1991 (AJC):
*        remove unused declaration ISTAT
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'


*  Arguments Given:
      CHARACTER*(*) TOPLOC            ! locator to an object

      CHARACTER*(*) COMPONENT         ! the component name


*  Arguments Returned:
      CHARACTER*(*) BOTLOC            ! locator to object


*  Status:
      INTEGER STATUS


*  Local Variables:
      LOGICAL STRUCARR                ! .TRUE. => an array element
                                      ! .FALSE. => a scalar

      LOGICAL SLICE                   ! .TRUE. => slice of an array element
                                      ! .FALSE. => otherwise

      CHARACTER*(DAT__SZNAM) NAME     ! the name of the component with any
                                      ! dimensional information removed.

      INTEGER NDIMS                   ! number of dimensions of the component

      INTEGER STARTS(DAT__MXDIM)      ! starts of dimensions specified

      INTEGER ENDS(DAT__MXDIM)        ! ends of dimensions specified

      CHARACTER*(DAT__SZLOC) TEMPLOC  ! locator to array object

*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*   Guard against hanging locators
      BOTLOC = ' '
      TEMPLOC = ' '
*
*   Check for array object
*
      CALL SUBPAR_HDSARR ( COMPONENT, STRUCARR, SLICE, NAME, NDIMS,
     :  STARTS, ENDS, STATUS )

      IF ( STRUCARR ) THEN
*
*      Get a locator to the array, then to the element/slice. Discard the
*      locator to the array.
*
         CALL DAT_FIND ( TOPLOC, NAME, TEMPLOC, STATUS )
         IF ( SLICE ) THEN
            CALL DAT_SLICE ( TEMPLOC, NDIMS, STARTS, ENDS, BOTLOC,
     :        STATUS )
         ELSE
            CALL DAT_CELL ( TEMPLOC, NDIMS, ENDS, BOTLOC, STATUS )
         ENDIF
         CALL DAT_ANNUL ( TEMPLOC, STATUS )

      ELSE
*
*      Scalar object. Return the locator to it.
*
         CALL DAT_FIND ( TOPLOC, COMPONENT, BOTLOC, STATUS )

      ENDIF

      IF (STATUS .NE. SAI__OK) THEN
*      Clean up if error
         CALL DAT_ANNUL( BOTLOC, STATUS)
         BOTLOC = ' '
      ENDIF

      END
