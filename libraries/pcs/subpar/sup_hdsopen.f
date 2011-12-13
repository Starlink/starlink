      SUBROUTINE SUBPAR_HDSOPEN ( FILENAME, COMPONENT, ACCESS, FILOC,
     :  BOTLOC, STATUS )
*+
*  Name:
*     SUBPAR_HDSOPEN

*  Purpose:
*     Open an HDS file and return the top-level locators.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_HDSOPEN ( FILENAME, COMPONENT, ACCESS, FILOC,

*  Description:
*     Open an HDS container file and return a locator to the top-level
*     object. If the object specified is an array element or slice, its
*     locator is returned also.
*     If the routine fails nullify (ie set to blank) the locators.
*     Retain the first stacked error message and add one for SUBPAR

*  Arguments:
*     FILENAME=CHARACTER*(*) (given)
*        name of HDS container file
*     COMPONENT=CHARACTER*(*) (given)
*        an HDS top-level component name
*     ACCESS=CHARACTER*(*) (given)
*        access mode - READ WRITE or UPDATE
*     FILOC=CHARACTER*(*) (returned)
*        locator to the container file
*     BOTLOC=CHARACTER*(*) (returned)
*        locator to the object
*     STATUS=INTEGER

*  Algorithm:
*     Given the name of a top-level HDS component, check whether it specifies
*     an array element/slice - ie is of the form NAME(x,y...) - and if it is,
*     return a locator to the top-level and to the array element/slice.
*     Otherwise, return two copies of the top-level locator.

*  Copyright:
*     Copyright (C) 1985, 1986, 1987, 1988, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     13-MAR-1986 (BDK):
*        close file on error
*     13-MAY-1987 (BDK):
*        handle slices
*     24-JUN-1988 (AJC):
*        correct case of filename in quotes when top
*        level is array element or slice
*     01-FEB-1990 (AJC):
*        Avoid hanging locators problem
*     15-NOV-1990 (AJC):
*        SUBPAR_SPLIT now avoids () on filenames
*        so can remove check
*     30-JUL-1991 (AJC):
*        EMS error reporting from HDS also
*     02-AUG-1991 (AJC):
*        Translate filename first
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
      INCLUDE 'EMS_PAR'


*  Arguments Given:
      CHARACTER*(*) FILENAME          ! the name of the container file

      CHARACTER*(*) COMPONENT         ! the component name

      CHARACTER*(*) ACCESS            ! READ, WRITE or UPDATE


*  Arguments Returned:
      CHARACTER*(*) FILOC             ! locator for container

      CHARACTER*(*) BOTLOC            ! locator to object


*  Status:
      INTEGER STATUS


*  Local Variables:
      CHARACTER*132 FULNAM            ! expanded filename

      INTEGER NAMLEN                  ! used length of FULNAM

      LOGICAL STRUCARR                ! .TRUE. => an array element
                                      ! .FALSE. => a scalar

      LOGICAL SLICE                   ! .TRUE. => slice of an array element
                                      ! .FALSE. => otherwise

      CHARACTER*(DAT__SZNAM) NAME     ! the name of the component with any
                                      ! dimensional information removed.

      INTEGER NDIMS                   ! number of dimensions of the component

      INTEGER STARTS(DAT__MXDIM)      ! starts of dimensions specified

      INTEGER ENDS(DAT__MXDIM)        ! ends of dimensions specified

      INTEGER ISTAT                   ! internal status

      CHARACTER*(EMS__SZPAR) EMSNAM   ! EMS message name

      INTEGER EMSLEN                  ! used length of EMSNAM

      CHARACTER*(EMS__SZMSG) EMSMES   ! EMS message

      INTEGER MESLEN                  ! used length of EMSMES

      CHARACTER*(EMS__SZMSG) TMES     ! EMS message to discard

      INTEGER TMLEN                   ! used length of TMES


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set new error context
      CALL EMS_MARK

*    Initialise locators
      FILOC = ' '
      BOTLOC = ' '

*    Translate components of the filename if necessary
*    Actually no action required on VMS, translate environment variables,
*    ~ etc. for UNIX,
      CALL SUBPAR_FNAME ( FILENAME, FULNAM, NAMLEN, STATUS )

*    Open the container and get a locator to the array element/slice.
      CALL HDS_OPEN ( FULNAM(1:NAMLEN), ACCESS, FILOC, STATUS )

*    Check for array object
      CALL SUBPAR_HDSARR ( COMPONENT, STRUCARR, SLICE, NAME, NDIMS,
     :    STARTS, ENDS, STATUS )

      IF ( STRUCARR ) THEN
         IF ( SLICE ) THEN
            CALL DAT_SLICE ( FILOC, NDIMS, STARTS, ENDS, BOTLOC,
     :           STATUS )
         ELSE
            CALL DAT_CELL ( FILOC, NDIMS, ENDS, BOTLOC, STATUS )
         ENDIF

      ELSE
*      Scalar object. BOTLOC is a clone of the file locator.
         CALL DAT_CLONE ( FILOC, BOTLOC, STATUS )

      ENDIF

*    Clean up and nullify locators if unsuccessful
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL ( BOTLOC, STATUS )
         ISTAT = SAI__OK
         CALL HDS_CLOSE ( FILOC, ISTAT )
         FILOC = ' '
         BOTLOC = ' '

*     Retain the first error message
         ISTAT = STATUS
         CALL EMS_ELOAD ( EMSNAM, EMSLEN, EMSMES, MESLEN, ISTAT )

*     forget the remainder - annuls the stack
         DOWHILE ( ISTAT .NE. SAI__OK )
            CALL EMS_ELOAD ( EMSNAM, EMSLEN, TMES, TMLEN, ISTAT )
         ENDDO

*     Re-report the first message
         CALL EMS_REP ( 'SUP_HDSOPEN', EMSMES(1:MESLEN), STATUS )
      ENDIF

*     Release error context
      CALL EMS_RLSE

      END
