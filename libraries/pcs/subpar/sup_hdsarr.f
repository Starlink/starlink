      SUBROUTINE SUBPAR_HDSARR ( COMPONENT, STRUCARR, SLICE, NAME,
     :  NDIMS, STARTS, ENDS, STATUS )
*+
*  Name:
*     SUBPAR_HDSARR

*  Purpose:
*     check whether an HDS component is an array element.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_HDSARR ( COMPONENT, STRUCARR, SLICE, NAME,

*  Description:
*     Given the name of an HDS component, check whether it specifies an
*     array element - ie is of the form NAME(x,y...) - or a slice of an
*     array, and if it is, determine the dimension information.

*  Arguments:
*     COMPONENT=CHARACTER*(*) (given)
*        an HDS component name
*     STRUCARR=LOGICAL (returned)
*        .TRUE. => an array element
*        .FALSE. => a scalar
*     SLICE=LOGICAL (returned)
*        .TRUE. => a slice of an array element
*        .FALSE. => otherwise
*     NAME=CHARACTER*(*) (returned)
*        the name of the component with any dimensional information
*        removed.
*     NDIMS=INTEGER (returned)
*        number of dimensions of the component
*     STARTS=INTEGER (returned)
*        starts of dimensions specified - all equal 1 if not SLICE
*     ENDS=INTEGER (returned)
*        ends of dimensions specified - all equal to sizes of
*        dimensions if not SLICE
*     STATUS=INTEGER

*  Algorithm:
*     Parse the given component name. Convert any given dimensions to
*     numbers.
*     eg   JUNK(20,30)  => STRUCARR, not SLICE
*          JUNK(20:30,50:70) => STRUCARR and SLICE
*          JUNK => not STRUCARR, not SLICE

*  Copyright:
*     Copyright (C) 1985, 1987, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     15-NOV-1985 (BDK):
*        don't pass brackets to ARRCHAR
*     13-MAY-1987 (BDK):
*        change to handle slices. This changes the argument
*        list.
*     16-JUL-1991 (AJC):
*        replace LIB$CVT_DX_DX by CHR routines
*        replace DAT__NAMIN by SUBPAR__NAMIN
*     22-JUL-1991 (AJC):
*        use PARSECON_ARRCHAR not STRING_*
*     14-SEP-1992 (AJC):
*        make (x,y1:y2) mean (x:x,y1:y2) not (1:x,y1:y2)
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      8-MAR-2000 (AJC):
*        Make missing upper bound 0 not error
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'            ! SUBPAR status values


*  Arguments Given:
      CHARACTER*(*) COMPONENT         ! the component name


*  Arguments Returned:
      LOGICAL STRUCARR                ! .TRUE. => an array element
                                      ! .FALSE. => a scalar
      LOGICAL SLICE                   ! .TRUE. => slice of an array element
                                      ! .FALSE. => otherwise
      CHARACTER*(*) NAME              ! the name of the component with any
                                      ! dimensional information removed.
      INTEGER NDIMS                   ! number of dimensions of the component
      INTEGER STARTS(DAT__MXDIM)      ! starts of dimensions specified
      INTEGER ENDS(DAT__MXDIM)        ! ends of dimensions specified


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER LBRACK        ! pointer to a left bracket

      INTEGER RBRACK        ! pointer to a right bracket

      INTEGER COLON         ! pointer to a colon

      CHARACTER*(DAT__SZNAM) CARRAY(DAT__MXDIM) ! array indices

      INTEGER CLENGTHS(DAT__MXDIM) ! lengths of CARRAY strings

      INTEGER J             ! loop counter

*.

      STRUCARR = .FALSE.
      SLICE = .FALSE.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Look for brackets
      LBRACK = INDEX ( COMPONENT, '(' )
      RBRACK = INDEX ( COMPONENT, ')' )

      IF ( ( LBRACK .EQ. 0 ) .OR. ( RBRACK .EQ. 0 ) ) THEN

         IF ( LBRACK .NE. RBRACK ) THEN
            STATUS = SUBPAR__NAMIN
         ELSE

*         Not an array element
            NAME = COMPONENT
            NDIMS = 0

         ENDIF

      ELSE IF ( RBRACK .LT. LBRACK+2 ) THEN

*     Nothing between brackets - error
         STATUS = SUBPAR__NAMIN

      ELSE

*      syntax like an array element
         STRUCARR = .TRUE.
         CALL PARSECON_ARRCHAR ( COMPONENT(LBRACK+1:RBRACK-1),
     :    DAT__MXDIM, NDIMS, CARRAY, CLENGTHS, STATUS )

*      Convert the strings into integers, checking for colons.
         J = 1
         DOWHILE ( ( J .LE. NDIMS )  .AND. ( STATUS .EQ. SAI__OK ) )
            COLON = INDEX ( CARRAY(J), ':' )

            IF ( COLON .EQ. 0 ) THEN
               CALL CHR_CTOI( CARRAY(J), ENDS(J), STATUS )
               STARTS(J) = ENDS(J)

            ELSE IF ( COLON .EQ. 1 ) THEN
               SLICE = .TRUE.
               STARTS(J) = 1
               CALL CHR_CTOI( CARRAY(J)(2:), ENDS(J), STATUS )

            ELSE
               SLICE = .TRUE.
               CALL CHR_CTOI( CARRAY(J)(1:COLON-1), STARTS(J), STATUS )
               IF ( COLON .NE. CLENGTHS(J) ) THEN
                  CALL CHR_CTOI( CARRAY(J)(COLON+1:), ENDS(J), STATUS )
               ELSE
                  ENDS(J) = 0
               ENDIF
            ENDIF

*        Do next item
            J = J + 1

         ENDDO

         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SUBPAR__NAMIN

         ELSE

*         copy the structure part of the name
            NAME = COMPONENT(1:LBRACK-1)

         ENDIF

      ENDIF

      END
