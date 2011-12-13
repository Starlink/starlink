      SUBROUTINE CAT1_CRTEL (CI, VNAME, VEID, STATUS)
*+
*  Name:
*     CAT1_CRTEL
*  Purpose:
*     Attempt to create an identifier for a vector element.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CRTEL (CI, VNAME; VEID, STATUS)
*  Description:
*     Attempt to create an identifier for a vector element.
*
*     Currently only vector columns, not vector parameters, are
*     supported.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     VNAME  =  CHARACTER*(*) (Given)
*        Name of the vector element for which an identifier is to be
*        created.
*     VEID  =  INTEGER (Returned)
*        Identifier for vector element.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Remember that the syntax for the name of a vector element is
*     the name of the vector column (or parameter) of which it is an
*     element, followed, without spaces,  by the element number enclosed
*     in square brackets.  For example: MAG[5].
*
*     Attempt to parse the name to yield the component name and element
*     number.
*     If ok then
*       Attempt to obtain an identifier for the component name.
*       If ok then
*         Check that the component is a vector.
*         If so then
*           Check whether the specified element lies within the bounds
*           for the vector.
*           If so then
*             Attempt to create a new identifier.
*             If ok then
*               Create the attributes for this identifier.
*             end if
*           else
*             Set the return status.
*             Set the error text.
*           end if
*         else
*           Set the return status.
*           Set the error text.
*         end if
*       else
*         Set the return status.
*         Set the error text.
*       end if
*     else
*       Set the return status.
*       Set the error text.
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     2/2/94  (ACD): Original version.
*     11/4/95 (ACD): Changed the name of the null identifier.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  VNAME*(*)
*  Arguments Returned:
      INTEGER
     :  VEID
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  VELEM,      ! Vector element number.
     :  CMPI,       ! Component identifier.
     :  DIMS,       ! Dimensionality of component (or scalar flag).
     :  SIZE,       ! No. of elements in vector component.
     :  DTYPE,      ! Data type of component.
     :  CSIZE,      ! Size of a component of type CHARACTER.
     :  LVNAME,     ! Length of VNAME  (excl. trail. blanks).
     :  MSGLEN,     !   "    "  MSGTXT ( "  .   "  .   "   ).
     :  ERRLEN      !   "    "  ERRTXT ( "  .   "  .   "   ).
      CHARACTER
     :  CNAME*(CAT__SZCMP),  ! Name of base component of vector.
     :  MSGTXT*70,  ! Message associated with error.
     :  ERRTXT*75   ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to parse the vector element name to yield the name of
*       the base component and the element within it.  Proceed if ok.

         CALL CAT1_PRSVN (VNAME, CNAME, VELEM, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Attempt to obtain an identifier for the base component, and
*          proceed if one is found.  Note that failure is indicated
*          by the null identifier being returned, not a bad status.

            CALL CAT1_TIDNT (CI, CNAME, CMPI, STATUS)

            IF (CMPI .NE. CAT__NOID  .AND.  STATUS .EQ. CAT__OK) THEN

*
*             Check that the component is a vector and proceed if it is.

               CALL CAT_TIQAI (CMPI, 'DIMS', DIMS, STATUS)

               IF (DIMS .NE. CAT__SCALR  .AND.  STATUS .EQ. CAT__OK)
     :           THEN

*
*                Obtain the number of elements in the vector and check
*                that the given element lies in the array bounds.

                  CALL CAT_TIQAI (CMPI, 'SIZE', SIZE, STATUS)

                  IF ( (VELEM .GE. 1)  .AND.  (VELEM .LE. SIZE)  .AND.
     :              (STATUS .EQ. CAT__OK) ) THEN

*
*                   The name given corresponds to a valid vector
*                   element.  Attempt to create a new identifier for it
*                   and proceed if ok.

                     CALL CAT1_CRTID (CAT__FETYP, CI, VEID, STATUS)

                     IF (STATUS .EQ. CAT__OK) THEN

*
*                      Attempt to create the attributes for this
*                      identifier.  Its data type is copied from the
*                      base component.

                        CALL CAT_TIQAI (CMPI, 'DTYPE', DTYPE, STATUS)
                        CALL CAT_TIQAI (CMPI, 'CSIZE', CSIZE, STATUS)

                        CALL CAT1_ADDAC (VEID, 'NAME', .FALSE., VNAME,
     :                    STATUS)
                        CALL CAT1_ADDAI (VEID, 'DTYPE', .FALSE., DTYPE,
     :                    STATUS)
                        CALL CAT1_ADDAI (VEID, 'CSIZE', .FALSE., CSIZE,
     :                    STATUS)
                        CALL CAT1_ADDAI (VEID, 'BASEID', .FALSE., CMPI,
     :                    STATUS)
                        CALL CAT1_ADDAI (VEID, 'ELEM', .FALSE., VELEM,
     :                    STATUS)

                     END IF
                  ELSE
                     STATUS = CAT__INVEC
                     MSGTXT = 'Vector subscript out of range: '

                  END IF
               ELSE
                  STATUS = CAT__INVEC
                  MSGTXT = 'Given component is not a vector: '

               END IF
            ELSE
               STATUS = CAT__NOCMP
               MSGTXT = 'Failure obtaining identifier for component: '

            END IF
         ELSE
            STATUS = CAT__INVEC
            MSGTXT = 'Failure parsing vector specification: '

         END IF

*
*       Report any error.

         IF (STATUS .NE. CAT__OK) THEN
            ERRTXT = ' '
            ERRLEN = 0

            MSGLEN = CHR_LEN (MSGTXT)

            CALL CHR_PUTC (MSGTXT(1 : MSGLEN), ERRTXT, ERRLEN)
            ERRLEN = ERRLEN + 1

            IF (VNAME .NE. ' ') THEN
               LVNAME = CHR_LEN (VNAME)
               CALL CHR_PUTC (VNAME(1 : LVNAME), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CAT1_ERREP ('CAT1_CRTEL_ERR', ERRTXT(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
