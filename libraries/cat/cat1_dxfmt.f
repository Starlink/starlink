      SUBROUTINE CAT1_DXFMT (DTYPE, CSIZE, UNITS, EXFMT, STATUS)
*+
*  Name:
*     CAT1_DXFMT
*  Purpose:
*     Generate a default external format for a component.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DXFMT (DTYPE, CSIZE, UNITS; EXFMT; STATUS)
*  Description:
*     Generate a default external format for a component (column or
*     parameter).  The external format is generated from the data
*     type and units of the component.
*  Arguments:
*     DTYPE  =  INTEGER (Given)
*        Data type of the component.
*     CSIZE  =  INTEGER (Given)
*        The size of a character data type, otherwise not used.
*     UNITS  =  CHARACTER*(*) (Given)
*        Units of the component.
*     EXFMT  =  CHARACTER*(*) (Returned)
*        External format for the component.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For each data type
*       Invent a default format for this data type.
*     end for
*
*     If the component's UNITS indicate that it represents an angle then
*     a format capable of representing the angle to a reasonable precision
*     is used.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
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
*     14/10/93 (ACD): Original version.
*     18/10/93 (ACD): First stable version.
*     23/1/94  (ACD): Modified error reporting.
*     8/2/94   (ACD): Changed parameters for data type codes.
*     19/4/01  (ACD): Changed the default format for a DOUBLE PRECISION
*        component from D12.3 to D20.12, on the assumption that if
*        DOUBLE PRECISION was specified then an accurate value is required.
*     24/4/01  (ACD): Implemented checks for whether the UNITS indicate
*        that the component represents an angle, and if so then for
*        data types REAL and DOUBLE PRECISION use a format capable of
*        representing an angle to a reasonable precision.
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
     :  DTYPE,
     :  CSIZE
      CHARACTER
     :  UNITS*(*)
*  Arguments Returned:
      CHARACTER
     :  EXFMT*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  LCSIZE,  ! Local size for the character string.
     :  POSN     ! Current position in EXFMT.
      CHARACTER
     :  LUNITS*(CAT__SZUNI)  ! Local upper case copy of UNITS.
      LOGICAL
     :  ANGCMP   ! Flag; does the component represent an angle?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check the data type of the component and invent an appropriate
*       external format.

         IF (DTYPE .EQ. CAT__TYPEUB) THEN
            EXFMT = 'I5'

         ELSE IF (DTYPE .EQ. CAT__TYPEB) THEN
            EXFMT = 'I5'

         ELSE IF (DTYPE .EQ. CAT__TYPEUW) THEN
            EXFMT = 'I5'

         ELSE IF (DTYPE .EQ. CAT__TYPEW) THEN
            EXFMT = 'I5'

         ELSE IF (DTYPE .EQ. CAT__TYPEI) THEN
            EXFMT = 'I5'

         ELSE IF (DTYPE .EQ. CAT__TYPEK) THEN
            EXFMT = 'I10'

         ELSE IF (DTYPE .EQ. CAT__TYPER) THEN
            IF (UNITS .NE. ' ') THEN
               LUNITS = UNITS
               CALL CHR_UCASE (LUNITS)
               CALL CHR_LDBLK (LUNITS)

               IF (LUNITS(1 : 7) .EQ. 'RADIANS') THEN
                  ANGCMP = .TRUE.
               ELSE
                  ANGCMP = .FALSE.
               END IF
            ELSE
               ANGCMP = .FALSE.
            END IF

            IF (ANGCMP) THEN
               EXFMT = 'E16.7'
            ELSE
               EXFMT = 'E12.3'
            END IF

         ELSE IF (DTYPE .EQ. CAT__TYPED) THEN
            IF (UNITS .NE. ' ') THEN
               LUNITS = UNITS
               CALL CHR_UCASE (LUNITS)
               CALL CHR_LDBLK (LUNITS)

               IF (LUNITS(1 : 7) .EQ. 'RADIANS') THEN
                  ANGCMP = .TRUE.
               ELSE
                  ANGCMP = .FALSE.
               END IF
            ELSE
               ANGCMP = .FALSE.
            END IF

            IF (ANGCMP) THEN
               EXFMT = 'D20.12'
            ELSE
               EXFMT = 'D16.7'
            END IF

         ELSE IF (DTYPE .EQ. CAT__TYPEL) THEN
            EXFMT = 'L3'

         ELSE IF (DTYPE .EQ. CAT__TYPEC) THEN
            LCSIZE = MAX(CSIZE, 1)

            EXFMT = ' '
            POSN = 0

            CALL CHR_PUTC ('A', EXFMT, POSN)
            CALL CHR_PUTI (LCSIZE, EXFMT, POSN)

         ELSE

*
*         The data type was invalid: set the external format to blank,
*         set the return status and report an error.

            EXFMT = ' '
            STATUS = CAT__INVDT

            CALL CAT1_ERREP ('CAT1_DXFMT_IDT', 'Error generating a '/
     :        /'default external format for a component.', STATUS)

         END IF

      END IF

      END
