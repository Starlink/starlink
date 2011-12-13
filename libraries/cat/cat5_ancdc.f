      SUBROUTINE CAT5_ANCDC (FIELD, NANGLE, ANGLER, CONVOK, STATUS)
*+
*  Name:
*     CAP_ANGDC
*  Purpose:
*     Decode a complex angle from a CHARACTER string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_ANCDC (FIELD, NANGLE; ANGLER, CONVOK; STATUS)
*  Description:
*     Decode a complex angle from a CHARACTER string.
*  Arguments:
*     FIELD  =  CHARACTER*(*) (Given)
*        Character string containing the angle to be decoded.
*     NANGLE  =  INTEGER (Given)
*        Index to the description of the angle in the common block.
*     ANGLER  =  DOUBLE PRECISION (Returned)
*        Decoded angle in radians.
*     CONVOK  =  LOGICAL (Returned)
*        Flag indicating whether the conversion was ok, coded as
*        follows:
*        .TRUE.  -  converted ok,
*        .FALSE. -  failed to convert.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Set the convert ok flag to true.
*     If there is a separate sign then
*       Attempt to read the sign.
*       If ok then
*         Remove any leading spaces.
*         If the first character is '-', 's' or 'S' then
*           Set the sign to negative.
*         else
*           Set the sign to positive.
*         end if
*       else
*         Set the convert ok flag to false.
*       end if
*     end if
*     If converted ok then
*       Set the accumulated value to zero.
*       For each sexagesimal subdivision
*         If the data type of the subdivision is 'I' then
*           Attempt to read the value into an INTEGER variable.
*           If ok then
*             Convert the value to DOUBLE PRECISION
*           else
*             Set the convert ok flag to false.
*           end if
*         else if the data type of the subdivision is 'D' then
*           Attempt to read the value into an DOUBLE PRECISION variable.
*           If not ok then
*             Set the convert ok flag to false.
*           end if
*         end if
*         If converted ok so far then
*           If this is the first sexagesimal subdivision and there
*           is no separate sign then
*             Set the sign flag from the value.
*           end if
*           If this is not the first sexagesimal subdivision and the
*           value is greater than or equal to sixty then
*             Set the convert ok flag to false.
*           end if
*           Take the absolute value.
*           Divide by the appropriate sexagesimal subdivision and
*           add to the accummulated value.
*         end if
*       end for
*       If converted ok so far then
*         Convert the value to radians.
*         Set the sign.
*       end if
*     end if
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     2/8/98 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal  "      "    .
*  Global Variables:
      INCLUDE 'CAT5_ANG_CMN'      ! STL angles common block.
*  Arguments Given:
      CHARACTER
     :  FIELD*(*)
      INTEGER
     :  NANGLE
*  Arguments Returned:
      DOUBLE PRECISION
     :  ANGLER
      LOGICAL
     :  CONVOK
*  Status:
      INTEGER STATUS    ! Global status.
*  Local Constants:
      INTEGER IOOK      ! Success status for internal Fortran I/O.
      PARAMETER (IOOK = 0)
*  Local Variables:
      CHARACTER
     :  ANGBUF*15,  ! Buffer for component of the angle.
     :  SIGN*15     ! Value for the sign component.
      LOGICAL
     :  POSTVE      ! Flag; is the value positive?
      INTEGER
     :  IVAL,       ! Value for INTEGER component.
     :  LSTAT,      ! Local Fortran I/O status.
     :  NSSUB       ! No. of sexagesimal subdivisions.
      DOUBLE PRECISION
     :  DVAL,       ! Value for DOUBLE PRECISION component.
     :  VAL         ! Accummulating value.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Initialise the convert ok flag to true.

         CONVOK = .TRUE.

*
*       If there is a separate sign component then attempt to read
*       it and then use the value to determine whether the angle is
*       positive or negative.

         IF (ANSGN__CAT5(NANGLE)) THEN
            ANGBUF = ' '
            ANGBUF(1 : ANWSG__CAT5(NANGLE)) = FIELD(ANPSG__CAT5(NANGLE)
     :        : ANPSG__CAT5(NANGLE) + ANWSG__CAT5(NANGLE) - 1)

            READ(ANGBUF, '(' // ANFSG__CAT5(NANGLE) // ')',
     :        IOSTAT=LSTAT) SIGN
            IF (LSTAT .EQ. IOOK) THEN
               CALL CHR_LDBLK (SIGN)
               IF (SIGN(1 : 1) .EQ. '-'  .OR.
     :             SIGN(1 : 1) .EQ. 's'  .OR.
     :             SIGN(1 : 1) .EQ. 'S') THEN
                  POSTVE = .FALSE.
               ELSE
                  POSTVE = .TRUE.
               END IF
            ELSE
               CONVOK = .FALSE.
            END IF
         END IF

*
*       Proceed if ok so far.

         IF (CONVOK) THEN

*
*          Set the accumulated value to zero.

            VAL = 0.0D0

*
*          Examine each sexagesimal subdivision

            DO NSSUB = 1, ANNSG__CAT5(NANGLE)

*
*             Check the data type and attempt to read a value
*             of the appropriate type.

               IF (ANTYP__CAT5(NANGLE, NSSUB) .EQ. CAT__TYPEI) THEN

*
*                Attempt to read an INTEGER value and if ok
*                convert it to DOUBLE PRECISION.

C                 print5000, ANSWD__CAT5(NANGLE, NSSUB)
C5000             format(3x, 'ANSWD__CAT5(NANGLE, NSSUB): ', i5 )

                  ANGBUF = ' '
                  ANGBUF(1 : ANSWD__CAT5(NANGLE, NSSUB) ) =
     :              FIELD(ANPOS__CAT5(NANGLE, NSSUB) :
     :                ANPOS__CAT5(NANGLE, NSSUB) +
     :                ANSWD__CAT5(NANGLE, NSSUB) - 1)

                  READ(ANGBUF, '(' // ANFMT__CAT5(NANGLE, NSSUB) // ')',
     :              IOSTAT=LSTAT) IVAL
C                 print5001, ival
C5001             format(3x, 'ival: ', i3 )

                  IF (LSTAT .EQ. IOOK) THEN
                     DVAL = DBLE(IVAL)
                  ELSE
                     CONVOK = .FALSE.
                  END IF

               ELSE IF (ANTYP__CAT5(NANGLE, NSSUB) .EQ. CAT__TYPED) THEN

*
*                Attempt to read aDOUBLE PRECISION value.

                  ANGBUF = ' '
                  ANGBUF(1 : ANSWD__CAT5(NANGLE, NSSUB) ) =
     :              FIELD(ANPOS__CAT5(NANGLE, NSSUB) :
     :                ANPOS__CAT5(NANGLE, NSSUB) +
     :                ANSWD__CAT5(NANGLE, NSSUB) - 1)

                  READ(ANGBUF, '(' // ANFMT__CAT5(NANGLE, NSSUB) // ')',
     :              IOSTAT=LSTAT) DVAL
                  IF (LSTAT .NE. IOOK) THEN
                     CONVOK = .FALSE.
                  END IF
               END IF

*
*             Proceed if a value was read successfully.

               IF (CONVOK) THEN

*
*                If this is the first sexagesimal subdivision and
*                there is no separate sign then determine the sign
*                from the value.

                  IF (NSSUB .EQ. 1  .AND.
     :               .NOT. ANSGN__CAT5(NANGLE)) THEN
                     IF (DVAL .GE. 0.0D0) THEN
                        POSTVE = .TRUE.
                     ELSE
                        POSTVE = .FALSE.
                     END IF
                  END IF

*
*               If this is not the first sexagesimal subdivision and
*               the value is greater than or equal to sixty then
*               Set the convert ok flag to false.

                 IF (NSSUB .GT. 1  .AND.  DVAL .GE. 6.0D1) THEN
                    CONVOK = .FALSE.
                 END IF

*
*                Take the absolute value, divide by the appropriate
*                scale factor for the sexagesimal subdivision and
*                add to the accummulated value.

                  DVAL = ABS(DVAL)

                  IF (NSSUB .EQ. 1) THEN
                     VAL = DVAL

                  ELSE IF (NSSUB .EQ. 2) THEN
                     VAL = VAL + (DVAL / 6.0D1)

                  ELSE IF (NSSUB .EQ. 3) THEN
                     VAL = VAL + (DVAL / (6.0D1 * 6.0D1) )

                  END IF
               END IF
            END DO

*
*          If a value has been ok so far then convert it to radians
*          and set the sign.

            IF (CONVOK) THEN
               IF (ANUNI__CAT5(NANGLE) .EQ. CAT1__HOUR) THEN
                  ANGLER = VAL * CAT1__PI / 1.2D1
               ELSE IF (ANUNI__CAT5(NANGLE) .EQ. CAT1__DEG) THEN
                  ANGLER = VAL * CAT1__PI / 1.8D2
               ELSE IF (ANUNI__CAT5(NANGLE) .EQ. CAT1__ARMIN) THEN
                  ANGLER = VAL * CAT1__PI / (1.8D2 * 6.0D1)
               ELSE IF (ANUNI__CAT5(NANGLE) .EQ. CAT1__ARSEC) THEN
                  ANGLER = VAL * CAT1__PI / (1.8D2 * 6.0D1 * 6.0D1)
               ELSE IF (ANUNI__CAT5(NANGLE) .EQ. CAT1__TIMIN) THEN
                  ANGLER = VAL * CAT1__PI / (1.2D1 * 6.0D1)
               ELSE IF (ANUNI__CAT5(NANGLE) .EQ. CAT1__TISEC) THEN
                  ANGLER = VAL * CAT1__PI / (1.2D1 * 6.0D1 * 6.0D1)
               ELSE
                  CONVOK = .FALSE.
                  ANGLER = 0.0D0
               END IF

               IF (.NOT. POSTVE) THEN
                  ANGLER = - ANGLER
               END IF
            END IF

         END IF

      END IF

      END
