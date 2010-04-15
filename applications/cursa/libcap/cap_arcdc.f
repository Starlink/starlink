      SUBROUTINE CAP_ARCDC (PDIST, STATUS)
*+
*  Name:
*     CAP_ARCDC
*  Purpose:
*     Decode a critical distance expressed in seconds or minutes of arc.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_ARCDC (PDIST; STATUS)
*  Description:
*     Decode a critical distance expressed in seconds or minutes of arc.
*
*     In order to be decoded by this routine the critical distance must
*     be a constant comprising a valid floating point number followed,
*     without spaces by the string 'arcsec' or 'arcmin'.
*
*     'arcsec' and 'arcmin' may have any capitalisation.
*
*     There is no requirement that the value should be less than a
*     minute of arc.
*
*     Some valid examples are:
*
*      23.5arcsec
*      7ARCSEC
*      1.113E2Arcsec
*
*      23.5arcmin
*      7ARCMIN
*      1.113E2Arcmin
*
*     If the string does not end in 'arcsec' then it is returned
*     unmodified.
*
*     If the string ends in 'arcsec' then the routine attempts to
*     extract the preceding floating point number, convert it from
*     seconds of arc to radians and overwrite the string with the new
*     value in radians.
*
*     If the string ends in 'arcmin' then the routine attempts to
*     extract the preceding floating point number, convert it from
*     minutes of arc to radians and overwrite the string with the new
*     value in radians.
*  Arguments:
*     PDIST  =  CHARACTER*(*) (Given and Returned)
*        Expression for the critical distance.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the string is not entirely blank then
*       Determine the length of the string.
*       If the string ends in 'arcsec' then
*         Attempt to extract a DOUBLE PRECISION number from the string.
*         If ok then
*           Convert the number from seconds of arc to radians.
*           Overwrite the string with the new number.
*         end if
*       else if the string ends in 'arcmin' then
*         Attempt to extract a DOUBLE PRECISION number from the string.
*         If ok then
*           Convert the number from minutes of arc to radians.
*           Overwrite the string with the new number.
*         end if
*       end if
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     20/4/95 (ACD): Original version.
*     18/8/99 (ACD): Added support for 'arcmin'.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAP_PAR'       ! CAP symbolic constants.
*  Arguments Given and Returned:
      CHARACTER
     :  PDIST*(*)
*  Status:
      INTEGER STATUS          ! Global status.
*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR
*  Local Variables:
      INTEGER
     :  LPDIST    ! Length of PDIST (excl. trail. blanks).
      DOUBLE PRECISION
     :  ANGLE     ! Angle decoded from the string.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check whether the string is entirely blank.

         IF (PDIST .NE. ' ') THEN

*
*          Determine the length of the string.

            LPDIST = CHR_LEN(PDIST)

*
*          Check whether the string ends in 'arcsec' or 'arcsec' (any
*          capitalisation is permitted).

            IF (LPDIST .GE. 7) THEN
               IF (CHR_SIMLR(PDIST(LPDIST-5 : LPDIST), 'ARCSEC') ) THEN

*
*                Attempt to decode a DOUBLE PRECISION number from the
*                preceding elements of the string and proceed if ok.

                  CALL CHR_CTOD (PDIST(1 : LPDIST-6), ANGLE, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Convert the angle from seconds of arc to radians.

                     ANGLE = ANGLE * CAP__PI / (1.8D2 * 6.0D1 * 6.0D1)

*
*                   Overwrite the string with the new value for the
*                   angle in radians.

                     PDIST = ' '
                     LPDIST = 0

                     CALL CHR_PUTD (ANGLE, PDIST, LPDIST)

                  END IF

               ELSE IF (CHR_SIMLR(PDIST(LPDIST-5 : LPDIST), 'ARCMIN') )
     :           THEN

*
*                Attempt to decode a DOUBLE PRECISION number from the
*                preceding elements of the string and proceed if ok.

                  CALL CHR_CTOD (PDIST(1 : LPDIST-6), ANGLE, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Convert the angle from seconds of arc to radians.

                     ANGLE = ANGLE * CAP__PI / (1.8D2 * 6.0D1)

*
*                   Overwrite the string with the new value for the
*                   angle in radians.

                     PDIST = ' '
                     LPDIST = 0

                     CALL CHR_PUTD (ANGLE, PDIST, LPDIST)

                  END IF

               END IF
            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('PDIST', PDIST)

            CALL ERR_REP ('CAP_ARCDC_ERR', 'Failed to decode maximum '/
     :        /'separation for pairing: ^PDIST ', STATUS)
         END IF

      END IF

      END
