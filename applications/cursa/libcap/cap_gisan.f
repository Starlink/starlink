      SUBROUTINE CAP_GISAN (UNITS, ISANG, SEXAG, STATUS)
*+
*  Name:
*     CAP_GISAN
*  Purpose:
*     Determine whether a UNITS string is recognised as an angle.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GISAN (UNITS; ISANG, SEXAG; STATUS)
*  Description:
*     Determine whether a string corresponding to a UNITS attribute is
*     recognised as an angle, and if so also return the sexagesimal
*     format specifier which it contains.
*  Arguments:
*     UNITS  =  CHARACTER*(*) (Given)
*        String corresponding to a UNITS attribute.
*     ISANG  =  LOGICAL (Returned)
*        Flag indicating whether the string was recognised as describing
*        an angle.  Coded as follows:
*        .TRUE.  -  string describes an angle.
*        .FALSE. -  string does not describe an angle.
*     SEXAG  =  CHARACTER*(*) (Returned)
*        If the string describes an angle then this argument contains
*        the CAT sexagesimal format specifier extracted from it.
*        Otherwise it is blank.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the units are not entirely blank then
*       Take a local copy of the units string.
*       Force the local copy of the units into upper case.
*       Remove any leading or embedded spaces.
*       Replace any curly brackets with spaces.
*       Split the string into its constituent words.
*       If the first word is 'RADIANS' then
*         Set the return flag to 'is an angle'.
*         If there is a second word then
*           Set the sexagesimal format specifier to the second word.
*         else
*           Set the sexagesimal format specifier to 'Degrees'.
*         end if
*       else
*         Set the return flag to 'is not an angle'.
*         Set the sexagesimal format specifier to blank.
*       end if
*     else
*       Set the return flag to 'is not an angle'.
*       Set the sexagesimal format specifier to blank.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/2/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      CHARACTER
     :  UNITS*(*)
*  Arguments Returned:
      LOGICAL
     :  ISANG
      CHARACTER
     :  SEXAG*(*)
*  Status:
      INTEGER STATUS         ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MAXWRD         ! Maximum permitted number of words in
      PARAMETER (MAXWRD = 3) ! CUNITS.
*  Local Variables:
      CHARACTER
     :  CUNITS*(CAT__SZUNI),       ! Local copy of UNITS.
     :  WORDS(MAXWRD)*(CAT__SZUNI) ! Constituent words of CUNITS.
      INTEGER
     :  LCUNIT,        ! Length of CUNITS (excl. trail. blanks).
     :  LOOP,          ! Loop index.
     :  NUMWRD,        ! Number of words found in CUNITS.
     :  START(MAXWRD), ! Start positions of words in CUNITS.
     :  STOP(MAXWRD),  ! Stop      "     "    "   "    "   .
     :  LSTAT          ! Local status.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check that the units are not completely blank.

         IF (UNITS .NE. ' ') THEN

*
*          Take a local copy of the units string, force it into upper
*          case and remove any leading or embedded spaces.

            CUNITS = UNITS

            CALL CHR_UCASE (CUNITS)
            CALL CHR_RMBLK (CUNITS)

*
*          Replace any curly brackets with spaces.

            LCUNIT = CHR_LEN(CUNITS)

            DO LOOP = 1, LCUNIT
               IF (CUNITS(LOOP : LOOP) .EQ. '{'  .OR.
     :             CUNITS(LOOP : LOOP) .EQ. '}') THEN
                 CUNITS(LOOP : LOOP) = ' '
               END IF
            END DO

*
*          Split the string into its constituent words.

            CALL CHR_DCWRD (CUNITS, MAXWRD, NUMWRD, START, STOP,
     :        WORDS, LSTAT)

*
*          If the first word is 'RADIANS' then the units correspond
*          to an angle; otherwise they do not.  Set the return
*          variables accordingly.  remember that if the sexagesimal
*          format specifier is omitted (that is, the units are simply
*          'RADIANS' or 'RADIANS{}') then degrees are assumed.

            IF (WORDS(1) .EQ. 'RADIANS') THEN
               ISANG = .TRUE.

               IF (NUMWRD .GE. 2) THEN
                  SEXAG = WORDS(2)
               ELSE
                  SEXAG = 'DEGREES'
               END IF

            ELSE
               ISANG = .FALSE.
               SEXAG = ' '

            END IF

         ELSE

*
*          The units are completely blank.  Therefore they do not
*          represent an angle; set the return variables accordingly.

            ISANG = .FALSE.
            SEXAG = ' '

         END IF

      END IF

      END
