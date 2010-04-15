      SUBROUTINE DTA1_DECDIM( STRING, MXDIM, NDIM, DIMS, STATUS )
*+
*  Name:
*     DTA1_DECDIM

*  Purpose:
*     Read comma-separated positive integers from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA1_DECDIM( STRING, MXDIM, NDIM, DIMS, STATUS )

*  Description:
*     This routine is based on Keith Shortridge's DTA_DECDIM. It reads
*     several numbers from a string like "21,45,3,19", without resorting
*     to an internal READ. The routine uses the ICHAR function and
*     assumes that the digits 0,1,2,3,4,5,6,7,8,9 are contiguous in the
*     machine's character set. NDIM must match the number of numbers in
*     the given string. The numbers in the string must be positive, and
*     must be integers. The string must contain only the digits and the
*     commas as separators. No leading or trailing (!) blanks are
*     allowed. The string may contain less than MXDIM numbers, but not
*     more. There must be numbers in the string, i.e. NDIM = 0 is not an
*     option.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The internal file from which to read the numbers.
*     MXDIM = INTEGER (Given)
*        The highest dimensionality the calling routine can cope with,
*        declared length of DIMS.
*     NDIM = INTEGER (Returned)
*        How many numbers there are in the string.
*     DIMS( MXDIM ) = INTEGER (Returned)
*        The numbers read.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Mar 1996 (hme):
*        Original version, based on KS' DTA_DECDIM.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING
      INTEGER MXDIM

*  Arguments Returned:
      INTEGER NDIM
      INTEGER DIMS( MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Character counter
      INTEGER N                  ! Current number
      INTEGER NCHR               ! Integer value of current digit
      CHARACTER * ( 1 ) CHR      ! Current character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Next number is first, starts off as zero.
      NDIM = 1
      N    = 0

*  Parse the whole string.
      DO 1 I = 1, LEN(STRING)

*     Get current character.
         CHR = STRING(I:I)

*     If separator to next number.
         IF ( CHR .EQ. ',' ) THEN

*        If current number has no digits or only zeros, abort.
            IF ( N .EQ. 0 ) THEN
               STATUS = SAI__ERROR
               GO TO 500
            END IF

*        Current number is complete, store it as currently highest
*        dimension. Initialise next number and increment dimension.
            DIMS( NDIM ) = N
            N    = 0
            NDIM = NDIM + 1

*        If the next number (the , indiates it exists) would be one too
*        many numbers, abort.
            IF ( NDIM .GT. MXDIM )  THEN
               STATUS = SAI__ERROR
               GO TO 500
            END IF

*     Else (current character is not separator, therefore digit).
         ELSE

*        Value of digit is its offset from zero in character set.
            NCHR = ICHAR(CHR) - ICHAR('0')

*        If it was not a digit after all, abort.
            IF ( ( NCHR .LT. 0 ) .OR. ( NCHR .GT. 9 ) ) THEN
               STATUS = SAI__ERROR
               GO TO 500
            END IF

*        Incorporate the digit into the current number.
            N = N * 10 + NCHR

         END IF

 1    CONTINUE

*  String has been processed to its end. The last number is not yet in
*  the returned array.

*  If last number had no digits or only zeros, abort.
      IF ( N .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         GO TO 500
      END IF

*  Store last number.
      DIMS(NDIM) = N

*  If the string contained less numbers than expected, fill with 1.
      DO 2 I = NDIM + 1, MXDIM
         DIMS(I) = 1
 2    CONTINUE

*  Return, after reporting any error.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'FDA_T020', STRING )
         CALL MSG_SETI( 'FDA_T028', MXDIM )
         CALL ERR_REP( 'FDA_E080', 'DTA1_DECDIM: Error reading ' //
     :      'up to ^FDA_T028 positive integers from the string ' //
     :      '"^FDA_T020".', STATUS )
      END IF

      END
