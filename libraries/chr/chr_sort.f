      SUBROUTINE CHR_SORT( CHR_SCOMP, MXARY, ARRAY, NSORT )
*+
*  Name:
*     CHR_SORT

*  Purpose:
*     Sort an array of character variables into alphabetical order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_SORT( CHR_SCOMP, MXARY, ARRAY, NSORT )

*  Description:
*     Sort an array of character variables into alphabetical order
*     using the collating sequence provided by the routine CHR_SCOMP.
*     After the sort, a search is made to remove any values which
*     occur more than once. The total number of unique values is
*     returned.

*  Arguments:
*     CHR_SCOMP = LOGICAL FUNCTION (Given)
*        An external function which compares two character strings
*        and returns whether the first string is less than the second.
*     MXARY = INTEGER (Given)
*        The number of character values to sort.
*     ARRAY( MXARY ) = CHARACTER * ( * ) (Given and Returned)
*        The array of character values to be sorted.
*     NSORT = INTEGER (Returned)
*        The number of unique character values returned.

*  Algorithm:
*     -  The sort algorithm used in this subroutine is a modified
*     bubble sort (the Shell-Mezgar algorithm). Using a diminishing
*     increment, this variant of the bubble sort has a speed gain of
*     N**1/2 over the conventional algorithm. Other, faster, sort
*     algorithms all require the use of work-space for character
*     strings and so were not used.

*  Notes:
*     To use this subroutine it is necessary to declare the function
*     CHR_SCOMP, or its equivalent, to be EXTERNAL in the calling
*     routine.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (Starlink)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-APR-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER MXARY

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARRAY( MXARY )

*  Arguments Returned:
      INTEGER NSORT

*  External References:
      LOGICAL CHR_SCOMP          ! String comparison function

*  Local Constants:
      DOUBLE PRECISION LN2INV    ! Inverse of ln(2)
      PARAMETER ( LN2INV = 1.0D+00 / 0.6931471805599453D+00 )

      DOUBLE PRECISION TINY      ! Tiny value to overcome rounding
      PARAMETER ( TINY = 1.0D-13 )

*  Local Variables:
      INTEGER ALEN               ! Declared length of ARRAY element
      INTEGER ICHR               ! Character loop index
      INTEGER ISTR               ! String loop index
      INTEGER ISTR1              ! First string index
      INTEGER ISTR2              ! Second string index
      INTEGER ISORT              ! Sort loop index
      INTEGER ENSORT             ! End of sort loop
      INTEGER LGBAS2             ! Log base 2 MXARY
      INTEGER MIDDLE             ! Middle of sort array
      INTEGER NLEVEL             ! Sort level loop index

      CHARACTER * 1 CTEMP        ! Temporary character

*.

*  Check the length of the given character array.
      IF ( MXARY .GT. 0 ) THEN

*     Get the declared length of each character array element.
         ALEN = LEN( ARRAY( 1 ) )

*     Check the length of each array element.
         IF ( ALEN .GT. 0 ) THEN

*        Get the maximum parent loop index: i.e. the maximum power of 2
*        divisible into MXARY.
            LGBAS2 = INT( DLOG( DBLE( MXARY ) ) * LN2INV + TINY )
            MIDDLE = MXARY

*        Loop to perform sort. First loop for all sort levels.
            DO 40 NLEVEL = 1, LGBAS2
               MIDDLE = MIDDLE / 2
               ENSORT = MXARY - MIDDLE

*           The sort loop.
               DO 30 ISORT = 1, ENSORT
                  ISTR1 = ISORT

*              Loop to perform the string comparison.
 10               CONTINUE
                  IF ( ISTR1 .GE. 1 ) THEN
                     ISTR2 = ISTR1 + MIDDLE

*                 Use the function CHR_SCOMP to compare the two strings.
                     IF ( .NOT. CHR_SCOMP( ARRAY( ISTR1 ),
     :                                     ARRAY( ISTR2 ) ) ) THEN

*                    The second string is lower in the collating
*                    sequence than the first, so loop to switch the
*                    strings.
                        DO 20 ICHR = 1, ALEN
                           CTEMP = ARRAY( ISTR1 )( ICHR : ICHR )
                           ARRAY( ISTR1 )( ICHR : ICHR ) =
     :                        ARRAY( ISTR2 )( ICHR : ICHR )
                           ARRAY( ISTR2 )( ICHR : ICHR ) = CTEMP
 20                     CONTINUE
                     END IF

*                 Update the first string index.
                     ISTR1 = ISTR1 - MIDDLE
                  GO TO 10
                  END IF
 30            CONTINUE
 40         CONTINUE

*        Loop to discard any duplicate strings.
            NSORT = 1

            DO 50 ISTR = 2, MXARY

               IF ( ARRAY( NSORT ) .NE. ARRAY( ISTR ) ) THEN
                  NSORT = NSORT + 1
                  IF ( NSORT .NE. ISTR ) ARRAY( NSORT ) = ARRAY( ISTR )
               END IF
 50         CONTINUE

*        Loop to tidy up the remaining strings in the array if necessary.
            DO 60 ISTR = NSORT+1, MXARY
               ARRAY( ISTR ) = ' '
 60         CONTINUE
         END IF
      END IF

      END
