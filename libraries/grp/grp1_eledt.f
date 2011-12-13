      SUBROUTINE GRP1_ELEDT( SLOT, IN, OUT, STATUS )
*+
*  Name:
*     GRP1_ELEDT

*  Purpose:
*     Apply editing to a literal name

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_ELEDT( SLOT, IN, OUT, STATUS )

*  Description:
*     This subroutine is supplied with an element expression containg a
*     single literal name, optionally contained within several levels
*     of nested OPEN_KERNEL and CLOSE_KERNEL characters. The kernel at
*     each level of nesting may have prefix, suffix and/or substitution
*     strings associated with it. The literal name contained within the
*     inner-most kernel is found, and any associated editing is
*     applied.  This is done by first doing any substitution specified
*     by a substitution string associated with the inner-most kernel.
*     Any prefix and suffix surrounding the inner most kernel are then
*     added to the name. Once the editing associated with the
*     inner-most kernel has been applied, the editing associated with
*     the next level of kernel nesting is applied, and so on until all
*     levels of kernel nesting have been processed. The resulting
*     literal name is returned in OUT.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number for the group whose control characters are to
*        be used to define the syntax of the element expression.
*     IN = CHARACTER * ( * ) (Given)
*        The element expression.
*     OUT = CHARACTER * ( * ) (Returned)
*        The literal name corresponding to the element expression given
*        by IN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1994 (DSB):
*        Original version.
*     27-AUG-1999 (DSB):
*        Added control character escape facility.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP_ private constants
      INCLUDE 'GRP_PAR'          ! GRP_ public constants

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP_ common blocks
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Read)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER SLOT
      CHARACTER IN*(*)

*  Arguments Returned:
      CHARACTER OUT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Function giving used length of a string
      INTEGER GRP1_INDEX         ! Finds un-escaped control characters

*  Local Variables:
      CHARACTER ESCCC*1          ! The escape character
      LOGICAL ESCOK              ! Is the escape character defined?
      INTEGER F                  ! Index of first used character
      INTEGER K1                 ! Start of kernel
      INTEGER K2                 ! End of kernel
      INTEGER L                  ! Index of last used character
      INTEGER LOUT               ! Used length of OUT
      LOGICAL MORE               ! Is loop to be executed again?
      CHARACTER NEW*(GRP__SZNAM) ! The new substitution string
      CHARACTER NEWEL*(GRP__SZNAM)! New element
      INTEGER NEWLEN             ! Used length of NEW
      LOGICAL NEWNUL             ! Is new substitution string null?
      INTEGER NEXT               ! Index of next element delimiter
      INTEGER NLEN               ! Size of NEWEL
      INTEGER NSUB               ! No. of substitutions made
      CHARACTER OLD*(GRP__SZNAM) ! The old substitution string
      INTEGER OLDLEN             ! Used length of OLD
      INTEGER P1                 ! Start of prefix
      INTEGER P2                 ! End of prefix
      INTEGER S1                 ! Start of suffix
      INTEGER S2                 ! End of suffix
      CHARACTER SEPCC*1          ! The separator character
      INTEGER SEP1               ! Position of first separator
      INTEGER SEP2               ! Position of second separator
      INTEGER SEP3               ! Position of third separator
      LOGICAL SEPOK              ! Is the separator character defined?
      INTEGER T1                 ! Start of substitution string
      INTEGER T2                 ! End of substitution string
      CHARACTER TEMP*(GRP__SZNAM)! Temporary element
      INTEGER TLEN               ! Used length of TEMP

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the output string to equal the input string.
      OUT = IN

*  Get the groups current SEPARATOR control character.
      CALL GRP1_CONC( SLOT, GRP__PMSPC, SEPCC, SEPOK, STATUS )

*  Get the groups current ESCAPE control character.
      CALL GRP1_CONC( SLOT, GRP__PESCC, ESCCC, ESCOK, STATUS )

*  Loop round until no more editing is required.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Get the innermost kernel, and the associated editing strings.
         CALL GRP1_FKERN( SLOT, 1, OUT, NEXT, P1, P2, K1, K2, S1, S2,
     :                    T1, T2, F, L, STATUS )

*  Store the prefix (if any) in the new string.
         IF( P2 .GE. P1 ) THEN
            NEWEL = OUT( P1 : P2 )
            NLEN = P2 - P1 + 1
         ELSE
            NEWEL = ' '
            NLEN = 0
         END IF

*  If a substitution is specified, get the string to be replaced, and
*  the string with which to replace it.
         IF( T2 .GE. T1 ) THEN
            SEP1 = GRP1_INDEX( OUT( T1 : T2 ), SEPCC, ESCCC, ESCOK )
     :             + T1 - 1
            SEP2 = GRP1_INDEX( OUT( SEP1 + 1 : T2 ), SEPCC, ESCCC,
     :                         ESCOK ) + SEP1
            SEP3 = GRP1_INDEX( OUT( SEP2 + 1 : T2 ), SEPCC, ESCCC,
     :                         ESCOK ) + SEP2

*  Save the old string and its length.
            OLD = OUT( SEP1 + 1 : SEP2 - 1 )
            OLDLEN = SEP2 - SEP1 - 1

*  If the new string is null, set a flag to indicate this.
            IF( SEP3 .LE. SEP2 + 1 ) THEN
               NEWNUL = .TRUE.
               NEWLEN = 1

*  Otherwise, save the new string and its length.
            ELSE
               NEWNUL = .FALSE.
               NEW = OUT( SEP2 + 1 : SEP3 - 1 )
               NEWLEN = SEP3 - SEP2 - 1
            END IF

*  Do the substitution within the input kernel, appending the result to
*  the end of the new string.
            IF( K2 .GE. K1 ) THEN
               CALL GRP1_SUBST( OUT( K1 : K2 ), OLD(:OLDLEN),
     :                          NEW(:NEWLEN), .TRUE.,
     :                          .NOT. CMN_UPPER( SLOT ), NEWNUL,
     :                          NEWEL( NLEN + 1 : ), NSUB, STATUS )
               NLEN = CHR_LEN( NEWEL )
            END IF

*  If no substitution is required, just append the kernel to the new
*  string.
         ELSE
            IF( K2 .GE. K1 ) CALL CHR_APPND( OUT( K1 : K2 ), NEWEL,
     :                                       NLEN )

         END IF

*  Now append any suffix to the new string.
         IF( S1 .LE. S2 ) CALL CHR_APPND( OUT( S1 : S2 ), NEWEL, NLEN )

*  If any of the string has not yet been used,
         LOUT = CHR_LEN( OUT )
         IF( F .GT. 1 .OR. L .LT. LOUT ) THEN

*  Replace the used section of the output string with the new string.
            IF( F .GT. 1 .AND. F .LE. LOUT + 1 ) THEN
               TEMP = OUT( : F - 1 )
               TLEN = F - 1
            ELSE
               TEMP = ' '
               TLEN = 0
            END IF

            IF( NLEN .GT. 0 ) CALL CHR_APPND( NEWEL( : NLEN ), TEMP,
     :                                        TLEN )
            IF( L .GE. 0 .AND. L .LT. LOUT )
     :                      CALL CHR_APPND( OUT( L + 1 : ), TEMP, TLEN )
            OUT = TEMP

*  If all the string was used, there is no need to do another pass
*  through this loop. Copy the new elementt to the returned variable.
         ELSE
            MORE = .FALSE.
            OUT = NEWEL

         END IF

      END DO

      END
