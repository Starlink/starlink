      SUBROUTINE SUBPAR_DEF1K ( NAMECODE, NVAL, VALUES, STATUS )
      IMPLICIT NONE

*+
*  Name:
*     SUBPAR_DEF1K

*  Purpose:
*     Set dynamic default vector values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DEF1K ( NAMECODE, NVAL, VALUES, STATUS )

*  Description:
*     Set default values for a vector primitive object associated with
*     a Parameter.
*     If there are six or less values they are stored in COMMON. The COMMON
*     area used corresponds with the type of value supplied. If the type and
*     number enables previously allocated dynamic default storage to be re-
*     used, that is done, otherwise new storage is allocated.
*     If there are more than six values, or there is no more storage space
*     left, the values are stored in an HDS object.
*     If NVAL is given as 0, a scalar is assumed. This allows this routine
*     to be called by the corresponding SUBPAR_DEF0 routine.
*
*     PARDYN(1,-) is set = 0 by SUBPAR activation (PARSECON_NEWPAR or
*     SUBPAR_LDIFC0/1), to the supplied dynamic default type by this routine
*     (or 20+type by SUBPAR_DEFN) and is negated if the dynamic default is
*     cancelled (by SUBPAR_UNSET) - the same allocated space can then be re-
*     used within the same task invocation.
*     At SUBPAR deactivation (SUBPAR_DEACT), PARDYN(1,-) is reset to 0
*     indicating that no space is allocated for dynamic defaults and the
*     list pointer is reset to its value after reading the interface file,
*     thus resetting everything for another invocation of the task
*     Therefore, PARDYN(1,-).GT.0 means space is allocated,
*           and  PARDYN(3,-).GT.0 (=type) means the space is in use
*           and  PARDYN(3,-).LT.0 (=-type) means the space is not in use.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        code-number of the parameter
*     NVAL=INTEGER
*        Expression specifying the number of default values.
*        This must match the object size.
*     VALUES(NVAL)=INTEGER*8
*        Array containing the default values .
*     STATUS=INTEGER

*  Copyright:
*     Copyright (C) 1984, 1986, 1990, 1992, 1993 Science & Engineering Research Council.
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
*     10-DEC-1984 (BDK):
*        Original
*     21-MAR-1986 (BDK):
*        Store small arrays in COMMON
*     16-SEP-1986 (BDK):
*        Correct indexing into VALUES
*     09-JUL-1990 (AJC):
*        Use 'type' as 'in use' flag
*     11-OCT-1990 (AJC):
*        Change PARDYN(3,-) test to .LE.0 so .IFCs work
*     28-JAN-1992 (AJC):
*        Save in type supplied - for efficiency and portability
*      9-MAR-1992 (AJC):
*        Assume NVAL 0 means a scalar value
*     23-MAR 1992 (AJC)
*        Re-structure to correctly re-use space
*     22-JUL-1992 (AJC)
*        Use PARDYN(1,-) as 'space allocated' flag
*        Re-use space if sufficient for new values (not just equal)
*        If cannot re-use space allocate more if available.
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! SAI Constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter code-number

      INTEGER NVAL                      ! number of values

      INTEGER*8 VALUES(*)               ! Vector to supply values

*    Status return :
      INTEGER STATUS			! Status Return


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:

      INTEGER NDIMS                           ! number of dimensions 0 or 1

      INTEGER NVAL1                           ! adjusted number of values

      INTEGER J                               ! loop counter

      INTEGER START                           ! start pos in table

      INTEGER FINISH                          ! end pos in table

      INTEGER K                               ! index into VALUES

*.

       IF (STATUS .NE. SAI__OK) RETURN

*   Check for scalar indicator
       IF ( NVAL .EQ. 0 ) THEN
          NDIMS = 0
          NVAL1 = 1
       ELSE
          NDIMS = 1
          NVAL1 = NVAL
       ENDIF

*   Store the values in COMMON if there are only a small number of them,
*   and if the parameter has had defaults set before, the number and type
*   are the same as previously.
      IF (( NVAL1 .LE. 6 ) .AND. (PARDYN(1,NAMECODE) .GT. 0 ) .AND.
     :    ((PARDYN(2,NAMECODE) - PARDYN(1,NAMECODE)) .GE. NVAL1-1 )
     :      .AND. ((PARDYN(3,NAMECODE) .EQ. -SUBPAR__INT64) .OR.
     :             (PARDYN(3,NAMECODE) .EQ. SUBPAR__INT64)) )
     : THEN
*     Space has been reserved of the correct type and sufficient size
*     Use same space as before
*     we are allowed to overwrite an existing set default value
         START = PARDYN(1,NAMECODE)
         FINISH = PARDYN(1,NAMECODE) + NVAL1 -1
         PARDYN(2,NAMECODE) = FINISH
         PARDYN(3,NAMECODE) = SUBPAR__INT64
         DO J = START, FINISH
            K = J - START + 1
            INT64LIST(J) = VALUES(K)
         ENDDO

      ELSE IF ( ( NVAL1 .LE. 6 )
     :    .AND. ( INT64PTR + NVAL1 .LT. SUBPAR__MAXLIMS ) ) THEN
*     New space required and there is room
         START = INT64PTR + 1
         FINISH = INT64PTR + NVAL1
         INT64PTR = FINISH
         PARDYN(1,NAMECODE) = START
         PARDYN(2,NAMECODE) = FINISH
         PARDYN(3,NAMECODE) = SUBPAR__INT64
         DO J = START, FINISH
            K = J - START + 1
            INT64LIST(J) = VALUES(K)
         ENDDO


      ELSE
*     Either there are more than 6 values or the default storage is full
         CALL SUBPAR_DEFNK
     :   ( NAMECODE, NDIMS, NVAL1, VALUES, NVAL1, STATUS )

      ENDIF

      END
