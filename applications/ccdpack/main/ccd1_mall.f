      SUBROUTINE CCD1_MALL( QUAN, TYPE, POINT, STATUS )
*+
*  Name:
*     CCD1_MALL

*  Purpose:
*     Allocates virtual memory in CCDPACK

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MALL( QUAN, TYPE, POINT, STATUS )

*  Description:
*     This routine allocates arbitrary amounts of virtual memory for
*     any of the HDS types _BYTE, _UBYTE, _WORD, _UWORD, _INTEGER, _INT64
*     _REAL, _DOUBLE and _LOGICAL. Using this routine (instead of PSX
*     directly) allows memory to be freed (by a call CCD1_MFREE)
*     without remembering which pointers are in use at any time.

*  Arguments:
*     QUAN = INTEGER (Given)
*        The quantity of memory required in units of TYPE.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the memory required - can be any of the HDS types
*        _BYTE, _UBYTE, _WORD, _UWORD, _INTEGER, _INT64, _REAL, _DOUBLE
*        and _LOGICAL.
*     POINT = INTEGER (Returned)
*        The pointer to the virtual memory.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Use the related routine CCD1_MFREE only or not at all
*     with this routine, do not mix calls. This routine must be
*     called before CCD1_MFREE.
*
*     - If the pointer storage resources are exceeded then this will not
*     result in an error. However if the extra memory isn't released correctly
*     before exit (if using the CCD1_MFREE routine), then a warning will
*     be issued about unreleased memory. This is an indication that you should
*     consider released memory via CCD1_MFREE calls that do not rely on
*     the -1 mechanism.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-SEP-1992 (PDRAPER):
*        Original version.
*     12-JUL-1995 (PDRAPER):
*        Now returns memory even if the storage space in the common
*        block is exceeded.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations

*  Global Variables:
      INCLUDE 'CCD1_MEMCM'       ! Dynamic memory common block
*        CCD1_MEMCM( CCD1__MXPNT ) = INTEGER (Read and Write)
*           Array of pointers to any data which is allocated by this
*           routine. This common block is updated with the value of the
*           pointer on allocation and the value is removed on
*           deallocation (by CCD1_MFREE).
*
*        CCD1_OVER = INTEGER (Read and Write)
*           The number of calls for memory that have exceeded the storage
*           space available in the common block.

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER QUAN

*  Arguments Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FIRST              ! True on first call of this routine
                                 ! or when common block is cleared
      INTEGER I                  ! Loop variable
      INTEGER SLOT               ! Position in pointer common block to
                                 ! store current reference
      INTEGER NREQD              ! Number of byte-size elements actually
                                 ! required

*  Save the value of first for all time
      SAVE FIRST

*  Local Data:
      DATA FIRST / .TRUE. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If FIRST is true then initialise the common block.
      IF ( FIRST ) THEN
         DO 1 I = 1, CCD1__MXPNT
            CCD1_MEMCM( I ) = -1
 1       CONTINUE

*  Set free slot to 1
         SLOT = 1

*  No memory overflow.
         CCD1_OVER = 0

*  Stop any more initialisation this load of the program.
         FIRST = .FALSE.
      ELSE

*  Look for a free slot in the
         SLOT = -1
         DO 2 I = 1, CCD1__MXPNT
            IF ( CCD1_MEMCM( I ) .EQ. -1 ) THEN
               SLOT = I
               GO TO 3
            END IF
 2       CONTINUE
 3       CONTINUE
         IF ( SLOT .EQ. -1 ) THEN

*  Increment the overflow counter and proceed anyway.
            CCD1_OVER = CCD1_OVER + 1
         END IF
      END IF

*  Try allocating the required amount of memory.
*  Check the input value for sense.
      IF ( QUAN .GT. 0 ) THEN

*  Convert the number of elements to a number of bytes.
         IF ( TYPE .EQ. '_BYTE' ) THEN
            NREQD = VAL__NBB * QUAN
         ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            NREQD = VAL__NBUB * QUAN
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            NREQD = VAL__NBUW * QUAN
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            NREQD = VAL__NBW * QUAN
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            NREQD = VAL__NBI * QUAN
         ELSE IF ( TYPE .EQ. '_INT64' ) THEN
            NREQD = VAL__NBK * QUAN
         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            NREQD = VAL__NBR * QUAN
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            NREQD = VAL__NBD * QUAN
         ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN

*  Fortran 77 says INTEGER has same storage size as LOGICAL.
            NREQD = VAL__NBI * QUAN
         ELSE

*  Unrecognised data type - set status and exit.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_MALL2',
     :      '  CCD1_MALL: Requested data type not valid', STATUS )
            GO TO 99
         END IF

*  Attempt to allocate a piece of memory.
         CALL PSX_MALLOC( NREQD, POINT, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed to access the memory - set status and exit.
            CALL MSG_SETI( 'QUAN', QUAN )
            CALL MSG_SETI( 'NREQD', NREQD )
            CALL MSG_SETC( 'TYPE', TYPE )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_MALL2',
     :      '  CCD1_MALL: Could not allocate ^QUAN (^NREQD bytes) '//
     :      'elements of type ^TYPE', STATUS )

*  Set pointer to ZERO.
            POINT = 0
            GO TO 99
         ELSE

*  Sucessfully obtained memory enter record into stack
            IF ( SLOT .NE. -1 ) CCD1_MEMCM( SLOT ) = POINT
         END IF
      ELSE

*  Invalid input value - set status and exit.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_MALL3',
     :   '  CCD1_MALL: Requested memory less than 1 element', STATUS )
         GO TO 99
      END IF

 99   CONTINUE

*  If overflow has occurred and we're exiting in error back out.
      IF ( STATUS .NE. SAI__OK .AND. SLOT .EQ. -1 ) THEN
         CCD1_OVER = CCD1_OVER - 1
      END IF
      END
* $Id$
