      SUBROUTINE RTD1_AQUAL( NDFID, GRAB, IPQUAL, HVQUAL )
*+
*  Name:
*    RTD1_AQUAL

*  Purpose:
*    Accesses the quality array associated with an NDF.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL RTD1_AQUAL( NDFID, GRAB, IPQUAL, HVQUAL )

*  Description:
*     This routines returns a pointer to a byte array with a copy of the
*     quality array associated with an NDF. The copy is persistent and
*     can therefore be modified until either it is copied into an NDF or
*     until is it no longer needed. When the array can be freed set the
*     flag GRAB to .FALSE.

*  Arguments:
*     NDFID = INTEGER (Given)
*        The NDF identifier.
*     GRAB = LOGICAL (Given)
*        If .TRUE. then the array is returned. Otherwise the array will
*        be freed.
*     IPQUAL = INTEGER (Returned)
*        Pointer to the quality array if one is available for this NDF.
*     HVQUAL = LOGICAL (Returned)
*        .TRUE. if quality is available. .FALSE. otherwise.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     17-NOV-1997 (PWD):
*        Original version.
*     02-SEP-2004 (PWD):
*        Converted to use CNF_PVAL for pointers.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Arguments Given:
      INTEGER NDFID
      LOGICAL GRAB

*  Arguments Returned:
      INTEGER IPQUAL
      LOGICAL HVQUAL

*  Status:
      INTEGER STATUS            ! Global status

*  Local parameters:
      INTEGER MAXARR            ! Maximum number of quality arrays
      PARAMETER (MAXARR = 100 ) ! we can deal with

*  Local Variables:
      LOGICAL INIT              ! Whether saved arrays are initialised.
      INTEGER IDS( MAXARR )     ! NDF identifiers.
      INTEGER IPS( MAXARR )     ! Pointers to quality.
      INTEGER I                 ! Loop variable.
      LOGICAL EXISTS            ! Does quality exist in NDF?
      INTEGER SLOT              ! Slot number of quality data
      INTEGER EL                ! Number of values in quality array
      INTEGER IPIN              ! Pointer to mapped data
      INTEGER IPMEM             ! Pointer to malloc'd memory

*  Set first call initialisation and save all persistent arrays.
      SAVE INIT
      SAVE IDS
      SAVE IPS
      DATA INIT /.FALSE./

*.

*  Do first-time initialisation.
      IF ( .NOT. INIT ) THEN
         DO 1 I = 1, MAXARR
            IDS( I ) = 0
            IPS( I ) = 0
 1       CONTINUE
         INIT = .TRUE.
         IF ( .NOT. GRAB ) THEN

*  Stop now if initialising and attempting to release memory (probably a
*  call without any access to quality information first).
            HVQUAL = .FALSE.
            STATUS = SAI__OK
            GO TO 99
         END IF
      END IF

*  No errors can escape this routine, so use a ERR_BEGIN/ERR_END block.
      STATUS = SAI__OK
      CALL ERR_BEGIN( STATUS )

*  Default is no quality.
      HVQUAL = .FALSE.

*  Check for a pointer slot using this NDF identifier.
      SLOT = 0
      DO 2 I = 1, MAXARR
         IF ( IDS( I ) .EQ. NDFID ) THEN
            SLOT = I
            GO TO 3
         END IF
 2    CONTINUE
 3    CONTINUE

      IF ( SLOT .NE. 0 ) THEN

*  Array already allocated, so return it or free it.
         IF ( GRAB ) THEN
            IPQUAL = IPS( SLOT )
            HVQUAL = .TRUE.
         ELSE
            CALL PSX_FREE( IPS( SLOT ), STATUS )
            IPS( SLOT ) = 0
            IDS( SLOT ) = 0
         END IF
      ELSE

*  No slot. Proceed only if grabbing.
         IF ( GRAB ) THEN


*  Check the NDF for a quality array.
            CALL NDF_STATE( NDFID, 'QUALITY', EXISTS, STATUS )
            IF ( EXISTS ) THEN

*  Find a free slot for the array.
               DO 4 I = 1, MAXARR
                  IF ( IDS( I ) .EQ. 0 ) THEN
                     SLOT = I
                     GO TO 5
                  END IF
 4             CONTINUE
 5             CONTINUE

*  If no slot found then do nothing...
               IF ( SLOT .NE. 0 ) THEN
                  CALL NDF_MAP( NDFID, 'QUALITY', '_BYTE', 'READ',
     :                          IPIN, EL, STATUS )
                  CALL PSX_MALLOC( EL, IPMEM, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  May now copy the array and record all the necessary information,
                     CALL RTD_CPB( .FALSE., 0, %VAL( CNF_PVAL( IPIN ) ),
     :                             EL, %VAL( CNF_PVAL( IPMEM ) ),
     :                             STATUS )
                     IDS( SLOT ) = NDFID
                     IPS( SLOT ) = IPMEM
                     HVQUAL = .TRUE.
                     IPQUAL = IPMEM

*  Finally unmap the quality.
                     CALL NDF_UNMAP( NDFID, 'QUALITY', STATUS )
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Exit clearing all errors that may have occurred.

      CALL ERR_END( STATUS )
 99   CONTINUE
      END


