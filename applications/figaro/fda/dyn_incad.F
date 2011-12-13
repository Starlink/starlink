#if HAVE_CONFIG_H
#  include "config.h"
#endif
      SUBROUTINE DYN_INCAD( BASEAD, TYPE, INCR, ADDRES, ISNEW, STATUS )
*+
*  Name:
*     DYN_INCAD

*  Purpose:
*     Returns address offset by a number of elements of a given type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DYN_INCAD( BASEAD, TYPE, INCR, ADDRES, ISNEW, STATUS )

*  Description:
*     This routine is a CNF pointer-based replacement for the
*     DYN_INCREMENT function. It generates an offset address from a
*     base address, the offset being for a number of elements of
*     a known (Fortran) type. The returned value, may or may not be an
*     actual address (depending on the address size of the program), and
*     should be converted into a memory address using the CNF_PVAL
*     function:
*
*           INCLUDE 'CNF_PAR'
*
*           INTEGER BASEAD, OFFAD
*
*           CALL DSA_MAP_DATA('SPECT','READ','FLOAT',BASEAD,SLOT,STATUS)
*
*     *  Offset by 1000 floating point values into spectrum.
*           CALL DYN_INCAD(BASEAD,'FLOAT',1000,OFFAD,ISNEW,STATUS)
*
*           CALL MYSUB(%VAL(CNF_PVAL(OFFAD)))
*
*           IF (ISNEW) THEN
*              CALL CNF_UNREGP(OFFAD)
*           END IF

*  Arguments:
*     BASEAD = INTEGER (Given)
*        The address to offset. Must be either a mapped data array
*        or a previously offset pointer.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the dynamically allocated memory array.  This
*        should be one of 'FLOAT ', 'INT', 'DOUBLE', 'SHORT', 'CHAR',
*        'BYTE', 'LOGICAL' or 'USHORT'. Case is not significant.  If
*        TYPE is none of these, STATUS is returned as SAI__ERROR, and
*        the result will be set equal to BASEAD.
*     INCR = INTEGER (Given)
*        The number of elements of the dynamically allocated array by
*        which the current element number is to be incremented. If INCR
*        is zero, the input pointer will be returned and ISNEW will be
*        set to FALSE.
*     ADDRES = INTEGER (Returned)
*        The new memory address.
*     ISNEW = INTEGER (Returned)
*        When true the returned pointer should be ideally be
*        de-registered by a call to CNF_UNREGP when it is no longer
*        required. This is important for applications with long
*        duty-cycles (as the memory used for registering the actual
*        pointer will otherwise leak).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is platform specific in so far as data types may have
*     different lengths in bytes on different machines. It also accepts
*     and returns pointers that may not be actual memory addresses and
*     should be looked up using CNF_PVAL. If INTEGER*8 is not supported
*     then a replacement for this routine will be required (in which
*     case a C implementation may be required).

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research Council.
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
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     MJC: Malcolm J. Currie (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     06-JUN-2005 (PWD):
*        Original version.
*     2005 June  7 (MJC):
*        Renamed from FIG_INCR and turned into a subroutine given that
*        it had a returned arguments.  Removed unused declarations, and
*        correct a few typos.  Added STATUS argument.
*     09-JUN-2005 (PWD):
*        Few tidyups.
*     24-APR-2006 (TIMJ):
*        Check return value from CNF_PREG. Add short circuit test
*        if INCR is 0
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DSA_TYPES'        ! DSA data types and their sizes
      INCLUDE 'CNF_PAR'          ! CNF functions

*  Arguments Given:
      INTEGER BASEAD
      CHARACTER * ( * ) TYPE
      INTEGER INCR

*  Arguments Returned:
      INTEGER ADDRES
      LOGICAL ISNEW

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * ( 7 ) TYPEUC   ! Given type in upper case
#if SIZEOF_VOIDP == 8
      INTEGER * 8 NEWAD          ! New address, full type.
      INTEGER * 8 OFFSET         ! Offset in bytes
#elif SIZEOF_VOIDP == 4
      INTEGER * 4 NEWAD          ! New address, full type.
      INTEGER * 4 OFFSET         ! Offset in bytes
#else
      Non-standard pointer size! (SIZEOF_VOIDP)
#endif
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Short circuit
      IF (INCR .EQ. 0) THEN
         ADDRES = BASEAD
         ISNEW = .FALSE.
         RETURN
      END IF

*  Actually need to increment
      TYPEUC = TYPE
      CALL CHR_UCASE( TYPEUC )
      NEWAD = CNF_PVAL( BASEAD )
      IF (NEWAD .EQ. 0) THEN
         IF (STATUS .NE. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP('DYN_INCAD_PVAL',
     :           'Variable given to DYN_INCAD is not '/
     :           /'registered with CNF',
     :           STATUS)
            GO TO 3
         END IF
      END IF

      OFFSET = 0
      DO 1 I = 1, MAX_TYPES
         IF ( TYPEUC .EQ. TYPE_NAMES( I ) ) THEN

*  Construct address in parts to avoid any possibility of truncation.
            OFFSET = TYPE_SIZE( I )
            OFFSET = OFFSET * INCR
            NEWAD = NEWAD + OFFSET
            GO TO 2
         END IF
 1    CONTINUE
      STATUS = SAI__ERROR
      CALL MSG_SETC( 'TYPE', TYPE )
      CALL ERR_REP( 'DYN_INC_BADTYPE',
     :  'Probable programming error.  DYN_INCAD invoked with invalid '/
     :  /'data type ^TYPE. ', STATUS )

*   Arrive here when a valid type has been located.
 2    CONTINUE

*  Register this address with CNF.
      IF (STATUS .EQ. SAI__OK) THEN
         ADDRES = CNF_PREG( NEWAD, ISNEW )
         IF (ADDRES .EQ. 0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'DYN_INC_PADREG',
     :           'DYN_INCAD: Unable to register offset pointer '/
     :           /'with CNF.',
     :           STATUS)

         END IF
      END IF

*  Jump here on error
 3    CONTINUE

      END
