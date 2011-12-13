      SUBROUTINE GNS_GWNI ( FILTER, ICNTX, NAME, DESCR, LD, STATUS )

*+
*  Name:
*     GNS_GWNI

*  Purpose:
*     Get next IDI workstation name

*  Invocation:
*     CALL GNS_GWNI( FILTER, ICNTX, NAME, DESCR, LD, STATUS )

*  Description:
*     The name and description of the "next" IDI workstation from the
*     list of defined workstation names is returned. If the context
*     argument is set to zero the first name in the list will be returned.
*     The context argument is incremented each time a new name is returned
*     until there are no more names in the list when it will be set to
*     zero.
*
*     FILTER is the name of a logical function that is called for each
*     workstation name in the list and can be used to select or reject
*     workstations on criteria such as the workstation class. It should
*     return the value .TRUE. if the name is to be included in the list
*     and .FALSE. if it should not. FILTER has one character argument;
*     the IDI workstation type.
*
*     The GNS library contains a suitable function called GNS_FILTI which
*     selects all workstations.

*  Arguments:
*     FILTER = LOGICAL FUNCTION (Given)
*        The name of the filter routine (which must be declared as
*        external in the calling routine).
*     ICNTX = INTEGER (Given and Returned)
*        Search context. An input value of zero starts at the beginning
*        of the list; returned as zero when there are no more names in
*        the list.
*     NAME = CHARACTER*(SZNAM) (Returned)
*        Workstation name
*     DESCR = CHARACTER*(SZDES) (Returned)
*        Text description of the workstation
*     LD = INTEGER (Returned)
*        Length of description
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Side Effects:
*     The GNS system may be initialized.

*  Copyright:
*     Copyright (C) 1989, 1992 Science & Engineering Research Council.
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
*     DLT: D.L. Terrett (STARLINK)
*     NE: Nick Eaton (Durham University)

*  History:
*      4-AUG-1989 (DLT):
*        Original version.
*      1-SEP-1992 (NE):
*        Updated prologue.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'GNS_PAR'
      INCLUDE 'gns.cmn'

*  Arguments Given:
      LOGICAL FILTER

*  Arguments Given and Returned:
      INTEGER ICNTX

*  Arguments Returned:
      CHARACTER*(*) NAME
      CHARACTER*(*) DESCR
      INTEGER LD

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL FILTER

*  Local Variables:
      LOGICAL INCL
*.

      IF (STATUS.EQ.0) THEN

         IF (ICNTX.LE.0) THEN

*        We are starting a new search
            CALL gns_1INITI(STATUS)
            ICNTX = 0

         END IF

         IF (STATUS.EQ.0) THEN

*     Increment context count
   10       CONTINUE
            ICNTX = ICNTX + 1

            IF (ICNTX.LE.NUMNAI) THEN

*           Call filter routine
               INCL = FILTER(TYPESI(ICNTX))

*           If we want this one then copy it's name and description
               IF (INCL) THEN
                  NAME = NAMESI(ICNTX)
                  LD = LDESCI(ICNTX)
                  IF (LD.GT.0) DESCR = WSDESI(ICNTX)(:LD)
               ELSE

*           Go and get another one (if there are any more)
                  GO TO 10
               END IF
            ELSE

*        There are no names left so reset the context argument
               ICNTX = 0
            END IF
         END IF
      END IF
      END
