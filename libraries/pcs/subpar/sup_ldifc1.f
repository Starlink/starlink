      SUBROUTINE SUBPAR_LDIFC1( LU, STATUS )
*+
*  Name:
*     SUBPAR_LDIFC1

*  Purpose:
*     To load the bulk of a version 1 style compiled interface file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_LDIFC1( LU, STATUS )

*  Description:
*     The routine reads the data from the .IFC file and unpacks it into
*     the SUBPAR COMMON blocks

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to be read
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     03-JUL-1991 (AJC):
*        Original version.
*     14-MAY-1992 (AJC)
*        Initialize PARDYN pointer
*     14-JUL-1992 (AJC):
*        Remove initialize PARDYN to LOADIFC
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     28-JUN-1995 (AJC):
*        Call correct UPKL for PARWRITE (not UKPKI).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'       ! SUBPAR status values

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'       ! SUBPAR COMMON blocks

*  Arguments Given:
      INTEGER LU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IOSTAT             ! IO status
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Read the data into the arrays.
*
      IF ( PARPTR .GT. 0 ) THEN

         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARLEN(J), J=1,PARPTR )

         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARTYPE(J), J=1,PARPTR )

         CALL PARSECON_UPKL( LU, PARWRITE, 1, PARPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 3, PARLIMS, 1, PARPTR, STATUS )

         CALL PARSECON_UPKL( LU, PARCONT, 1, PARPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 3, PARDEF, 1, PARPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 2, PARASSOC, 1, PARPTR, STATUS )

         CALL PARSECON_UPKI( LU, PARPOS, 1, PARPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 2, PARRPATH, 1, PARPTR, STATUS )

         CALL PARSECON_UPKNB( LU, 5, PARVPATH, 1, PARPTR, STATUS )

         CALL PARSECON_UPKC( LU, PARHELP, 1, PARPTR, STATUS )

         CALL PARSECON_UPKC( LU, PARNAMES, 1, PARPTR, STATUS )

         CALL PARSECON_UPKC( LU, PARPROM, 1, PARPTR, STATUS )

         CALL PARSECON_UPKC( LU, PARKEY, 1, PARPTR, STATUS )

         CALL PARSECON_UPKC( LU, PARPTY, 1, PARPTR, STATUS )

         CALL PARSECON_UPKL( LU, PARLIT, 1, PARPTR, STATUS )

         CALL PARSECON_UPKC( LU, PARMENU, 1, PARPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 2, PARCOORDS, 1, PARPTR, STATUS )

         CALL PARSECON_UPKNB( LU, 5, PARPPATH, 1, PARPTR, STATUS )

         CALL PARSECON_UPKC( LU, PARHKEY, 1, PARPTR, STATUS )

      ENDIF

      IF ( ACTPTR .GT. 0 ) THEN

         CALL PARSECON_UPKC( LU, ACTNAMES, 1, ACTPTR, STATUS )

         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ACTLEN(J), J=1,ACTPTR )

         CALL PARSECON_UPKL( LU, MAYOB, 1, ACTPTR, STATUS )

         CALL PARSECON_UPKL( LU, MAYCAN, 1, ACTPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 2, NEEDOB, 1, ACTPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 2, NEEDCAN, 1, ACTPTR, STATUS )

         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PROGADD(I,J), I=1,2 ), J=1,ACTPTR )

         CALL PARSECON_UPKC( LU, ACTHELP, 1, ACTPTR, STATUS )

         CALL PARSECON_UPKC( LU, ACTKEY, 1, ACTPTR, STATUS )

         CALL PARSECON_UPKC( LU, ACTMENU, 1, ACTPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 2, ACTCOORDS, 1, ACTPTR, STATUS )

      ENDIF

      IF ( NEEDPTR .GT. 0 ) THEN

         CALL PARSECON_UPKI( LU, NEEDPAR, 1, NEEDPTR, STATUS )

         CALL PARSECON_UPKNI( LU, 2, NEEDLIMS, 1, NEEDPTR, STATUS )

         CALL PARSECON_UPKL( LU, NEEDCONT, 1, NEEDPTR, STATUS )

      ENDIF

      IF ( DOUBLEPTR .GT. 0 ) THEN
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( DOUBLELIST(J), J=1,DOUBLEPTR )
      ENDIF

      IF ( INTPTR .GT. 0 ) THEN
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( INTLIST(J), J=1,INTPTR )
      ENDIF

      IF ( INT64PTR .GT. 0 ) THEN
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( INT64LIST(J), J=1,INT64PTR )
      ENDIF

      IF ( REALPTR .GT. 0 ) THEN
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( REALLIST(J), J=1,REALPTR )
      ENDIF

      IF ( CHARPTR .GT. 0 ) THEN
         CALL PARSECON_UPKC( LU, CHARLIST, 1, CHARPTR, STATUS )
      ENDIF

      IF ( LOGPTR .GT. 0 ) THEN
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( LOGLIST(J), J=1,LOGPTR )
      ENDIF

      GO TO 1000

  999 CONTINUE

*  An error has occurred in one of the reads.
      STATUS = SUBPAR__BADIFC
      CALL EMS_REP( 'SUP_LDIFC11',
     :'SUBPAR: Error reading interface module', STATUS )
      CALL EMS_FIOER( 'IOSTAT', IOSTAT )
      CALL EMS_REP( 'SUP_LDIFC12',
     :'^IOSTAT', STATUS )

 1000 CONTINUE

      END
