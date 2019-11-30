      SUBROUTINE SUBPAR_LDIFC0( LU, STATUS )
*+
*  Name:
*     SUBPAR_LDIFC0

*  Purpose:
*     To load the bulk of a version 0 style compiled interface file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_LDIFC0( LU, STATUS )

*  Description:
*     The routine reads the data from the .IFC file into the SUBPAR
*     COMMON blocks

*  Arguments:
*     LU = INTEGER (Given)
*        The logical unit number to be read
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1992, 1993 Science & Engineering Research Council.
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
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARWRITE(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PARLIMS(I,J), I=1,3 ), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARCONT(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PARDEF(I,J), I=1,3 ), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PARASSOC(I,J), I=1,2 ), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARPOS(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PARRPATH(I,J), I=1,2 ), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PARVPATH(I,J), I=1,5 ), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARHELP(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARNAMES(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARPROM(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARKEY(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARPTY(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARLIT(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( PARMENU(J), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PARCOORDS(I,J), I=1,2 ), J=1,PARPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PARPPATH(I,J), I=1,5 ), J=1,PARPTR )

      ENDIF

      IF ( ACTPTR .GT. 0 ) THEN

         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ACTNAMES(J), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ACTLEN(J), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( MAYOB(J), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( MAYCAN(J), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( NEEDOB(I,J), I=1,2 ), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( NEEDCAN(I,J), I=1,2 ), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( PROGADD(I,J), I=1,2 ), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ACTHELP(J), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ACTKEY(J), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ACTMENU(J), J=1,ACTPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( ACTCOORDS(I,J), I=1,2 ), J=1,ACTPTR )

      ENDIF

      IF ( NEEDPTR .GT. 0 ) THEN

         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( NEEDPAR(J), J=1,NEEDPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( ( NEEDLIMS(I,J), I=1,2 ), J=1,NEEDPTR )
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( NEEDCONT(J), J=1,NEEDPTR )

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
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( CHARLIST(J), J=1,CHARPTR )
      ENDIF
      IF ( LOGPTR .GT. 0 ) THEN
         READ ( LU, ERR=999, END=999, IOSTAT=IOSTAT )
     :    ( LOGLIST(J), J=1,LOGPTR )
      ENDIF

*   Additional parameter specifiers
*   Ignore any errors to allow for old-type .IFCs
      IF ( PARPTR .GT. 0 ) THEN
         READ ( LU, ERR=999, END=998, IOSTAT=IOSTAT )
     :    ( PARHKEY(J), J=1,PARPTR )
      ENDIF
      GOTO 1000

*   No PARHKEY record present - set to spaces
  998 DO J = 1, PARPTR
         PARHKEY(J) = ' '
      ENDDO
      GOTO 1000

  999 CONTINUE

*  An error has occurred in one of the reads.
      STATUS = SUBPAR__BADIFC
      CALL EMS_REP( 'SUP_LDIFC01',
     :'SUBPAR: Error reading interface module', STATUS )
      CALL EMS_FIOER( 'IOSTAT', IOSTAT )
      CALL EMS_REP( 'SUP_LDIFC02',
     :'^IOSTAT', STATUS )

 1000 CONTINUE

      END
