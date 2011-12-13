      SUBROUTINE GKS1_ASS ( WSTYP, CONID, WKID, STATUS )
*+
*  Name:
*     GKS1_ASS

*  Purpose:
*     Start and activate GKS Workstation

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS_ASS( WSTYP, CONID, WKID, STATUS )

*  Description:
*     Open and activate the Workstation sepcified by (WSTYP,CONID) with
*     Workstation-ID given by wkid.
*     If GKS is not already open, open it.

*  Arguments:
*     WSTYP = INTEGER (Given)
*        A variable containing the Workstation Type.
*     CONID = INTEGER (Given)
*        A variable containing the connection identifier.
*     WKID = INTEGER (Returned)
*        The workstation identifier.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     Open GKS if not already open, check that a workstation with the same
*     type and connection id isn't already open, pick an unused workstation
*     id and open and activate the workstation.

*  Copyright:
*     Copyright (C) 1983, 1985, 1986, 1990, 1992 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     AJC: A J Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*    History:
*     11-OCT-1983: (SLW):
*         Starlink Version.
*     07-MAR-1985: (BDK):
*         ADAM version
*     21-FEB-1986: (AJC):
*         Convert for GKS 7.2
*     08-FEB-1990: (AJC):
*         Allow devices of same type and different conid.
*     08-JAN-1992: (DLT):
*         Reformat comments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*    Global constants :
      INCLUDE 'SAE_PAR'   ! SAE Symbolic Constants
      INCLUDE 'GKS_PAR'   ! GKS Internal parameters

*  Arguments Given:
      INTEGER WSTYP                     ! Workstation Sequence Number
      INTEGER CONID                     ! Connection-ID

*  Arguments Returned:
      INTEGER WKID                      ! Workstation-ID

*  Status:
      INTEGER STATUS                    ! status return

*  Local Constants:
      INTEGER LUEGKS            ! Unit number for GKS error messages (not used)
      PARAMETER (LUEGKS = 6)
      INTEGER MAXOPWK
      PARAMETER (MAXOPWK = 4)   ! Maximum number of open workstations

*  Local variables:
      INTEGER IOPSTA                    ! GKS Open status
      INTEGER NWKOPN                    ! number of open workstations
      INTEGER IWKID(MAXOPWK)            ! list of work id's
      INTEGER I                         ! loop variable
      INTEGER ICONID                    ! connection id
      INTEGER IWSTYP                    ! workstation number
      INTEGER ERRIND                    ! Error indication from queries
      INTEGER IFACT                     ! Workstation active ?
      LOGICAL DONE                      ! loop controller
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   See if GKS open
      CALL GQOPS( IOPSTA )
      IF (IOPSTA .EQ. GGKCL) THEN
         CALL GOPKS( LUEGKS )
      ENDIF
      CALL GKS_GSTAT ( STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      GKS is successfully open
         DONE = .FALSE.
         NWKOPN = 0
         IF (IOPSTA .NE. GGKOP) THEN

*         At least one workstation is open
            NWKOPN = 1

*         Compile list of wkids in IWKID and
*          see if required workstation is already open
            I = 0

            DO WHILE ( ( .NOT. DONE ) .AND. ( I .LT. NWKOPN ) )
               I = I + 1

*            Inquire wkid of the Ith open workstation and true number
*            of open workstations
               CALL GQOPWK(I, ERRIND, NWKOPN, IWKID(I))

*            Inquire type and connection id of open workstation
               CALL GQWKC(IWKID(I), ERRIND, ICONID, IWSTYP)
               CALL GKS_GSTAT(STATUS)
               IF ( STATUS .NE. SAI__OK ) THEN
                  DONE = .TRUE.
               ELSE IF ( IWSTYP .EQ. WSTYP ) THEN

*               Workstation of required type already open
*               check connection id is the same
                  IF ( ICONID .EQ. CONID ) THEN
                     WKID = IWKID(I)
                     DONE = .TRUE.
                  ENDIF
               ENDIF
            ENDDO

         ENDIF
      ENDIF

*   Open the workstation
      IF ((STATUS.EQ.SAI__OK) .AND. (.NOT.DONE )) THEN

*      Pick a Workstation number that is not being used
         IF ( NWKOPN .LE. 0 ) THEN
            WKID = 1
         ELSE
            WKID = 0
            DO I = 1, NWKOPN
               WKID = MAX ( WKID, IWKID(I) )
            ENDDO
            WKID = WKID +1
         ENDIF

*      now open it
         CALL GOPWK ( WKID, CONID, WSTYP )
         CALL GKS_GSTAT ( STATUS )

      ENDIF

      IF ( STATUS .EQ. SAI__OK ) THEN

*      See if required workstation is already active
*      if not activate it.
         CALL GQWKS(WKID, ERRIND, IFACT)
         IF ( IFACT .NE. GACTIV ) CALL GACWK(WKID)

*      check whether GKS has reported an error
         CALL GKS_GSTAT(STATUS)

      ENDIF

      END
