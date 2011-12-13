      SUBROUTINE PGP_ASSOC ( PNAME, ACMODE, NX, NY, UNIT, STATUS )
*+
*  Name:
*     PGP_ASSOC

*  Purpose:
*     Associate a graphics workstation with a parameter, and open it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGP_ASSOC ( PNAME, MODE, NX, NY, UNIT, STATUS )

*  Description:
*     Associate a graphics workstation with the specified Graphics
*     Device Parameter and return an PGP unit identifier

*  Arguments:
*     PNAME = CHARACTER*(*) (Given)
*        Expression specifying the name of a graphics parameter.
*     MODE = CHARACTER*(*) (Given)
*        Expression specifying the access mode.
*        If it is 'READ' or 'UPDATE', a vdu workstation screen
*        will not be cleared when the workstation is opened.
*        If it is 'WRITE', the screen will be cleared as usual.
*     NX = INTEGER (Given)
*        Number of sub-plots in X.
*     NY = INTEGER (Given)
*        Number of sub-plots in Y.
*     UNIT = INTEGER (Returned)
*        A Variable to contain the unit identifier.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*      PGPLOT currently only supports one workstation so the unit is
*      always returned as one.

*  Algorithm:
*     Get the name of the required device, append "/APPEND" to the
*     workstation name if required and open PGPLOT.

*  Implementation Deficiencies:
*     <none>

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
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
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1992 (DLT):
*        Original version
*     13-JAN-1992 (DLT):
*        Strip blanks from device name to improve look of error messages
*     18-FEB-1994 (DLT):
*        Correct invocation comment
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SAE Symbolic Constants

      INCLUDE 'PAR_PAR'          ! PAR Symbolic Constants

      INCLUDE 'PAR_ERR'          ! PAR Error codes

      INCLUDE 'GNS_PAR'          ! GNS Symbolic Constants

      INCLUDE 'PGP_ERR'          ! PGP Error codes

      INCLUDE 'pgpenv_par'       ! PGP Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'pgpgo_cmn'        ! PGP Initialisation Switch

      INCLUDE 'pgppa_cmn'        ! PGP Parameter Table

      EXTERNAL PGP1_BLK

*  Arguments Given:
      CHARACTER*(*) PNAME        ! Device Parameter Name
      CHARACTER*(*) ACMODE       ! Device access mode
      INTEGER NX                 ! Sub plots in X
      INTEGER NY                 ! Sub plots in Y

*  Arguments Returned:
      INTEGER UNIT               ! PGPLOT unit Number

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(GNS__SZNAM+GNS__SZDEV) DEVICE ! Graphics Device (GNS-style name)
      CHARACTER*(6) ACMODU       ! ACMODE in upper case
      INTEGER RUD                ! Relative unit descriptor
      INTEGER TRUD               ! Relative unit descriptor
      INTEGER NAMEC              ! Pointer to parameter
      LOGICAL FINISH             ! Loop controller

*  Functions called
      INTEGER PGBEG, CHR_LEN
*.


      IF (STATUS .NE. SAI__OK) RETURN

*   Set new error context
      CALL ERR_MARK

*   Check whether initialised
      IF ( PGPSLP ) THEN
         CALL PGP1_ACTIV ( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            RETURN
         ENDIF
      ENDIF

*   look whether there is already a unit associated with this
*   parameter
      CALL PGP1_GETUD ( PNAME, RUD, STATUS )

      IF (STATUS .EQ. PGP__ISACT) THEN

         UNIT = PDESC(RUD)
         CALL ERR_ANNUL( STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*      Check the specified access mode
         ACMODU = ACMODE
         CALL CHR_UCASE(ACMODU)
         IF ( (ACMODU .NE. 'UPDATE')
     :    .AND. (ACMODU .NE. 'READ')
     :    .AND. (ACMODU .NE. 'WRITE') ) THEN
            CALL MSG_SETC( 'PAR', ACMODE )
            STATUS = PGP__ILLAC
            CALL ERR_REP('PGP_ASSOC_ILLAC',
     :      'PGP_ASSOC: Illegal access mode (^PAR) specified', STATUS )

         ELSE
*         Access mode is legal - Look-up the parameter name
            CALL SUBPAR_FINDPAR ( PNAME, NAMEC, STATUS )

*         Loop trying to get the name of the graphics device
            FINISH = .FALSE.

            DO WHILE ( ( .NOT. FINISH ) .AND.
     :                 ( STATUS .EQ. SAI__OK ) )

*            Get the user's name for the graphics device from the
*            parameter system
               CALL SUBPAR_GETNAME ( NAMEC, DEVICE, STATUS )

               IF ( ( STATUS .EQ. PAR__NULL ) .OR.
     :              ( STATUS .EQ. PAR__ABORT ) .OR.
     :              ( STATUS .EQ. PAR__NOUSR ) ) THEN

                  FINISH = .TRUE.

               ELSEIF ( STATUS .EQ. SAI__OK ) THEN

*               Find an unused unit number
                  UNIT = 0
                  DO WHILE(STATUS.EQ.SAI__OK)
                     UNIT = UNIT + 1
                     CALL PGP1_CHKUN( UNIT, TRUD, STATUS)
                  END DO

*               Open the workstation and return a unit number for it.
                  IF (( ACMODU.EQ.'UPDATE').OR.(ACMODU.EQ.'READ')) THEN
                     STATUS = PGBEG(0,
     :                    DEVICE(:CHR_LEN(DEVICE))//'/APPEND', NX, NY)
                  ELSE
                     STATUS = PGBEG(0, DEVICE, NX, NY)
                  ENDIF

*               Convert status to an ADAM status by retrieving the last
*               error from the stack.
                  IF (STATUS.EQ.1) THEN
                     STATUS = SAI__OK
                  ELSE
                     CALL EMS_STAT(STATUS)
                  END IF

                  IF (STATUS .EQ. SAI__OK) THEN

*                  Disable prompting before starting a new page as the
*                  PGPLOT prompt routine bypassed the parameter system
                     CALL PGASK(.FALSE.)

*                  Store its details in the common block, so they can be
*                  used for closing it at some future time
                     PTNAME(RUD) = PNAME
                     CALL CHR_UCASE ( PTNAME(RUD) )
                     PDESC(RUD) = UNIT
                     PFREE(RUD) = .FALSE.
                     FINISH = .TRUE.

                  ELSE
*                  PGBEG Failed to open device
*                  Error should already be reported
*                  Cancel the parameter, flush the error messages and
*                  try again.
                     CALL PAR_CANCL ( PNAME, STATUS )
                     CALL ERR_FLUSH ( STATUS )

                  ENDIF

               ELSE
*               Failed on getting name
*               Failed to get name
                  CALL MSG_SETC( 'PAR', PNAME )
                  CALL ERR_REP(' ',
     :            'PGP_ASSOC: Failed to get value for parameter %^PAR',
     :            STATUS )
                  CALL ERR_REP(' ', '^STATUS', STATUS )

               ENDIF

            ENDDO

         ENDIF

      ELSE
*      Status failure in getting unit descriptor
        CALL ERR_REP( 'PGP_ASSOC_TOOUD',
     :  'PGP_ASSOC: Failed to allocate a unit descriptor',
     :  STATUS )

      ENDIF

*   Release error context
      CALL ERR_RLSE

      END
