      SUBROUTINE SGS_ASSOC ( PNAME, ACMODE, ZONE, STATUS )
*+
*  Name:
*     SGS_ASSOC

*  Purpose:
*     Associate a graphics workstation with a parameter, and open it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SGS_ASSOC ( PNAME, MODE, ZONE, STATUS )

*  Description:
*     Associate a graphics workstation with the specified Graphics
*     Device Parameter and return an SGS zone identifier to reference
*     the base zone of the workstation.

*  Arguments:
*     PNAME = CHARACTER*(*) (Given)
*        Expression specifying the name of a graphics parameter.
*     MODE = CHARACTER*(*) (Given)
*        Expression specifying the access mode.
*        If it is 'READ' or 'UPDATE', a vdu workstation screen
*        will not be cleared when the workstation is opened.
*        If it is 'WRITE', the screen will be cleared as usual.
*     ZONE = INTEGER (Returned)
*        A Variable to contain the Zone identifier.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*     The facility whereby update mode does not to clear the
*     workstation is specific to RAL GKS and is not implemented for all
*     workstations.

*  Algorithm:
*     Get the name of the required device, set the device noclear
*     flag if required and open the SGS zone.

*  Implementation Deficiencies:
*     <none>

*  Copyright:
*     Copyright (C) 1983, 1985, 1986, 1989, 1990, 1992 Science & Engineering Research Council.
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
*     BDK: Dennis Kelley (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     14-MAR-1985 (BDK):
*        ADAM version.
*     15-APR-1986 (AJC):
*        Improved error reporting.
*     29-MAY-1986 (AJC):
*        Set NO CLEAR escape if ACMODE is UPDATE.
*     11-AUG-1989 (AJC):
*        Remove initial call to SGS_$HSTAT.
*        Check ACMODE outside loop.
*        Do a complete check.
*     13-FEB-1990 (AJC):
*        Use GNS not DEVDATASET.
*        improve documentation.
*        New error report philosophy.
*     27-NOV-1990 (PMA):
*        Converted prologue to new style.
*        Remove tabs from in line comments.
*     10-JAN-1992 (DLT):
*        Eliminate redundant include files and add par_par
*        Change SGS__SZNAM to GNS__SZNAM
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

      INCLUDE 'SGS_ERR'          ! SGS Error codes

      INCLUDE 'sgsenv_par'       ! SGS Environment Symbolic Constants


*  Global Variables:
      INCLUDE 'sgsgo_cmn'        ! SGS Initialisation Switch

      INCLUDE 'sgspa_cmn'        ! SGS Parameter Table

      EXTERNAL SGS1_BLK

*  Arguments Given:
      CHARACTER*(*) PNAME        ! Device Parameter Name
      CHARACTER*(*) ACMODE       ! Device access mode

*  Arguments Returned:
      INTEGER ZONE               ! SGS Zone Number

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(GNS__SZNAM+GNS__SZDEV) DEVICE ! Graphics Device (GNS-style name)
      CHARACTER*(6) ACMODU       ! ACMODE in upper case
      INTEGER RZD                ! Relative graphics descriptor
      INTEGER NAMECODE           ! Pointer to parameter
      LOGICAL FINISHED           ! Loop controller

*.


      IF (STATUS .NE. SAI__OK) RETURN

*   Set new error context
      CALL ERR_MARK

*   Check whether initialised
      IF ( SGSSLP ) THEN
         CALL SGS1_ACTIV ( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            RETURN
         ENDIF
      ENDIF

*   look whether there is already a zone associated with this
*   parameter
      CALL SGS1_GETZD ( PNAME, RZD, STATUS )

      IF (STATUS .EQ. SGS__ISACT) THEN

         ZONE = PDESC(RZD)
         CALL ERR_ANNUL( STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*      Check the specified access mode
         ACMODU = ACMODE
         CALL CHR_UCASE(ACMODU)
         IF ( (ACMODU .NE. 'UPDATE')
     :    .AND. (ACMODU .NE. 'READ')
     :    .AND. (ACMODU .NE. 'WRITE') ) THEN
            STATUS = SGS__ILLAC
            CALL MSG_SETC( 'PAR', ACMODE )
            CALL ERR_REP('SGS_ASSOC_ILLAC',
     :      'SGS_ASSOC: Illegal access mode (^PAR) specified', STATUS )

         ELSE
*         Access mode is legal - Look-up the parameter name
            CALL SUBPAR_FINDPAR ( PNAME, NAMECODE, STATUS )

*         Loop trying to get the name of the graphics device
            FINISHED = .FALSE.

            DO WHILE ( ( .NOT. FINISHED ) .AND.
     :                 ( STATUS .EQ. SAI__OK ) )

*            Get the user's name for the graphics device from the
*            parameter system
               CALL SUBPAR_GETNAME ( NAMECODE, DEVICE, STATUS )

               IF ( ( STATUS .EQ. PAR__NULL ) .OR.
     :              ( STATUS .EQ. PAR__ABORT ) .OR.
     :              ( STATUS .EQ. PAR__NOUSR ) ) THEN

                  FINISHED = .TRUE.

               ELSEIF ( STATUS .EQ. SAI__OK ) THEN
*               Set the NO CLEAR escape if required
                  IF (( ACMODU.EQ.'UPDATE').OR.(ACMODU.EQ.'READ')) THEN
                     CALL SGS_CLRFG(1)
                  ELSE
                     CALL SGS_CLRFG(0)
                  ENDIF

*               Open the workstation and return a zone number for it.
                  CALL SGS_OPNWK ( DEVICE, ZONE, STATUS )

                  IF (STATUS .EQ. SAI__OK) THEN
*                  Store its details in the common block, so they can be
*                  used for closing it at some future time
                     PTNAME(RZD) = PNAME
                     CALL CHR_UCASE ( PTNAME(RZD) )
                     PDESC(RZD) = ZONE
                     PFREE(RZD) = .FALSE.
                     FINISHED = .TRUE.

                  ELSE
*                  SGS_OPNWK Failed to open device
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
     :            'SGS_ASSOC: Failed to get value for parameter %^PAR',
     :            STATUS )
                  CALL ERR_REP(' ', '^STATUS', STATUS )

               ENDIF

            ENDDO

         ENDIF

      ELSE
*      Status failure in getting zone descriptor
        CALL ERR_REP( 'SGS_ASSOC_TOOZD',
     :  'SGS_ASSOC: Failed to allocate a zone descriptor',
     :  STATUS )

      ENDIF

*   Release error context
      CALL ERR_RLSE

      END
