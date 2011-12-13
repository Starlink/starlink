      SUBROUTINE GKS_ASSOC ( PNAME, ACMODE, WKID, STATUS )
*+
*  Name:
*     GKS_ASSOC

*  Purpose:
*     Associate a graphics workstation with a parameter, and open it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GKS_ASSOC ( PNAME, MODE, WKID, STATUS )

*  Description:
*     Associate a graphics workstation with the specified Graphics
*     Device Parameter and return a GKS Workstation Identifier to
*     reference it.
*     In GKS terms, the following actions occur --
*        If GKS is not already open, it is opened and, if
*        the workstation associated with the device parameter is
*        not already open, that too is opened and activated.

*  Arguments:
*     PNAME = CHARACTER*(*) (Given)
*        Expression specifying the name of a Graphics Device
*        Parameter.
*     MODE = CHARACTER*(*) (Given)
*        Expression specifying the access mode -  'READ', 'WRITE'
*        or 'UPDATE', as appropriate.
*     WKID = INTEGER (Returned)
*        A Variable to contain the Workstation Identifier.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Notes:
*     The facility whereby update mode does not to clear the
*     workstation is specific to RAL GKS and is not implemented for all
*     workstations.

*  Algorithm:
*     Activate the GKS package if necessary and get a graphics
*     parameter descriptor, if the parameter is active return the
*     graphics descriptor.  'Open' and 'activate' the Workstation.  Set
*     up the GKS parameter table entry in COMMON.

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-OCT-1983 (SLW):
*        Starlink Version.
*     05-MAR-1985 (BDK):
*        ADAM version.
*     18-APR-1986 (AJC):
*        Improved error reporting.
*     29-MAY-1986 (AJC):
*        Set 'NO CLEAR' escape if ACMODE is 'UPDATE'.
*     12-SEP-1986 (AJC):
*        Shorten prologue lines.
*     06-FEB-1990 (AJC):
*        Remove DEVDATASET bit; GKS now uses GNS.
*     08-FEB-1990 (AJC):
*        Use CHR_UCASE not STR$UPCASE.
*        New error reporting philosophy.
*     26-NOV-1990 (PMA):
*        Convert prologue to new style.
*        Reorder type definitions to new standard order.
*     09-JAN-1992 (DLT):
*        Internal routines renamed from GKS_ to GKS1_.
*        par_par included
*        GKs 7.4 version of "No clear open" escape added
*        Use of GNS_GMESS eliminated
*        Declare GKS1_BLK as EXTERNAL to force loading on block data subprogam
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'      ! SAE Symbolic Constants
      INCLUDE 'PAR_PAR'      ! PAR Symbolic Constants
      INCLUDE 'GKS_PAR'      ! GKS Symbolic Constants
      INCLUDE 'PAR_ERR'      ! PAR Error codes
      INCLUDE 'GKS_ERR'      ! GKS Error codes
      INCLUDE 'gksenv_par'                 ! GKS Environment Symbolic Constants
      INCLUDE 'GNS_PAR'      ! GNS Symbolic constants


*  Global Variables:
      INCLUDE 'gksgo_cmn'                  ! GKS Initialisation Switch
      EXTERNAL GKS1_BLK
      INCLUDE 'gkspa_cmn'                  ! GKS Parameter Table

*  Arguments Given:
      CHARACTER*(*) PNAME        ! Device Parameter Name
      CHARACTER*(*) ACMODE       ! Device access mode

*  Arguments Returned:
      INTEGER WKID               ! GKS Workstation-ID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(GNS__SZNAM+GNS__SZDEV) NAME ! Graphics Device (GNS name)
      CHARACTER*6 ACMODU         ! ACMODE in upper case
      INTEGER NCH                ! Size of ACMODE
      INTEGER RGD                ! Relative graphics descriptor
      INTEGER WSN                ! Workstation Sequence Number
      INTEGER CONID              ! Connection-ID
      INTEGER NAMECODE           ! Pointer to parameter
      LOGICAL FINISHED           ! Loop controller

*  Declarations for Starlink "no clear on open" escape function
*-This code is for RAL-GKS 7.2
*      CHARACTER*80 DATREC(1)
*-This code is for RAL-GKS 7.4
      INTEGER IA(2), IER, LIESC, LOESC
      CHARACTER*7 RESET
      CHARACTER*80 STR(1), IESCDR(1), OESCDR(1)
      REAL RA(1)
      DATA STR/' '/
*-end

*.


      IF (STATUS .NE. SAI__OK) RETURN

*   Check whether initialised
      IF ( GKSSLP ) THEN
         CALL GKS1_ACTIV ( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            RETURN
         ENDIF
      ENDIF

*   Set new error context level
      CALL ERR_MARK

*   Look whether there is already a graphics device associated with this
*   parameter
      CALL GKS1_GETGD ( PNAME, RGD, STATUS )

      IF (STATUS .EQ. GKS__ISACT) THEN

         WKID = PDESC(RGD)
         CALL ERR_ANNUL( STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*      Look-up the parameter name
         CALL SUBPAR_FINDPAR ( PNAME, NAMECODE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'PAR', PNAME )
            CALL ERR_REP( 'GKS_ASSOC_NOPAR',
     :      'GKS_ASSOC: Parameter ^PAR not found',
     :      STATUS )
         ENDIF

*      Loop trying to get the name of the graphics device
         FINISHED = .FALSE.

         DO WHILE ( ( .NOT. FINISHED ) .AND. ( STATUS .EQ. SAI__OK ) )

*         Get the user's name for the graphics device from the parameter
*         system
            CALL SUBPAR_GETNAME ( NAMECODE, NAME, STATUS )

            IF ( ( STATUS .EQ. PAR__NULL ) .OR.
     :        ( STATUS .EQ. PAR__ABORT ) .OR.
     :        ( STATUS .EQ. PAR__NOUSR ) ) THEN

               FINISHED = .TRUE.

            ELSEIF (STATUS .EQ. SAI__OK) THEN

*            Translate the name to a device specification
               CALL GNS_TNG( NAME, WSN, CONID, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*               Set escape according to ACMODE
                  NCH = LEN (ACMODE)
                  IF (NCH.GT.6) NCH=6
                  ACMODU = ACMODE(1:NCH)
                  CALL CHR_UCASE ( ACMODU )

*-This code is for RAL-GKS 7.2
*                  IF ((ACMODU.EQ.'UPDATE').OR.(ACMODU.EQ.'READ')) THEN
*                     DATREC(1)(1:1) = CHAR(1)
*                  ELSE
*                     DATREC(1)(1:1) = CHAR(0)
*                  ENDIF
*                  CALL GESC(1000, 1, DATREC)

*-This code is for RAL-GKS 7.4
                  IF ((ACMODU.EQ.'UPDATE').OR.(ACMODU.EQ.'READ')) THEN

*                 Check that this workstation supports the escape
                     CALL GNS_ITWCG( WSN, 'OPEN', RESET, STATUS)
                     IF ( STATUS.EQ.SAI__OK) THEN
                        IF (RESET.EQ.'NORESET') THEN

                           IA(1) = WSN
                           IA(2) = GYES
                           CALL GPREC(2, IA, 1, RA, 1, 1, STR, 1, IER,
     :                               LIESC, IESCDR)
                           CALL GESC(-3, LIESC, IESCDR, 1, LOESC,
     :                               OESCDR)
                        END IF
                      END IF
                   END IF
*-end

*               Activate the workstation and return a workstation
*               identifier (WKID) for it.
                  CALL GKS1_ASS ( WSN, CONID, WKID, STATUS )

                  IF (STATUS .EQ. SAI__OK) THEN

*                  store its details in the common block, so they can be
*                  used for closing it at some future time
                     PTNAME(RGD) = PNAME
                     CALL CHR_UCASE ( PTNAME(RGD) )
                     PDESC(RGD) = WKID
                     PFREE(RGD) = .FALSE.
                     FINISHED = .TRUE.

                  ELSE

*                  Failed to open device - reason already reported
                     CALL PAR_CANCL( PNAME, STATUS )

                  ENDIF

               ELSE

*               Failed to translate name
                  CALL MSG_SETC( 'NAME', NAME )
                  CALL ERR_REP('GKS_ASSOC_BADNM',
     :            'GKS_ASSOC: Failed to open graphics device (^NAME)',
     :            STATUS )
                  STATUS = GKS__DVERR
                  CALL PAR_CANCL( PNAME, STATUS )
               ENDIF

            ELSE

*            Failed to get name
               CALL MSG_SETC( 'PAR', PNAME )
               CALL ERR_REP('GKS_ASSOC_NAMFL',
     :         'GKS_ASSOC: Failed to get value for parameter %^PAR',
     :         STATUS )
               CALL ERR_REP('GKS_ASSOC_NAMFL2', '^STATUS', STATUS )

            ENDIF

            IF ( (STATUS .NE. SAI__OK) .AND.
     :           (.NOT.FINISHED)) THEN

*            Flush errors if not finished.
               CALL ERR_FLUSH( STATUS )
            ENDIF

         ENDDO

      ENDIF

*    Release the error context
      CALL ERR_RLSE

      END
