      SUBROUTINE RIO_ASSOC( PNAME, ACMODE, FORM, RECSZ, FD, STATUS )
*+
*  Name:
*     RIO_ASSOC

*  Purpose:
*     Create/open a direct access file associated with a parameter

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RIO_ASSOC( PNAME, ACMODE, FORM, RECSZ, FD, STATUS )

*  Description:
*     Open the direct access file specified by parameter PNAME and
*     return a file descriptor for it.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        Expression giving the name of a file parameter.
*     ACMODE = CHARACTER * ( * ) (Given)
*        Expression giving the required access mode.
*        Valid modes are:
*        'READ' - Open the file READONLY. The file must exist.
*        'WRITE' - Create a new file and open it to write/read.
*        'UPDATE' - Open a file to read/write. The file must exist.
*        'APPEND' - Open a file to write/read.
*                   If the file does not already exist, create it.
*                   (APPEND has no other effect for direct access)
*     FORM = CHARACTER * ( * ) (Given)
*        Expression giving the required record formatting.
*        Valid options are 'FORMATTED' or 'UNFORMATTED'
*     RECSZ = INTEGER (Returned)
*        Expression giving the record size in bytes.
*     FD = INTEGER (Returned)
*        Variable to contain the file descriptor.
*     STATUS = INTEGER (Given and returned)
*        Global status

*  Implementation Deficiencies:
*     This routine handles the STATUS value FIO__ISACT returned by
*     FIO1_GETFP without annulling an error report. This is because
*     FIO1_GETFP does not generate an error report for this status
*     value, which in turn is because FIO1_GETFP is only ever called by
*     this routine which always handles this status value and would
*     always annul any error report. In the future, it is intended that
*     FIO1_GETFP will not return this status value, but will communicate
*     the information by an additional subroutine argument.

*  Notes:
*     -  RECSZ is used to specify the length of a record in bytes.
*     -  If ACMODE is 'WRITE' this is the record length of the new file.
*     -  If ACMODE is 'APPEND' and the file does not already exist, then
*        this is the record length of the new file.
*     -  If ACMODE is 'APPEND' and the file already exists, then RECSZ
*        must match the record length of the existing file.
*     -  If ACMODE is 'UPDATE' then RECSZ must match the record length
*        of the existing file.
*     -  If ACMODE is 'READ' then RECSZ must match the record length of
*        the existing file.
*     -  An exception to the last point is allowed on VMS, where it is
*        possible to set RECSZ to zero when opening a file for read
*        access.

*  Algorithm:
*     The FIO facility is activated if necessary.
*     Obtain a File Parameter Descriptor and check if the file is
*     already open.
*     If it is, use the existing File Descriptor.
*     If not, use SUBPAR_GETNAME to get the name of the file and
*     RIO_OPEN to assign a channel to the  file and return a file
*     descriptor.

*  External Routines Used:
*     CHR:
*        CHR_SIMLR

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     AJC: Alan Chipperfield  (Starlink, RAL)
*     KFH: Ken Hartley (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     BKM: Brian McIlwrath (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     29-Feb-1988:  Original: Adapted from MAG (RAL::AJC)
*     30-Jun-1988:  Add FORM argument (RAL::AJC)
*     28-Aug-1990:  Get file descriptor before parameter (RAL::AJC)
*     01-Nov-1991:  Change to new style prologue (RAL::KFH)
*        Insert IMPLICIT NONE (RAL::KFH)
*        Change fac_$name to FAC1_name (RAL::KFH)
*        Replace tabs with spaces in end-of-line comments (RAL::KFH)
*     27-FEB-1992 (PMA):
*        Tidy up prologue.
*     17-MAR-1992 (PMA):
*        Add error reporting with EMS.
*     25-MAR-1992 (PMA):
*        Add local variable ERRMSG to hold an error message.
*        Add include file 'PAR_PAR' for use on Unix systems.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*        Add EXTERNAL reference to FIO_BLK.
*     2-JUL-1992 (PMA):
*        Change the use of local variable ERRMSG to store the error
*        message to using the token PNAME. Remove the variable ERRMSG as
*        it is no longer needed.
*        If the file cannot be opened, reprompt the user.
*        Change the calls to EMS to calls to ERR.
*     24-AUG-1992 (PMA):
*        Swap the order of the IF statement after the call to FIO1_GETFP
*        so that the first case handled is STATUS = SAI__OK. This has
*        also changed the logic as previously there were two cases;
*        STATUS = FIO__ISACT and everthing else, including STATUS =
*        SAI__OK. This was incorrect, there are now three cases; STATUS
*        = SAI__OK; STATUS = FIO__ISACT, which handles the error; and
*        everthing else, which is a true error.
*        Check on the state of the error returned by FIO_OPEN before
*        flushing the error.
*        Do not cancel the parameter and make the final error report if
*        the user entered ! or !!.
*     8-SEP-1992 (PMA):
*        Removed declaration of CHR_LEN as it is no longer used.
*     19-FEB-1993 (PMA):
*        When checking the error returned by RIO_OPEN, handle invalid
*        format as well as invalid access mode.
*     4-AUG-1993 (PMA):
*        Remove the error report saying that the routine failed to open
*        the file since RIO_OPEN now does this.
*     21-JAN-1994 (PMA):
*        Change the description of RECSZ and add some notes to describe
*        its function.
*      5-DEC-1996 (BKM):
*        CHR_SIMLR is LOGICAL
*      2-AUG-2002 (AJC):
*        Split FIOGO_CMN/FIOGOPA_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! ADAM parameter system error constants
      INCLUDE 'PAR_PAR'          ! ADAM parameter system constants
      INCLUDE 'FIO_PAR'          ! FIO global constants
      INCLUDE 'FIO_SYS'          ! FIO internal constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'FIOPAR_SYS'       ! FIOPAR internal constants

*  Global Variables:
      INCLUDE 'FIOGOPA_CMN'        ! FIO Initialisation Switch
*        FIOSLP = LOGICAL (Read)
*           Whether package is asleep

      INCLUDE 'FIOPA_CMN'        ! FIO Parameter Table
*        PFREE( FIO__MXPAR ) = LOGICAL (Write)
*           Whether slot used
*        PDESC( FIO__MXPAR ) = INTEGER (Read and Write)
*           File descriptor for parameter
*        PTNAME( FIO__MXPAR ) = CHARACTER * ( PAR__SZNAM ) (Write)
*           Parameter names
*        PACMOD( FIO__MXPAR ) = CHARACTER * ( PAR__SZMOD ) (Write)
*           Parameter access modes

*  Arguments Given:
      CHARACTER * ( * ) PNAME
      CHARACTER * ( * ) ACMODE
      CHARACTER * ( * ) FORM
      INTEGER RECSZ

*  Arguments Returned:
      INTEGER FD
      INTEGER RFD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive comparison of strings
      EXTERNAL FIOPA_BLK           ! Block data subprogram

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FILE ! File Name
      INTEGER FP                 ! Parameter Descriptor
      INTEGER RFP                ! Relative Parameter Descriptor
      INTEGER NAMCOD             ! Parameter name's code number
      LOGICAL EXISTS             ! Does the file exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Is FIO/RIO Initialised ?
      IF ( FIOSLP ) THEN
         CALL FIO_ACTIV( STATUS )
      END IF

*  Begin a new error reporting context to handle errors returned by
*  internal routines. We will handle the following errors:
*     FIO1_CHKFD returning FIO__NTOPN
*  If FIO1_GETFP returns FIO__ISACT, it does not report an error, so
*  there is no error report to annul.

      CALL ERR_MARK

*  Get a pointer to a new file parameter descriptor.
      CALL FIO1_GETFP( PNAME, FP, RFP, STATUS )

*  If the file parameter is already active, clear the error from
*  FIO1_GETFP and check that the file is open.
      IF ( STATUS .EQ. FIO__ISACT ) THEN
         STATUS = SAI__OK
*  ( FIO1_GETFP does not report an error when it sets STATUS to FIO_ISACT. )

*  Check the associated file is open.
         FD = PDESC( RFP )
         CALL FIO1_CHKFD( FD, RFD, STATUS )

      END IF

*  If everything is ok, or the parameter is active but the associated
*  file is not open, then
      IF ( ( STATUS .EQ. SAI__OK ) .OR. ( STATUS .EQ. FIO__NTOPN ) )
     :   THEN

*  Annul any error message.
         CALL ERR_ANNUL( STATUS )

*  Get the filename as a character string.
         CALL SUBPAR_FINDPAR( PNAME, NAMCOD, STATUS )
    1    CONTINUE
         CALL SUBPAR_GETNAME( NAMCOD, FILE, STATUS )

*  If all is well, try to open the file.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Open the file and get the file descriptor.
            CALL RIO_OPEN( FILE, ACMODE, FORM, RECSZ, FD, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
*  Set the file parameter descriptor.
               PDESC( RFP ) = FD
               PTNAME( RFP ) = PNAME
               PACMOD( RFP ) = ACMODE
               PFREE( RFP ) = .FALSE.
            ELSE

*  If the error was not an invalid access mode or invalid format, flush
*  the error to the user and reprompt the user.
               IF ( STATUS .NE. FIO__IVACM .AND.
     :              STATUS .NE. FIO__IVFMT ) THEN

*  If the access mode was not READ, see if the file already exists.
*  If it does, report another error saying so before flushing the error
*  reports.
                  IF ( .NOT. CHR_SIMLR( ACMODE, 'READ' ) ) THEN
                     INQUIRE( FILE=FILE, EXIST=EXISTS )
                     IF ( EXISTS ) THEN
                        CALL ERR_REP( 'RIO_ASSOC_EXIST',
     :                     'File already exists', STATUS )
                     END IF
                  END IF
                  CALL ERR_FLUSH( STATUS )
                  CALL SUBPAR_CANCL( NAMCOD, STATUS )
                  GOTO 1
               END IF
            END IF
         END IF
      END IF

*  End the error context begun at the start of this routine.
      CALL ERR_RLSE

*  If failure, cancel parameter and report the error.
      IF ( STATUS .NE. SAI__OK .AND.
     :     STATUS .NE. PAR__NULL .AND. STATUS .NE. PAR__ABORT ) THEN
         CALL RIO_CANCL( PNAME, STATUS )
         CALL MSG_SETC( 'PNAME', PNAME )
         CALL ERR_REP( 'RIO_ASSOC_ERROR', 'Error with parameter ^PNAME',
     :      STATUS )
      END IF

      END
