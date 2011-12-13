      SUBROUTINE CAT_EXIST (PCNAME, MODE, CI, STATUS)
*+
*  Name:
*     CAT_EXIST
*  Purpose:
*     Attempt to open a catalogue, the name being taken from the ADAM
*     parameter system.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_EXIST (PCNAME, MODE; CI; STATUS)
*  Description:
*     Attempt to open a catalogue, the name being taken from the ADAM
*     parameter system.  If the attempt fails then instead of
*     re-prompting the routine returns with an error status.  This
*     routine can be used to check the existence of a catalogue.
*  Arguments:
*     PCNAME  =  CHARACTER*(*) (Given)
*        Name of the ADAM parameter from which the catalogue name will
*        be obtained.
*     MODE  =  CHARACTER*(*) (Given)
*        Mode in which the catalogue will be accessed.  One of:
*        WRITE  -  an new catalogue is to be written.
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.  If the specified catalogue is not
*        opened successfully, the null identifier is returned.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Mark the error stack (so that reporting local errors does not
*     disturb any pre-existing errors).
*     Find the parameter index in the parameter tables.
*     If ok then
*       Obtain the name of the catalogue from the parameter system.
*       If ok then
*         Attempt to open the catalogue.
*         If the catalogue open fails then
*           Report an error with contextual information.
*           Flush an error messages.
*           Cancel the parameter association (annulling any additional
*           error messages which this may generate).
*         end if
*       end if
*     end if
*     If not ok then
*       Set the catalogue identifier to the null identifier.
*       If an abort was requested then
*         Annul any error messages.
*         Issue an appropriate error message.
*       else if a null value was specified then
*         Annul any error messages.
*         Issue an appropriate error message.
*       else (all other errors)
*         Add context information.
*         Report the error.
*       end if
*     end if
*     Release the error stack.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     10/2/94 (ACD): First version created from CAT_ASSOC.
*     11/4/95 (ACD): Changed the name of the null identifier.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CAT_PAR'
      INCLUDE 'PAR_ERR'
*  Arguments Given:
      CHARACTER
     :  PCNAME*(*),
     :  MODE*(*)
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  PARIND,   ! Parameter table index.
     :  LSTAT     ! Local copy of ADAM status.
      CHARACTER
     :  CNAME*(CAT__SZCNF) ! Catalogue name (inc. directory spec.).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Mark the error stack, so that flushing errors does not disturb
*       any pre-existing errors in the error stack.

         CALL ERR_MARK

*
*       Attempt to find the index for the parameter in the parameter
*       tables.

         CALL SUBPAR_FINDPAR (PCNAME, PARIND, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

*
*          Attempt to obtain a name for the catalogue from the
*          parameter system and proceed if ok.

            CALL SUBPAR_GETNAME (PARIND, CNAME, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN

*
*             Attempt to open the catalogue.

               CALL CAT_TOPEN (CNAME, 'OLD', MODE, CI, STATUS)

*
*             If the status is ok then the catalogue opened successfully
*             and the termination flag can be set.  Otherwise an error
*             must be reported and looping continues.

               IF (STATUS .NE. SAI__OK) THEN

*
*                The given catalogue could not be opened and the
*                user must be reprompted.  The procedure for handling
*                the error is:
*                - report contextual information,
*                - flush any errors,
*                - cancel the parameter association (annulling any
*                  additional errors which this may generate).

                  CALL MSG_SETC ('PCNAME', PCNAME)
                  CALL ERR_REP ('CAT_EXIST_CTX',
     :              'CAT_EXIST: unable to associate a catalogue '/
     :              /'with parameter ''%^PCNAME''.', STATUS)
                  CALL ERR_FLUSH (STATUS)
                  CALL SUBPAR_CANCL (PARIND, STATUS)
                  CALL ERR_ANNUL (STATUS)

               END IF
            END IF
         END IF

*
*       Check for any error.

         IF (STATUS .NE. SAI__OK) THEN

*
*          The attempt to open the catalogue has terminated with an
*          error.  In all cases the catalogue identifier is set to the
*          null identifier.  The special cases where the parameter
*          system was aborted or a null value was given are handled
*          separately.

            CI = CAT__NOID

*
*          If an abort was requested, then annul any error messages and
*          substitute a more appropriate one.

            IF (STATUS .EQ. PAR__ABORT) THEN
               LSTAT = STATUS
               CALL ERR_ANNUL (LSTAT)
               CALL MSG_SETC ('PCNAME', PCNAME)
               CALL ERR_REP ('CAT_EXIST_ABT',
     :           'Attempt to associate an existing catalogue with '/
     :           /'parameter ''%^PCNAME'' aborted.', STATUS)

*
*          If a null value was specified, then annul any error messages
*          and substitute a more appropriate one.


            ELSE IF (STATUS .EQ. PAR__NULL) THEN
               LSTAT = STATUS
               CALL ERR_ANNUL (LSTAT)
               CALL MSG_SETC ('PCNAME', PCNAME)
               CALL ERR_REP ('CAT_EXIST_NULL',
     :           'Null catalogue specified for parameter'/
     :           /' ''%^PCNAME''.', STATUS)

*
*          For all other errors, add context information and report the
*          error.

            ELSE
               CALL MSG_SETC ('PCNAME', PCNAME)
               CALL ERR_REP ('CAT_EXIST_ERR',
     :           'CAT_EXIST: error associating an existing catalogue '/
     :           /'with parameter ''%^PARAM''.', STATUS)
            END IF
         END IF

*
*       Release the error stack.

         CALL ERR_RLSE

      END IF

      END
