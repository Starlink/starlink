      SUBROUTINE CAT_ASSOC (PCNAME, MODE, CI, STATUS)
*+
*  Name:
*     CAT_ASSOC
*  Purpose:
*     Open an existing catalogue; the name of the catalogue is obtained
*     from an ADAM parameter.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT_ASSOC (PCNAME, MODE; CI; STATUS)
*  Description:
*     Open an existing catalogue; the name of the catalogue is obtained
*     from an ADAM parameter.  If an existing catalogue is opened with
*     MODE = 'WRITE' then it is overwritten.
*  Arguments:
*     PCNAME  =  CHARACTER*(*) (Given)
*        Name of the ADAM parameter from which the catalogue name will
*        be obtained.
*     MODE  =  CHARACTER*(*) (Given)
*        Mode in which the catalogue will be accessed.  One of:
*        READ   -  the catalogue may only be read from,
*        WRITE  -  a new catalogue is to be written.
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Mark the error stack (so that reporting local errors does not
*     disturb any pre-existing errors).
*     Find the parameter index in the parameter tables.
*     If ok then
*       Do while (a valid catalogue has not been given and a
*       non-recoverable error has not occurred)
*         Obtain the name of the catalogue from the parameter system.
*         If ok then
*           Attempt to open the catalogue.
*           If the catalogue opens ok then
*             Set the termination flag.
*           else the catalogue failed to open (and a message must be
*           reported prior to re-trying)
*             Report an error with contextual information.
*             Flush an error messages.
*             Cancel the parameter association (annulling any additional
*             error messages which this may generate).
*           end if
*         else (failed to obtain a value from the parameter system)
*           Set the termination flag.
*         end if
*       end do
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
*     3/5/93   (ACD): Prologue only.
*     20/7/93  (ACD): First implementation.
*     21/1/94  (ACD): Re-written (using NDF_ASSOC as an example).
*     10/2/94  (ACD): Changed options given for 'MODE' in the prologue
*        to correspond to what has actually been implemented.
*     11/4/95  (ACD): Changed the name of the null identifier.
*     17/11/98 (ACD): Improved the error reporting.
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
*  External References:
*     <...>
*  Local Constants:
*     <...>
*  Local Variables:
      INTEGER
     :  PARIND,   ! Parameter table index.
     :  LSTAT     ! Local copy of ADAM status.
      LOGICAL
     :  MORE      ! Flag; continue looping for catalogue?
      CHARACTER
     :  CNAME*(CAT__SZCNF) ! Catalogue name (inc. directory spec.).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the catalogue name (in case it gets used in an
*       error message without being set).

         CNAME = ' '

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
*          Loop until a catalogue has been opened successfully or a
*          non-recoverable error occurs.

            MORE = .TRUE.

            DO WHILE (MORE)

*
*             Attempt to obtain a name for the catalogue from the
*             parameter system and proceed if ok.

               CALL SUBPAR_GETNAME (PARIND, CNAME, STATUS)
               IF (STATUS .EQ. SAI__OK) THEN

*
*                Attempt to open the catalogue.

                  CALL CAT1_TOPEN (CNAME, 'OLD', MODE, CI, STATUS)

*
*                If the status is ok then the catalogue opened
*                successfully and the termination flag can be set.
*                Otherwise an error must be reported and looping
*                continues.

                  IF (STATUS .EQ. SAI__OK) THEN
                     MORE = .FALSE.

                  ELSE

*
*                   The given catalogue could not be opened and the
*                   user must be reprompted.  The procedure for handling
*                   the error is:
*                   - report contextual information,
*                   - flush any errors,
*                   - cancel the parameter association (annulling any
*                     additional errors which this may generate).

                     CALL MSG_SETC ('CNAME', CNAME)
                     CALL MSG_SETC ('PCNAME', PCNAME)
                     CALL ERR_REP ('CAT_ASSOC_CTX',
     :                 'Failed to open catalogue ^CNAME (from '/
     :                 /'parameter %^PCNAME).', STATUS)
                     CALL ERR_FLUSH (STATUS)
                     CALL SUBPAR_CANCL (PARIND, STATUS)
                     CALL ERR_ANNUL (STATUS)
                  END IF

               ELSE

*
*                There was a failure obtaining the parameter from
*                the parameter system.  Such a failure is necessarily
*                unrecoverable.  Set the termination flag.

                  MORE = .FALSE.
               END IF
            END DO
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
               CALL ERR_REP ('CAT_ASSOC_ABT',
     :           'CAT_ASSOC: association of catalogue with parameter '/
     :           /'%^PCNAME aborted.', STATUS)

*
*          If a null value was specified, then annul any error messages
*          and substitute a more appropriate one.


            ELSE IF (STATUS .EQ. PAR__NULL) THEN
               LSTAT = STATUS
               CALL ERR_ANNUL (LSTAT)
               CALL MSG_SETC ('PCNAME', PCNAME)
               CALL ERR_REP ('CAT_ASSOC_NULL',
     :           'CAT_ASSOC: null catalogue specified for parameter '/
     :           /'%^PCNAME.', STATUS)

*
*          For all other errors, add context information and report the
*          error.

            ELSE
               CALL MSG_SETC ('PCNAME', PCNAME)
               CALL ERR_REP ('CAT_ASSOC_ERR',
     :           'CAT_ASSOC: error associating an existing catalogue '/
     :           /'with parameter %^PCNAME.', STATUS)
            END IF
         END IF

*
*       Release the error stack.

         CALL ERR_RLSE

      END IF

      END
