      SUBROUTINE ADAM_PRCNAM ( NAME, LENGTH, STATUS )
*+
*  Name:
*     ADAM_PRCNAM

*  Purpose:
*     gives the name by which the task is to register with MESSYS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADAM_PRCNAM ( NAME, LENGTH, STATUS )

*  Description:
*     Obtains name by which the task is to register with MESSYS
*     This is the Unix version - returns either the name set by ICL OR the
*     name of the exe running.

*  Arguments:
*     NAME=CHARACTER*(*) (returned)
*        name, with any path information removed
*     LENGTH=INTEGER (returned)
*        length of the name
*     STATUS=INTEGER

*  Algorithm:
*     If the environment variable ICL_TASK_NAME exists then take the
*     its translation as the MESSYS task name to use.
*     Otherwise get ARG 0, find the end of any path information and take the
*     the remainder as the name.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
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
*     BKM: B K McIlwrath (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-NOV-1992 (AJC):
*        Original Version - dummy
*     23-FEB-1993 (AJC):
*        Version to use exe name
*     26-APR-1993 (AJC):
*        Add the translation of ICL_PARENT_PID to the name where appropriate
*     02-AUG-1993 (BKM):
*        Replace ICL_PARENT_PID by ICL_TASK_NAME
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Returned:
      CHARACTER NAME*(*)   !  process name returned
      INTEGER LENGTH       !  length of name (will be 0-15 chars)

*  Status:
      INTEGER STATUS
*    External References:
      INTEGER CHR_LEN      !  used length of string
      EXTERNAL CHR_LEN
      INTEGER STRING_IANYR !  find char from right
      EXTERNAL STRING_IANYR

*  Local Variables:
      CHARACTER*256 ARGV0
      INTEGER STNM         ! Start of command name

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL EMS_MARK

      CALL PSX_GETENV( 'ICL_TASK_NAME', NAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         LENGTH = CHR_LEN( NAME )
      ELSE
         CALL EMS_ANNUL( STATUS )

         CALL GETARG( 0, ARGV0 )
         LENGTH = CHR_LEN( ARGV0 )
         STNM = STRING_IANYR( ARGV0(1:LENGTH), '/' ) + 1
         NAME = ARGV0(STNM:LENGTH)
         LENGTH = LENGTH - STNM + 1
      ENDIF

      CALL EMS_RLSE

      END
