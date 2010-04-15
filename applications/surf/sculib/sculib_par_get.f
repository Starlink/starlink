
*+
*  Name:
*     SCULIB_PAR_GET0?

*  Purpose:
*     Wrapper for the standard PAR_GET0 routines

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_PAR_GET0?(PARAM, VALUE, STATUS)

*  Description:
*     This routine provides a wrapper for PAR_GET0? so that
*     STATUS can be checked and an informative error message
*     added without adding large numbers of lines to other routines.

*  Arguments:
*     PARAM  = CHARACTER (Given)
*        Name of requested parameter
*     VALUE  = TYPE (Returned)
*        Parameter value (bad if bad status from PAR_GET)
*     STATUS = INTEGER (Given & Returned)
*        Global status value

*  Implementation Status:
*     - All return types are present as different subroutines

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  1999/08/06 02:24:47  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.3  1999/08/03 19:35:17  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.2  1999/07/30 19:58:15  timj
*     Minor header tweaks.
*
*     Revision 1.1  1997/10/07 07:04:16  timj
*     Initial revision
*
*     1997 October 7 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

***** SCULIB_PAR_GET0I

      SUBROUTINE SCULIB_PAR_GET0I(PARAM, VALUE, STATUS)


*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'               ! VAL__ constants

*  Arguments Given
      CHARACTER * (*) PARAM

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the default value of the parameter
      VALUE = VAL__BADI

*     Find the parameter value
      CALL PAR_GET0I(PARAM, VALUE, STATUS)

*     Check return status

      IF (STATUS .NE. SAI__OK) THEN

         CALL MSG_SETC('PAR', PARAM)
         CALL ERR_REP(' ', 'SCULIB_PAR_GET0I: Error obtaining '//
     :        'value for parameter ^PAR', STATUS)

      END IF

      END


**** SCULIB_PAR_GET0R

      SUBROUTINE SCULIB_PAR_GET0R(PARAM, VALUE, STATUS)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'               ! VAL__ constants

*  Arguments Given
      CHARACTER * (*) PARAM

*  Arguments Returned:
      REAL VALUE

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the default value of the parameter
      VALUE = VAL__BADR

*     Find the parameter value
      CALL PAR_GET0R(PARAM, VALUE, STATUS)

*     Check return status

      IF (STATUS .NE. SAI__OK) THEN

         CALL MSG_SETC('PAR', PARAM)
         CALL ERR_REP(' ', 'SCULIB_PAR_GET0R: Error obtaining '//
     :        'value for parameter ^PAR', STATUS)

      END IF

      END

**** SCULIB_PAR_GET0D

      SUBROUTINE SCULIB_PAR_GET0D(PARAM, VALUE, STATUS)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'               ! VAL__ constants

*  Arguments Given
      CHARACTER * (*) PARAM

*  Arguments Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the default value of the parameter
      VALUE = VAL__BADD

*     Find the parameter value
      CALL PAR_GET0D(PARAM, VALUE, STATUS)

*     Check return status

      IF (STATUS .NE. SAI__OK) THEN

         CALL MSG_SETC('PAR', PARAM)
         CALL ERR_REP(' ', 'SCULIB_PAR_GET0D: Error obtaining '//
     :        'value for parameter ^PAR', STATUS)

      END IF

      END

**** SCULIB_PAR_GET0L

      SUBROUTINE SCULIB_PAR_GET0L(PARAM, VALUE, STATUS)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'               ! VAL__ constants

*  Arguments Given
      CHARACTER * (*) PARAM

*  Arguments Returned:
      LOGICAL VALUE

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the default value of the parameter
      VALUE = .FALSE.

*     Find the parameter value
      CALL PAR_GET0L(PARAM, VALUE, STATUS)

*     Check return status

      IF (STATUS .NE. SAI__OK) THEN

         CALL MSG_SETC('PAR', PARAM)
         CALL ERR_REP(' ', 'SCULIB_PAR_GET0L: Error obtaining '//
     :        'value for parameter ^PAR', STATUS)

      END IF

      END

**** SCULIB_PAR_GET0C

      SUBROUTINE SCULIB_PAR_GET0C(PARAM, VALUE, STATUS)

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'               ! VAL__ constants

*  Arguments Given
      CHARACTER * (*) PARAM

*  Arguments Returned:
      CHARACTER * (*) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the default value of the parameter
      VALUE = ' '

*     Find the parameter value
      CALL PAR_GET0C(PARAM, VALUE, STATUS)

*     Check return status

      IF (STATUS .NE. SAI__OK) THEN

         CALL MSG_SETC('PAR', PARAM)
         CALL ERR_REP(' ', 'SCULIB_PAR_GET0C: Error obtaining '//
     :        'value for parameter ^PAR', STATUS)

      END IF

      END
