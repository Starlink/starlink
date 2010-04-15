      SUBROUTINE SCULIB_GET_FILENAME(PARAM, FILENAME, STATUS)
*+
*  Name:
*     SCULIB_GET_FILENAME

*  Purpose:
*     Find the filename associated with a parameter

*  Invocation:
*     CALL SCULIB_GET_FILENAME(PARAM, FILENAME, STATUS)

*  Description:
*     This routine finds the name of the filename associated with
*     a NDF parameter.

*  Arguments:
*     PARAM = CHAR (Given)
*       Name of the parameter
*     FILENAME = CHAR (Returned)
*       Name of the file associated with PARAM
*     STATUS = INTEGER (Given & Returned)
*       Global status

*  Implementation Status:
*     Uses SUBPAR

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.3  1999/08/03 19:35:03  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.2  1997/09/05 01:49:34  timj
*     Strip the path and leave just the filename.
*
*     Revision 1.1  1997/09/03 21:54:39  timj
*     Initial revision
*

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions

*  Arguments Given:
      CHARACTER * (*) PARAM

*  Arguments Returned:
      CHARACTER * (*) FILENAME

*  Status:
      INTEGER STATUS                 ! Global status

*  Local Variables:
      INTEGER IPAR                   ! Location in the parameter system
      INTEGER IPOSN                  ! Position in string

*  External references:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Get the filename from the parameter sub-system

      CALL SUBPAR_FINDPAR(PARAM, IPAR, STATUS)
      CALL SUBPAR_GETNAME(IPAR, FILENAME, STATUS)

*     It is possible that this name will include a full path
*     In general we don't want to have a full path as a default name
*     so strip everything except the last '/'

*     Search backwards for a /

      IPOSN = CHR_LEN(FILENAME)

      CALL CHR_FIND(FILENAME, '/', .FALSE., IPOSN)

      IF (IPOSN .GE. 1) THEN
         FILENAME = FILENAME(IPOSN+1:)
      END IF

      END

