      SUBROUTINE SCULIB_CONSTRUCT_OUT(IN_STRING, ENV_NAME,
     :     N_OPTIONS, OPTIONS, OPTION_STRINGS, OUT_STRING, STATUS)
*+
*  Name:
*     SCULIB_CONSTRUCT_OUT

*  Purpose:
*     Append a identification string to a string

*  Invocation:
*     CALL SCULIB_CONSTRUCT_OUT(IN_STRING, ENV_NAME,
*    :  N_OPTIONS, OPTIONS, OPTION_STRINGS, OUT_STRING, STATUS)

*  Description:
*     Appends an ID string to an input string given the contents
*     of the environment variable specified by ENV_NAME
*     Allowed values of ENV_NAME are stored in OPTIONS and the
*     corresponding string stored in OPTION_STRINGS.
*     Default is to go for the first entry in the IDSTRING array.
*     if the value of ENV_NAME is either not set or unrecognized.

*  Arguments:
*     IN_STRING = CHAR (Given)
*        Input string
*     ENV_NAME = CHAR (Given)
*        Name of environment variable to examine for suffix preference
*     N_OPTIONS = INTEGER (Given)
*        Number of suffix options
*     OPTIONS(N_OPTIONS) = CHAR (Given)
*        Names of different options
*     OPTION_STRINGS(N_OPTIONS) = CHAR (Given)
*        Suffixes for each option. If the string starts with a '!'
*        the input string is chopped from the last '_' before adding
*        the new string.
*     OUT_STRING = CHAR (Returned)
*        Input string with suffix
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.4  1999/08/03 19:34:50  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.3  1997/11/12 00:06:01  timj
*     Make sure that the input string can not equal the output string.
*
*     Revision 1.2  1997/11/08 00:40:58  timj
*     Add the chopping '!' facility
*
*     Revision 1.1  1997/09/03 21:55:09  timj
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
      INTEGER N_OPTIONS
      CHARACTER * (*) ENV_NAME
      CHARACTER * (*) IN_STRING
      CHARACTER * (*) OPTIONS(N_OPTIONS)
      CHARACTER * (*) OPTION_STRINGS(N_OPTIONS)

*  Arguments Returned:
      CHARACTER * (*) OUT_STRING

*  Status:
      INTEGER STATUS                 ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*  Local Constants:
      CHARACTER*(1) CHOP_CHAR        ! Character that indicates a chop
      PARAMETER (CHOP_CHAR = '!')

*  Local Variables:
      CHARACTER * (132)ENV_VALUE     ! Value of environment variable
      INTEGER I                      ! Loop counter
      INTEGER IPOSN                      ! Position in string
      CHARACTER * (32) SUFFIX        ! The chosen suffix


*.

      IF (STATUS .NE. SAI__OK) RETURN

*     N_OPTIONS must be greater than 0
      IF (N_OPTIONS .LT. 1) THEN

         CALL MSG_SETI('N', N_OPTIONS)
         CALL ERR_REP(' ', 'CONSTRUCT_OUT: There must be at least '//
     :        '1 option string (currently ^N)', STATUS)
         RETURN

      END IF

*     Setup the default (assumes SCUBA_SUFFIX is not set)
      SUFFIX = OPTION_STRINGS(1)

*     Get the environment variable value and convert to upper case
      CALL PSX_GETENV(ENV_NAME, ENV_VALUE, STATUS)

      CALL CHR_UCASE(ENV_VALUE)

      IF (STATUS .EQ. SAI__OK) THEN

*     Variable existed

         DO I = 1, N_OPTIONS

            IF (ENV_VALUE .EQ. OPTIONS(I)) THEN
               SUFFIX = OPTION_STRINGS(I)
            END IF

         END DO

      ELSE
*     No variable so just take the long form

         CALL ERR_ANNUL(STATUS)

      END IF

*     First copy the input string to the output
      OUT_STRING = IN_STRING

*     If the SUFFIX contains the chopping character
*     see if we can chop on an underscore

      IF (SUFFIX(1:1) .EQ. CHOP_CHAR) THEN

*     Search from the end of the string for an underscore.

         IPOSN = CHR_LEN(OUT_STRING)
         CALL CHR_FIND(OUT_STRING, '_', .FALSE., IPOSN)

*     Okay so have we found something
         IF (IPOSN .GT. 1) OUT_STRING = OUT_STRING(1:IPOSN-1)

*       Remove the chop character from the suffix string
         SUFFIX = SUFFIX(2:)

      END IF

*     Now just append the new suffix
      IPOSN = CHR_LEN(OUT_STRING)
      CALL CHR_APPND(SUFFIX, OUT_STRING, IPOSN)

*     Check that OUT_STRING does not equal the input string.
*     If it does we should add a little something to the end

      IF (OUT_STRING .EQ. IN_STRING) THEN

         CALL CHR_APPND('_-', OUT_STRING, IPOSN)

      END IF


      END
