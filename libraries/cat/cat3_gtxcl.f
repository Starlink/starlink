      SUBROUTINE CAT3_GTXCL (TEXT, CLASS, STATUS)
*+
*  Name:
*     CAT3_GTXCL 
*  Purpose:
*     Determine the class of a line of textual information.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GTXCL (TEXT; CLASS; STATUS)
*  Description:
*     Determine the class of a line of textual information extracted
*     from a FITS table.
*
*     The first eight characters of the line of text are examined to
*     define its clas..
*  Arguments:
*     TEXT   =  CHARACTER*(*) (Given)
*         A single line of textual information extracted from the
*         header of a FITS table.
*     CLASS   =  CHARACTER*(*) (Returned)
*         Class of the textual information.  The values permitted are:
*         COMMENT, HISTORY, BLANK and KEYWORD.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Examine the first eight characters of the line of text for a
*     match against any of the classes supported.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     23/9/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  TEXT*(*)
*  Arguments Returned:
      CHARACTER
     :  CLASS*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Examine the first eight characters of the text line to
*       determine the class.  Note that trailing blanks are significant
*       in these comparisons.  Thus, for example, if the first eight
*       characters were 'COMMENTZ', the class would be 'KEYWORD', not
*       'COMMENT'.

         IF (TEXT(1 : 8) .EQ. 'COMMENT ') THEN
            CLASS = 'COMMENT'

         ELSE IF (TEXT(1 : 8) .EQ. 'HISTORY ') THEN
            CLASS = 'HISTORY'

         ELSE IF (TEXT(1 : 8) .EQ. '        ') THEN
            CLASS = 'BLANK'

         ELSE
            CLASS = 'KEYWORD'

         END IF

      END IF

      END
