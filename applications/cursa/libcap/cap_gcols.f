      SUBROUTINE CAP_GCOLS (CIIN, CIOUT, MAXCOL, TCOLS, COLNAM, COLIDS,
     :  NCOLS, FIIN, FIOUT, STATUS)
*+
*  Name:
*     CAP_GCOLS
*  Purpose:
*     Get identifiers for columns in input and output catalogues.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCOLS (CIIN, CIOUT, MAXCOL; TCOLS, COLNAM, COLIDS;
*       NCOLS, FIIN, FIOUT; STATUS)
*  Description:
*     Get identifiers for the columns to be copied from the input
*     to the output catalogues.  The columns are created in the
*     output catalogue.  The names of the columns are obtained from
*     the parameter system.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     MAXCOL  =  INTEGER (Given)
*        Maximum permitted number of columns.
*     TCOLS  =  INTEGER (Given and Returned)
*        Total current number of columns in the output catalogue.
*     COLNAM(MAXCOL)  =  CHARACTER*(*) (Given and Returned)
*        Names of the columns in the output catalogue.
*     COLIDS(MAXCOL)  =  INTEGER (Given and Returned)
*        Identifiers for the columns in the output catalogue.
*     NCOLS  =  INTEGER (Returned)
*        Number of columns to be copied.
*     FIIN(MAXCOL)  =  INTEGER (Returned)
*        Identifiers for the columns in the input catalogue.
*     FIOUT(MAXCOL)  =  INTEGER (Returned)
*        Identifiers for the corresponding columns in the output
*        catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the number of columns to zero.
*     Do while (more columns are to be input)
*       Obtain a column specification from the environment.
*       If ok then
*         If the specification is non-blank then
*           Convert the specification to upper case.
*           If the specification is not 'END' then
*             If there are any right chevrons in the specification
*             then replace the first with a space.
*             Determine the number of words in the specification.
*             If the number of words is greater than or equal to two
*             then
*               Accept the first word as the name of the column in the
*               input catalogue.
*               Accept the second word as the name of the column in the
*               output catalogue.
*             else
*               Accept the first word as the name of the column in both
*               the input and output catalogues.
*             end if
*             Get identifiers for the input and output columns, and
*             create the output column if necessary.
*           else (end of input has been specified)
*             Set the termination flag.
*           end if
*         end if
*       end if
*       If any error has occurred then
*         Set the termination flag.
*       end if
*     end do
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     20/2/95 (ACD): Original version.
*     21/2/95 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  CIIN,
     :  CIOUT,
     :  MAXCOL
*  Arguments Given and Returned:
      INTEGER
     :  TCOLS,
     :  COLIDS(MAXCOL)
      CHARACTER
     :  COLNAM(MAXCOL)*(*)
*  Arguments Returned:
      INTEGER
     :  NCOLS,
     :  FIIN(MAXCOL),
     :  FIOUT(MAXCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Constants:
      INTEGER MAXWRD         ! Maximum permitted no. of words in a
      PARAMETER (MAXWRD = 3) ! specification.
*  Local Variables:
      LOGICAL
     :  MORE           ! Flag: more columns to be input?
      CHARACTER
     :  COLBUF*75,     ! Current column specification.
     :  WORDS(MAXWRD)*(CAT__SZCMP), ! Words in the current specification.
     :  NAMIN*(CAT__SZCMP),    ! Name of the current input  column.
     :  NAMOUT*(CAT__SZCMP)    !  "   "   "    "     output   "   .
      INTEGER
     :  CHVPOS,        ! Position of first right chevron in specification.
     :  START(MAXWRD), ! Start positions of words in current spec.
     :  STOP(MAXWRD),  ! Stop      "     "    "   "    "      "  .
     :  LSTAT,         ! Local status.
     :  NUMWRD         ! Number of words in the current spec.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the number of columns to zero.

         NCOLS = 0

*
*       Process columns while more are required.

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          obtain a specification for the columns to be copied from the
*          environment, and proceed of ok.

            CALL PAR_GET0C ('COLBUF', COLBUF, STATUS)
            CALL PAR_CANCL ('COLBUF', STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Check that the specification is non-blank.

               IF (COLBUF .NE. ' ') THEN

*
*                Convert the specification to upper case.

                  CALL CHR_UCASE (COLBUF)

*
*                Check whether the specification is signalling that all
*                the required columns have been input.

                  IF (COLBUF .NE. 'END') THEN

*
*                   If there are any right chevrons in the specification
*                   then replace the first with a space (in order to
*                   ensure that there is at least one space between
*                   the input column name and any output column name).

                     CHVPOS = INDEX(COLBUF, '>')

                     IF (CHVPOS .GT. 0) THEN
                        COLBUF(CHVPOS : CHVPOS) = ' '
                     END IF

*
*                   Decompose the specification into its constituent
*                   words.

                     CALL CHR_DCWRD (COLBUF, MAXWRD, NUMWRD, START,
     :                 STOP, WORDS, LSTAT)

*
*                   If the specification contained two or more words
*                   then accept the first and second words as the names
*                   of the column in the input and output catalogues
*                   respectively.  Otherwise accept the first word as the
*                   name in both catalogues.

                     IF (NUMWRD .GE. 2) THEN
                        NAMIN = WORDS(1)
                        NAMOUT = WORDS(2)
                     ELSE
                        NAMIN = WORDS(1)
                        NAMOUT = WORDS(1)
                     END IF

*
*                   Attempt to get identifiers for the specified columns
*                   and, if necessary, create the column in the output
*                   catalogue.

                     CALL CAP_GIDS (CIIN, CIOUT, NAMIN, NAMOUT, MAXCOL,
     :                 TCOLS, COLNAM, COLIDS, NCOLS, FIIN, FIOUT,
     :                 STATUS)

                  ELSE

*
*                   The end of column specification was specified; set
*                   the termination flag.

                     MORE = .FALSE.

                  END IF
               END IF
            END IF

*
*          If any error has occurred then set the termination flag.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.
            END IF

         END DO

      END IF

      END
