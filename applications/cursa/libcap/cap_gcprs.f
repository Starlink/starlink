      SUBROUTINE CAP_GCPRS (CMPLST, CMPS, EXPFLG, NAME, EXPRN, UNITS,
     :  STATUS)
*+
*  Name:
*     CAP_GCPRS
*  Purpose:
*     Parse a string holding holding a list of components.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCPRS (CMPLST; CMPS, EXPFLG, NAME, EXPRN, UNITS; STATUS)
*  Description:
*     Parse a string holding holding a list of components to get a
*     list of component names.  Component names in the input list should
*     be separated by semi-colons.
*
*     The syntax for each component is as follows:
*
*     Column, parameter or vector element:
*
*        name
*
*     Expression:
*
*        name{expression}units
*
*     'name' and 'expression' or mandatory; 'units' is optional.
*  Arguments:
*     CMPLST  =  CHARACTER*(*) (Given and returned)
*        List of names separated by semi-colons (the list is modified
*        during the parsing).
*     CMPS  =  INTEGER (Returned)
*        Number of names found in the list.
*     EXPFLG(SGZ__MXCMP)  =  LOGICAL (Returned)
*        Flag indicating whether the component is an expression.  It is
*        coded as follows:
*        .TRUE.  -  component is an expression,
*        .FALSE. -  component is a column, parameter or vector element.
*     NAME(SGZ__MXCMP)  =  CHARACTER*(*) (Returned)
*        Names found in the list.
*     EXPRN(SGZ__MXCMP)  =  CHARACTER*(*) (Returned)
*        Defining expression, if the component is an expression.
*     UNITS(SGZ__MXCMP)  =  CHARACTER*(*) (Returned)
*        Units of the expression, if the component is an expression.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Clean the string.
*     If the string is not entirely blank then
*       Strip any leading or trailing delimiters.
*       Examine the string to find the positions of any delimiters.
*       Use the delimiters to calculate the starting and stoping
*       positions of any names in the string.
*       for each component
*         Check for the occurence of the delimiters ('{' and '}')
*         which will determine whether the component is an expression
*         or not.
*         If the component is an expression then
*           Determine the starting and stopping positions of the name,
*           expression and any units.
*           Copy the name and expression to the return arrays.
*           Copy any units to the return arrays.
*         else (the component is not an expression)
*           Copy the name to the return array.
*           Set the expression and units to blank.
*         end if
*       end for
*     else
*       Set the number of components to zero.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/4/94 (ACD): Original version.
*     30/3/95 (ACD): First stable version.
*     7/11/97 (ACD): Fixed a transient bug which sometimes resulted
*       in an invalid string when an expression was given without any
*       units.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Arguments Given:
      CHARACTER
     :  CMPLST*(*)
*  Arguments Returned:
      INTEGER
     :  CMPS
      LOGICAL
     :  EXPFLG(SGZ__MXCMP)
      CHARACTER
     :  NAME(SGZ__MXCMP)*(*),
     :  EXPRN(SGZ__MXCMP)*(*),
     :  UNITS(SGZ__MXCMP)*(*)
*  Status:
      INTEGER STATUS         ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      CHARACTER DELIM*1      ! Delimiter for components.
      PARAMETER (DELIM = ';')
*
      CHARACTER EXPDLM*1     ! Delimiter for expressions.
      PARAMETER (EXPDLM = '{')
*
      CHARACTER UNTDLM*1     ! Delimiter for expression units.
      PARAMETER (UNTDLM = '}')
*  Local Variables:
      INTEGER
     :  DPOS(SGZ__MXCMP),    ! Positions for the delimiters.
     :  START(SGZ__MXCMP),   ! Start positions for the names.
     :  STOP(SGZ__MXCMP),    ! Stop      "      "   "    "  .
     :  LOOP,                ! Loop index.
     :  LCMPLS,              ! Length of CMPLST (excl. trail. blanks).
     :  DELIMS               ! Number of delimiters.
      INTEGER
     :  EXPPOS,   ! Position of expression delimiter.
     :  UNTPOS,   ! Position of expression units delimiter.
     :  STRNME,   ! Start position of expression name.
     :  STPNME,   ! Stop     "     "      "       "  .
     :  STREXP,   ! Start    "     "  expression.
     :  STPEXP,   ! Stop     "     "      "     .
     :  STRUNT,   ! Start    "     "  expression units.
     :  STPUNT    ! Stop     "     "      "        "  .
      LOGICAL
     :  UNITFL    ! Flag; does the current expression have units?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Clean the string; remove any unprintable characters.

         CALL CHR_CLEAN (CMPLST)

*
*       Check that the string is not entirely blank.

         IF (CMPLST .NE. ' ') THEN

*
*          Strip any leading or trailing delimiters.

            CALL CHR_LDBLK (CMPLST)

            LCMPLS = CHR_LEN (CMPLST)

            IF (CMPLST(1 : 1) .EQ. DELIM) THEN
               CMPLST(1 : 1) = ' '
            END IF

            IF (CMPLST(LCMPLS : LCMPLS) .EQ. DELIM) THEN
               CMPLST(LCMPLS : LCMPLS) = ' '
            END IF

*
*          Remove any leading blanks.

            CALL CHR_LDBLK (CMPLST)

*
*          Examine the string to find the number and positions of any
*          delimiters.

            DELIMS = 0

            LCMPLS = CHR_LEN (CMPLST)

            DO LOOP = 1, LCMPLS
               IF (CMPLST(LOOP : LOOP) .EQ. DELIM) THEN
                  IF (DELIMS .LT. SGZ__MXCMP-1) THEN
                     DELIMS = DELIMS + 1
                     DPOS(DELIMS) = LOOP
                  END IF
               END IF
            END DO

*
*          Use the delimiters to calculate the starting and stoping
*          positions of any names in the string.

            START(1) = 1

            DO LOOP = 1, DELIMS
               STOP(LOOP) = DPOS(LOOP) - 1
               START(LOOP + 1) = DPOS(LOOP) + 1
            END DO

            STOP(DELIMS + 1) = LCMPLS

            CMPS = DELIMS + 1

*
*          Copy the components found to the return arrays.

            DO LOOP = 1, CMPS

*
*             Attempt to locate the delimiters for expressions and
*             expression units.

               EXPPOS = INDEX(CMPLST(START(LOOP) : STOP(LOOP) ), EXPDLM)
               UNTPOS = INDEX(CMPLST(START(LOOP) : STOP(LOOP) ), UNTDLM)

*
*             If there is an expression delimiter then the component is
*             an expression; otherwise it is a column, parameter or
*             vector element.

               IF (EXPPOS .GT. 0) THEN
                  EXPPOS = EXPPOS + START(LOOP) - 1

*
*                The component is an expression; determine the starting
*                and stopping positions for the name, expression and any
*                units.

                  STRNME = START(LOOP)
                  STPNME = EXPPOS - 1

                  STREXP = EXPPOS + 1

                  IF (UNTPOS .GT. 0) THEN
                     UNTPOS = UNTPOS + START(LOOP) - 1

                     STPEXP = UNTPOS - 1

                     IF (UNTPOS .LT. STOP(LOOP) ) THEN
                        STRUNT = UNTPOS + 1
                        STPUNT = STOP(LOOP)

                        UNITFL = .TRUE.
                     ELSE
                        UNITFL = .FALSE.
                     END IF

                  ELSE
                     STPEXP = STOP(LOOP)

                  END IF

*
*                Set the expression flag, copy the name and expression
*                and, if present, units.

                  EXPFLG(LOOP) = .TRUE.

                  NAME(LOOP) = CMPLST(STRNME : STPNME)
                  EXPRN(LOOP) = CMPLST(STREXP : STPEXP)

                  IF (UNITFL) THEN
                     UNITS(LOOP) = CMPLST(STRUNT : STPUNT)
                  ELSE
                     UNITS(LOOP) = ' '
                  END IF

               ELSE

*
*                The component is a column, parameter or vector element;
*                set the expression flag, copy the name and set the
*                expression and units to blank.

                  EXPFLG(LOOP) = .FALSE.

                  NAME(LOOP) = CMPLST(START(LOOP) : STOP(LOOP) )
                  EXPRN(LOOP) = ' '
                  UNITS(LOOP) = ' '

               END IF

            END DO

         ELSE

*
*          The string is blank; set the number of components to zero.

            CMPS = 0
         END IF

      END IF

      END
