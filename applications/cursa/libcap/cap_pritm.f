      SUBROUTINE CAP_PRITM (BUFFER, STATE, LINE, PRSOK, STATUS)
*+
*  Name:
*     CAP_PRITM
*  Purpose:
*     Parse an item from the graphics translation file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PRITM (BUFFER, STATE, LINE; PRSOK; STATUS)
*  Description:
*     Parse an item from the graphics translation file.
*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given)
*        Buffer containing the line.
*     STATE  =  INTEGER (Given)
*        State of the parser: in or out of an IF block.
*     LINE  =  INTEGER (Given)
*        Number of the line in the graphics translation file (for
*        use in parser error messages).
*     PRSOK  =  LOGICAL (Given and Returned)
*        Flag indicating whether the line parsed ok (= .TRUE. if it
*        did).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the line is not completely blank then
*       Attempt to locate the position of the first equals sign.
*       If found then
*         Attempt to extract the name of the item.
*         Attempt to extract the value of the item.
*         If the name is not blank then
*           If the value is not blank then
*             Convert the item name to upper case.
*             Check the item name against each permitted item name
*             and attempt to decode the value.
*           else
*             Report a parser error: missing value.
*             Set the parse failed flag.
*           end if
*         else
*           Report a parser error: missing item name.
*           Set the parse failed flag.
*         end if
*       else
*         Report a parser error: missing equals sign.
*         Set the parse failed flag.
*       end if
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     2/8/96  (ACD): Original version.
*     11/8/96 (ACD): First stable version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*     18/4/01 (ACD): Replaced the single symbol for an ellipse (CIO__SELLP)
*       with separate symbols for open and filled ellipses.
*     20/4/01 (ACD): Removed unused variables.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CIO_CMN'           ! CIO common block.
*  Arguments Given:
      CHARACTER
     :  BUFFER*(*)
      INTEGER
     :  STATE,
     :  LINE
*  Arguments Given and Returned:
      LOGICAL
     :  PRSOK
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  EQLPOS,   ! Position of equals sign in the buffer.
     :  BUFLEN,   ! Length of BUFFER (excl. trail. blanks).
     :  SYMBOL,   ! Current plotting symbol.
     :  COLOUR,   !    "       "     colour.
     :  UNITS,    !    "       "     units.
     :  CURCLS    ! Current IF clause.
      CHARACTER
     :  NAME*(20),           ! Name of the item.
     :  VALUE*(CIO__SZREC),  ! Value of the item.
     :  NAMEU*(20),          ! Name of the item (upper case).
     :  VALUEU*(CIO__SZREC)  ! Value of the item (upper case).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check that the line is not completely blank.

         IF (BUFFER .NE. ' ') THEN

*
*          Attempt to locate the position of the first equals sign
*          and proceed if one is found.

            EQLPOS = INDEX(BUFFER, '=')
            IF (EQLPOS .GT. 0) THEN

*
*             Attempt to extract the name and value of the item.

               NAME = ' '

               IF (EQLPOS .GT. 1) THEN
                  NAME = BUFFER(1 : EQLPOS-1)
               END IF

               VALUE = ' '

               BUFLEN = CHR_LEN(BUFFER)

               IF (EQLPOS .LT. BUFLEN) THEN
                  VALUE = BUFFER(EQLPOS+1 : BUFLEN)
               END IF

*
*             Proceed if a name and value were found.

               IF (NAME .NE. ' ') THEN
                  IF (VALUE .NE. ' ') THEN

*
*                   For both the name and value remove any leading
*                   blanks and convert to upper case (note that the
*                   mixed-case versions are preserved in the original
*                   variables for use in error messages).

                     CALL CHR_LDBLK (NAME)
                     NAMEU = NAME
                     CALL CHR_UCASE (NAMEU)

                     CALL CHR_LDBLK (VALUE)
                     VALUEU = VALUE
                     CALL CHR_UCASE (VALUEU)

*
*                   Check the name against each of the permitted names
*                   attempt to decode the value as appropriate.
*
*                   Plotting symbol.

                     IF (NAMEU .EQ. 'SYMBOL') THEN
                        SYMBOL = CIO__NULL

                        IF (VALUEU .EQ. 'OMIT') THEN
                           SYMBOL = CIO__SOMIT
                        ELSE IF (VALUEU .EQ. 'UNDEFINED') THEN
                           SYMBOL = CIO__SUNDF
                        ELSE IF (VALUEU .EQ. 'DOT') THEN
                           SYMBOL = CIO__SDOT
                        ELSE IF (VALUEU .EQ. 'OPENCIRCLE') THEN
                           SYMBOL = CIO__SOPCR
                        ELSE IF (VALUEU .EQ. 'FILLEDCIRCLE') THEN
                           SYMBOL = CIO__SFLCR
                        ELSE IF (VALUEU .EQ. 'OPENSQUARE') THEN
                           SYMBOL = CIO__SOPSQ
                        ELSE IF (VALUEU .EQ. 'FILLEDSQUARE') THEN
                           SYMBOL = CIO__SFLSQ
                        ELSE IF (VALUEU .EQ. 'OPENTRIANGLE') THEN
                           SYMBOL = CIO__SOPTR
                        ELSE IF (VALUEU .EQ. 'FILLEDTRIANGLE') THEN
                           SYMBOL = CIO__SFLTR
                        ELSE IF (VALUEU .EQ. 'OPENSTAR') THEN
                           SYMBOL = CIO__SOPSR
                        ELSE IF (VALUEU .EQ. 'FILLEDSTAR') THEN
                           SYMBOL = CIO__SFLSR
                        ELSE IF (VALUEU .EQ. 'PLUS') THEN
                           SYMBOL = CIO__SPLUS
                        ELSE IF (VALUEU .EQ. 'MULT') THEN
                           SYMBOL = CIO__SMULT
                        ELSE IF (VALUEU .EQ. 'ASTERISK') THEN
                           SYMBOL = CIO__SAST
                        ELSE IF (VALUEU .EQ. 'XERROR') THEN
                           SYMBOL = CIO__SXERR
                        ELSE IF (VALUEU .EQ. 'YERROR') THEN
                           SYMBOL = CIO__SYERR
                        ELSE IF (VALUEU .EQ. 'XYERROR') THEN
                           SYMBOL = CIO__SXYER
                        ELSE IF (VALUEU .EQ. 'LOZENGE') THEN
                           SYMBOL = CIO__SLOZG
                        ELSE IF (VALUEU .EQ. 'OPENELLIPSE') THEN
                           SYMBOL = CIO__SOELP
                        ELSE IF (VALUEU .EQ. 'FILLEDELLIPSE') THEN
                           SYMBOL = CIO__SFELP
                        ELSE
                           PRSOK = .FALSE.

                           CALL MSG_SETC ('VALUE', VALUE)
                           CALL CAP_PRERR (LINE, '^VALUE is an '/
     :                       /'invalid value.', STATUS)
                        END IF

                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DSYMB__CIO = SYMBOL

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           SYMB__CIO(NIFB__CIO, CURCLS) = SYMBOL

                        END IF

*
*                   Plotting colour.

                     ELSE IF (NAMEU .EQ. 'COLOUR') THEN
                        COLOUR = CIO__NULL

                        IF (VALUEU .EQ. 'DEFAULT') THEN
                           COLOUR = CIO__CDEF
                        ELSE IF (VALUEU .EQ. 'RED') THEN
                           COLOUR = CIO__CRED
                        ELSE IF (VALUEU .EQ. 'GREEN') THEN
                           COLOUR = CIO__CGRN
                        ELSE IF (VALUEU .EQ. 'BLUE') THEN
                           COLOUR = CIO__CBLUE
                        ELSE IF (VALUEU .EQ. 'CYAN') THEN
                           COLOUR = CIO__CCYAN
                        ELSE IF (VALUEU .EQ. 'MAGENTA') THEN
                           COLOUR = CIO__CMAGN
                        ELSE IF (VALUEU .EQ. 'YELLOW') THEN
                           COLOUR = CIO__CYELL
                        ELSE
                           PRSOK = .FALSE.

                           CALL MSG_SETC ('VALUE', VALUE)
                           CALL CAP_PRERR (LINE, '^VALUE is an '/
     :                       /'invalid value.', STATUS)
                        END IF

                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DCOLR__CIO = COLOUR

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           COLR__CIO(NIFB__CIO, CURCLS) = COLOUR

                        END IF

*
*                   Plotting units.

                     ELSE IF (NAMEU .EQ. 'UNITS') THEN
                        UNITS = CIO__NULL

                        IF (VALUEU .EQ. 'FRACTION') THEN
                           UNITS = CIO__UFRAC
                        ELSE IF (VALUEU .EQ. 'ARCSEC') THEN
                           UNITS = CIO__UASEC
                        ELSE IF (VALUEU .EQ. 'ARCMIN') THEN
                           UNITS = CIO__UAMIN
                        ELSE IF (VALUEU .EQ. 'DEGREES') THEN
                           UNITS = CIO__UDEGR
                        ELSE IF (VALUEU .EQ. 'HOURS') THEN
                           UNITS = CIO__UHOUR
                        ELSE IF (VALUEU .EQ. 'RADIANS') THEN
                           UNITS = CIO__URADN
                        ELSE
                           PRSOK = .FALSE.

                           CALL MSG_SETC ('VALUE', VALUE)
                           CALL CAP_PRERR (LINE, '^VALUE is an '/
     :                       /'invalid value.', STATUS)
                        END IF

                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DUNIT__CIO = UNITS

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           UNIT__CIO(NIFB__CIO, CURCLS) = UNITS

                        END IF

*
*                   First size item.

                     ELSE IF (NAMEU .EQ. 'SIZE1') THEN
                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DSIZ1__CIO = VALUE

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           SIZ1__CIO(NIFB__CIO, CURCLS) = VALUE

                        END IF

*
*                   Second size item.

                     ELSE IF (NAMEU .EQ. 'SIZE2') THEN
                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DSIZ2__CIO = VALUE

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           SIZ2__CIO(NIFB__CIO, CURCLS) = VALUE

                        END IF

*
*                   Third size item.

                     ELSE IF (NAMEU .EQ. 'SIZE3') THEN
                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DSIZ3__CIO = VALUE

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           SIZ3__CIO(NIFB__CIO, CURCLS) = VALUE

                        END IF

*
*                   Fourth size item.

                     ELSE IF (NAMEU .EQ. 'SIZE4') THEN
                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DSIZ4__CIO = VALUE

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           SIZ4__CIO(NIFB__CIO, CURCLS) = VALUE

                        END IF

*
*                   Column label item.

                     ELSE IF (NAMEU .EQ. 'LABEL') THEN
                        IF (STATE .EQ. CIO__OUTIB) THEN
                           DLABL__CIO = VALUE

                        ELSE
                           CURCLS = NCLS__CIO(NIFB__CIO)
                           LABL__CIO(NIFB__CIO, CURCLS) = VALUE

                        END IF

                     ELSE
                        PRSOK = .FALSE.

                        CALL MSG_SETC ('NAME', NAME)
                        CALL CAP_PRERR (LINE, '^NAME is an '/
     :                       /'unrecognised name.', STATUS)
                     END IF

                  ELSE
                     PRSOK = .FALSE.

                     CALL CAP_PRERR (LINE, 'Missing value.', STATUS)
                  END IF

               ELSE
                  PRSOK = .FALSE.

                  CALL CAP_PRERR (LINE, 'Missing name.', STATUS)
               END IF

            ELSE
               PRSOK = .FALSE.

               CALL CAP_PRERR (LINE, 'Missing equals sign.', STATUS)
            END IF

         END IF

      END IF

      END
