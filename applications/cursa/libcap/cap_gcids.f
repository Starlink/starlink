      SUBROUTINE CAP_GCIDS (CI, CMPIN, EXPFLG, NAMEIN, EXPRN, UNITS,
     :  CMPOUT, NAMOUT, IDS, STATUS)
*+
*  Name:
*     CAP_GCIDS
*  Purpose:
*     Attempt to get identifiers for a list of components.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCIDS (CI, CMPIN, EXPFLG, NAMEIN, EXPRN, UNITS; CMPOUT,
*       NAMOUT, IDS; STATUS)
*  Description:
*     Attempt to get identifiers for a list of components.  The
*     components may be either columns, expressions or parameters.
*     A vector name is expanded into the names of all its constituent
*     elements and a name and identifier are returned for each element.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     CMPIN  =  INTEGER (Given)
*        Number of components in the input list.
*     EXPFLG(SGZ__MXCMP)  =  LOGICAL (Given)
*        Flag indicating whether the component is an expression.  It is
*        coded as follows:
*        .TRUE.  -  component is an expression,
*        .FALSE. -  component is a column, parameter or vector element.
*     NAMEIN(SGZ__MXCMP)*(*)  =  CHARACTER (Given)
*        Names of the input components.
*     EXPRN(SGZ__MXCMP)  =  CHARACTER*(*) (Given)
*        Defining expression, if the component is an expression.
*     UNITS(SGZ__MXCMP)  =  CHARACTER*(*) (Given)
*        Units of the expression, if the component is an expression.
*     CMPOUT  =  INTEGER (Returned)
*        Number of components in the output list.
*     NAMOUT(SGZ__MXCMP)*(*)  =  CHARACTER (Returned)
*        Names of the output components.  Any vector names in the input
*        list will have a been replaced with the names of all the
*        elements in the vector.
*     IDS(SGZ__MXCMP)  =  INTEGER (Returned)
*        Identifiers for the components.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Initialise the number of output components.
*     For every input component
*       If the component is not an expression then
*         Try to get an identifier for a pre-existing component.
*         If the status is ok then
*           If the identifier is not null then
*             If the identifier does not correspond to a vector element
*             then
*               Determine the dimensionality of the column.
*               If the column is a scalar then
*                 Add the name and identifier to the output list.
*               else if the column is a vector then
*                 Determine the number of elements in the vector.
*                 For every element
*                   Assemble the name of the element.
*                   Get an identifier for the vector element.
*                   If the status is ok then
*                     Add the name and identifier to the output list.
*                   end if
*                 end for
*               end if
*             else the identifier corresponds to a vector element
*               Add the name and identifier to the output list.
*             end if
*           end if
*         else (the component is is expression
*           Try to get an expression identifier for the name.
*           Set the NAME and UNITS attributes for the expression.
*           If the status is ok then
*             Add the name and identifier to the output list.
*           end if
*         end if
*       end if
*       If an identifier had not been obtained successfully then
*         Report an error.
*         Flush the error.
*         Annul the status.
*     end for
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/4/94 (ACD): Original version.
*     12/6/94 (ACD): First stable version.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     30/3/95 (ACD): Revised handling of expressions to include setting
*        the NAME and UNITS attributes.
*     11/4/95 (ACD): Changed the name of the null identifier.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Arguments Given:
      INTEGER
     :  CI, CMPIN
      LOGICAL
     :  EXPFLG(SGZ__MXCMP)
      CHARACTER
     :  NAMEIN(SGZ__MXCMP)*(*),
     :  EXPRN(SGZ__MXCMP)*(*),
     :  UNITS(SGZ__MXCMP)*(*)
*  Arguments Returned:
      INTEGER
     :  CMPOUT,
     :  IDS(SGZ__MXCMP)
      CHARACTER
     :  NAMOUT(SGZ__MXCMP)*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  LOOP,   ! Loop index.
     :  FI,     ! Column identifier.
     :  IDTYPE, ! Type of the current identifier.
     :  DIMS,   ! Dimensionality of the current column.
     :  SIZE,   ! Number of elements in the current column.
     :  ELEM,   ! The current vector element.
     :  ELPOS,  ! Current position in the vector element name.
     :  LNAME,  ! Length of the current column name.
     :  FIEL,   ! Identifier for the current vector element.
     :  EI      ! Expression identifier.
      CHARACTER
     :  ELNAM*(CAT__SZEXP)  ! Name of the current vector element.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Initialise the number of output components.

         CMPOUT = 0

*
*       Process all the component names.

         DO LOOP = 1, CMPIN

*
*          Check whether the component is an expression.

            IF (.NOT. EXPFLG(LOOP) ) THEN

*
*             The component is not an expression; try to get an identifier
*             for a pre-existing component and proceed if ok.

               CALL ERR_MARK
               CALL CAT_TIDNT (CI, NAMEIN(LOOP), FI, STATUS)
               CALL ERR_ANNUL (STATUS)
               CALL ERR_RLSE

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Check whether the identifier is null.

                  IF (FI .NE. CAT__NOID) THEN

*
*                   The identifier is ok; check whether it corresponds
*                   to a vector element.

                     CALL CAT_TIDTP (FI, IDTYPE, STATUS)

                     IF (IDTYPE .NE. CAT__FETYP) THEN

*
*                      The identifier is not a vector element; check
*                      whether it corresponds to a scalar or an entire
*                      vector.

                        CALL CAT_TIQAI (FI, 'DIMS', DIMS, STATUS)

                        IF (DIMS .EQ. CAT__SCALR) THEN

*
*                         The column is a scalar then add it to the output
*                         list.

                           IF (CMPOUT .LT. SGZ__MXCMP) THEN
                              CMPOUT = CMPOUT + 1

                              NAMOUT(CMPOUT) = NAMEIN(LOOP)
                              IDS(CMPOUT) = FI
                           END IF

                        ELSE

*
*                         The column is a vector.  For all its elements
*                         assemble their names, get identifiers for them
*                         and add them to the list.

                           CALL CAT_TIQAI (FI, 'SIZE', SIZE, STATUS)

                           DO ELEM = 1, SIZE
                              ELPOS = 0
                              ELNAM = ' '

                              LNAME = CHR_LEN(NAMEIN(LOOP) )
                              CALL CHR_PUTC (NAMEIN(LOOP)(1 : LNAME),
     :                          ELNAM, ELPOS)

                              CALL CHR_PUTC ('[', ELNAM, ELPOS)
                              CALL CHR_PUTI (ELEM, ELNAM, ELPOS)
                              CALL CHR_PUTC (']', ELNAM, ELPOS)

                              CALL CAT_TIDNT (CI, ELNAM, FIEL, STATUS)

                              IF (STATUS .EQ. SAI__OK  .AND.
     :                            FIEL .NE. CAT__NOID) THEN
                                 IF (CMPOUT .LT. SGZ__MXCMP) THEN
                                    CMPOUT = CMPOUT + 1

                                    NAMOUT(CMPOUT) = ELNAM
                                    IDS(CMPOUT) = FIEL
                                 END IF
                              END IF

                           END DO
                        END IF

                     ELSE

*
*                      The identifier corresponds to a vector element; add
*                      if to the list.

                        IF (CMPOUT .LT. SGZ__MXCMP) THEN
                           CMPOUT = CMPOUT + 1

                           NAMOUT(CMPOUT) = NAMEIN(LOOP)
                           IDS(CMPOUT) = FI
                        END IF
                     END IF

                  END IF

               END IF

            ELSE

*
*             Try to get an expression identifier for the name.

               CALL CAT_EIDNT (CI, EXPRN(LOOP), EI, STATUS)

*
*             Set the NAME and UNITS attributes for the expression.

               CALL CAT_TATTC (EI, 'NAME', NAMEIN(LOOP), STATUS)
               CALL CAT_TATTC (EI, 'UNITS', UNITS(LOOP), STATUS)

*
*             If an identifier has been obtained successfully then
*             copy it to the list.

               IF (STATUS .EQ.SAI__OK) THEN
                  IF (CMPOUT .LT. SGZ__MXCMP) THEN
                     CMPOUT = CMPOUT + 1

                     NAMOUT(CMPOUT) = NAMEIN(LOOP)
                     IDS(CMPOUT) = EI
                  END IF

               END IF
            END IF

*
*          If an identifier had not been obtained successfully for the
*          current component then report an error, flush the error and
*          annul the status.

            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC ('NAMEIN', NAMEIN(LOOP) )
               CALL ERR_REP ('CAP_GCIDS_IVNM', 'Invalid column or '/
     :           /'expression: ^NAMEIN', STATUS)

               CALL ERR_FLUSH (STATUS)
               CALL ERR_ANNUL (STATUS)
            END IF

         END DO

      END IF

      END
