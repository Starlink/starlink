      SUBROUTINE CAP_GCPID (CI, MXCOL, COLS, IDS, NAME, STATUS)
*+
*  Name:
*     CAP_GCPID
*  Purpose:
*     Get names and identifiers for preferentially displayed columns.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GCPID (CI, MXCOL; COLS, IDS, NAME; STATUS)
*  Description:
*     Get names and identifiers for any columns in the catalogue which
*     are flagged for preferential display.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     MXCOL  =  INTEGER (Given)
*        Maximum permitted number of columns.
*     COLS  =  INTEGER (Returned)
*        Number of columns chosen for display.
*     IDS(MXCOL)  =  INTEGER (Returned)
*        Identifiers for the chosen columns.
*     NAME(MXCOL)  =  CHARACTER*(*) (Returned)
*        Names of the chosen columns.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Do while (more)
*       Increment to the next column in the catalogue.
*       Attempt to get an identifier for this column.
*       If all ok and the identifier is not null then
*         Attempt to get the name of the column.
*         Attempt to get the preferential display flag for the column.
*         If ok then
*           If the preferential display flag is true then
*             Determine the dimensionality of the column.
*             If the column is a scalar then
*               Add the name and identifier to the output list.
*             else if the column is an array then
*               Determine the number of elements in the array.
*               For every element
*                 Assemble the name of the element.
*                 Get an identifier for the array element.
*                 If the status is ok then
*                   Add the name and identifier to the output list.
*                 end if
*               end for
*             end if
*           end if
*         end if
*       end if
*       If the identifier is null or the status is bad then
*         Set the termination flag.
*       end if
*       If the return arrays for the list of columns are full then
*         Set the termination flag.
*       end if
*     end do
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     2/6/94  (ACD): Original version.
*     12/6/94 (ACD): First stable version.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     11/4/95 (ACD): Changed the name of the null identifier.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  MXCOL
*  Arguments Returned:
      INTEGER
     :  COLS,
     :  IDS(MXCOL)
      CHARACTER
     :  NAME(MXCOL)*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  FI,      ! Identifier for the current column.
     :  CURCOL,  ! Number of the current column.
     :  DIMS,    ! Dimensionality of the current column.
     :  SIZE,    ! Number of elements in the current column.
     :  ELEM,    ! The current array element.
     :  ELPOS,   ! Current position in the array element name.
     :  LNAME,   ! Length of the current column name.
     :  FIEL     ! Identifier for the current array element.
      CHARACTER
     :  FNAME*(CAT__SZCMP),  ! Name of the current column.
     :  ELNAM*(CAT__SZEXP)   ! Name of the current array element.
      LOGICAL
     :  MORE,    ! Flag; more columns to process?
     :  PRFDSP   ! Preferential display flag for the current column.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Attempt to obtain identifiers and names for all the columns
*       in the catalogue for which the preferential display flag is
*       .TRUE., looping until either they have all been obtained, an
*       error occurs or the return arrays for the list are full.

         COLS = 0
         CURCOL = 0
         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Increment to the next column in the catalogue.

            CURCOL = CURCOL + 1

*
*          Attempt to get an identifier for this column.

            CALL CAT_TNDNT (CI, CAT__FITYP, CURCOL, FI, STATUS)

*
*          Proceed if no error has occured and the identifier is not
*          null.

            IF (STATUS .EQ. SAI__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*             Attempt to get the name and the preferential display flag
*             for the column and proceed if all is ok.

               CALL CAT_TIQAC (FI, 'NAME', FNAME, STATUS)
               CALL CAT_TIQAL (FI, 'PRFDSP', PRFDSP, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                If the preferential display flag is true then check whether
*                it corresponds to a scalar or a vector.

                  CALL CAT_TIQAI (FI, 'DIMS', DIMS, STATUS)

                  IF (DIMS .EQ. CAT__SCALR) THEN

*
*                   The column is a scalar then add it to the output
*                   list.

                     IF (COLS .LT. MXCOL) THEN
                        COLS = COLS + 1

                        NAME(COLS) = FNAME
                        IDS(COLS) = FI
                     END IF

                  ELSE

*
*                   The column is an array.  For all its elements
*                   assemble their names, get identifiers for them
*                   and add them to the list.

                     CALL CAT_TIQAI (FI, 'SIZE', SIZE, STATUS)

                     DO ELEM = 1, SIZE
                        ELPOS = 0
                        ELNAM = ' '

                        LNAME = CHR_LEN(FNAME)
                        CALL CHR_PUTC (FNAME(1 : LNAME),
     :                    ELNAM, ELPOS)

                        CALL CHR_PUTC ('[', ELNAM, ELPOS)
                        CALL CHR_PUTI (ELEM, ELNAM, ELPOS)
                        CALL CHR_PUTC (']', ELNAM, ELPOS)

                        CALL CAT_TIDNT (CI, ELNAM, FIEL, STATUS)

                        IF (STATUS .EQ. SAI__OK  .AND.
     :                      FIEL .NE. CAT__NOID) THEN
                           IF (COLS .LT. MXCOL) THEN
                              COLS = COLS + 1

                              NAME(COLS) = ELNAM
                              IDS(COLS) = FIEL
                           END IF
                        END IF

                     END DO
                  END IF

               END IF
            END IF

*
*          If the identifier is null (that is, all the columns have
*          already been obtained) or the status is bad then set the
*          termination flag.

            IF (STATUS .NE. SAI__OK  .OR.  FI .EQ. CAT__NOID) THEN
               MORE = .FALSE.
            END IF

*
*          If the return arrays for the list of columns are full then
*          set the termination flag.  Note that this condition does
*          NOT constitute an error.

            IF (COLS .GE. MXCOL) THEN
               MORE = .FALSE.
            END IF

         END DO

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI ('CURCOL', CURCOL)
            CALL ERR_REP ('CAP_GCPID_ERR', 'CAP_GCPID: error getting '/
     :        /'details for column ^CURCOL.', STATUS)
         END IF

      END IF

      END
