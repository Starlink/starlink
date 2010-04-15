      SUBROUTINE CAP_GIDS (CIIN, CIOUT, NAMIN, NAMOUT, MAXCOL, TCOLS,
     :  COLNAM, COLIDS, NCOLS, FIIN, FIOUT, STATUS)
*+
*  Name:
*     CAP_GIDS
*  Purpose:
*     Get identifiers for named columns in input and output catalogues.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GIDS (CIIN, CIOUT, NAMIN, NAMOUT, MAXCOL; TCOLS,
*       COLNAM, COLIDS, NCOLS, FIIN, FIOUT; STATUS)
*  Description:
*     Get identifiers for the named columns to be copied from the input
*     to the output catalogues.  The columns are created in the
*     output catalogue.  The identifiers for the named columns are
*     appended to the lists of column identifiers.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     NAMIN  =  CHARACTER*(*) (Given)
*        Name of the column in the input catalogue.
*     NAMOUT  =  CHARACTER*(*) (Given)
*        Name of the column in the output catalogue.
*     MAXCOL  =  INTEGER (Given)
*        Maximum permitted number of columns.
*     TCOLS  =  INTEGER (Given and Returned)
*        Total current number of columns in the output catalogue.
*     COLNAM(MAXCOL)  =  CHARACTER*(*) (Given and Returned)
*        Names of the columns in the output catalogue.
*     COLIDS(MAXCOL)  =  INTEGER (Given and Returned)
*        Identifiers for the columns in the output catalogue.
*     NCOLS  =  INTEGER (Given and Returned)
*        Number of columns to be copied.  Incremented because of the
*        identifiers for the new columns.
*     FIIN(MAXCOL)  =  INTEGER (Given and Returned)
*        Identifiers for the columns in the input catalogue.  Modified
*        by the addition of identifiers for the new columns.
*     FIOUT(MAXCOL)  =  INTEGER (Given and Returned)
*        Identifiers for the corresponding columns in the output
*        catalogue.  Modified by the addition of identifiers for the new
*        columns.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there is space for more columns in the return arrays then
*       Attempt to get an identifier for the column in the input
*       catalogue.
*       If ok then
*         Check if the column already exists in the list of output
*         catalogue columns.
*         If it does exist then
*           Copy the identifiers for the column to the return lists.
*         else
*           Inquire the attributes of the corresponding column in the
*           input catalogue.
*           Attempt to create a column in the output catalogue.
*           Set the attributes for this new column.
*           Copy the identifiers for the column to the return array.
*           Copy the identifier and name to the list for all the
*           columns in the output catalogue.
*         end if
*       else
*         Report error getting identifier for column in the input
*         catalogue.
*       end if
*     else
*       Report message: too many columns.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     20/2/95 (ACD): Original version.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     28/3/97 (ACD): Changed the definition of column and parameter
*        names to use the correct parametric contstant (CAT__SZCMP).
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
      CHARACTER
     :  NAMIN*(*),
     :  NAMOUT*(*)
*  Arguments Given and Returned:
      INTEGER
     :  TCOLS,
     :  COLIDS(MAXCOL),
     :  NCOLS,
     :  FIIN(MAXCOL),
     :  FIOUT(MAXCOL)
      CHARACTER
     :  COLNAM(MAXCOL)*(*)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      LOGICAL
     :  FOUND     ! Flag: found name in list for output catalogue?
      INTEGER
     :  FIINC,    ! Identifier for the current input  column.
     :  FIOUTC,   !     "       "   "     "    output   "   .
     :  LOOP,     ! Loop index.
     :  COLIND    ! Index for name in list for output catalogue.

*
*    The following variables represent the attributes of the current
*    column (or field).

      INTEGER
     :  FCI,         ! Parent catalogue.
     :  FGENUS,      ! Genus.
     :  FDTYPE,      ! Data type.
     :  FCSIZE,      ! Size if a character string.
     :  FDIMS,       ! Dimensionality.
     :  FSIZEA(10),  ! Size of each array dimension.
     :  FNULL,       ! Null flag.
     :  FORDER       ! Order.
      CHARACTER
     :  FNAME*(CAT__SZCMP),    ! Name.
     :  FEXPR*(CAT__SZEXP),    ! Defining expression.
     :  FXCEPT*(CAT__SZVAL),   ! Exception value.
     :  FUNITS*(CAT__SZUNI),   ! Units.
     :  FXTFMT*(CAT__SZEXF),   ! External format.
     :  FCOMM*(CAT__SZCOM)     ! Comments.
      DOUBLE PRECISION
     :  FSCALE,      ! Scale factor.
     :  FZEROP,      ! Zero point.
     :  FDATE        ! Modification date.
      LOGICAL
     :  FPRFDS      ! Preferential display flag.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is space for more columns in the return arrays.

         IF (NCOLS .LT. MAXCOL  .AND.  TCOLS .LT. MAXCOL) THEN

*
*          Attempt to get an identifier for the column in the input
*          catalogue and proceed if ok.

            CALL CAT_TIDNT (CIIN, NAMIN, FIINC, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Check if the specified output column already exists in the
*             list of columns in the output catalogue.

               FOUND = .FALSE.

               DO LOOP = 1, TCOLS
                  IF (NAMOUT .EQ. COLNAM(LOOP) ) THEN
                     FOUND = .TRUE.
                     COLIND = LOOP
                  END IF
               END DO

*
*             If the column already exists then simply copy the
*             identifiers to the return lists.  Otherwise attempt to
*             create the column in the output catalogue, then copy
*             the identifiers to the output lists.

               IF (FOUND) THEN
                  NCOLS = NCOLS + 1

                  FIIN(NCOLS) = FIINC
                  FIOUT(NCOLS) = COLIDS(COLIND)

               ELSE

*
*                A new column must be created in the output catalogue.
*                Inquire the values of all the attributes of the
*                corresponding column in the input catalogue.

                  CALL CAT_CINQ (FIINC, 10, FCI, FNAME, FGENUS, FEXPR,
     :              FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :              FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :              FDATE, STATUS)

*
*                Attempt to create a corresponding column in the output
*                catalogue.  Note that the name used is the name
*                specified, not the name of the corresponding column
*                in the input catalogue.

                  CALL CAT_PNEW0 (CIOUT, CAT__FITYP, NAMOUT, FDTYPE,
     :              FIOUTC, STATUS)

*
*                Set the mutable attributes of this column to correspond
*                to the input column.  Note that the 'ORDER' attribute is
*                forced to be unordered.

                  CALL CAT_TATTC (FIOUTC, 'EXPR', FEXPR, STATUS)
                  CALL CAT_TATTI (FIOUTC, 'CSIZE', FCSIZE, STATUS)
                  CALL CAT_TATTI (FIOUTC, 'DIMS', FDIMS, STATUS)
                  CALL CAT_TATTI (FIOUTC, 'SIZE', FSIZEA(1), STATUS)
                  CALL CAT_TATTD (FIOUTC, 'SCALEF', FSCALE, STATUS)
                  CALL CAT_TATTD (FIOUTC, 'ZEROP', FZEROP, STATUS)
                  CALL CAT_TATTI (FIOUTC, 'ORDER', CAT__NOORD, STATUS)
                  CALL CAT_TATTD (FIOUTC, 'DATE', FDATE, STATUS)
                  CALL CAT_TATTC (FIOUTC, 'UNITS', FUNITS, STATUS)
                  CALL CAT_TATTC (FIOUTC, 'EXFMT', FXTFMT, STATUS)
                  CALL CAT_TATTL (FIOUTC, 'PRFDSP', FPRFDS, STATUS)
                  CALL CAT_TATTC (FIOUTC, 'COMM', FCOMM, STATUS)

*
*                Copy the identifiers to the return arrays.

                  NCOLS = NCOLS + 1

                  FIIN(NCOLS) = FIINC
                  FIOUT(NCOLS) = FIOUTC

*
*                Copy the identifier and name for the column in the
*                output catalogue to the list of all the columns in the
*                output catalogue.

                  TCOLS = TCOLS + 1

                  COLNAM(TCOLS) = NAMOUT
                  COLIDS(TCOLS) = FIOUTC

               END IF

            ELSE

*
*             An error occurred getting an identifier for the column
*             in the input catalogue; report it.

               CALL MSG_SETC ('NAMIN', NAMIN)
               CALL ERR_REP ('CAP_GIDS_NMI', 'CAP_GIDS: Failure '/
     :           /'accessing column ^NAMIN in the input catalogue.',
     :           STATUS)

            END IF

         ELSE

            CALL MSG_OUT (' ', 'The maximum permitted number of '/
     :        /'columns have been entered.', STATUS)

            CALL MSG_SETC ('NAMIN', NAMIN)
            CALL MSG_OUT (' ', 'Column ^NAMIN will be ignored.',
     :        STATUS)

         END IF

      END IF

      END
