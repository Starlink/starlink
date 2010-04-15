      SUBROUTINE CAP_PACOL (CIP, CIS, CIOUT, MXCOL, NCOLP, FIP, FIPOUT,
     :  NCOLS, FIS, FISOUT, STATUS)
*+
*  Name:
*     CAP_PACOL
*  Purpose:
*     Create columns in paired catalogue, correspding to all input cols.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PACOL (CIP, CIS, CIOUT, MXCOL; NCOLP, FIP, FIPOUT,
*       NCOLS, FIS, FISOUT; STATUS)
*  Description:
*     Create columns in a paired output catalogue.  The columns
*     correspond to all the columns in both the secondary and primary
*     catalogues.  If necessary the names of the columns created from
*     the secondary catalogue are changed in order to disambiguate
*     them.  Lists of identifiers for the columns in the input
*     catalogues and the corresponding columns in the output catalogue
*     are returned.
*  Arguments:
*     CIP  =  INTEGER (Given)
*        Identifier for the primary input catalogue.
*     CIS  =  INTEGER (Given)
*        Identifier for the secondary input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the paired output catalogue.
*     MXCOL  =  INTEGER (Given)
*        Maximum permitted number of catalogues.
*     NCOLP  =  INTEGER (Returned)
*        Number of columns in the primary input catalogue.
*     FIP(MXCOL)  =  INTEGER (Returned)
*        Identifiers for all the columns in the primary input catalogue.
*     FIPOUT(MXCOL)  =  INTEGER (Returned)
*        Identifiers for the columns in the output catalogue which
*        correspond to columns in the primary catalogue.
*     NCOLS  =  INTEGER (Returned)
*        Number of columns in the secondary input catalogue.
*     FIS(MXCOL)  =  INTEGER (Returned)
*        Identifiers for all the columns in the secondary input catalogue.
*     FISOUT(MXCOL)  =  INTEGER (Returned)
*        Identifiers for the columns in the output catalogue which
*        correspond to columns in the secondary catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Do while there are more columns in the primary catalogue.
*       Attempt to obtain an identifier for the next column in the
*       primary catalogue.
*       If ok then
*         Inquire the details of the column.
*         Attempt to create a new column in the output catalogue.
*         Set the details of the new column.
*         Copy the identifiers for the input and output columns to
*         the return arrays.
*         Save the name of the column.
*       else
*         Set the termination flag.
*       end if
*       If any error has occurred then
*         Set the termination flag.
*       end if
*     end do
*     If ok then
*       Do while there are more columns in the secondary catalogue.
*         Attempt to obtain an identifier for the next column in the
*         secondary catalogue.
*         If ok then
*           Inquire the details of the column.
*           for every columns in the primary catalogue
*             If the name of the current column from the secondary is
*             the same as the current primary column then
*               Disambiguate the secondary column by appending '_S'
*               to its name.
*             end if
*           end do
*           Attempt to create a new column in the output catalogue.
*           Set the details of the new column.
*           Copy the identifiers for the input and output columns to
*           the return arrays.
*         else
*           Set the termination flag.
*         end if
*         If any error has occurred then
*           Set the termination flag.
*         end if
*       end do
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     13/2/95 (ACD): Original version (based on CAP_CPCOL).
*     17/2/95 (ACD): First stable version.
*     6/3/95  (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     11/4/95 (ACD): Changed the name of the null identifier.
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
     :  CIP,
     :  CIS,
     :  CIOUT,
     :  MXCOL
*  Arguments Returned:
      INTEGER
     :  NCOLP,
     :  FIP(MXCOL),
     :  FIPOUT(MXCOL),
     :  NCOLS,
     :  FIS(MXCOL),
     :  FISOUT(MXCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  MORE      ! Flag: more parameters or columns to access?
      INTEGER
     :  FCOUNT,   ! Number of the current column.
     :  FIINC,    ! Identifier for the current input  column.
     :  FIOUTC,   !     "       "   "     "    output   "   .
     :  LOOP,     ! Loop index.
     :  LFNAME    ! Length of FNAME (excl. trail. blanks).
      CHARACTER
     :  PNAME(CAT__MXCOL)*(CAT__SZCMP) ! Names of columns in the primary.
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
*       Copy each of the columns in the primary catalogue.

         MORE = .TRUE.
         FCOUNT = 0
         NCOLP = 0

         DO WHILE (MORE)

*
*          Attempt to obtain an identifier for the next column in the
*          primary catalogue, and proceed if ok.

            FCOUNT = FCOUNT + 1

            CALL CAT_TNDNT (CIP, CAT__FITYP, FCOUNT, FIINC, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FIINC .NE. CAT__NOID) THEN

*
*             Inquire the values of all the attributes for this column.

               CALL CAT_CINQ (FIINC, 10, FCI, FNAME, FGENUS, FEXPR,
     :           FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :           FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :           FDATE, STATUS)

*
*             Attempt to create a corresponding column in the output
*             catalogue.

               CALL CAT_PNEW0 (CIOUT, CAT__FITYP, FNAME, FDTYPE,
     :           FIOUTC, STATUS)

*
*             Set the mutable attributes of this column to correspond
*             to the input column.  Note that the 'ORDER' attribute is
*             forced to be unordered.

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
*             If all is ok then copy the identifiers for the input
*             and output columns to the return arrays and save the name
*             of the column.

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NCOLP .LT. MXCOL) THEN
                     NCOLP = NCOLP + 1

                     FIP(NCOLP) = FIINC
                     FIPOUT(NCOLP) = FIOUTC

                     PNAME(NCOLP) = FNAME
                  END IF
               END IF
            ELSE

*
*             Either an error has occurred or the last column has been
*             accessed from the input catalogue; set the termination
*             status.

               MORE = .FALSE.
            END IF

*
*          Set the termination flag if any error has occurred.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.
            END IF

         END DO

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Copy each of the columns in the secondary catalogue.

            MORE = .TRUE.
            FCOUNT = 0
            NCOLS = 0

            DO WHILE (MORE)

*
*             Attempt to obtain an identifier for the next column in the
*             secondary catalogue, and proceed if ok.

               FCOUNT = FCOUNT + 1

               CALL CAT_TNDNT (CIS, CAT__FITYP, FCOUNT, FIINC, STATUS)

               IF (STATUS .EQ. CAT__OK  .AND.  FIINC .NE. CAT__NOID)
     :           THEN

*
*                Inquire the values of all the attributes for this column.

                  CALL CAT_CINQ (FIINC, 10, FCI, FNAME, FGENUS, FEXPR,
     :              FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :              FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :              FDATE, STATUS)

*
*                Check that no column with the same name exists in the
*                primary catalogue, and if it does then disambiguate the
*                secondary column by appending '_S' to it.  Note that
*                any modified name is prevented from containing more
*                than the maximum number of characters permitted for a
*                column name.

                  DO LOOP = 1, NCOLP
                     IF (FNAME .EQ. PNAME(LOOP) ) THEN
                        LFNAME = CHR_LEN(FNAME)
                        LFNAME = MIN(LFNAME, CAT__MXCOL-2)
                        CALL CHR_PUTC ('_S', FNAME, LFNAME)
                     END IF
                  END DO

*
*                Attempt to create a corresponding column in the output
*                catalogue.

                  CALL CAT_PNEW0 (CIOUT, CAT__FITYP, FNAME, FDTYPE,
     :              FIOUTC, STATUS)

*
*                Set the mutable attributes of this column to correspond
*                to the input column.  Note that the 'ORDER' attribute
*                is forced to be unordered.


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
*                If all is ok then copy the identifiers for the input
*                and output columns to the return arrays.

                  IF (STATUS .EQ. SAI__OK) THEN
                     IF (NCOLS .LT. MXCOL) THEN
                        NCOLS = NCOLS + 1

                        FIS(NCOLS) = FIINC
                        FISOUT(NCOLS) = FIOUTC
                     END IF
                  END IF
               ELSE

*
*                Either an error has occurred or the last column has been
*                accessed from the input catalogue; set the termination
*                status.

                  MORE = .FALSE.
               END IF

*
*             Set the termination flag if any error has occurred.

               IF (STATUS .NE. SAI__OK) THEN
                  MORE = .FALSE.
               END IF

            END DO

         END IF

      END IF

      END
