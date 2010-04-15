      SUBROUTINE CAP_CPTGC (CIIN, CIOUT, RAC, DECC, MXCOL, NUMCOL,
     :  FIIN, FIOUT, STATUS)
*+
*  Name:
*     CAP_CPTGC
*  Purpose:
*     Create target list columns corresponding to the input cat. ones.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPTGC (CIIN, CIOUT, RAC, DECC, MXCOL; NUMCOL, FIIN, FIOUT;
*       STATUS)
*  Description:
*     Create columns in an output target list corresponding to those in
*     an input catalogue.  Also, lists of identifiers for the columns
*     in both catalogues are returned.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     RAC  =  CHARACTER*(*) (Given)
*        Name of Right Ascension column.
*     DECC  =  CHARACTER*(*) (Given)
*        Name of Declination column.
*     MXCOL  =  INTEGER (Given)
*        Maximum permitted number of catalogues.
*     NUMCOL  =  INTEGER (Returned)
*        Number of columns in the input (and hence output) catalogue.
*     FIIN(MXCOL)  =  INTEGER (Returned)
*        identifiers for the columns in the input catalogue.
*     FIOUT(MXCOL)  =  INTEGER (Returned)
*        identifiers for the columns in the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     The salient point to note about this algorithm is that it ensures
*     that the Right Ascension and Declination occur as respectively
*     the first and second columns in the target list and that their
*     names are 'RA' and 'DEC'.
*
*     Attempt to get an identifier for the Right Ascension column.
*     Add it to the list of input identifiers.
*     Attempt to get an identifier for the Declination column.
*     Add it to the list of input identifiers.
*     Do while there are more columns in the input catalogue.
*       Attempt to get an identifier for the next column.
*       If ok then
*         If the identifier is not equal to the identifier for the
*         Right Ascension or Declination then
*           Add the identifier to the list of input column identifiers.
*         end if
*       else
*         Set the termination flag.
*       end if
*       If there is any bad status then
*         Set the termination flag.
*       end if
*     end do
*     If all ok then
*       For every input column identifier
*         Inquire all the attributes of the column.
*         If the identifier corresponds to the Right Ascension or
*         Declination then
*           Reset the column name.
*         end if
*         Attempt to create a new column in the output catalogue.
*         Set the mutable attributes of the new column.
*         Copy the identifier for the output columns to the return
*         array.
*       end for
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/9/96  (ACD): Original version (loosely based on CAP_CPCOL).
*     28/3/97  (ACD): Changed the definition of column and parameter
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
     :  MXCOL
      CHARACTER
     :  RAC*(*),
     :  DECC*(*)
*  Arguments Returned:
      INTEGER
     :  NUMCOL,
     :  FIIN(MXCOL),
     :  FIOUT(MXCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      LOGICAL
     :  MORE      ! Flag: more parameters or columns to access?
      INTEGER
     :  FRAI,     ! Identifier for the Right Ascension column.
     :  FDECI,    !     "       "   "  Declination       "   .
     :  CURCOL,   ! Number of the current column.
     :  FCOUNT,   ! Number of the current identifier.
     :  FIINC,    ! Identifier for the current input  column.
     :  FIOUTC    !     "       "   "     "    output   "   .

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
*       Attempt to get an identifier for the Right Ascension column
*       and add it to the list of input identifiers.

         CALL CAT_TIDNT (CIIN, RAC, FRAI, STATUS)
         FIIN(1) = FRAI

*
*       Attempt to get an identifier for the Declination column and
*       add it to the list of input identifiers.

         CALL CAT_TIDNT (CIIN, DECC, FDECI, STATUS)
         FIIN(2) = FDECI

*
*       Attempt to get identifiers for the remaining columns in the
*       input catalogue.

         MORE = .TRUE.
         FCOUNT = 0
         CURCOL = 2

         DO WHILE (MORE)

*
*          Attempt to get an identifier for the next column and
*          proceed if ok.

            FCOUNT = FCOUNT + 1

            CALL CAT_TNDNT (CIIN, CAT__FITYP, FCOUNT, FIINC, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FIINC .NE. CAT__NOID) THEN

*
*             If the identifier is not equal to the identifier for the
*             Right Ascension or Declination then add it to the list of
*             input column identifiers.

               IF (FIINC .NE. FRAI  .AND.  FIINC .NE.  FDECI) THEN
                  CURCOL = CURCOL + 1
                  FIIN(CURCOL) = FIINC
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
*       Set the number of columns found in the input catalogue.

         NUMCOL = CURCOL

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Copy all the columns in the input catalogue.

            DO CURCOL = 1, NUMCOL
               FIINC = FIIN(CURCOL)

*
*             Inquire the values of all the attributes for this column.

               CALL CAT_CINQ (FIINC, 10, FCI, FNAME, FGENUS, FEXPR,
     :           FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :           FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :           FDATE, STATUS)

*
*             If the identifier corresponds to the Right Ascension or
*             Declination then reset the name.

               IF (FIINC .EQ. FRAI) THEN
                  FNAME = 'RA'
               END IF

               IF (FIINC .EQ. FDECI) THEN
                  FNAME = 'DEC'
               END IF

*
*             Attempt to create a corresponding column in the output
*             catalogue.

               CALL CAT_PNEW0 (CIOUT, CAT__FITYP, FNAME, FDTYPE,
     :           FIOUTC, STATUS)

*
*             Set the mutable attributes of this column to correspond
*             to the input column.

               CALL CAT_TATTC (FIOUTC, 'EXPR', FEXPR, STATUS)
               CALL CAT_TATTI (FIOUTC, 'CSIZE', FCSIZE, STATUS)
               CALL CAT_TATTI (FIOUTC, 'DIMS', FDIMS, STATUS)
               CALL CAT_TATTI (FIOUTC, 'SIZE', FSIZEA(1), STATUS)
               CALL CAT_TATTD (FIOUTC, 'SCALEF', FSCALE, STATUS)
               CALL CAT_TATTD (FIOUTC, 'ZEROP', FZEROP, STATUS)
               CALL CAT_TATTI (FIOUTC, 'ORDER', FORDER, STATUS)
               CALL CAT_TATTD (FIOUTC, 'DATE', FDATE, STATUS)
               CALL CAT_TATTC (FIOUTC, 'UNITS', FUNITS, STATUS)
               CALL CAT_TATTC (FIOUTC, 'EXFMT', FXTFMT, STATUS)
               CALL CAT_TATTL (FIOUTC, 'PRFDSP', FPRFDS, STATUS)
               CALL CAT_TATTC (FIOUTC, 'COMM', FCOMM, STATUS)

*
*             Copy the identifier for the output catalogue to the return
*             array.

               FIOUT(CURCOL) = FIOUTC
            END DO
         END IF

      END IF

      END
