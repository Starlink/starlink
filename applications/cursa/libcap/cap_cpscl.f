      SUBROUTINE CAP_CPSCL (CIOUT, MXCOL, NUMCOL, FIIN, FIOUT,
     :  STATUS)
*+
*  Name:
*     CAP_CPSCL
*  Purpose:
*     Create output cat. columns corresponding to currently chosen ones.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPSCL (CIOUT, MXCOL; NUMCOL, FIIN, FIOUT; STATUS)
*  Description:
*     Create a set of columns in an output catalogue which correspond
*     to the StarGaze currently chosen columns.  Columns are created
*     for expressions and any vector elements are replaced with the
*     entire vector, but taking care to avoid duplication.
*
*     Lists of identifiers for the columns in both catalogues are returned.
*  Arguments:
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
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
*     For every entry in the list of currently chosen components
*       Determine the type of the component: column, exression or
*       vector column element.
*       If the column is a vector element then
*         Get the identifier of the base column.
*       end if
*       If the identifier is for a column (scalar or vector) then
*         Inquire the attributes of the column.
*         Set the 'add column flag'.
*       else if the identifier is an expression then
*         Invent attributes for the new column.
*         Set the 'add column flag'.
*       end if
*       If the 'add column' flag is set then
*         Attempt to create a column in the output catalogue.
*         Set the attributes for this column.
*         If ok then
*           Increment the number of columns.
*           Copy the details to the return arrays.
*         end if
*       end if
*     end for
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     30/10/94 (ACD): Original version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     30/3/95  (ACD): Modified the way that expressions are copied,
*        so that the name, units and external format are preserved.
*     10/12/96 (ACD): Removed unused argument from calling list.
*     29/7/97  (ACD): Changed the subroutine name to avoid a clash.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
      INCLUDE 'SGZ_PAR'     ! StarGaze symbolic constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'     ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  CIOUT,
     :  MXCOL
*  Arguments Returned:
      INTEGER
     :  NUMCOL,
     :  FIIN(MXCOL),
     :  FIOUT(MXCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      INTEGER
     :  CURCMP,   ! No. of the current component.
     :  CURID,    ! Identifier for the current component.
     :  IDTYPE,   ! Type of the current identifier.
     :  BASEID,   ! Base identifier for vector element.
     :  CURCOL,   ! No. of current column in return argument list.
     :  LFNAME,   ! Length of FNAME (excl. trail. blanks).
     :  NEXPRN,   ! No. of expressions encountered.
     :  FIOUTC    ! Identifier for the current output column.
      LOGICAL
     :  ADDCOL,   ! Flag; add column to the output catalogue?
     :  FOUND     ! Flag; current column alread in output list?
      CHARACTER
     :  EXPR*(CAT__SZEXP) ! Defining string for an expression.

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
     :  FNAME*(CAT__SZCNM),    ! Name.
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
*       Examine every entry in the list of components.

         NEXPRN = 0
         NUMCOL = 0

         DO CURCMP = 1, CMPS__SGZ
            ADDCOL = .FALSE.

*
*          Determine the type of the component.  The possibilities are:
*          column, expression or vector column element.

            CURID = CMPID__SGZ(CURCMP)

            CALL CAT_TIDTP (CURID, IDTYPE, STATUS)

*
*          If the identifier corresponds to a vector element then get
*          the base identifier for the vector.

            IF (IDTYPE .EQ. CAT__FETYP) THEN
               CALL CAT_TIQAI (CURID, 'BASEID', BASEID, STATUS)
               CURID = BASEID
            END IF

*
*          Check for the case where the identifier corresponds to either
*          a column or a vector column element.

            IF (IDTYPE .EQ. CAT__FITYP  .OR.  IDTYPE .EQ. CAT__FETYP)
     :        THEN

*
*             Check if the column is already in the list of components
*             to be copied.  If not then add it to then get its details
*             and set the flag indicating that it is to be added.

               FOUND = .FALSE.

               DO CURCOL = 1, NUMCOL
                  IF (FIIN(CURCOL) .EQ. CURID) THEN
                     FOUND = .TRUE.
                  END IF
               END DO

               IF (.NOT. FOUND) THEN

*
*                Inquire all the attributes of the column.

                  CALL CAT_CINQ (CURID, 10, FCI, FNAME, FGENUS, FEXPR,
     :              FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :              FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :              FDATE, STATUS)

*
*                Set the 'add column' flag.

                  ADDCOL = .TRUE.
               END IF

*
*          Check for the case where the identifier corresponds to an
*          expression.

            ELSE IF (IDTYPE .EQ. CAT__EITYP) THEN

*
*             Invent or copy values for the attributes of the column
*             to be created from the expression.

               CALL CAT_TIQAC (CURID, 'NAME', FNAME, STATUS)

               IF (FNAME .EQ. ' ') THEN
                  LFNAME = 0

                  CALL CHR_PUTC ('EXPRN_', FNAME, LFNAME)

                  NEXPRN =NEXPRN + 1
                  CALL CHR_PUTI (NEXPRN, FNAME, LFNAME)
               END IF

               FGENUS = CAT__GPHYS
               FEXPR = ' '
               FDTYPE = CAT__TYPED
               FCSIZE = 0
               FDIMS = CAT__SCALR
               FSIZEA(1) = 1
               FNULL = CAT__NULLD
               FXCEPT = ' '
               FSCALE = 1.0D0
               FZEROP = 0.0D0
               FORDER = CAT__NOORD
               FPRFDS = .FALSE.

               CALL CAT_TIQAC (CURID, 'EXPR', EXPR, STATUS)
               FCOMM = EXPR

               CALL CAT_TIQAC (CURID, 'UNITS', FUNITS, STATUS)
               CALL CAT_TIQAC (CURID, 'EXFMT', FXTFMT, STATUS)

               CALL CAT1_GTDAT (FDATE, STATUS)

*
*             Set the 'add column' flag.

               ADDCOL = .TRUE.
            END IF

*
*          Proceed if all is ok and  new column is to be created.

            IF (STATUS .EQ. SAI__OK  .AND.  ADDCOL) THEN

*
*             Attempt to create the corresponding column in the output
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
*             If all is ok then copy the identifiers for the input
*             and output columns to the return arrays.

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NUMCOL .LT. MXCOL) THEN
                     NUMCOL = NUMCOL + 1

                     FIIN(NUMCOL) = CURID
                     FIOUT(NUMCOL) = FIOUTC
                  END IF
               END IF

            END IF

         END DO

      END IF

      END
