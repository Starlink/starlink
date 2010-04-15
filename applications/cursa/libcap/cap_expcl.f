      SUBROUTINE CAP_EXPCL (NUMCOL, FIIN, FIOUT, MAXID, NUMID, FIINE,
     :  FIOUTE, FITYPE, STATUS)
*+
*  Name:
*     CAP_EXPCL
*  Purpose:
*     Expand lists of column ids. to include vector elements.
*  Language:
*     Fortran 77.
*  Invocation:
*     CAP_EXPCL (NUMCOL, FIIN, FIOUT, MAXID; NUMID, FIINE, FIOUTE,
*       FITYPE; STATUS)
*  Description:
*     Given two lists of column identifiers (one for an input catalogue,
*     the other for an output catalogue), generate output lists where
*     the identifiers for any vectors are replaced with a set of
*     identifiers for the individual vector elements.
*
*     Also return a list of data types for the columns.
*  Arguments:
*     NUMCOL  =  INTEGER (Given)
*        Number of input identifiers.
*     FIIN(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the input columns in the input catalogue.
*     FIOUT(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the input columns in the output catalogue.
*     MAXID  =  INTEGER (Given)
*        Maximum permitted number of identifiers in the output list.
*     NUMID  =  INTEGER (Returned)
*        Number of output identifiers, with vectors replaced by a
*        list of vector element identifiers.
*     FIINE(MAXID)  =  INTEGER (Returned)
*        Generated list for the input catalogue.
*     FIOUTE(MAXID)  =  INTEGER (Returned)
*        Generated list for the output catalogue.
*     FITYPE(MAXID)  =  INTEGER (Returned)
*        Data types for the columns to which the generated lists of
*        identifiers correspond.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every column identifier in the input catalogue
*       Get the data type of the column.
*       Get the dimensionality of the column.
*       If the column is a scalar then
*         Add the column to the list of columns to copy.
*       else (the column is a vector)
*         Determine the parent input and output catalogues.
*         Determine the number of elements in the vector.
*         Determine the name of the vector.
*         For every element in the vector
*           Get an identifier for the element.
*           Add the element to the list of columns to copy.
*         end for
*       end if
*     end for
*     If not ok then
*       Report error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/2/95 (ACD): Original version (based on CAP_CPTAB).
*     17/2/95 (ACD): First stable version.
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
      INCLUDE 'CAT_ERR'     ! Symbolic constants for CAT error codes.
*  Arguments Given:
      INTEGER
     :  NUMCOL,
     :  FIIN(NUMCOL),
     :  FIOUT(NUMCOL),
     :  MAXID
*  Arguments Returned:
      INTEGER
     :  NUMID,
     :  FIINE(MAXID),
     :  FIOUTE(MAXID),
     :  FITYPE(MAXID)
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CURCOL,  ! Number of the current column.
     :  FIINC,   ! Identifier to current input  column.
     :  FIOUTC,  !     "      "     "    output   "   .
     :  IDTYPE,  ! Type of the identifier to current input column.
     :  DTYPE,   ! Data type of current column.
     :  DIMS,    ! Dimensionality of the current column.
     :  SIZE,    ! Size (number of elements) of the current column.
     :  CIIN,    ! Identifier for input  catalogue.
     :  CIOUT,   !     "       "  output     "    .
     :  ELEM,    ! Current vector element.
     :  ENMPOS,  ! Current position in ENAME.
     :  LFNAME,  ! Length of FNAME (excl. trail. blanks).
     :  EIINE,   ! Identifier to current input  scalar or vector element.
     :  EIOUTE   !     "      "     "    output   "    "    "       "   .
      CHARACTER
     :  FNAME*(CAT__SZCMP),  ! Name of the current columnt.
     :  ENAME*(CAT__SZCMP)   ! Name of the current vector element.
*.

      IF (STATUS .EQ. SAI__OK) THEN

         NUMID = 0

*
*       Determine the data type and dimensionality of every column in
*       the input catalogue.  If the column is a vector then get
*       identifiers for all the individual elements.

         DO CURCOL = 1, NUMCOL
            FIINC = FIIN(CURCOL)
            FIOUTC = FIOUT(CURCOL)

*
*          Get the data type and dimensionality of the column.  Note
*          that expressions are scalars with a data type of DOUBLE
*          PRECISION.

            CALL CAT_TIDTP (FIINC, IDTYPE, STATUS)

            IF (IDTYPE .NE. CAT__EITYP) THEN
               CALL CAT_TIQAI (FIINC, 'DTYPE', DTYPE, STATUS)
               CALL CAT_TIQAI (FIINC, 'DIMS', DIMS, STATUS)
            ELSE
               DTYPE = CAT__TYPED
               DIMS = CAT__SCALR
            END IF

*
*          Check whether the column is a scalar or a vector.

            IF (DIMS .EQ. CAT__SCALR) THEN

*
*             Add the identifier to the list of columns to copy.

               IF (STATUS .EQ. SAI__OK) THEN
                  IF (NUMID .LT. CAT__MXCOL) THEN
                     NUMID = NUMID + 1

                     FIINE(NUMID) = FIINC
                     FIOUTE(NUMID) = FIOUTC
                     FITYPE(NUMID) = DTYPE
                  END IF
               END IF
            ELSE

*
*             The column is a vector.  First obtain identifiers for the
*             parent input and output catalogues.  Then determine the number
*             of elements in the column and and its name. Finally get an
*             identifier for each element.

               CALL CAT_TIDPR (FIINC, CIIN, STATUS)
               CALL CAT_TIDPR (FIOUTC, CIOUT, STATUS)

               CALL CAT_TIQAI (FIINC, 'SIZE', SIZE, STATUS)
               CALL CAT_TIQAC (FIINC, 'NAME', FNAME, STATUS)

               DO ELEM = 1, SIZE
                  ENAME = ' '
                  ENMPOS = 0

                  IF (FNAME .NE. ' ') THEN
                     LFNAME = CHR_LEN(FNAME)
                  ELSE
                     LFNAME = 1
                  END IF

                  CALL CHR_PUTC (FNAME(1 : LFNAME), ENAME, ENMPOS)
                  CALL CHR_PUTC ('[', ENAME, ENMPOS)
                  CALL CHR_PUTI (ELEM, ENAME, ENMPOS)
                  CALL CHR_PUTC (']', ENAME, ENMPOS)

                  CALL CAT_TIDNT (CIIN, ENAME, EIINE, STATUS)
                  CALL CAT_TIDNT (CIOUT, ENAME, EIOUTE, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
                     IF (NUMID .LT. CAT__MXCOL) THEN
                        NUMID = NUMID + 1

                        FIINE(NUMID) = EIINE
                        FIOUTE(NUMID) = EIOUTE
                        FITYPE(NUMID) = DTYPE
                     END IF
                  END IF
               END DO

            END IF
         END DO

*
*       If the status is not ok then report an error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_EXPCL_ERR', 'CAP_EXPCL: Failure '/
     :        /'replacing vector ids. with corresponding element ids.',
     :        STATUS)
         END IF

      END IF

      END
