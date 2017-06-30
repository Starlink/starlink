      SUBROUTINE POL1_CPCOL( CIIN, CIOUT, MXCOL, NUMCOL, FIIN, FIOUT,
     :                       STATUS)
*+
*  Name:
*     POL1_CPCOL

*  Purpose:
*     Create output cat. columns corresponding to the input cat. ones.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPCOL( CIIN, CIOUT, MXCOL, NUMCOL, FIIN, FIOUT, STATUS)

*  Description:
*     Create columns in the output catalogue corresponding to those in
*     the input catalogue.  Also, lists of identifiers for the columns
*     in both catalogues are returned.

*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
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

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     29-JUN-2017 (DSB):
*        Original version, based on cap_cpcol.f by ACD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing



*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.

*  Arguments Given:
      INTEGER CIIN
      INTEGER CIOUT
      INTEGER MXCOL

*  Arguments Returned:
      INTEGER NUMCOL
      INTEGER FIIN(MXCOL)
      INTEGER FIOUT(MXCOL)

*  Status:
      INTEGER STATUS

*  Local Variables:
      LOGICAL MORE         ! Flag: more parameters or columns to access?
      INTEGER FCOUNT       ! Number of the current column.
      INTEGER FIINC        ! Identifier for the current input  column.
      INTEGER FIOUTC       !     "       "   "     "    output   "   .

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

*  Check inherited status.
      IF (STATUS .NE. SAI__OK) RETURN

*  Copy each of the columns in the input catalogue.
      MORE = .TRUE.
      FCOUNT = 0
      NUMCOL = 0

      DO WHILE( MORE )

*  Attempt to obtain an identifier for the next column in the input
*  catalogue, and proceed if ok.
         FCOUNT = FCOUNT + 1

         CALL CAT_TNDNT( CIIN, CAT__FITYP, FCOUNT, FIINC, STATUS )
         IF( STATUS .EQ. CAT__OK  .AND.  FIINC .NE. CAT__NOID ) THEN

*  Inquire the values of all the attributes for this column.
            CALL CAT_CINQ( FIINC, 10, FCI, FNAME, FGENUS, FEXPR,
     :              FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :              FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :              FDATE, STATUS )

*  Attempt to create a corresponding column in the output catalogue.
            CALL CAT_PNEW0( CIOUT, CAT__FITYP, FNAME, FDTYPE,
     :                      FIOUTC, STATUS )

*  Set the mutable attributes of this column to correspond to the input column.
            CALL CAT_TATTC( FIOUTC, 'EXPR', FEXPR, STATUS )
            CALL CAT_TATTI( FIOUTC, 'CSIZE', FCSIZE, STATUS )
            CALL CAT_TATTI( FIOUTC, 'DIMS', FDIMS, STATUS )
            CALL CAT_TATTI( FIOUTC, 'SIZE', FSIZEA(1), STATUS )
            CALL CAT_TATTD( FIOUTC, 'SCALEF', FSCALE, STATUS )
            CALL CAT_TATTD( FIOUTC, 'ZEROP', FZEROP, STATUS )
            CALL CAT_TATTI( FIOUTC, 'ORDER', FORDER, STATUS )
            CALL CAT_TATTD( FIOUTC, 'DATE', FDATE, STATUS )
            CALL CAT_TATTC( FIOUTC, 'UNITS', FUNITS, STATUS )
            CALL CAT_TATTC( FIOUTC, 'EXFMT', FXTFMT, STATUS )
            CALL CAT_TATTL( FIOUTC, 'PRFDSP', FPRFDS, STATUS )
            CALL CAT_TATTC( FIOUTC, 'COMM', FCOMM, STATUS )

*  If all is ok then copy the identifiers for the input and output columns
*  to the return arrays.
            IF( STATUS .EQ. SAI__OK ) THEN
               IF( NUMCOL .LT. MXCOL ) THEN
                  NUMCOL = NUMCOL + 1

                  FIIN( NUMCOL ) = FIINC
                  FIOUT( NUMCOL ) = FIOUTC
               END IF
            END IF
         ELSE

*  Either an error has occurred or the last column has been accessed from
*  the input catalogue; set the termination status.
            MORE = .FALSE.
         END IF

*  Set the termination flag if any error has occurred.
         IF( STATUS .NE. SAI__OK ) MORE = .FALSE.

      END DO

      END


