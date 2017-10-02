      SUBROUTINE POL1_CPCOL( CIIN, CIOUT, MXCOL, REPORT, NUMCOL, FIIN,
     :                       FIOUT, STATUS)
*+
*  Name:
*     POL1_CPCOL

*  Purpose:
*     Create output cat. columns corresponding to the input cat. ones.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPCOL( CIIN, CIOUT, MXCOL, REPORT, NUMCOL, FIIN, FIOUT,
*                      STATUS)

*  Description:
*     Ensure columns exist in the output catalogue corresponding to those
*     in the input catalogue.  Also, lists of identifiers for the columns
*     in both catalogues are returned.

*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     MXCOL  =  INTEGER (Given)
*        Maximum permitted number of catalogues.
*     REPORT =  LOGICAL (Given)
*        If .TRUE., then the required columns should already exist in the
*        output. In which case, report an error if there are any differences
*        between the columns in the input and ouput catalogues. If
*        .FALSE. we know that there are no columns in the supplied output
*        catalogue, and this routine will therefore create them.
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
*     27-SEP-2017 (DSB):
*        Modified to allow new columns to be added into a pre-existing
*        output catalogue that may already contain columns of the same
*        name.
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
      LOGICAL REPORT

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
     :  FCSIZE,      ! Size if a character string.
     :  FDIMS,       ! Dimensionality.
     :  FDTYPE,      ! Data type.
     :  FGENUS,      ! Genus.
     :  FNULL,       ! Null flag.
     :  FORDER,      ! Order.
     :  FSIZEA(10),  ! Size of each array dimension.
     :  GCI,         ! Parent catalogue.
     :  GCSIZE,      ! Size if a character string.
     :  GDIMS,       ! Dimensionality.
     :  GDTYPE,      ! Data type.
     :  GGENUS,      ! Genus.
     :  GNULL,       ! Null flag.
     :  GORDER,      ! Order.
     :  GSIZEA(10)   ! Size of each array dimension.

      CHARACTER
     :  FCOMM*(CAT__SZCOM),    ! Comments.
     :  FEXPR*(CAT__SZEXP),    ! Defining expression.
     :  FNAME*(CAT__SZCMP),    ! Name.
     :  FUNITS*(CAT__SZUNI),   ! Units.
     :  FXCEPT*(CAT__SZVAL),   ! Exception value.
     :  FXTFMT*(CAT__SZEXF),   ! External format.
     :  GCOMM*(CAT__SZCOM),    ! Comments.
     :  GEXPR*(CAT__SZEXP),    ! Defining expression.
     :  GNAME*(CAT__SZCMP),    ! Name.
     :  GUNITS*(CAT__SZUNI),   ! Units.
     :  GXCEPT*(CAT__SZVAL),   ! Exception value.
     :  GXTFMT*(CAT__SZEXF)   ! External format.

      DOUBLE PRECISION
     :  FDATE,       ! Modification date.
     :  FSCALE,      ! Scale factor.
     :  FZEROP,      ! Zero point.
     :  GDATE,       ! Modification date.
     :  GSCALE,      ! Scale factor.
     :  GZEROP       ! Zero point.

      LOGICAL
     :  FPRFDS,     ! Preferential display flag.
     :  GPRFDS      ! Preferential display flag.

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
     :             FDTYPE, FCSIZE, FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :             FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :             FDATE, STATUS )

*  If the output catalogue should already contain a column with this name, we
*  need to check that the column exists and has the correct attributes.
            IF( REPORT ) THEN
               CALL POL1_GTCOL( CIOUT, FNAME, .FALSE., FIOUTC, STATUS )
               IF( FIOUTC .EQ. CAT__NOID ) THEN
                  IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'C', FNAME )
                     CALL ERR_REP( ' ', 'POL1_CPCOL: Column ^C not '//
     :                             'found in supplied output '//
     :                             'catalogue, and REPORT is TRUE '//
     :                             '(possible programming error).',
     :                               STATUS )
                  END IF

*  If the pre-existing column was found...
               ELSE

*  Inquire the values of all its attributes.
                  CALL CAT_CINQ( FIOUTC, 10, GCI, GNAME, GGENUS, GEXPR,
     :                           GDTYPE, GCSIZE, GDIMS, GSIZEA, GNULL,
     :                           GXCEPT, GSCALE, GZEROP, GORDER, GUNITS,
     :                           GXTFMT, GPRFDS, GCOMM, GDATE, STATUS )

*  Check the mutable attributes in the output column are set to the right
*  values. If not, report an error.
                  IF( GEXPR .NE. FEXPR .OR.
     :                GCSIZE .NE. FCSIZE .OR.
     :                GDIMS .NE. FDIMS .OR.
     :                GSIZEA(1) .NE. FSIZEA(1) .OR.
     :                GSCALE .NE. FSCALE .OR.
     :                GZEROP .NE. FZEROP .OR.
     :                GORDER .NE. FORDER .OR.
     :                GDATE .NE. FDATE .OR.
     :                GUNITS .NE. FUNITS .OR.
     :                GXTFMT .NE. FXTFMT .OR.
     :                GPRFDS .NEQV. FPRFDS .OR.
     :                GCOMM .NE. FCOMM ) THEN
                     IF( STATUS .EQ. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'POL1_CPCOL: Existing '//
     :                                'column has unexpected '//
     :                                'attribute values (possible '//
     :                                'programming error).', STATUS )
                     END IF
                  END IF
               END IF

*  If we know that the output catalogue does not contain a column with
*  the current name, create one now.
            ELSE

*  Attempt to create a corresponding column in the output catalogue.
               CALL CAT_PNEW0( CIOUT, CAT__FITYP, FNAME, FDTYPE,
     :                         FIOUTC, STATUS )

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
            END IF

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


