      SUBROUTINE CAP_TBLPH (CIIN, CIOUT, ZENDST, INSCON, ZEROP, ATMOS,
     :  EINSMG, EAIRMS, FCALMG, NUMCOL, FIIN, FIOUT, STATUS)
*+
*  Name:
*     CAP_TBLPH
*  Purpose:
*     Copy a table from an input to an output cat, calculating cal. mag.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_TBLPH (CIIN, CIOUT, ZENDST, INSCON, ZEROP, ATMOS,
*       EINSMG, EAIRMS, FCALMG, NUMCOL, FIIN, FIOUT; STATUS)
*  Description:
*     Copy a table from an input to an output catalogue, calculating
*     a calibarated magnitude from an instrumental magnitude for each
*     row.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier to the input catalogue.  Note that this identifier
*        may be a catalogue or index or a selection.
*     CIOUT  =  INTEGER (Given)
*        Identifier to the output catalogue.
*     ZENDST  =  LOGICAL (Given)
*        Flag indicating whether the air mass is to be computed from
*        the zenith distance or is supplied.  It is coded as follows:
*        .TRUE.  - compute from zenith distance,
*        .FALSE. - supplied.
*     INSCON  =  DOUBLE PRECISION (Given)
*        Arbitrary constant applied to the instrumental magnitudes.
*     ZEROP  =  DOUBLE PRECISION (Given and Returned)
*        Zero point of the transformation.
*     ATMOS  =  DOUBLE PRECISION (Given and Returned)
*        Atmospheric extinction of the transformation.
*     EINSMG  =  INTEGER (Given)
*        Expression identifier for instrumental magnitudes in the input
*        catgalogue.
*     EAIRMS  =  INTEGER (Given)
*        Expression identifier for air masses in the input catalogue.
*        Note that if ZENDST = .TRUE. then the expression should contain
*        the observed zenith distnace, not the air mass.
*     FCALMG  =  INTEGER (Given)
*        Identifier for column of calibrated magnitudes in the output
*        catalogue.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns in the input (and hence output) catalogue.
*     FIIN(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the input catalogue.
*     FIOUT(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Expand the identifiers for the list of columns to be copied to
*     replace any vector identifiers with a set of identifiers for
*     every element in the vector.
*     Determine the number of rows in the input catalogue.
*     If all is ok then
*       For every row in the input catalogue.
*         Read the next row from the input catalogue.
*         If required then
*           Compute the air mass from the zenith distance.
*         end if
*         Compute the calibrated magnitude.
*         Write the new magnitude to the catalogue.
*         Copy the other fields from the input catalogue.
*         Append the current row to the output catalogue.
*       end for
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     6/10/97 (ACD): Original version.
*     7/10/97 (ACD): First stable version.
*     1/12/99 (ACD): Changed SNGL to REAL for Linux.
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
     :  EINSMG,
     :  EAIRMS,
     :  FCALMG,
     :  NUMCOL,
     :  FIIN(NUMCOL),
     :  FIOUT(NUMCOL)
      LOGICAL
     :  ZENDST
      DOUBLE PRECISION
     :  INSCON,
     :  ZEROP,
     :  ATMOS
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      DOUBLE PRECISION SLA_AIRMAS
*  Local Variables:
      DOUBLE PRECISION
     :  VINSMG,  ! Instrumental magnitude for the current row.
     :  VAIRMS   ! Air mass                "   "     "     " .
      LOGICAL
     :  NINSMG,  ! Null flag of instrumental magnitude for current row.
     :  NAIRMS   !  "    "   "  air mass                "     "     " .
      REAL
     :  VCALMG   ! Calibrated magnitude for the current row.
      INTEGER
     :  NUMID,   ! Total number of identifiers (inc. vector elements).
     :  FIINE(CAT__MXCOL),  ! List of input  identifiers (inc. vectors).
     :  FIOUTE(CAT__MXCOL), !  "   "  output      "      ( " .    "   ).
     :  FDTYPE(CAT__MXCOL), ! Data types corresponding to identifiers.
     :  ROWS,    ! Number of rows in the input catalogue.
     :  ROW      ! Current row.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Expand the list of identifiers to be copied, replacing vector
*       identifiers with a set of identifiers corresponding to all the
*       vector elements.

         CALL CAP_EXPCL (NUMCOL, FIIN, FIOUT, CAT__MXCOL, NUMID,
     :     FIINE, FIOUTE, FDTYPE, STATUS)

*
*       Determine the number of rows in the input catalogue.

         CALL CAT_TROWS (CIIN, ROWS, STATUS)

*
*       Proceed if all is ok.

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Copy all the rows, calculating new coordinates for each row.

            DO ROW = 1, ROWS

*
*             Read the next row from the input catalogue.

               CALL CAT_RGET (CIIN, ROW, STATUS)

*
*             Compute the calibrated magnitude.  Note that the input
*             instrumental magnitudes and air mass are checked for null
*             values.  If required then the air is calculated from the
*             observed zenith distance.

               CALL CAT_EGT0D (EINSMG, VINSMG, NINSMG, STATUS)
               CALL CAT_EGT0D (EAIRMS, VAIRMS, NAIRMS, STATUS)

               IF (.NOT. NINSMG  .AND.  .NOT. NAIRMS) THEN
                  IF (ZENDST) THEN
                     VAIRMS = SLA_AIRMAS(VAIRMS)
                  END IF

                  VCALMG = REAL(
     :              VINSMG - INSCON + ZEROP - (ATMOS * VAIRMS) )
                  CALL CAT_PUT0R (FCALMG, VCALMG, .FALSE., STATUS)
               ELSE
                  CALL CAT_PUT0R (FCALMG, 0.0E0, .TRUE., STATUS)
               END IF

*
*             Copy the other fields from the input catalogue.

               CALL CAP_CPFLD (NUMID, FIINE, FIOUTE, FDTYPE, STATUS)

*
*             Append the current row to the output catalogue.

               CALL CAT_RAPND (CIOUT, STATUS)
            END DO
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_TBLPH_ERR', 'CAP_TBLPH: error '/
     :        /'calculating calibrated magnitudes and copying table.',
     :        STATUS)
         END IF

      END IF

      END
