      SUBROUTINE CAP_JTWRT (CIP, CIS, CIOUT, PCOLS, FIPRM, FIPOUT,
     :  SCOLS, FISEC, FISOUT, SEPNI, PMLTI, SMLTI, OPAIR, OPRMPR,
     :  OSECPR, OSEPN, OPMULT, OSMULT, STATUS)
*+
*  Name:
*     CAP_JTWRT
*  Purpose:
*     Write the table for a paired catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_JTWRT (CIP, CIS, CIOUT, PCOLS, FIPRM, FIPOUT,
*       SCOLS, FISEC, FISOUT, SEPNI, PMLTI, SMLTI, OPAIR, OPRMPR,
*       OSECPR, OSEPN, OPMULT, OSMULT; STATUS)
*  Description:
*     Write the table for a paired catalogue.
*  Arguments:
*     CIP  =  INTEGER (Given)
*        Identifier to the primary input catalogue.
*     CIS  =  INTEGER (Given)
*        Identifier to the secondary input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier to the paired output catalogue.
*     PCOLS  =  INTEGER (Given)
*        Number of columns in the primary input catalogue.
*     FIPRM(PCOLS)  =  INTEGER (Given)
*        Identifiers for the columns in the primary input catalogue.
*     FIPOUT(PCOLS)  =  INTEGER (Given)
*        Identifiers for the columns in the output catalogue which
*        correspond to columns in the primary input catalogue.
*     SCOLS  =  INTEGER (Given)
*        Number of columns in the secondary input catalogue.
*     FISEC(SCOLS)  =  INTEGER (Given)
*        Identifiers for the columns in the secondary input catalogue.
*     FISOUT(SCOLS)  =  INTEGER (Given)
*        Identifiers for the columns in the output catalogue which
*        correspond to columns in the secondary input catalogue.
*     SEPNI  =  INTEGER (Given)
*        Identifier for optional column containing the separation of
*        paired objects.  Set to CAT__NOID if the column is not required.
*     PMLTI  =  INTEGER (Given)
*        Identifier for optional column containing the number of
*        primary matches for the row.  Set to CAT__NOID if the column is
*        not required.
*     SMLTI  =  INTEGER (Given)
*        Identifier for optional column containing the number of
*        secondary matches for the row.  Set to CAT__NOID if the column is
*        not required.
*     OPAIR  =  INTEGER (Given)
*        Number of rows in the list of paired rows.
*     OPRMPR(OPAIR)  =  INTEGER (Given)
*        List of primary paired rows.
*     OSECPR(OPAIR)  =  INTEGER (Given)
*        List of secondary paired rows.
*     OSEPN(OPAIR)  =  DOUBLE PRECISION (Given)
*        List of the separation between two paired objects (radians).
*     OPMULT(OPAIR)  =  INTEGER (Given)
*        List of the number of primary objects which the row matches.
*     OSMULT(OPAIR)  =  INTEGER (Given)
*        List of the number of secondary objects which the row matches.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Expand the list of primary column identifiers to include vector
*     elements.
*     Expand the list of secondary column identifiers to include vector
*     elements.
*     If all is ok then
*       For every row in the list of pairs
*         If there is no paired row in the secondary catalogue then
*           Set the fields in the output catalogue corresponding to
*           fields in the secondary catalogue to null.
*         end if
*         If there is a paired row in the primary catalogue then
*           Read this row from the primary catalogue.
*           Copy the primary fields to the output catalogue.
*         else
*           Set the fields in the output catalogue corresponding to
*           fields in the primary catalogue to null.
*         end if
*         If there is a paired row in the secondary catalogue then
*           Read this row from the secondary catalogue.
*           Copy the secondary fields to the output catalogue.
*         end if
*         Write the special columns if required.
*         Append the current output row to the output catalogue.
*       end for
*     end if
*     Report any error.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     16/2/95  (ACD): Original version.
*     17/2/94  (ACD): First stable version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     20/11/95 (ACD): Fixed a bug when writing the output fields for
*        the case where columns from the two input catalogues are
*        written to a single column in the output catalogue.  Values
*        from the primary should be used when there are no values from
*        the secondary.  However, the secondary was always being used,
*        with null values being written when there were no values.
*     18/8/99 (ACD): Added optional output of columns containing the
*        separation of paired objects and the number of primary and
*        secondary matches for each paired row.
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
     :  PCOLS,
     :  FIPRM(PCOLS),
     :  FIPOUT(PCOLS),
     :  SCOLS,
     :  FISEC(SCOLS),
     :  FISOUT(SCOLS),
     :  SEPNI
      INTEGER
     :  PMLTI,
     :  SMLTI,
     :  OPAIR,
     :  OPRMPR(OPAIR),
     :  OSECPR(OPAIR),
     :  OPMULT(OPAIR),
     :  OSMULT(OPAIR)
      DOUBLE PRECISION
     :  OSEPN(OPAIR)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      INTEGER
     :  NUMIDP, ! Total number of ids. in the primary (inc. vector elements).
     :  FIPRME(CAT__MXCOL), ! List of primary input  ids. (inc. vectors).
     :  FIPOUE(CAT__MXCOL), !  "   "     "    output  " . ( " .    "   ).
     :  FIPTYP(CAT__MXCOL), ! Data types for primary identifiers.
     :  NUMIDS, ! Total number of ids. in the secondary (inc. vector elements).
     :  FISECE(CAT__MXCOL), ! List of secondary input  ids. (inc. vectors).
     :  FISOUE(CAT__MXCOL), !  "   "      "     output  " . ( " .    "   ).
     :  FISTYP(CAT__MXCOL), ! Data types for secondary identifiers.
     :  CURPR,  ! Current row in the list of paired rows.
     :  PRMROW, ! Current primary   row.
     :  SECROW  !    "    secondary  " .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Expand the list of primary column identifiers to include vector
*       elements.

         CALL CAP_EXPCL (PCOLS, FIPRM, FIPOUT, CAT__MXCOL, NUMIDP,
     :     FIPRME, FIPOUE, FIPTYP, STATUS)

*
*       Expand the list of secondary column identifiers to include vector
*       elements.

         CALL CAP_EXPCL (SCOLS, FISEC, FISOUT, CAT__MXCOL, NUMIDS,
     :     FISECE, FISOUE, FISTYP, STATUS)

*
*       Proceed if all is ok then

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Copy all the rows in the list of paired objects.

            DO CURPR = 1, OPAIR

*
*             For the current paired row check if there is a
*             corresponding secondary row.  If not then write null
*             values to the fields in the output catalogue.  Note that
*             this check is made first in order to avoid overwriting
*             any values from the primary with nulls in the case where
*             fields from the two input catalogues are being written
*             to a single column in the output catalogue.

               IF (OSECPR(CURPR) .EQ. CAT__UPAIR) THEN
                  CALL CAP_CPNUL (NUMIDS, FISOUE, FISTYP, STATUS)
               END IF

*
*             For the current paired row in the list check if there is
*             a corresponding primary row.  If so then read this row and
*             copy the fields to the output catalogue.  Otherwise copy
*             null values to the appropriate fields in the output
*             catalogue.

               IF (OPRMPR(CURPR) .NE. CAT__UPAIR) THEN
                  PRMROW = OPRMPR(CURPR)
                  CALL CAT_RGET (CIP, PRMROW, STATUS)

                  CALL CAP_CPFLD (NUMIDP, FIPRME, FIPOUE, FIPTYP,
     :              STATUS)
               ELSE
                  CALL CAP_CPNUL (NUMIDP, FIPOUE, FIPTYP, STATUS)
               END IF

*
*             Finally, check if there is a corresponding secondary
*             row.  If so then read this row and copy the fields to the
*             output catalogue.  Note that in the case where there are
*             rows in both the primary and secondary and fields from the
*             catalogues are being written to a single column in the
*             output catalogue then values from the secondary are the
*             ones preserved.

               IF (OSECPR(CURPR) .NE. CAT__UPAIR) THEN
                  SECROW = OSECPR(CURPR)
                  CALL CAT_RGET (CIS, SECROW, STATUS)

                  CALL CAP_CPFLD (NUMIDS, FISECE, FISOUE, FISTYP,
     :              STATUS)
               END IF

*
*             Write the special columns if required.  (If the column
*             is required its identifier is not equal to CAT__NOID.)

               IF (SEPNI .NE. CAT__NOID) THEN
                  IF (OSEPN(CURPR) .GE. 0.0D0) THEN
                     CALL CAT_PUT0D (SEPNI, OSEPN(CURPR), .FALSE.,
     :                 STATUS)
                  ELSE
                     CALL CAT_PUT0D (SEPNI, OSEPN(CURPR), .TRUE.,
     :                 STATUS)
                  END IF
               END IF

               IF (PMLTI .NE. CAT__NOID) THEN
                  CALL CAT_PUT0I (PMLTI, OPMULT(CURPR), .FALSE.,
     :              STATUS)
               END IF

               IF (SMLTI .NE. CAT__NOID) THEN
                  CALL CAT_PUT0I (SMLTI, OSMULT(CURPR), .FALSE.,
     :              STATUS)
               END IF

*
*             Append the current output row to the output catalogue.

               CALL CAT_RAPND (CIOUT, STATUS)
            END DO
         END IF

*
*       If the status is not ok then report an error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CAP_JTWRT_ERR', 'CAP_JTWRT: Failure '/
     :        /'while writing the table for the output catalogue.',
     :        STATUS)
         END IF

      END IF

      END
