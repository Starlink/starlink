      SUBROUTINE CAP_CPCCL (CRDTYP, CRDOUT, CIIN, CIOUT, MXCOL,
     :  CRDOI, NUMCOL, FIIN, FIOUT, STATUS)
*+
*  Name:
*     CAP_CPCCL
*  Purpose:
*     Copy column defns. for output cat., creating new cols. for coords.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPCCL (CRDTYP, CRDOUT, CIIN, CIOUT, MXCOL; CRDOI, NUMCOL,
*       FIIN, FIOUT; STATUS)
*  Description:
*     Create column definitions for an output catalogue from those
*     for an input catalogue, but creating columns to hold new
*     celestial coordinates.
*
*     Note: if there are columns in the input catalogue with the same
*     name as the new celestial coordinates.
*  Arguments:
*     CRDTYP  =  INTEGER (Given)
*        Code indicating the type of coordinates which are to be
*        calculated.
*     CRDOUT(2)  =  CHARACTER*(*) (Given)
*        Names of the columns to hold the new Right Ascension and
*        Declination or Galactic longitude and latitude.  The names
*        are stored in the array elements as follows:
*        1 - Right Ascension or Galactic longitude,
*        2 - Declination or Galactic latitude.
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     MXCOL  =  INTEGER (Given)
*        Maximum permitted number of catalogues.
*     CRDOI  =  INTEGER (Returned)
*        Identifiers for the new celestial coordinates in the output
*        catalogue.
*     NUMCOL  =  INTEGER (Returned)
*        Number of columns in the input (and hence output) catalogue.
*     FIIN(MXCOL)  =  INTEGER (Returned)
*        identifiers for the columns in the input catalogue.
*     FIOUT(MXCOL)  =  INTEGER (Returned)
*        identifiers for the columns in the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Create columns in the output catalogue for the appropriate
*     type of coordinates.
*     Do while there are more columns in the input catalogue.
*       Attempt to obtain an identifier for the next colunm in the
*       input catalogue.
*       If ok then
*         Inquire the details of the column.
*         If the name of the column does not match either of the names
*         columns for the new coordinates then
*           Attempt to create a new column in the output catalogue.
*           Set the details of the new column.
*           Copy the identifiers for the input and output columns to
*           the return arrays.
*         end if
*       else
*         Set the termination flag.
*       end if
*       If any error has occurred then
*         Set the termination flag.
*       end if
*     end do
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     29/5/97 (ACD): Original version (from CAP_CPCOL).
*     6/10/97 (ACD): Corrected error in the prologue comments.
*     20/5/98 (ACD): Modified for additional types of coordinates:
*        observed equatorial, local equatorial, horizon, and
*        supergalactic.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
      INCLUDE 'CAP_PAR'     ! CAP symbolic constants.
*  Arguments Given:
      INTEGER
     :  CRDTYP,
     :  CIIN,
     :  CIOUT,
     :  MXCOL
      CHARACTER
     :  CRDOUT(2)*(*)
*  Arguments Returned:
      INTEGER
     :  CRDOI(2),
     :  NUMCOL,
     :  FIIN(MXCOL),
     :  FIOUT(MXCOL)
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      LOGICAL
     :  MORE      ! Flag: more parameters or columns to access?
      INTEGER
     :  FCOUNT,   ! Number of the current column.
     :  FIINC,    ! Identifier for the current input  column.
     :  FIOUTC    !     "       "   "     "    output   "   .
      CHARACTER
     :  CRDOUU(2)*(CAT__SZCMP),  ! Upper case copy of CRDOUT.
     :  FNAMEU*(CAT__SZCMP)      !   "    "    "   "  FNAME.

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
*       Create columns in the output catalogue for the appropriate
*       type of coordinates.

         IF (CRDTYP .EQ. CAP__CDEQM) THEN

*          ... mean equatorial coordinates.

            CALL CAT_CNEWS (CIOUT, CRDOUT(1), CAT__TYPED, 0,
     :        'RADIANS{HOURS}', 'F14.9', 'Right Ascension.', CRDOI(1),
     :        STATUS)

            CALL CAT_CNEWS (CIOUT, CRDOUT(2), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Declination.', CRDOI(2),
     :        STATUS)

         ELSE IF (CRDTYP .EQ. CAP__CDEQO) THEN

*          ... observed equatorial coordinates.

            CALL CAT_CNEWS (CIOUT, CRDOUT(1), CAT__TYPED, 0,
     :        'RADIANS{HOURS}', 'F14.9', 'Right Ascension (observed).',
     :        CRDOI(1), STATUS)

            CALL CAT_CNEWS (CIOUT, CRDOUT(2), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Declination (observed).',
     :        CRDOI(2), STATUS)

         ELSE IF (CRDTYP .EQ. CAP__CDEQL) THEN

*          ... local equatorial coordinates.

            CALL CAT_CNEWS (CIOUT, CRDOUT(1), CAT__TYPED, 0,
     :        'RADIANS{HOURS}', 'F14.9', 'Hour Angle (observed).',
     :        CRDOI(1), STATUS)

            CALL CAT_CNEWS (CIOUT, CRDOUT(2), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Declination (observed).',
     :        CRDOI(2), STATUS)

         ELSE IF (CRDTYP .EQ. CAP__CDHOR) THEN

*          ... local horizon coordinates.

            CALL CAT_CNEWS (CIOUT, CRDOUT(1), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Azimuth', CRDOI(1),
     :        STATUS)

            CALL CAT_CNEWS (CIOUT, CRDOUT(2), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9',
     :        'Zenith distance (observed).', CRDOI(2), STATUS)

         ELSE IF (CRDTYP .EQ. CAP__CDGAL) THEN

*          ... Galactic coordinates.

            CALL CAT_CNEWS (CIOUT, CRDOUT(1), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Galactic longitude.',
     :        CRDOI(1), STATUS)

            CALL CAT_CNEWS (CIOUT, CRDOUT(2), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Galactic latitude.',
     :        CRDOI(2), STATUS)

         ELSE IF (CRDTYP .EQ. CAP__CDSPG) THEN

*          ... Supergalactic coordinates.

            CALL CAT_CNEWS (CIOUT, CRDOUT(1), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Supergalactic longitude.',
     :        CRDOI(1), STATUS)

            CALL CAT_CNEWS (CIOUT, CRDOUT(2), CAT__TYPED, 0,
     :        'RADIANS{DEGREES}', 'F14.9', 'Supergalactic latitude.',
     :        CRDOI(2), STATUS)
         END IF

*
*       Copy each of the columns in the input catalogue, skipping any
*       with the same names as the columns to hold the new coordinates.

         CRDOUU(1) = CRDOUT(1)
         CALL CHR_UCASE(CRDOUU(1) )

         CRDOUU(2) = CRDOUT(2)
         CALL CHR_UCASE(CRDOUU(2) )

         MORE = .TRUE.
         FCOUNT = 0
         NUMCOL = 0

         DO WHILE (MORE)

*
*          Attempt to obtain an identifier for the next column in the
*          input catalogue, and proceed if ok.

            FCOUNT = FCOUNT + 1

            CALL CAT_TNDNT (CIIN, CAT__FITYP, FCOUNT, FIINC, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FIINC .NE. CAT__NOID) THEN

*
*             Inquire the values of all the attributes for this column.

               CALL CAT_CINQ (FIINC, 10, FCI, FNAME, FGENUS, FEXPR,
     :           FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :           FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :           FDATE, STATUS)

*
*             Create a corresponding column in the output catalogue if
*             the name of the column does not match the name of either
*             the columns to hold the new celestial coordinates.
*             Remember that in CURSA/CAT column names are not case
*             sensitive.

               FNAMEU= FNAME
               CALL CHR_UCASE (FNAMEU)

               IF (FNAMEU .NE. CRDOUU(1)  .AND.
     :             FNAMEU .NE. CRDOUU(2) ) THEN

*
*                Attempt to create a corresponding column in the output
*                catalogue.

                  CALL CAT_PNEW0 (CIOUT, CAT__FITYP, FNAME, FDTYPE,
     :              FIOUTC, STATUS)

*
*                Set the mutable attributes of this column to correspond
*                to the input column.

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
*                If all is ok then copy the identifiers for the input
*                and output columns to the return arrays.

                  IF (STATUS .EQ. SAI__OK) THEN
                     IF (NUMCOL .LT. MXCOL) THEN
                        NUMCOL = NUMCOL + 1

                        FIIN(NUMCOL) = FIINC
                        FIOUT(NUMCOL) = FIOUTC
                     END IF
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

      END IF

      END
