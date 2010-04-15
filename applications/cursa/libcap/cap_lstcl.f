      SUBROUTINE CAP_LSTCL (CI, OFLAG, FLUNIT, BAR, STATUS)
*+
*  Name:
*     CAP_LSTCL
*  Purpose:
*     List full details of all the columns in a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_LSTCL (CI, OFLAG, FLUNIT, BAR; STATUS)
*  Description:
*     List full details of all the columns in a catalogue.
*
*     The text is listed to the standard output stream (via MSG_OUT)
*     and/or a text file.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     OFLAG  =  INTEGER (Given)
*        Flag indicating which output is to be produced.  It is coded
*        as follows:
*         1 - standard output (usually the command screen) only,
*         2 - text file only,
*         3 - both screen and file.
*     FLUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the text file.
*     BAR  =  LOGICAL (Given)
*        A flag indicating whether or a vertical bar ('|') will be
*        inserted at the start of lines of text sent to standard output.
*        It is coded as follows:
*        .TRUE.  -  insert a bar,
*        .FALSE. -  do not insert a bar.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Do while there are more colums
*       Attempt to obtain an identifier for the next column.
*       If ok and not finished then
*         Attempt to get the details for the column.
*         If ok then
*           Convert the details to a form suitable for output.
*           If required then
*             Display the details to the standard output.
*           end if
*           If required then
*             Write the details to the output file.
*           end if
*         end if
*       end if
*       If finished or the status is not ok then
*         Set the termination flag.
*       end if
*     end do
*  Implementation Deficiencies:
*     Parameter attributes which are not fully implemented, such as the
*     genus, expression or modification date, are not output.  Output of
*     these attributes should be added as they are implemented.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     19/9/94 (ACD): Original version.
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
      INCLUDE 'CAT_PAR'     ! Standard CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  CI,
     :  OFLAG,
     :  FLUNIT
      LOGICAL
     :  BAR
*  Status:
      INTEGER STATUS        ! Global status.
*  Local Variables:
      INTEGER
     :  LSTAT,    ! Local Fortran I/O status.
     :  FI,       ! Column (or field) identifier.
     :  FCOUNT    !   "    "   "     "    columns.
      LOGICAL
     :  MORE      ! Flag: more columns to access?

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
     :  FSCALE,     ! Scale factor.
     :  FZEROP,     ! Zero point.
     :  FDATE       ! Modification date.
      LOGICAL
     :  FPRFDS      ! Preferential display flag.

*
*    The following variables represent the attributes of the current
*    column converted or re-formatted for display.

      CHARACTER
     :  FCTYPE*10,  ! Data type.
     :  DIMS*40,    ! Dimensionality.
     :  NULTRT*40,  ! Treatment of null values.
     :  ORDER*40,   ! Order.
     :  PRFDSP*4    ! Preferential display.
      INTEGER
     :  FCTYPS,     ! Size of FCTYPE (excl. trail. blanks).
     :  LDIMS,      !  "   "  DIMS   ( "  .   "  .   "   ).
     :  LNULTR,     !  "   "  NULTRT ( "  .   "  .   "   ).
     :  LORDER      !  "   "  ORDER  ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Write a title.

         IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
            CALL CAP_OUT (BAR, ' ', 'Details of the Columns.', STATUS)
            CALL CAP_OUT (BAR, ' ', ' ', STATUS)
         END IF

         IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
            WRITE(FLUNIT, 2000, IOSTAT=LSTAT)
 2000       FORMAT(1X, 'Details of the Columns.' / )
            CALL FIO_SERR (LSTAT, STATUS)
         END IF

*
*       Get and display the details for all the columns.

         FCOUNT = 0
         MORE = .TRUE.

         DO WHILE (MORE)
            FCOUNT = FCOUNT + 1

*
*          Attempt to obtain an identifier for the next column and
*          proceed if the status is ok and the null identifier is not
*          returned.

            CALL CAT_TNDNT (CI, CAT__FITYP, FCOUNT, FI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  FI .NE. CAT__NOID) THEN

*
*             Attempt to get the details for the column and proceed if
*             ok.

               CALL CAT_CINQ (FI, 10, FCI, FNAME, FGENUS, FEXPR,
     :           FDTYPE, FCSIZE,FDIMS, FSIZEA, FNULL, FXCEPT, FSCALE,
     :           FZEROP, FORDER, FUNITS, FXTFMT, FPRFDS, FCOMM,
     :           FDATE, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Convert the details to a form suitable for output:
*
*                ... data type,

                  FCTYPE = ' '
                  FCTYPS = 0

                  CALL CAT_TYFMT (FDTYPE, FCSIZE, FCTYPE, FCTYPS,
     :              STATUS)

*
*                ... dimensionality,

                  DIMS = ' '
                  LDIMS = 0

                  IF (FDIMS .EQ. CAT__SCALR) THEN
                     CALL CHR_PUTC ('scalar.', DIMS, LDIMS)

                  ELSE IF (FDIMS .EQ. CAT__VECTR) THEN
                     CALL CHR_PUTC ('vector of ', DIMS, LDIMS)
                     CALL CHR_PUTI (FSIZEA(1), DIMS, LDIMS)
                     CALL CHR_PUTC (' element', DIMS, LDIMS)

                     IF (FSIZEA(1) .NE. 1) THEN
                        CALL CHR_PUTC ('s', DIMS, LDIMS)
                     END IF

                     CALL CHR_PUTC ('.', DIMS, LDIMS)

                  ELSE
                     CALL CHR_PUTC ('unknown (illegal code: ',
     :                 DIMS, LDIMS)
                     CALL CHR_PUTI (FDIMS, DIMS, LDIMS)
                     CALL CHR_PUTC (').', DIMS, LDIMS)

                  END IF

*
*                ... null values,

                  NULTRT = ' '
                  LNULTR = 0

                  IF (FNULL .EQ. CAT__NULLD) THEN
                     CALL CHR_PUTC ('standard Starlink or HDS.',
     :                 NULTRT, LNULTR)

                  ELSE IF (FNULL .EQ. CAT__NULLS) THEN
                     CALL CHR_PUTC ('specific to the catalogue.',
     :                 NULTRT, LNULTR)

                  ELSE IF (FNULL .EQ. CAT__LOCUM) THEN
                     CALL CHR_PUTC ('locum values only.',
     :                 NULTRT, LNULTR)

                  ELSE
                     CALL CHR_PUTC ('unknown (illegal code: ',
     :                 NULTRT, LNULTR)
                     CALL CHR_PUTI (FNULL, NULTRT, LNULTR)
                     CALL CHR_PUTC (').', NULTRT, LNULTR)

                  END IF

*
*                ... order,

                  ORDER = ' '
                  LORDER = 0

                  IF (FORDER .EQ. CAT__ASCND) THEN
                     CALL CHR_PUTC ('ascending.', ORDER, LORDER)

                  ELSE IF (FORDER .EQ. CAT__DSCND) THEN
                     CALL CHR_PUTC ('descending.', ORDER, LORDER)

                  ELSE IF (FORDER .EQ. CAT__NOORD) THEN
                     CALL CHR_PUTC ('unsorted.', ORDER, LORDER)

                  ELSE
                     CALL CHR_PUTC ('unknown (illegal code: ',
     :                 ORDER, LORDER)
                     CALL CHR_PUTI (FORDER, ORDER, LORDER)
                     CALL CHR_PUTC (').', ORDER, LORDER)

                  END IF

*
*                ... preferential display.

                  IF (FPRFDS) THEN
                     PRFDSP = 'yes.'
                  ELSE
                     PRFDSP = 'no.'
                  END IF

*
*                Display the details interactively if required.

                  IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
                     CALL MSG_SETI ('FCOUNT', FCOUNT)
                     CALL CAP_OUT (BAR, ' ', 'Column number: ^FCOUNT',
     :                 STATUS)

                     CALL MSG_SETC ('FNAME', FNAME)
                     CALL CAP_OUT (BAR, ' ', '  Name: ^FNAME', STATUS)

                     CALL MSG_SETC ('FCTYPE', FCTYPE)
                     CALL CAP_OUT (BAR, ' ', '  Data type: ^FCTYPE',
     :                 STATUS)

                     CALL MSG_SETC ('DIMS', DIMS)
                     CALL CAP_OUT (BAR, ' ', '  Dimensionality: ^DIMS',
     :                 STATUS)

                     CALL MSG_SETC ('NULTRT', NULTRT)
                     CALL CAP_OUT (BAR, ' ', '  Type of null '/
     :                 /'values: ^NULTRT', STATUS)

                     CALL MSG_SETD ('FSCALE', FSCALE)
                     CALL CAP_OUT (BAR, ' ', '  Scale factor: ^FSCALE',
     :                 STATUS)

                     CALL MSG_SETD ('FZEROP', FZEROP)
                     CALL CAP_OUT (BAR, ' ', '  Zero point: ^FZEROP',
     :                 STATUS)

                     CALL MSG_SETC ('ORDER', ORDER)
                     CALL CAP_OUT (BAR, ' ', '  Order: ^ORDER', STATUS)

                     CALL MSG_SETC ('FUNITS', FUNITS)
                     CALL CAP_OUT (BAR, ' ', '  Units: ^FUNITS', STATUS)

                     CALL MSG_SETC ('FXTFMT', FXTFMT)
                     CALL CAP_OUT (BAR, ' ', '  External format: '/
     :                 /'^FXTFMT', STATUS)

                     CALL MSG_SETC ('PRFDSP', PRFDSP)
                     CALL CAP_OUT (BAR, ' ', '  Preferential display: '/
     :                 /'^PRFDSP', STATUS)

                     CALL MSG_SETC ('FCOMM', FCOMM)
                     CALL CAP_OUT (BAR, ' ', '  Comments: ^FCOMM',
     :                 STATUS)

                     CALL CAP_OUT (BAR, ' ', ' ', STATUS)
                  END IF

*
*                Write the details to the output file, if required.

                  IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
                     WRITE(FLUNIT, 2001, IOSTAT=LSTAT) FCOUNT,
     :                 FNAME, FCTYPE, DIMS, NULTRT, FSCALE, FZEROP,
     :                 ORDER, FUNITS, FXTFMT, PRFDSP, FCOMM
 2001                FORMAT(
     :                 1X, 'Column number: ', I5, ':' /
     :                 3X, 'Name: ', A /
     :                 3X, 'Data type: ', A /
     :                 3X, 'Dimensionality: ', A /
     :                 3X, 'Type of null values: ', A /
     :                 3X, 'Scale Factor: ', 1PD15.6/
     :                 3X, 'Zero point: ', 1PD15.6 /
     :                 3X, 'Order: ', A /
     :                 3X, 'Units: ', A /
     :                 3X, 'External format: ', A /
     :                 3X, 'Preferential display: ', A /
     :                 3X, 'Comments: ', A / )
                     CALL FIO_SERR (LSTAT, STATUS)
                  END IF

               END IF
            END IF

*
*          If the status is not ok or all the columns have been
*          obtained then set the termination flag.

            IF (STATUS .NE. CAT__OK  .OR.  FI .EQ. CAT__NOID) THEN
               MORE = .FALSE.
            END IF

         END DO

      END IF

      END
