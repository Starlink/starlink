      SUBROUTINE CAP_LSTPR (CI, OFLAG, FLUNIT, BAR, STATUS)
*+
*  Name:
*     CAP_LSTPR
*  Purpose:
*     List full details of all the parameters in a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_LSTPR (CI, OFLAG, FLUNIT, BAR; STATUS)
*  Description:
*     List full details of all the parameters in a catalogue.
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
*     Do while there are more parameters
*       Attempt to obtain an identifier for the next parameter.
*       If ok and not finished then
*         Attempt to get the details for the parameter.
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
*     modification date, are not output.  Output of these attributes
*     should be added as they are implemented.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     19/9/94 (ACD): Original version.
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
     :  QI,       ! Parameter identifier.
     :  QCOUNT    ! Number of the current parameter.
      LOGICAL
     :  MORE      ! Flag: more parameters to access?

*
*    The following variables represent the attributes of the current
*    parameter.

      INTEGER
     :  QCI,        ! Parent catalogue.
     :  QDTYPE,     ! Data type.
     :  QCSIZE,     ! Size if a character string.
     :  QDIMS,      ! Dimensionality.
     :  QSIZEA(10)  ! Size of each array dimension.
      CHARACTER
     :  QNAME*(CAT__SZCMP),   ! Name.
     :  QUNITS*(CAT__SZUNI),  ! Units.
     :  QXTFMT*(CAT__SZEXF),  ! External format.
     :  QCOMM*(CAT__SZCOM),   ! Comments.
     :  QVALUE*(CAT__SZVAL)   ! Value.
      LOGICAL
     :  QPRFDS      ! Preferential display flag.
      DOUBLE PRECISION
     :  QDATE       ! Modification date.

*
*    The following variables represent the attributes of the current
*    column converted or re-formatted for display.

      CHARACTER
     :  QCTYPE*10,  ! Character representation of data type.
     :  DIMS*40,    ! Dimensionality.
     :  PRFDSP*4    ! Preferential display.
      INTEGER
     :  QCTYPS,     ! Size of QCTYPE (excl. trail. blanks).
     :  LDIMS       !  "   "  DIMS   ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Write a title.

         IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
            CALL CAP_OUT (BAR, ' ', 'Details of the parameters.',
     :        STATUS)
            CALL CAP_OUT (BAR, ' ', ' ', STATUS)
         END IF

         IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
            WRITE(FLUNIT, 2000, IOSTAT=LSTAT)
 2000       FORMAT(1X, 'Details of the parameters.' / )
            CALL FIO_SERR (LSTAT, STATUS)
         END IF

*
*       Get and display the details for all the columns.

         QCOUNT = 0
         MORE = .TRUE.

         DO WHILE (MORE)
            QCOUNT = QCOUNT + 1

*
*          Attempt to obtain an identifier for the next parameter and
*          proceed if the status is ok and the null identifier is not
*          returned.

            CALL CAT_TNDNT (CI, CAT__QITYP, QCOUNT, QI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  QI .NE. CAT__NOID) THEN

*
*             Attempt to get the details for the parameter and proceed
*             if ok.

               CALL CAT_PINQ (QI, 10, QCI, QNAME, QDTYPE, QCSIZE, QDIMS,
     :           QSIZEA, QUNITS, QXTFMT, QPRFDS, QCOMM, QVALUE, QDATE,
     :           STATUS)


               IF (STATUS .EQ. SAI__OK) THEN

*
*                Convert the details to a form suitable for output:
*
*                ... data type,

                  QCTYPE = ' '
                  QCTYPS = 0

                  CALL CAT_TYFMT (QDTYPE, QCSIZE, QCTYPE, QCTYPS,
     :              STATUS)

*
*                ... dimensionality,

                  DIMS = ' '
                  LDIMS = 0

                  IF (QDIMS .EQ. CAT__SCALR) THEN
                     CALL CHR_PUTC ('scalar.', DIMS, LDIMS)

                  ELSE IF (QDIMS .EQ. CAT__VECTR) THEN
                     CALL CHR_PUTC ('vector of ', DIMS, LDIMS)
                     CALL CHR_PUTI (QSIZEA(1), DIMS, LDIMS)
                     CALL CHR_PUTC (' element', DIMS, LDIMS)

                     IF (QSIZEA(1) .NE. 1) THEN
                        CALL CHR_PUTC ('s', DIMS, LDIMS)
                     END IF

                     CALL CHR_PUTC ('.', DIMS, LDIMS)

                  ELSE
                     CALL CHR_PUTC ('unknown (illegal code: ',
     :                 DIMS, LDIMS)
                     CALL CHR_PUTI (QDIMS, DIMS, LDIMS)
                     CALL CHR_PUTC (').', DIMS, LDIMS)

                  END IF

*
*                ... preferential display.

                  IF (QPRFDS) THEN
                     PRFDSP = 'yes.'
                  ELSE
                     PRFDSP = 'no.'
                  END IF

*
*                Display the details interactively if required.

                  IF (OFLAG .EQ. 1  .OR.  OFLAG .EQ. 3) THEN
                     CALL MSG_SETI ('QCOUNT', QCOUNT)
                     CALL CAP_OUT (BAR, ' ', 'Parameter number: '/
     :                 /'^QCOUNT', STATUS)

                     CALL MSG_SETC ('QNAME', QNAME)
                     CALL CAP_OUT (BAR, ' ', '  Name: ^QNAME', STATUS)

                     CALL MSG_SETC ('QCTYPE', QCTYPE)
                     CALL CAP_OUT (BAR, ' ', '  Data type: ^QCTYPE',
     :                 STATUS)

                     CALL MSG_SETC ('DIMS', DIMS)
                     CALL CAP_OUT (BAR, ' ', '  Dimensionality: ^DIMS',
     :                 STATUS)

                     CALL MSG_SETC ('QUNITS', QUNITS)
                     CALL CAP_OUT (BAR, ' ', '  Units: ^QUNITS', STATUS)

                     CALL MSG_SETC ('QXTFMT', QXTFMT)
                     CALL CAP_OUT (BAR, ' ', '  External format: '/
     :                 /'^QXTFMT', STATUS)

                     CALL MSG_SETC ('PRFDSP', PRFDSP)
                     CALL CAP_OUT (BAR, ' ', '  Preferential display: '/
     :                 /'^PRFDSP', STATUS)

                     CALL MSG_SETC ('QCOMM', QCOMM)
                     CALL CAP_OUT (BAR, ' ', '  Comments: ^QCOMM',
     :                 STATUS)

                     CALL MSG_SETC ('QVALUE', QVALUE)
                     CALL CAP_OUT (BAR, ' ', '  Value: ^QVALUE',
     :                 STATUS)

                     CALL CAP_OUT (BAR, ' ', ' ', STATUS)
                  END IF

*
*                Write the details to the output file, if required.

                  IF (OFLAG .EQ. 2  .OR.  OFLAG .EQ. 3) THEN
                     WRITE(FLUNIT, 2001, IOSTAT=LSTAT) QCOUNT,
     :                 QNAME, QCTYPE, DIMS, QUNITS, QXTFMT, PRFDSP,
     :                 QCOMM, QVALUE
 2001                FORMAT(
     :                 1X, 'Parameter number: ', I5, ':' /
     :                 3X, 'Name: ', A /
     :                 3X, 'Data type:', A /
     :                 3X, 'Dimensionality: ', A /
     :                 3X, 'Units: ', A /
     :                 3X, 'External format: ', A /
     :                 3X, 'Preferential display: ', A /
     :                 3X, 'Comments: ', A /
     :                 3X, 'Value: ', A / )
                     CALL FIO_SERR (LSTAT, STATUS)
                  END IF

               END IF
            END IF

*
*          If the status is not ok or all the parameters have been
*          obtained then set the termination flag.

            IF (STATUS .NE. CAT__OK  .OR.  QI .EQ. CAT__NOID) THEN
               MORE = .FALSE.
            END IF

         END DO

      END IF

      END
