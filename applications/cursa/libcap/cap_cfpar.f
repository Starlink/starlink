      SUBROUTINE CAP_CFPAR (PFILT, CIIN, CIOUT, STATUS)
*+
*  Name:
*     CAP_CFPAR
*  Purpose:
*     Create selected output cat. parameters corresponding to input cat. ones.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CFPAR (CIIN, CIOUT; STATUS)
*  Description:
*     Create selected parameters in the output catalogue corresponding to
*     those in the input catalogue.
*  Arguments:
*     PFILT  =  CHARACTER*(*) (Given)
*        Comma-separated list of parameters to be filtered out (ie. not
*        copied).
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Decode the list of parameters to be filtered out.
*     Do while there are more parameters in the input catalogue.
*       Attempt to obtain an identifier for the next parameter in the
*       input catalogue.
*       If ok then
*         Inquire the details of the parameter.
*         Check whether the current parameter is in the list of those
*         which are not to be copied.
*         If not (ie. it is to be copied) then
*           Determine whether a parameter of the given name already
*           exists in the output catalogue.
*           If not then
*             Attempt to create a new parameter in the output catalogue.
*             Set the details of the new parameter.
*           else
*             Report a warning.
*           end if
*         else
*           Report a message; parameter omitted.
*         end if
*       else
*         Set the termination flag.
*       end if
*       If any error has occurred then
*         Set the termination flag.
*       end if
*     end do
*     Report any parameters which were to be filtered out but which
*     were not present in the input catalogue.
*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     23/3/01 (ACD): Original version (from CAP_CPPAR).
*     24/4/01 (ACD): First stable version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
*  Arguments Given:
      CHARACTER
     :  PFILT*(*)
      INTEGER
     :  CIIN,
     :  CIOUT
*  Status:
      INTEGER STATUS     ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MXFILT     ! Maximum number of parameters to be filtered out.
      PARAMETER (MXFILT = 100)
*  Local Variables:
      LOGICAL
     :  MORE,      ! Flag: more parameters or parameters to access?
     :  CPYPAR,    ! Flag; copy the current parameter?
     :  FNDPAR(MXFILT) ! Flag; parameter found in input catalogue?
      INTEGER
     :  QCOUNT,    ! Number of the current parameter.
     :  QIINC,     ! Identifier for the current input  parameter.
     :  QIOUTC,    !     "       "   "     "    output   "   .
     :  LOOP,      ! Loop index.
     :  LPFILT,    ! Length of PFILT (excl. trail. blanks).
     :  NFILT,     ! No. of parameters to be filtered out.
     :  START(MXFILT), ! Start position of parameters to be filtered out.
     :  STOP(MXFILT),  ! Stop     "     "      "      "  "     "      " .
     :  LSTAT,     ! Local status.
     :  BUFLEN     ! Length of BUFFER (excl. trail. blanks).
      CHARACTER
     :  UPFILT*75, ! Upper case copy of PFILT.
     :  PFILTS(MXFILT)*(CAT__SZCMP), ! List of parameters to be filtered out.
     :  UQNAME*(CAT__SZCMP),         ! Upper case copy of QNAME.
     :  BUFFER*75  ! Output buffer.

*
*    The following variables represent the attributes of the current
*    parameter.

      INTEGER
     :  QCI,         ! Parent catalogue.
     :  QDTYPE,      ! Data type.
     :  QCSIZE,      ! Size if a character string.
     :  QDIMS,       ! Dimensionality.
     :  QSIZEA(10),  ! Size of each array dimension.
     :  OSTAT,       ! Status checking whether parameter in output cat.
     :  OQI,         ! Parameter identifier in the output catalogue.
     :  LQNAME       ! Length of QNAME (excl. trail. blanks).
      CHARACTER
     :  QNAME*(CAT__SZCMP),    ! Name.
     :  QUNITS*(CAT__SZUNI),   ! Units.
     :  QXTFMT*(CAT__SZEXF),   ! External format.
     :  QCOMM*(CAT__SZCOM),    ! Comments.
     :  QVALUE*(CAT__SZVAL)    ! Value.
      LOGICAL
     :  QPRFDS       ! Preferential display flag.
      DOUBLE PRECISION
     :  QDATE        ! Modification date.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Decode the list of parameters to be filtered out.

         IF (PFILT .NE. ' ') THEN

            UPFILT = PFILT
            CALL CHR_UCASE (UPFILT)

            LPFILT = CHR_LEN(UPFILT)

            DO LOOP = 1, LPFILT
               IF (UPFILT(LOOP : LOOP) .EQ. ',') THEN
                  UPFILT(LOOP : LOOP) = ' '
               END IF
            END DO

            CALL CHR_DCWRD (UPFILT, MXFILT, NFILT, START, STOP, PFILTS,
     :        LSTAT)

         ELSE
            NFILT = 0

         END IF

         DO LOOP = 1, NFILT
            FNDPAR(LOOP) = .FALSE.
         END DO

*
*       Copy each of the parameters in the input catalogue.

         MORE = .TRUE.
         QCOUNT = 0

         DO WHILE (MORE)

*
*          Attempt to obtain an identifier for the next parameter in the
*          input catalogue, and proceed if ok.

            QCOUNT = QCOUNT + 1

            CALL CAT_TNDNT (CIIN, CAT__QITYP, QCOUNT, QIINC, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  QIINC .NE. CAT__NOID) THEN

*
*             Inquire the values of all the attributes for this
*             parameter.

               CALL CAT_PINQ (QIINC, 10, QCI, QNAME, QDTYPE, QCSIZE,
     :           QDIMS, QSIZEA, QUNITS, QXTFMT, QPRFDS, QCOMM, QVALUE,
     :           QDATE, STATUS)

*
*             Check if the parameter is in the list of those which are
*             to be filtered out (ie. not copied).  If it is not in this
*             list then copy it.

               UQNAME = QNAME
               CALL CHR_UCASE (UQNAME)

               CPYPAR = .TRUE.

               DO LOOP = 1, NFILT
                  IF (UQNAME .EQ. PFILTS(LOOP)) THEN
                     CPYPAR = .FALSE.
                     FNDPAR(LOOP) = .TRUE.
                  END IF
               END DO

               IF (CPYPAR) THEN

*
*                Determine whether the output catalogue already contains
*                a parameter (or column) of the given name.  Proceed if
*                it does not; otherwise issue a warning.  (Note: CAT_TIDNT
*                returns a bad status if the output catalogue does not
*                contain the parameter).

                  OSTAT = SAI__OK
                  CALL CAT_TIDNT (CIOUT, QNAME, OQI, OSTAT)

                  IF (OSTAT .NE. SAI__OK) THEN

*
*                   Annul the error generated by CAT_TIDNT.

                     CALL ERR_ANNUL (OSTAT)

*
*                   Attempt to create a corresponding parameter in the
*                   output catalogue.

                     CALL CAT_PNEW0 (CIOUT, CAT__QITYP, QNAME, QDTYPE,
     :                 QIOUTC, STATUS)

*
*                   Set the attributes of this parameter to correspond to
*                   the input parameter.  Note that only those attributes
*                   which can vary in a SCAR/ADC catalogue are set.

                     CALL CAT_TATTI (QIOUTC, 'CSIZE', QCSIZE, STATUS)
                     CALL CAT_TATTC (QIOUTC, 'UNITS', QUNITS, STATUS)
                     CALL CAT_TATTC (QIOUTC, 'EXFMT', QXTFMT, STATUS)
                     CALL CAT_TATTC (QIOUTC, 'COMM', QCOMM, STATUS)
                     CALL CAT_TATTC (QIOUTC, 'VALUE', QVALUE, STATUS)

                  ELSE
                     BUFFER = ' '
                     BUFLEN = 0

                     CALL CHR_PUTC ('Parameter ', BUFFER, BUFLEN)

                     IF (QNAME .NE. ' ') THEN
                        LQNAME = CHR_LEN(QNAME)
                        CALL CHR_PUTC (QNAME(1 : LQNAME), BUFFER,
     :                    BUFLEN)
                     ELSE
                        CALL CHR_PUTC ('<blank>', BUFFER, BUFLEN)
                     END IF

                     CALL CHR_PUTC (' has been modified.', BUFFER,
     :                 BUFLEN)

                     CALL CAP_WARN (.TRUE., ' ', BUFFER(1 : BUFLEN),
     :                 STATUS)

                  END IF

               ELSE
                  CALL MSG_SETC ('UQNAME', UQNAME)
                  CALL CAP_INFO (.TRUE., ' ', 'Omitted parameter '/
     :              /'^UQNAME.', STATUS)

               END IF

            ELSE

*
*             Either an error has occurred or the last parameter has
*             been accessed from the input catalogue; set the termination
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
*       Report any parameters which were to be filtered out but which
*       were not present in the input catalogue.

         DO LOOP = 1, NFILT
            IF (.NOT. FNDPAR(LOOP)) THEN
               CALL MSG_SETC ('PFILTS', PFILTS(LOOP))
               CALL CAP_WARN (.TRUE., ' ', 'Parameter ^PFILTS is '/
     :           /'not present in input catalogue.', STATUS)
            END IF
         END DO

      END IF

      END
