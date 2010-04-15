      SUBROUTINE CAP_CPPRS (CIIN, CIOUT, STATUS)
*+
*  Name:
*     CAP_CPPRS
*  Purpose:
*     Copy parameters from a secondary to an output catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPPRS (CIIN, CIOUT; STATUS)
*  Description:
*     Copy parameters from a secondary catalogue to an output catalogue.
*     This routine differs from CAP_CPPAR in that any existing
*     parameters in the output catalogue (which, typically, will have
*     been copied from the primary) are checked for name clashes before
*     writing the new parameter.  If there is a name clash then '_S'
*     is appended to the name of the new parameter in order to
*     disambiguate it.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     For every parameter already in the output catalogue then
*       Obtain its name.
*       Force the name into upper case.
*     end for
*     Do while there are more parameters in the input catalogue.
*       Attempt to obtain an identifier for the next colunm in the
*       input catalogue.
*       If ok then
*         Inquire the details of the parameter.
*         Force the name of the parameter into upper case.
*         If the name of the parameter already exists in the output
*         catalogue then
*           Append '_S' to the name.
*         end if
*         Attempt to create a new parameter in the output catalogue.
*         Set the details of the new parameter.
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
*     27/5/97 (ACD): Original version (based on CAP_CPPAR).
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
     :  CIOUT
*  Status:
      INTEGER STATUS        ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  MORE,     ! Flag: more parameters or parameters to access?
     :  FOUND     ! Flag; has the name been found?
      INTEGER
     :  QCOUNT,   ! Number of the current parameter.
     :  QIINC,    ! Identifier for the current input  parameter.
     :  QIOUTC,   !     "       "   "     "    output   "   .
     :  QPI,      ! Identifier for current existing parameter.
     :  QPCNT,    ! Number of existing parameters.
     :  LQNAME,   ! Length of QNAME (excl. trail. blanks).
     :  LOOP      ! Loop index.
      CHARACTER
     :  QPNAME(CAT__MXPAR)*(CAT__SZCMP), ! Names of existing parameters.
     :  QNAMEU*(CAT__SZCMP)              ! QNAME in upper case.

*
*    The following variables represent the attributes of the current
*    parameter.

      INTEGER
     :  QCI,         ! Parent catalogue.
     :  QDTYPE,      ! Data type.
     :  QCSIZE,      ! Size if a character string.
     :  QDIMS,       ! Dimensionality.
     :  QSIZEA(10)   ! Size of each array dimension.
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
*       For every parameter in the output catalogue obtain its name and
*       force it into upper case.

         MORE = .TRUE.
         QCOUNT = 0

         DO WHILE (MORE)

*
*          Attempt to obtain an identifier for the next parameter in the
*          input catalogue, and proceed if ok.

            QCOUNT = QCOUNT + 1

            CALL CAT_TNDNT (CIOUT, CAT__QITYP, QCOUNT, QPI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  QPI .NE. CAT__NOID) THEN

*
*             Get the name of the parameter and force it into upper
*             case.

               CALL CAT_TIQAC (QPI, 'NAME', QNAME, STATUS)

C              print2001, qcount, qpi, qname
C2001          format(1x, 'qcount, qpi, qname: ', i5, i5, 2x, a )

               CALL CHR_UCASE (QNAME)

               QPNAME(QCOUNT) = QNAME
            ELSE

*
*             Either an error has occurred or the last parameter has
*             been accessed from the input catalogue; set the
*             termination status.

               MORE = .FALSE.
            END IF

*
*          Set the termination flag if any error has occurred.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.
            END IF

         END DO

         QPCNT = QCOUNT - 1

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
*             If there are any pre-existing parameters to be checked
*             then force the name of the current parameter into upper
*             case, check if it already exists and if so append '_S'
*             to it to disambiguate it.

               IF (QPCNT .GT. 0) THEN
                  QNAMEU = QNAME
                  CALL CHR_UCASE(QNAMEU)

                  FOUND = .FALSE.
                  LOOP = 0

                  DO WHILE (.NOT. FOUND  .AND.  LOOP .LT. QPCNT)
                     LOOP = LOOP + 1

                     IF (QNAMEU .EQ. QPNAME(LOOP) ) THEN
                        FOUND = .TRUE.
                     END IF
                  END DO

                  IF (FOUND) THEN
                     LQNAME = CHR_LEN(QNAME)
                     CALL CHR_PUTC ('_S', QNAME, LQNAME)
                  END IF
               END IF

*
*             Attempt to create a corresponding parameter in the output
*             catalogue.

               CALL CAT_PNEW0 (CIOUT, CAT__QITYP, QNAME, QDTYPE,
     :           QIOUTC, STATUS)

*
*             Set the attributes of this parameter to correspond to the
*             input parameter.  Note that only those attributes which
*             can vary in a SCAR/ADC catalogue are set.

               CALL CAT_TATTI (QIOUTC, 'CSIZE', QCSIZE, STATUS)
               CALL CAT_TATTC (QIOUTC, 'UNITS', QUNITS, STATUS)
               CALL CAT_TATTC (QIOUTC, 'EXFMT', QXTFMT, STATUS)
               CALL CAT_TATTC (QIOUTC, 'COMM', QCOMM, STATUS)
               CALL CAT_TATTC (QIOUTC, 'VALUE', QVALUE, STATUS)

            ELSE

*
*             Either an error has occurred or the last parameter has
*             been accessed from the input catalogue; set the
*             termination status.

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
