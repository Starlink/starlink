      SUBROUTINE CAT6_WRPAR (CI, TSUNIT, STATUS)
*+
*  Name:
*     CAT6_WRPAR
*  Purpose:
*     Write the parameters for a tab-separated table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT6_WRPAR (CI, TSUNIT; STATUS)
*  Description:
*     Write the parameters for a tab-separated table.
*
*     Note that the TST special parameters ID_COL, RA_COL and DEC_COL
*     are not copied.  They are generated anew elsewhere because any
*     existing values might not be valid.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     TSUNIT  =  INTEGER (Given)
*        Fortran unit number for writing to the tab-separated table.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Write the 'start of parameters' comment line.
*     While there are more parameters
*       Attempt to get an identifier for the next parameter.
*       If ok and the identifier is not the null identifier then
*         Get the parameter name.
*         Convert the name to upper case.
*         If the name does not correspond to a TST special parameter then
*           Get the parameter value.
*           Assemble the output line.
*           Write the output line.
*         end if
*       else
*         Set the termination flag.
*         If the status is not ok then
*           Report error.
*         end if
*       end if
*     end do
*     Write the 'end of parameters' comment line.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     16/9/99 (ACD): Original version.
*     14/7/00 (ACD): Changed to omit copying the TST special parameters.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
*  Global Variables:
      INCLUDE 'CAT6_TST_CMN'      ! TST back-end common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  TSUNIT
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  QI,     ! Identifier for current parameter.
     :  LQNAME, ! Length of QNAME  (excl. trail. blanks).
     :  LQVAL,  !   "    "  QVALUE ( "  .   "  .   "   ).
     :  BUFLEN, !   "    "  BUFFER ( "  .   "  .   "   ).
     :  LSTAT,  ! Fortan I/O status from writing to the file.
     :  PARS    ! Sequence number of current parameter.
      CHARACTER
     :  QNAME*(CAT__SZCMP),  ! Parameter name  attribute.
     :  QVALUE*(CAT__SZVAL), !     "     Value     "    .
     :  UQNAME*(CAT__SZCMP), ! QNAME converted to upper case.
     :  BUFFER*(CAT6__SZDRC) ! Output buffer.
      LOGICAL
     :  MORE    ! Flag; more parameters to process?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Write the 'start of parameters' comment line.

         WRITE(TSUNIT, 2000, IOSTAT=LSTAT)
 2000    FORMAT('# Start of parameter definitions.')
         CALL CAT1_IOERR (LSTAT, STATUS)

*
*       Write the parameters.

         PARS = 0
         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to get an identifier for the next parameter.  Then
*          proceed if all is ok and the identifier is not the null
*          identifier.

            PARS = PARS + 1
            CALL CAT_TNDNT (CI, CAT__QITYP, PARS, QI, STATUS)

            IF (STATUS .EQ. CAT__OK  .AND.  QI .NE. CAT__NOID) THEN

*
*             Get the parameter name.

               CALL CAT_TIQAC (QI, 'NAME', QNAME, STATUS)

*
*             Convert the name to upper case and check that it is not
*             one of the TST special parameters.

               UQNAME = QNAME
               CALL CHR_UCASE (UQNAME)

               IF (UQNAME .NE. 'ID_COL'  .AND.
     :             UQNAME .NE. 'RA_COL'  .AND.
     :             UQNAME .NE. 'DEC_COL') THEN


*                Get the parameter value.  Note that the name and value
*                are the only parameter details written to the file.

                  CALL CAT_TIQAC (QI, 'VALUE', QVALUE, STATUS)

*
*                Assemble the output line.

                  BUFLEN = 0
                  BUFFER = ' '

                  IF (QNAME .NE. ' ') THEN
                     LQNAME = CHR_LEN(QNAME)
                     CALL CHR_PUTC (QNAME(1 : LQNAME), BUFFER, BUFLEN)
                  ELSE
                     CALL CHR_PUTC ('<no-name>', BUFFER, BUFLEN)
                  END IF

                  CALL CHR_PUTC (': ', BUFFER, BUFLEN)

                  IF (QVALUE .NE. ' ') THEN
                     LQVAL = CHR_LEN(QVALUE)
                     CALL CHR_PUTC (QVALUE(1 : LQVAL), BUFFER, BUFLEN)
                  END IF

*
*                Write the output line.

                  WRITE(TSUNIT, 2001, IOSTAT=LSTAT) BUFFER(1 : BUFLEN)
 2001             FORMAT(A)
                  CALL CAT1_IOERR (LSTAT, STATUS)

               END IF

            ELSE

*
*             A parameter could not be obtained.  Set the termination
*             flag and report any error.

               MORE = .FALSE.

               IF (STATUS .NE. CAT__OK) THEN
                  CALL CAT1_ERREP ('CAT6_WRPAR_ERR', 'Error writing '/
     :              /'parameter.', STATUS)
               END IF
            END IF
         END DO

*
*       Write the 'end of parameters' comment line.

         WRITE(TSUNIT, 2002, IOSTAT=LSTAT)
 2002    FORMAT('# End of parameter definitions.')
         CALL CAT1_IOERR (LSTAT, STATUS)

      END IF

      END
