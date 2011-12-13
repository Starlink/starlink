      SUBROUTINE CAT1_TOPEN (CNAME, STATE, MODE, CI, STATUS)
*+
*  Name:
*     CAT1_TOPEN
*  Purpose:
*       Open a catalogue and obtain an identifier to it.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_TOPEN (CNAME, STATE, MODE; CI; STATUS)
*  Description:
*     Open a catalogue and obtain an identifier to it.
*
*     The matrix of possibilities for STATE and MODE is:
*
*     STATE = 'NEW', then
*       MODE = 'WRITE' is ok
*       and MODE = 'READ' if forbidden.
*
*     STATE = 'OLD' then
*       MODE = 'WRITE' is ok
*       and MODE = 'READ' is ok.
*
*     Notes:
*
*     * If a catalogue already exists and an attempt is made to open
*       it with STATE = 'NEW', CAT1_TOPEN will fail with an error.
*
*     * If an existing catalogue is opened with STATE = 'OLD' and
*       MODE = 'WRITE' it will be overwritten.
*
*     * This is an internal routine and exits without reporting any
*       error.  However, it does report illegal values of STATE and
*       MODE.
*
*  Arguments:
*     CNAME  =  CHARACTER*(*) (Given)
*        Catalogue name.
*     STATE  =  CHARACTER*(*) (Given)
*        Required state of the catalogue.  One of:
*        NEW  -  A new catalogue is to be created,
*        OLD  -  An existing (that is, old) catalogue is to be opened.
*     MODE  =  CHARACTER*(*) (Given)
*        Mode in which the catalogue will be accessed.  One of:
*        READ   -  the catalogue may only be read from,
*        WRITE  -  an new catalogue is to be written.
*     CI  =  INTEGER (Returned)
*        Catalogue identifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If there are no open catalogues then
*       Initialise the expression parser.
*     end if
*     Initialise the catalogue identifier to the null identifier.
*     Parse the combined catalogue name to yield the catalogue name
*     proper, directory path and back-end type.
*     If ok then
*       Convert the character representations of the STATE and MODE to
*       the equivalent internal integer codes.
*       If ok then
*         for each combination of STATE and MODE
*           If the combination is permitted then
*             Attempt to open the catalogue in the specified fashion.
*           else
*             Set the error status.
*           end if
*         end for
*         If ok then
*           Save the internal codes for the state and mode.
*           Initialise the AST directives.
*         end if
*       end if
*     end if
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     3/5/93   (ACD): Prologue only.
*     24/6/93  (ACD): First implementation.
*     11/10/93 (ACD): First stable version.
*     24/1/94  (ACD): Modified error reporting.
*     27/1/94  (ACD): Variable EXTRA initialised inside CAT1_CNMPR
*        rather than here.
*     11/2/94  (ACD): Added EXTERNAL references to block data statements
*        for back-end independent common blocks in order to force
*        loading of the block data statements.
*     23/11/94 (ACD): Changed the 'Description' section of the prologue
*        so that it was not corrupted by PROLAT.
*     11/4/95  (ACD): Changed the name of the null identifier.
*     23/7/96  (ACD): Added explicit propogation of the catalogue file
*        type.
*     10/12/96 (ACD): Variable EXTRA passed when writing a new catalogue
*        as well as reading an existing one.
*     29/5/98  (ACD): Modified the handling of file names and error
*        reporting.
*     4/6/98   (ACD): Changed the error reporting not to change the
*        status.
*     17/11/98 (ACD): Converted from the external routine CAT_TOPEN to
*        the internal routine CAT1_TOPEN.  The main change was to
*        remove the error report on exit.  However, reports for bad
*        values of STATE and MODE were added and the initialisation
*        of CI to the null identifier was rationalised.
*     17/8/99  (ACD): Changed the initialisation of the parser so that
*        it occurs whenever there are no open catalogues.
*     14/10/99 (ACD): Added initialisation of the AST directives.
*     2/11/99  (ACD): Revised initialisation of the AST directives.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Global Variables:
      INCLUDE 'CAT1_CTRL_CMN'     ! Control common block.
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT1_IDS_CMN'      ! Identifiers common block.
      INCLUDE 'CAT1_AST_CMN'      ! CAT - AST common block.
*  Arguments Given:
      CHARACTER
     :  CNAME*(*),
     :  STATE*(*),
     :  MODE*(*)
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      EXTERNAL ATTRBB    ! External references to back-end independent
      EXTERNAL CATSB     ! block data statements in order to force
      EXTERNAL CTRLB     ! loading of the block data statements and
      EXTERNAL IDSB      ! hence the initialisations they contain.
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  CATNAM*(CAT__SZCNM),  ! Catalogue name.
     :  CATFIL*(CAT1__SZDIR), ! Full catalogue file name.
     :  CSTATE*3,             ! Local copy of STATE.
     :  CMODE*6,              !   "    "   "  MODE.
     :  EXTRA*10,             ! Extra info. to access catalogue.
     :  ERRBUF*75             ! Text of error message.
      INTEGER
     :  CURCAT,     ! Sequence number for the current catalogue.
     :  BCKTYP,     ! Back-end type.
     :  ISTATE,     ! Integer code corresponding to the STATE.
     :  IMODE,      !    "     "        "        "   "  MODE.
     :  CIELM,      ! Array element for the catalogue.
     :  ERRLEN,     ! Length of ERRBUF (excl. trail. blanks).
     :  LSTATE,     ! Length of CSTATE,
     :  LMODE       !   "    "  CMODE
      LOGICAL
     :  NONPEN      ! Flag; are there no catalogues open?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       If there are no open catalogues initialise the expression parser.

         NONPEN = .TRUE.

         IF (NCATS__CAT1 .GT. 0) THEN
            DO CURCAT = 1, CAT__MXCAT
               IF (CIDS__CAT1(CURCAT) .NE. CAT__NOID) THEN
                  NONPEN = .FALSE.
               END IF
            END DO
         END IF

         IF (NONPEN) THEN
            CALL ANT_XINIT (STATUS)
C           print3000
C3000       format(1x, '** parser intialised.')
         END IF

*
*       Initialise the catalogue identifier to the null identifier.

         CI = CAT__NOID

*
*       Parse the combined catalogue name to obtain the genuine
*       catalogue name, directory path and back-end type.

         CALL CAT1_CNMPR (CNAME, CATNAM, CATFIL, BCKTYP, EXTRA,
     :     STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Take local copies of STATE and MODE and force them into
*          upper case.

            CSTATE = STATE
            CALL CHR_UCASE (CSTATE)

            CMODE = MODE
            CALL CHR_UCASE (CMODE)

*
*          Convert the character representations of the STATE and MODE
*          to the equivalent internal integer codes.

            IF (CSTATE .EQ. 'NEW' )THEN
               ISTATE = CAT1__STNEW

            ELSE IF (CSTATE .EQ. 'OLD') THEN
               ISTATE = CAT1__STOLD

            ELSE
               STATUS = CAT__ILLOP

               ERRBUF = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('CAT1_TOPEN: illegal value of argument '/
     :           /'STATE: ', ERRBUF, ERRLEN)

               IF (CSTATE .NE. ' ') THEN
                  LSTATE = CHR_LEN(CSTATE)
                  CALL CHR_PUTC (CSTATE(1 : LSTATE), ERRBUF, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
               END IF

               CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

               CALL CAT1_ERREP ('CAT1_TOPEN_STA', ERRBUF(1 : ERRLEN),
     :           STATUS)
            END IF

            IF (CMODE .EQ. 'READ') THEN
               IMODE = CAT1__MDRD

            ELSE IF (CMODE .EQ. 'UPDATE') THEN
               IMODE = CAT1__MDUPD

            ELSE IF (CMODE .EQ. 'MODIFY') THEN
               IMODE = CAT1__MDMOD

            ELSE IF (CMODE .EQ. 'WRITE') THEN
               IMODE = CAT1__MDWRT

            ELSE
               STATUS = CAT__ILLOP

               ERRBUF = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('CAT1_TOPEN: illegal value of argument '/
     :           /'MODE: ', ERRBUF, ERRLEN)

               IF (CSTATE .NE. ' ') THEN
                  LMODE = CHR_LEN(CMODE)
                  CALL CHR_PUTC (CMODE(1 : LMODE), ERRBUF, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
               END IF

               CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

               CALL CAT1_ERREP ('CAT1_TOPEN_MOD', ERRBUF(1 : ERRLEN),
     :           STATUS)
            END IF

            IF (STATUS .EQ. CAT__OK) THEN

*
*             Attempt to open the catalogue using the specified STATE
*             and mode.

               IF (IMODE  .EQ. CAT1__MDWRT  .AND.
     :             ISTATE .EQ. CAT1__STNEW) THEN
                  CALL CAT0_CNEW (BCKTYP, CATNAM, CATFIL, EXTRA, CI,
     :              STATUS)

               ELSE IF (IMODE  .EQ. CAT1__MDWRT  .AND.
     :                  ISTATE .EQ. CAT1__STOLD) THEN
                  CONTINUE    ! 2B added...

               ELSE IF (IMODE  .EQ. CAT1__MDUPD  .AND.
     :                  ISTATE .EQ. CAT1__STNEW) THEN
                  STATUS = CAT__INVOP

               ELSE IF (IMODE  .EQ. CAT1__MDUPD  .AND.
     :                  ISTATE .EQ. CAT1__STOLD) THEN
                  CONTINUE    ! 2B added...

               ELSE IF (IMODE  .EQ. CAT1__MDMOD  .AND.
     :                  ISTATE .EQ. CAT1__STNEW) THEN
                  STATUS = CAT__INVOP

               ELSE IF (IMODE  .EQ. CAT1__MDMOD  .AND.
     :                  ISTATE .EQ. CAT1__STOLD) THEN
                  CONTINUE    ! 2B added...

               ELSE IF (IMODE  .EQ. CAT1__MDRD  .AND.
     :                  ISTATE .EQ. CAT1__STNEW) THEN
                  STATUS = CAT__INVOP

                ELSE IF (IMODE  .EQ. CAT1__MDRD  .AND.
     :                   ISTATE .EQ. CAT1__STOLD) THEN
                  CALL CAT0_OPNEX (BCKTYP, CATNAM, CATFIL, EXTRA,
     :              'READ', CI, STATUS)

               ELSE
                  STATUS = CAT__ILLOP
               END IF

*
*             Save the integer codes for the STATE and MODE and
*             initialise the AST directives.

C              print4000, ci, cielm
C4000          format(1x, 'ci, cielm: ', i5, i5 )

               IF (STATUS .EQ. CAT__OK) THEN
                  CALL CAT1_CIELM (CI, CIELM, STATUS)

                  IF (STATUS .EQ. CAT__OK) THEN
                     MODE__CAT1(CIELM) = IMODE
                     STATE__CAT1(CIELM) = ISTATE

                     SKYFR__AST(CIELM) = .TRUE.
                     DOMAN__AST(CIELM) = 'SKY'
                     COLS__AST(CIELM, 1) = 'RA'
                     COLS__AST(CIELM, 2) = 'DEC'
                     ATTRB__AST(CIELM) = ' '
                     SYS__AST(CIELM) = ' '
                     EPOCH__AST(CIELM) = ' '
                     EQUIN__AST(CIELM) =  ' '
                  END IF
               END IF

            END IF

         END IF

      END IF

      END
