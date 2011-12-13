      SUBROUTINE CAT3_PUTXT (CI, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT3_PUTXT
*  Purpose:
*     Put a line of textual information to a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_PUTXT (CI, CLASS, TEXT; STATUS)
*  Description:
*     Put a line of textual information to a catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Catalogue identifier.
*     CLASS   =  CHARACTER*(*) (Returned)
*         Class of the textual information.  The classes permitted for
*         putting text are:
*         COMMENT  -  intended for general comments,
*         HISTORY  -  intended for history information.
*     TEXT   =  CHARACTER*(*) (Given)
*        A single line of textual information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the class is 'HISTORY' or 'COMMENT' then
*       Get the array element for the catalogue.
*       Get the FITSIO unit number for the table.
*       If the current state accessing the catalogue is not 'in the
*       extension header' then
*         Attempt to move to the primary header.
*         If ok then
*           Attempt to move to the extension header.
*           If ok then
*             Set the state to 'in the extension header'.
*           else
*             Set the status.
*             Report an error.
*           end if
*         else
*           Set the status.
*           Report an error.
*         end if
*       end if
*       If ok then
*         Attempt to write the text to the table.
*       end if
*     else
*       Set the status.
*       Report error: attempt to put an line of text with an illegal
*       class.
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
*     23/9/94  (ACD): Original version.
*     20/3/96  (ACD): Fixed bug; variable FITSTT was not being
*        initialised.
*     25/11/96 (ACD): Moved initialisation of FITSTT to the correct
*        place.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end constants.
*  Global Variables:
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      INTEGER
     :  CI
      CHARACTER
     :  CLASS*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS    ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER FITOK     ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  FITUNT,  ! FITSIO unit number for the table.
     :  FITSTT,  ! FITSIO status.
     :  HDUTYP,  ! FITSIO code for the header type.
     :  LCLASS,  ! Length of CLASS  (excl. trail. blanks).
     :  ERRLEN   !   "    "  ERRTXT ( "  .   "  .   "   ).
      CHARACTER
     :  ERRTXT*75   ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the class for the line of text is one of the
*       permitted classes.

         IF (CLASS .EQ. 'COMMENT'  .OR.  CLASS .EQ. 'HISTORY') THEN

*
*          Get the array element for the catalogue and the FITSIO
*          unit number for the table.

            CALL CAT1_CIELM (CI, CIELM, STATUS)

            FITUNT = FUNT__CAT3(CIELM)

            FITSTT = FITOK

*
*          Check whether the current state accessing the textual
*          information is 'in the extension header'.  If not then
*          move to the extension header.

            IF (HSTAT__CAT3(CIELM) .NE. CAT3__HSTTE) THEN

*
*             Attempt to move to the primary header and proceed if ok.
*             This strategem is necessary in order to establish an
*             absolutely known position.

               CALL FTMAHD (FITUNT, 1, HDUTYP, FITSTT)
               IF (FITSTT .EQ. FITOK) THEN

*
*                Attempt to move to the extension header and proceed if
*                ok.

C                 print2000, extn__cat3(cielm)
C2000             format(1x, 'CAT3_PUTXT, extension header number: ',
C    :              i5 )

                  CALL FTMRHD (FITUNT, EXTN__CAT3(CIELM), HDUTYP,
     :              FITSTT)
                  IF (FITSTT .EQ. FITOK) THEN

*
*                   Set the state accessing text to 'in the extension
*                   header'.

                     HSTAT__CAT3(CIELM) = CAT3__HSTTE

                  ELSE

*
*                   Set the status and report an error.

                     STATUS = CAT__ERROR

                     CALL CAT3_FITER ('CAT3_PUTXT_RHD', 'Failed to '/
     :                 /'access the specified FITS extension header.',
     :                 FITSTT, STATUS)
                  END IF

               ELSE

*
*                Set the status and report an error.

                  STATUS = CAT__ERROR

                  CALL CAT3_FITER ('CAT3_PUTXT_RHD', 'Failed to '/
     :              /'access the FITS primary header.', FITSTT,
     :              STATUS)
               END IF

            END IF

*
*          If all is ok then attempt to write the text to the table,
*          using the appropriate class.

            IF (STATUS .EQ. CAT__OK) THEN
               IF (CLASS .EQ. 'COMMENT') THEN
                  CALL FTPCOM (FITUNT, TEXT, FITSTT)
               ELSE
                  CALL FTPHIS (FITUNT, TEXT, FITSTT)
               END IF

               IF (FITSTT .NE. FITOK) THEN
                  STATUS = CAT__ERROR

                  CALL CAT3_FITER ('CAT3_PUTXT_PUT', 'Failed to put '/
     :              /'a line of text to the table.', FITSTT, STATUS)
               END IF
            END IF
         ELSE

*
*          The given class does not correspond to one of the permitted
*          classes.  Set the status and report an error.

            STATUS = CAT__ERROR

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Text class supplied, ', ERRTXT, ERRLEN)

            IF (CLASS .NE. ' ') THEN
               LCLASS = CHR_LEN(CLASS)
               CALL CHR_PUTC (CLASS(1 : LCLASS), ERRTXT, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRTXT, ERRLEN)
            END IF

            CALL CHR_PUTC (', is illegal.  Line of text not written.',
     :        ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT3_PUTXT_ICL', ERRTXT(1 : ERRLEN),
     :        STATUS)

         END IF

      END IF

      END
