      SUBROUTINE CAT3_CNEW (CATNAM, CATFIL, EXTRA, CI, STATUS)
*+
*  Name:
*     CAT3_CNEW
*  Purpose:
*     Create a new catalogue held as a FITS binary table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_CNEW (CATNAM, CATFIL, EXTRA; CI; STATUS)
*  Description:
*     Create a new catalogue held as a FITS binary table.
*  Arguments:
*     CATNAM  =  CHARACTER*(*) (Given)
*        The name of the catalogue.
*     CATFIL  =  CHARACTER*(*) (Given)
*        The file name of the catalogue.  It includes the file type
*        and optionally a directory specification.
*     EXTRA  =  CHARACTER*(*) (Given)
*        Any extra information needed to write the catalogue.  This
*        argument is not used in the FITS back-end.
*     CI  =  INTEGER (Returned)
*        Identifier to the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the catalogue.
*     If ok then
*       Add the details for the catalogue to the common block, including
*       setting the number of rows to zero.
*       Attempt to get a free FITS unit number.
*       Attempt to create the file to hold the FITS binary table.
*       If ok then
*         Copy the FITS unit number to the common block.
*       else
*         Report any error creating this file.
*       end if
*     else
*       Report an error.
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
*     5/8/93   (ACD): Original version.
*     7/9/93   (ACD): First stable version.
*     23/1/94  (ACD): Modified error reporting.
*     10/12/96 (ACD): Added argument EXTRA.
*     4/6/98   (ACD): Modified the handling of the catalogue file name
*       and the error reporting.
*     26/10/99 (ACD): Modified to get a free unit number using FTGIOU
*       rather than CAT1_GETLU.  This change was necessary for CFITSIO.
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
      INCLUDE 'CAT1_CATS_CMN'     ! Catalogues common block.
      INCLUDE 'CAT3_FIT_CMN'       ! FITS common block.
*  Arguments Given:
      CHARACTER
     :  CATNAM*(*),
     :  CATFIL*(*),
     :  EXTRA*(*)
*  Arguments Returned:
      INTEGER
     :  CI
*  Status:
      INTEGER STATUS   ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
       INTEGER FITOK   ! FITSIO Success status.
       PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  CIELM,     ! Common block array element for the catalogue.
     :  FITUNT,    ! Fortran unit no. for access the FITS file.
     :  FITSTT     ! FITSIO running status.
      CHARACTER
     :  ERRBUF*75  ! Text of error message.
      INTEGER
     :  ERRLEN,    ! Length of ERRBUF (excl. trail. blanks).
     :  LCATFL     !   "    "  CATFIL ( "  .   "  .   "   ).
      DOUBLE PRECISION
     :  CDATE      ! Catalogue modification date.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Attempt to create a new catalogue identifier and add it to the
*       list of identifiers.  Also store the attributes which pertain to
*       the entire catalogue, specifically its name and the modification
*       date.

         CALL CAT1_CRTCI (CI, STATUS)
         CALL CAT1_CIELM (CI, CIELM, STATUS)

         CALL CAT1_ADDAC (CI, 'NAME', .FALSE., CATNAM, STATUS)

         CALL CAT1_GTDAT (CDATE, STATUS)
         CALL CAT1_ADDAD (CI, 'DATE', .TRUE., CDATE, STATUS)

         IF (STATUS .EQ. CAT__OK) THEN

*
*          Add the details for the catalogue to the common block,
*          including setting the number of rows to zero.

            CIDS__CAT1(CIELM) = CI
            BKTYP__CAT1(CIELM) = CAT1__BKFIT
            NROW__CAT1(CIELM) = 0
            NPCOL__CAT1(CIELM) = 0
            NVCOL__CAT1(CIELM) = 0
            NIND__CAT1(CIELM) = 0
            NPAR__CAT1(CIELM) = 0
            NSEL__CAT1(CIELM) = 0
            CROW__CAT1(CIELM) = 1
            FINSH__CAT1(CIELM) = .FALSE.

*
*          Attempt to create the file to hold the FITS binary table.
*          First get a free FITS unit number, then attempt to create
*          the file.

            FITSTT = FITOK

            CALL FTGIOU (FITUNT, FITSTT)
            CALL FTINIT (FITUNT, CATFIL, 2880, FITSTT)

*
*          If ok then copy the FITS unit number to the common block;
*          otherwise report an error.

            IF (FITSTT .EQ. FITOK) THEN
               FUNT__CAT3(CIELM) = FITUNT

            ELSE
               STATUS = CAT__NOCAT

               ERRBUF = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Failed to create FITS file ',  ERRBUF,
     :           ERRLEN)

               IF (CATFIL .NE. ' ') THEN
                  LCATFL = CHR_LEN(CATFIL)
                  CALL CHR_PUTC (CATFIL(1 :  LCATFL), ERRBUF, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
               END IF

               CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

               CALL CAT3_FITER ('CAT3_CNEW_CRT', ERRBUF(1 : ERRLEN),
     :           FITSTT, STATUS)
            END IF

         ELSE
            CALL CAT1_ERREP ('CAT3_CNEW_OID', 'Failed to obtain '/
     :        /'catalogue identifier', STATUS)

         END IF

      END IF

      END
