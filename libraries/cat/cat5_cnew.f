      SUBROUTINE CAT5_CNEW (CATNAM, CATFIL, EXTRA, CI, STATUS)
*+
*  Name:
*     CAT5_CNEW
*  Purpose:
*     Create a new catalogue held as a small text list (STL).
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_CNEW (CATNAM, CATFIL, EXTRA; CI; STATUS)
*  Description:
*     Create a new catalogue held as a small text list (STL).
*  Arguments:
*     CATNAM  =  CHARACTER*(*) (Given)
*        The name of the catalogue.
*     CATFIL  =  CHARACTER*(*) (Given)
*        The file name of the catalogue.  It includes the file type
*        and optionally a directory specification.
*     EXTRA  =  CHARACTER*(*) (Given)
*        Any extra information needed to write the catalogue.  If the
*        argument contains a string starting with a 'K' (in either
*        case) then the catalogue is written in 'KAPPA format',
*        otherwise it is written as a standard format STL.
*     CI  =  INTEGER (Returned)
*        Identifier to the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Attempt to get an identifier for the catalogue.
*     If ok then
*       Add the details for the catalogue to the common block, including
*       setting the number of rows to zero.
*       Determine whether the catalogue is to be written in 'KAPPA
*       format' or as a standard STL.
*       Attempt to create the file to hold the FITS binary table.
*       Report any error creating this file.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     18/7/96  (ACD): Original version.
*     24/7/96  (ACD): First stable version.
*     10/12/96 (ACD): Added writing 'KAPPA format' STLs.
*     6/6/98   (ACD): Modified the handling of the catalogue file name.
*     18/11/98 (ACD): Improved the error reporting.
*     16/6/99  (ACD): Removed unused parametric constant.
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
      INCLUDE 'CAT5_STL_CMN'      ! Small text list common block.
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
*  Local Variables:
      INTEGER
     :  CIELM,     ! Common block array element for the catalogue.
     :  DFUNIT,    ! Fortran unit no. to access the catalogue.
     :  LSTAT,     ! Local Fortran status.
     :  LCNAM,     ! Length of CATNAM (excl. trail. blanks).
     :  LCFIL,     !   "    "  CATFIL ( "  .   "  .   "   ).
     :  ERRLEN     !   "    "  ERRBUF ( "  .   "  .   "   ).
      DOUBLE PRECISION
     :  CDATE      ! Catalogue modification date.
      LOGICAL
     :  KFLAG      ! Flag; is STL in 'KAPPA format' or standard.
      CHARACTER
     :  ERRBUF*75   ! Error message text.
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
            BKTYP__CAT1(CIELM) = CAT1__BKSTL
            EROW__CAT1(CIELM) = 0
            NROW__CAT1(CIELM) = 0
            NPCOL__CAT1(CIELM) = 0
            NVCOL__CAT1(CIELM) = 0
            NIND__CAT1(CIELM) = 0
            NPAR__CAT1(CIELM) = 0
            NSEL__CAT1(CIELM) = 0
            CROW__CAT1(CIELM) = 1
            FINSH__CAT1(CIELM) = .FALSE.

*
*          Determine whether the catalogue is to be written in 'KAPPA
*          format' or as a standard STL.

            IF (EXTRA(1 : 1) .EQ. 'K'  .OR.
     :          EXTRA(1 : 1) .EQ. 'k') THEN
               KFLAG__CAT5(CIELM) = .TRUE.
               KFLAG = .TRUE.
            ELSE
               KFLAG__CAT5(CIELM) = .FALSE.
               KFLAG = .FALSE.
            END IF

*
*          Attempt to create the file to hold the STL catalogue.
*          First get a free Fortran unit number.

            CALL CAT1_GETLU (DFUNIT, STATUS)
            STUNT__CAT5(CIELM) = DFUNIT

*
*          Now attempt to create the file.

            OPEN(UNIT=DFUNIT, STATUS='NEW', FILE=CATFIL, IOSTAT=LSTAT)
            CALL CAT1_IOERR (LSTAT, STATUS)

            IF (STATUS .EQ. CAT__OK) THEN
               IF (CATNAM .NE. ' ') THEN
                  LCNAM = CHR_LEN(CATNAM)
               ELSE
                  LCNAM = 1
               END IF

               IF (.NOT. KFLAG) THEN
                  WRITE(DFUNIT, 2000, IOSTAT=LSTAT) CATNAM(1 : LCNAM)
 2000             FORMAT(
     :              '!+' /
     :              '!  Catalogue: ', A /
     :              '!' /
     :              '!  This catalogue is formatted as a CURSA small ',
     :                   'text list (STL).' /
     :              '!  For a description of this format see Starlink ',
     :                'User Note 190 or URL' /
     :              '!  http://www.roe.ac.uk/acdwww/cursa/home.html.' /
     :              '!-' / )
               ELSE
                  WRITE(DFUNIT, 2001, IOSTAT=LSTAT) CATNAM(1 : LCNAM)
 2001             FORMAT(
     :              '# +++' /
     :              '#  Catalogue: ', A /
     :              '#' /
     :              '#  This catalogue is formatted as a KAPPA format ',
     :                 'CURSA small text list (STL).' /
     :              '#  For a description of this format see Starlink ',
     :                'User Note 190 or URL' /
     :              '#  http://www.roe.ac.uk/acdwww/cursa/home.html.' /
     :              '# ---' / )
               END IF

               CALL CAT1_IOERR (LSTAT, STATUS)

            END IF

*
*          Report any error.

            IF (STATUS .NE. CAT__OK) THEN
               ERRLEN = 0
               ERRBUF = ' '

               CALL CHR_PUTC ('Failed to create catalogue file ',
     :           ERRBUF, ERRLEN)

               IF (CATFIL .NE. ' ') THEN
                  LCFIL = CHR_LEN(CATFIL)
                  CALL CHR_PUTC (CATFIL(1 : LCFIL), ERRBUF, ERRLEN)
               ELSE
                  CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
               END IF

               CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

               CALL CAT1_ERREP ('CAT5_CNEW_CRT', ERRBUF(1 : ERRLEN),
     :           STATUS)
            END IF

         ELSE
            ERRLEN = 0
            ERRBUF = ' '

            CALL CHR_PUTC ('Failed to obtain an identifier for '/
     :           /'catalogue ', ERRBUF, ERRLEN)

            IF (CATNAM .NE. ' ') THEN
               LCNAM = CHR_LEN(CATNAM)
               CALL CHR_PUTC (CATNAM(1 : LCNAM), ERRBUF, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRBUF, ERRLEN)
            END IF

            CALL CHR_PUTC ('.', ERRBUF, ERRLEN)

            CALL CAT1_ERREP ('CAT5_CNEW_OID', ERRBUF(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
