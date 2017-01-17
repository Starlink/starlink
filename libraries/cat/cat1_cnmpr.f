      SUBROUTINE CAT1_CNMPR (CNAME, CATNAM, CATFIL, BCKTYP,
     :  EXTRA, STATUS)
*+
*  Name:
*     CAT1_CNMPR
*  Purpose:
*     Parse the full catalogue name.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CNMPR (CNAME; CATNAM, CATFIL, BCKTYP, EXTRA; STATUS)

*       STATUS)
*  Description:
*     Parse the full catalogue name to get the actual catalogue name,
*     the catalogue file name, the back-end type and any extra
*     information.
*
*     This routine parses a combined catalogue name in order to
*     determine the catalogue file name, the catalogue name,
*     the type of the catalogue (that is, format of the catalogue and
*     hence the back-end to be used to access it: FITS, ADC, CHI etc.)
*     and any extra back-end specific information.  The full catalogue
*     name comprises four components:
*
*      *  the directory specification,
*      *  the actual name of the catalogue in the chosen directory,
*      *  the catalogue back-end type (FITS etc),
*      *  optional back-end specific information (for example, the
*         extension number of a table in a FITS file).
*
*     The components occur in this order.  Of the four, only the
*     actual name of the catalogue is mandatory.  The actual name of
*     the catalogue is the same as the name of the file which
*     instantiates it, without any file type.  The file type is used to
*     define the catalogue back-end type.  For example, FITS catalogue
*     MYCAT would be held as file MYCAT.FIT.
*
*     Both VMS and unix directory specifications are supported.  VMS
*     directory specifications may be logical names as well as absolute
*     specifications.
*
*     Every catalogue back-end type supported by CAT has an
*     identifying file type associated with it.  The file types for the
*     various back-ends are:
*
*        file type  back-end type
*        ---------  -------------
*      *    DAT    -    ADC,
*      *    FIT    -    FITS,
*      *    FITS   -     "  ,
*      *    GSC    -     "  ,
*      *    SDF    -    CHI/HDS,
*      *    TXT    -    STL (Small Text List).
*
*     Note that the check for file type is case independent.
*
*     If the file type is omitted a FITS file is assumed.
*
*     Examples:
*
*     MYCAT                            (directory spec. omitted)
*     MYCAT.FIT                        (    "      "  .    "   )
*
*     MYDIR:MYCAT                      (VMS, logical name)
*     MYDIR:MYCAT.FIT                  ( " ,    "     "  )
*
*     DISK$USER:[MYACC]MYCAT           (VMS, full directory spec.)
*     DISK$USER:[MYACC]MYCAT.FIT       ( " ,  "       "      "  .)
*
*     /usr/home/myacc/mycat            (unix)
*     /usr/home/myacc/mycat.fit        ( "  )
*  Arguments:
*     CNAME  =  CHARACTER*(*) (Given)
*         Combined catalogue name (optionally includes directory spec,
*         back-end type and extra back-end specific information).
*     CATNAM  =  CHARACTER*(*) (Returned)
*         Actual catalogue name.
*     CATFIL  =  CHARACTER*(*) (Returned)
*         Catalogue file name.
*     BCKTYP  =  INTEGER (Returned)
*         Code for the back-end type.
*     EXTRA  =  CHARACTER*(*) (Returned)
*         Optional extra information necessary to identify the
*         catalogue.  This information is back-end specific.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the string is not blank then
*       Take a working copy.
*       Remove any leading blanks.
*       Locate the starting points for the catalogue name, file type
*       and extra information.
*       Extract any directory specification.
*       Extract the catalogue name.
*       Extract the file type and determine the back-end type from it.
*       Extract any extra information.
*       If ok then
*         Assemble the full file specification.
*       end if
*     else
*       Set the status.
*     end if
*     Report any error.
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
*     DSB: David S Berry (EAO)
*  History:
*     2/7/93  (ACD): Original version.
*     23/1/94 (ACD): Modified error reporting.
*     26/1/94 (ACD): Re-written to implement a revised syntax for
*        specifiying the catalogue name, back-end type and extra
*        information.
*     10/5/95 (ACD): Modified to handle HST GSC FITS tables (file type
*        .GSC and completely numeric file names).
*     27/2/96 (ACD): Modified to handle file type '.FITS' for FITS
*        tables.
*     11/7/96 (ACD): Added the small text list (STL) file type ('.TXT').
*     23/7/96 (ACD): Added return argument for the catalogue file type.
*     29/5/98 (ACD): Re-written from scratch.
*     15/6/99 (ACD): Added the tab-separated table (TST) file type ('.TST').
*     22/6/99 (ACD): Changed the TST file type to '.TAB'.
*     17/1/17 (DSB): Remove alphanumeric check on file name characters.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
*  Arguments Given:
      CHARACTER
     :  CNAME*(*)
*  Arguments Returned:
      CHARACTER
     :  CATNAM*(*),
     :  CATFIL*(*),
     :  EXTRA*(*)
      INTEGER
     :  BCKTYP
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      LOGICAL
     :  FNDDIR,  ! Flag: found directory?
     :  FNDTYP,  ! Flag: found file type?
     :  FNDXTR,  ! Flag: found extra information?
     :  CNMALM   ! Flag; is the catalogue name alphanumeric?
      INTEGER
     :  POSDIR,  ! Position of directory specification.
     :  POSTYP,  ! Position of file type.
     :  POSXTR,  ! Position of extra information.
     :  POSNAM,  ! Position of catalogue name.
     :  LOOP     ! Loop index.
      INTEGER
     :  LCNAMC,  ! Length of CNAMEC (excl. trail. blanks).
     :  LCATDR,  !   "    "  CATDIR ( "  .   "  .   "   ).
     :  LCATNM,  !   "    "  CATNAM ( "  .   "  .   "   ).
     :  LCATYP,  !   "    "  CATTYP ( "  .   "  .   "   ).
     :  LCATFL,  !   "    "  CATFIL ( "  .   "  .   "   ).
     :  ERRLEN,  !   "    "  ERRMSG ( "  .   "  .   "   ).
     :  STOP     ! Stop position of current sub-string.
      CHARACTER
     :  CNAMEC*(CAT1__SZDIR + CAT__SZCNM + 16),   ! Copy of CNAME.
     :  CATDIR*(CAT1__SZDIR),  ! Directory specification.
     :  CATTYP*5,              ! Catalogue file type.
     :  CATYPU*5,              ! Catalogue file type (upper case).
     :  ERRMSG*75              ! Text of error message.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the combined name is not completely blank.

         IF (CNAME .NE. ' ') THEN

*
*          Take a copy of the combined name and remove any leading
*          blanks.

            CNAMEC = CNAME
            CALL CHR_LDBLK (CNAMEC)

*
*          Locate the starting points for the catalogue name,
*          file type and extra information.  The possible
*          terminators for any directory specification are:
*
*          : - VMS logical name,
*          ] - VMS full file specification,
*          / - unix file specification.
*
*          The characters preceding the file type and extra
*          information are:
*
*          . - file type,
*          { - extra information.

            FNDDIR = .FALSE.
            FNDTYP = .FALSE.
            FNDXTR = .FALSE.

            POSNAM = 1

            LCNAMC = CHR_LEN(CNAMEC)

            DO LOOP = LCNAMC, 1, -1
               IF (.NOT. FNDDIR) THEN
                  IF (CNAMEC(LOOP : LOOP) .EQ. ':'  .OR.
     :                CNAMEC(LOOP : LOOP) .EQ. ']'  .OR.
     :                CNAMEC(LOOP : LOOP) .EQ. '/') THEN
                     FNDDIR = .TRUE.
                     POSDIR = LOOP
                     POSNAM = LOOP + 1
                  END IF
               END IF

               IF (.NOT. FNDTYP) THEN
                  IF (CNAMEC(LOOP : LOOP) .EQ. '.') THEN
                     FNDTYP = .TRUE.
                     POSTYP = LOOP + 1
                  END IF
               END IF

               IF (.NOT. FNDXTR) THEN
                  IF (CNAMEC(LOOP : LOOP) .EQ. '{') THEN
                     FNDXTR = .TRUE.
                     POSXTR = LOOP + 1
                  END IF
               END IF
            END DO

*
*          Extract any directory specification.  Note that the
*          directory specification includes any final separator
*          because the separator is different on VMS and Unix.

            IF (FNDDIR) THEN
               IF (POSDIR .GT. 1) THEN
                  CATDIR = CNAMEC(1 : POSDIR)
               ELSE
                  STATUS = CAT__INVCN
               END IF
            ELSE
               CATDIR = ' '
            END IF

*
*          Extract the catalogue name and check that it contains only
*          alphanumeric characters.

            IF (FNDTYP) THEN
               STOP = POSTYP - 2
            ELSE
               IF (FNDXTR) THEN
                  STOP = POSXTR - 2
               ELSE
                  STOP = LCNAMC
               END IF
            END IF

            IF (POSNAM .LE. STOP) THEN
               CATNAM = CNAMEC(POSNAM : STOP)
            ELSE
               STATUS = CAT__INVCN
            END IF

*
*          Extract the file type and determine the back-end type from it.

            IF (FNDTYP) THEN
               IF (FNDXTR) THEN
                  STOP = POSXTR - 2
               ELSE
                  STOP = LCNAMC
               END IF

               IF (POSTYP .LE. STOP) THEN
                  CATTYP = CNAMEC(POSTYP : STOP)
                  CATYPU = CATTYP
                  CALL CHR_UCASE (CATYPU)

*
*                Attempt to convert the file type to a CAT back-end
*                type.  If the conversion fails then set the status.

                  IF (CATYPU .EQ. 'DAT') THEN
                     BCKTYP = CAT__BKADC

                  ELSE IF (CATYPU .EQ. 'FIT'  .OR.  CATYPU .EQ. 'FITS'
     :              .OR.  CATYPU .EQ. 'GSC') THEN
                     BCKTYP = CAT__BKFIT

                  ELSE IF (CATYPU .EQ. 'SDF') THEN
                     BCKTYP = CAT__BKCHI

                  ELSE IF (CATYPU .EQ. 'TXT') THEN
                     BCKTYP = CAT__BKSTL

                  ELSE IF (CATYPU .EQ. 'TAB') THEN
                     BCKTYP = CAT__BKTST

                  ELSE
                     STATUS = CAT__INVBK

                  END IF

               ELSE
                  STATUS = CAT__INVCN

               END IF

            ELSE
               CATTYP = 'FIT'
               BCKTYP = CAT__BKFIT

            END IF

*
*          Extract any extra information.

            IF (FNDXTR) THEN
               IF (CNAMEC(LCNAMC : LCNAMC) .EQ. '}') THEN
                  STOP = LCNAMC - 1
               ELSE
                  STOP = LCNAMC
               END IF

               IF (STOP .GE. POSXTR) THEN
                  EXTRA = CNAMEC(POSXTR : STOP)
               END IF

            ELSE
               EXTRA = ' '

            END IF

*
*          If all is ok then assemble the full file specification.

            IF (STATUS .EQ. CAT__OK) THEN
               CATFIL = ' '
               LCATFL = 0

*             ... directory specificiation,

               IF (FNDDIR) THEN
                  LCATDR = CHR_LEN(CATDIR)
                  CALL CHR_PUTC (CATDIR(1 : LCATDR), CATFIL, LCATFL)
               END IF

*             ... catalogue name,

               LCATNM = CHR_LEN(CATNAM)
               CALL CHR_PUTC (CATNAM(1 : LCATNM), CATFIL, LCATFL)

*             ... file type

               CALL CHR_PUTC ('.', CATFIL, LCATFL)

               LCATYP = CHR_LEN(CATTYP)
               CALL CHR_PUTC (CATTYP(1 : LCATYP), CATFIL, LCATFL)
            END IF

         ELSE

*
*          The catalogue name is blank; set the status.

            STATUS = CAT__INVCN

         END IF

*
*       Report any error and set the return arguments.

         IF (STATUS .NE. CAT__OK) THEN
            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Bad catalogue name: ', ERRMSG, ERRLEN)

            IF (CNAMEC .NE. ' ') THEN
               LCNAMC = CHR_LEN (CNAMEC)
               CALL CHR_PUTC (CNAMEC(1 : LCNAMC), ERRMSG, ERRLEN)
            ELSE
               CALL CHR_PUTC ('<blank>', ERRMSG, ERRLEN)
            END IF

            CALL CAT1_ERREP ('CAT1_CNMPR_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)

            CATNAM = ' '
            CATFIL = ' '
            BCKTYP = 0
            EXTRA = ' '
         END IF

C        print2000, cname, catnam, catfil, bcktyp, extra, status
C2000    format(1x, 'cname: ', a /
C    :     1x, 'catnam: ', a /
C    :     1x, 'catfil: ', a /
C    :     1x, 'bcktyp: ', i10 /
C    :     1x, 'extra: ', a /
C    :     1x, 'status: ' i20 / )

      END IF

      END
