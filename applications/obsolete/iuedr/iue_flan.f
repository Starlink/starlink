      SUBROUTINE IUE_FLAN( TP, MAXLN, STATUS )
*+
*  Name:
*     SUBROUTINE IUE_FLAN

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IUE_FLAN( TP, MAXLN, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The VICAR header is read and printed up to a maximum of maxln
*     lines. Then the data array size is used to guess at what the
*     file contains assuming it was written by IUESIPS. Then, if the
*     file is a RAW, GPHOT or PHOT image, the image is read and the
*     statistics calculated. These disregard the quality information
*     and image geometry.
*
*     STATUS=MTEOV, MTEOT or MTEOF are possible returns.

*  Method:
*     The tape file number is obtained from the MT library and printed
*     if known.
*     The VICAR header is read from tape and written to a file.
*     Any non-printable characters are converted to blanks.
*     A limit of MAXLN is set on the number of header lines that are
*     stored in the file (a value -1 means all lines copied).
*     Then a block size analysis is done on the remaining part of the
*     file.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     07-SEP-81 (JRG):
*       AT4 version.
*     02-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     23-APR-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*       Inclusion of image statistics options.
*     01-OCT-92 (DMILLS)
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*     09-JUN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMISAF'

*  Arguments Given:
      INTEGER TP         ! Tape descriptor.
      INTEGER MAXLN      ! Maximum number of lines to be printed.

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Constants:
      INTEGER MAXL       ! Maximum number of IUE lines.
      INTEGER MAXS       ! Maximum number of IUE samples.
      INTEGER OK         ! OK status.
      PARAMETER ( MAXL = 768, MAXS = 768, OK = 0 )

      INTEGER OTHER      ! OTHER data type.
      INTEGER PHOT       ! PHOT image type index.
      INTEGER RAW        ! RAW image type index.
      PARAMETER ( OTHER = 3, PHOT = 2, RAW = 1 )

*  Local Variables:
      INTEGER NREC       ! Number of records (image lines).
      INTEGER RECSIZ     ! Size of a record containing an image line.
      INTEGER TYPE       ! Internal image type index.
*.

*  Write a line to show where start of file info is in listings.
      CALL LINE_WCONT( '%p--------------------------------------\\' )
      CALL LINE_WCONT( '----------------------------------------\\' )
      CALL PRTBUF( STATUS )

*  Get and print tape file number.
      IF ( .NOT. ISAFILE ) THEN
         CALL TAPE_SHOP( STATUS )
      END IF

      IF ( STATUS .EQ. OK ) THEN
         CALL PRTEOL( STATUS )

*     Read VICAR header.
         CALL VIC_TRHD( TP, MAXLN, NREC, RECSIZ, STATUS )
         IF ( STATUS .EQ. OK ) THEN

*        Print record size of image and number of image lines.
            CALL PRTEOL( STATUS )
            CALL line_WRITI( '%p Data part contains %i lines\\', NREC )
            CALL line_WRITI( ' of size %i bytes.\\', RECSIZ )
            CALL PRTBUF( STATUS )

*        Initialise TYPE variable.
            TYPE = OTHER

*        Guess at contents.
            CALL line_WCONT( '%p This is\\' )
            IF ( NREC .EQ. MAXL ) THEN
               IF ( RECSIZ .EQ. MAXS ) THEN
                  CALL LINE_WCONT( ' a Raw Image (IUE_RAW)\\' )
                  TYPE = RAW

               ELSE IF ( RECSIZ .EQ. MAXS * 2 ) THEN
                  CALL LINE_WCONT(
     :              ' a photometric image (IUE_GPHOT or IUE_PHOT)\\' )
                  TYPE = PHOT

               ELSE
                  CALL LINE_WCONT(
     :                 ' not a recognised IUESIPS format\\' )
               END IF

            ELSE IF (RECSIZ.EQ.MAXL*2) THEN
               CALL line_WCONT(' a Rotated Image Segment (IUE_RIS)\\')

            ELSE IF ( RECSIZ.EQ.1204 .OR. RECSIZ.EQ.2048 ) THEN
               IF ( NREC .LE. 7 ) THEN
                  CALL LINE_WCONT(
     :                 ' a Low Resolution Spectrum (IUE_LORES)\\' )

               ELSE IF ( NREC .LE. 200 ) THEN
                  CALL LINE_WCONT(
     :                 ' a Line By Line Spectrum (IUE_LBLS)\\' )

               ELSE
                  CALL LINE_WCONT(
     :                 ' a High Resolution Spectrum (IUE_HIRES)\\' )
               END IF

               IF ( RECSIZ .EQ. 1204 ) THEN
                  CALL LINE_WCONT( ' from IUESIPS#1\\' )

               ELSE
                  CALL LINE_WCONT( ' from IUESIPS#2\\' )
               END IF

            ELSE IF ( RECSIZ.EQ.1000 .OR. RECSIZ.EQ.2000 ) THEN
               IF ( NREC .LE. 7 ) THEN
                  CALL LINE_WCONT(
     :                 ' a Low Resolution Spectrum (IUE_LORES)\\' )

               ELSE IF ( NREC .LE. 200 ) THEN
                  CALL LINE_WCONT(
     :                 ' a Line By Line Spectrum (IUE_LBLS)\\' )

               ELSE
                  CALL LINE_WCONT(
     :                 ' a High Resolution Spectrum (IUE_HIRES)\\' )
               END IF

               IF ( RECSIZ .EQ. 1000 ) THEN
                  CALL LINE_WCONT( ' from IUESIPS#1\\' )

               ELSE
                  CALL LINE_WCONT( ' from IUESIPS#2\\' )
               END IF

            ELSE
               CALL LINE_WCONT( ' not a recognised IUESIPS format\\' )
            END IF

            CALL LINE_WCONT( '.\\' )
            CALL PRTBUF( STATUS )
         END IF
      END IF

      END
