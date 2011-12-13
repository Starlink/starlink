      SUBROUTINE NDF2BDF( STATUS )
*+
*  Name:
*     NDF2BDF

*  Purpose:
*     Converts an NDF to a Starlink Interim BDF file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2BDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an NDF (see SUN/33) to the Bulk Data
*     Frame (BDF) format used by the INTERIM Environment (see SUN/4).
*     Type conversion may be performed at the same time.

*  Usage:
*     NDF2BDF IN OUT [TYPE] [DESCRIP]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The NDF to be converted to a BDF.  The suggested default is
*        the current NDF if one exists, otherwise it is the current
*        value.
*     OUT = BDF (Write)
*        The name of BDF converted from the NDF.  No file extension
*        should be given, as the application will automatically give
*        extension ".BDF".
*     TYPE = LITERAL (Read)
*        The data type of the output BDF.  It must be one of the
*        following Interim types: "SB", "SW", "R", "SL", "DP", "UB",
*        "UW" corresponding to signed byte, signed word, real, signed
*        longword, double precision, unsigned byte, unsigned word.
*        See SUN/4 for further details.  The default is the type
*        corresponding to that of the NDF.  []
*     DESCRIP = _LOGICAL (Read)
*        If true the keyword and values in a FITS extension are copied
*        to the BDF's descriptors with a number of exceptions listed
*        in the Notes.  [FALSE]
*     CONNECT = FILENAME (Write)
*        The Interim connection file.  It is deleted when the
*        application terminates.  [NDF2BDF.TMP]
*     COMMAND = FILENAME (Write)
*        The Interim command file.  It is deleted when the application
*        terminates.  [USERCOM.TMP]

*  Examples:
*     NDF2BDF NEW OLD
*        This converts the NDF called NEW (in file NEW.SDF) to the
*        BDF called OLD (in file OLD.BDF).  OLD's data array will have
*        the same data type as that of NEW.  The FITS header within
*        NEW is converted to descriptors within OLD.
*     NDF2BDF NEW OLD DESCRIP
*        This converts the NDF called NEW (in file NEW.SDF) to the
*        BDF called OLD (in file OLD.BDF).  OLD's data array will have
*        the same data type as that of NEW.  The FITS header within
*        NEW is converted to descriptors within OLD, and are reported
*        to the user.
*     NDF2BDF HORSE HORSE TYPE=R
*        This converts the NDF called HORSE (in file HORSE.SDF) to the
*        BDF also called HORSE (in file HORSE.BDF).  The BDF's data
*        array will contain 4-byte floating-point numbers.  The FITS
*        header within HORSE is converted to descriptors within
*        HORSE.BDF.

*  Notes:
*     The details of the conversion are as follows:
*        -  the NDF main data array is written to the BDF data array.
*        -  QUALITY, and VARIANCE have no counterparts in the BDF, and
*        so cannot be propagated.
*        -  HISTORY is not propagated.
*        -  UNITS is written to descriptor BUNITS.
*        -  The number of dimensions of the data array is written
*        to the BDF descriptor NAXIS, and the actual dimensions to
*        NAXIS1, NAXIS2 etc. as appropriate.
*        -  If the NDF contains any linear axis structures the
*        information necessary to generate these structures is written
*        to the BDF descriptors (except when there is a non-zero value
*        of CROTAn in the FITS extension).  For example, if a linear
*        AXIS(1) structure exists in the input NDF the value of the
*        first data point is stored in the BDF descriptor CRVAL1,
*        and the incremental value between successive axis data is
*        stored in CDELT1. If there is an axis label it is written to
*        descriptor CRTYPE1, and axis unit is written to CTYPE1.
*        (Similarly for AXIS(2) structures etc.) FITS does not have a
*        standard method of storing axis widths and variances, so these
*        NDF components will not be propagated.  Non-linear axis data
*        arrays cannot be represented by CRVALn and CDELTn, and must be
*        ignored.
*        -  If the input NDF contains TITLE and LABEL components these
*        are stored in the BDF descriptors TITLE and LABEL.
*        -  If the input NDF contains a FITS extension, the FITS items
*        may be written to the BDF descriptors, with the following
*        exceptions:
*           o  NAXIS, and NAXISn are derived from the dimensions of
*           the NDF data array as described above, so these items
*           are not copied from the NDF FITS extension.
*           o  The TITLE, LABEL, and BUNITS descriptors are only copied
*           if no TITLE, LABEL, and UNITS NDF components have already
*           been copied into these descriptors.
*           o  The CDELTn, CRVALn, CRPIXn, CTYPEn, and CRTYPEn
*           descriptors in the FITS extension are only copied if the
*           input NDF contained no linear axis structures or there is a
*           non-zero CROTAn descriptor.
*           o  The standard order of the FITS keywords is preserved,
*           thus NAXIS and NAXISn appear immediately after the second
*           card image, which should be BITPIX.  No FITS comments are
*           written following the values of the above exceptions for
*           compatibility with certain INTERIM applications.
*           FITS-header lines with blank keywords are not copied.
*        -  Other extensions have no BDF counterparts and therefore are
*        not propagated.
*        -  All character objects longer than 70 characters are
*        truncated in a BDF descriptor.

*  Algorithm:
*     -  Get and validate the Interim format code for the output BDF.
*     -  Create a old-style Starlink 'connection file' for a program
*     NDF2BDF.
*     -  Open the input NDF and map the main data array.
*     -  Get the parameter BDFFILE and create a RUNSTAR-type command
*     line in another file. Assign the logical names PROGCON and
*     USERCOM to these files. This will fool the old-style Starlink
*     Interface Routines into working correctly.
*     -  Create and map the required data into VM using the old-style
*     routine WRDATA.
*     -  Copy the data from old-style to new-style VM.
*     -  Establish the values for the FITS-like descriptors NAXIS,
*     NAXISn, CDELTn, CRVALn, CRTYPEn, CTYPEn, BUNITS, TITLE and LABEL
*     from the NDF standard items if possible. These are copied into
*     the appropriate BDF FITS descriptors.
*     -  If the NDF contains a FITS extension, those items not already
*     set, are copied to the appropriate BDF descriptor.  Detect any
*     rotated axis.
*     -  Handle rotated axes by re-writing the axis descriptors (CRVALn,
*     CDELTn, CRPIXn) from the FITS headers.
*     -  Reset INTERIM environment in case NDF2BDF is re-run without re-
*     loading.

*  Related Applications:
*     CONVERT: BDF2NDF.

*  Implementation Status:
*     -  Primitive NDFs are created.
*     -  The value of bad pixels is not written to the descriptor BLANK.
*     -  Only available on VMS platforms.

*  Copyright:
*     Copyright (C) 1991-1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 March 5th (JM):
*        Adapted from STAROUT.
*     1991 October 30 (MJC):
*        Changed FORMAT parameter to TYPE for consistency with the paper
*        documentation.  Also gave TYPE a dynamic default of the type of
*        the NDF.  Added the ADAM parameters and Usage sections to the
*        prologue.  Tidied the prologue.
*     1992 January 30 (MJC):
*        Renamed the parameters for the NDF and BDF to IN and OUT
*        respectively to agree with SUN/55 and consistency.  Added
*        examples.  Processes UNITS and AXIS.LABEL components of the
*        NDF.
*     1992 September 5 (MJC):
*        Prevented special keywords from being copied from the FITS
*        header when there are overriding objects present in the
*        NDF; these are formatted into FITS-like descriptors.
*     1992 November 16 (MJC):
*        Made more robust for the case when there are duplicated
*        standard FITS headers at the start of the FITS extension.
*        Allowed for non-zero axis rotation so that axis information is
*        not lost when converting a BDF to an NDF and then back to a
*        BDF.
*     1993 January 4 (MJC):
*        Fixed a bug that made it was possible not to write mandatory
*        descriptors to the BDF.  Allowed lowercase Interim types.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2009 June 29 (MJC):
*        Replaced deprecated CON_MOVE (and CON_TYPSZ) with KPG1_COPY
*        from KAPLIBS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE                  ! No implicit typing

*  Global constants:
      INCLUDE 'SAE_PAR'              ! Standard ADAM constants
      INCLUDE 'PAR_ERR'              ! Parameter-system error codes
      INCLUDE 'PRM_PAR'              ! PRIMDAT public constants
      INCLUDE 'NDF_PAR'              ! NDF_ public constants
      INCLUDE 'FIO_PAR'              ! FIO_ constants
      INCLUDE 'CNF_PAR'              ! For CNF_PVAL function

*  Status:
      INTEGER STATUS                 ! Global status

*  External References:
      INTEGER CHR_LEN                ! Get effective length of string

*  Local Constants:
      INTEGER   NFLAGS              ! Number of flags to indicate
                                    ! presence special NDF components
      PARAMETER( NFLAGS = 6 )
      INTEGER   SZDESC              ! Size of descriptor names
      PARAMETER( SZDESC = 8 )
      INTEGER   SZFITS              ! Size of FITS string
      PARAMETER( SZFITS = 80 )
      INTEGER   SZVAL               ! Size of descriptor values
      PARAMETER( SZVAL = 70 )

*  Local Variables:
      INTEGER   APNTR(DAT__MXDIM)   ! Pointers to NDF axis arrays
      INTEGER   ADIM                ! Axis loop counter
      LOGICAL   AXIFND              ! True if NDF contains a linear axis
                                    ! comps.
      LOGICAL   AXLFND              ! True if NDF contains axis label
      REAL      AXROT               ! Rotation angle of an axis
      LOGICAL   AXUFND              ! True if NDF contains axis units
      CHARACTER BDFILE*(FIO__SZFNM) ! BDF filename
      INTEGER   BDFORM              ! Format in BDF style
      CHARACTER C*1                 ! Accommodates character string
      CHARACTER CDELT*(SZDESC)      ! Descriptor name of CDELTn
      CHARACTER CMDFIL*(FIO__SZFNM) ! Command filename
      LOGICAL   CMPFND( NFLAGS )    ! True if certain special NDF
                                    ! components are present
      CHARACTER COMAND*(80)         ! RUNSTAR-type command line
      CHARACTER CONECT*(FIO__SZFNM) ! Connection filename
      CHARACTER CRPIX*(SZDESC)      ! Descriptor name of CRPIXn
      CHARACTER CRVAL*(SZDESC)      ! Descriptor name of CRVALn
      CHARACTER CVALUE*(SZVAL)      ! Accommodates descriptor value
      CHARACTER DESCR*(SZDESC)      ! Accommodates descriptor name
      CHARACTER DEFFMT*(2)          ! Default Interim data format
      LOGICAL   DESCRP              ! True if descriptors output to user
      INTEGER   DIMS(DAT__MXDIM)    ! IMAGE dimensions (axis length)
      INTEGER   FD                  ! File descriptor
      LOGICAL   FITSPR              ! True if FITS extension is present
      CHARACTER FITSTR*(SZFITS)     ! FITS string
      CHARACTER FORMAT*(2)          ! Interim data format required
      CHARACTER FTLOC*(DAT__SZLOC)  ! Locator to NDF FITS extension
      CHARACTER FTLOCI*(DAT__SZLOC) ! Locator to NDF FITS extension
      CHARACTER HDSTYP*(DAT__SZTYP) ! HDS type data type
      INTEGER   I                   ! Loop variable
      INTEGER   IMAGEP              ! Pointer to BDF data
      REAL      INCREM              ! Incremental value for axis array
      INTEGER   ISTAT               ! Local status return
      INTEGER   J                   ! Loop variable
      LOGICAL   LABFND              ! True if NDF LABEL found
      LOGICAL   LINEAR              ! True if an axis is linear
      INTEGER   NBPI                ! Number of bytes per item
      INTEGER   NCHAR               ! Length of a character string
      INTEGER   NCOMP               ! No. of components
      INTEGER   NDF                 ! NDF identifier
      CHARACTER * (NDF__SZTYP) NDFTYP ! Data type of the NDF
      INTEGER   NDIM                ! Number of dimensions
      INTEGER   NELM                ! Number of elements
      INTEGER   NITEM               ! Number of items in data
      INTEGER   POINTR              ! Pointer to DSS data
      LOGICAL   ROTAX( DAT__MXDIM ) ! True if an axis is rotated in the
                                    ! FITS extension
      REAL      START               ! Start value for an axis structure
      LOGICAL   STDDSR              ! True if BDF has had its mandatory
                                    ! descriptors written
      LOGICAL   THERE               ! True if NDF has FITS extension
      LOGICAL   TITFND              ! True if NDF TITLE found
      LOGICAL   UNTFND              ! True if NDF UNITS found
      CHARACTER VALUE*(SZVAL)       ! Accommodates descriptor value
      INTEGER   WRSTAT              ! Status for WRDSCR routine

*.

*   Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Activate the FIO package.
      CALL FIO_ACTIV( STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 998

*   Obtain the NDF and its shape and type.
*   ======================================

*   Get the input NDF structure.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

*   Locate, find shape and type of the data array.
      CALL NDF_DIM( NDF, DAT__MXDIM, DIMS, NDIM, STATUS )
      CALL NDF_TYPE( NDF, 'Data', NDFTYP, STATUS )

*   Obtain the output data type in HDS and Interim forms.
*   =====================================================

*   Convert the HDS type to an Interim type to be used as the
*   dynamic default.
      CALL CON_H2ITY( NDFTYP, DEFFMT, STATUS )

*   Get Interim format of data required, using NDF's data type as the
*   default.  Note this is in uppercase as this is all Interim will
*   recognise.
      CALL PAR_CHOIC( 'TYPE', DEFFMT, 'R,DP,SB,SW,SL,UB,UW', .FALSE.,
     :                FORMAT, STATUS )

*   Convert format to old-style value and get its HDS equivalent.
      CALL CON_INTCL( FORMAT, BDFORM, NBPI, STATUS )
      CALL CON_I2HTY( FORMAT, HDSTYP, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 996

*   Map the NDF data array.
*   =======================

*   Map the input data-array component in the format required for
*   the output BDF.
      CALL NDF_MAP( NDF, 'Data', HDSTYP, 'READ', POINTR, NITEM,
     :              STATUS )

*   Create Interim-like files and point logicals to them.
*   =====================================================
*
*   Here we must simulate an Interim environment application running.
*   Two files are created: the connection file (equivalent to a
*   modern interface file), and a command file.  Logical names used
*   by Interim's RUNSTAR must also be defined.

*   Create a 'connection file' for the program NDF2BDF.
      CALL PAR_GET0C ('CONNECT', CONECT, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 996

*   Open the connection file.
      CALL FIO_OPEN(CONECT, 'WRITE', 'LIST', 80, FD, STATUS)

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP('NDF2BDF_OPTER', 'Unable to open '/
     :                /'temporary connection file', STATUS)

         GO TO 996
      END IF

*   Write to the connection file.
      CALL FIO_WRITE(FD, 'IMAGE/FRAME(W)', STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('NDF2BDF_WRTER', 'Unable to write '/
     :                 /'to temporary "connection" file',
     :             STATUS)
         GO TO 996
      END IF

*   Close the connection file.
      ISTAT = SAI__OK
      CALL FIO_CLOSE(FD, ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         IF (STATUS .EQ. SAI__OK) STATUS = ISTAT
         CALL ERR_REP('NDF2BDF_CLTER', 'Unable to close'/
     :                /' temporary "connection" file',
     :                STATUS)
         GO TO 996
      END IF

*   Create logical name to point to the 'connection file'.
*   This is Vax-specific code.
      CALL SYS$CRELOG (%VAL(2), 'PROGCON', CONECT,)

*   Get the name of the BDF file.
      CALL PAR_GET0C ('OUT', BDFILE, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 996

*   Construct RUNSTAR-like command line.
      COMAND = 'NDF2BDF/IMAGE='
      NCHAR = CHR_LEN (COMAND)
      CALL CHR_UCASE (BDFILE)
      CALL CHR_APPND (BDFILE, COMAND, NCHAR)
      CALL CHR_PUTC ('/', COMAND, NCHAR)

*   Get name of the command file to hold this command line.
      CALL PAR_GET0C ('COMMAND', CMDFIL, STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 996

*   Create the temporary command file.
      CALL FIO_OPEN (CMDFIL, 'WRITE', 'LIST', 80, FD, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP('NDF2BDF_OPCER', 'Unable to open '/
     :                /'temporary "command" file', STATUS)
         GO TO 996
      END IF

*   Write the command.
      CALL FIO_WRITE(FD, COMAND, STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP('NDF2BDF_WRTER', 'Unable to write'/
     :                /' to temporary "command" file', STATUS)
      END IF

*   Close the command file.
      ISTAT = SAI__OK
      CALL FIO_CLOSE(FD, ISTAT)
      IF (ISTAT .NE. SAI__OK) THEN
         CALL ERR_REP('NDF2BDF_CLCER', 'Unable to close'/
     :                /' temporary "command" file',
     :                STATUS)
         IF (STATUS .EQ. SAI__OK) STATUS = ISTAT
      END IF

      IF (STATUS.NE.SAI__OK) GO TO 996

*   Create a logical name to point to Interim command file.
*   This is Vax-specific code.
      CALL SYS$CRELOG(%VAL(2), 'USERCOM', CMDFIL,)

*   Copy the data from the NDF to the BDF's data array.
*   ===================================================

*   Map the BDF's data array using the selected data type.
      CALL WRDATA( 'IMAGE', BDFORM, 'IMAGE', NITEM, IMAGEP, ISTAT)
      IF (ISTAT .NE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI ('WRSTAT', ISTAT)
         CALL ERR_REP ('NDF2BDF_RDERR',
     :                'WRIMAG error : ^WRSTAT', STATUS)
         GO TO 995
      END IF

*   Copy the data array to the BDF.
      CALL KPG1_COPY( HDSTYP, NITEM, POINTR, IMAGEP, STATUS )

      IF (STATUS.NE.SAI__OK) GO TO 995


*   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
*
*   Now the various descriptors are written to the BDF.
*   ===================================================

*   This is a really messy bit of code, all because INTERIM handling of
*   descriptors is so vague, undocumented, and badly designed...  The
*   problem is that when there are FITS descriptors, where the keywords
*   have to appear in a certain order.  There will be other descriptors
*   derived from the NDF, such as dimensions and axis centres, which
*   have to be merged.  Unfortunately, Interim descriptors appear in an
*   undefined order.  Experimentation reveals they appear in the order
*   they were written.  One solution might have been to write the
*   FITS descriptors and then replace them in situ when the value is
*   to be overridden by another NDF object.  This fails because Interim
*   just flags the overwritten descriptor as not being available and
*   writes the replaced value at the end of the descriptors.

*   First decide whether or not these are output to the user.
      CALL PAR_GET0L ('DESCRIP', DESCRP, STATUS)
      IF ( STATUS .NE. SAI__OK ) GO TO 995

*   Check for presence of NDF FITS extension.
      CALL NDF_XSTAT (NDF, 'FITS', FITSPR, STATUS)

*   Given no FITS extension we can just write out the few special
*   descriptors without hassle.

      IF ( .NOT. FITSPR ) THEN
         CALL CON_SPDES( NDF, 'IMAGE', DESCRP, NFLAGS, CMPFND, STATUS )


*   Deal with the FITS extension if it is present.
*   ==============================================
*
      ELSE

*      Each item is copied to a BDF descriptor except:
*         NAXIS, NAXISn - these are derived directly from the NDF data
*           array;
*         CRVALn, CDELTn, CRPIXn, CRTYPEn, CTYPEn - derived from the
*           NDF axis structures if possible.  If no linear NDF axis
*           structures are present, the values in the NDF FITS
*           extension are copied.  If any are non-linear, all FITS axis
*           information is lost.
*         TITLE, LABEL, BUNITS - the values held in NDF TITLE, LABEL,
*           and UNITS are used if present, otherwise any values found in
*           the FITS extension are used.

*      Initialise flags to indicate that these components have not
*      been found.
         AXIFND = .FALSE.
         AXLFND = .FALSE.
         AXUFND = .FALSE.
         TITFND = .FALSE.
         LABFND = .FALSE.
         UNTFND = .FALSE.

*      Initialise the flag to indicate that the mandatory NAXIS, NAXISn
*      descriptors have not been written to the BDF.
         STDDSR = .FALSE.

*      Initialise the flags that indicate a rotated axis.
         DO I = 1, NDIM
            ROTAX( I ) = .FALSE.
         END DO

*      Initialise count of FITS items copied to the BDF.
         J = 0

*      Deal with the items in the NDF FITS extension one by one.
         CALL NDF_XLOC (NDF, 'FITS', 'READ', FTLOC, STATUS)
         CALL DAT_SIZE (FTLOC, NCOMP, STATUS)

         DO I = 1, NCOMP

*         Get locator to successive elements in the FITS extension.
            CALL DAT_CELL (FTLOC, 1, I, FTLOCI, STATUS)

*         Read the FITS string, and extract the keyword and value.
            CALL DAT_GET0C (FTLOCI, FITSTR, STATUS)
            DESCR = FITSTR(1:SZDESC)
            VALUE(1:SZVAL) = FITSTR(11:SZFITS)

*         Leave out NAXIS, NAXISn, and possibly CDELTn, CRVALn,
*         CRPIXn,CRTYPEn, CTYPEn, TITLE, LABEL, and BUNITS as described
*         above.  Note CROTAn are also excluded.   Also ignore blank
*         keywords---they confuse the INTERIM descriptors.
            IF ( (INDEX(DESCR,'NAXIS') .EQ. 0) .AND.
     :           DESCR .NE. ' ' .AND.
     :           (INDEX(DESCR,'CDELT') .EQ. 0 .OR. .NOT. AXIFND) .AND.
     :           (INDEX(DESCR,'CRVAL') .EQ. 0 .OR. .NOT. AXIFND) .AND.
     :           (INDEX(DESCR,'CRPIX') .EQ. 0 .OR. .NOT. AXIFND) .AND.
     :           (INDEX(DESCR,'CRTYPE') .EQ. 0 .OR. .NOT. AXLFND) .AND.
     :           (INDEX(DESCR,'CTYPE') .EQ. 0 .OR. .NOT. AXUFND) .AND.
     :           (INDEX(DESCR,'LABEL') .EQ. 0 .OR. .NOT. LABFND) .AND.
     :           (INDEX(DESCR,'BUNITS') .EQ. 0 .OR. .NOT. UNTFND) .AND.
     :           (INDEX(DESCR,'TITLE') .EQ. 0 .OR. .NOT. TITFND) ) THEN

*            Look for a rotated axis in the FITS extension (CROTAn is
*            present and non-zero).  If there is one, the NDF AXIS
*            structure will contain pixel co-ordinates, which are
*            probably not the original co-ordinates for the axis.  If
*            there are rotated axes, the descriptors defining the axis
*            will be re-written later using the values of CRVALn and
*            CDELTn in the FITS extension.
               IF ( INDEX(DESCR,'CROTA') .NE. 0 ) THEN
                  CALL CHR_CTOI( DESCR( 6: ), ADIM, STATUS )
                  CALL CHR_CTOR( VALUE, AXROT, STATUS )
                  ROTAX( ADIM ) = ABS( AXROT ) .GT. VAL__EPSR
               END IF

*            Write descriptor to the BDF.
               CALL WRDSCR('IMAGE', DESCR, VALUE, 1, WRSTAT)
               IF (WRSTAT .NE. 0) GO TO 994

*            Count it.
               J = J + 1

*            This code ssumes that the FITS header will begin with
*            the mandatory headers in the mandatory order.  It is not
*            worth the great effort to validate the FITS descriptors
*            of a defunct format.  Thus keyword two is BITPIX.  Want
*            to write after this.
               IF ( J .EQ. 2 ) THEN

*               Insert NAXIS, AXISn, and optional keywords if the
*               appropriate special objects are present in the NDF.
                  CALL CON_SPDES( NDF, 'IMAGE', DESCRP, NFLAGS, CMPFND,
     :                            STATUS )

*               The mandatory headers have just been written.
                  STDDSR = .TRUE.

*               Use more obvious flags to indicate the certain items
*               have been written to the descriptors already.
                  AXIFND = CMPFND( 1 )
                  AXLFND = CMPFND( 2 )
                  AXUFND = CMPFND( 3 )
                  TITFND = CMPFND( 4 )
                  LABFND = CMPFND( 5 )
                  UNTFND = CMPFND( 6 )
               END IF
               IF ( STATUS .NE. SAI__OK ) GO TO 994

*            Output the descriptor's name and value to the user if
*            required.
               IF ( DESCRP ) THEN
                  CALL MSG_SETC ('DESCR', DESCR)
                  CALL MSG_SETC ('VALUE', VALUE)
                  CALL MSG_OUT (' ', '^DESCR : ^VALUE', STATUS)
               END IF

            END IF
         END DO

*      Check that the mandatory headers have been written.  They might
*      not have been created because the FITS extension only includes
*      standard FITS keywords, such as the mandatory descriptors.
         IF ( .NOT. STDDSR )
     :     CALL CON_SPDES( NDF, 'IMAGE', DESCRP, NFLAGS, CMPFND,
     :     STATUS )

*      Deal with rotated axes.
*      =======================

*      For each dimension check if there are any rotated axes.
         DO J = 1, NDIM
            IF ( ROTAX( J ) ) THEN

*            Search through the FITS headers to find the values of
*            CRVALn, CDELTn, and CRPIXn.
               DO I = 1, NCOMP

*               Get locator to successive elements in the FITS
*               extension.
                  CALL DAT_CELL (FTLOC, 1, I, FTLOCI, STATUS)

*               Read the FITS string, and extract the keyword and value.
                  CALL DAT_GET0C (FTLOCI, FITSTR, STATUS)
                  DESCR = FITSTR(1:SZDESC)
                  VALUE(1:SZVAL) = FITSTR(11:SZFITS)

*               Create the keywords being searched.
                  CALL CHR_ITOC( J, C, NCHAR )
                  CDELT = 'CDELT'//C(:1)
                  CRPIX = 'CRPIX'//C(:1)
                  CRVAL = 'CRVAL'//C(:1)

*               Test the current keyword.
                  IF ( ( INDEX(DESCR,CDELT) .EQ. 0 ) .OR.
     :                 ( INDEX(DESCR,CRPIX) .EQ. 0 ) .OR.
     :                 ( INDEX(DESCR,CRVAL) .EQ. 0 ) ) THEN

*                  Write the revised descriptor to the BDF.
                     CALL WRDSCR('IMAGE', DESCR, VALUE, 1, WRSTAT)
                     IF (WRSTAT .NE. 0) GO TO 994
                  END IF
               END DO
            END IF
         END DO
      END IF

*   Closedown.
*   ==========

*   Jump to here when there has been an error processing a descriptor.
994   CONTINUE
      IF (WRSTAT .NE. 0) THEN
         CALL MSG_SETC('DESCR', DESCR)
         STATUS = WRSTAT
         CALL ERR_REP('NDF2BDF_WRERR',
     :                'Error copying descriptor ^DESCR', STATUS)
      END IF

995   CONTINUE

*   Free the BDF data and report any Interim error arising.
      CALL FRDATA ('IMAGE', ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL MSG_SETI ('FRSTAT', ISTAT)
         STATUS = SAI__ERROR
         CALL ERR_REP ('NDF2BDF_FRERR',
     :                'FRDATA error : ^FRSTAT', STATUS)
      END IF

*   Cancel the FRAME parameter and report any Interim error.
      CALL CNPAR ('IMAGE', ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL MSG_SETI ('FRSTAT', ISTAT)
         STATUS = SAI__ERROR
         CALL ERR_REP ('NDF2BDF_FRERR', 'CNPAR error : ^FRSTAT',
     :                STATUS)
      END IF

*   Delete the temporary files. Allow for problems caused by earlier
*   errors. For example, files that do not exist.
      ISTAT = SAI__OK
      CALL FIO_ERASE (CONECT, ISTAT)
      IF (STATUS.EQ.SAI__OK) STATUS = ISTAT
      ISTAT = SAI__OK
      CALL FIO_ERASE (CMDFIL, ISTAT)
      IF (STATUS.EQ.SAI__OK) STATUS = ISTAT

996   CONTINUE

*   Close down NDF routines.
      CALL NDF_END (STATUS)

997   CONTINUE

*   Clear the INTERIM common blocks.
      CALL CON_RESCM

998   CONTINUE

*   De-activate FIO.
      CALL FIO_DEACT (STATUS)

999   CONTINUE
      END
