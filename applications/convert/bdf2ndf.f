      SUBROUTINE BDF2NDF( STATUS )
*+
*  Name:
*     BDF2NDF

*  Purpose:
*     Converts a Starlink Interim BDF file to an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL BDF2NDF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts data files from the Bulk Data Frame
*     (BDF) format used by the INTERIM Environment (see SUN/4) to the
*     Starlink standard NDF format (see SUN/33 or SGP/38).  Type
*     conversion may be performed at the same time.

*  Usage:
*     BDF2NDF IN OUT [TYPE] [DESCRIP]

*  ADAM Parameters:
*     IN = BDF (Read)
*        The BDF to be converted to an NDF.  A file extension must not
*        be given since ".BDF" is assumed.
*     OUT = NDF (Write)
*        The name of NDF converted from the BDF.  It becomes the new
*        current NDF.
*     TYPE = LITERAL (Read)
*        The data type of the output NDF.  It must be one of the
*        following HDS types: "_BYTE", "_WORD", "_REAL", "_INTEGER",
*        "_DOUBLE", "_UBYTE", "_UWORD" corresponding to signed byte,
*        signed word, real, integer, double precision, unsigned byte,
*        and unsigned word.  See SUN/92 for further details.  An
*        unambiguous abbreviation may be given. ["_REAL"]
*     DESCRIP = _LOGICAL (Read)
*        If true the descriptors in the BDF are reported as they are
*        copied to the FITS extension within the output NDF. [FALSE]
*     CONNECT = FILENAME (Write)
*        The Interim connection file.  It is deleted when the
*        application terminates.  [NDF2BDF.TMP]
*     COMMAND = FILENAME (Write)
*        The Interim command file.  It is deleted when the application
*        terminates.  [USERCOM.TMP]

*  Examples:
*     BDF2NDF OLD NEW
*        This converts the BDF called OLD (in file OLD.BDF) to the NDF
*        called NEW (in file NEW.SDF).  NEW's data array will have
*        the _REAL data type.  Descriptors are copied to the FITS
*        extension but are not reported.
*     BDF2NDF OLD NEW DESCRIP
*        This converts the BDF called OLD (in file OLD.BDF) to the NDF
*        called NEW (in file NEW.SDF).  NEW's data array will have
*        the _REAL data type.  Descriptors are copied to the FITS
*        extension and are reported.
*     BDF2NDF HORSE HORSE TYPE="_WORD"
*        This converts the BDF called HORSE (in file HORSE.BDF) to the
*        NDF also called HORSE (in file HORSE.SDF).  The NDF's data
*        array will contain signed 2-byte integers.  Descriptors are
*        copied to the FITS extension but are not reported.

*  Notes:
*     -  The conversion rules can be summarised as follow:
*        o  the BDF data array is copied to the NDF main data array;
*        by default the output data array has the data type _REAL.
*        o  The BLANK descriptor is not used to flag pixels in the NDF
*        with the bad value.  Use KAPPA's SETMAGIC to flag another
*        value.
*        o  The BDF descriptors are written to the NDF FITS extension.
*        Long values may be truncated, 65 for characters and 20 for
*        numbers.  The formating adheres to the FITS standard.
*        Descriptors already in the FITS format are copied as is, so
*        La Palma ING-format headers can be propagated.
*        o  If the BDF descriptors contain the FITS keywords CRVALn,
*        CDELTn, the appropriate axis structures are generated in
*        the output NDF. In addition should CRTYPEn also be present
*        the labels are added to these structures.
*        o  If the BDF descriptors contain the FITS keywords TITLE or
*        LABEL or BUNITS, the associated character string value is
*        written to the NDF TITLE, LABEL or UNITS component as
*        appropriate.
*        o  HISTORY descriptors are not used to make an HISTORY
*        component in the NDF.
*     -  WARNING: the BDF size may grow when TYPE is specified due to
*     an incarnation being created.  See below and SUN/4 for more
*     details.

*  Incarnations:
*     A BDF contains one or more `incarnations' of the data array.  An
*     incarnation of a data array is simply a copy of that data array
*     stored with a particular data type.  For example, a BDF may
*     contain incarnations of the same data array stored as a REAL
*     array, and an INTEGER array.  This rather strange behaviour is a
*     consequence of the way the INTERIM environment deals with data
*     access.  For example, if a application attempts to map a BDF data
*     array of INTEGER type as a REAL array, type conversion must take
*     place.  Instead of doing this in virtual memory, a second
*     incarnation of the data, this time of type REAL is created and
*     stored in the original file (causing perhaps a doubling of the
*     file size).  This second incarnation of the data array is stored
*     to avoid performing the same type conversion in the future.  (It
*     is perhaps significant that no such scheme has been employed with
*     subsequent data systems.)  One consequence of this behaviour is
*     that a BDF data array may not have a unique type.  BDF2NDF will
*     use the first type it finds as the default.  Specify the
*     TYPE parameter explicitly to ensure that the correct data type is
*     created within the NDF.
*
*     To prevent the BDF growing, just remove write access from the
*     file.

*  Algorithm:
*     -  Activate FIO.
*     -  Get the name of the input BDF. Get and validate the HDS
*     type. Convert it to an Interim format code for the input BDF.
*     -  Create a old-style Starlink 'connection file' for an
*     application BDF2NDF. Create a RUNSTAR-type command line in another
*     file. Assign the logical names PROGCON and USERCOM to these files.
*     This will fool the old-style Starlink Interface Routines into
*     working correctly.  Map in VM the data array using the old-style
*     routine RDIMAG.
*     -  Create the output NDF with a primitive NDF and map the main
*     data array with write access.
*     -  Copy the BDF's data array to the NDF.
*     -  Create TITLE, LABEL, and UNITS components in the NDF when the
*     relevant descriptors are present in the BDF.
*     -  Create AXIS structures in the NDF when the FITS axis keywords
*     are present in the BDF.
*     -  Copy any other descriptors from the BDF to the NDF FITS
*     extension.
*     -  Unmap arrays, close down the NDF system and deactivate FIO.
*     -  Reset INTERIM environment in case BDF2NDF is re-run without re-
*     loading.

*  Related Applications:
*     CONVERT: NDF2BDF.

*  Implementation Status:
*     -  Note the data array in the NDF is of the primitive form.
*     -  Only available on VMS platforms.

*  Copyright:
*     Copyright (C) 1989, 1991-1993 Science & Engineering Research
*     Council. Copyright (C) 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     1989 August 16th (JM):
*        Adapted from STARIN. This version outputs an NDF rather than
*        an IMAGE file. The BDF descriptors are copied to .MORE.FITS.
*     1991 February 5th (JM):
*        Program changed to use NDF_ routines.  If BDF descriptors
*        contain sufficient information, i.e. values for CRVALn,
*        CDELTn, the appropriate axis structures are generated in the
*        output NDF.  If the BDF descriptors contain values for the
*        data files TITLE and LABEL, these are copied to the NDF TITLE
*        and LABEL object respectively.
*     1992 January 30 (MJC):
*        Changed FORMAT parameter to TYPE for consistency with the
*        paper documentation (SUN/55).  Also gave TYPE a dynamic
*        default of the type of the BDF.  Added the ADAM parameters
*        section to the prologue.  Renamed the parameters for the NDF
*        and BDF to IN and OUT respectively to agree with SUN/55 and
*        consistency.  Added examples.  Tidied the prologue.  Processes
*        UNITS and AXIS.LABEL components of the NDF.  Corrected the
*        evaluation of the axis centres to allow for a pixel offset
*        (as defined in the FITS standard) in the descriptors.  Ensured
*        that an NDF axis structure will be valid by making a complete
*        set of axis-centre arrays.
*     1992 July 17 (MJC):
*        Removed loop when obtaining the type as PAR routine validates
*        the data types.
*     1993 January 27 (MJC):
*        Corrected the description of the default value of parameter
*        TYPE, and hence modified the examples.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     -  May give spurious error messages if running under ICL.  This
*     happens if a non-existent BDF is given as the input file.  A
*     subsequent invocation of the application may result in a repeat of
*     the error message although the conversion is carried out
*     correctly.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                  ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'              ! Standard ADAM constants
      INCLUDE 'PAR_ERR'              ! Parameter-system errors
      INCLUDE 'FIO_PAR'              ! FIO_ constants
      INCLUDE 'CNF_PAR'              ! For CNF_PVAL function

*  Status:
      INTEGER STATUS                 ! Global status

*  External References:
      INTEGER CHR_LEN                ! Get effective length of string

*  Local Constants:
      INTEGER   MAXDIM
      PARAMETER (MAXDIM=7)           ! Maximum number of dimensions

*  Local Variables:
      LOGICAL   AXTHER(DAT__MXDIM)   ! Indicates if an axis (along a
                                     ! dimension) can be created
      CHARACTER BDFILE*(FIO__SZFNM)  ! BDF filename
      INTEGER   BDFORM               ! Format in BDF style
      REAL      CDELT(DAT__MXDIM)    ! Accommodates CDELTn values
      INTEGER   CLEN                 ! Length of command line
      CHARACTER CMDFIL*(FIO__SZFNM)  ! Command filename
      CHARACTER CONECT*(FIO__SZFNM)  ! Connection filename
      LOGICAL   CRAXIS               ! Create axis structure if true
      REAL      CRPIX(DAT__MXDIM)    ! Accommodates CRPIXLn values
      CHARACTER*(70) CRTYPE(DAT__MXDIM) ! Accommodates CRTYPEn values
      REAL      CRVAL(DAT__MXDIM)    ! Accommodates CRVALn values
      CHARACTER COMAND*(80)          ! RUNSTAR-type command line
      INTEGER   DATBDF               ! Pointer to BDF data
      INTEGER   DATNDF               ! Pointer to NDF data
      INTEGER   DIM(DAT__MXDIM)      ! IMAGE dimensions (axis lengths)
      INTEGER   FD                   ! File descriptor for temporary
                                     ! files
      CHARACTER FITLOC*(DAT__SZLOC)  ! Locator to NDF FITS structure
      CHARACTER FORMAT*(DAT__SZTYP)  ! Data format required (HDS)
      INTEGER   I                    ! Loop variable
      CHARACTER IFORM*(2)            ! Data format required (Interim)
      INTEGER   ISTAT                ! Local status return
      CHARACTER LABEL*(72)           ! NDF LABEL
      INTEGER   LBND(DAT__MXDIM)     ! IMAGE dimensions (axis lengths)
      INTEGER   NBPI                 ! Number of bytes per item
      INTEGER   NBYTES               ! Number of bytes to be moved
      INTEGER   NDF                  ! NDF identifier
      INTEGER   NDIM                 ! Number of dimensions
      INTEGER   NELM                 ! Number of elements
      REAL      OFFSET               ! Value of the first element of an
                                     ! axis centre array
      CHARACTER TITLE*(72)           ! NDF Title
      INTEGER   UBND(DAT__MXDIM)     ! IMAGE dimensions (axis lengths)
      CHARACTER UNITS*(72)           ! NDF Units
      INTEGER   WPTR                 ! Pointer to NDF axis data
      LOGICAL   WRDSCR               ! True if descriptors to be
                                     ! reported

*.

*   Check the inherited status.
      IF (STATUS .NE. SAI__OK) RETURN

*   Activate FIO package.
      CALL FIO_ACTIV (STATUS)

*   Get name of BDF file.
      CALL PAR_GET0C ('IN', BDFILE, STATUS)
      IF (STATUS .NE. SAI__OK) GO TO 999

*   Get type of data required, selected from the menu of HDS numeric
*   types.
      CALL PAR_CHOIC( 'TYPE', '_REAL', '_BYTE,_DOUBLE,_INTEGER,_REAL,'/
     :               /'_UBYTE,_UWORD,_WORD', .FALSE., FORMAT, STATUS )

*   Convert format from HDS to Interim old-style character value.
      CALL CON_H2ITY( FORMAT, IFORM, STATUS )

*   Convert the Interim character data type to its integer form,
*   and obtain the number of bytes per value for the data type.
      CALL CON_INTCL( IFORM, BDFORM, NBPI, STATUS )

*   Create Interim-like files and point logicals to the them.
*   =========================================================
*
*   Here we must simulate an Interim environment application running.
*   Two files are created: the connection file (equivalent to a
*   modern interface file), and a command file.  Logical names used
*   by Interim's RUNSTAR must also be defined.

*   Get name of the `connection file' and create it.
      CALL PAR_GET0C ('CONNECT', CONECT, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*      Open the connection file.
         CALL FIO_OPEN (CONECT, 'WRITE','LIST', 80, FD, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('BDF2NDF_OPTER', 'Unable to open temporary'/
     :                /' connection file', STATUS)

         ELSE

*         Write to the connection file.
            CALL FIO_WRITE (FD, 'IMAGE/FRAME(R)', STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP('BDF2NDF_WRTER', 'Unable to write to'/
     :                /' temporary connection file', STATUS)

            END IF

*         Close the connection file.
            ISTAT = SAI__OK
            CALL FIO_CLOSE(FD, ISTAT)
            IF (ISTAT .NE. SAI__OK) THEN
               CALL ERR_REP ('BDF2NDF_CLTER',
     :                       'Unable to close temporary connection '//
     :                       'file', STATUS)
               IF (STATUS .EQ. SAI__OK) STATUS = ISTAT
            END IF
         END IF

         IF (STATUS .EQ. SAI__OK) THEN

*         Create logical name to point to the 'connection file'.
*         This is Vax-specific code.
            CALL SYS$CRELOG (%VAL(2), 'PROGCON', CONECT,)

*         Construct RUNSTAR-like command line.
            COMAND = 'BDF2NDF/IMAGE='
            CLEN = CHR_LEN(COMAND)
            CALL CHR_UCASE(BDFILE)
            CALL CHR_APPND(BDFILE, COMAND, CLEN)
            CALL CHR_PUTC('/', COMAND, CLEN)

*         Get name of the command file to hold this command line.
            CALL PAR_GET0C ('COMMAND', CMDFIL, STATUS)

*         Create the temporary command file.
            CALL FIO_OPEN (CMDFIL, 'WRITE','LIST', 80, FD, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP ('BDF2NDF_OPCER',
     :                       'Unable to open temporary command file',
     :                        STATUS)

            ELSE

*            Write the command.
               CALL FIO_WRITE (FD, COMAND, STATUS)
               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_REP ('BDF2NDF_WRTER', 'Unable to write to'/
     :                /' temporary "command" file', STATUS)
               END IF

*            Close the command file.
               ISTAT = SAI__OK
               CALL FIO_CLOSE (FD, ISTAT)
               IF (ISTAT .NE. SAI__OK) THEN
                  CALL ERR_REP ('BDF2NDF_CLCER', 'Unable to close'/
     :                /' temporary "command" file', STATUS)
                  IF (STATUS .EQ. SAI__OK) STATUS = ISTAT
               END IF
            END IF
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN

*      Create logical name to point to this file
*      This is Vax-specific code.
         CALL SYS$CRELOG (%VAL(2), 'USERCOM', CMDFIL,)

*      Copy the data from the BDF to the NDF's data array.
*      ===================================================

*      Map in BDF file using the selected data type.
         ISTAT = SAI__OK
         CALL RDIMAG ('IMAGE', BDFORM, MAXDIM, DIM, NDIM, DATBDF, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL MSG_SETI ('RDSTAT', ISTAT)
            STATUS = SAI__ERROR
            CALL ERR_REP ('BDF2NDF_RDERR', 'RDIMAG ERROR : ^RDSTAT',
     :                 STATUS)
         ELSE

*         Begin an NDF context.
            CALL NDF_BEGIN

*         Set up appropriate upper and lower bounds for output NDF.
            DO I=1, NDIM
               LBND(I) = 1
               UBND(I) = DIM(I)
            END DO

*         Create he output NDF.  A primitive NDF is created for the
*         time being. When all KAPPA  applications can support simple
*         NDFs, the NDF_CREAT call will be reinstated.
*            CALL NDF_CREAT ('OUT', FORMAT, NDIM, LBND, UBND, NDF,
*     :                       STATUS)
            CALL NDF_CREP ('OUT', FORMAT, NDIM, DIM, NDF, STATUS)

*         Map NDF data array.
            CALL NDF_MAP (NDF, 'DATA', FORMAT, 'WRITE', DATNDF, NELM,
     :                    STATUS)

*         Copy the data array to the NDF.
            IF (STATUS .EQ. SAI__OK) THEN
               NBYTES = NELM * NBPI
               CALL CON_MOVE (NBYTES, %VAL(CNF_PVAL(DATBDF)),
     :                        %VAL(CNF_PVAL(DATNDF)),
     :                        STATUS)
            END IF

*         Process the descriptors.
*         ========================

*         Output the descriptor values if required.
            CALL PAR_GET0L( 'DESCRIP', WRDSCR, STATUS )

*         Always copy descriptors (differs from STARIN), except
*         for a few special ones that are returned.

            IF (STATUS .EQ. SAI__OK) THEN
               CALL CON_DESCR ('IMAGE', NDF, NDIM, WRDSCR, FITLOC,
     :                          CRVAL, CDELT, CRPIX, CRTYPE, AXTHER,
     :                          TITLE, LABEL, UNITS, STATUS)
            END IF

*         If a TITLE was found among the BDF descriptors insert the
*         associated value into the NDF TITLE.
            IF (CHR_LEN(TITLE) .GT. 0) THEN
               CALL NDF_CPUT (TITLE, NDF, 'TITLE', STATUS)
            END IF

*         If a LABEL was found among the BDF descriptors insert the
*         associated value into the NDF LABEL.
            IF (CHR_LEN(LABEL) .GT. 0) THEN
               CALL NDF_CPUT (LABEL, NDF, 'LABEL', STATUS)
            END IF

*         If a BUNITS was found among the BDF descriptors insert the
*         associated value into the NDF UNITS.
            IF (CHR_LEN(UNITS) .GT. 0) THEN
               CALL NDF_CPUT (UNITS, NDF, 'UNITS', STATUS)
            END IF

*         See if any axes structures can be created.
            CRAXIS = .FALSE.
            DO I = 1, NDIM
               CRAXIS = CRAXIS .OR. AXTHER( I )
            END DO

*         If they can fill all the axis centres with the evaluated zero
*         point and scale from the origin defined by CRPIXn, using
*         defaults if these are not present in the descriptors for a
*         given dimension.  (This is done by CON_DESCR.) The axis
*         centres must be filled before processing any other axis
*         component.
            IF ( CRAXIS ) THEN
               DO I = 1, NDIM
                  CALL NDF_AMAP (NDF, 'CENTRE', I, '_REAL', 'WRITE',
     :                           WPTR, NELM, STATUS)

*               Compute the actual start value of the axis allowing for
*               the displacement between it and the reference pixel.
                  OFFSET = CRVAL( I ) - ( CRPIX( I ) - 1.0 ) *
     :                     CDELT( I )
                  CALL CON_FILL (NELM, OFFSET, CDELT( I ),
     :                           %VAL(CNF_PVAL(WPTR)), STATUS)
               END DO

               DO I = 1, NDIM

*               If a CRTYPEn was found among the BDF descriptors insert
*               the associated value into the axis LABEL.
                  IF (CHR_LEN(CRTYPE(I)) .GT. 0) THEN
                     CALL NDF_ACPUT (LABEL, NDF, 'LABEL', I, STATUS)
                  END IF

               END DO
            END IF

*         Free the BDF data.
            CALL FRDATA ('IMAGE', ISTAT)
            IF (ISTAT .NE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('FRSTAT', ISTAT)
               CALL ERR_REP ('BDF2NDF_FRERR', 'FRDATA error : ^FRSTAT',
     :                   STATUS)
            END IF

*         Cancel the FRAME parameter.
            CALL CNPAR ('IMAGE', ISTAT)
            IF (ISTAT .NE. 0) THEN
               CALL MSG_SETI ('FRSTAT', ISTAT)
               STATUS = SAI__ERROR
               CALL ERR_REP ('BDF2NDF_FRERR', 'CNPAR error : ^FRSTAT',
     :                   STATUS)
            END IF
         END IF

*      End the NDF context.
         CALL NDF_END (STATUS)
      END IF

*   Delete the temporary files. Allow for problems caused by earlier
*   errors. E.g. files do not exist.
      ISTAT = SAI__OK
      CALL FIO_ERASE (CONECT, ISTAT)
      IF (STATUS.EQ.SAI__OK) STATUS = ISTAT
      ISTAT = SAI__OK
      CALL FIO_ERASE (CMDFIL, ISTAT)
      IF (STATUS.EQ.SAI__OK) STATUS = ISTAT

999   CONTINUE

*   De-activate FIO.
      CALL FIO_DEACT (STATUS)

*   Clear the INTERIM common blocks.
      CALL CON_RESCM

      END
