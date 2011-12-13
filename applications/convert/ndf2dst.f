      SUBROUTINE NDF2DST( STATUS )
*+
*  Name:
*     NDF2DST

*  Purpose:
*     Converts an NDF to a Figaro (Version 2) DST file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NDF2DST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Description:
*     This application converts an NDF to a Figaro (Version 2) `DST'
*     file.  The rules for converting the various components of a DST
*     are listed in the Notes.  Since both are hierarchical formats
*     most files can be be converted with little or no information
*     lost.

*  Usage:
*     ndf2dst in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF data structure.  The suggested default is the
*        current NDF if one exists, otherwise it is the current value.
*     OUT = Figaro (Write)
*        Output Figaro file name.  This excludes the file extension.
*        The file created will be given extension ".dst".

*  Examples:
*     ndf2dst old new
*        This converts the NDF called old (in file old.sdf) to the
*        Figaro file new.dst.
*     ndf2dst spectre spectre
*        This converts the NDF called spectre (in file spectre.sdf) to
*        the Figaro file spectre.dst.

*  Notes:
*     The rules for the conversion are as follows:
*     _________________________________________________________________
*          NDF                   Figaro file
*     -----------------------------------------------------------------
*          Main data array     -> .Z.DATA
*          Imaginary array     -> .Z.IMAGINARY
*          Bad-pixel flag      -> .Z.FLAGGED
*          Units               -> .Z.UNITS
*          Label               -> .Z.LABEL
*          Variance            -> .Z.ERRORS (after processing)
*          Quality             -> It is not copied directly though bad
*                                 values indicated by QUALITY flags will
*                                 be flagged in .Z.DATA in addition to
*                                 any flagged values actually in the
*                                 input main data array.  .Z.FLAGGED is
*                                 set accordingly.
*          Title               -> .OBS.OBJECT
*
*          AXIS(1) structure   -> .X
*             AXIS(1) Data     -> .X.DATA  (unless there is a DATA_ARRAY
*                                 component of AXIS(1).MORE.FIGARO to
*                                 allow for a non-1-dimensional array)
*             AXIS(1) Variance -> .X.VARIANCE  (unless there is a
*                                 VARIANCE component of
*                                 AXIS(1).MORE.FIGARO to allow for a
*                                 non-1-dimensional array)
*             AXIS(1) Width    -> .X.WIDTH  (unless there is a WIDTH
*                                 component of AXIS(1).MORE.FIGARO to
*                                 allow for a non-1-dimensional array)
*             AXIS(1) Units    -> .X.UNITS
*             AXIS(1) Label    -> .X.LABEL
*             AXIS(1).MORE.FIGARO.xxx -> .X.xxx
*          (Similarly for AXIS(2), ..., AXIS(6) which are renamed to
*          .Y .T .U .V or .W)
*
*          FIGARO extension:
*          .MORE.FIGARO.MAGFLAG -> .Z.MAGFLAG
*          .MORE.FIGARO.RANGE -> .Z.RANGE
*          .MORE.FIGARO.SECZ  -> .OBS.SECZ
*          .MORE.FIGARO.TIME  -> .OBS.TIME
*          .MORE.FIGARO.xxx    -> .xxx   (recursively)
*
*          FITS extension:
*          .MORE.FITS
*                   Items      -> .FITS.xxx
*                   Comments   -> .COMMENTS.xxx
*
*          Other extensions:
*          .MORE.other         -> .MORE.other

*  Related Applications:
*     CONVERT: DST2NDF.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     Copyright (C) 1995-1997, 2004 Central Laboratory of the Research
*     Councils.
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
*     1991 February 1 (JM):
*        Original version.
*     1991 March 22 (JM):
*        The NDF quality array is no longer copied to the output Figaro
*        file. This is because Figaro does not supports the use of both
*        flagged values and a quality array in the same data structure.
*        Extensions in the NDF are copied from NDF.MORE to Figaro.MORE.
*     1991 September 25 (MJC):
*        Corrected looping bug, when an extension is empty.
*     1992 January 30 (MJC):
*        Added further comments to the code.  Modified to preserve the
*        type of extensions in the FIGARO.MORE structure.  Added route
*        for other extensions to the prologue.  Made the file parameters
*        IN and OUT, as given in SUN/55.  Corrected the processing of
*        character-valued card-images in the FITS extension.  Searches
*        for the comment delimiter rather than assuming where the
*        comment is located.  Reordered the annulling of the comments
*        locator to prevent an error.
*     1992 June 2 (MJC):
*        Checks for a blank FITS keyword: if found the card image is
*        omitted from the DST.
*     1992 September 2 (MJC):
*        Write FLAGGED value to DST as Figaro (DSA_) makes the opposite
*        assumption about the presence of bad pixels compared with
*        NDF_.
*     1992 September 9 (MJC):
*        Propagated axis variance and width, and the contents of any
*        axis Figaro extension.  Fixed bug whereby if there was only
*        one object in the Figaro extension, it was not copied to the
*        DST.
*     1992 September 10 (MJC):
*        Moved special cases of .OBS.SECZ, .OBS.TIME, .Z.MAGFLAG,
*        .Z.RANGE from the top-level Figaro extension as this is where
*        DSA_ now expects to find them in an NDF.
*     1995 July 25 (MJC):
*        Fixed bug when n-dimensional axis is present: it was getting
*        pixel co-ordinate DATA component and a DATA_ARRAY component
*        for the n-dimensional array.  Made the latter the DATA
*        component.
*     1996 February 7 (MJC):
*        Checked whether or not AXIS().MORE.FIGARO.DATA_ARRAY is
*        primitive.  If it is, its DATA component becomes the new
*        n-D axis array, rather than renaming DATA_ARRAY to DATA, in
*        the Figaro file.
*     1996 February 9 (MJC):
*        Checked whether or not AXIS().MORE.FIGARO.WIDTH is primitive.
*        If it is, its DATA component becomes the new n-D axis-width
*        array in the Figaro file.
*     1996 July 17 (MJC):
*        Transfers the DATA_ARRAY.IMAGINARY_DATA component to
*        .Z.IMAGINARY
*     1997 April 24 (MJC):
*        Fixed bug that could result in .OBS and .Z structures of the
*        FIGARO extension being lost.  This occurred when FIGARO
*        extension components SECZ or TIME (for OBS), and MAGFLAG or
*        RANGE (for Z) are present and are physically stored following
*        their respective structure.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL.
*     2009 June 29 (MJC):
*        Replaced deprecated CON_MOVE (and CON_TYPSZ) with KPG1_COPY
*        from KAPLIBS.
*     {enter_further_changes_here}

*-
*  Type definitions:
      IMPLICIT  NONE                ! No implicit typing

*  Global constants:
      INCLUDE  'SAE_PAR'            ! Standard SAE constants
      INCLUDE  'DAT_PAR'            ! Data-system constants
      INCLUDE  'FIO_PAR'            ! FIO constants
      INCLUDE  'NDF_PAR'            ! NDF symbolic constants
      INCLUDE  'CNF_PAR'            ! For CNF_PVAL function

*  External References:
      INTEGER  CHR_LEN              ! Get effective length of string

*  Status:
      INTEGER  STATUS               ! Global status

*  Local Constants:
      INTEGER                MAXCOM ! Maximum number of comment images
      PARAMETER( MAXCOM = 200 )
      INTEGER                MAXHIS ! Maximum number of history images
      PARAMETER( MAXHIS = 100 )
      INTEGER                MINFCV ! Minimum number of characters in a
                                    ! FITS character value
      PARAMETER( MINFCV = 8 )

*  Local Variables:
      CHARACTER*1 AXNAME(6)         ! Axis names
      INTEGER                AXPTR  ! Pointer to NDF axis data
      LOGICAL                AXDATA ! True if axis data exist
      LOGICAL                AXVAR  ! True if axis-variance array exists
      LOGICAL                AXWIDT ! True if axis-width array exists
      LOGICAL                BAD    ! True if bad values recognised
      INTEGER                BADBIT ! Value sof the NDF bad-bits
      LOGICAL                BADPIX ! True if bad values may be present
                                    ! in the main data array
      INTEGER                CLEN   ! String length
      LOGICAL                CMPLEX ! True if complex data array present
      INTEGER                CSTAT  ! Status value in CHR calls
      INTEGER                DIM(NDF__MXDIM)  ! Dimensions
      CHARACTER*(DAT__SZTYP) EXTTYP ! Extension type
      LOGICAL                EXIST  ! True if  a component is there
      INTEGER                FAXPTR ! Pointers to Figaro axis data
      INTEGER                FDPTR  ! Pointer to Figaro main data array

      INTEGER                FEPTR  ! Pointer to Figaro error array
      CHARACTER*(FIO__SZFNM) FIGEXT ! Figaro extension
      CHARACTER*(FIO__SZFNM) FIGFIL ! Figaro filename
      LOGICAL                FIGMOR ! True if Figaro .MORE created
      LOGICAL                FIGOBS ! True if Figaro .OBS created
      INTEGER                FIPTR  ! Pointer to Figaro imaginary array      LOGICAL                FLPRES ! Value of the .Z.FLAGGED component
                                    ! in the DST
      CHARACTER*48           FITCOM ! FITS comment
      CHARACTER*70           FITDAT ! FITS value
      CHARACTER*8            FITNAM ! FITS keyword
      LOGICAL                FLPRES ! Value of the .Z.FLAGGED component
                                    ! in the DST
      CHARACTER*(NDF__SZFTP) FTYPE  ! Full data type
      LOGICAL                HUSED  ! FITS header already exists if true
      INTEGER                I      ! Loop variable
      INTEGER                IAXIS  ! Axis number
      INTEGER                IERR   ! First numerical error
      INTEGER                IVAL   ! Holds integer value
      CHARACTER*80           LABEL  ! Label
      INTEGER                LEN    ! Length of character value
      CHARACTER*(DAT__SZLOC) LF     ! Figaro output structure locator
      CHARACTER*(DAT__SZLOC) LFAX   ! A Figaro axis locator
      CHARACTER*(DAT__SZLOC) LFAXD  ! Figaro AXIS DATA locator
      CHARACTER*(DAT__SZLOC) LFAXDA ! Figaro AXIS DATA_ARRAY locator
      CHARACTER*(DAT__SZLOC) LFAXL  ! Figaro AXIS LABEL  locator
      CHARACTER*(DAT__SZLOC) LFAXU  ! Figaro AXIS UNITS locator
      CHARACTER*(DAT__SZLOC) LFAXV  ! Figaro AXIS VARIANCE locator
      CHARACTER*(DAT__SZLOC) LFAXW  ! Figaro AXIS WIDTH locator
      CHARACTER*(DAT__SZLOC) LFAXWD ! Figaro AXIS WIDTH DATA locator
      CHARACTER*(DAT__SZLOC) LFCOM  ! Figaro .COMMENTS locator
      CHARACTER*(DAT__SZLOC) LFFT   ! Figaro .FITS locator
      CHARACTER*(DAT__SZLOC) LFFTC  ! Figaro .FITS.xxx.DESCRIPTION locator
      CHARACTER*(DAT__SZLOC) LFFTD  ! Figaro .FITS.xxx.DATA locator
      CHARACTER*(DAT__SZLOC) LFMFI  ! Figaro FITS item locator
      CHARACTER*(DAT__SZLOC) LFMOR  ! Figaro .MORE structure locator
      CHARACTER*(DAT__SZLOC) LFMORI ! Figaro .MORE.xxx structure locator
      CHARACTER*(DAT__SZLOC) LFO    ! Figaro .OBS locator
      CHARACTER*(DAT__SZLOC) LFOO   ! Figaro .OBS.OBJECT locator
      CHARACTER*(DAT__SZLOC) LFZ    ! Figaro '.Z' locator
      CHARACTER*(DAT__SZLOC) LFZD   ! Figaro '.Z.DATA' locator
      CHARACTER*(DAT__SZLOC) LFZE   ! Figaro '.Z.ERRORS' locator
      CHARACTER*(DAT__SZLOC) LFZI   ! Figaro '.Z.IMAGINARY' locator
      CHARACTER*(DAT__SZLOC) LFZL   ! Figaro '.Z.LABEL' locator
      CHARACTER*(DAT__SZLOC) LFZU   ! Figaro '.Z.UNITS' locator
      CHARACTER*(DAT__SZLOC) LOCA   ! NDF axis-structure locator
      CHARACTER*(DAT__SZLOC) LOCAI  ! NDF axis element locator
      CHARACTER*(DAT__SZLOC) LOCF   ! NDF axis Figaro-extension locator
      CHARACTER*(DAT__SZLOC) LOCM   ! NDF axis extension locator
      CHARACTER*(DAT__SZLOC) LOCN   ! NDF top-level locator
      LOGICAL                LVAL   ! Value of a logical FITS item
      CHARACTER*20           MFNAM  ! Name of item in Figaro extension
      INTEGER                NAXELM ! No elements in axis data structure
      INTEGER                NCOMP  ! Number of components
      INTEGER                NCC    ! Column of FITS comment delimiter
      INTEGER                NCCOM  ! Number of characters in FITS
                                    ! COMMENT or HISTORY card
      INTEGER                NCCQ   ! Column of FITS trailing quote for
                                    ! character value
      INTEGER                NCDQ   ! Column of FITS double quote for
                                    ! character value
      INTEGER                NCFD   ! Column position in output FITS
                                    ! character value
      INTEGER                NCOMS  ! Column from where to start search
                                    ! for FITS comment delimiter
      INTEGER                NCSTQ  ! Column from where to start search
                                    ! for a trailing quote for a FITS
                                    ! character value
      INTEGER                NDF    ! NDF identifier
      INTEGER                NDIM   ! Number of dimensions
      INTEGER                NDPTR  ! Pointer to NDF main data array
      INTEGER                NELM   ! Number of elements
      INTEGER                NERR   ! Number of numerical errors
      INTEGER                NEXT   ! Counter for NDF extensions
      INTEGER                NIPTR  ! Pointer to NDF imaginary data
                                    ! array
      INTEGER                NVPTR  ! Pointer to NDF variance array
      CHARACTER*(DAT__SZLOC) NXFIG  ! NDF FIGARO extension locator
      CHARACTER*(DAT__SZLOC) NXFIGI ! NDF FIGARO.something locator
      CHARACTER*(DAT__SZLOC) NXLOC  ! NDF extension locator
      CHARACTER*(DAT__SZLOC) NXLOCI ! NDF extension.something locator
      CHARACTER*(DAT__SZLOC) NXFIT  ! NDF FITS extension locator
      LOGICAL                QUPRES ! True if QUALITY is present in the
                                    ! NDF
      LOGICAL                PRIM   ! True if component is primitive
      REAL                   RVAL   ! Holds real value
      CHARACTER*80           STRING ! FITS string
      CHARACTER*80           TITLE  ! Title
      CHARACTER*(NDF__SZTYP) TYPE   ! Data type
      CHARACTER*80           UNITS  ! Units
      CHARACTER*80           XNAME  ! Extension name
      LOGICAL                VALDEF ! FITS value defined if true

*  Local Data:
      DATA AXNAME/'X','Y','T','U','V','W'/

*.
*   Check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain the input NDF.
*   =====================

*   Begin an NDF context.
      CALL NDF_BEGIN

*   Get the name of the input NDF and associate an identifier with it.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )

*   If no error then continue
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*   Create the Figaro file.
*   =======================
*
*   Obtain the name of the output Figaro file, and append the standard
*   file extension.
      CALL PAR_GET0C ('OUT', FIGFIL, STATUS )
      FIGEXT  =  '.dst'
      CLEN  =  CHR_LEN( FIGFIL )
      CALL CHR_APPND( FIGEXT, FIGFIL, CLEN )

*   Create the new Figaro structure and associate a locator with it.
      CALL HDS_NEW( FIGFIL, 'OUT', 'FIGARO', 0, 0, LF, STATUS )

*   Deal with the Figaro extension (if any).
*   ========================================
*
*   NDF.MORE.FIGARO.xxx -> OUTPUT.xxx
      CALL NDF_XSTAT( NDF, 'FIGARO', EXIST, STATUS )
      IF ( EXIST ) THEN

*      Obtain a locator to the Figaro extension.
         CALL NDF_XLOC( NDF, 'FIGARO', 'READ', NXFIG, STATUS )

*      Find the number of components it contains.
         CALL DAT_NCOMP( NXFIG, NCOMP, STATUS )

*      Look out for an empty extension.
         IF ( NCOMP .GE. 1 .AND. STATUS .EQ. SAI__OK ) THEN

*         Some special Figaro-extension components---SECZ, TIME,
*         MAGFLAG, RANGE---are written to structures (OBS and Z).  If
*         these structures also exist in the NDF they must be copied to
*         the DST before inserting the special values.
*
*         Deal with the OBS structure first.
            CALL DAT_THERE( NXFIG, 'OBS', EXIST, STATUS )
            IF ( EXIST ) THEN

*            Copy the OBS component to the Figaro structure.
               CALL DAT_FIND( NXFIG, 'OBS', NXFIGI, STATUS )
               CALL DAT_COPY( NXFIGI, LF, 'OBS', STATUS )
               CALL DAT_ANNUL( NXFIGI, STATUS )
            END IF

*         Now copy any Z structure.
            CALL DAT_THERE( NXFIG, 'Z', EXIST, STATUS )
            IF ( EXIST ) THEN

*            Copy the Z component to the Figaro structure.
               CALL DAT_FIND( NXFIG, 'Z', NXFIGI, STATUS )
               CALL DAT_COPY( NXFIGI, LF, 'Z', STATUS )
               CALL DAT_ANNUL( NXFIGI, STATUS )
            END IF

*         Why in this order Jo? (MJC)
            DO I = NCOMP, 1, -1

*            Obtain a locator to the component and find its name.
               CALL DAT_INDEX( NXFIG, I, NXFIGI, STATUS )
               CALL DAT_NAME( NXFIGI, MFNAM, STATUS )

*            There are some special cases.  First the magnitude flag and
*            the range, which go to the .Z structure.
               IF ( MFNAM .EQ. 'MAGFLAG' .OR. MFNAM .EQ. 'RANGE' ) THEN

*               Create .Z structure in the Figaro file, if it does not
*               exist, and get a locator to it.
                  CALL DAT_THERE( LF, 'Z', EXIST, STATUS )
                  IF ( .NOT. EXIST ) THEN
                     CALL DAT_NEW( LF, 'Z', 'IMAGE', 0, 0, STATUS )
                  END IF
                  CALL DAT_FIND( LF, 'Z', LFZ, STATUS )

*               Copy the component to the Figaro Z structure.
                  CALL DAT_COPY( NXFIGI, LFZ, MFNAM, STATUS )

*            Next special cases are the airmass and time, which go to
*            the .OBS structure.
               ELSE IF ( MFNAM .EQ. 'SECZ' .OR. MFNAM .EQ. 'TIME' ) THEN

*               Create .OBS structure in the Figaro file, if it does not
*               exist, and get a locator to it.
                  CALL DAT_THERE( LF, 'OBS', EXIST, STATUS )
                  IF ( .NOT. EXIST ) THEN
                     CALL DAT_NEW( LF, 'OBS', 'OBS', 0, 0, STATUS )
                  END IF
                  CALL DAT_FIND( LF, 'OBS', LFO, STATUS )

*               Copy the component to the Figaro OBS structure.
                  CALL DAT_COPY( NXFIGI, LFO, MFNAM, STATUS )

*            Move a non-standard object to the default destination
*            structure.  OBS and Z structures have already been created.
               ELSE IF ( MFNAM .NE. 'OBS' .AND. MFNAM .EQ. 'Z' ) THEN

*               Before performing the copy, ensure that the object does
*               not exist. Ideally, one would like to cope with the
*               non-standard objects by going down a level and
*               processing them individually.
                  CALL DAT_THERE( LF, MFNAM, EXIST, STATUS )
                  IF ( .NOT. EXIST ) THEN

*                  Copy the component to the Figaro structure.
                     CALL DAT_COPY( NXFIGI, LF, MFNAM, STATUS )
                  END IF
               END IF
            END DO

*         Tidy unwanted locators.
            CALL DAT_ANNUL( NXFIGI, STATUS )
         END IF

         CALL DAT_ANNUL( NXFIG, STATUS )
      END IF

*   Deal with other extensions (if any).
*   ====================================
*
*   These are copied recursively from NDF.MORE to FIGARO.MORE.  The
*   exceptions are the FIGARO and FITS extensions which already have a
*   defined destination.

      NEXT = 0
      FIGMOR = .FALSE.
      DO WHILE ( XNAME .NE. ' ' .AND. STATUS .EQ. SAI__OK )
         NEXT = NEXT + 1

*       Search through the NDF's extensions.  Only process non-standard
*       extensions.
         CALL NDF_XNAME( NDF, NEXT, XNAME, STATUS )
         IF (XNAME(1:6) .NE. 'FIGARO' .AND. XNAME .NE. ' ' .AND.
     :       XNAME(1:4) .NE. 'FITS') THEN

*         Obtain a locator to the current extension.
            CALL NDF_XLOC( NDF, XNAME, 'READ', NXLOC, STATUS )

*         Create a .MORE structure if one does not already exist.
            IF ( .NOT. FIGMOR ) THEN
               CALL DAT_NEW( LF, 'MORE', 'EXT', 0, 0, STATUS )
               CALL DAT_FIND( LF, 'MORE', LFMOR, STATUS )
               FIGMOR = .TRUE.
            END IF

*         Inquire the type of the extension.
            CALL DAT_TYPE( NXLOC, EXTTYP, STATUS )

*         Create the NDF extension structure into the Figaro .MORE
*         structure and obtain a locator to it.
            CALL DAT_NEW( LFMOR, XNAME, EXTTYP, 0, 0, STATUS )
            CALL DAT_FIND( LFMOR, XNAME, LFMORI, STATUS )

*         Obtain the number of components.
            CALL DAT_NCOMP( NXLOC, NCOMP, STATUS )

*         For each component of the extension, find its name and
*         recursively copy it into the Figaro .MORE structure.
            DO I = NCOMP, 1, -1
               CALL DAT_INDEX( NXLOC, I, NXLOCI, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL DAT_NAME( NXLOCI, MFNAM, STATUS )
                  CALL DAT_COPY( NXLOCI, LFMORI, MFNAM, STATUS )
               END IF
            END DO

*         Tidy the locator to the input extension and component.
            CALL DAT_ANNUL( NXLOCI, STATUS )
            CALL DAT_ANNUL( NXLOC, STATUS )
         END IF
      END DO

*   Deal with main data structure.
*   ==============================
*
*   NDF data array -> OUTPUT.Z.DATA
*   NDF imaginary component of data array -> OUTPUT.Z.IMAGINARY

*  Obtain the full type of the data component.
      CALL NDF_FTYPE( NDF, 'Data', FTYPE, STATUS )
      CMPLEX = INDEX( FTYPE, 'COMPLEX' ) .NE. 0

*   Map the NDF data array with the appropriate type.
      CALL NDF_TYPE( NDF, 'Data', TYPE, STATUS )
      CALL NDF_DIM( NDF, NDF__MXDIM, DIM, NDIM, STATUS )
      IF ( CMPLEX ) THEN
         CALL NDF_MAPZ( NDF, 'Data', TYPE, 'READ', NDPTR, NIPTR, NELM,
     :                  STATUS )
      ELSE
         CALL NDF_MAP( NDF, 'Data', TYPE, 'READ', NDPTR, NELM, STATUS )
      END IF

*   Create .Z structure in the Figaro file, if it does not exist, and
*   get a locator to it.
      CALL DAT_THERE( LF, 'Z', EXIST, STATUS )
      IF ( .NOT. EXIST ) THEN
         CALL DAT_NEW( LF, 'Z', 'IMAGE', 0, 0, STATUS )
      END IF
      CALL DAT_FIND( LF, 'Z', LFZ, STATUS )

*   Create .Z.DATA in the Figaro file and get a locator to it.
      CALL DAT_NEW( LFZ, 'DATA', TYPE, NDIM, DIM, STATUS )
      CALL DAT_FIND( LFZ, 'DATA', LFZD, STATUS )

*   Map .Z.DATA.
      CALL DAT_MAP( LFZD, TYPE, 'WRITE', NDIM, DIM, FDPTR, STATUS )

*   Move data into the Figaro data array.
      CALL KPG1_COPY( TYPE, NELM, NDPTR, FDPTR, STATUS )

*   Unmap .Z.DATA.
      CALL DAT_ANNUL( LFZD, STATUS )

*   Now deal with any imaginary part of complex data.
      IF ( CMPLEX ) THEN

*   Create .Z.IMAGINARY in the Figaro file and get a locator to it.
         CALL DAT_NEW( LFZ, 'IMAGINARY', TYPE, NDIM, DIM, STATUS )
         CALL DAT_FIND( LFZ, 'IMAGINARY', LFZI, STATUS )

*   Map .Z.IMAGINARY
         CALL DAT_MAP( LFZI, TYPE, 'WRITE', NDIM, DIM, FIPTR, STATUS )

*   Move imaginary data into the Figaro data array.
         CALL KPG1_COPY( TYPE, NELM, NIPTR, FIPTR, STATUS )

*   Unmap .Z.IMAGINARY
         CALL DAT_ANNUL( LFZI, STATUS )

      END IF

*   Unmap NDF mapped arrays.
      CALL NDF_UNMAP( NDF, 'Data', STATUS )

*   Determine whether or not there may be bad pixels in the NDF.
*   ============================================================
*
*   This is needed because Figaro assumes that if the FLAGGED logical
*   is not present, that there are no bad values, whereas NDF_ (SGP/38)
*   assumes that the absence of BAD_PIXEL indicates that bad values may
*   be present.  Bad values in the mapped NDF data array arise either
*   because of bad values already in situ, or from the QUALITY array
*   when BADBITS is non-zero.
      CALL NDF_STATE( NDF, 'Quality', QUPRES, STATUS )
      CALL NDF_BB( NDF, BADBIT, STATUS )
      CALL NDF_BAD( NDF, 'Data', .TRUE., BADPIX, STATUS )

*   Set the FLAGGED logical in the DST when there may be bad values in
*   the mapped NDF data array.
      FLPRES = ( QUPRES .AND. BADBIT .NE. 0 ) .OR. BADPIX
      CALL DAT_NEW( LFZ, 'FLAGGED', '_LOGICAL', 0, 0, STATUS )
      CALL CMP_PUT0L( LFZ, 'FLAGGED', FLPRES, STATUS )

*   Copy the character components.
*   ==============================
*
*   Copy UNITS TO .Z.UNITS
      CALL NDF_STATE( NDF, 'UNITS', EXIST, STATUS )
      IF ( EXIST ) THEN

*      Obtain the UNITS component and the number of characters within
*      it.
         CALL NDF_CGET( NDF, 'UNITS', UNITS, STATUS )
         CALL NDF_CLEN( NDF, 'UNITS', LEN, STATUS )

*      Create the UNITS component in the Figaro file, obtain a locator
*      to it, and put the value in it.
         CALL DAT_NEWC( LFZ, 'UNITS', LEN, 0, 0,  STATUS )
         CALL DAT_FIND( LFZ, 'UNITS', LFZU, STATUS )
         CALL DAT_PUTC( LFZU, 0, 0, UNITS, STATUS )
         CALL DAT_ANNUL( LFZU, STATUS )
      END IF


*   Copy LABEL TO .Z.LABEL.
      CALL NDF_STATE( NDF, 'LABEL', EXIST, STATUS )
      IF ( EXIST ) THEN

*      Obtain the LABEL component and the number of characters within
*      it.
         CALL NDF_CGET( NDF, 'LABEL', LABEL, STATUS )
         CALL NDF_CLEN( NDF, 'LABEL', LEN, STATUS )

*      Create the LABEL component in the Figaro file, obtain a locator
*      to it, and put the value in it.
         CALL DAT_NEWC( LFZ, 'LABEL', LEN, 0, 0,  STATUS )
         CALL DAT_FIND( LFZ, 'LABEL', LFZL, STATUS )
         CALL DAT_PUTC( LFZL, 0, 0, LABEL, STATUS )
         CALL DAT_ANNUL( LFZL, STATUS )
      END IF

*   Copy TITLE TO .OBS.OBJECT.
      CALL NDF_STATE( NDF, 'TITLE', EXIST, STATUS )
      IF (EXIST) THEN

*      Obtain the TITLE component and the number of characters within
*      it.
         CALL NDF_CGET( NDF, 'TITLE', TITLE, STATUS )
         CALL NDF_CLEN( NDF, 'TITLE', LEN, STATUS )

*      Look for the destination OBS structure.  Create one if not
*      already present, and obtain a locator ot it.
         CALL DAT_THERE( LF, 'OBS', EXIST, STATUS )
         IF ( .NOT. EXIST ) THEN
            CALL DAT_NEW( LF, 'OBS', 'OBS', 0, 0,  STATUS )
         END IF
         CALL DAT_FIND( LF, 'OBS', LFO, STATUS )

*      Create the OBJECT component in the Figaro OBS structure, obtain
*      a locator to it, and put the value in it.
         CALL DAT_NEWC( LFO, 'OBJECT', LEN, 0, 0, STATUS )
         CALL DAT_FIND( LFO, 'OBJECT', LFOO, STATUS )
         CALL DAT_PUTC( LFOO, 0, 0, TITLE, STATUS )
         CALL DAT_ANNUL( LFOO, STATUS )

*      Note that the OBS structure is present.
         FIGOBS=.TRUE.
      END IF

*   Deal with variance structure.
*   =============================
*
*   VARIANCE -> .Z.ERRORS

*   See whether or not there is variance in the NDF.
      CALL NDF_STATE( NDF, 'VARIANCE', EXIST, STATUS )
      IF (EXIST) THEN

*      Map the NDF data array with the appropriate type.
         CALL NDF_TYPE( NDF, 'VARIANCE', TYPE, STATUS )
         CALL NDF_MAP( NDF, 'VARIANCE', TYPE, 'READ', NVPTR, NELM,
     :                 STATUS )

*      Create .Z.ERRORS in the Figaro file and get a locator to it.
         CALL DAT_NEW( LFZ, 'ERRORS', TYPE, NDIM, DIM, STATUS )
         CALL DAT_FIND( LFZ, 'ERRORS', LFZE, STATUS )

*      Map .Z.ERRORS.  Bad pixels may be present.
         CALL DAT_MAP( LFZE, TYPE, 'WRITE', NDIM, DIM, FEPTR, STATUS )
         BAD=.TRUE.

*      Take the square root of the variance to make the error array.
*      Call a routine appropriate to the variance type.
         IF ( TYPE .EQ. '_REAL' ) THEN
            CALL VEC_SQRTR( BAD, NELM, %VAL(CNF_PVAL(NVPTR)),
     :                      %VAL(CNF_PVAL(FEPTR)),
     :                      IERR, NERR, STATUS )
         ELSE IF ( TYPE .EQ. '_DOUBLE') THEN
            CALL VEC_SQRTD( BAD, NELM, %VAL(CNF_PVAL(NVPTR)),
     :                      %VAL(CNF_PVAL(FEPTR)),
     :                      IERR, NERR, STATUS )
         ELSE IF ( TYPE .EQ. '_INTEGER') THEN
            CALL VEC_SQRTI( BAD, NELM, %VAL(CNF_PVAL(NVPTR)),
     :                      %VAL(CNF_PVAL(FEPTR)),
     :                      IERR, NERR, STATUS )
         ELSE IF ( TYPE .EQ. '_BYTE') THEN
            CALL VEC_SQRTB( BAD, NELM, %VAL(CNF_PVAL(NVPTR)),
     :                      %VAL(CNF_PVAL(FEPTR)),
     :                      IERR, NERR, STATUS )
         ELSE IF ( TYPE .EQ. '_WORD') THEN
            CALL VEC_SQRTW( BAD, NELM, %VAL(CNF_PVAL(NVPTR)),
     :                      %VAL(CNF_PVAL(FEPTR)),
     :                      IERR, NERR, STATUS )
         ELSE IF ( TYPE .EQ. '_UBYTE') THEN
            CALL VEC_SQRTUB( BAD, NELM, %VAL(CNF_PVAL(NVPTR)),
     :                       %VAL(CNF_PVAL(FEPTR)),
     :                      IERR, NERR, STATUS )
         ELSE IF ( TYPE .EQ. '_UWORD') THEN
            CALL VEC_SQRTUW( BAD, NELM, %VAL(CNF_PVAL(NVPTR)),
     :                       %VAL(CNF_PVAL(FEPTR)),
     :                      IERR, NERR, STATUS )
         END IF

*      Unmap .Z.ERRORS and the variance.
         CALL DAT_ANNUL( LFZE, STATUS )
         CALL NDF_UNMAP( NDF, 'Variance', STATUS )

      END IF


*   Deal with the axis structure.
*   =============================

*   Obtain a locator to the NDF.  This is needed as there are no
*   NDF_ routines to handle axis extensions.
      CALL NDF_LOC( NDF, 'READ', LOCN, STATUS )

*   Loop for each axis element.
      DO IAXIS = 1, NDIM

*      Determine whether or not there is an axis structure in the NDF.
         CALL NDF_ASTAT( NDF, 'Centre', IAXIS, EXIST, STATUS )
         IF ( EXIST ) THEN

*         Initialise flags to indicate that standard components have
*         not been copied.
            AXDATA = .FALSE.
            AXWIDT = .FALSE.
            AXVAR = .FALSE.

*         Copy any Figaro extension objects.
*         ==================================

*         Search for a Figaro extension.  First get locators to the
*         axis, then the particular element.
            CALL DAT_FIND( LOCN, 'AXIS', LOCA, STATUS )
            CALL DAT_CELL( LOCA, 1, IAXIS, LOCAI, STATUS )

*         Look for the extension structure.  If it exists, look for a
*         Figaro axis extension.
            CALL DAT_THERE( LOCAI, 'MORE', EXIST, STATUS )
            IF ( EXIST ) THEN
               CALL DAT_FIND( LOCAI, 'MORE', LOCM, STATUS )
               CALL DAT_THERE( LOCM, 'FIGARO', EXIST, STATUS )
               IF ( EXIST ) THEN
                  CALL DAT_FIND( LOCM, 'FIGARO', LOCF, STATUS )

*               There is a Figaro axis extension.  Now we need to copy
*               it to the Figaro axis structure.
                  CALL DAT_COPY( LOCF, LF, AXNAME( IAXIS ), STATUS )

*               This copy may have implications for items in the
*               top-level axis, for example if an extension data array
*               is already propagated we do not want to copy the
*               top-level axis centres.  Therefore we have to check for
*               the existence of the axis centres, variances, and
*               widths.
                  CALL DAT_THERE( LOCF, 'DATA_ARRAY', AXDATA, STATUS )
                  CALL DAT_THERE( LOCF, 'VARIANCE', AXVAR, STATUS )
                  CALL DAT_THERE( LOCF, 'WIDTH', AXWIDT, STATUS )

*               Tidy the locator to the Figaro axis extension.
                  CALL DAT_ANNUL( LOCF, STATUS )
               END IF

*            Tidy the locator to the axis-extension structure.
               CALL DAT_ANNUL( LOCM, STATUS )
            END IF

*         Tidy some locators.
            CALL DAT_ANNUL( LOCAI, STATUS )
            CALL DAT_ANNUL( LOCA, STATUS )

*         Create the appropriate Figaro axis structure if not already
*         present; these are: (.X .Y .T .U .V or .W).
            CALL DAT_THERE( LF, AXNAME( IAXIS ), EXIST, STATUS )
            IF ( .NOT. EXIST ) THEN
               CALL DAT_NEW( LF, AXNAME( IAXIS ), 'AXIS', 0, 0, STATUS )
            END IF
            CALL DAT_FIND( LF, AXNAME( IAXIS ), LFAX, STATUS )

*         Deal with the standard top-level components.  All non-standard
*         components will be lost.

*         If there is an axis-centre array already either just use the
*         existing primitive array after renaming from NDF standard to
*         DST nomenclature (i.e. DATA_ARRAY to DATA); or if it's an
*         <ARRAY> structure move the primitive component (called DATA)
*         from within the DATA_ARRAY <ARRAY> structure, and then erase
*         the <ARRAY> structure.  Tidy the temporary locator at the
*         end (DAT_MOVE annuls the LFAXD locator and the erase
*         operation annuls LFAXDA).
            IF ( AXDATA ) THEN
               CALL DAT_FIND( LFAX, 'DATA_ARRAY', LFAXDA, STATUS )
               CALL DAT_PRIM( LFAXDA, PRIM, STATUS )
               IF ( PRIM ) THEN
                  CALL DAT_RENAM( LFAXDA, 'DATA', STATUS )
                  CALL DAT_ANNUL( LFAXDA, STATUS )
               ELSE
                  CALL DAT_FIND( LFAXDA, 'DATA', LFAXD, STATUS )
                  CALL DAT_MOVE( LFAXD, LFAX, 'DATA', STATUS )
                  CALL DAT_ERASE( LFAX, 'DATA_ARRAY', STATUS )
               END IF

*         Otherwise deal with the (1-dimensional) data array if there is
*         not an axis-centre array already.
            ELSE

*            Map the axis data array with the appropriate type.
               CALL NDF_ATYPE( NDF, 'Centre', IAXIS, TYPE, STATUS )
               CALL NDF_AMAP( NDF, 'Centre', IAXIS, TYPE, 'READ',
     :                        AXPTR, NAXELM, STATUS )

*            Create the axis.DATA structure.
               CALL DAT_NEW( LFAX, 'DATA', TYPE, 1, DIM( IAXIS ),
     :                       STATUS )
               CALL DAT_FIND( LFAX, 'DATA', LFAXD, STATUS )

*            Map the FIGARO axis data.
               CALL DAT_MAP( LFAXD, TYPE, 'WRITE', 1, DIM( IAXIS ),
     :                       FAXPTR, STATUS )

*            Move the NDF axis data into Figaro axis data array.
               CALL KPG1_COPY( TYPE, DIM( IAXIS ), AXPTR, FAXPTR,
     :                         STATUS )

*            Unmap Figaro axis data.
               CALL DAT_ANNUL( LFAXD, STATUS )

*            Unmap NDF axis centres.
               CALL NDF_AUNMP( NDF, 'Centre', IAXIS, STATUS )
            END IF

*         Copy the (1-dimensional) variance array if there is not a
*         variance array already.
            IF ( .NOT. AXVAR ) THEN

*            Look to see if there is an axis-variance array.
               CALL NDF_ASTAT( NDF, 'Variance', IAXIS, EXIST, STATUS )

               IF ( EXIST ) THEN

*               Map the axis variance array with the appropriate type.
                  CALL NDF_ATYPE( NDF, 'Variance', IAXIS, TYPE, STATUS )
                  CALL NDF_AMAP( NDF, 'Variance', IAXIS, TYPE, 'READ',
     :                           AXPTR, NAXELM, STATUS )

*               Create the axis.VARIANCE structure.
                  CALL DAT_NEW( LFAX, 'VARIANCE', TYPE, 1, DIM( IAXIS ),
     :                          STATUS )
                  CALL DAT_FIND( LFAX, 'VARIANCE', LFAXV, STATUS )

*               Map the FIGARO axis variances.
                  CALL DAT_MAP( LFAXV, TYPE, 'WRITE', 1, DIM( IAXIS ),
     :                          FAXPTR, STATUS )

*               Move the NDF axis variances into Figaro axis-variance
*               array.
                  CALL KPG1_COPY( TYPE, DIM( IAXIS ), AXPTR, FAXPTR,
     :                            STATUS )

*                Unmap Figaro axis variances.
                  CALL DAT_ANNUL( LFAXV, STATUS )

*                Unmap NDF variance array.
                  CALL NDF_AUNMP( NDF, 'Variance', IAXIS, STATUS )
               END IF
            END IF

*         If there is an axis-width array already either just use the
*         existing primitive array; or if it's an <ARRAY> structure
*         move the primitive component (called DATA) from within the
*         WIDTH <ARRAY> structure, and then erase the <ARRAY>
*         structure.  Since in the latter case these have the same name
*         we rename the original width array to OWIDTH.  Tidy the
*         temporary locator at the end (DAT_MOVE annuls the LFAXWD
*         locator and the erase operation annuls LFAXW).
            IF ( AXWIDT ) THEN
               CALL DAT_FIND( LFAX, 'WIDTH', LFAXW, STATUS )
               CALL DAT_PRIM( LFAXW, PRIM, STATUS )
               IF ( PRIM ) THEN
                  CALL DAT_ANNUL( LFAXW, STATUS )
               ELSE
                  CALL DAT_RENAM( LFAXW, 'OWIDTH', STATUS )
                  CALL DAT_FIND( LFAXW, 'DATA', LFAXWD, STATUS )
                  CALL DAT_MOVE( LFAXWD, LFAX, 'WIDTH', STATUS )
                  CALL DAT_ERASE( LFAX, 'OWIDTH', STATUS )
               END IF

*         Copy the (1-dimensional) width array if there is not a width
*         array already.
            ELSE

*            Look to see if there is an axis-width array.
               CALL NDF_ASTAT( NDF, 'Width', IAXIS, EXIST, STATUS )

               IF ( EXIST ) THEN

*               Map the axis width array with the appropriate type.
                  CALL NDF_ATYPE( NDF, 'Width', IAXIS, TYPE, STATUS )
                  CALL NDF_AMAP( NDF, 'Width', IAXIS, TYPE, 'READ',
     :                           AXPTR, NAXELM, STATUS )

*               Create the axis.WIDTH structure.
                  CALL DAT_NEW( LFAX, 'WIDTH', TYPE, 1, DIM( IAXIS ),
     :                          STATUS )
                  CALL DAT_FIND( LFAX, 'WIDTH', LFAXW, STATUS )

*               Map the FIGARO axis widths.
                  CALL DAT_MAP( LFAXW, TYPE, 'WRITE', 1, DIM( IAXIS ),
     :                          FAXPTR, STATUS )

*               Move the NDF axis widths into Figaro axis-width array.
                  CALL KPG1_COPY( TYPE, DIM( IAXIS ), AXPTR, FAXPTR,
     :                            STATUS )

*                Unmap Figaro axis widths.
                  CALL DAT_ANNUL( LFAXW, STATUS )

*                Unmap NDF width array.
                  CALL NDF_AUNMP( NDF, 'Width', IAXIS, STATUS )
               END IF
            END IF

*         Copy the AXIS label if any.
            CALL NDF_ASTAT( NDF, 'LABEL', IAXIS, EXIST, STATUS )
            IF ( EXIST ) THEN

*             Obtain the axis label and its length in characters.
               CALL NDF_ACGET( NDF, 'LABEL', IAXIS, LABEL, STATUS )
               CALL NDF_ACLEN( NDF, 'LABEL', IAXIS, LEN, STATUS )

*             Create the label component in the Figaro axis structure
*             and put the value into it.
               CALL DAT_NEWC( LFAX, 'LABEL', LEN, 0, 0,  STATUS )
               CALL DAT_FIND( LFAX, 'LABEL', LFAXL, STATUS )
               CALL DAT_PUTC( LFAXL, 0, 0, LABEL, STATUS )
               CALL DAT_ANNUL( LFAXL, STATUS )
            END IF

*         Copy AXIS units if any.
            CALL NDF_ASTAT( NDF, 'UNITS', IAXIS, EXIST, STATUS )
            IF ( EXIST ) THEN

*             Obtain the axis units and its length in characters.
               CALL NDF_ACGET( NDF, 'UNITS', IAXIS, UNITS, STATUS )
               CALL NDF_ACLEN( NDF, 'UNITS', IAXIS, LEN, STATUS )

*             Create the units component in the Figaro axis structure
*             and put the value into it.
               CALL DAT_NEWC( LFAX, 'UNITS', LEN, 0, 0,  STATUS )
               CALL DAT_FIND( LFAX, 'UNITS', LFAXU, STATUS )
               CALL DAT_PUTC( LFAXU, 0, 0, UNITS, STATUS )
               CALL DAT_ANNUL( LFAXU, STATUS )
            END IF

*         Tidy Figaro axis locator.
            CALL DAT_ANNUL( LFAX, STATUS )

         END IF
      END DO

*   Tidy the locator to the axis structure.
      CALL DAT_ANNUL( LOCN, STATUS )

*   Deal with FITS extension (if any).
*   ==================================
*
*   First see if it exists.
      CALL NDF_XSTAT( NDF, 'FITS', EXIST, STATUS )

      IF ( EXIST ) THEN

*      Get a locator to the FITS extension and find the number of
*      FITS items.
         CALL NDF_XLOC( NDF, 'FITS', 'READ', NXFIT, STATUS )
         CALL DAT_SHAPE( NXFIT, 10, DIM, NDIM, STATUS )

*      Create FIGARO .FITS structure.
         CALL DAT_NEW( LF, 'FITS', 'FITS', 0, 0, STATUS )
         CALL DAT_FIND( LF, 'FITS', LFFT, STATUS )

*      Create FIGARO .COMMENTS structure.
         CALL DAT_NEW( LF, 'COMMENTS', 'COMMENTS', 0, 0, STATUS )
         CALL DAT_FIND( LF, 'COMMENTS', LFCOM, STATUS )

*      Get the FITS items one by one, test the type and create an
*      item of appropriate type in the Figaro FITS structure.
*      If an item has an associated comment, this is written to the
*      Figaro COMMENTS structure.

         DO I = 1, DIM( 1 )

*         Get a FITS item.
            CALL DAT_SLICE( NXFIT, 1, I, I, LFMFI, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL DAT_GET( LFMFI, '_CHAR', 1, 1, STRING, STATUS )

*            Extract the various components of the FITS card image.
*            ======================================================

*            First 8 characters contain the keyword.
               FITNAM = STRING( 1:8 )

*            The value is not defined yet.
               VALDEF = .FALSE.

*            The FITS object string the value and comments uses the
*            FITS keyword for their names.  Since there can only be one
*            object of a given name in a structure and that Figaro DSA
*            does not support arrays of values for a given FITS keyword
*            we must determine whether or not the HDS object already
*            exists.  Allow for the case where there is a blank card,
*            i.e. the name is blank.
               IF ( FITNAM .NE. ' ' ) THEN
                  CALL DAT_THERE( LFFT, FITNAM, HUSED, STATUS )
               ELSE
                  HUSED = .TRUE.
               END IF

*            Look for a blank keyword.  Skip the card image if there
*            is no keyword present.  Note that this cannot handle
*            hierarchical keywords.  Skip over the END keyword too.
*            Also we cannot write the value and comments if the keyword
*            is duplicated because the the HDS object already exists.
               IF ( FITNAM .NE. ' ' .AND. FITNAM .NE. 'END     ' .AND.
     :             .NOT. HUSED ) THEN

*               Deal with the special case of COMMENT and HISTORY
*               keywords.
                  IF ( FITNAM .EQ. 'HISTORY' .OR.
     :                 FITNAM .EQ. 'COMMENT' ) THEN

*                  There is no equals sign for these so extract the
*                  comment or history.  Get it's effective length.
                     FITDAT = STRING( 9:80 )
                     NCCOM = CHR_LEN( FITDAT )

*                  Create the object and a locator to it.
                     CALL DAT_NEWC( LFFT, FITNAM, NCCOM, 0, 0, STATUS )
                     CALL DAT_FIND( LFFT, FITNAM, LFFTD, STATUS )

*                  Write the value to the FITS structure.
                     CALL DAT_PUTC( LFFTD, 0, 0, FITDAT( :NCCOM ),
     :                              STATUS )

*               Deal with the normal headers...
                  ELSE

*                  Columns 11:30 contain FITS item value if numeric or
*                  logical.  Character values may extend to column 80.
*                  Search for the leading quote.
                     IF ( STRING( 11:11 ) .EQ. '''' ) THEN

*                     Initialise the character value.
                        FITDAT = ' '

*                     Determine where the trailing blank is located.
*                     ==============================================

*                     First look for any double quotes.  These mean a
*                     single quote, so O'Hara appears as O''Hara in a
*                     FITS card image.  Start the search from column
*                     12, i.e. after the leading quote, and
*                     subsequently immediately following a double
*                     quote.  Prevent the search extending beyond the
*                     end of the value.  Keep a count of the absolute
*                     column.
                        NCDQ = 1
                        NCSTQ = 12
                        NCFD = 1
                        DO WHILE ( NCDQ .NE. 0 )
                           NCDQ = INDEX( STRING( NCSTQ: ), '''''' )

*                         If there is no double quote no action is
*                         necessary.  When there is we form part of the
*                         value, removing the second quote.
                           IF ( NCDQ .GT. 0 ) THEN
                              FITDAT( NCFD: NCFD + NCDQ - 1 ) =
     :                                STRING( NCSTQ: NCSTQ + NCDQ - 1 )

*                            Increment the counter to where to append to
*                            the value.
                              NCFD = NCFD + NCDQ

*                            Increment the counter in the FITS card,
*                            skipping over the double quote.
                              NCSTQ = NCSTQ + NCDQ + 1
                           END IF
                        END DO

*                     Closing quote must occur in or after column 10
*                     (20 in the card).
                        NCCQ = INDEX( STRING( MAX( 12, NCSTQ ): ),
     :                                '''' )

*                     There is no trailing quote so assume that the
*                     character value extends to the end of the card
*                     image.  Copy the value.
                        IF ( NCCQ .EQ. 0 ) THEN
                           FITDAT = STRING( NCSTQ: )

*                     Set the position as if the quote were at the end
*                     of the card.
                           NCCQ = 67

*                     Append the remainder or all of the value (the
*                     latter when there is no double quote in the FITS
*                     card image) to the value
                        ELSE
                           FITDAT( NCFD:NCFD + NCCQ - 2 ) =
     :                             STRING( NCSTQ:NCSTQ + NCCQ - 2 )
                        END IF

*                     The value is defined.
                        VALDEF = .TRUE.

*                     Define the column from which to look for a
*                     comment, i.e. two columns after the trailing
*                     quote.
                        NCOMS = NCCQ + 13

*                  Straightforward for other types.
                     ELSE
                        FITDAT = STRING( 11:30 )
                        NCOMS = 31
                     END IF

*                  Look for the comment character /.
                     NCC = INDEX( STRING( NCOMS: ), '/' ) + NCOMS - 1
                     IF ( NCC .EQ. NCOMS - 1 ) THEN

*                     There is no comment.
                        FITCOM = ' '
                     ELSE

*                     Deal with the special case of a comment delimiter,
*                     but no comment.
                        IF ( CHR_LEN( STRING( NCC: ) ) .EQ. 1 ) THEN
                           FITCOM = ' '

*                     Extract the comment associated with the keyword.
*                     Watch for a leading space, which can be ignored.
                        ELSE IF ( STRING( NCC+1:NCC+1 ) .EQ. ' ' ) THEN
                           FITCOM = STRING( NCC+2: )
                        ELSE
                           FITCOM = STRING( NCC+1: )
                        END IF
                     END IF

*                 Put value of item into .FITS.xxx.
*                 =================================

*                  Find its type.  Although we have partially done
*                  this, i.e.  located a character value, there are
*                  naughty people who don't put the quotes about the
*                  character value.  For these it is assumed that the
*                  value extends no further than column 30 of the FITS
*                  card-image.

*                  Look to see if it's already been identified as a
*                  string.
                     IF ( VALDEF ) THEN

*                     Find the length of the string.  This must have
*                     at least the minimum number of characters, even it
*                     is blank.
                        NCCOM = MAX( MINFCV, CHR_LEN( FITDAT ) )

*                     Create the object and a locator to it.
                        CALL DAT_NEWC( LFFT, FITNAM, NCCOM, 0, 0,
     :                                 STATUS )
                        CALL DAT_FIND( LFFT, FITNAM, LFFTD, STATUS )

*                     Write the value to the FITS structure.
                        CALL DAT_PUTC( LFFTD, 0, 0, FITDAT( :NCCOM ),
     :                                 STATUS )
                     ELSE

*                     Check for an INTEGER.
                        CSTAT = 0
                        CLEN = CHR_LEN( FITDAT )
                        CALL CHR_CTOI( FITDAT, IVAL, CSTAT )
                        IF ( CSTAT .EQ. 0 .AND. CLEN .GT. 0 ) THEN

*                        Create the integer object and give it the
*                        value.
                           TYPE = '_INT'
                           CALL DAT_NEW0I( LFFT, FITNAM, STATUS )
                           CALL DAT_FIND( LFFT, FITNAM, LFFTD, STATUS )
                           CALL DAT_PUTI( LFFTD, 0, 0, IVAL, STATUS )
                        ELSE

*                        Check for a REAL.
                           CSTAT = 0
                           CALL CHR_CTOR( FITDAT, RVAL, CSTAT )
                           IF ( CSTAT .EQ. 0 .AND. CLEN.GT.0 ) THEN

*                           Create the real object and give it the
*                           value.
                              TYPE = '_REAL'
                              CALL DAT_NEW0R( LFFT, FITNAM, STATUS )
                              CALL DAT_FIND( LFFT, FITNAM, LFFTD,
     :                                       STATUS )
                              CALL DAT_PUTR( LFFTD, 0, 0, RVAL, STATUS )
                           ELSE

*                           Check for a logical.  Note it must be T or
*                           F in the header column 30 (20 in in the
*                           data value).
                              CSTAT = 0
                              CALL CHR_CTOL( FITDAT(20:20), LVAL,
     :                                       CSTAT )
                              IF ( CSTAT .EQ. 0 .AND. CLEN .GT. 0 .AND.
     :                             FITDAT(1:19) .EQ. ' ' ) THEN

*                              Create the logical object and give it the
*                              value.
                                 TYPE = '_LOGICAL'
                                 CALL DAT_NEW0L( LFFT, FITNAM, STATUS )
                                 CALL DAT_FIND( LFFT, FITNAM, LFFTD,
     :                                          STATUS )
                                 CALL DAT_PUTL( LFFTD, 0, 0, LVAL,
     :                                          STATUS )
                              ELSE

*                              Assume it's just a character string
*                              without quotes.  Make the size equal to
*                              the length of the character string.
*                              Create the object and write the
*                              character value.
                                 CALL DAT_NEWC( LFFT, FITNAM, NCOMS-11,
     :                                          0, 0, STATUS )
                                 CALL DAT_FIND( LFFT, FITNAM, LFFTD,
     :                                          STATUS )
                                 CALL DAT_PUTC( LFFTD, 0, 0, FITDAT,
     :                                          STATUS )
                              END IF
                           END IF
                        END IF
                     END IF

*                Put any comments into .COMMENTS.XXX.  First create the
*                object, get a locator to it, and put the value into the
*                object.
                     IF ( CHR_LEN( FITCOM ) .NE. 0 ) THEN
                        CALL DAT_NEWC( LFCOM, FITNAM, 48, 0, 0, STATUS )
                        CALL DAT_FIND( LFCOM, FITNAM, LFFTC, STATUS )
                        CALL DAT_PUTC( LFFTC, 0, 0, FITCOM, STATUS )
                        CALL DAT_ANNUL( LFFTC, STATUS )

                     END IF
                     CALL DAT_ANNUL( LFFTD, STATUS )
                  END IF
               END IF

            END IF
            CALL DAT_ANNUL( LFMFI, STATUS )
         END DO

*      Erase Figaro .COMMENTS structure if this is empty.  Note that the
*      locator to the comments structure must be annul before the
*      structure is deleted.
         CALL DAT_NCOMP( LFCOM, NCOMP, STATUS )
         CALL DAT_ANNUL( LFCOM, STATUS )
         IF (NCOMP .EQ. 0) THEN
            CALL DAT_ERASE( LF, 'COMMENTS', STATUS )
         END IF

*      Annul locators.
         CALL DAT_ANNUL( LFFT, STATUS )
         CALL DAT_ANNUL( NXFIT, STATUS )
      END IF

*   Annul locators to Figaro .OBS structure if these have been used.
      IF ( FIGOBS ) THEN
         CALL DAT_ANNUL( LFO, STATUS )
      END IF

*   Annul locators to other objects.
      CALL DAT_ANNUL( LFZ, STATUS )

999   CONTINUE
      CALL NDF_END( STATUS )
      CALL HDS_CLOSE( LF, STATUS )
      END
