      SUBROUTINE FLATCOR( STATUS )
*+
*  Name:
*     FLATCOR

*  Purpose:
*     Divides a series of images by a flatfield

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FLATCOR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine applies a flat field correction to a series of images.
*     If any of the input data have been flagged as saturated using a
*     saturation value (instead of being marked as BAD) then the
*     saturation values may be protected from modification.

*  Usage:
*     flatcor in out flat

*  ADAM Parameters:
*     FLAT = LITERAL (Read)
*        Name of the image which contains the normalised (mean of one)
*        flatfield data. This should have been produced by a program
*        such as MAKEFLAT. The data should have a floating point HDS
*        data type (_REAL or _DOUBLE).  If USESET is true, FLAT should
*        be a group expression referring to one flatfield data file
*        matching each of the Set Index attributes represented in the
*        IN list; again the name of the file produced by MAKEFLAT will
*        normally be suitable.  The name of this file may be specified
*        using indirection through a file.
*        [Global flatfield]
*     IN = LITERAL (Read)
*        Names of the images containing the data which are to have the
*        flatfield correction applied.  The image names should be
*        separated by commas and may include wildcards.
*     KEEPIN = _LOGICAL (Read)
*        Whether to keep (i.e. not delete) the input images (parameter IN)
*        or not. Deleting the input images has the advantage of saving
*        disk space, but should probably only be used if this program
*        is part of a sequence of commands and the intermediary data
*        produced by it are not important.
*
*        The calibration master frame (parameter FLAT) is never deleted.
*
*        The default for this parameter is TRUE and this cannot be
*        overridden except by assignment on the command line or in
*        response to a forced prompt.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP,
*        then the value specified there will be used. Otherwise, the
*        default is "CCDPACK.LOG".
*        [CCDPACK.LOG]
*     LOGTO = LITERAL (Read)
*        Every CCDPACK application has the ability to log its output
*        for future reference as well as for display on the terminal.
*        This parameter controls this process, and may be set to any
*        unique abbreviation of the following:
*           -  TERMINAL  -- Send output to the terminal only
*           -  LOGFILE   -- Send output to the logfile only (see the
*                           LOGFILE parameter)
*           -  BOTH      -- Send output to both the terminal and the
*                           logfile
*           -  NEITHER   -- Produce no output at all
*
*        If the logging system has been initialised using CCDSETUP
*        then the value specified there will be used. Otherwise, the
*        default is "BOTH".
*        [BOTH]
*     OUT = LITERAL (Write)
*        Names of the output images. These may be specified as list of
*        comma separated names, using indirection if required, or,
*        as a single modification element (of the input names).
*        The simplest modification element is the asterisk "*" which
*        means call each of the output NDFs the same name as the
*        corresponding input images. So,
*           IN > *
*           OUT > *
*        signifies that all the images in the current directory should be
*        used and the output images should have the same names.
*
*        Other types of modification can also
*        occur, such as,
*           OUT > tmp_*
*        which means call the output images the same as the input images but
*        put tmp_ in front of the names. Replacement of a specified
*        string with another in the output file names can also be used,
*           OUT > tmp_*|debias|flattened|
*        this replaces the string debias with flattened in any of the
*        output names tmp_*.
*     SATURATION = _DOUBLE (Read)
*        The value at which the input data has been saturated. This
*        is only required if the saturation has been flagged using a
*        non-BAD value.
*        [1.0D6]
*     SETSAT = _LOGICAL (Read)
*        If the input data has had a saturation value applied then this
*        value should be set to TRUE. However, if the saturation has
*        been applied within CCDPACK then this will not be necessary as
*        this information will have been stored in the CCDPACK
*        extension.  Note that data with different saturation
*        properties (i.e. saturation values) which have not been set
*        within CCDPACK will require separate processing (see notes).
*        [FALSE]
*     PRESERVE = _LOGICAL (Read)
*        If the input data types are to be preserved and used for
*        processing then this parameter should be set TRUE [default].
*        If this parameter is set FALSE then the input data will be
*        processed and returned in a suitable floating point
*        representation. This option is useful if the output data will
*        have a significant number of BAD values due to numeric errors
*        (over or under flow), or if unacceptable loss of precision
*        will occur if the data are processed in the original data type
*        (due to rounding errors).
*
*        If a global value for this parameter has been set using
*        CCDSETUP then this will be used.
*        [TRUE]
*     TITLE = LITERAL (Read)
*        Title for the output images.
*        [Output from FLATCOR]
*     USESET = _LOGICAL (Read)
*        Whether to use Set header information or not.  If USESET is
*        false then any Set header information will be ignored.
*        If USESET is true, then the FLAT parameter is taken to
*        refer to a group of files, and each IN file will be processed
*        using a flatfield dataset with a Set Index attribute which
*        matches its own.  An IN file with no Set header is considered
*        to match a FLAT file with no Set header, so USESET can safely
*        be set true (the default) when the input files contain no
*        Set header information.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]

*  Examples:
*     flatcor frame1 frame1_f flatr
*        In this example the data in image frame1 are corrected for the
*        flatfield response stored in image flatr. The result of dividing
*        FRAME1 by flatr is written to image frame1_f. If a saturation
*        value has been applied to the data in frame1 then this will be
*        automatically accommodated by FLATCOR providing the saturation
*        has been applied within CCDPACK.
*
*     flatcor n4151r1 n4151r1f flatfield setsat=true saturation=32767
*        In this example the data have had a saturation value applied
*        which has not been recorded within CCDPACK and the required
*        information has been supplied.
*
*     flatcor in='*' out='*_flattened' flat=master_flatr
*        In this example all the images in the current directory are
*        processed. The resultant data are written to files with the
*        same name as the corresponding input images, but with the
*        characters "_flattened" appended to the filename.

*  Implementation Status:
*     - Supports processing of all non-complex numeric types.
*       BAD pixels are processed as are all NDF components.

*  Notes:
*     - If any of the input data have had their saturation values set by
*       applications not within CCDPACK, then this routine will require
*       this information if the values are to be propagated properly. If
*       more than one saturation value has been used then the input
*       frames will need to be processed singly. This is because FLATCOR
*       only uses one saturation value per input group. If the
*       saturation values have been set within CCDPACK (by DEBIAS)
*       these will be processed correctly and may be different.

*  Behaviour of Parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exceptions to this rule are:
*        - TITLE   -- always "Output from FLATCOR"
*        - KEEPIN  -- always TRUE
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets/different devices, or after a break of sometime.
*     The intrinsic default behaviour of the application may be
*     restored by using the RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE, PRESERVE, FLAT and USESET) have
*     global values. These global values will always take precedence,
*     except when an assignment is made on the command line.  In general
*     global values may be set and reset using the CCDSETUP and
*     CCDCLEAR commands, however, the FLAT parameter may only be set by
*     a run of the application MAKEFLAT.

*  Copyright:
*     Copyright (C) 1991-1992, 1994 Science & Engineering Research
*     Council. Copyright (C) 1995-1997, 1999-2001 Central Laboratory of
*     the Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-MAY-1991 (PDRAPER):
*        Original version.
*     13-JAN-1992 (PDRAPER):
*        Changed to add more generic facilities.
*     2-FEB-1994 (PDRAPER):
*        Added ability to delete input NDFs.
*     6-OCT-1995 (PDRAPER)
*        Updated for CCDPACK 2.0.
*     12-AUG-1996 (PDRAPER):
*        Fixed incorrect assignment of EXTSAT to SATVAL (linux port
*        bug).
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator controls (foreign data access upgrade).
*     23-FEB-1999 (MBT):
*        Modified to propagate WCS component.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     9-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'NDF_PAR'          ! NDF string sizes
      INCLUDE 'GRP_PAR'          ! Standard GRP constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are equal apart from case
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) DFILT ! Filter type of data
      CHARACTER * ( CCD1__NMLEN ) FILTER ! Filter type of flatfield
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Input data type.
      CHARACTER * ( NDF__SZTYP ) FTYPE ! Flatfield data type.
      DOUBLE PRECISION SATVAL    ! Saturation value
      INTEGER EL                 ! Number of elements in input array components
      INTEGER FLTGRP             ! NDG identifier for flatfield NDFs
      INTEGER GIDIN              ! Group identifier for input NDFs
      INTEGER GIDOUT             ! Group identifier for output NDFs
      INTEGER I                  ! Loop index
      INTEGER IDFLT              ! Input NDF identifier (flatfield)
      INTEGER IDFTMP             ! NDF identifier for flatfield section
      INTEGER IDIN               ! Input NDF identifier
      INTEGER IDOUT              ! Output NDF identifier
      INTEGER INDEX              ! Counter for NDF processing loop
      INTEGER INGRP              ! NDG identifier for all input NDFs
      INTEGER IPDFLT             ! Pointer to input Data component (ff)
      INTEGER IPDIN              ! Pointer to input Data component
      INTEGER IPDOUT             ! Pointer to output Data component
      INTEGER IPVFLT             ! Pointer to input Variance component (ff)
      INTEGER IPVIN              ! Pointer to input Variance component
      INTEGER IPVOUT             ! Pointer to output Data component
      INTEGER ISUB               ! Subgroup loop index
      INTEGER IVAL               ! Dummy
      INTEGER KEYGRP             ! GRP identifier for subgroup keys
      INTEGER LBND( 2 )          ! Lower of bounds of input NDF
      INTEGER LBNDC( 2 )         ! Lower bounds of calibration NDF.
      INTEGER LBNDL( 2 )         ! Lower bounds of last NDF
      INTEGER NDIM               ! Number of dimensions of NDFs
      INTEGER NNDF               ! Number of NDFs in subgroup
      INTEGER NSUB               ! Number of subgroups
      INTEGER NTOT               ! Total number of NDFs to process
      INTEGER OUTGRP             ! GRP identifier for output NDFs
      INTEGER SUBGRP( CCD1__MXNDF ) ! NDG identifiers for subgroups of NDFs
      INTEGER UBND( 2 )          ! Upper bounds of input NDF
      INTEGER UBNDC( 2 )         ! Upper bounds of calibration NDF.
      INTEGER UBNDL( 2 )         ! Upper bounds of last NDF
      LOGICAL BAD                ! Set if BAD values are present
      LOGICAL DELETE             ! Delete input NDFs
      LOGICAL EXTSAT             ! Saturation value from NDF extension
      LOGICAL FCHECK             ! Whether to check filter types or not
      LOGICAL HAVDV              ! Set if have variance component
      LOGICAL HAVFV              ! Set if have variance component (ff)
      LOGICAL HAVFV2             ! Set if data variance and an flatfield variance component
      LOGICAL OK                 ! Extension item obtained successfully
      LOGICAL PRESER             ! Set if the input type is to be preserved
      LOGICAL REMAP              ! Controls re-mapping of flatfield
      LOGICAL SETSAT             ! Set if have saturated values
      LOGICAL USESET             ! Are we using Sets?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup log file system, write introduction.
      CALL CCD1_START( 'FLATCOR', STATUS )

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an uninitialised group cannot cause trouble.
      GIDIN = GRP__NOID
      GIDOUT = GRP__NOID
      INGRP = GRP__NOID
      OUTGRP = GRP__NOID
      KEYGRP = GRP__NOID
      FLTGRP = GRP__NOID
      DO I = 1, CCD1__MXNDF
         SUBGRP( I ) = GRP__NOID
      END DO

*  See if the user wants to save disk space by deleting the input NDFs
*  when DEBIAS is finished with them. This will use the NDF_DELET
*  call which will delete container files if the NDF is associated with
*  the top-level object, otherwise the NDF itself will just be deleted.
*  In the latter case the space used by the NDF in the container file
*  will be released, the size of the file will probably not reduce.
      CALL PAR_GET0L( 'KEEPIN', DELETE, STATUS )
      DELETE = .NOT. DELETE

*  Access an NDG group containing a list of NDF names.
      CALL NDF_BEGIN
      CALL CCD1_NDFGL( 'IN', 1, CCD1__MXNDF, INGRP, NTOT, STATUS )

*  Find out if we are using Set header information.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Split the group of input NDFs up by Set Index if necessary.
      NSUB = 1
      IF ( USESET ) THEN
         CALL CCD1_SETSP( INGRP, 'INDEX', CCD1__MXNDF, SUBGRP, NSUB,
     :                    KEYGRP, STATUS )
      ELSE
         SUBGRP( 1 ) = INGRP
         KEYGRP = GRP__NOID
      END IF

*  Ask for a flatfield NDF, or a group of NDFs matching the Set Index
*  attributes that we have.
      IF ( USESET ) THEN
         CALL CCD1_NDFMI( 'FLAT', KEYGRP, FLTGRP, STATUS )
      ELSE
         CALL CCD1_NDFGL( 'FLAT', 1, 1, FLTGRP, IVAL, STATUS )
      END IF

*  Get a group of NDF names for the outputs.  Use the input names as
*  a modification list.
      CALL CCD1_NDFPG( 'OUT', INGRP, NTOT, OUTGRP, STATUS )

*  Loop over subgroups performing the calculations separately for each
*  one.
      DO ISUB = 1, NSUB

*  Write a header unless this is the only subgroup.
         IF ( NSUB .GT. 1 ) THEN
            CALL CCD1_SETHD( KEYGRP, ISUB, 'Applying flat correction',
     :                       'Index', STATUS )
         END IF

*  Set up the group of input NDFs for this subgroup.
         GIDIN = SUBGRP( ISUB )
         CALL GRP_GRPSZ( GIDIN, NNDF, STATUS )

*  Set up the group of output NDFs for this subgroup.
         CALL CCD1_ORDG( INGRP, OUTGRP, GIDIN, GIDOUT, STATUS )

*  Get the flat field NDF identifier for this subgroup.
         CALL NDG_NDFAS( FLTGRP, ISUB, 'READ', IDFLT, STATUS )

*  Check for the flatfield's variance.
         CALL NDF_STATE( IDFLT, 'Variance', HAVFV, STATUS )

*  Check that the type of the input NDF is recognised by this
*  application
         CALL CCD1_CKTYP( IDFLT, 1, 'MASTER_FLAT', STATUS )

*  Get the filter type of the calibration frame. It is OK for this not
*  to be present. If it is present then checks will be made to make sure
*  that the data to be flatfielded also have this filter.
         CALL CCG1_FCH0C( IDFLT, 'FILTER', FILTER, FCHECK, STATUS )
         IF ( .NOT. FCHECK ) FILTER = ' '

*  Get the bounds of the flatfield.
         CALL NDF_BOUND( IDFLT, 2, LBNDC, UBNDC, NDIM, STATUS )

*  Get the data type of the flatfield.
         CALL NDF_TYPE( IDFLT, 'Data,Variance', FTYPE, STATUS )

*  Check that it's valid. Must be floating point. If it's not then issue
*  a warning and set the type to _REAL.
         IF ( FTYPE .NE. '_REAL' .AND. FTYPE .NE. '_DOUBLE' ) THEN
            CALL MSG_SETC( 'FTYPE', FTYPE )
            CALL CCD1_MSG( ' ',
     :' Warning - flatfield does not have an appropriate type'//
     :' (^FTYPE). Data accessed using a type of _REAL', STATUS )
            FTYPE = '_REAL'
         END IF

*  Begin processing loop.
         DO 99999 INDEX = 1, NNDF

*  Get the input NDF identifier
            CALL NDG_NDFAS( GIDIN, INDEX, 'READ', IDIN, STATUS )

*  Check for presence of a variance component.
            CALL NDF_STATE( IDIN, 'Variance', HAVDV, STATUS )

*  If this input NDF has no variance component then switch off the
*  calibration frame variance if it exists.
            IF ( .NOT. HAVDV ) THEN
               HAVFV2 = .FALSE.
            ELSE
               HAVFV2 = HAVFV
            END IF

*  Write out name of this NDF.
            CALL CCD1_MSG( ' ',  ' ', STATUS )
            CALL NDF_MSG( 'CURRENT_NDF', IDIN )
            CALL CCD1_MSG( ' ', '  +++ Processing NDF: ^CURRENT_NDF',
     :                     STATUS )

*  Inform user how many NDFs we've processed out of the total number.
            CALL MSG_SETI( 'CURRENT_NUM', INDEX )
            CALL MSG_SETI( 'MAX_NUM', NNDF )
            CALL CCD1_MSG( ' ', '  (Number ^CURRENT_NUM of ^MAX_NUM)',
     :                     STATUS )

*  Check that this is the sort of frame we would like to correct in this
*  fashion. This means that it must have been debiassed and have a frame
*  type of TARGET.
            CALL CCD1_CKFLC( IDIN, STATUS )

*  Look for a filter type. Compare this with the type of the master
*  calibration frame.
            IF ( FCHECK ) THEN
               CALL CCG1_FCH0C( IDIN, 'FILTER', DFILT, OK, STATUS )
               IF ( .NOT. OK ) THEN

*  Failed to match the filter types.
                  CALL CCD1_MSG( ' ', ' Warning - failed to locate a '//
     :'filter type; cannot check against flatfield filter', STATUS )
               ELSE IF ( .NOT. CHR_SIMLR( FILTER, DFILT ) ) THEN

*  Filter specifications do not match. Could be a serious problem.
                  CALL MSG_SETC( 'DFILT', DFILT )
                  CALL MSG_SETC( 'FILTER', FILTER )
                  CALL CCD1_MSG( ' ', ' Warning - filter type'//
     :' (^DFILT) does not match flatfield (^FILTER)', STATUS )
               END IF
            END IF

*  Get the bounds of the input NDF.
            CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )

*  If the size of the input NDF has changed from the last NDF, then we
*  require a re-trimming of the calibration frame. Trim anyway if this
*  is the first loop.
            IF ( INDEX .EQ. 1 ) THEN
               REMAP = .TRUE.
            ELSE
               IF ( LBND( 1 ) .NE. LBNDL( 1 ) .OR.
     :              UBND( 1 ) .NE. UBNDL( 1 ) .OR.
     :              LBND( 2 ) .NE. LBNDL( 2 ) .OR.
     :              UBND( 2 ) .NE. UBNDL( 2 ) ) THEN
                  REMAP = .TRUE.
               ELSE

*  No change in bonds re-mapping may not be necessary.
                  REMAP = .FALSE.
               END IF
            END IF

*  Check the bounds of the flatfield against those of the
*  current input NDF. If the bounds are not the same as those of the
*  input NDF then trimming will occur.
            IF ( LBND( 1 ) .NE. LBNDC( 1 ) .OR.
     :           UBND( 1 ) .NE. UBNDC( 1 ) .OR.
     :           LBND( 2 ) .NE. LBNDC( 2 ) .OR.
     :           UBND( 2 ) .NE. UBNDC( 2 ) ) THEN
               REMAP = .TRUE.

*  Issue warning about this, in general it should be a mistake.
               CALL MSG_OUT( 'BAD_BOUNDS',
     :         ' Warning - FLATFIELD bounds do not match input NDF'
     :         , STATUS )
            END IF

*  Determine the type of the input NDF. It will be processed in this
*  type unless preserve is set true, in which case a floating point data
*  type will be used.
            CALL PAR_GET0L( 'PRESERVE', PRESER, STATUS )

*  Get the input data type.
            CALL NDF_TYPE( IDIN, 'Data,Variance', DTYPE, STATUS )

*  If not preserving then determine a suitable floating point
*  representation..
            IF ( .NOT. PRESER ) THEN
               IF ( ( DTYPE .EQ. '_UBYTE' )  .OR.
     :              ( DTYPE .EQ. '_BYTE'  )  .OR.
     :              ( DTYPE .EQ. '_WORD'  )  .OR.
     :              ( DTYPE .EQ. '_UWORD' )  .OR.
     :              ( DTYPE .EQ. '_REAL'  ) )    THEN

*  Single precision enough use this.
                  DTYPE = '_REAL'
               ELSE

*  Need a double precision representation (for _DOUBLE or _INTEGER)
*  _INT64 may well need _LDOUBLE
                  DTYPE = '_DOUBLE'
               END IF
            END IF

*  Re-mapping decisions complete, unmap the old flatfield section
*  and annul the identifier.
            IF ( REMAP ) THEN

*  If not the first loop unmap the calibration data.
               IF ( INDEX .NE. 1 ) THEN
                  CALL NDF_UNMAP( IDFTMP, '*', STATUS )
                  CALL NDF_ANNUL( IDFTMP, STATUS )
               END IF

*  Get a clone of the flatfield identifier, this ensures
*  that a valid copy of the input NDF identifier is always used.
*  NDF_MBND annuls the input identifiers and replaces them with the
*  section ones.
               CALL NDF_CLONE( IDFLT, IDFTMP, STATUS )

*  Trim the data to match bounds.
               CALL NDF_MBND( 'TRIM', IDIN, IDFTMP, STATUS )
            END IF

*  Merge the BAD pixel flags.
            CALL NDF_MBAD( .TRUE., IDIN, IDFTMP, 'Data,Variance',
     :                     .FALSE., BAD, STATUS )

*  Get a name for the output NDF. Propagate everything except the Data.
*  If the variance is only available for the input NDF then it is
*  propagated unchanged as a best estimate.
            CALL NDG_NDFPR( IDIN, 'Axis,Quality,Variance,WCS', GIDOUT,
     :                      INDEX, IDOUT, STATUS )

*  Make sure that output data is in the intended type.
            CALL NDF_STYPE( DTYPE, IDOUT, 'Data,Variance', STATUS )

*  Map in the flatfield data at the processing precision, if required.
*  Note using new strategy of mapping permanent data first, helps stop
*  virtual address space fragmentation.
            IF ( REMAP ) THEN
               CALL NDF_MAP( IDFTMP, 'Data', FTYPE, 'READ', IPDFLT, EL,
     :                       STATUS )
               IPVFLT = 0
               IF ( HAVFV2 ) CALL NDF_MAP( IDFTMP, 'Variance', FTYPE,
     :                                     'READ', IPVFLT, EL, STATUS )
            END IF

*  Map in all the data at the processing precision.(Volatile)
            CALL NDF_MAP( IDIN, 'Data', DTYPE, 'READ', IPDIN, EL,
     :                    STATUS )
            IPVIN = 0
            IF ( HAVDV ) CALL NDF_MAP( IDIN, 'Variance', DTYPE, 'READ',
     :                                 IPVIN, EL, STATUS )

*  Map in the outputs at this precision also. Note the output variance
*  is not propagated if does not exist. (Volatile)
            CALL NDF_MAP( IDOUT, 'Data', DTYPE, 'WRITE', IPDOUT, EL,
     :                    STATUS )
            IPVOUT = 0
            IF ( HAVDV ) CALL NDF_MAP( IDOUT, 'Variance', DTYPE,
     :                                 'WRITE', IPVOUT, EL, STATUS )

*  Look at the CCDPACK extension and see if a saturation _value_ has
*  been applied (instead of using BAD values). If it has then extract
*  the value.
            CALL CCG1_FCH0D( IDIN, 'SATVAL', SATVAL, SETSAT, STATUS )
            EXTSAT = SETSAT
            IF ( .NOT. SETSAT ) THEN

*  Find out if a saturation value has been applied by some other route.
               CALL PAR_GET0L( 'SETSAT', SETSAT, STATUS )

*  If saturation has been applied at what value?
               IF ( SETSAT ) THEN
                  CALL PAR_GET0D( 'SATURATION', SATVAL, STATUS )
               END IF
            END IF

*  Do the actual processing.
            CALL CCD1_FFCOR( FTYPE, DTYPE, BAD, EL, IPDIN, IPVIN,
     :                       IPDFLT, IPVFLT, HAVDV, HAVFV2, SETSAT,
     :                       SATVAL, IPDOUT, IPVOUT, STATUS )

*  Set BAD flag.
            CALL NDF_SBAD( BAD, IDOUT, 'Data,Variance', STATUS )

*  Output title.
            CALL NDF_CINP( 'TITLE', IDOUT, 'TITLE', STATUS )

*  Report this loop.
            CALL CCD1_RFCR( IDFTMP, SETSAT, SATVAL, EXTSAT, IDOUT,
     :                      FTYPE, DTYPE, STATUS )

*  Set extension item showing that FLATCOR has run.
            CALL CCD1_TOUCH( IDOUT, 'FLATCOR', STATUS )

*  Write terminator for Processing NDF: message.
            CALL CCD1_MSG( ' ', '  ---',STATUS )

*  Release the input NDF.
            CALL NDF_UNMAP( IDIN, '*', STATUS )
            CALL NDF_ANNUL( IDIN, STATUS )

*  Release output NDF.
            CALL NDF_UNMAP( IDOUT, '*', STATUS )
            CALL NDF_ANNUL( IDOUT, STATUS )

*  Set re-mapping flag to default.
            REMAP =.TRUE.

*  Store the present size of the NDFs.
            LBNDL( 1 ) = LBND( 1 )
            LBNDL( 2 ) = LBND( 2 )
            UBNDL( 1 ) = UBND( 1 )
            UBNDL( 2 ) = UBND( 2 )

*  Break out if status set BAD.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
99999    CONTINUE

      END DO

*  Delete the input NDFs if so requested.
      IF ( DELETE .AND. STATUS .EQ. SAI__OK ) THEN
         CALL CCD1_MSG( ' ', ' ', STATUS )
         CALL CCD1_MSG( ' ', '  *** Deleting input NDFs.', STATUS )
         DO I = 1, NTOT
            CALL CCD1_NGDEL( INGRP, I, .TRUE., STATUS )
         END DO
      END IF

 99   CONTINUE

*  Exit NDF context.
      CALL NDF_END( STATUS )

*  Release group identifiers.
      CALL CCD1_GRDEL( GIDIN, STATUS )
      CALL CCD1_GRDEL( GIDOUT, STATUS )
      CALL CCD1_GRDEL( INGRP, STATUS )
      CALL CCD1_GRDEL( OUTGRP, STATUS )
      CALL CCD1_GRDEL( KEYGRP, STATUS )
      CALL CCD1_GRDEL( FLTGRP, STATUS )
      DO I = 1, MIN( NSUB, CCD1__MXNDF )
         CALL CCD1_GRDEL( SUBGRP( I ), STATUS )
      END DO

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'FLATCOR_ERR',
     :   'FLATCOR: Error correcting with flatfield.',
     :   STATUS )
      END IF

*  Close down log file system and write terminator.
      CALL CCD1_END( STATUS )

      END
* $Id$
