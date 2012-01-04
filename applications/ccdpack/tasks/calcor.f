      SUBROUTINE CALCOR( STATUS )
*+
*  Name:
*     CALCOR

*  Purpose:
*     Subtracts a scaled dark or flash calibration image from a series of
*     images.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CALCOR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     CALCOR subtracts dark or flash calibration data from a series of
*     bias-corrected images. The calibration data are multiplied by a
*     constant before subtraction, so that calibration data which have
*     been normalised to counts per unit of time per pixel, can be
*     scaled to the "exposure" times suitable for correcting the input
*     data. If the calibration frame data levels are already correct
*     to perform the necessary correction then the data should be
*     scaled by a factor of one. In addition to subtracting the
*     calibration data CALCOR also processes saturated values
*     protecting them from modification. This protection is necessary
*     if the saturated pixels are not to become differentiated.

*  Usage:
*     calcor in out cal expose [preserve] [title]

*  ADAM Parameters:
*     CAL = LITERAL (Read)
*        Name of the image containing the calibration data, this would
*        normally be the output from MAKECAL. The data should be
*        normalised to one exposure unit. It is expected that the
*        calibration image contains dark or flash exposure CCD data
*        which have been bias corrected.
*
*        If USESET is true, CAL should be a group expression referring
*        to one calibration frame matching each of the Set Index
*        attributes represented in the IN list; again the name of
*        the file produced by MAKECAL will normally be suitable.
*
*        The name of this file may be specified using indirection
*        through a file.
*        [Global calibration image]
*     EXPOSE = LITERAL (Read)
*        A list of (comma separated) values specifying the numbers by
*        which the calibration data need to be multiplied before
*        subtraction from the input data. These are the "exposure"
*        factors for the dark counts expected in the input data or the
*        flash exposure times. If the calibration data have been
*        normalised to reflect the number of counts per second of time,
*        then this is the number of seconds of flash exposure or the
*        number of seconds duration between readouts, if it is a dark
*        counts image.  If the calibration image has been produced so
*        that the correct levels are already present, then these values
*        should be returned as one. A quick method of specifying that
*        all the images have the same "exposure" factors is to return a
*        single value, this will then be used for all input images.
*
*        The given values must be in the same order as the input images.
*        Indirection through an ASCII file may be used.  If more than
*        one line is required to enter the information then a
*        continuation line may be requested by adding "-" to the end of
*        the last value.
*     IN = LITERAL (Read)
*        Names of the images to be processed. The calibration data will be
*        scaled and subtracted from these.  The image names should be
*        separated by commas and may include wildcards.
*
*        NOTE the use of wildcards with this program is NOT recommended
*        unless the input images all have the same calibration exposure
*        factors. The processing order of any wildcarded images cannot
*        be guaranteed.
*     KEEPIN = _LOGICAL (Read)
*        Whether to keep (i.e. not delete) the input images (parameter IN)
*        or not. Deleting the input images has the advantage of saving
*        disk space, but should probably only be used if this program
*        is part of a sequence of commands and the intermediary data
*        produced by it are not important.
*
*        The calibration master frame (parameter CAL) is never deleted.
*
*        The default for this parameter is TRUE and this cannot be
*        overridden except by assignment on the command line or in
*        reponse to a forced prompt.
*        [TRUE]
*     LOGFILE = FILENAME (Read)
*        Name of the CCDPACK logfile.  If a null (!) value is given for
*        this parameter then no logfile will be written, regardless of
*        the value of the LOGTO parameter.
*
*        If the logging system has been initialised using CCDSETUP
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
*     OUT = LITERAL (Read)
*        Names of the output images. These may be specified as list of
*        comma separated names, using indirection if required, OR,
*        as a single modification element (of the input names).
*        The simplest modification element is the asterisk "*" which
*        means call each of the output images the same name as the
*        corresponding input images. So,
*           IN > *
*           OUT > *
*        signifies that all the images in the current directory should be
*        used and the output images should have the same names.
*
*        Other types of modification can also occur, such as,
*           OUT > tmp_*
*        which means call the output images the same as the input images but
*        put tmp_ in front of the names. Replacement of a specified
*        string with another in the output file names can also be used,
*           OUT > tmp_*|debias|flattened|
*        this replaces the string debias with flattened in any of the
*        output names tmp_*.
*
*        NOTE the use of wildcards with this program is not recommended
*        unless the input images all have the same calibration exposure
*        factors. The order of processing of any wildcarded images cannot
*        be guaranteed.
*     PRESERVE = _LOGICAL (Read)
*        If the input data type is to be preserved and used for
*        processing then this parameter should be set TRUE.
*        If this parameter is set FALSE then the input data will be
*        processed and returned in a suitable floating point
*        representation. This option is useful if the output data will
*        have a significant number of BAD values due to numeric errors
*        (over or under flow), or if unacceptable loss of precision
*        will occur if the data are processed in their initial data type
*        (due to rounding errors).
*
*        Note if a global value for this parameter has been set, using
*        CCDSETUP, then this will be used.
*        [TRUE]
*     SATURATION = _DOUBLE (Read)
*        The data saturation value, if it has been applied. See SETSAT.
*        [1.0D6]
*     SETSAT = _LOGICAL (Read)
*        If the input data have had a saturation value applied then
*        this parameter should be given as TRUE. If the input data
*        have been processed within CCDPACK then the saturation value
*        will have been stored within the CCDPACK extension, if this
*        is so then this value will be used. Note that data with
*        different saturation properties (i.e. values) which have not
*        been set within CCDPACK will require separate processing
*        (i.e. in groups with the same properties -- see notes).
*        [FALSE]
*     TITLE = LITERAL (Read)
*        Title for the output images.
*        [Output from CALCOR].
*     USESET = _LOGICAL (Read)
*        Whether to use Set header information or not.  If USESET is
*        false then any Set header information will be ignored.
*        If USESET is true, then the CAL parameter is taken to
*        refer to a group of files, and each IN file will be
*        processed using a calibration image with a Set Index
*        attribute which matches its own.  An IN file with no Set
*        header is considered to match a CAL file with no Set header,
*        so USESET can safely be set true  when the
*        input files contain no Set header information.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]

*  Examples:
*     calcor frame1 frame2 calibration 250
*        This example runs CALCOR in its most basic mode. The input data
*        in image frame1 has the data in image calibration subtracted, after
*        multiplying by 250. The resultant data is written to image
*        frame2. Note that if saturation values have been applied to the
*        data in frame1 within CCDPACK, then this will be handled
*        automatically. The output data will be of the same type as the
*        input data.
*
*     calcor in=^frames.dat out='*_darksub' cal=dark_master
*            expose=^dark_exposures
*        In this example a list of images are sequentially processed. The
*        list of image names is stored in the file frames.dat. The output
*        images are named after the corresponding input image with the
*        characters _darksub appended. The dark times for each input
*        frame are read from the file dark_exposures. This is the
*        recommended method for processing lists of input images.
*
*     calcor l1551_f11 l1551_f11_ds dark_master 1.0 preserve=false
*            logto=both logfile=l1551_darkcor.log
*            title=dark_corrected_data
*        This example follows a similar theme to the first example,
*        except that the output data type is now _REAL or _DOUBLE,
*        depending on the precision required to process the data. The
*        calibration correction data are assumed to have the right
*        exposure factor. The output image is given the title
*        "dark_corrected_data" and the parameters used by CALCOR are
*        stored in the logfile l1551_darkcor.log.
*
*     calcor in=ngc4151r_f1 cal=flash_master out=ngc4151r_f1_dc
*            expose=310.0 setsat saturation=32767
*        In this example a saturation value external to CCDPACK has
*        been applied to the input image. This is indicated by setting
*        SETSAT TRUE and by supplying the saturation value. Values
*        which are greater than or equal to the saturation value are
*        left unmodified by the calibration frame subtraction. This may
*        leave the saturated values "displaced" from the local values,
*        causing a discontinuity in the local isophotes, but is the
*        only method by which the saturated pixels may still be
*        readily identified after the subtraction of the calibration
*        frame.

*  Implementation Status:
*     - Supports processing of all non-complex numeric types.
*       BAD pixels are processed as are all NDF components.

*  Notes:
*     - If any of the input data have had their saturation values set
*       by applications not within CCDPACK, then this routine will
*       require the saturation value which has been used if the values
*       are to be propagated properly. If more than one saturation
*       value has been used then the input frames will need to be
*       processed singly. This is because CALCOR only uses one
*       saturation value per input group. If the saturation values
*       have been set within CCDPACK (by DEBIAS) these will be
*       processed correctly and may be different.

*  Behaviour of Parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*     The exceptions to this rule are:
*        - TITLE   -- always "Output from CALCOR"
*        - KEEPIN  -- always TRUE
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when using the application
*     on new datasets/different devices, or after a break of sometime.
*     The intrinsic default behaviour of the application may be
*     restored by using the RESET keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE, USESET, PRESERVE and CAL) have
*     global values. These global values will always take precedence,
*     except when an assignment is made on the command line.  In general
*     global values may be set and reset using the CCDSETUP and
*     CCDCLEAR commands, however, the CAL parameter may only be set by
*     a run of the application MAKECAL.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1997, 1999-2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     30-MAY-1991 (PDRAPER):
*        Original Version.
*     5-JAN-1994 (PDRAPER):
*        Automated CCDPACK extensions added.
*     2-FEB-1994 (PDRAPER):
*        Added ability to delete input NDFs.
*     19-JUL-1995 (PDRAPER):
*        Removed AIF_ calls.
*     26-FEB-1997 (PDRAPER):
*        Modified to use foreign data access IRG.
*     23-FEB-1999 (MBT):
*        Modified to propagate WCS component.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     14-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     4-JAN-2012 (DSB):
*        Store provenance info explicitly in each output NDF, rather than
*        relying on the provenance block within the monolith routine.
*        This is needed because the loop over output NDF is done within
*        this subroutine rather than within the monolith (as is done for
*        instance in KAPPA). This causes all input NDFs to be recorded as
*        ancestors of all output NDFs.
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
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      CHARACTER * ( CCD1__NMLEN ) FTYPE ! Expected frame type
      CHARACTER * ( NDF__SZTYP ) CTYPE ! Calibration data type
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Current data type
      DOUBLE PRECISION EFACIN( CCD1__MXNDF ) ! Exposure factors for all NDFs
      DOUBLE PRECISION EFACT( CCD1__MXNDF ) ! Exposure factors for subgroup
      DOUBLE PRECISION EXPOSE    ! Exposure factor (dark or flash).
      DOUBLE PRECISION SATVAL    ! Saturation value
      INTEGER ANCS( 2 )          ! Ancestor NDFs
      INTEGER CALGRP             ! GRP identifier for CAL NDFs
      INTEGER EL                 ! Number of elements in input array components
      INTEGER GIDIN              ! Group identifier for input NDFs
      INTEGER GIDOUT             ! Group identifier for output NDFs
      INTEGER I                  ! Loop index
      INTEGER IDCAL              ! Input NDF identifier (calibration)
      INTEGER IDCTMP             ! Current section of input NDF (calibration)
      INTEGER IDIN               ! Input NDF identifier
      INTEGER IDOUT              ! Output NDF identifier
      INTEGER INDEX              ! Counter for NDF processing loop
      INTEGER INGRP              ! NDG identifer for group of all IN NDFs
      INTEGER IPDCAL             ! Pointer to input Data component (cal)
      INTEGER IPDIN              ! Pointer to input Data component
      INTEGER IPDOUT             ! Pointer to output Data component
      INTEGER IPVCAL             ! Pointer to input Variance component (cal)
      INTEGER IPVIN              ! Pointer to input Variance component
      INTEGER IPVOUT             ! Pointer to output Data component
      INTEGER ISUB               ! Subgroup loop index
      INTEGER IVAL               ! Dummy
      INTEGER KEYGRP             ! GRP identifer for subgroup Index keys
      INTEGER LBND( 2 )          ! Lower of bounds of input NDF
      INTEGER LBNDC( 2 )         ! Lower bounds of calibration NDF.
      INTEGER LBNDL( 2 )         ! Lower bounds of last ndf
      INTEGER NDIM               ! Number of dimensions of input NDF
      INTEGER NNDF               ! Number of NDFs to process
      INTEGER NSUB               ! Number of subgroups
      INTEGER NTOT               ! Total number of IN NDFs
      INTEGER OUTGRP             ! GRP identifier for all output NDFs
      INTEGER SUBGRP( CCD1__MXNDF ) ! NDG identifer for NDFs in each subgroup
      INTEGER UBND( 2 )          ! Upper bounds of input NDF
      INTEGER UBNDC( 2 )         ! Upper bounds of calibration NDF.
      INTEGER UBNDL( 2 )         ! Upper bounds of last NDF
      LOGICAL BAD                ! Set if BAD values are present
      LOGICAL DELETE             ! Delete input NDFs
      LOGICAL EXTSAT             ! Saturation value from extension
      LOGICAL HAVCV              ! Set if have variance component (cal)
      LOGICAL HAVCV2             ! "" and data variance
      LOGICAL HAVDV              ! Set if have variance component
      LOGICAL PRESER             ! Set if the input type is to be preserved
      LOGICAL REMAP              ! Controls remapping of Calibration frame
      LOGICAL SETSAT             ! Set if have saturated values
      LOGICAL USEEXT             ! Use extension items
      LOGICAL USESET             ! Are we using Set headers?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup logging and write task introduction.
      CALL CCD1_START( 'CALCOR', STATUS )

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an uninitialised group cannot cause trouble.
      GIDIN = GRP__NOID
      GIDOUT = GRP__NOID
      INGRP = GRP__NOID
      OUTGRP = GRP__NOID
      KEYGRP = GRP__NOID
      CALGRP = GRP__NOID
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

*  See if we are to use the NDF extensions to look for suitable
*  exposure times.
      CALL PAR_GET0L( 'USEEXT', USEEXT, STATUS )

*  Get the frame type which is expected. This reflects whether the
*  input data are dark or flash frames, or if this isn't a useful
*  operation (default).
      CALL PAR_CHOIC( 'TYPE', 'NONE', 'NONE,DARK,FLASH', .TRUE., FTYPE,
     :                STATUS )

*  Access a group of NDF and exposure factors, which require processing.
*  If asked look for these values in the extensions of the input NDFs.
*  Do not allow this if the frame type is unknown.
      CALL NDF_BEGIN
      IF ( FTYPE .EQ. 'NONE' ) USEEXT = .FALSE.
      CALL CCD1_NDFAB( FTYPE, USEEXT, 'IN', CCD1__MXNDF, 'EXPOSE',
     :                 INGRP, EFACIN, NTOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Find out if we are using Set header information.
      CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Split the group of input NDFsby Set Index if necessary.
      NSUB = 1
      IF ( USESET ) THEN
         CALL CCD1_SETSP( INGRP, 'INDEX', CCD1__MXNDF, SUBGRP, NSUB,
     :                    KEYGRP, STATUS )
      ELSE
         SUBGRP( 1 ) = INGRP
         KEYGRP = GRP__NOID
      END IF

*  Ask for a calibration NDF, or a group of NDFs matching the Set Index
*  attributes we have.
      IF ( USESET ) THEN
         CALL CCD1_NDFMI( 'CAL', KEYGRP, CALGRP, STATUS )
      ELSE
         CALL CCD1_NDFGL( 'CAL', 1, 1, CALGRP, IVAL, STATUS )
      END IF

*  Get the names of the output NDFs. Use the input names as a
*  modification group for these.
      CALL CCD1_NDFPG( 'OUT', INGRP, NTOT, OUTGRP, STATUS )

*  Loop over subgroups performing calculations separately for each one.
      DO ISUB = 1, NSUB

*  Write a header unless this is the only subgroup.
         IF ( NSUB .GT. 1 ) THEN
            CALL CCD1_SETHD( KEYGRP, ISUB,
     :                       'Applying calibration correction', 'Index',
     :                       STATUS )
         END IF

*  Set up the group of input NDFs for this subgroup.
         GIDIN = SUBGRP( ISUB )
         CALL GRP_GRPSZ( GIDIN, NNDF, STATUS )

*  Set up the exposure factors for this subgroup.
         CALL CCG1_ORDD( INGRP, EFACIN, SUBGRP( ISUB ), EFACT, STATUS )

*  Set up the group of output NDFs for this subgroup.
         CALL CCD1_ORDG( INGRP, OUTGRP, GIDIN, GIDOUT, STATUS )

*  Get the calibration frame's NDF identifier for this subgroup.
         CALL NDG_NDFAS( CALGRP, ISUB, 'READ', IDCAL, STATUS )

*  Check for the calibration frame's variance.
         CALL NDF_STATE( IDCAL, 'Variance', HAVCV, STATUS )

*  Check the frame type of the calibration frame. This should be
*  MASTER_something, depending on the value of FTYPE. If FTYPE is none
*  just look for a MASTER_CAL type. If this is wrong this is not a
*  serious problem.
         IF ( FTYPE .EQ. 'DARK' ) THEN
            CALL CCD1_CKTYP( IDCAL, 1, 'MASTER_DARK', STATUS )
         ELSE IF ( FTYPE .EQ. 'FLASH' ) THEN
            CALL CCD1_CKTYP( IDCAL, 1, 'MASTER_FLASH', STATUS )
         ELSE
            CALL CCD1_CKTYP( IDCAL, 1, 'MASTER_CAL', STATUS )
         END IF

*  Get bounds of calibration NDF.
         CALL NDF_BOUND( IDCAL, 2, LBNDC, UBNDC, NDIM, STATUS )

*  Get the data type of the calibration frame.
         CALL NDF_TYPE( IDCAL, 'Data,Variance', CTYPE, STATUS )

*  Begin processing loop.
         DO 99999 INDEX = 1, NNDF

*  Get the input NDF identifier
            CALL NDG_NDFAS( GIDIN, INDEX, 'READ', IDIN, STATUS )

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

*  Check that this is an NDF which is ok for correcting in this fashion.
*  This means it shouldn't be a BIAS or MASTER of any kind and should
*  have been debiassed.
            CALL CCD1_CKCCL( IDIN, STATUS )

*  And the associated exposure factor.
            EXPOSE = EFACT( ISUB )

*  Check for presence of a variance component.
            CALL NDF_STATE( IDIN, 'Variance', HAVDV, STATUS )

*  If no input variance is present them cannot process any variances
*  set the calibration variance flag.
            IF ( .NOT. HAVDV ) THEN
               HAVCV2 = .FALSE.
            ELSE
               HAVCV2 = HAVCV
            END IF

*  Get the bounds of the current NDF.
            CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )

*  If the size of the input NDF has changed from the last NDF, then we
*  require a retrimming of the calibration frame. Trim any way if this
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

*  No change in bonds remapping may not be necessary.
                  REMAP = .FALSE.
               END IF
            END IF

*  Check the bounds of the CALIBRATION frame against those of the
*  current input NDF. If the bounds are not the same as those of the
*  input NDF then trimming will occur.
            IF ( LBND( 1 ) .NE. LBNDC( 1 ) .OR.
     :           UBND( 1 ) .NE. UBNDC( 1 ) .OR.
     :           LBND( 2 ) .NE. LBNDC( 2 ) .OR.
     :           UBND( 2 ) .NE. UBNDC( 2 ) ) THEN
               REMAP = .TRUE.

*  Issue warning about this, in general it should be a mistake.
               CALL MSG_OUT( 'BAD_BOUNDS',
     :         ' Warning - bounds of NDFs do not match'
     :         , STATUS )
            END IF

*  Sort out data typing. If the user wants to preserve the input data
*  types on exit then no action is required CALCOR can process any
*  combination of calibration and input data types. If the user does not
*  want to preserve the input data type (say because many numeric
*  errors are expected in the current precision) then select a minimum
*  floating data type.
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
                  DTYPE = '_DOUBLE'
               END IF
            END IF

*  By this stage remapping decisions complete, unmap the old
*  calibration frame section and annul the identifier.
            IF ( REMAP ) THEN

*  If not the first loop unmap the calibration data.
               IF ( INDEX .NE. 1 ) THEN
                  CALL NDF_UNMAP( IDCTMP, '*', STATUS )
                  CALL NDF_ANNUL( IDCTMP, STATUS )
               END IF

*  Get a clone of the input calibration frame identifier, this ensures
*  that a valid copy of the input NDF identifier is always used.
*  NDF_MBND annuls the input identifiers and replaces them with the
*  section ones.
               CALL NDF_CLONE( IDCAL, IDCTMP, STATUS )

*  Trim the data to match bounds.
               CALL NDF_MBND( 'TRIM', IDIN, IDCTMP, STATUS )
            END IF

*  Merge the BAD pixel flags.
            CALL NDF_MBAD( .TRUE., IDIN, IDCTMP, 'Data,Variance',
     :                     .FALSE., BAD, STATUS )

*  Get the output NDF. Propagate everything except the Data.
*  The variance will be unchanged if the calibration frame has none.
            CALL NDG_NDFPR( IDIN, 'Axis,Quality,Variance,WCS', GIDOUT,
     :                      INDEX, IDOUT, STATUS )

*  Make sure that data is in the intended output form
            CALL NDF_STYPE( DTYPE, IDOUT, 'Data,Variance', STATUS )

*  Map in the calibration data if input data size has changed.
*  New stratedgy -- map in (possible) permanent NDFs first may decrease
*  virtual space fragmentation. (Mapping/remapping of large contiguous
*  addresses.)
            IF ( REMAP ) THEN
               CALL NDF_MAP( IDCTMP, 'Data', CTYPE, 'READ', IPDCAL, EL,
     :                       STATUS )
               IPVCAL = 0
               IF ( HAVCV2 ) CALL NDF_MAP( IDCTMP, 'Variance', CTYPE,
     :                                     'READ', IPVCAL, EL, STATUS )
            END IF

*  Map in the input data. (Volatile)
            CALL NDF_MAP( IDIN, 'Data', DTYPE, 'READ', IPDIN, EL,
     :                    STATUS )
            IPVIN = 0
            IF ( HAVDV ) CALL NDF_MAP( IDIN, 'Variance', DTYPE, 'READ',
     :                                 IPVIN, EL, STATUS )

*  Map in the outputs. Note the output variance is not propagated if
*  does not exist. (Volatile)
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
            IF ( .NOT. EXTSAT ) THEN

*  Find out if a saturation value has been applied by some other route.
               CALL PAR_GET0L( 'SETSAT', SETSAT, STATUS )

*  If saturation has been applied at what value?
               IF ( SETSAT ) THEN
                  CALL PAR_GET0D( 'SATURATION', SATVAL, STATUS )
               END IF
            END IF

*  Do the actual processing.
            CALL CCD1_CLCOR( CTYPE, DTYPE, BAD, EL, IPDIN, IPVIN,
     :                       IPDCAL, IPVCAL, HAVDV, HAVCV2, EXPOSE,
     :                       SETSAT, SATVAL, IPDOUT, IPVOUT, STATUS )

*  Set the output type if required.
            CALL NDF_UNMAP( IDOUT, '*', STATUS )

*  Set BAD flag.
            CALL NDF_SBAD( BAD, IDOUT, 'Data,Variance', STATUS )

*  Output title.
            CALL NDF_CINP( 'TITLE', IDOUT, 'TITLE', STATUS )

*  Report this NDF processing.
            CALL CCD1_RCCR( FTYPE, IDCAL, EXPOSE, USEEXT, SETSAT,
     :                      SATVAL, EXTSAT, IDOUT, PRESER, CTYPE,
     :                      DTYPE, STATUS )

*  Touch the NDF leaving an audit-like trail.
            IF ( FTYPE .EQ. 'DARK' ) THEN
               CALL CCD1_TOUCH( IDOUT, 'DARKCOR', STATUS )
            ELSE IF ( FTYPE .EQ. 'FLASH' ) THEN
               CALL CCD1_TOUCH( IDOUT, 'FLASHCOR', STATUS )
            END IF
            CALL CCD1_TOUCH( IDOUT, 'CALCOR', STATUS )

*  We cannot rely on the NDG provenance block established in the CCDPACK
*  monolith routine, since it would make all output NDFs depend on all
*  input NDFs. Instead, we store provenance info in the output NDF
*  explicitly, by copying provenance info from just the current input NDF
*  and the calibration frame to the output NDF. When the provenance block
*  established in the monolith routine ends, the presence of this provenance
*  in the output NDF will prevent any further provenance being added to the
*  output NDF.
            ANCS( 1 ) = IDIN
            ANCS( 2 ) = IDCAL
            CALL NDG_ADDPROV( IDOUT, 'CCDPACK:CALCOR', 2, ANCS, STATUS )

*  Write terminator for Processing NDF: message.
            CALL CCD1_MSG( ' ', '  ---',STATUS )

*  Release the NDF.
            CALL NDF_UNMAP( IDIN, '*', STATUS )
            CALL NDF_ANNUL( IDIN, STATUS )

*  Release the output NDF.
            CALL NDF_ANNUL( IDOUT, STATUS )

*  Reset the mapping control flag.
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

*  Break out here if status set BAD.
 99   CONTINUE

*  Exit NDF context.
      CALL NDF_END( STATUS )

*  Release group resources.
      CALL CCD1_GRDEL( GIDIN, STATUS )
      CALL CCD1_GRDEL( GIDOUT, STATUS )
      CALL CCD1_GRDEL( INGRP, STATUS )
      CALL CCD1_GRDEL( OUTGRP, STATUS )
      CALL CCD1_GRDEL( KEYGRP, STATUS )
      CALL CCD1_GRDEL( CALGRP, STATUS )
      DO I = 1, MIN( NSUB, CCD1__MXNDF )
         CALL CCD1_GRDEL( SUBGRP( I ), STATUS )
      END DO

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'CALCOR_ERR',
     :   'CALCOR: Error correcting with calibration frame.',
     :   STATUS )
      END IF

*  Close down log system and write terminator.
      CALL CCD1_END( STATUS )

      END
* $Id$
