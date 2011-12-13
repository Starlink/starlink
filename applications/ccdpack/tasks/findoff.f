      SUBROUTINE FINDOFF( STATUS )
*+
*  Name:
*     FINDOFF

*  Purpose:
*     Performs pattern-matching between position lists related by
*     simple offsets.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FINDOFF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine is designed to determine which positions in many
*     unaligned and unlabelled lists match, subject to the condition
*     that the transformations between the lists are well modelled by
*     simple translations.  Although the position lists are written
*     in pixel coordinates, the objects can be related by translations
*     in the Current coordinate system of the associated NDFs.
*
*     The results from this routine are labelled position lists (one
*     for each input list) which may be used to complete image
*     registration using the REGISTER routine. The estimated offsets are
*     reported, but REGISTER should be used to get accurate values.

*  Usage:
*     findoff inlist error outlist

*  ADAM Parameters:
*     COMPLETE = _DOUBLE (Read)
*        A completeness threshold for rejecting matched position
*        list pairs. A completeness factor is estimated by counting the
*        number of objects in the overlap region of two lists, taking
*        the minimum of these two values (this adjusts for
*        incompleteness due to a different object detection threshold)
*        and comparing this with the number of objects actually
*        matched. Ideally a completeness of 1 should be found, the lower
*        this value the lower the quality of the match.
*        [0.5]
*     ERROR = _DOUBLE (Read)
*        The error, in pixels, in the X and Y positions. This value is
*        used to determine which positions match within an error box
*        (SLOW) or as a bin size (FAST). An inaccurate value may result
*        in excessive false or null matches.
*        [1.0]
*     FAILSAFE = _LOGICAL (Read)
*        If FAST is TRUE then this parameter indicates whether the SLOW
*        algorithm is to be used when FAST fails.
*        [TRUE]
*     FAST = _LOGICAL (Read)
*        If TRUE then the FAST matching algorithm is used, otherwise
*        just the SLOW algorithm is used.
*        [TRUE]
*     INLIST = LITERAL (Read)
*        This parameter is used to access the names of the lists
*        which contain the positions and, if NDFNAMES is TRUE, the names
*        of the associated NDFs. If NDFNAMES is TRUE the names of the
*        position lists are assumed to be stored in the extension of the
*        NDFs (in the CCDPACK extension item CURRENT_LIST) and the names
*        of the NDFs themselves should be given in response (and may
*        include wildcards).
*
*        If NDFNAMES is FALSE then the actual names of the position
*        lists should be given. These may not use wildcards but may be
*        specified using indirection (other CCDPACK position list
*        processing routines will write the names of their results file
*        into files suitable for use in this manner) the indirection
*        character is "^".
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
*     MAXDISP = _DOUBLE (Read)
*        This parameter gives the maximum acceptable displacement
*        (in pixels) between the original alignment of the NDFs and the
*        alignment in which the objects are matched.  If frames have
*        to be displaced more than this value to obtain a match, the
*        match is rejected.  This will be of use when USEWCS is set
*        and the NDFs are already fairly well aligned in their Current
*        coordinate systems.  It should be set to the maximum
*        expected inaccuracy in that alignment.  If null, arbitrarily
*        large displacements are allowed, although note that a
*        similar restriction is effectively imposed by setting the
*        RESTRICT parameter.
*        [!]
*     MINMATCH = _INTEGER (Read)
*        This parameter specifies the minimum number of positions
*        which must be matched for a comparison of two lists to be
*        deemed successful.  Small values (especially less than 3) of
*        this parameter can lead to a high probability of false matches,
*        and are only advisable for very sparsely populated lists
*        and/or small values of the MAXDISP parameter (presumably in
*        conjunction with USEWCS).
*        [3]
*     MINSEP = _DOUBLE (Read)
*        Positions which are very close may cause false matches by being
*        within the error box of other positions. The value of this
*        parameter controls how close (in pixels) objects may be before
*        they are both rejected (this occurs before pattern-matching).
*        [Dynamic -- 5.0*ERROR]
*     NAMELIST = LITERAL (Read)
*        The name of a file to contain the names of the output
*        position lists. The names written to this file are those
*        generated using the expression given to the OUTLIST parameter.
*        This file may be used in an indirection expression to input
*        all the position lists output from this routine into another
*        routine (say REGISTER), if the associating position lists with
*        NDFs option is not being used.
*        [FINDOFF.LIS]
*     NDFNAMES = _LOGICAL (Read)
*        If TRUE then the routine will assume that the names of the
*        position lists are stored in the NDF CCDPACK extensions under
*        the item "CURRENT_LIST". The names will be present in the
*        extension if the positions were located using a CCDPACK
*        application (such as FINDOBJ). Using this facility allows the
*        transparent propagation of position lists through processing
*        chains.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [TRUE]
*     OUTLIST = FILENAME (Write)
*        A list of names specifying the result files. These contain
*        labelled positions which can be used in registration.
*        The names of the lists may use modifications of the
*        input names (NDF names if available otherwise the names of the
*        position lists). So if you want to call the output lists
*        the same name as the input NDFs except to add a type use.
*
*           OUTLIST > *.find
*
*        If no NDF names are given (NDFNAMES is FALSE) then if you want
*        to change the extension of the files (from ".find" to ".off"
*        in this case) use
*
*           OUTLIST > *|find|off|
*
*        Or alternatively you can use an explicit list of names.
*        These may use indirection elements as well as names separated
*        by commas.
*     OVERRIDE = _LOGICAL (Read)
*        This parameter controls whether to continue and create an
*        incomplete solution. Such solutions will result when only a
*        subset of the input position lists have been matched.
*        If the associating position lists with NDFs option has
*        been chosen, an position list will still be written for
*        each input NDF, but for NDFs which were not matched the
*        output list will be empty (will consist only of comment lines).
*
*        Incomplete matching would ideally indicate that one, or more,
*        of the input lists are from positions not coincident with the
*        others, in which case it is perfectly legimate to proceed.
*        However, it is equally possible that they have too few positions
*        and have consequently been rejected.
*        [TRUE]
*     RESTRICT = _LOGICAL (Read)
*        This parameter determines whether the Current coordinate system
*        is used to restrict the choice of objects to match with each
*        other.  If set TRUE, then the only objects which are
*        considered for matching are those which would appear in
*        the overlap of two frames given that they are correctly
*        aligned in their Current coordinate system.  If it is set
*        FALSE, then all objects in both frames are considered for
*        matching.
*
*        This parameter should therefore be set TRUE if the frames
*        are quite well aligned in their Current coordinate systems
*        (especially in the case that there are many objects and a
*        small overlap), and FALSE if they are not.
*
*        This parameter is ignored if USEWCS is FALSE.
*        [FALSE]
*     USECOMP = _LOGICAL (Read)
*        This parameter specifies whether the completeness value will
*        be used to weight the number of matches between a pair, when
*        determining the graph connecting all input datasets. Using
*        a completeness weight increases the chance of selecting high
*        quality matches, but may reduce the chance of selecting matches
*        with the highest counts in favour of those with lower counts.
*        [TRUE]
*     USESET = _LOGICAL (Read)
*        This parameter determines whether Set header information should
*        be used in the object matching.  If USESET is true,
*        FINDOFF will try to group position lists according to
*        the Set Name attribute of the NDF to which they are attached.
*        All lists coming from NDFs which share the same (non-blank)
*        Set Name attribute, and which have a CCD_SET coordinate frame
*        in their WCS component, will be grouped together and treated
*        by the program as a single position list.  Thus no attempt
*        is made to match objects between members of the same Set;
*        it is assumed that the relative alignment within a Set
*        is already known and has been fixed.
*
*        If USESET is false, all Set header information is ignored.
*        If NDFNAMES is false, USESET will be ignored.  If the input
*        NDFs have no Set headers, or if they have no CCD_SET frame
*        in their WCS components, the setting of USESET will make
*        no difference.
*
*        If a global value for this parameter has been set using
*        CCDSETUP then that value will be used.
*        [FALSE]
*     USEWCS = _LOGICAL (Read)
*        This parameter specifies whether the coordinates in the
*        position lists should be transformed from Pixel coordinates
*        into the Current coordinate system of the associated NDF
*        before use.
*        If the Current coordinates are related to pixel coordinates
*        by a translation, the setting of this parameter is usually
*        unimportant (but see also the RESTRICT parameter).
*
*        This parameter is ignored if NDFNAMES is false.
*        [TRUE]

*  Examples:
*     findoff inlist='*' error=1 outlist='*.off'
*        In this example all the NDFs in the current directory are
*        accessed and their associated position lists are used.
*        The NDFs are related by a simple offset (translation) in
*        their Current coordinate system, although not necessarily
*        their pixel coordinate system.  The matched position lists are
*        named *.off.  The method used is to try the FAST algorithm,
*        switching to SLOW if FAST fails. The completeness measure
*        is used when forming the spanning tree.  Matches with
*        completenesses less than 0.5 and or with less than three
*        positions, are rejected.
*
*     findoff fast nofailsafe
*        In this example the only the FAST algorithm is used.
*
*     findoff usecomp=false
*        In this example the completeness factor is derived but not used
*        to weight the edges of the spanning tree.
*
*     findoff error=8 minsep=100
*        In this example very fuzzy measurements (or small pixels) are
*        being used.  The intrinsic error in the measurements is around
*        8 pixels and positions within a box 100 pixels of each other
*        are rejected.
*
*     findoff inlist='data*' outlist='*.off' restrict=true
*        This form would be used if the NDFs 'data*' are already
*        approximately aligned in their Current coordinates. Setting the
*        RESTRICT parameter then tells FINDOFF to consider only objects
*        in the region which overlaps in the Current coordinates of
*        each pair of frames. This can save a lot of time if there
*        are many objects and a small overlap, but will result in
*        failure of the program if the NDFs are not translationally
*        aligned reasonably well in the first place.
*
*     findoff inlist='data*' outlist='*.off' restrict minmatch=2
*             maxdisp=20 minsep=30
*        In this example the NDFs are sparsely populated, and a pair
*        will be considered to match if as few as two matching objects
*        can be found.  The NDFs have been initially aligned in their
*        Current coordinate systems to an accuracy of 20 or better.  As
*        an additional safeguard, no objects within 30 units (in
*        coordinates of the Current frame) of each other in the same NDF
*        are used for matching.

*  Notes:
*     - Position list formats.
*
*       CCDPACK supports data in two formats.
*
*       CCDPACK format - the first three columns are interpreted as the
*       following.
*
*          - Column 1: an integer identifier
*          - Column 2: the X position
*          - Column 3: the Y position
*
*       The column one value must be an integer and is used to identify
*       positions.  In the output position lists from one run of FINDOFF,
*       lines with the same column-1 value in different files represent
*       the same object.  In the input position lists column-1 values
*       are ignored.  If additional columns are present they must be
*       numeric, and there must be the same number of them in every
*       line.  These have no effect on the calculations, but FINDOFF
*       will propagate them to the corresponding lines in the output
*       list.
*
*       EXTERNAL format - positions are specified using just an X
*       and a Y entry and no other entries.
*
*          - Column 1: the X position
*          - Column 2: the Y position
*
*       This format is used by KAPPA applications such as CURSOR.
*
*       Comments may be included in a file using the characters "#" and
*       "!". Columns may be separated by the use of commas or spaces.
*
*       In all cases, the coordinates in position lists are pixel
*       coordinates.
*
*     - NDF extension items.
*
*       If NDFNAMEs is TRUE then the names of the input position lists
*       will be gotten from the item "CURRENT_LIST" of the CCDPACK
*       extension of the input NDFs. On exit this item will be updated
*       to contain the name of the appropriate output lists.

*  Behaviour of Parameters:
*     Most parameters retain their current value as default. The
*     "current" value is the value assigned on the last run of the
*     application. If the application has not been run then the
*     "intrinsic" defaults, as shown in the parameter help, apply.
*
*     Retaining parameter values has the advantage of allowing you to
*     define the default behaviour of the application but does mean
*     that additional care needs to be taken when re-using the
*     application after a break of sometime. The intrinsic default
*     behaviour of the application may be restored by using the RESET
*     keyword on the command line.
*
*     Certain parameters (LOGTO, LOGFILE, NDFNAMES and USESET) have global
*     values. These global values will always take precedence, except
*     when an assignment is made on the command line.  Global values may
*     be set and reset using the CCDSETUP and CCDCLEAR commands.

*  Notes On Algorithms:
*     The pattern-matching process uses two main algorithms, one which
*     matches all the point pair-offsets between any two input lists,
*     looking for the matches with the most common positions, and one
*     which uses a statistical method based on a histogram of the
*     differences in the offsets (where the peak in the histogram is
*     assumed the most likely difference). In each case an estimate of
*     the positional error must be given as it is used when deciding
*     which positions match (given an offset) or is used as the bin
*     size when forming histograms.
*
*     Which algorithm you should use depends on the number of points
*     your position lists contain and the expected size of the overlaps
*     between the datasets. Obviously it is much easier to detect two
*     lists with most of their positions in common. With small overlaps
*     a serious concern is the likelihood of finding a `false' match.
*     False matches must be more likely the larger the datasets and the
*     smaller the overlap.
*
*     The first algorithm (referred to as SLOW) is more careful and is
*     capable of selecting out positions when small overlaps in the
*     data are present (although a level of false detections will
*     always be present) but the process is inherently slow (scaling as
*     n**3log2(n)).  The second algorithm (referred to as FAST) is an
*     n*n process so is much quicker, but requires much better
*     overlapping.
*
*     Because the FAST process takes so little CPU time it is better to
*     try this first (without the SLOW process as a backup), only use
*     the SLOW algorithm when you have small datasets and do not
*     expect large areas (numbers of positions) of overlap.
*
*     A third algorithm, referred to as SNGL, is used automatically if
*     one or both of the lists in a pair contains only a single object.
*     In this case object matching is trivial and, of course, may
*     easily be in error.  SNGL can only be used if the MINMATCH
*     parameter has been set to 1, which should be done with care.  The
*     SNGL algorithm may be useful if there really is only one object,
*     correctly identified, in all the frames.  If this is not the
*     case, it should only be used when USEWCS is true and MAXDISP is
*     set to a low value, indicating that the alignment of the NDFs in
*     their Current coordinate systems is already fairly accurate.
*
*     The global registration process works by forming a graph with
*     each position list at a node and with connecting edges of weight
*     the number of matched position-pairs. The edge weights may be
*     modified by a completeness factor which attempts to assess the
*     quality of the match (this is based on the ratio of the expected
*     number of matches in the overlap region to the actual number,
*     random matches shouldn't return good statistics when compared
*     with genuine ones). This still leaves a possibility of false
*     matches disrupting any attempt to register the datasets so a
*     single "spanning tree" is chosen (this is a graph which just
*     visits each node the minimum number of times required to get
*     complete connectivity, no loops allowed) which has the highest
*     possible number of matched positions (rejecting edges with few
*     matched positions/low completenesses where possible). This gives
*     a most likely solution to the offsets between the position lists,
*     rather than the "best" solution which could well include false
*     matches; compare this solution with a median as opposed to a
*     mean. The final registration is then used to identify all the
*     objects which are the same in all datasets (using a relaxation
*     method), resulting in labelled position lists which are output
*     for use by REGISTER.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1995-2002 Central Laboratory of the Research
*     Copyright (C) 2008 Science and Technology Facilities Council
*     Councils. All Rights Reserved.

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
*     29-SEP-1992 (PDRAPER):
*        Original version.
*     12-MAY-1993 (PDRAPER):
*        Added new techniques for SLOW.
*     6-OCT-1995 (PDRAPER):
*        Updated for CCDPACK 2.0.
*     16-SEP-1996 (PDRAPER):
*        Removed all NAG calls.
*     3-MAR-1997 (PDRAPER):
*        Removed top-level locator control (foreign data access upgrade).
*     23-MAR-1998 (PDRAPER):
*        Changed to open input formatted files as required (rather than
*        all at once). This works around the FIO limit of 40 open files
*        and achieves the CCDPACK limit of 100 instead.
*     16-DEC-1998 (PDRAPER):
*        Added OVERRIDE parameter to control the behaviour when
*        only some of the datasets are paired.
*     20-JAN-1999 (PDRAPER):
*        Sorted out workspace problems for incomplete matching
*        (IPBEEN and IPQUE increased to NOPEN from NMATCH).
*     30-MAR-1999 (MBT):
*        Changed to deal with WCS components of NDFs.
*     26-APR-1999 (MBT):
*        Now erases .MORE.CCPACK.CURRENT_LIST component for unmatched
*        NDFs.
*     20-MAY-1999 (MBT):
*        Major changes to fix major misconceptions about the best way
*        for it to work.
*     28-OCT-1999 (MBT):
*        Modified so that ERROR and MAXDISP are in units of pixels (not
*        current coordinates).
*     1-NOV-1999 (MBT):
*        Modified so that output is in units appropriate to current
*        coordinate frame.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     23-JAN-2001 (MBT):
*        Fixed a bug in place probably since MAR-1999 which altogether
*        prevented FINDOFF working when NDFNAMES=false (failure to
*        initialise PSIZE).
*     25-JAN-2001 (MBT):
*        Fixed an old bug which caused good positions to be discarded
*        when USECOMP=TRUE.
*     30-JAN-2001 (MBT):
*        Modified to propagate columns beyond column 3 of input position
*        lists to the output lists.
*     20-FEB-2001 (MBT):
*        Upgraded for use with Sets.
*     18-MAY-2001 (MBT):
*        Fixed the propagation of extra columns - it was failing to do
*        more than one of them properly.
*     22-MAY-2001 (MBT):
*        Changed to write an empty list file rather than no list file
*        at all.
*     11-FEB-2002 (MBT):
*        Modified a call to CCD1_GMMP to take care of an uninitialised
*        array bug when OVERRIDE is true.
*     8-FEB-2008 (PWD):
*        Initialise NSUP so that related resources are not accidently
*        released when exiting in error.
*     11-MAR-2008 (PWD):
*        Don't release memory allocated for output lists. That's done
*        automatically. Releasing that memory (some of which was already
*        freed), had a side effect of releasing memory allocated
*        elsewhere on rare occasions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PAR_ERR'          ! PAR error codes
      INCLUDE 'GRP_PAR'          ! Standard GRP system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of character string
      EXTERNAL CHR_LEN

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Line buffer for reading in data
      CHARACTER * ( CCD1__BLEN ) LINE1 ! Line buffer for writing out text
      CHARACTER * ( CCD1__BLEN ) LINE2 ! Line buffer for writing out text
      CHARACTER * ( DAT__SZLOC ) LOCEXT ! HDS locator for CCDPACK extension
      CHARACTER * ( FIO__SZFNM ) FNAME ! Buffer to store filenames
      CHARACTER * ( 4 ) METHOD  ! Last method attempted for object matching
      DOUBLE PRECISION BNDX( 4, CCD1__MXLIS ) ! X coords of bounding boxes
      DOUBLE PRECISION BNDY( 4, CCD1__MXLIS ) ! Y coords of bounding boxes
      DOUBLE PRECISION XP( 4 )  ! Pixel domain dummy point X coordinates
      DOUBLE PRECISION YP( 4 )  ! Pixel domain dummy point Y coordinates
      DOUBLE PRECISION COMFAC   ! Completeness factor
      DOUBLE PRECISION ERROR    ! Error in input positions
      DOUBLE PRECISION MINSEP   ! Minimum input data separation
      DOUBLE PRECISION MAXDIS   ! Maximum displacement for matching
      DOUBLE PRECISION NEDFAC   ! Minimum completeness factor required
      DOUBLE PRECISION PIXSIZ   ! Linear size of a pixel for this pair
      DOUBLE PRECISION PSIZE( CCD1__MXLIS ) ! Linear size of a pixel
      DOUBLE PRECISION TOLS( CCD1__MXLIS ) ! Deduplication tolerances
      DOUBLE PRECISION WEIGHT( CCD1__MXLIC ) ! Weights of matched positions
      DOUBLE PRECISION XOFF( CCD1__MXLIC ) ! Determined X translation
      DOUBLE PRECISION XOFFN( CCD1__MXLIS ) ! Final X translation
      DOUBLE PRECISION YOFF( CCD1__MXLIC ) ! Determined Y translation
      DOUBLE PRECISION YOFFN( CCD1__MXLIS ) ! Final Y translation
      INTEGER COUNT             ! Dummy loop counter
      INTEGER DUMMY             ! Dummy value
      INTEGER FDIN              ! Input FIO descriptor
      INTEGER FDOUT             ! Output FIO descriptor
      INTEGER FIOGR             ! Input group identifier
      INTEGER FRM( CCD1__MXLIS ) ! AST pointers to Current frames by superlist
      INTEGER FRMS( CCD1__MXLIS ) ! AST pointers to Current coordinate frames
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in CHR_ string
      INTEGER IDIN              ! NDF identifier
      INTEGER ILIS( CCD1__MXLIS ) ! Array of list indices in superlist order
      INTEGER ILISOF( CCD1__MXLIS + 1 ) ! Offsets into ILIS for each superlist
      INTEGER IOFF              ! Offset into superlist
      INTEGER IPBEEN            ! Pointer to workspace
      INTEGER IPDAT             ! Pointer to input data
      INTEGER IPGOOD            ! Pointer to bounds inclusion logical array
      INTEGER IPGRA             ! Pointer to graph
      INTEGER IPID( CCD1__MXLIS ) ! Pointer to Output X positions
      INTEGER IPIND             ! Pointer to input indices in file 1
      INTEGER IPLID             ! Pointer to matched IDs in list
      INTEGER IPLRAN            ! Pointer to matched ranks in list
      INTEGER IPLX              ! Pointer to matched X coords in list
      INTEGER IPLY              ! Pointer to matched Y coords in list
      INTEGER IPOS              ! Global index of next position encountered
      INTEGER IPQUE             ! Pointer to workspace
      INTEGER IPRAN( CCD1__MXLIS ) ! Pointer to sort ranks
      INTEGER IPRAN1            ! Pointer to sort ranks
      INTEGER IPRAN2            ! Pointer to sort ranks
      INTEGER IPRBN1            ! Intermediate pointer to sort ranks
      INTEGER IPRBN2            ! Intermediate pointer to sort ranks
      INTEGER IPRCN1( CCD1__MXLIC ) ! Pointer to sort ranks per edge
      INTEGER IPRCN2( CCD1__MXLIC ) ! Pointer to sort ranks per edge
      INTEGER IPRDN             ! Pointer to sort ranks
      INTEGER IPSEQ( CCD1__MXLIS ) ! Pointer to list of global point indices
      INTEGER IPSPAN            ! Pointer to graph (spanning)
      INTEGER IPSUB             ! Pointer to sub-graph (spanning)
      INTEGER IPWRK1            ! Workspace pointers
      INTEGER IPWRK2            ! Workspace pointers
      INTEGER IPWRK3            ! Workspace pointers
      INTEGER IPWRK4            ! Workspace pointers
      INTEGER IPWRK5            ! Workspace pointers
      INTEGER IPX( CCD1__MXLIS ) ! Pointer to out/input X positions
      INTEGER IPXDAT( CCD1__MXLIS ) ! Pointer to extra data from lists
      INTEGER IPXDP             ! Pointer to permuted extra data
      INTEGER IPXI1             ! Pointer to current frame list 1 X coords
      INTEGER IPXI2             ! Pointer to current frame list 2 X coords
      INTEGER IPXN              ! Pointer to usable pixel frame X coords
      INTEGER IPXP              ! Pointer to all pixel frame X coords
      INTEGER IPXQ( CCD1__MXLIS ) ! Pointer to transformed X coordinates
      INTEGER IPXO1( CCD1__MXLIC ) ! Pointer to list 1 output X coords
      INTEGER IPXO2( CCD1__MXLIC ) ! Pointer to list 2 output X coords
      INTEGER IPXT              ! Pointer to temporary X coords
      INTEGER IPY( CCD1__MXLIS ) ! Pointer to out/input Y coords
      INTEGER IPYI1             ! Pointer to current frame list 1 Y coords
      INTEGER IPYI2             ! Pointer to current frame list 2 Y coords
      INTEGER IPYN              ! Pointer to usable pixel frame Y coords
      INTEGER IPYP              ! Pointer to all pixel frame Y coords
      INTEGER IPYQ( CCD1__MXLIS ) ! Pointer to transformed X coordinates
      INTEGER IPYO1( CCD1__MXLIC ) ! Pointer to list 1 output Y coords
      INTEGER IPYO2( CCD1__MXLIC ) ! Pointer to list 2 output Y coords
      INTEGER IPYT              ! Pointer to temporary Y coords
      INTEGER IS                ! Superlist loop variable
      INTEGER ISUP( CCD1__MXLIS ) ! Superlist index applying to this list
      INTEGER IWCS              ! AST pointer to WCS frameset
      INTEGER J                 ! Loop variable
      INTEGER LBND( 2 )         ! Lower pixel-index bounds of NDF
      INTEGER K                 ! Loop index
      INTEGER L                 ! Loop index for lists
      INTEGER LOOPS             ! Number of comparison loops
      INTEGER MAPS( CCD1__MXLIS ) ! AST pointers to PIXEL->Current mappings
      INTEGER MAPSET( CCD1__MXLIS ) ! AST pointers to CCD_SET->Current mappings
      INTEGER MINMAT            ! Minimum number of positions for match
      INTEGER NDFGR             ! Input NDF group
      INTEGER NDIM              ! Number of dimensions in NDF
      INTEGER NEDGES            ! Number of edges in graph
      INTEGER NEWED             ! Number of edges in spanning graph
      INTEGER NLGR              ! Group of NDFs with no associated lists
      INTEGER NLOUT             ! Number of matched points in list
      INTEGER NMAT( CCD1__MXLIC ) ! Number of matched positions
      INTEGER NMATCH            ! Number of matches
      INTEGER NNODE             ! Number of nodes in spanning graph
      INTEGER NNOLIS            ! Number of NDFs with no associated lists
      INTEGER NOPEN             ! Number of input files opened
      INTEGER NOUT( CCD1__MXLIS ) ! Number of output positions in superlist
      INTEGER NPOSS             ! Number of possible point pairs
      INTEGER NREC( CCD1__MXLIS ) ! Number of records in list
      INTEGER NRECN( CCD1__MXLIS ) ! Number of records in list after removal
      INTEGER NRECS( CCD1__MXLIS ) ! Number of records in superlist
      INTEGER NRET              ! Dummy variable
      INTEGER NSUP              ! Number of superlists
      INTEGER NUMI1             ! Number of list 1 points in list 2 box
      INTEGER NUMI2             ! Number of list 2 points in list 1 box
      INTEGER NVAL              ! Number of values per-record
      INTEGER NXVAL( CCD1__MXLIS ) ! Number of extra data columns
      INTEGER OFFS( CCD1__MXLIS + 1 ) ! Offsets into extended lists
      INTEGER OUTGRP            ! Output group identifier
      INTEGER TOTNOD            ! Total number of nodes in graph
      INTEGER UBND( 2 )         ! Upper pixel-index bounds of NDF
      LOGICAL ALLOK             ! Trur no input positions removed in preselection phase
      LOGICAL COMPL             ! True if graph is complete
      LOGICAL CYCLIC            ! True if graph is cyclic
      LOGICAL FAILED            ! True if FAST and FSAFe are true and error occurs during match
      LOGICAL FAST              ! True if n**2 method used
      LOGICAL FSAFE             ! True if n**4 method used if n**2 fails
      LOGICAL NDFS              ! True if position list names are stored in NDF extensions
      LOGICAL OK                ! Match is ok
      LOGICAL RSTRCT            ! True if only objects in overlap considered
      LOGICAL USECOM            ! Use completeness measure as a weight
      LOGICAL USESET            ! Use Set header information to group lists
      LOGICAL USEWCS            ! True if we will attempt to use WCS extension
      LOGICAL OVERRD            ! True if partial selection is allowed
      LOGICAL PAIRED( CCD1__MXNDF ) ! Whether list is paired with someone

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start the CCDPACK logging system.
      CALL CCD1_START( 'FINDOFF', STATUS )

*  Begin AST context.
      CALL AST_BEGIN( STATUS )

*  Begin NDF context.
      CALL NDF_BEGIN

*  Initialise GRP identifiers, so that a later call of CCD1_GRDEL on
*  an uninitialised group cannot cause trouble.
      FIOGR = GRP__NOID
      OUTGRP = GRP__NOID
      NDFGR = GRP__NOID

*  No super lists and associated resources yet.
      NSUP = 0

*  Find out what is to be used for the source of the position list
*  names. Are they stored in NDF extensions or will just straight list
*  names be given.
      NDFS = .TRUE.
      CALL PAR_GET0L( 'NDFNAMES', NDFS, STATUS )

*  Get the lists of of positions.
      CALL CCD1_GTLIG( NDFS, 'CURRENT_LIST', 'INLIST', 1, CCD1__MXLIS,
     :                 .FALSE., NOPEN, FIOGR, NDFGR, NNOLIS, NLGR,
     :                 STATUS )
      CALL CCD1_GRDEL( NLGR, STATUS )

*  If not all supplied NDFs have position lists, warn the user of
*  this fact and continue.
      IF ( NNOLIS .GT. 0 ) THEN
         CALL CCD1_MSG( ' ', '  NDFs with no associated position '//
     :                  'lists will be ignored.', STATUS )
         CALL CCD1_MSG( ' ', ' ', STATUS )
      END IF

*  Get the error in the position measurements.
      CALL PAR_GET0D( 'ERROR', ERROR, STATUS )

*  Determine whether we should group lists according to Set.
      USESET = .FALSE.
      IF ( NDFS ) CALL PAR_GET0L( 'USESET', USESET, STATUS )

*  Get the minimum separation of points which is allowed. This should be
*  several times the point spread function (error).
      CALL PAR_DEF0D( 'MINSEP', ERROR * 5.0D0, STATUS )
      CALL PAR_GET0D( 'MINSEP', MINSEP, STATUS )

*  Get the maximum displacement allowed between frames, relative to
*  their original positioning, either using WCS information or not.
*  A zero value of MAXDIS represents a null value of the parameter.
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      CALL PAR_GET0D( 'MAXDISP', MAXDIS, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         MAXDIS = 0D0
         CALL ERR_ANNUL( STATUS )
      END IF

*  Get the minimum completeness factor which is ok.
      CALL PAR_GET0D( 'COMPLETE', NEDFAC, STATUS )
      NEDFAC = MAX( 0.0D0, MIN( NEDFAC, 1.0D0 ) )

*  Is the completeness to be used to weight the edges when picking out
*  the spanning graph?
      CALL PAR_GET0L( 'USECOMP', USECOM, STATUS )

*  Get the minimum number of positions which are required for a match.
      CALL PAR_GET0I( 'MINMATCH', MINMAT, STATUS )
      MINMAT = MAX( 1, MINMAT )

*  Get the operation mode. FAST which uses the statistical analysis or
*  .NOT.FAST which uses the straight-forward distance comparison.
      FAST = .FALSE.
      CALL PAR_GET0L( 'FAST', FAST, STATUS )

*  If the mode is fast does the user want to use the none fast option
*  when fast intercomparison fails?
      FSAFE = .FALSE.
      IF ( FAST ) CALL PAR_GET0L( 'FAILSAFE', FSAFE, STATUS )

*  See if we should continue with registration if only a few of the
*  datasets have been matched.
      OVERRD = .FALSE.
      CALL PAR_GET0L( 'OVERRIDE', OVERRD, STATUS )

*  See if we should use WCS extension information in NDFs.
      IF ( NDFS ) THEN
         USEWCS = .TRUE.
         CALL PAR_GET0L( 'USEWCS', USEWCS, STATUS )
      ELSE
         USEWCS = .FALSE.
      END IF

*  See if we should use WCS information to restrict lists of objects
*  to be matched.
      IF ( USEWCS ) THEN
         CALL PAR_GET0L( 'RESTRICT', RSTRCT, STATUS )
      ELSE
         RSTRCT = .FALSE.
      END IF

*  Extract required Set and WCS information from the position lists,
*  and write logging messages to the user.
      CALL CCD1_SWLIS( NDFGR, FIOGR, NOPEN, GRP__NOID, 0, NDFS, USEWCS,
     :                 USESET, FRMS, MAPS, MAPSET, ISUP, NSUP, ILIS,
     :                 ILISOF, DUMMY, STATUS )

*  Get the pixel size and bounds if required.
      IF ( USEWCS ) THEN
         DO I = 1, NOPEN

*  Get the NDF identifier.
            CALL NDG_NDFAS( NDFGR, I, 'READ', IDIN, STATUS )

*  Get NDF bounding box in pixel coordinates.
            CALL NDF_BOUND( IDIN, 2, LBND, UBND, NDIM, STATUS )
            IF ( RSTRCT ) THEN

*  Get bounding box: BNDX and BNDY contain the X and Y pixel coordinates
*  of the corners of the NDF's DATA array, for the purpose of determining
*  where overlaps are expected once WCS information has been obtained.
*  They must be listed in BNDX and BNDY in a clockwise, or anti-clockwise,
*  order.  They are modified here by the ERROR parameter so that pixels
*  outside the box by that distance are considered for matching.
               XP( 1 ) = DBLE( LBND( 1 ) - 1 ) - ERROR
               XP( 2 ) = DBLE( UBND( 1 ) ) + ERROR
               XP( 3 ) = DBLE( UBND( 1 ) ) + ERROR
               XP( 4 ) = DBLE( LBND( 1 ) - 1 ) - ERROR
               YP( 1 ) = DBLE( LBND( 2 ) - 1 ) - ERROR
               YP( 2 ) = DBLE( LBND( 2 ) - 1 ) - ERROR
               YP( 3 ) = DBLE( UBND( 2 ) ) + ERROR
               YP( 4 ) = DBLE( UBND( 2 ) ) + ERROR

*  Convert the bounding box from pixel to current coordinates.
               CALL AST_TRAN2( MAPS( I ), 4, XP, YP, .TRUE.,
     :                         BNDX( 1, I ), BNDY( 1, I ), STATUS )
            END IF

*  Work out the approximate linear size of a pixel.  This will
*  be used to convert ERROR and MAXDISP from pixel coordinates to the
*  coordinates of the frame in question.  It's chosen
*  arbitrarily from one or other of the NDFs in a pair being matched,
*  but if they aren't of very similar scale they are not going to
*  match anyway.
            CALL CCD1_GTWCS( IDIN, IWCS, STATUS )
            CALL CCD1_PSIZE( IWCS, AST__CURRENT, PSIZE( I ), STATUS )

*  Relase the NDF.
            CALL NDF_ANNUL( IDIN, STATUS )
         END DO

*  Not using WCS; set pixel size to unity, since the coordinates we will
*  be using will be pixel coordinates.
      ELSE
         DO I = 1, NOPEN
            PSIZE( I ) = 1D0
         END DO
      END IF

*  Report the initial parameters.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '     Initial parameters', STATUS )
      CALL CCD1_MSG( ' ', '     ------------------', STATUS )

*  And the rest of the parameters.
      CALL MSG_SETD( 'ERROR', ERROR )
      CALL CCD1_MSG( '  ', '  Error in positions: ^ERROR pixels',
     :               STATUS )
      IF ( MAXDIS .NE. 0D0 ) THEN
         CALL MSG_SETD( 'MAXDIS', MAXDIS )
         CALL CCD1_MSG( ' ',
     :    '  Maximum displacement allowed: ^MAXDIS pixels', STATUS )
      ELSE
         CALL CCD1_MSG( ' ',
     :    '  Arbitrarily large displacements allowed', STATUS )
      END IF
      CALL MSG_SETD( 'MINSEP', MINSEP )
      CALL CCD1_MSG( '  ',
     : '  Minimum distance between positions: ^MINSEP pixels', STATUS )
      CALL MSG_SETI( 'MINMAT', MINMAT )
      CALL CCD1_MSG( ' ', '  Minimum number of positions required'//
     : ' for positive match: ^MINMAT', STATUS )
      CALL MSG_SETD( 'COMPL', NEDFAC )
      CALL CCD1_MSG( ' ', '  Minimum completeness level'//
     : ' for positive match: ^COMPL', STATUS )
      IF ( USECOM ) THEN
         CALL CCD1_MSG( ' ','  Completeness estimates will be used to'//
     :   ' weight connections', STATUS )
      ELSE
         CALL CCD1_MSG( ' ','  Completeness estimates will not be'//
     :   ' used to weight connections', STATUS )
      END IF
      IF ( USEWCS ) THEN
         CALL CCD1_MSG( ' ', '  Coordinates will be remapped to'//
     :   ' Current frame before use', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  Pixel coordinates will be used'//
     :   ' direct', STATUS )
      END IF
      IF ( RSTRCT ) THEN
         CALL CCD1_MSG( ' ', '  Attempted matches will be'//
     :   ' restricted to apparent overlap zones', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  All objects will be considered'//
     :   ' for possible matches', STATUS )
      END IF
      IF ( USESET ) THEN
         CALL CCD1_MSG( ' ', '  Set Name attributes are used to group'//
     :   ' position lists', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  Each list is treated separately',
     :   STATUS )
      END IF

*  What sort of comparison will be performed.
      IF ( FAST ) THEN
         CALL CCD1_MSG( ' ', '  Using FAST matching algorithm', STATUS )
         IF ( FSAFE ) THEN
            CALL CCD1_MSG( ' ', '  Failsafe in operation', STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Failsafe not in operation', STATUS )
         END IF
      ELSE
         CALL CCD1_MSG( ' ', '  Using SLOW matching algorithm',
     :   STATUS )
      END IF

*=======================================================================
*  Data extraction section. Group into superlists and reject positions
*  which are too close.
*=======================================================================
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','    Data PRESELECTION', STATUS )
      CALL CCD1_MSG( ' ','    -----------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Set flag to indicate that no positions have been removed.
      ALLOK = .TRUE.

*  Initialise global index of next position to be encountered.
      IPOS = 1

*  Initialise number of positions in each superlist so far.
      DO I = 1, NSUP
         NRECS( I ) = 0
      END DO

*  Extract the data present in the input files.
      DO I = 1, NOPEN

*  Open the input files and test the number of entries.
         CALL GRP_GET( FIOGR, I, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'READ', 'LIST', 0, FDIN, STATUS )
         CALL CCD1_LTEST( FDIN, LINE, CCD1__BLEN, 2, 0, NVAL, STATUS )
         IF ( NVAL .GT. 0 ) THEN
            IF ( NVAL .EQ. 2 ) THEN

*  Map in X and Y positions only (non-standard file)
               NXVAL( I ) = 0
               CALL CCD1_NLMAP( FDIN, LINE, CCD1__BLEN, IPDAT,
     :                          NREC( I ), NVAL, STATUS )
            ELSE

*  Standard file format map these in.
               NXVAL( I ) = NVAL - 3
               CALL CCD1_LMAP( FDIN, LINE, CCD1__BLEN, IPIND, IPDAT,
     :                         NREC( I ), NVAL, STATUS )
               CALL CCD1_MFREE( IPIND, STATUS )
            END IF

*  Accumulate the number of records in this superlist.
            NRECS( ISUP( I ) ) = NRECS( ISUP( I ) ) + NREC( I )

*  Get workspace for storing the point identifiers and X and Y pixel
*  coordinate values.
            CALL CCD1_MALL( NREC( I ), '_INTEGER', IPSEQ( I ), STATUS )
            CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPXP, STATUS )
            CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPYP, STATUS )

*  Prepare a list of unique indices identifying each point.  This will
*  enable us later on to keep track of the file in which each point
*  originated.
            CALL CCD1_GISEQ( IPOS, 1, NREC( I ),
     :                       %VAL( CNF_PVAL( IPSEQ( I ) ) ),
     :                       STATUS )
            IPOS = IPOS + NREC( I )

*  Extract the X and Y values from the mapped data array.
            CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ),
     :                      NREC( I ), NVAL, 1,
     :                      %VAL( CNF_PVAL( IPXP ) ), STATUS )
            CALL CCD1_LEXT( %VAL( CNF_PVAL( IPDAT ) ),
     :                      NREC( I ), NVAL, 2,
     :                      %VAL( CNF_PVAL( IPYP ) ), STATUS )

*  Transform the X and Y values into the correct coordinate system.
            IF ( USEWCS ) THEN
               CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPXQ( I ), STATUS )
               CALL CCD1_MALL( NREC( I ), '_DOUBLE', IPYQ( I ), STATUS )
               CALL AST_TRAN2( MAPS( I ), NREC( I ),
     :                         %VAL( CNF_PVAL( IPXP ) ),
     :                         %VAL( CNF_PVAL( IPYP ) ), .TRUE.,
     :                         %VAL( CNF_PVAL( IPXQ( I ) ) ),
     :                         %VAL( CNF_PVAL( IPYQ( I ) ) ), STATUS )
               CALL CCD1_MFREE( IPXP, STATUS )
               CALL CCD1_MFREE( IPYP, STATUS )
            ELSE
               IPXQ( I ) = IPXP
               IPYQ( I ) = IPYP
            END IF

*  Extract additional values from the mapped data array if present.
            IF ( NXVAL( I ) .GT. 0 ) THEN
               CALL CCD1_MALL( NREC( I ) * NXVAL( I ), '_DOUBLE',
     :                         IPXDAT( I ), STATUS )
               CALL CCD1_XDAT( %VAL( CNF_PVAL( IPDAT ) ),
     :                         NREC( I ), NVAL,
     :                         %VAL( CNF_PVAL( IPXDAT( I ) ) ), STATUS )
            END IF

*  Release workspace.
            CALL CCD1_MFREE( IPDAT, STATUS )

*  No lines in the position list file.
         ELSE
            NREC( I ) = 0
            NXVAL( I ) = 0
         END IF

*  And close the file.
         CALL FIO_CLOSE( FDIN, STATUS )
      END DO

*  Construct the initial position superlists.  If Set headers are not
*  being used, these will just be the position lists read in.
*  However, if Set headers are being used, one position superlist will
*  contain the union of all the points in all the read lists with
*  the same Set Name attribute.
      DO I = 1, NSUP
         IF ( NRECS( I ) .GE. MINMAT ) THEN
            IF ( USESET ) THEN
               CALL CCD1_MALL( NRECS( I ), '_INTEGER', IPRDN, STATUS )
               CALL CCD1_MALL( NRECS( I ), '_DOUBLE', IPXN, STATUS )
               CALL CCD1_MALL( NRECS( I ), '_DOUBLE', IPYN, STATUS )
               IOFF = 1
               DO J = ILISOF( I ), ILISOF( I + 1 ) - 1
                  L = ILIS( J )
                  IF ( NREC( L ) .GT. 0 ) THEN
                     CALL CCG1_COPSI( 1, %VAL( CNF_PVAL( IPSEQ( L ) ) ),
     :                                NREC( L ),
     :                                IOFF, %VAL( CNF_PVAL( IPRDN ) ),
     :                                STATUS )
                     CALL CCG1_COPSD( 1, %VAL( CNF_PVAL( IPXQ( L ) ) ),
     :                                NREC( L ),
     :                                IOFF, %VAL( CNF_PVAL( IPXN ) ),
     :                                STATUS )
                     CALL CCG1_COPSD( 1, %VAL( CNF_PVAL( IPYQ( L ) ) ),
     :                                NREC( L ),
     :                                IOFF, %VAL( CNF_PVAL( IPYN ) ),
     :                                STATUS )
                     IOFF = IOFF + NREC( L )
                     CALL CCD1_MFREE( IPSEQ( L ), STATUS )
                     CALL CCD1_MFREE( IPXQ( L ), STATUS )
                     CALL CCD1_MFREE( IPYQ( L ), STATUS )
                  END IF
               END DO
            ELSE
               IPRDN = IPSEQ( I )
               IPXN = IPXQ( I )
               IPYN = IPYQ( I )
            END IF

*  Ok. Now select which of these point will be considered for matching.
*  Remove points which are too close.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            CALL CCD1_MALL( NRECS( I ), '_INTEGER', IPRAN( I ), STATUS )
            CALL CCD1_MALL( NRECS( I ), '_DOUBLE', IPX( I ), STATUS )
            CALL CCD1_MALL( NRECS( I ), '_DOUBLE', IPY( I ), STATUS )
            CALL CCD1_PRMIN( %VAL( CNF_PVAL( IPXN ) ),
     :                       %VAL( CNF_PVAL( IPYN ) ),
     :                       %VAL( CNF_PVAL( IPRDN ) ),
     :                       NRECS( I ),
     :                       MINSEP * PSIZE( ILIS( ILISOF( I ) ) ),
     :                       %VAL( CNF_PVAL( IPX( I ) ) ),
     :                       %VAL( CNF_PVAL( IPY( I ) ) ),
     :                       %VAL( CNF_PVAL( IPRAN( I ) ) ),
     :                       NRECN( I ), STATUS )
            CALL CCD1_MFREE( IPRDN, STATUS )
            CALL CCD1_MFREE( IPXN, STATUS )
            CALL CCD1_MFREE( IPYN, STATUS )

*  If any points have been rejected, report this and set ALLOK false
*  to indicate that at least some input positions have been rejected.
            IF ( NRECN( I ) .NE. NRECS( I ) ) THEN
               ALLOK = .FALSE.
               CALL MSG_SETI( 'NREJ', NRECS( I ) - NRECN( I ) )
               CALL MSG_SETI( 'N', I )
               CALL CCD1_MSG( ' ',
     :'  ^NREJ positions too close in list ^N); positions removed',
     :    STATUS )
            END IF
         ELSE
            NRECN( I ) = NRECS( I )
         END IF

*  Check list has at least MINMAT values, otherwise it isn't possible
*  to proceed.
         IF ( NRECN( I ) .LT. MINMAT ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', I )
            CALL MSG_SETI( 'NRECN', NRECN( I ) )
            CALL MSG_SETI( 'MINMAT', MINMAT )
            CALL ERR_REP( 'TOOFEW','  List ^N) only contains '//
     :'^NRECN positions. At least ^MINMAT are required.', STATUS )
            GO TO 99
         END IF
      END DO
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Comment if no positions have been removed.
      IF ( ALLOK )
     :   CALL CCD1_MSG( ' ',
     :   '  No positions removed in pre-selection phase', STATUS )

*=======================================================================
*  Main loop for intercomparisons of position data.
*=======================================================================
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    INTERCOMPARISON and MATCHING', STATUS )
      CALL CCD1_MSG( ' ', '    ----------------------------', STATUS )

*  Determine the number of loops we will perform.
      LOOPS = 0
      DO I = 1, NSUP - 1
        DO J = I + 1, NSUP
           LOOPS = LOOPS + 1
        END DO
      END DO

*  Inform user of the number of intercomparisons.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL MSG_SETI( 'LOOPS', LOOPS )
      CALL CCD1_MSG( ' ', '  No. of intercomparisons: ^LOOPS', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Set and output header labels.
      LINE1 = '    List  List  No. matches  Completeness  Status   '
      LINE2 = '    ----  ----  -----------  ------------  ------   '
      IF ( ( FAST .AND. FSAFE ) .OR. MINMAT .EQ. 1 ) THEN
         LINE1( 53: ) = 'Algorithm  '
         LINE2( 53: ) = '---------  '
      END IF
      CALL CCD1_MSG( ' ', LINE1, STATUS )
      CALL CCD1_MSG( ' ', LINE2, STATUS )

*  Loop comparing each list with all the lists which follow it.
      COUNT = 0
      NMATCH = 0
      DO 3 I = 1, NSUP - 1
         DO 4 J = I + 1, NSUP

*  Increment counters and set number of matched positions.
            COUNT = COUNT + 1
            NMATCH = NMATCH + 1
            NMAT( COUNT ) = 0

*  Is this match ok?
            OK = .TRUE.
            FAILED = .FALSE.
            METHOD = ' '

*  Get pixel size for this pair.  Use the value for list I; no reason
*  we shouldn't use the value for list J, but if they're not pretty
*  similar the points aren't going to match anyway.
            PIXSIZ = PSIZE( ILIS( ILISOF( I ) ) )

*  Now get workspace arrays for matching routines (plus 2 for CCD1_SOFF
*  sorting routines).
            NPOSS = NRECN( I ) * ( NRECN( J ) + 2 )
            CALL CCD1_MALL( NPOSS, '_DOUBLE', IPWRK1, STATUS )

*  Workspace for the output X and Y values.
            CALL CCD1_MALL( NRECN( I ), '_DOUBLE', IPXO1( COUNT ),
     :                      STATUS )
            CALL CCD1_MALL( NRECN( I ), '_DOUBLE', IPYO1( COUNT ),
     :                      STATUS )
            CALL CCD1_MALL( NRECN( J ), '_DOUBLE', IPXO2( COUNT ),
     :                      STATUS )
            CALL CCD1_MALL( NRECN( J ), '_DOUBLE', IPYO2( COUNT ),
     :                      STATUS )

*  And for remembering the original positions in input data sets.
            CALL CCD1_MALL( NRECN( I ), '_INTEGER', IPRAN1, STATUS )
            CALL CCD1_MALL( NRECN( J ), '_INTEGER', IPRAN2, STATUS )
            CALL CCD1_MALL( NRECN( I ), '_INTEGER', IPRBN1, STATUS )
            CALL CCD1_MALL( NRECN( J ), '_INTEGER', IPRBN2, STATUS )

*  Prepare the lists of points we will actually use (these may omit
*  ones which do not appear in the overlap).
            IF ( RSTRCT ) THEN

*  Allocate space for points in the overlap between NDFs.
               CALL CCD1_MALL( NRECN( I ), '_DOUBLE', IPXI1, STATUS )
               CALL CCD1_MALL( NRECN( I ), '_DOUBLE', IPYI1, STATUS )
               CALL CCD1_MALL( NRECN( J ), '_DOUBLE', IPXI2, STATUS )
               CALL CCD1_MALL( NRECN( J ), '_DOUBLE', IPYI2, STATUS )

*  Select only points in list I which fall inside bounding boxes
*  associated with list J.
               CALL CCD1_MALL( NRECN( I ), '_LOGICAL', IPGOOD, STATUS )
               CALL CCG1_STVL( .FALSE., NRECN( I ),
     :                         %VAL( CNF_PVAL( IPGOOD ) ),
     :                         STATUS )
               DO K = ILISOF( J ), ILISOF( J + 1 ) - 1
                  L = ILIS( K )
                  CALL CCD1_INPLY( BNDX( 1, L ), BNDY( 1, L ), 4,
     :                             %VAL( CNF_PVAL( IPX( I ) ) ),
     :                             %VAL( CNF_PVAL( IPY( I ) ) ),
     :                             NRECN( I ),
     :                             %VAL( CNF_PVAL( IPGOOD ) ), STATUS )
               END DO
               CALL CCD1_CHUSP( %VAL( CNF_PVAL( IPGOOD ) ),
     :                          %VAL( CNF_PVAL( IPX( I ) ) ),
     :                          %VAL( CNF_PVAL( IPY( I ) ) ),
     :                          NRECN( I ),
     :                          %VAL( CNF_PVAL( IPXI1 ) ),
     :                          %VAL( CNF_PVAL( IPYI1 ) ),
     :                          %VAL( CNF_PVAL( IPRBN1 ) ),
     :                          NUMI1, STATUS )
               CALL CCD1_MFREE( IPGOOD, STATUS )

*  Select only points in list J which fall inside bounding boxes
*  associated with list I.
               CALL CCD1_MALL( NRECN( J ), '_LOGICAL', IPGOOD, STATUS )
               CALL CCG1_STVL( .FALSE., NRECN( J ),
     :                         %VAL( CNF_PVAL( IPGOOD ) ),
     :                         STATUS )
               DO K = ILISOF( I ), ILISOF( I + 1 ) - 1
                  L = ILIS( K )
                  CALL CCD1_INPLY( BNDX( 1, L ), BNDY( 1, L ), 4,
     :                             %VAL( CNF_PVAL( IPX( J ) ) ),
     :                             %VAL( CNF_PVAL( IPY( J ) ) ),
     :                             NRECN( J ),
     :                             %VAL( CNF_PVAL( IPGOOD ) ), STATUS )
               END DO
               CALL CCD1_CHUSP( %VAL( CNF_PVAL( IPGOOD ) ),
     :                          %VAL( CNF_PVAL( IPX( J ) ) ),
     :                          %VAL( CNF_PVAL( IPY( J ) ) ),
     :                          NRECN( J ),
     :                          %VAL( CNF_PVAL( IPXI2 ) ),
     :                          %VAL( CNF_PVAL( IPYI2 ) ),
     :                          %VAL( CNF_PVAL( IPRBN2 ) ),
     :                          NUMI2, STATUS )
               CALL CCD1_MFREE( IPGOOD, STATUS )
            ELSE

*  No restrictions on which points to consider - copy all to the
*  arrays for matching.
               IPXI1 = IPX( I )
               IPYI1 = IPY( I )
               IPXI2 = IPX( J )
               IPYI2 = IPY( J )
               NUMI1 = NRECN( I )
               NUMI2 = NRECN( J )
               CALL CCD1_GISEQ( 1, 1, NUMI1, %VAL( CNF_PVAL( IPRBN1 ) ),
     :                          STATUS )
               CALL CCD1_GISEQ( 1, 1, NUMI2, %VAL( CNF_PVAL( IPRBN2 ) ),
     :                          STATUS )
            END IF

*  Check if we have enough points in each list to satisfy the matching
*  criteria.
            IF ( NUMI1 .LT. MINMAT .OR. NUMI2 .LT. MINMAT )
     :         OK = .FALSE.

*  Generate the offset statistics.
            IF ( STATUS .NE. SAI__OK ) GO TO 99
            IF ( ( NUMI1 .EQ. 1 .OR. NUMI2 .EQ. 1 ) .AND. OK ) THEN

*  One or both lists have only one object.  Treat as a special case.
               METHOD = 'SNGL'
               CALL CCD1_SNGL( MAXDIS * PIXSIZ,
     :                         %VAL( CNF_PVAL( IPXI1 ) ),
     :                         %VAL( CNF_PVAL( IPYI1 ) ),
     :                         %VAL( CNF_PVAL( IPRBN1 ) ), NUMI1,
     :                         %VAL( CNF_PVAL( IPXI2 ) ),
     :                         %VAL( CNF_PVAL( IPYI2 ) ),
     :                         %VAL( CNF_PVAL( IPRBN2 ) ), NUMI2,
     :                         %VAL( CNF_PVAL( IPXO1( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPYO1( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPXO2( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPYO2( COUNT ) ) ),
     :                         NMAT( COUNT ),
     :                         XOFF( COUNT ), YOFF( COUNT ),
     :                         %VAL( CNF_PVAL( IPRAN1 ) ),
     :                         %VAL( CNF_PVAL( IPRAN2 ) ), STATUS )

*  If the match was successful, completeness must be unity.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  COMFAC = 1D0
               ELSE
                  COMFAC = 0D0
               END IF

            ELSE IF ( FAST .AND. OK ) THEN

*  Perform matching using histogram of X and Y offsets refined through
*  iteration.
               METHOD = 'FAST'
               CALL CCD1_STAO( ERROR * PIXSIZ, MAXDIS * PIXSIZ,
     :                         %VAL( CNF_PVAL( IPXI1 ) ),
     :                         %VAL( CNF_PVAL( IPYI1 ) ),
     :                         %VAL( CNF_PVAL( IPRBN1 ) ), NUMI1,
     :                         %VAL( CNF_PVAL( IPXI2 ) ),
     :                         %VAL( CNF_PVAL( IPYI2 ) ),
     :                         %VAL( CNF_PVAL( IPRBN2 ) ), NUMI2,
     :                         %VAL( CNF_PVAL( IPWRK1 ) ),
     :                         %VAL( CNF_PVAL( IPXO1( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPYO1( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPXO2( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPYO2( COUNT ) ) ),
     :                         NMAT( COUNT ),
     :                         XOFF( COUNT ), YOFF( COUNT ),
     :                         %VAL( CNF_PVAL( IPRAN1 ) ),
     :                         %VAL( CNF_PVAL( IPRAN2 ) ), STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN

*  This mode has failed. If failsafe is set set FAILED to TRUE. Annul
*  the error and continue in any case.
                  IF ( FSAFE ) THEN
                     FAILED = .TRUE.
                  ELSE

*  Match has failed.
                    OK = .FALSE.
                  END IF
                  CALL ERR_ANNUL( STATUS )
               ELSE

*  Match is a success. If failsafe is in operation then check that this
*  has also passed the other success criteria, such as minimum number
*  and completeness.
                  IF ( FSAFE ) THEN

*  Now do the test.
                     CALL CCD1_OVCOM( %VAL( CNF_PVAL( IPX( I ) ) ),
     :                                %VAL( CNF_PVAL( IPY( I ) ) ),
     :                                NRECN( I ),
     :                                %VAL( CNF_PVAL( IPX( J ) ) ),
     :                                %VAL( CNF_PVAL( IPY( J ) ) ),
     :                                NRECN( J ),
     :                                NMAT( COUNT ), XOFF( COUNT ),
     :                                YOFF( COUNT ), ERROR * PIXSIZ,
     :                                COMFAC, STATUS )

*  Now check for minimum number match and completeness. Set failed if
*  this match fails now.
                     IF ( COMFAC .LT. NEDFAC .OR.
     :                    NMAT( COUNT ) .LT. MINMAT ) THEN
                        FAILED = .TRUE.
                     END IF
                  END IF
               END IF
            END IF

*  Perform matching using the straight-forward distance comparisons
*  if this is required.
            IF ( ( .NOT. FAST .OR. ( FSAFE .AND. FAILED ) )
     :           .AND. METHOD .NE. 'SNGL' .AND. OK ) THEN

*  Get workspace and perform comparison.
               METHOD = 'SLOW'
               CALL CCD1_MALL( NPOSS, '_DOUBLE', IPWRK2, STATUS )
               CALL CCD1_MALL( NPOSS, '_INTEGER', IPWRK3, STATUS )
               CALL CCD1_MALL( NUMI2 + 2, '_INTEGER', IPWRK4,
     :                         STATUS )
               CALL CCD1_MALL( NUMI2 + 2, '_INTEGER', IPWRK5,
     :                         STATUS )
               CALL CCD1_SOFF( ERROR * PIXSIZ, MAXDIS * PIXSIZ,
     :                         %VAL( CNF_PVAL( IPXI1 ) ),
     :                         %VAL( CNF_PVAL( IPYI1 ) ),
     :                         %VAL( CNF_PVAL( IPRBN1 ) ), NUMI1,
     :                         %VAL( CNF_PVAL( IPXI2 ) ),
     :                         %VAL( CNF_PVAL( IPYI2 ) ),
     :                         %VAL( CNF_PVAL( IPRBN2 ) ), NUMI2,
     :                         %VAL( CNF_PVAL( IPWRK1 ) ),
     :                         %VAL( CNF_PVAL( IPWRK2 ) ),
     :                         %VAL( CNF_PVAL( IPWRK3 ) ),
     :                         %VAL( CNF_PVAL( IPWRK4 ) ),
     :                         %VAL( CNF_PVAL( IPWRK5 ) ),
     :                         %VAL( CNF_PVAL( IPXO1( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPYO1( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPXO2( COUNT ) ) ),
     :                         %VAL( CNF_PVAL( IPYO2( COUNT ) ) ),
     :                         NMAT( COUNT ),
     :                         XOFF( COUNT ), YOFF( COUNT ),
     :                         %VAL( CNF_PVAL( IPRAN1 ) ),
     :                         %VAL( CNF_PVAL( IPRAN2 ) ), STATUS )
               CALL CCD1_MFREE( IPWRK2, STATUS )
               CALL CCD1_MFREE( IPWRK3, STATUS )
               CALL CCD1_MFREE( IPWRK4, STATUS )
               CALL CCD1_MFREE( IPWRK5, STATUS )
            END IF
            IF ( STATUS .NE. SAI__OK ) THEN

*  Match has failed.
               CALL ERR_ANNUL( STATUS )
               OK = .FALSE.
            END IF

*  Now (re-) determine the completeness of the match. If no positions
*  have been matched then set the completeness factor to 0.
            IF ( .NOT. OK ) THEN
               COMFAC = 0.0D0
            ELSE

*  Now do the test.
               CALL CCD1_OVCOM( %VAL( CNF_PVAL( IPX( I ) ) ),
     :                          %VAL( CNF_PVAL( IPY( I ) ) ),
     :                          NRECN( I ),
     :                          %VAL( CNF_PVAL( IPX( J ) ) ),
     :                          %VAL( CNF_PVAL( IPY( J ) ) ),
     :                          NRECN( J ),
     :                          NMAT( COUNT ), XOFF( COUNT ),
     :                          YOFF( COUNT ), ERROR * PIXSIZ,
     :                          COMFAC, STATUS )
            END IF

*  Final check for ok match.
            IF ( OK ) THEN
               IF ( COMFAC .LT. NEDFAC .OR.
     :              NMAT( COUNT ) .LT. MINMAT ) THEN
                 OK = .FALSE.
               END IF
            END IF

*  Write information about this loop.
            CALL CHR_ITOC( I, LINE( 6: ), IAT )
            CALL CHR_ITOC( J, LINE( 12: ), IAT )
            CALL CHR_ITOC( NMAT( COUNT ), LINE( 19: ), IAT )
            CALL CHR_RTOC( REAL( COMFAC ), LINE( 30: ), IAT )
            IF ( OK ) THEN
                LINE( 44: ) = 'ACCEPTED'
            ELSE
                LINE( 44: ) = 'rejected'
            END IF
            IF ( ( FAST .AND. FSAFE ) .OR. MINMAT .EQ. 1 )
     :         LINE( 56: ) = METHOD

*  And write the line of information
            CALL CCD1_MSG( ' ', LINE, STATUS )

*  If matching process failed then reset values for this pass and
*  release workspace which will not now be used.
            IF ( .NOT. OK ) THEN
               NMAT( COUNT ) = 0
               NMATCH = NMATCH - 1
               CALL CCD1_MFREE( IPXO1( COUNT ), STATUS )
               CALL CCD1_MFREE( IPYO1( COUNT ), STATUS )
               CALL CCD1_MFREE( IPXO2( COUNT ), STATUS )
               CALL CCD1_MFREE( IPYO2( COUNT ), STATUS )
            ELSE

*  Keep track of the permutation of the points.
               CALL CCD1_MALL( NMAT( COUNT ), '_INTEGER',
     :                         IPRCN1( COUNT ), STATUS )
               CALL CCD1_MALL( NMAT( COUNT ), '_INTEGER',
     :                         IPRCN2( COUNT ), STATUS )
               CALL CCG1_PRMTI( %VAL( CNF_PVAL( IPRAN( I ) ) ),
     :                          %VAL( CNF_PVAL( IPRAN1 ) ),
     :                          1, NMAT( COUNT ),
     :                          %VAL( CNF_PVAL( IPRCN1( COUNT ) ) ),
     :                          STATUS )
               CALL CCG1_PRMTI( %VAL( CNF_PVAL( IPRAN( J ) ) ),
     :                          %VAL( CNF_PVAL( IPRAN2 ) ),
     :                          1, NMAT( COUNT ),
     :                          %VAL( CNF_PVAL( IPRCN2( COUNT ) ) ),
     :                          STATUS )

*  Generate the weight; it is the number of matches, perhaps multiplied
*  by the completeness.
               WEIGHT( COUNT ) = DBLE( NMAT( COUNT ) )
               IF ( USECOM ) WEIGHT( COUNT ) = WEIGHT( COUNT ) * COMFAC
            END IF

*  Free resources used for handling overlap information.
            IF ( RSTRCT ) THEN
               CALL CCD1_MFREE( IPXI1, STATUS )
               CALL CCD1_MFREE( IPYI1, STATUS )
               CALL CCD1_MFREE( IPXI2, STATUS )
               CALL CCD1_MFREE( IPYI2, STATUS )
            END IF

*  Free common workspace.
            CALL CCD1_MFREE( IPWRK1, STATUS )
            CALL CCD1_MFREE( IPRAN1, STATUS )
            CALL CCD1_MFREE( IPRAN2, STATUS )
            CALL CCD1_MFREE( IPRBN1, STATUS )
            CALL CCD1_MFREE( IPRBN2, STATUS )
 4       CONTINUE
 3    CONTINUE

*  Free rank pointer workspace.
      DO I = 1, NSUP
         CALL CCD1_MFREE( IPRAN( I ), STATUS )
      END DO

*  Comment on the success or overwise of the intercomparisons. If no
*  intercomparisons were successful set status and abort, otherwise, if
*  requested, push on to check the connectivity of graph of
*  intercomparisons. To make this test definitive we need to check all
*  lists, as some may be matched more than once.
      DO 10 I = 1, NSUP
         PAIRED( I ) = .FALSE.
 10   CONTINUE
      COUNT = 0
      DO 8 I = 1, NSUP - 1
         DO 9 J = I + 1, NSUP
            COUNT = COUNT + 1
            IF ( NMAT( COUNT ) .NE. 0 ) THEN
               PAIRED( I ) = .TRUE.
               PAIRED( J ) = .TRUE.
            END IF
 9       CONTINUE
 8    CONTINUE
      OK = .TRUE.
      DO 11 I = 1, NSUP
         IF ( .NOT. PAIRED( I ) ) OK = .FALSE.
 11   CONTINUE
      IF ( .NOT. OK ) THEN
         IF ( NMATCH .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL CCD1_ERREP( 'TOTAL_FAILURE',
     :'  No positions were matched between any dataset', STATUS )
            GO TO 99
         ELSE
            IF ( OVERRD ) THEN
               CALL CCD1_MSG( ' ', ' ', STATUS )
               CALL CCD1_MSG( ' ',
     :'  Warning - not all position datasets have been successfully'//
     :' matched.', STATUS )
               CALL CCD1_MSG( ' ', '  Continuing with those that have.',
     :                        STATUS )
               CALL CCD1_MSG( ' ', '  (associated position lists will'//
     :                        ' be empty for the others).', STATUS )
               CALL CCD1_MSG( ' ', ' ', STATUS )
            ELSE
               STATUS = SAI__ERROR
               CALL CCD1_ERREP( 'TOTAL_FAILURE',
     :'  Not all position datasets have been successfully matched',
     :                          STATUS )
               GO TO 99
            END IF
         END IF
      END IF

*=======================================================================
*  End of data intercomparison and offset estimation
*=======================================================================
*  Spanning graph determination section
*=======================================================================
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','    CONNECTIONS and OFFSETS', STATUS )
      CALL CCD1_MSG( ' ','    -----------------------', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Get workspace for the graph testing section.
      CALL CCD1_MALL( NMATCH * 4, '_INTEGER', IPGRA, STATUS )
      CALL CCD1_MALL( NMATCH * 4, '_INTEGER', IPSPAN, STATUS )
      CALL CCD1_MALL( NMATCH * 4, '_INTEGER', IPSUB, STATUS )
      CALL CCD1_MALL( MAX( NSUP, NMATCH ), '_INTEGER', IPQUE, STATUS )
      CALL CCD1_MALL( MAX( NSUP, NMATCH ), '_LOGICAL', IPBEEN, STATUS )

*  Look for a maximum likelihood span of the graph of positions (each
*  position is treated as a node, the node-node transformations are
*  the edges with weights the number of positions matched).
*  First create the graph.
      CALL CCD1_CRGR( NMAT, COUNT, NSUP, %VAL( CNF_PVAL( IPGRA ) ),
     :                NEDGES,
     :                STATUS )

*  Call routine to determine if the graph is complete.
      CALL CCD1_GRAPC( %VAL( CNF_PVAL( IPGRA ) ), NEDGES, 1,
     :                 %VAL( CNF_PVAL( IPQUE ) ),
     :                 %VAL( CNF_PVAL( IPBEEN ) ), COMPL, CYCLIC,
     :                 %VAL( CNF_PVAL( IPSUB ) ),
     :                 NEWED, TOTNOD, STATUS )
      IF ( COMPL ) THEN

*  Graph is complete -- all nodes connected. Determine the most likely
*  spanning sub-graph. The most likely one is the graph which is most
*  strongly connected (largest count of matched pairs).
         CALL CCD1_MLSPG( %VAL( CNF_PVAL( IPGRA ) ),
     :                    WEIGHT, NEDGES, TOTNOD,
     :                    %VAL( CNF_PVAL( IPQUE ) ),
     :                    %VAL( CNF_PVAL( IPBEEN ) ),
     :                    %VAL( CNF_PVAL( IPSPAN ) ),
     :                    %VAL( CNF_PVAL( IPSUB ) ),
     :                    NEWED, NNODE, STATUS )

*  Determine the "complete" solution.
*  Find the offsets of all positions to the `reference' set (first
*  node of first edge of spanning graph is assumed to be the reference
*  set).
         CALL CCD1_GROFF( %VAL( CNF_PVAL( IPSUB ) ), NEWED, XOFF, YOFF,
     :                    NSUP, %VAL( CNF_PVAL( IPBEEN ) ),
     :                    %VAL( CNF_PVAL( IPQUE ) ),
     :                    XOFFN, YOFFN, STATUS )

*  Output the offsets to the user.
         DO I = 1, NSUP
            FRM( I ) = FRMS( ILIS( ILISOF( I ) ) )
         END DO
         CALL CCD1_PROFF( NSUP, %VAL( CNF_PVAL( IPBEEN ) ),
     :                    XOFFN, YOFFN, FRM,
     :                    USEWCS, STATUS )

*  Generate the ID's for the output lists. Matching positions between
*  the lists and final merging all positions for each node.
         DO I = 1, NSUP
            TOLS( I ) = 0D0
         END DO
         CALL CCD1_GMMP( %VAL( CNF_PVAL( IPSUB ) ),
     :                   NEWED, NSUP, IPXO1, IPYO1,
     :                   IPRCN1, IPXO2, IPYO2, IPRCN2, NMAT, TOLS,
     :                   OFFS, IPX, IPY, IPRAN, IPID, NOUT, STATUS )
      ELSE

*  Set STATUS and issue error (may change this to cope with error
*  by forming most likely connection).
         STATUS = SAI__ERROR
         CALL CCD1_ERREP( 'FINDOFF_NOTCON',
     :'  Intercomparison of positions does not produce a complete'//
     :' registration of all frames -- graph incomplete', STATUS )
         GO TO 99
      END IF

*  Free memory used in matching (this may have overflowed the internal
*  resources of CCD1_MALL/MFREE, so need to be more careful than just
*  using -1 in MFREE call).
      DO 1 I = 1, COUNT
         IF ( NMAT( I ) .NE. 0 ) THEN
            CALL CCD1_MFREE( IPXO1( I ), STATUS )
            CALL CCD1_MFREE( IPYO1( I ), STATUS )
            CALL CCD1_MFREE( IPXO2( I ), STATUS )
            CALL CCD1_MFREE( IPYO2( I ), STATUS )
            CALL CCD1_MFREE( IPRCN1( I ), STATUS )
            CALL CCD1_MFREE( IPRCN2( I ), STATUS )
         END IF
 1    CONTINUE
      CALL CCD1_MFREE( IPGRA, STATUS )
      CALL CCD1_MFREE( IPSPAN, STATUS )
      CALL CCD1_MFREE( IPSUB, STATUS )
      CALL CCD1_MFREE( IPQUE, STATUS )
      CALL CCD1_MFREE( IPBEEN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*=======================================================================
*   End of spanning graph section
*=======================================================================
*   Writing output lists and updating NDF extensions
*=======================================================================
*  Get the output position lists file names. Use the input NDF names
*  as possible modification elements when possible otherwise use the
*  input position list names.
      IF ( NDFS ) THEN
         CALL CCD1_STRGR( 'OUTLIST', NDFGR, NOPEN, NOPEN, OUTGRP, NRET,
     :                    STATUS )
      ELSE
         CALL CCD1_STRGR( 'OUTLIST', FIOGR, NOPEN, NOPEN, OUTGRP, NRET,
     :                    STATUS )
      END IF

*  Tell user the names.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '    Output position lists:', STATUS )
      CALL CCD1_MSG( ' ', '    ----------------------', STATUS )

*  Initialise the value of the next position in the global list to
*  be encountered.
      IPOS = 1

*  Create each of these files and write the appropriate information
*  to them.
      DO L = 1, NOPEN
         IS = ISUP( L )
         NLOUT = 0

*  Open the output file and write a header.
         CALL GRP_GET( OUTGRP, L, 1, FNAME, STATUS )
         CALL CCD1_OPFIO( FNAME, 'WRITE', 'LIST', 0, FDOUT, STATUS )
         CALL CCD1_FIOHD( FDOUT, 'Output from FINDOFF', STATUS )

*  If there are any points, get them in a suitable form for output.
         IF ( NOUT( IS ) .GT. 0 ) THEN

*  Get arrays representing the points of the list L, which may need
*  to be extracted from the points of the superlist IS.
            IF ( USESET ) THEN

*  Allocate memory for storing points extracted by list.
               CALL CCD1_MALL( NREC( L ), '_INTEGER', IPLRAN, STATUS )
               CALL CCD1_MALL( NREC( L ), '_INTEGER', IPLID, STATUS )
               CALL CCD1_MALL( NREC( L ), '_DOUBLE', IPLX, STATUS )
               CALL CCD1_MALL( NREC( L ), '_DOUBLE', IPLY, STATUS )

*  Extract the positions belonging to this list from the owning superlist.
               CALL CCD1_EXLIS( %VAL( CNF_PVAL( IPRAN( IS ) ) ),
     :                          %VAL( CNF_PVAL( IPID( IS ) ) ),
     :                          %VAL( CNF_PVAL( IPX( IS ) ) ),
     :                          %VAL( CNF_PVAL( IPY( IS ) ) ),
     :                          NOUT( IS ), IPOS, IPOS + NREC( L ) - 1,
     :                          %VAL( CNF_PVAL( IPLRAN ) ),
     :                          %VAL( CNF_PVAL( IPLID ) ),
     :                          %VAL( CNF_PVAL( IPLX ) ),
     :                          %VAL( CNF_PVAL( IPLY ) ), NLOUT,
     :                          STATUS )
            ELSE
               IPLRAN = IPRAN( IS )
               IPLID = IPID( IS )
               IPLX = IPX( IS )
               IPLY = IPY( IS )
               NLOUT = NOUT( IS )
            END IF

*  Only proceed if we have any positions to write to this list.
            IF ( NLOUT .GT. 0 ) THEN

*  If we have previously transformed from Pixel to Current coordinates,
*  transform the output list back again now.
               IF ( USEWCS ) THEN
                  CALL CCD1_MALL( NLOUT, '_DOUBLE', IPXT, STATUS )
                  CALL CCD1_MALL( NLOUT, '_DOUBLE', IPYT, STATUS )
                  CALL AST_TRAN2( MAPS( L ), NLOUT,
     :                            %VAL( CNF_PVAL( IPLX ) ),
     :                            %VAL( CNF_PVAL( IPLY ) ), .FALSE.,
     :                            %VAL( CNF_PVAL( IPXT ) ),
     :                            %VAL( CNF_PVAL( IPYT ) ), STATUS )
                  CALL CCD1_MFREE( IPLX, STATUS )
                  CALL CCD1_MFREE( IPLY, STATUS )
                  IPLX = IPXT
                  IPLY = IPYT
               END IF

*  If extra data columns were present in the input file, output them too.
               IF ( NXVAL( L ) .GT. 0 ) THEN

*  First doctor the point index values so they refer to position within
*  this file not position within all encountered files.
                  CALL CCG1_CADDI( %VAL( CNF_PVAL( IPLRAN ) ),
     :                             NLOUT, 1 - IPOS,
     :                             STATUS )

*  Permute the data values into the right order using the doctored
*  point index values.
                  CALL CCD1_MALL( NXVAL( L ) * NLOUT, '_DOUBLE', IPXDP,
     :                            STATUS )
                  CALL CCG1_PRMTD( %VAL( CNF_PVAL( IPXDAT( L ) ) ),
     :                             %VAL( CNF_PVAL( IPLRAN ) ),
     :                             NXVAL( L ), NLOUT,
     :                             %VAL( CNF_PVAL( IPXDP ) ),
     :                             STATUS )

*  And write them out.
                  CALL CCD1_WRIDI( FDOUT, %VAL( CNF_PVAL( IPLID ) ),
     :                             %VAL( CNF_PVAL( IPLX ) ),
     :                             %VAL( CNF_PVAL( IPLY ) ),
     :                             %VAL( CNF_PVAL( IPXDP ) ),
     :                             NXVAL( L ), NLOUT, LINE, CCD1__BLEN,
     :                             STATUS )
                  CALL CCD1_MFREE( IPXDP, STATUS )
                  CALL CCD1_MFREE( IPXDAT( L ), STATUS )

*  If no extra data columns, just output ID, X and Y; no permutations
*  are necessary.
               ELSE
                  CALL CCD1_WRIXY( FDOUT, %VAL( CNF_PVAL( IPLID ) ),
     :                             %VAL( CNF_PVAL( IPLX ) ),
     :                             %VAL( CNF_PVAL( IPLY ) ),
     :                             NLOUT, LINE,
     :                             CCD1__BLEN, STATUS )
               END IF

*  Release memory.
               IF ( USESET ) THEN
                  CALL CCD1_MFREE( IPLRAN, STATUS )
                  CALL CCD1_MFREE( IPLID, STATUS )
                  CALL CCD1_MFREE( IPLX, STATUS )
                  CALL CCD1_MFREE( IPLY, STATUS )
               END IF
            END IF
         END IF

*  Close the output file.
         CALL FIO_CLOSE( FDOUT, STATUS )

*  Output name.
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL CCD1_MSG( ' ', '  ^FNAME', STATUS )

*  If the names of the positions lists were accessed using NDF extension
*  information then update the extension.
         IF ( NDFS ) THEN

*  Get the NDF identifier.
            CALL NDG_NDFAS( NDFGR, L, 'UPDATE', IDIN, STATUS )

*  Store a new position list.
            CALL CCG1_STO0C( IDIN, 'CURRENT_LIST', FNAME, STATUS )

*  Release the NDF.
            CALL NDF_ANNUL( IDIN, STATUS )
         END IF

*  Update the index of the next encountered position.
         IPOS = IPOS + NREC( L )
      END DO

*  If the filenames were supplied directly then write an output list of
*  the names for other applications to use.
      IF ( .NOT. NDFS .AND. STATUS .EQ. SAI__OK ) THEN

*  Write the names of the output files to a file which may be used for
*  indirection into other applications.
         CALL CCD1_LNAM( 'NAMELIST', 1, NOPEN,
     :   '# FINDOFF - output position lists', OUTGRP, GRP__NOID,
     :                   .TRUE., STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL CCD1_MSG( ' ', '  No namelist written ', STATUS )
         END IF
      END IF

*  Abort on error label.
 99   CONTINUE

*  End NDF context.
      CALL NDF_END( STATUS )

*  Annul group identifiers.
      CALL CCD1_GRDEL( FIOGR, STATUS )
      CALL CCD1_GRDEL( OUTGRP, STATUS )
      CALL CCD1_GRDEL( NDFGR, STATUS )

*  Close AST.
      CALL AST_END( STATUS )

*  Free all workspace.
      CALL CCD1_MFREE( -1, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL CCD1_ERREP( 'FINDOFF_ERR',
     :   'FINDOFF: Error determining position list matches',
     :   STATUS )
      END IF

*  Finally close the logging system.
      CALL CCD1_END( STATUS )

      END
* $Id$
