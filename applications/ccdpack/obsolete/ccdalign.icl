hidden proc ccdalign
{+
{  Name:
{     CCDALIGN

{  Purpose:
{     Interactive procedure to aid the alignment of NDFs.

{  Language:
{     ICL/C-shell

{  Description:
{      This procedure aids the registration of NDFs which are not
{      related by simple offsets (see FINDOFF and PAIRNDF if they are).
{      It also has the capability of dealing with groups of NDFs which
{      are almost registered (frames which have not been moved on the
{      sky) saving effort in re-identification of image features.
{ 
{      The basic method used is to access groups of NDFs (or a series
{      of single NDFs if all are moved between exposures) and an
{      optional reference NDF. The first NDF of the first group or the
{      reference NDF is then displayed and a cursor application is used
{      to record the positions of centroidable image features. The
{      first NDFs of all the other groups are then displayed and you
{      are invited to identify the image features in the order which
{      corresponds to that used for the reference NDF. Missing image
{      features are identified as off the currently displayed image (so
{      the centroid routine will fail to find them). The reference set
{      of image features may be extended by identification after the
{      last reference feature has been marked.
{
{      After centroiding you are then given the option to stop. If
{      you decide to then you will have labelled position lists to use
{      in the other CCDPACK routines (the labelled positions will be
{      called NDF_NAME.acc). If you chose the option to continue then
{      a full registration of the NDFs will be attempted. This may only
{      be performed for "linear" transformations.
{
{      After choosing a transformation type the procedure will then go on
{      to calculate a transformation set between all the NDFs, this is
{      then used (with the extended reference set from REGISTER) to
{      approximate the position of all possible image features, these are
{      then located by centroiding and a final registration of all NDFs
{      is performed. The resultant NDFs then have associated lists of
{      labelled positions and TRANSFORM structures which may be used to
{      transform other position lists or when resampling the data.

{  ADAM parameters:
{     NONE

{  Authors:
{     PDRAPER: Peter Draper (STARLINK)
{     {enter_new_authors_here}

{  History:
{     15-JUN-1993 (PDRAPER):
{        Original version.
{     11-SEP-1995 (PDRAPER):
{        Ported to UNIX ICL.
{     {enter_changes_here}
{-
{
{  Symbols for writing output
     defstring echo print
{
{  Introduce the application:
     echo " "
     echo "    CCDALIGN"
     echo "    ========"
     echo " "
     echo "  An interactive aid for aligning groups of NDFs."
{
{  Initialise the appropriate application packages.
     kappa
     ccdpack
{
{  Get the name of a display device.
     echo " "
     echo "  Give the name of an image display device"
     echo " "
     input "DEVICE - Image display device > " (display_device)
{
{  Check the return for sense. If the abort signal has been given do so.
     if ( ( display_device = "!!" ) OR ~
          ( display_device = ""   ) OR ~
          ( display_device = "!"  ) )
        signal escape
     else
        display_device = '@' & (display_device)
     endif

{
{  Get a percentile range for displaying the images.
     echo " "
     echo "  What percentile range do you want to use when displaying" 
     echo "  images? The default is 2,98."
     echo " "
     input "PERCENTILES - data percentiles > " (display_range)
{
{  Check the return for sense. If the abort signal has been given do so.
     if ( ( display_range = "!!" ) OR ~
          ( display_range = "!"  ) )
        signal escape
     endif
     if ( display_range = "" )
        display_range = "2,98"
     endif
     echo "Displaying using range " (display_range)

{  Due to ICL not handling vectors this needs to be split into the lower
{  and upper value and passed as percentiles=[ (rlow&','&rhigh)].
     rlow =  element(0, ",", display_range)
     rhigh = element(1, ",", display_range)
{
{  First get the names of groups of NDFs
     echo " "
     echo "  Give the names of a series of groups of NDFs whose positions "
     echo "  have not been moved on the sky. If all NDFs have been moved "
     echo "  then give a single NDF at each prompt. When all NDF groups/NDFs"
     echo "  have been given respond with a ! (null)"
     echo " "
{
{  Loop until no file is created.
     ok = true
     ngroup  = 1
     loop
        if ( ok )
           namelist = 'ccdalign_ndf' & (ngroup) & '.list'
           if ( file_exists( namelist) )
              sh \rm (namelist)
           endif
           ccdndfac ~
              namelist=(namelist) ~
              echo=true ~
              maxndf=100 ~
              reset
{          end ccdndfac
{
{  Now check that a file was created, failure indicates that no NDFs
{  where given.
           if ( file_exists( namelist ) )
              ngroup = ngroup + 1
           else
              ok = false
{
{  Trap the condition when no groups have been given.
              if ( ngroup = 1 )
                 echo "  No groups given"
                 signal escape
              endif
           endif
        else
           break
        endif
     endloop
{
{  Has the user a reference image in mind?
     echo " "
     echo "  If you have a reference image to which the other NDFs are to be"
     echo "  aligned then give its name at the next prompt. If no reference"
     echo "  NDF is specified (signified by a ! response) then the first NDF"
     echo "  of the first group will be used."
     echo " "
     namelist = "ccdalign_ref.list"
     if ( file_exists( namelist ) )
        sh \rm (namelist)
     endif
     ccdndfac ~
        namelist=(namelist) ~
        echo=true ~
        maxndf=1 ~
        reset
{    end ccdndfac
{
     if ( file_exists( namelist ) )
        have_reference = 1
     else
        have_reference = 0
     endif
{
{  Display the reference image or the very first NDF.
     if ( have_reference = 1 )
        open reference_list ccdalign_ref.list
        read reference_list (reference_ndf)
        close reference_list
     else
        open reference_list ccdalign_ndf1.list
        read reference_list (reference_ndf)
        close reference_list
     endif
     echo " "
     echo "  Using reference NDF " (reference_ndf)
     echo " "
{
{  Display this NDF.
     echo "  Displaying NDF "(reference_ndf)
     ref_obj = '@' & (reference_ndf)
     display  ~
        in=(ref_obj) ~
        mode=percentiles ~
        percentiles=[ (rlow&','&rhigh)] ~
        device=(display_device) ~
        accept
{    end display
{
{  Now use the cursor routine to read the image feature positions.
     echo " "
     echo "  Use the cursor to mark the image features. Remember the order"
     echo "  as this is important for later identifications."
     echo " "
{
{  Activate the cursor routine. If this the first NDF of the first group
{  then use all the NDF names of this group as the IN parameter. This will
{  associate this position list with all the NDFs.
     outlist = ( reference_ndf ) & '.fea'
     if ( have_reference = 1 )
        idicurs ~
           outlist=(outlist) ~
           device=(display_device) ~
           in=(reference_ndf) ~
           accept
{       end idicurs
     else
        idicurs ~
           outlist=(outlist) ~
           device=(display_device) ~
           in=^ccdalign_ndf1.list ~
           accept
{       end idicurs
     endif
{
{  Now plot the identifiers of the image features.
     plotlist ~
        logto=n ~
        inlist=(outlist) ~
        mtype=-1 ~
        device=(display_device) ~
        palnum = 3  ~
        ndfnames=false
{    end plotlist
{
{  See if user wants a hardcopy of the display.
     echo " "
     echo "  Do you want a hardcopy of the image display"
     echo " "
     input "HARDCOPY - produce hardcopy of display /TRUE/ > " (get_hardcopy)
{
{  Check the return for sense. If the abort signal has been given do so.
     if ( ( get_hardcopy = "!!" ) OR ~
          ( get_hardcopy = "!"  ) )
        signal escape
     else
{
{  Have a valid return of somekind. Default is true.
        if ( ( get_hardcopy = ""     ) OR ~
             ( get_hardcopy = "yes"  ) OR ~
             ( get_hardcopy = "YES"  ) OR ~
             ( get_hardcopy = "true" ) OR ~
             ( get_hardcopy = "TRUE" ) )
           make_copy = 1
        else
           make_copy = 0
        endif
     endif
{
{  Do the hardcopy if asked.
     if ( make_copy = 1 )
        input   "HARDDEV - name of hardcopy device > " (hard_device)
{
{  Check the return for sense. If the abort signal has been given do so.
        if ( ( hard_device = "!!" ) OR ~
             ( hard_device = "!"  ) )
           signal escape
        else
{
{  Get a snapshot.
           print_file = 'snapshot.ps'
           hard_device = '@' & (hard_device) & ';' & (print_file)
           if file_exists(print_file)
              sh \rm (print_file)
           endif
           print "Harddevice = " (hard_device)
           echo "  Capturing snapshot of display... Select portion of interest"
           snapshot ~
              device=(display_device) ~
              odevice=(hard_device) ~
              whole=false ~
              negative=false ~
              accept
{          end snapshot
{
{  Need to print the output. Get the name of the most recently created
{  file.
           print_prompt = "PRINT - print command /lpr "&(print_file)&" / > "
           input (print_prompt) (print_command)
           if ( print_command = "!!" ) OR ( print_command = "!"  )
              signal escape
           else
              if ( print_command = "" )
                 print_command = "lpr " & (print_file)
              endif
              sh (print_command)
           endif
        endif
     endif
{
{  Set the number of NDF groups we need to process.
     if ( have_reference = 1 )
        group_offset = 1
     else
        group_offset = 2
     endif
{
{  Introduction to next stage
     echo " "
     echo "  Now the first member of each NDF group or each NDF will be"
     echo "  displayed. You will then be given the opportunity to use the"
     echo "  cursor to mark the image features which correspond to those "
     echo "  which you marked on the first (reference) NDF. The order in"
     echo "  which you identify the image features must be the same. If an"
     echo "  image feature does not exist mark a position off the frame."
     echo "  You may extent the complete set of positions by indicating "
     echo "  image features after the last one in the reference set."
     echo " "
{
     loop
        if ( group_offset <> ngroup )
{
{  Extract the name of the first member of this group.
           open_file = 'ccdalign_ndf' & (group_offset) & '.list'
           open group_list (open_file)
           read group_list (this_ndf)
           close group_list
{
{  Display this NDF.
           echo "  Displaying NDF" (this_ndf)
           ndf_obj = '@' & (this_ndf)
           display ~
              in=(ndf_obj) ~
              mode=percentiles ~
              percentiles=[ (rlow&','&rhigh)]  ~
              device=(display_device) ~
              accept
{          end display
{
{  Activate the cursor routine. Use all the NDF names of this group as the IN
{  parameter. This will associate this position list with all the NDFs.
           outlist = (this_ndf) & '.fea'
           in = '^ccdalign_ndf' & (group_offset) & '.list'
           idicurs ~
              outlist=(outlist) ~
              device=(display_device) ~
              in=(in) ~
              accept
{          end idicurs
{
{  Increment offset for next group.
           group_offset = group_offset + 1
        else
           break
        endif
     endloop
{
{  Now centroid all the image feature positions to get accurate ones.
     echo " "
     echo "  Centroiding the image feature positions."
     echo " "
     group_offset = 1
     loop
        if ( group_offset <> ngroup )
           in = '^ccdalign_ndf' & (group_offset) & '.list'
           findcent ~
              in=(in) ~
              ndfnames=true ~
              outlist='*.acc' ~
              accept
{          end findcent
           group_offset = group_offset + 1
        else
           break
        endif
     endloop
{
{  See if the user wants to continue past this point.
     echo " "
     echo "  You may stop processing at this point if all you require are"
     echo "  labelled position lists associated with NDFs. If you want to"
     echo "  determine the NDF registrations, then this procedure will aid"
     echo "  this but only for linear transformations."
     echo " "
     input "CONTINUE - continue processing /TRUE/ > " (continue_proc)
     if ( NOT ( ( continue_proc = ""     ) OR ~
                 ( continue_proc = "yes"  ) OR ~
                 ( continue_proc = "YES"  ) OR ~
                 ( continue_proc = "true" ) OR ~
                 ( continue_proc = "TRUE" ) ) )
        signal escape
     endif
{
{  Don't forget the aborts
     if ( ( continue_proc = "!!" ) OR ~
          ( continue_proc = "!"  ) )
        signal escape
     endif
{
{  Will continue get the type of transformation to use.
     echo " "
     echo "  Ok will continue processing which type of transformation do"
     echo "  you require?"
     echo " "
     echo "     1 = shift of origin only"
     echo "     2 = shift of origin and rotation"
     echo "     3 = shift of origin and magnification"
     echo "     4 = shift of origin rotation and magnification"
     echo "     5 = full six parameter fit"
     echo " "
     again = true
     loop
        if ( again )
           input "FITTYPE - transformation type (1-5) /5/ > " (transformation)
{
{  Check the return for sense. If the abort signal has been given do so.
           if ( ( transformation = "!!" ) OR ~
                ( transformation = "!"  ) )
              signal escape
           else
{
{  Must be in the range 1 to 5
              if ( transformation = "" )
                 transformation = 5
                 again = false
              else
                 if ( transformation > 5 OR transformation < 1 )
                    echo " "
                    echo "  The value of this parameter must lie in the range"
                    echo "  1 to 5."
                    echo " "
                 else
                    again = false
                 endif
              endif
           endif
        else
           break
        endif
     endloop

{  Need a list of all the input NDFs.                                      }
     if file_exists('ccdalign_ndfs.list')
        ! \rm ccdalign_ndfs.list
     endif
     if ngroup < 9
        group_files = 'ccdalign_ndf[1-' & (ngroup) & '].list'
     else
        n = substr(ngroup,2,1)
        group_files = 'ccdalign_ndf[1-9][0-' & (n) & '].list ' & ~
                      'ccdalign_ndf[1-9]'
     endif
     if have_reference = 1
        ! cat ccdalign_ref.list (group_files) >ccdalign_ndfs.list
     else
        ! cat (group_files) >ccdalign_ndfs.list
     endif

{  Find the initial transformations, saving the extended reference set
{  to look for new positions on frames.
     echo " "
     echo "   Determining initial transformations."
     echo " "
     register ~
        fittype=(transformation) ~
        ndfnames=true ~
        inlist=^ccdalign_ndfs.list ~
        refpos=1 ~
        outref=ccdalign_ref.ext ~
        accept
{    end register
{
{  Now need to transform all the extended reference set to the coordinates of
{  all the other NDFs before re-centroiding to get accurate positions for any
{  image features beyond the initial reference set.
{  Associate extended reference set with all NDFs
{
     ccdedit logto=n ~
        mode=alist ~
           in=^ccdalign_ndfs.list ~
           inlist=ccdalign_ref.ext ~
           accept
{    end ccdedit
{
     tranlist logto=n ~
        ndfnames=true ~
        inlist=^ccdalign_ndfs.list ~
        outlist=*.ext ~
        inext=true ~
        forward=false ~
        trtype=struct ~
        accept
{    end tranndf
{
     findcent logto=n ~
        ndfnames=true ~
        outlist=*.acc ~
        in=^ccdalign_ndfs.list ~
        accept
{    end findcent
{
{  Now rework the transformations
     echo " "
     echo "   Determining final transformations."
     echo " "
     register ~
         fittype=(transformation) ~
         ndfnames=true ~
         inlist=^ccdalign_ndfs.list ~
         refpos=1 ~
         outref=ccdalign_ref.ext ~
         accept
{    end register
{
{  Set the goto behaviour
     exception escape
     end exception
end proc
{ $Id$
