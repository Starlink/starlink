$!+
$!  Name:
$!     CCDALIGN

$!  Purpose:
$!     Interactive procedure to aid the alignment of NDFs.

$!  Language:
$!     DCL

$!  Description:
$!      This procedure aids the registration of NDFs which cannot be
$!      registered using simple offsetting techniques (see FINDOFF and
$!      PAIRNDF). It also has the capability of dealing with groups of
$!      NDFs which are almost registered (frames which have not been
$!      moved on the sky) saving effort in re-identification of image
$!      features.
$!
$!      The basic method used is to access groups of NDFs (or a series of
$!      single NDFs if all are moved sufficiently between exposures) and
$!      an optional reference NDF. The first NDF of the first group or
$!      the reference NDF is then displayed and a cursor application is
$!      used to record the positions of centroidable image features. The
$!      first NDFs of all the other groups are then displayed and you
$!      are invited to identify the image features in the order which
$!      corresponds to that used for the reference NDF. Missing image
$!      features are identified as off the currently displayed image (so
$!      the centroid routine will fail to find them). The reference set
$!      of image features may be extended by identification after the last
$!      reference feature has been marked.
$!
$!      After centroiding you are then given the option to stop. If
$!      you decide to then you will have labelled position lists to use
$!      in the other CCDPACK routines (the labelled positions will be
$!      called NDF_NAME.acc). If you chose the option to continue then
$!      a full registration of the NDFs will be attempted. This may only
$!      be performed for "linear" transformations.
$!
$!      After choosing a transformation type the procedure will then go on
$!      to calculate a transformation set between all the NDFs, this is
$!      then used (with the extended reference set from REGISTER) to
$!      approximate the position of all possible image features, these are
$!      then located by centroiding and a final registration of all NDFs
$!      is performed. The resultant NDFs then have associated lists of
$!      labelled positions and TRANSFORM structures which may be used to
$!      transform other position lists or when resampling the data.

$!  ADAM parameters:
$!     NONE

$!  Authors:
$!     PDRAPER: Peter Draper (STARLINK)
$!     {enter_new_authors_here}

$!  History:
$!     15-JUN-1993 (PDRAPER):
$!        Original version.
$!     {enter_changes_here}
$!-
$!
$!  Symbols for writing output
$      echo:=write sys$output
$!
$!  Introduce the application:
$      echo " "
$      echo "    CCDALIGN"
$      echo "    ========"
$      echo " "
$      echo "  An interactive aid for aligning groups of NDFs."
$
$!
$!  Initialise the appropriate application packages.
$     kappa
$     ccdpack
$!
$!  Set the interrupt behaviour
$     on warning then goto tidy_up
$     on control_y then goto tidy_up
$!
$!  Get the name of a display device.
$     echo " "
$     echo "  Give the name of an image display device"
$     echo " "
$     inquire/nopunctuation display_device "DEVICE - Image display device > "
$!
$!  Check the return for sense. If the abort signal has been given do so.
$     if ( ( display_device .eqs. "!!" ) .or. -
           ( display_device .eqs. ""   ) .or. -
           ( display_device .eqs. "!"  ) ) then goto tidy_up
$!
$!  First get the names of groups of NDFs
$     echo " "
$     echo "  Give the names of a series of groups of NDFs whose positions "
$     echo "  have not been moved on the sky. If all NDFs have been moved "
$     echo "  then give a single NDF at each prompt. When all NDF groups/NDFs"
$     echo "  have been given respond with a ! (null)"
$     echo " "
$!
$!  Loop until no file is created.
$     ok = 1
$     ngroup  = 1
$while_loop1:
$     if ( ok )
$     then
$        namelist = "ccdalign_ndf''ngroup'.list"
$        if ( f$search( namelist ) .nes. "" )
$        then
$           delete/nolog/noconfirm 'namelist';*
$        endif
$        define/user_mode sys$input sys$command
$        ccdndfac -
            namelist='namelist' -
            echo=true -
            maxndf=100 -
            reset
$!       end ccdndfac
$
$!
$!  Now check that a file was created, failure indicates that no NDFs
$!  where given.
$        if ( f$search( namelist ) .nes. "" )
$        then
$           ngroup = ngroup + 1
$        else
$           ok = 0
$!
$!  Trap the condition when no groups have been given.
$           if ( ngroup .eq. 1 )
$           then 
$              echo "  No groups given"
$              goto tidy_up
$           endif
$        endif
$        goto while_loop1
$     endif
$!
$!  Has the user a reference image in mind?
$     echo " "
$     echo "  If you have a reference image to which the other NDFs are to"
$     echo "  aligned then give its name at the next prompt. If no reference"
$     echo "  NDF is specified (signified by a ! reponse) then the first NDF"
$     echo "  of the first group will be used."
$     echo " "
$     namelist = "ccdalign_ref.list"
$     if ( f$search( namelist ) .nes. "" )
$     then
$        delete/nolog/noconfirm 'namelist';*
$     endif
$     define/user_mode sys$input sys$command
$     ccdndfac -
         namelist='namelist' -
         echo=true -
         maxndf=1 -
         reset
$!    end ccdndfac
$
$     if ( f$search( namelist ) .nes. "" )
$     then
$        have_reference = 1
$     else
$        have_reference = 0
$     endif
$!
$!  Display the reference image or the very first NDF.
$     if ( have_reference )
$     then
$        open reference_list ccdalign_ref.list
$        read reference_list reference_ndf
$        close reference_list
$     else
$        open reference_list ccdalign_ndf1.list
$        read reference_list reference_ndf
$        close reference_list
$     endif
$     echo " "
$     echo "  Using reference NDF ''reference_ndf'"
$     echo " "
$!
$!  Display this NDF.
$     echo "  Displaying NDF ''reference_ndf'"
$     define/user_mode sys$input sys$command
$     display  -
         in='reference_ndf' -
         mode=percentiles -
         percentiles=[2,98] -
         device='display_device' -
         accept
$!    end display
$
$!
$!  Now use the cursor routine to read the image feature positions.
$     echo " " 
$     echo "  Use the cursor to mark the image features. Remember the order"
$     echo "  as this is important for later identifications."
$     echo " "
$!
$!  Activate the cursor routine. If this the first NDF of the first group
$!  then use all the NDF names of this group as the IN parameter. This will
$!  associate this position list with all the NDFs.
$     if ( have_reference )
$     then
$        define/user_mode sys$input sys$command
$        idicurs -
            outlist='reference_ndf'.fea -
            device='display_device' -
            in='reference_ndf' -
            accept
$!       end idicurs
$     else
$        define/user_mode sys$input sys$command
$        idicurs -
            outlist='reference_ndf'.fea -
            device='display_device' -
            in=^ccdalign_ndf1.list -
            accept
$!       end idicurs
$     endif
$!
$!  Now plot the identifiers of the image features.
$     define/user_mode sys$input sys$command
$     plotlist -
         logto=n -
         inlist='reference_ndf'.fea -
         mtype=-1 -
         device='display_device' -
         palnum = 3 -
         ndfnames=false
$!    end plotlist
$
$!
$!  See if user wants a hardcopy of the display.
$     echo " "
$     echo "  Do you want a hardcopy of the image display"
$     echo " "
$     inquire/nopunctuation get_hardcopy -
         "HARDCOPY - produce hardcopy of display /TRUE/ > "
$!
$!  Check the return for sense. If the abort signal has been given do so.
$     if ( ( get_hardcopy .eqs. "!!" ) .or. -
           ( get_hardcopy .eqs. "!"  ) )
$     then
$        goto tidy_up
$     else
$!
$!  Have a valid return of somekind. Default is true.
$        if ( ( get_hardcopy .eqs. ""     ) .or. -
              ( get_hardcopy .eqs. "yes"  ) .or. -
              ( get_hardcopy .eqs. "YES"  ) .or. -
              ( get_hardcopy .eqs. "true" ) .or. -
              ( get_hardcopy .eqs. "TRUE" ) ) 
$        then
$           make_copy = 1
$        else
$           make_copy = 0
$        endif
$     endif
$!
$!  Do the hardcopy if asked.
$     if ( make_copy ) 
$     then
$        inquire/nopunctuation hard_device -
            "HARDDEV - name of hardcopy device > "
$!
$!  Check the return for sense. If the abort signal has been given do so.
$        if ( ( hard_device .eqs. "!!" ) .or. -
              ( hard_device .eqs. "!"  ) )
$        then
$           goto tidy_up
$        else
$!
$!  Get a snapshot.
$           echo "  Capturing snapshot of display... Select portion of interest"
$           define/user_mode sys$input sys$command
$           snapshot -
               device='display_device' -
               odevice='hard_device' -
               whole=false -
               negative=false -
               accept
$!          end snapshot
$
$!
$!  Need to print the output. Get the name of the most recently created
$!  file.
$           directory/nohead/notrail/width=(display=256,filename=132) -
                     /since/columns=1/output=ccdalign_junk.tmp/date
$           sort /key=(position=147,size=2,descending) -
                 /key=(position=150,size=2,descending) -
                 /key=(position=153,size=5,descending) -
                 ccdalign_junk.tmp ccdalign_junk.tmp
$           open junk_file ccdalign_junk.tmp
$           read junk_file print_file
$           read junk_file print_file
$           print_file = f$extract(0,f$locate(" ",print_file),print_file)
$           close junk_file
$           inquire/nopunctuation print_command -
            "PRINT - print command /print ''print_file' / > "
$           if ( ( print_command .eqs. "!!" ) .or. -
                 ( print_command .eqs. "!"  ) )
$           then
$              goto tidy_up
$           else
$              if ( print_command .eqs. "" )
$              then
$                 print_command = "print ''print_file'"
$              endif
$              'print_command'
$           endif
$        endif
$     endif
$!
$!  Set the number of NDF groups we need to process.
$     if ( have_reference )
$     then
$        group_offset = 1
$     else
$        group_offset = 2
$     endif
$!
$!  Introduction to next stage
$     echo " "
$     echo "  Now the first member of each NDF group or each NDF will be"
$     echo "  displayed. You will then be given the opportunity to use the"
$     echo "  cursor to mark the image features which correspond to those "
$     echo "  which you marked on the first (reference) NDF. The order in"
$     echo "  which you identify the image features must be the same. If an"
$     echo "  image feature does not exist mark a position off the frame."
$     echo "  You may extent the complete set of positions by indicating "
$     echo "  image features after the last one in the reference set."
$     echo " "
$!
$while_loop2:
$     if ( group_offset .ne. ngroup )
$     then
$!
$!  Extract the name of the first member of this group.
$        open group_list ccdalign_ndf'group_offset'.list
$        read group_list this_ndf
$        close group_list
$!
$!  Display this NDF.
$        echo "  Displaying NDF ''this_ndf'"
$        define/user_mode sys$input sys$command
$        display -
            in='this_ndf' -
            mode=percentiles -
            percentiles=[2,98] -
            device='display_device' -
            accept
$!       end display
$
$!
$!  Activate the cursor routine. Use all the NDF names of this group as the IN
$!  parameter. This will associate this position list with all the NDFs.
$        define/user_mode sys$input sys$command
$        idicurs -
            outlist='this_ndf'.fea -
            device='display_device' -
            in=^ccdalign_ndf'group_offset'.list -
            accept
$!       end idicurs
$
$!
$!  Increment offset for next group.
$        group_offset = group_offset + 1
$        goto while_loop2
$     endif
$!
$!  Now centroid all the image feature positions to get accurate ones.
$     echo " "
$     echo "  Centroiding the image feature positions."
$     echo " "
$     group_offset = 1
$while_loop3:
$     if ( group_offset .ne. ngroup )
$     then
$        define/user_mode sys$input sys$command
$        findcent -
            in=^ccdalign_ndf'group_offset'.list -
            ndfnames=true -
            outlist='*.acc' -
            accept
$!       end findcent
$
$        group_offset = group_offset + 1
$     endif
$!
$!  See if the user wants to continue past this point.
$     echo " "
$     echo "  You may stop processing at this point if all you require are"
$     echo "  labelled postion lists associated with NDFs. If you want to"
$     echo "  determine the NDF registrations, then this procedure will aid"
$     echo "  this but only for linear transformations"
$     echo " "
$     inquire/nopunctuation continue_proc -
         "CONTINUE - continue processing /TRUE/ > "
$     if ( .not. ( ( continue_proc .eqs. ""     ) .or. -
                   ( continue_proc .eqs. "yes"  ) .or. -
                   ( continue_proc .eqs. "YES"  ) .or. -
                   ( continue_proc .eqs. "true" ) .or. -
                   ( continue_proc .eqs. "TRUE" ) ) )
$     then
$        goto tidy_up
$     endif
$!
$!  Don't forget the aborts
$     if ( ( continue_proc .eqs. "!!" ) .or. -
           ( continue_proc .eqs. "!"  ) )
$     then
$        goto tidy_up
$     endif
$!
$!  Will continue get the type of transformation to use.
$     echo " "
$     echo "  Ok will continue processing which type of transformation do"
$     echo "  you require?"
$     echo " "
$     echo "     1 = shift of origin only"
$     echo "     2 = shift of origin and rotation"
$     echo "     3 = shift of origin and magnification"
$     echo "     4 = shift of origin rotation and magnification"
$     echo "     5 = full six parameter fit"
$     echo " "
$
$get_transformation_again:
$     inquire/nopunctuation transformation -
         "FITTYPE - transformation type (1-5) /5/ > "
$!
$!  Check the return for sense. If the abort signal has been given do so.
$     if ( ( transformation .eqs. "!!" ) .or. -
           ( transformation .eqs. "!"  ) )
$     then
$        goto tidy_up
$     else
$!
$!  Must be in the range 1 to 5
$        if ( transformation .eqs. "" )
$        then
$           transformation = 5
$        else
$           if ( transformation .gt. 5 .or. transformation .lt. 1 ) 
$           then
$              echo " "
$              echo "  The value of this parameter must lie in the range"
$              echo "  1 to 5."
$              echo " "
$              goto get_transformation_again
$           endif
$        endif
$     endif
$
$!
$!  Need a list of all the input NDFs.
$     group_offset=1
$     if ( f$search( "ccdalign_ndfs.list" ) .nes. "" )
$     then
$        delete/nolog/noconfirm ccdalign_ndfs.list;*
$     endif
$     if ( have_reference ) 
$     then
$        copy/nolog/noconfirm ccdalign_ref.list ccdalign_ndfs.list
$        group_offset=1
$     else
$        copy/nolog/noconfirm ccdalign_ndf1.list ccdalign_ndfs.list
$        group_offset=2
$     endif
$while_loop4:
$     if ( group_offset .ne. ngroup )
$     then
$        append/nolog/noconfirm ccdalign_ndf'group_offset'.list -
                                ccdalign_ndfs.list
$        group_offset = group_offset + 1
$     endif
$!
$!  Find the initial transformations, saving the extended reference set
$!  to look for new positions on frames. 
$     echo " "
$     echo "   Determining initial transformations."
$     echo " "
$     define/user_mode sys$input sys$command
$     register -
         fittype='transformation' -
         ndfnames=true -
         inlist=^ccdalign_ndfs.list -
         refpos=1 -
         outref=ccdalign_ref.ext -
         accept
$!    end register
$
$!
$!  Now need to transform all the extended reference set to the coordinates of
$!  all the other NDFs before re-centroiding to get accurate positions for any
$!  image features beyond the initial reference set.
$!  Associate extended reference set with all NDFs
$!
$     define/user_mode sys$input sys$command
$     ccdedit logto=n -
$        mode=alist -
$        in=^ccdalign_ndfs.list -
$        inlist=ccdalign_ref.ext -
$        accept
$!    end ccdedit
$!
$     define/user_mode sys$input sys$command
$     tranndf logto=n -
$        ndfnames=true -
$        inlist=^ccdalign_ndfs.list -
$        outlist=*.ext -
$        inext=true -
$        forward=false -
$        trtype=struct -
$        accept
$!    end tranndf
$!
$     define/user_mode sys$input sys$command
$     findcent logto=n -
$        ndfnames=true -
$        outlist=*.acc -
$        in=^ccdalign_ndfs.list -
$        accept
$!    end findcent
$!
$!  Now rework the transformations
$     echo " "
$     echo "   Determining final transformations."
$     echo " "
$     define/user_mode sys$input sys$command
$     register -
         fittype='transformation' -
         ndfnames=true -
         inlist=^ccdalign_ndfs.list -
         refpos=1 -
         outref=ccdalign_ref.ext -
         accept
$!    end register
$!  Exit label
$tidy_up:
$!
$!  Close any files which may be opened
$!
$     close/nolog reference_list
$     close/nolog junk_file
$     close group_list
$exit:
$! $Id$
