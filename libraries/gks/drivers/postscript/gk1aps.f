*---------------------------------------------------------------
      SUBROUTINE GK1APS
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send PostScript Prologue to the external file and start first page
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*  DUMMY   - Dummy character, required by the buffering routine.
*  IREM    - Dummy integer, required by the buffering routine.
*  NBC     - Number of bytes in a colour code (1 monochrome, 3 colour)
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*

*     Integer workspace offset parameters
      INTEGER    IORIEN,   IWIDTH,   IFORMT,   ICOLR
      PARAMETER (IORIEN=14,IWIDTH=15,IFORMT=16,ICOLR=17)
*     Real  workspace offset parameters
      INTEGER    IMARGN
      PARAMETER (IMARGN=10)

*     Parameter indicating action for Page (not whole file)
      INTEGER    IPAGE
      PARAMETER (IPAGE=1)
*     Parameter indicating not EPSF
      INTEGER    NOEPSF
      PARAMETER (NOEPSF=0)
*
      INTEGER IREM, NBC
      CHARACTER DUMMY,S*80
*
*  ALGORITHM
*  ---------
*     Write PostScript prologue to the external file. Before the first
*     procedure, begin a new dictionary to contain all these and be removed
*     when the file is finished. This is desirable for EPSF. After the last
*     procedure initialise the device, send the End-Prologue and Page
*     comments and then do a save. This is to ensure that, regardless of
*     output device's processing habits, every page of our document is
*     going to be properly set-up.
*
*     There's a pattern in the way procedures are invoked by the driver.
*     For every output primitive there is a set attribute procedure, called
*     from the driver if any of the attributes affecting the primitive's
*     appearance has been changed. After the attributes have been sorted out,
*     the graphics state is saved, the primitive is invoked and executed and
*     afterwards the graphics state is restored.
*
*     Comments that follow show for each procedure (array) its
*     purpose, stack usage ang algorithm. "R" as a first letter in
*     a procedure argument denotes a real, "I"-integer, "C"-character.
*     Extra spaces in procedure definitions are to achieve "structured"
*     appearance of the output file. No standard exists nor has been
*     followed here.
*
* -----------------------------------------------------------------------------
*
*     Define Number of Bytes per Colourcode
      IF(KWKDAT(ICOLR,KWKIX) .EQ. GCOLOR)THEN
         NBC = 3
      ELSE
         NBC = 1
      ENDIF
*
*     Begin new dictionary with upto 200 entries
*     (must end it before trailer)
      CALL GKFOCO(KIOPB,'200 dict begin',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Define all necessary procedures (in alphabetical order).
*

*     Procedure align: Reduces Rnum1 so that it becomes Rnum2 - N*Rnum3
*                      (where N is an integer). Called by fapdo to adjust
*                      pattern bounding box left corner, so that pattern
*                      replication behaves in accordance with pattern size
*                      and pattern reference point.
*
*     Rnum1 Rnum2 Rnum3 align => Rnum1'
*
*     Algorithm: Subtract Rnum1 from Rnum2 and adjust the result so that it
*                becomes N*Rnum3. Use this to work out new Rnum1.
*

      CALL GKFOCO(KIOPB,'/align{2 copy exch 5 -1 roll sub'//
     : ' exch div ceiling mul sub}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure ca: reads a row of hexadecimal raster data from current file.
*                   Called from GK1ACA; fapi; GK1ARO.
*
*     Algorithm: Uses current file as the input stream and reads one hex
*                string into buffer line (defined elsewhere).

      CALL GKFOCO(KIOPB,
     : '/ca{currentfile line readhexstring pop}def',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure clnstat: sets colour, linewidth and linestyle.
*                        Called from GK1AST.
*
*     Rred Rblue Rgreen Iline_type Rline_width clnstat => -
*
*     Algorithm: Sets linewidth using first argument on the stack, takes second
*                argument (linestyle) and uses it to fetch appropriate dash
*                pattern definition from plsty array. Every element of this
*                definition is then scaled in proportion to the linewidth.
*                Resulting pattern is used as an argument to setdash. The
*                remaining three reals are fed to setrgbcolor operator for
*                grey level setting.

      CALL GKFOCO(KIOPB,'/clnstat{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'         setlinewidth plsty exch get',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'         [',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          exch',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'              {',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                currentlinewidth 1 1'//
     : ' itransform pop 0 0 itransform pop sub',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                10 mul div 1 add mul',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'              }',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          forall',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'         ]',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'         0 setdash',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'         setrgbcolor',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure fagen: forms but doesn't close the path. Called from GK1AFL
*
*     [RX_coordinates] [RY_coordinates] fagen => -
*
*     Algorithm: Assumes that the two arrays on the stack hold the
*                X-coordinates and Y-coordinates respectfully. Assumes
*                the graphics state has been saved in the calling
*                routine and a moveto to the first point of the array
*                has been been performed.
*                Uses length of the array to find out the number of
*                points and sets up a loop where for each point plseg
*                is called. Note that path is formed but not filled.
*

      CALL GKFOCO(KIOPB,'/fagen{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       dup length 1 sub',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       1 1 3 -1 roll',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          {plseg}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       for',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       pop pop',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure fapdo: outputs the required pattern to fill the
*                      generated path. Called from GK1AFL.
*
*     - fapdo => -
*
*     Algorithm: Closes the path (which it assumes had been formed and
*                unclosed), finds the bounding box and defines its lower
*                left and upper left coordinates (variables llx, lly, urx,
*                ury respectfully). It then adjsuts llx and lly so that
*                pattern replication starts properly at the reference point.
*                (this is done by procedure align). Following this, fapdo
*                goes on to calculate the number of pixels per scan line in
*                the bounding box, the number of bytes (nxbyte) in such
*                and the number of scan lines (nxpix and nypix).
*                Assuming that array patdef (->fapi) contains required pattern
*                definition in hexadecimal, fapdo then takes each row of this
*                definition  and concatenates it to itself, intermediate
*                results being stored in ww. When substring of ww containing
*                our pattern definition exceeds nxbyte+nxb elements in length,
*                we leave the loop. At the end of this process, thus, scan
*                array will contain as many scan-lines as there are rows in
*                patdef.
*                Then fapdo clips to the bounding box, translates to its lower
*                left corner and scales to its width and height, so as to
*                prepare the ground for the im procedure. At this stage faput
*                is called to fill the path.
*

      CALL GKFOCO(KIOPB,'/fapdo{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       closepath pathbbox',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          /ury exch def/urx exch def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          /lly exch refy psizey align def',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          /llx exch refx psizex align def',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          /nxpix urx llx sub psizex div nx'//
     : ' mul round cvi 1 add def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          /nypix ury lly sub psizey div ny'//
     : ' mul round cvi 1 add def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      WRITE(S,100) NBC
  100 FORMAT          ( '          /nxbyte nxpix ',I1,' mul def')
      CALL GKFOCO(KIOPB,S(1:33),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       /scan',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          ny',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'             {nxbyte string}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          repeat',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          ny array astore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       0 1 ny 1 sub',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          {',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           dup patdef exch get/ww'//
     : ' nxbyte nxb add 2 mul string def/indn nxb def',
     :  IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           ww 0 3 -1 roll putinterval ww',
     :  IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'              {',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'               0 indn getinterval ww exch'//
     : ' indn dup nxbyte nxb add',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'               lt',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                   {exch putinterval /indn'//
     : ' indn dup add def ww}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                   {',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                    pop 0 nxbyte nxb add'//
     : ' getinterval exch pop 0 nxbyte',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                    getinterval exit',
     :  IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                   }',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'               ifelse',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'              }',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'              loop',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           scan 3 1 roll put',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          }',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       for',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*     scan is ready - output the pattern.
      CALL GKFOCO(KIOPB,'       gsave',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          eoclip',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'             newpath',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                llx lly translate',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                nxpix psizex mul nx div'//
     : ' nypix psizey mul ny div scale',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

      CALL GKFOCO(KIOPB,'                faput',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       grestore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM )
      CALL GKFOCO(KIOPB,'      }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure fapi: Reads in and defines the reference point coordinates.
*                     and number of columns and rows in pattern definition.
*                     Called from GK1AFL.
*
*     INo_rows INo_columns RX_ref RY_ref RPatsize_X RPatsize_Y fapi => -
*
*     Algorithm: The pattern size dimensions and reference point coordinates
*                are defined; buffer line and array patdef prepared for reading
*                the pattern definition. Pattern definition is read in a loop
*                calling the ca procedure.
*
*

      CALL GKFOCO(KIOPB,'/fapi{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      /psizey exch def /psizex exch def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      /refy exch def/refx exch def/ny'//
     : ' exch def/nx exch def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      WRITE(S,110) NBC
  110 FORMAT          ( '      /nxb nx ',I1,' mul def')
      CALL GKFOCO(KIOPB,S(1:23),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      /line nxb string def /patdef ny array '//
     : ' def',IREM)

      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'         0 1 ny 1 sub ',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'            {patdef exch ca nxb string'//
     : ' copy put}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'         for',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'     }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure faput: using the image operator, outputs scan lines
*                      until the bounding box is filled. Called by fapdo.
*
*     - faput => -
*
*     Algorithm: Assumes that scan array scan holds ny-1 scan lines with nxpix
*                pixels each, formed by pattern replication. Temporary
*                variable temp serves as an index used to access the right
*                scan line from the array. It is initialised at the start,
*                then the environment for image is set up: width and height
*                of image (nxpix & nypix), number of bits/sample (8) and image
*                matrix (Note the negative number for height, due to different
*                way in which PostScript and GKS define the pattern) are pushed
*                on to stack. Procedure for image to get the correct scan line
*                is then defined and image operator invoked.

*                If the workstation is colour the colorimage operator is used
*                instead of the image operator. It has the multiproc argument
*                set to false and 3 values per colour.
      CALL GKFOCO(KIOPB,'/faput{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       /temp ny 1 sub nypix ny mod sub'//
     : ' def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       nxpix nypix 8'//
     : ' [nxpix 0 0 nypix neg 0 nypix]',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          {',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           /temp',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'              temp 1 add dup ny 1 sub le',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                                         {}',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                                         {'//
     : 'pop 0}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                                      ifelse'
     : ,IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           scan temp get',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          }',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       im',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure fasoldo: deals with solid interior style. Called from GK1AFL
*
*     - fasoldo => -
*
*     Algorithm: Assumes that path has been formed and unclosed. Uses
*                Even-Odd rule to close, clip and fill (eoclip could be
*                used before eofill to close the path and clip, but this
*                is redundant since eofill does it implicitly. Also, eoclip
*                has a bug!). *Temporarily put eoclip back*.
*

      CALL GKFOCO(KIOPB,'/fasoldo{eoclip eofill}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure fastat: defines fill area style attribute fsi.
*                       Called from GK1AFL
*
*     Ifa_style_index fat => -
*

      CALL GKFOCO(KIOPB,'/fastat{/fsi exch def}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure hitdot: in a device independent way, aligns pair of user
*                       coordinates to the nearest device dot. Used to be
*                       called by fapi, but (since pattern size introduced)
*                       now no longer. Left for possible future use.
*
*
*     RX_user_space RY_user_space hitdot => RXALD_user_space RYALD_user_space
*
*     Algorithm: takes the Current Transformation Matrix and passes the
*                input coordinate pair through it. Left on the stack are
*                two coordinates in true device raster space. Rounds them
*                to nearest integer and feeds to the itransform operator.
*
      CALL GKFOCO(KIOPB,'/hitdot{transform round exch round exch'//
     : ' itransform}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure im:  (colour)image operator. Called from GK1ACA & GK1ARO

*     Algorithm: if monochrome = image
*                if colour     = colorimage with multiproc set to false and
*                                3 values per colour
*
      IF(KWKDAT(ICOLR,KWKIX) .EQ. GCOLOR)THEN
         CALL GKFOCO(KIOPB,'/im{false 3 colorimage}def',IREM)
      ELSE
         CALL GKFOCO(KIOPB,'/im{image}def',IREM)
      ENDIF
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure init: initializes the device and graphics state.
*                     Called from GK1AID.
*
*     Ipage_orient Ipage_width Rmargin Ifile_format init => -
*
*     Algorithm: Define eflag (internal indicator of a file in EPSF format)
*                as 1. Take Ifile_format off the stack and define epsf -
*                current workstation's desired format. Take Rmargin off the
*                stack and define margin - indentation from the device's
*                imageable area left edge. Take Ipage_width and define delta,
*                i.e. page width in PostScript points. Then define pflag (
*                internal indicator of Portrait page orientation) as 0 and
*                put Ipage_orient into orient.
*                Define port and land. They are arrays which hold first 4
*                coefficients of a matrix we are to concatenate the CTM with
*                in order to get the proper page setup.
*                Use defaultmatrix to push CTM on the stack (can't use
*                initmatrix as might need to have EPSF).
*                For non-EPSF workstation get the imageable area of the device
*                and define its lower left corner coordinates llx and lly. For
*                an EPSF workstation set llx and lly to 0.
*                Depending on page orientation compose the modifying matrix,
*                then concatenate.
*                Set line join and line cap parameters to 1.
*     NOTE: any change of eflag/pflag values here must be followed
*                accordingly in GK1AID.
*
      CALL GKFOCO(KIOPB,'/init{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      /eflag 1 def/epsf exch def/margin'//
     : ' exch def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      1 sub/delta exch def/pflag 0 def'//
     : '/orient exch def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      /port [1 0 0 1] def/land [0 1 -1 0]'//
     : ' def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      epsf eflag eq',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                    {/llx 0 def/lly 0 def}',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                    {initmatrix',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                     clippath pathbbox',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                        pop pop/lly exch def'//
     : '/llx exch def}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                 ifelse',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      [',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       orient pflag eq',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                       {/off 0 def port}',
     :  IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                       {/off delta def land}',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'                    ifelse',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       aload pop off llx margin add add lly',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      ]',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      concat',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      1 setlinejoin 1 setlinecap',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      newpath',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'     }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure pln: forms the path for a long polyline. Does not stroke it.
*                    Called from plong.
*
*     [RX_coordinates] [RY_coordinates] plong => -
*
*     Algorithm: copy the arrays, get their respective 0-th elements and move
*                there. Use length of the array to find out the number of
*                points and set up a loop where for each point plseg is called.
*                Note that path is formed but not stroked.
*

      CALL GKFOCO(KIOPB,'/pln{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'     2 copy 0 get exch 0 get exch moveto dup'//
     : ' length 1 sub',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'     1 1 3 -1 roll',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        {plseg}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'     for',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'     pop pop',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'    }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure plong: polyline command for long polylines (i.e. those with
*                      >CUTOFF points, c.f. GK1ALN). Matching save is
*                      supplied in GK1ALN. Called from GK1ALN.
*
*     [RX_coordinates] [RY_coordinates] plong => -
*
*     Algorithm: Call pln to form the path and then stroke it.
*

      CALL GKFOCO(KIOPB,'/plong{pln stroke restore}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure pls: outputs short polylines (i.e. those with <CUTOFF points,
*                    c.f. GK1ALN). Called from GK1ALN.
*
*     RX_coordinates RY_coordinates pls => -
*
*     Algorithm: Moveto to first coordinate pair. Then lineto in a loop.
*

      CALL GKFOCO(KIOPB,'/pls{1 sub 3 1 roll moveto {lineto} repeat'//
     : ' stroke}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure plseg: draws a segment for the pln procedure. Called by
*                      (who else?!) pln.
*
*     [RX_coord] [RY_coord] Iloop_index plseg => [RX_coord] [RY_coord]
*
*     Algorithm: copies the loop index and coordinate arrays and uses these
*                to access Iloop_index-th coordinate pair. Then linesto.
*

      CALL GKFOCO(KIOPB,'/plseg{3 copy get 3 1 roll exch get exch'//
     : ' lineto}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Array plsty: holds dash patterns for various linestyles (in PS points!).
*                  Used by the clnstat procedure.

      CALL GKFOCO(KIOPB,
     : '/plsty[ [] [] [12 5.5] [0.5 2] [10 2 0.5 2]'//
     : ' [ 8 2 0.5 2 0.5 2] ]def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure pm: outputs polymarker. Called from GK1APM.
*
*     [RX_coordinates] [RY_coordinates] pm => -
*
*     Algorithm: Set up a loop and at each coordinate pair output a marker
*                by calling the pmdomk procedure.
*

      CALL GKFOCO(KIOPB,
     : '/pm{dup length 1 sub 0 1 3 -1 roll {pmdomk} for pop pop}def',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure pmdomk: draws one marker. Called by the pm procedure
*
*     [RX_coord] [RY_coord] Iloop_index pmdomk => [RX_coord] [RY_coord]
*
*     Algorithm: copies the loop index and coordinate arrays and uses these
*                to access Iloop_index-th coordinate pair. Translates, sets
*                the linetype to solid and uses marker size scale factor to
*                arrive at the figure for marker's size. Then the linewidth is
*                adjusted (must scale down, since scaled up for the size) and
*                marker drawn using array of procedures pmx.
*

      CALL GKFOCO(KIOPB,'/pmdomk{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        3 copy get 3 1 roll exch get'//
     : ' exch',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        gsave',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           translate [] 0 setdash',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           /tempwd currentlinewidth msz div'//
     : ' def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           msz dup scale',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           tempwd setlinewidth',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           newpath',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'              pmx mt get exec stroke',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        grestore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure pmstat: defines marker size and type. Called from GK1APM.
*
*     Imarker_type Imarker_size_scale_factor pmstat =>
*
      CALL GKFOCO(KIOPB, '/pmstat{/msz exch def/mt exch def}def', IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Array pmx: for each markertype holds a procedure to draw it's outline.
*                Used by pmdomk.
*
*     Algorithm: pmdomk has translated and scaled. Depending on the shape
*                desired, after moving to 0 0 we lineto around the origin.

      CALL GKFOCO(KIOPB,'/pmx[{}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*        1 -> smallest displayable dot
      CALL GKFOCO(KIOPB,'     {0 0 moveto 0 0 lineto}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*        2 -> the + sign
      CALL GKFOCO(KIOPB,'     {-.5 0 moveto .5 0 lineto 0 -.5 moveto'//
     : ' 0 .5 lineto}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*        3 -> the * sign, combining + and X
      CALL GKFOCO(KIOPB,'     {pmx 2 get exec pmx 5 get exec}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*        4 -> circle
      CALL GKFOCO(KIOPB,'     {.5 0 moveto 0 0 .5 0 360 arc}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*        5 -> X
      CALL GKFOCO(KIOPB,'     {-.45 -.45 moveto .45 .45 lineto'//
     : ' -.45 .45 moveto .45 -.45 lineto}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'    ]def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure s: replaces PostScript's save with a shorter command.
*                  Used in GK1ALN.

      CALL GKFOCO(KIOPB,'/s{save}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)


*     Procedure setclipbox: sets the clipping rectangle. Called from GK1ACB.
*
*     RXL RYB RXL RYT RXR RYT RXR RYB setclipbox => -
*
*     Algorithm: form a path using clipping rectangle coordinates, close it and
*                clip.

      CALL GKFOCO(KIOPB,'/setclipbox{newpath moveto lineto lineto'//
     : ' lineto closepath clip newpath}def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure txchar: outputs CHAR precision text. Called from GK1AXS.
*
*     Hex_String [RX_coord] [RY_coord] txchar => -
*
*     Algorithm: A loop is set up and for each coordinate pair the txdoch
*                procedure is called.
*

      CALL GKFOCO(KIOPB, '/txchar{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB, '        dup length 1 sub',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB, '        0 1 3 -1 roll',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB, '           {txdoch}',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB, '        for',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB, '        pop pop pop',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB, '       }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure txdoch: outputs one CHAR precision character. Called by the
*                       txchar procedure.
*     Hex_String [RX_coord] [RY_coord] Iloop_index txdoch =>  Hex_String
*                                                         [RX_coord][RY_coord]
*
*     Algorithm: Copy the arguments found on stack, use loop index to
*                access the Iloop_index-th coordinate pair and translate
*                to there. Rotate tbg degrees and scale in x-direction using
*                the expansion factor. Then get the right character from
*                the string and show it.
*
      CALL GKFOCO(KIOPB,'/txdoch{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        dup 4 1 roll 3 copy get 3 1 roll'//
     : ' exch get exch',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        gsave',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           translate',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           tng rotate',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           texp 1 scale',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           3 -1 roll 3 index exch 1'//
     : ' getinterval 0 0 moveto',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           show',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        grestore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)



*     Procedure txstat: sets up text attributes and the font. Called from
*                       GK1AXS.
*
*     CFont_name RCharacter_Height RCharacter_Rotation_Angle
*     RCharacter_Expansion_Factor txstat => -
*
*     Algorithm: Pick value for expansion factor off the stack and define texp.
*                Pick value for rotation angle off the stack and define tng.
*                Then take the CFont_Name and define ft. Find the requested
*                font, scale it to 1 point and determine the width of its
*                space character. Remembering that RCharacter_height is
*                actually a corrected width of a properly scaled requested
*                font (see GK1AXS and GK1AXF for explanation) and as such
*                is a function of a corrected character height, divide
*                RCharacter_Height by the 1 point font's space character
*                width and use this value to scale the font. Finally, reset
*                ft.
*

      CALL GKFOCO(KIOPB,'/txstat{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        /texp exch def /tng exch def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        gsave',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           /ft 3 -1 roll findfont def ft 1'//
     : ' scalefont setfont',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'           0 0 moveto ( ) stringwidth pop',
     : IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        grestore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        div ft exch scalefont setfont',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'        /ft 0 def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure txstr: outputs a STRING precision string. Called from
*                      GK1AXS.
*     Hex_String RX_coord RY_coord txstr => -
*
*     Algorithm: Copy the arguments found on stack, use loop index to
*                access the Iloop_index-th coordinate pair and translate
*                to there. Rotate tbg degrees and scale in x-direction using
*                the expansion factor. Then get the right character from
*                the string and show it.
*
      CALL GKFOCO(KIOPB,'/txstr{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       gsave',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          translate',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          tng rotate',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          texp 1 scale',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          0 0 moveto',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'          show',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'       grestore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'      }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*     Procedure tz: Add a trapezoid to the current path as a subpath.
*                   It is used to provide a compact way of putting
*                   trapezoids into a file when trapezoid decomposition
*                   is used. Called from GK1ATF.
*
*     ulx llx urx lrx uy ly => -
*
*     Algorithm: Copies the two Y values swaps them and then pairs
*                with corresponding X values putting into the current
*                path using 'moveto' first then 'lineto'.
*
*     The effect of 'ulx llx urx lrx uy ly tz' is the same as
*
*     'ulx uy moveto llx ly lineto lrx ly lineto urx uy lineto'.
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'/tz{',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'    2 copy exch',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'    8 -1 roll exch moveto',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'    6 -1 roll exch lineto',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'    3 -1 roll exch lineto lineto',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'   }def',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*
*     Initialise the page (if we do it here and save the VM state,
*     showpage will not erase the initial page settings).
*
      WRITE(S,1000) KWKDAT(IORIEN,KWKIX), KWKDAT(IWIDTH,KWKIX),
     :              QWKDAT(IMARGN,KWKIX), KWKDAT(IFORMT,KWKIX)
 1000 FORMAT(I1, I5, F9.3, I3, ' init')
      CALL GKFOCO(KIOPB, S(1:23), IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Mark end of the prologue and beginning of Page 1
*
      CALL GKFOCO(KIOPB,'%%EndProlog',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'%%Page: ? 1',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*
*     Send Page Header, if not EPSF
*
      IF(KWKDAT(IFORMT,KWKIX) .EQ. NOEPSF) CALL GK1AHD (IPAGE)

*
*     Do the first save - matching restore will be supplied at the
*     end of each frame.
*
      CALL GKFOCO(KIOPB,'save',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

      END
