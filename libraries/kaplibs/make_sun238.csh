#!/bin/tcsh
      echo "Gathering files to put in sun238"

#  Ensure the sdt grp command is available.
      source $SDT_DIR/startup.csh

#  Fetch the master tex file holdiong the fixed part of the document.
      fetch sun238.master

#  Create a temporary directory for the sources
      mkdir tsources

#  Empty all tar files into this directory
      cd tsources 
      foreach n (../*.tar)
         tar -xf $n
      end

#  Delete any files to be excluded.
      foreach n (`grp exclude`)
         rm -f $n
      end

#  Create a temporary directory for the docs
      cd ..
      mkdir tdocs

#  Move all the .f files which need to be documented in sun238.tex
#  into in the temporary docs directory.
      foreach n (ctg lpg aif fts1 ira kpg1)
         cp tsources/${n}_*.f tdocs >& /dev/null
      end

#  Remove the sources directory.
      rm -rf tsources

#  Move the master document into the docs directory
      cd tdocs
      mv ../sun238.master .

#  Run SST:prolat on all the .f files, putting the output in prolat.tex
      source $INSTALL/bin/sst/start
      echo "Running prolat on all .f files"
      rm -f prolat.tex
      foreach f (*.f)      
         rm -f .sst.tmp
         prolat in=$f out=$f.tex noatask single nopage nodocument >& $f.err
         grep "\!\!" $f.err
         if( $status == 1 ) then
            cat $f.tex  >> prolat.tex
            rm $f.err
         else
           cat $f.err
         endif
      end

#  Insert this file, together with a list of all routines, into sun238.tex
      echo "Running make_sun238.tcl"
      $KAPLIBS_DEV/bin/make_sun238.tcl

#  Copy the final sun238.tex to the parent directory.
      cp sun238.tex ..

#  Move to the parent directory.
      cd ..

#  Store a copy of the .tex file in KAPLIBS_SYS.
      cp -f sun238.tex $KAPLIBS_SYS

#  Create the hypertext docs.
      echo "Running star2html"
      star2html sun238.tex

#  Temporaily tar up the htx files.
      tar -cvhf tmp.tar sun238.htx

#  Create a htx.index file and move it into the tdocs directory.
      echo "Linking sun238.htx"
      unsetenv HTX_NOLINK
      hlink .
      mv sun238.htx/htx.index tdocs
      setenv HTX_NOLINK 1

#  Replaced the linked htx directory with the unlinked original.
      rm -rf sun238.htx
      tar -xf tmp.tar
      rm -f tmp.tar

#  Move into the tdocs directory and create the kaplibs.js file containing
#  a javascript description of the documented prologues. Move it into the
#  hypertext directory.
      cd tdocs
      echo "Making kaplibs.js"
      $KAPLIBS_DEV/bin/make-js.tcl kaplibs
      mv kaplibs.js ../sun238.htx
      cd ..

#  Move the other files needed for the prologue searching tool into the
#  hypertext directory.
      mv `grp searchtools` sun238.htx

#  Tar up the hypertext docs.
      tar -cvhf sun238.htx_tar sun238.htx
      rm -rf sun238.htx

#  Store a copy of the hypertext docs in KAPLIBS_SYS.
      cp -f sun238.htx_tar $KAPLIBS_SYS

#  Delete the docs directory.
      rm -rf tdocs
