#!/bin/tcsh
      echo "Editing routine names and purposes into kplsearch"

#  Ensure the sdt grp command is available.
      source $SDT_DIR/startup.csh

#  Create a temporary directory
      mkdir tdir

#  Copy all required .f files into in the temporary directory.
      foreach n (aif fts1 ira kpg1)
         cp ${n}_*.f tdir >& /dev/null
      end

#  Now get the ctg and lpg .f files.
      mkdir ctglpg
      cd ctglpg
      tar -xf ../ctg_source.tar
      tar -xf ../lpg_source.tar
      foreach n (ctg lpg)
         cp ${n}_*.f ../tdir >& /dev/null
      end
      cd ..
      rm -rf ctglpg

#  Copy the template kpsearch script into the temp directory
      cp kplsearch tdir

#  Move into the temporary directory.
      cd tdir

#  Delete any files to be excluded.
      foreach n (`grp exclude`)
         rm -f $n
      end

#  Insert a list of all routines into kplsearch.
      $KAPLIBS_DEV/bin/make_kplsearch.tcl

#  Copy the final kplsearch to the parent directory.
      cp kplsearch ..

#  Delete the temp directory.
      cd ..
      rm -rf tdir

