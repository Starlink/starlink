#!/bin/tcsh 

# Create file "documented_files" in each subdirectory containing the
# names of the files to be documented.

foreach dir (aif ctg fts ira irq kpg lpg)
   cd $dir

# Get a basic list of files. For IRQ and IRA, only document IRQ_ and IRA_
# routines (not IRQ1_ or IRA1_).
   rm -f temp-files >& /dev/null
   touch temp-files

   if( $dir != "irq" && $dir != "ira" ) then 
      foreach file (*.f *.g* *.c)
         echo $file >> temp-files
      end

   else
      foreach file (${dir}_*.f ${dir}_*.g*)
         echo $file >> temp-files
      end
   endif

# Only document those files that are mentioned in Makefile.am and are not
# blockdata or common block definition files.
   rm -f documented_files >& /dev/null
   touch documented_files

   foreach file (`cat temp-files`)
      grep -q $file Makefile.am
      set s1 = $status

      grep -q "BLOCK DATA" $file
      set s2 = $status

      if( $s1 == 0 && $s2 == 1 ) then
         echo $file >> documented_files
      else
         echo "Ignoring $dir/$file"
      endif
   end

   rm -rf temp-files >& /dev/null

   cd ..
end

# Copy the sun header to the output tex file.
cp sun_head.tex sun238.tex

# Add a noteroutine for each documented file
foreach dir (aif ctg fts ira irq kpg lpg)
   cd $dir
   rm -f notes >& /dev/null
   ../make_noteroutines.pl > notes
   grep -q \!\!\! notes
   if( $status == 0 ) then
      cat notes
      exit
   endif
   cat notes >> ../sun238.tex
   cd ..
end

# Copy the middle section of the fixed text 
cat sun_mid.tex >> sun238.tex

# Add an sstroutine for each documented file
foreach dir (aif ctg fts ira irq kpg lpg)
   cd $dir

   foreach file ( `cat documented_files`)
      set name = `echo $file | sed -e 's/\.[^\.]*$//'`
      rm -f ${name}.tex >& /dev/null
      $STARCONF_DEFAULT_STARLINK/bin/sst/prolat $file out=${name}.tex \
                                   noatask nodocument single nopage > /dev/null
      cat ${name}.tex >> ../sun238.tex
      rm -f ${name}.tex >& /dev/null
   end

   rm -f documented_files >& /dev/null
   cd ..
end

# Copy the footer
cat sun_tail.tex >> sun238.tex
