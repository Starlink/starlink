#!/bin/tcsh

# Create file "documented_files" in each subdirectory containing the
# names of the files to be documented.

foreach dir (aif ccg fts ira kpg)
   cd $dir

# Get a basic list of files. For IRA, only document IRA_ routines
# (not IRA1_). C files are only included if they are designed to be
# called from Fortran (as indicated by the presence of the string
# "F77_" somewhere in the file).
   rm -f temp-files conly-files >& /dev/null
   touch temp-files conly-files

   if( $dir != "ira" ) then
      foreach file (*.f *.g* *.c)
         if( $file != "kaplibs.c" && $file != "kaplibs_adam.c" ) then

            set ok = 1
            grep -q "^#include " $file
            if( $status == 0 ) then
               grep -q "F77_" $file
               if( $status == 1 ) then
                  set ok = 0
                  echo $file >> conly-files
               endif
            endif

            if( $ok == 1 ) then
               echo $file >> temp-files
            endif

         endif
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

   foreach file (`cat temp-files | sort `)
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
foreach dir (aif ccg fts ira kpg)
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

# Add an sstroutine for each Fortran-callable documented file
foreach dir (aif ccg fts ira kpg)
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

# Add an sstroutine for each C-callable documented KPG file
echo '\\newpage' >> sun238.tex
echo '\section{C-only Routine Descriptions}' >> sun238.tex

foreach dir (kpg)
   cd $dir

   foreach file ( `cat conly-files | sort`)
      set name = `echo $file | sed -e 's/\.[^\.]*$//'`
      rm -f ${name}.tex >& /dev/null
      $STARCONF_DEFAULT_STARLINK/bin/sst/prolat $file out=${name}.tex \
                                   noatask nodocument single nopage > /dev/null
      cat ${name}.tex >> ../sun238.tex
      rm -f ${name}.tex >& /dev/null
   end

   rm -f conly-files >& /dev/null
   cd ..
end


# Copy the footer
cat sun_tail.tex >> sun238.tex
