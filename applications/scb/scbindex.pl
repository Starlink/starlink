#!/usr/local/bin/perl -w

#+
#  Name:
#     scbindex.pl

#  Purpose:
#     Make index of all Starlink source code files.

#  Language:
#     Perl 5

#  Description:
#     This program attempts to find all the source code files which form
#     part of the USSC.  Given a base directory $srcdir, typically
#     /star/sources, it looks underneath it for anything which
#     looks like a source file, specifically files with the following 
#     extensions: 
#        .f .gen .c .h
#     These files may be sitting 'naked' in one of the subordinate 
#     directories, or may be in a (compressed or uncompressed) tar file,
#     or may be in a tar file within such a tar file (nested to an 
#     arbitrary level, although not usually deeper than 2).
#
#     When such a file is found, it is searched for program modules
#     which could be referred to by other programs (e.g. functions and
#     subroutines).  For each instance of such a module, the name of 
#     the module and information about its location is written to a 
#     DBM file ($indexfile).
#
#     The resulting DBM file $indexfile can then be read by a separate 
#     program to locate the source code for a module, using no 
#     information other than its name, and perhaps what package it is
#     part of.
#
#     Additionally a list of packages, and tasks within each package,
#     is written out to a second file.
#
#  Index DBM file format.
#     The index file named $indexfile and stored as a Perl SDBM file.  
#     This resembles a Unix dbm(3) file and is therefore an unordered 
#     set of (key, value) pairs.
#
#     For each record, the key is just the name of the module 
#     (usually, just the name of the subroutine, or the include file).
#
#     The value is a 'location', that is, a string containing one or
#     more space-separated 'logical pathnames'.
#     A logical pathname resembles an ordinary pathname except that
#     it starts with the sequence "package#", where "package" is the
#     name of a Starlink package.  Furthermore, a logical path 
#     may contain the special character '>' which indicates
#     inclusion in a (possibly compressed) tar file.  Thus some
#     example logical pathnames would be:
#
#         AST#ast_source.tar>frame.f
#         FIGARO#figaro_applic.tar.Z>applic/bclean.f
#
#     A 'package' is identified as a directory
#     immediately below the base directory ($srcdir) or a (possibly 
#     compressed) tar file in the base directory.  Thus the tar file
#     AST#ast_source.tar may be stored either in the directory 
#     $srcdir/ast, or in the tar file $srcdir/ast.tar.Z.
#     No distinction between the two is made; so that an index will
#     remain valid if a package is compressed or extracted according
#     to this scheme.
#
#     A 'location' can contain more than one logical pathname because
#     there may be modules in different packages which share the 
#     same name.  There ought not to be more than one pathname per
#     package in a given location.
#
#     If there are any other files in $srcdir apart from directories 
#     and tar files (there shouldn't be) then they will be indexed 
#     with the pseudo-package name 'SOURCE'.
#
#     As well as scanning all files under $srcdir, the program also
#     indexes all modules (presumed to be include files) from a second 
#     specified directory (typically /star/include), by filename 
#     (capitalised if it looks like a fortran include) rather than 
#     module name.  These packages are indexed with the pseudo-
#     package name 'INCLUDE'.
#
#  Task file format.
#     An file name $taskfile is also written containing names of
#     all packages, and the tasks within each package.  Althought the
#     file is plain text, some of the lines may be quite long.
#
#     Each line is of the form
#
#        package: task1 task2 task3 ...
#
#     where 'package' is a package name as defined above (every task
#     containing at least one module indexed in $indexfile is included
#     exactly once) and the tasks are supposed to be modules of 
#     particular importance.  Their meaning is not defined other than 
#     that, but they should typically be commands which can be invoked
#     from the command line.  Identifying them is a haphazard business,
#     and the method for this is explained elsewhere in this source
#     file.

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     24-AUG-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Flags.

$verbose = 1;

#  Shell commands required.

$generic = "generic";

#  Required libraries.

use Fcntl;
use Cwd;
use SDBM_File;
use libscb;

#  Debug: override source directory location.

$srcdir = "/local/star/src/from-ussc";

#  Declarations.

sub index_list;
sub index_incdir;
sub index_dir;
sub index_tar;
sub index_hlp;
sub index_f;
sub index_c;
sub index_h;
sub index_gen;
sub indexinc_list;
sub write_entry;

#  Set up scratch directory.

$tmpdir = "/local/junk/scb/index";
print "rm -rf $tmpdir\n" if $verbose;
system "rm -rf $tmpdir" and die "Couldn't clean out $tmpdir: $?\n";
system "mkdir -p $tmpdir" and die "Couldn't create $tmpdir: $?\n";

#  Initialise and open index file.

unlink "$indexfile.pag", "$indexfile.dir";
tie %locate, SDBM_File, $indexfile, O_CREAT | O_RDWR, 0644;

#  Index source files.

chdir $srcdir or die "Couldn't enter $srcdir\n";
index_dir "SOURCE#", ".";
write_entry "SOURCE#", $srcdir;

#  Index files from include directory.

chdir $incdir or die "Couldn't enter $incdir\n";
index_incdir "INCLUDE#", ".";

#  Write checked task names out to text file.
#  Currently, a task is identified as the text of any entry in a .hlp file
#  for which a module of the same name exists in the same package.

open TASKS, ">$taskfile" or die "Couldn't open $taskfile\n";
foreach $package (sort keys %tasks) {
   $npackages++;
   print TASKS "$package:";
   print "$package:" if $verbose;
   foreach $task (sort @{$tasks{$package}}) {
      if ($locate{$task}) {
         foreach $path (split ' ', $locate{$task}) {
            if (starpack ($path) eq $package) {
               print TASKS " $task";
               print       " $task" if $verbose;
               next;
            }
         }
      }
   }
   print TASKS "\n";
   print       "\n" if $verbose;
}
close TASKS;

#  Print statistics

if ($verbose) {
   printf "\nSTATISTICS ($npackages packages):\n";
   printf "%20s %10s %10s\n\n", "File type", "Files", "Lines";
   foreach $ftype (keys %nfiles) {
      $nfiles += $nfiles{$ftype};
      $nlines{$ftype} ||= 0;
      $nlines += $nlines{$ftype};
      printf "%20s %10d %10d\n", $ftype, $nfiles{$ftype}, $nlines{$ftype};
   }
   printf "%20s %10s %10s\n", ' ', '--------', '--------';
   printf "%20s %10d %10d\n", ' ', $nfiles, $nlines;
}

#  Terminate processing.

untie %locate;
untie %tasks;

exit;

########################################################################
#  Subroutines.
########################################################################

########################################################################
sub index_list {

#  Examine and index a list of source files; identify each file as a 
#  naked file of an interesting type, a tar file, or a directory, and 
#  hand off to a routine designed to deal with that type.  
#  Any other files are ignored.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my @files = @_;        #  Files in current directory to be indexed.

#  Rephrase logical path as a package reference if it looks like it needs 
#  doing i.e. if it starts off with the literal 'SOURCE#' followed by a 
#  tarfile or a directory.

   local $package;
   $path =~ s%([/#>])./%$1%g;    #  Tidy up the path.
   $path =~ s%//*%/%g;           #
   if ($path =~ s%^SOURCE#([^./>]+)(.tar.Z>|.tar.gz>|.tar>|/)$% 
              ($package = uc $1) . '#' %e) {
      $tasks{$package} ||= [];
   }

   my $file;
   foreach $file (@files) {
      if (-d $file) {                 #  directory.
         index_dir $path, $file; 
      }
      elsif ($file =~ /\.tar\b/) {    #  tar archive (possibly compressed).
         index_tar $path, $file;
      }
      elsif ($file =~ /\.f$/) {       #  fortran source file.
         $nfiles{'f'}++;
         index_f $path, $file;
      }
      elsif ($file =~ /\.gen$/) {     #  generic fortran source file.
         $nfiles{'gen'}++;
         index_gen $path, $file;
      }
      elsif ($file =~ /\.c$/) {       #  C source file.
         $nfiles{'c'}++;
         index_c $path, $file;
      }
      elsif ($file =~ /\.h$/) {       #  C header file.
         $nfiles{'h'}++;
         index_h $path, $file;
      }
      elsif ($file =~ /\.hlp$/) {     #  starlink help file
         index_hlp $path, $file;
      }
   }
}


########################################################################
sub index_dir {

#  Examine and index a directory file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $dir = shift;       #  Directory in current directory to be indexed.

   pushd $dir;
   index_list "$path$dir/", <*>;
   popd;
}

########################################################################
sub index_tar {

#  Examine and index a (possibly compressed) tar file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $tarfile = shift;   #  Tarfile in current directory to be indexed.

#  Define fully qualified pathname for file.

   my $fqtarfile = cwd . '/' . $tarfile;

#  Change to scratch directory.

   pushd $tmpdir;

#  Unpack tar file.

   my @files = tarxf $fqtarfile;

#  Pass list of files to indexing routine.

   index_list "$path$tarfile>", @files;
   
#  Tidy up.

   unlink @files               or die "Error removing files\n";
   popd;
}

########################################################################
sub index_fortran_file {

#  Examine and index a fortran source file.

#  Arguments.

   my $path = shift;          #  Logical pathname of FILE to be indexed.
   my $file = shift;          #  File in current directory to be indexed.
   my $ftype = shift || 'f';  #  Type of file ('f' or 'gen' - default 'f').

#  Cycle through source file writing index lines where appropriate.

   open F, $file or die "Couldn't open $file in directory ".cwd."\n";
   while (<F>) {
      write_entry $name, $path if ($name = module_name $ftype, $_);
      $nlines{$ftype}++;
   }
   close F;
}

########################################################################
sub index_f {

#  Examine and index a fortran source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .f file in current directory to be indexed.

   index_fortran_file "$path$file", $file, 'f';
}

########################################################################
sub index_gen {

#  Examine and index a .gen (fortran Generic) source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .gen file in current directory to be indexed.

#  Index unprocessed source code.

   index_fortran_file "$path$file", $file, 'gen';

#  Process file to produce normal fortran source code.

   system "$generic $file" and die "Command '$generic $file' failed: $?\n";

#  Hand file over to routine which handles normal fortran source code.

   my $ffile = $file;
   $ffile =~ s/\.gen$/.f/;
   index_fortran_file "$path$file", $ffile, 'f';

#  Tidy up.

   unlink $file or die "Failed to remove file $file\n";
}

########################################################################
sub index_hlp {

#  Examine and index a .hlp (Starlink interactive help) file.
#  This routine is a bit hit-and-miss; it uses the (package).hlp file
#  to guess the names of the top level A-tasks in the package.
#  It makes the assumption that the help file is called (package).hlp,
#  and furthermore that all the A-tasks have level-1 entries in the
#  help file.
#  If it guesses wrong of course it's not too bad, that is it will not
#  inhibit any correctly-named modules from being found; the index file
#  by this routine is only used to generate the top-level menu for the
#  browser if no module name is entered.

#  Note help sublibraries (@-lines) are not yet supported; any help files
#  which are split into different .hlp files will turn up looking like
#  different packages.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .hlp file in current directory to be indexed.

   my $package = starpack $path;
   open HLP, $file or die "Couldn't open $file in directory ".cwd."\n";
   my ($level, $baselevel);
   while (<HLP>) {

#     Bail out if we've picked up an IRAF help file instead.

      last if (/^\.help/);

#     Identify a header line and store it in the hash of lists %tasks.

      next unless (/^([0-9]) *(\S+)/); 
      ($level, $topic) = ($1, $2);
      unless (defined $baselevel) {
         $baselevel = $level;
      }
      if ($level == $baselevel+1) {
         push @{$tasks{$package}}, $topic;
      }
   }
   close HLP;
}


########################################################################
sub index_h {

#  Examine and index a C header file in the source tree (not include dir).

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .h file in current directory to be indexed.

#  Strip head part of path.

   my $include = $file;
   $include =~ s%^.*/%%;
   $file =~ s%^./%%;

#  Write to index.

   write_entry $include, "$path$file";
}


########################################################################
sub index_c {

#  Examine and index a C source file.
#  This relies on use of the macros in /star/include/f77.h to declare C
#  functions.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .c file in current directory to be indexed.

   open C, $file or die "Couldn't open $file in directory ".cwd."\n";
   while (<C>) {
      write_entry $name, "$path$file" if ($name = module_name 'c', $_);
      $nlines{'c'}++;
   }
   close C;
}



########################################################################
sub index_incdir {

#  Examine and index a directory of files containing include files.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $dir = shift;       #  Directory in current directory to be indexed.

   pushd $dir;
   indexinc_list "$path$dir/", <*>;
   popd;
}

########################################################################
sub indexinc_list {

#  Examine and index a list of include files.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my @files = @_;        #  Files in current directory to be indexed.

   foreach $file (@files) {
      if ($file =~ /\.h$/) {              #  C type header file.
         write_entry $file, "$path$file";
      }
      elsif ($file !~ /\./) {             #  Fortran type header file.
         write_entry uc ($file), "$path$file";
      }
   }
}
   

########################################################################
sub write_entry {

#  Write index entry to database.

#  Arguments.

   my $name = shift;      #  Name of module.
   my $location = shift;  #  Logical pathname of module.

#  Write entry to database (hash of hashes %locate); if no identifiable 
#  package the hash key is ''.

   $locate{$name} .= ' ' if (defined $locate{$name});
   $locate{$name} .= $location;

#  Optionally log entry to stdout.

   if ($verbose) {
      printf "%-20s =>  %s\n", $name, $location;
   }
}


########################################################################
sub error {
   $_ = shift;
   chomp;
   die "$_\n";
}
