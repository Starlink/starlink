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
#     part of the USSC.  Given a starting point in the directory tree, 
#     typically /star/sources, it looks underneath it for anything which
#     looks like a source file, specifically files with the following 
#     extensions: .f .gen
#     These files may be sitting 'naked' in one of the subordinate 
#     directories, or may be in a (compressed or uncompressed) tar file,
#     or may be in a tar file within such a tar file.  If more than one
#     instance of a given file is found, the 'most accessible' one is
#     used, i.e. first naked, then in a tar file, then in a tar file 
#     in a tar file.
#
#     When such a file is found, it is searched for program modules
#     which could be referred to by other programs (e.g. functions and
#     subroutines).  For each instance of such a module, the name of 
#     the module and information about its location is written to a 
#     DBM file.
#
#     It also indexes all include files from a second specified 
#     directory, by filename (capitalised if it looks like a fortran
#     include) rather than module name.  This will typically be
#     /star/include.
#
#     The location of files is stored in the dbm file as a 'logical
#     pathname'; this resembles an ordinary pathname except that
#     it starts with one of the literals 'SOURCE' or 'INCLUDE',
#     and may contain the special character '>' which indicates
#     inclusion in a (possibly compressed) tar file.  Thus some
#     example logical pathnames would be:
#
#         SOURCE#ast.tar.Z>ast_source.tar>frame.f
#         SOURCE#figaro/figaro_applic.tar>applic/bclean.f
#         INCLUDE#par_err
#
#     Additionally, some further information is stored in the dbm file:
#         SOURCE#    - name of the source directory
#         INCLUDE#   - name of the include directory 
#     These are for information to whatever program is reading the file
#     and may be ignored or used as desired.
#
#     The resulting DBM file can then be read by a separate program to
#     locate the source code for a module, using no information other
#     than its name.

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
print "rm -rf $tmpdir\n";
system "rm -rf $tmpdir" and die "Couldn't clean out $tmpdir: $?\n";
system "mkdir -p $tmpdir" and die "Couldn't create $tmpdir: $?\n";

#  Initialise and open index file.

unlink "$indexfile.pag", "$indexfile.dir";
tie %locate, SDBM_File, $indexfile, O_CREAT | O_RDWR, 0644;

#  Index source files.

chdir $srcdir or die "Couldn't enter $srcdir\n";
index_dir "SOURCE#", ".";
write_entry "SOURCE#", $srcdir;

#  Index include files.

chdir $incdir or die "Couldn't enter $incdir\n";
index_incdir "INCLUDE#", ".";
write_entry "INCLUDE#", $incdir;

#  Write checked task names out to text file.

open TASKS, ">$taskfile" or die "Couldn't open $taskfile\n";
foreach $package (sort keys %tasks) {
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

#  Rephrase logical path as a package reference if it looks it needs doing,
#  i.e. if it starts off with the literal 'SOURCE#' followed by a tarfile
#  or a directory.

   local $package;
   $path =~ s%([/#>])./%$1%g;    #  Tidy it up.
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
         index_f $path, $file;
      }
      elsif ($file =~ /\.gen$/) {     #  generic fortran source file.
         index_gen $path, $file;
      }
      elsif ($file =~ /\.c$/) {       #  C source file.
         index_c $path, $file;
      }
      elsif ($file =~ /\.h$/) {       #  C header file.
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

   my $path = shift;      #  Logical pathname of FILE to be indexed.
   my $file = shift;      #  File in current directory to be indexed.

#  Cycle through source file writing index lines where appropriate.

   open F, $file or die "Couldn't open $file in directory ".cwd."\n";
   while (<F>) {
      write_entry $name, $path if ($name = module_name 'f', $_);
   }
   close F;
}

########################################################################
sub index_f {

#  Examine and index a fortran source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .f file in current directory to be indexed.

   index_fortran_file "$path$file", $file;
}

########################################################################
sub index_gen {

#  Examine and index a .gen (fortran Generic) source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my $file = shift;      #  .gen file in current directory to be indexed.

#  Process file to produce normal fortran source code.

   system "$generic $file" and die "Command '$generic $file' failed: $?\n";

#  Hand file over to routine which handles normal fortran source code.

   my $ffile = $file;
   $ffile =~ s/\.gen$/.f/;
   index_fortran_file "$path$file", $ffile;

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

   $file =~ s%^.*/%%;

#  Write to index.

   write_entry $file, "$path$file";
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
