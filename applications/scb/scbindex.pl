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
#     DBM file ($func_indexfile).
#
#     The resulting DBM file $func_indexfile can then be read by a separate 
#     program to locate the source code for a module, using no 
#     information other than its name, and perhaps what package it is
#     part of.
#
#     Additionally a list of packages, and tasks within each package,
#     is written out to a second file ($taskfile).
#
#  Invocation:
#     scbindex.pl [package-location package-location ...]
#
#  Arguments:
#     If no arguments are specified, the program indexes all the packages
#     under the default source directory as specified in libscb.pm 
#     (typically /star/sources).  If any arguments are specified, 
#     then instead these are taken as package locations.
#
#  Index DBM file format.
#     The index file is named $func_indexfile and stored as a Perl SDBM file.  
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
#     containing at least one module indexed in $func_indexfile is included
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

$| = 1;

#  Flags.

$verbose = 1;

#  Shell commands required.

$generic = "generic";

#  Required libraries.

use Fcntl;
use Cwd;
use Scb;
use FortranTag;
use CTag;
use Directory;

#  Declarations.

sub index_list;
sub index_pack;
sub index_dir;
sub index_tar;
sub index_hlp;
sub index_f;
sub index_c;
sub index_h;
sub index_gen;
sub indexinc_dir;
sub indexinc_list;
sub write_entry;
sub uniq;

#  Set up scratch directory.

$tmpdir = "/local/junk/scb/index";
rmrf $tmpdir;
system "mkdir -p $tmpdir" and die "Couldn't create $tmpdir: $?\n";

#  Initialise index object containing locations of all modules.

$func_index = Directory->new($func_indexfile, O_CREAT | O_RDWR);

#  If task file exists, read values from it into @tasks.

if (open TASKS, "$taskfile") {
   my ($pack, $tasks);
   while (<TASKS>) {
      ($pack, $tasks) = /^ *(\S+) *: *(.*)$/;
      ${tasks{$pack}} = [ split ' ', $tasks ];
   }
}

#  Index packages.

if (@ARGV) {
   foreach $package_file (@ARGV) {
      index_pack $package_file;
   }
}
else {
   $func_index->put('SOURCE#', $srcdir);
   foreach $package_file (glob "$srcdir/*") {
      index_pack $package_file;
   }
}

#  Index files from include directory.

if (@ARGV == 0) {
   indexinc_dir "INCLUDE#", $incdir;
}

$verbose = 0;

#  Write checked task names out to text file.
#  Currently, a task is identified as the text of any entry in a .hlp file
#  for which a module of the same name (possibly with appended underscore)
#  exists in the same package.

my ($line, $module);
open TASKS, ">$taskfile" or die "Couldn't open $taskfile\n";
foreach $pack (sort keys %tasks) {
   $npackages++;
   $line = "$pack:";
   foreach $task (uniq sort @{$tasks{$pack}}) {

#     Look for an indexed module called "$task_", then "$task".  Add to
#     tasks line only if one of these is found.

      $module = $task;
      $module .= "_" if ($module !~ /_$/);
      chop $module unless ($func_index->get($module, packmust => $pack));
      $line .= " $module"  if ($func_index->get($module, packmust => $pack));
   }
   print TASKS "$line\n";
   print       "$line\n" if $verbose;
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

rmrf $tmpdir;

exit;

########################################################################
#  Subroutines.
########################################################################

########################################################################
sub index_pack {

#  Given the location of a package, examine all files in it and write 
#  index entries for each indexable module.

#  Arguments.

   my $pack_file = shift;             #  Tar file or directory

#  Get name of package.

   $pack_file =~ m%^(.*/)?([^.]+)(\.tar.*)?%;
   my ($dir, $tarext);
   ($dir, $package, $tarext) = ($1, $2, $3);
   print "PACKAGE: $package\n";
   $func_index->put("$package#", $pack_file);

#  If any records for this package already exist in the index, delete them.

   my ($key, $value);
   while (($key, $value) = $func_index->each($package)) {
      $func_index->delete($key, $package);
   }

#  If any tasks exist for this package delete them.

   $tasks{$package} = [ ];

#  Perform the indexing.

   if (-d $pack_file) {
      index_dir "$package#", "$dir$package";
   }
   elsif ($tarext) {
      index_tar "$package#", "$dir$package$tarext";
   }
   else {
      die "Arguments must be package tar files or directories\n";
   }

#  Look for task candidates in the $bindir/$package directory.

   my $file;
   if (-d "$bindir/$package") {
      pushd "$bindir/$package";
      foreach $file (glob "*") {
         push @{$tasks{$package}}, $file if (-x $file);
      }
      popd;
   }
   
}
 


########################################################################
sub index_list {

#  Examine and index a list of source files; identify each file as a 
#  naked file of an interesting type, a tar file, or a directory, and 
#  hand off to a routine designed to deal with that type.  
#  Any other files are ignored.

#  Arguments.

   my $path = shift;      #  Logical pathname of current directory.
   my @files = @_;        #  Files in current directory to be indexed.

   my $file;
   foreach $file (@files) {
      if (-d $file) {                 #  directory.
         index_dir "$path$file/", $file; 
      }
      elsif ($file =~ /\.tar\b/) {    #  tar archive (possibly compressed).
         index_tar "$path$file>", $file;
      }
      elsif ($file =~ /\.f$/) {       #  fortran source file.
         $nfiles{'f'}++;
         index_f "$path$file", $file;
      }
      elsif ($file =~ /\.gen$/) {     #  generic fortran source file.
         $nfiles{'gen'}++;
         index_gen "$path$file", $file;
      }
      elsif ($file =~ /\.c$/) {       #  C source file.
         $nfiles{'c'}++;
         index_c "$path$file", $file;
      }
      elsif ($file =~ /\.h$/) {       #  C header file.
         $nfiles{'h'}++;
         index_h "$path$file", $file;
      }
      elsif ($file =~ /\.hlp$/) {     #  starlink help file
         index_hlp "$path$file", $file;
      }
   }
}


########################################################################
sub index_dir {

#  Examine and index a directory file.

#  Arguments.

   my $path = shift;      #  Logical pathname of directory to be indexed.
   my $dir = shift;       #  Directory in current directory to be indexed.

   pushd $dir;
   index_list $path, glob "*";
   popd;
}

########################################################################
sub index_tar {

#  Examine and index a (possibly compressed) tar file.

#  Arguments.

   my $path = shift;      #  Logical pathname of tar file.
   my $tarfile = shift;   #  Tarfile to be indexed.

#  Define fully qualified pathname for file.

   $tarfile = cwd . "/$tarfile" unless ($tarfile =~ m%^/%);

#  Change to scratch directory.

   pushd $tmpdir;

#  Unpack tar file.

   print "*** Unpacking $tarfile  ***\n" if ($verbose);
   my @files = tarxf $tarfile;

#  Pass list of files to indexing routine.

   index_list $path, @files;
   
#  Tidy up.

   unlink @files;
   popd;
}


########################################################################
sub index_source {

#  Tag and index a source code file.

#  Arguments.

   my $path = shift;          #  Logical pathname of file to be indexed.
   my $file = shift;          #  File in current directory to be indexed.
   my $ftype = shift;         #  Type of file ('f' or 'c').

#  Tag source file using language-specific tagging routine.

   open SOURCE, $file or die "Failed to open $file in directory " . cwd . "\n";
   if ($ftype eq 'f' || $ftype eq 'gen') {
      $tagged = FortranTag::tag join '', <SOURCE>;
   }
   elsif ($ftype eq 'c' || $ftype eq 'h') {
      $tagged = CTag::tag join '', <SOURCE>;
   }
   close SOURCE;

   while ($tagged =~ /(<[^>]+>)/g) {
      %tag = parsetag $1;
      if (($tag{'Start'} eq 'a') && $tag{'name'}) {
         write_entry $tag{'name'}, $path;
      }
      elsif (($tag{'Start'} eq 'a') && ($tag{'href'} =~ /^INCLUDE-(.*)/)) {
         $include = uc $1;
      }
   }

   $nlines{$ftype} += ($tagged =~ tr/\n/\n/);
}


########################################################################
sub index_c {

#  Examine and index a C source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of .c file.
   my $file = shift;      #  .c file in current directory to be indexed.

   index_source $path, $file, 'c';
}


########################################################################
sub index_f {

#  Examine and index a fortran source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of .f file.
   my $file = shift;      #  .f file in current directory to be indexed.

   index_source $path, $file, 'f';
}


########################################################################
sub index_gen {

#  Examine and index a .gen (fortran Generic) source file.

#  Arguments.

   my $path = shift;      #  Logical pathname of .gen file.
   my $file = shift;      #  .gen file in current directory to be indexed.

#  Index unprocessed source code.

   index_source $path, $file, 'gen';

#  Move file to scratch space.

   my $gtmpdir = "$tmpdir/generic_tmpdir";
   my $tail = $file;
   $tail =~ s%.*/%%;
   system "mkdir -p $gtmpdir" and die "Failed to mkdir $gtmpdir\n";
   system "cp $file $gtmpdir/$tail" and die "Failed to copy $file\n";
   pushd $gtmpdir;

#  Process file to produce normal fortran source code.

   system "$generic $tail" and die "Command '$generic $tail' failed: $?\n";

#  Hand file over to routine which handles normal fortran source code.

   my $ffile = "$gtmpdir/$tail";
   $ffile =~ s/\.gen$/.f/;
   index_source $path, $ffile, 'f';

#  Tidy up.

   popd;
   unlink "$gtmpdir/$tail", $ffile;
   rmdir $gtmpdir or die "Failed to rmdir $gtmpdir\n";
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

#  Arguments.

   my $path = shift;      #  Logical pathname of .hlp file.
   my $file = shift;      #  .hlp file in current directory to be indexed.

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
         push @{$tasks{$package}}, lc ($topic);
      }
   }
   close HLP;
}


########################################################################
sub index_h {

#  Examine and index a C header file in the source tree (not include dir).

#  Arguments.

   my $path = shift;      #  Logical pathname of .h file.
   my $file = shift;      #  .h file in current directory to be indexed.

#  Strip head part of path.

   my $include = $file;
   $include =~ s%^.*/%%;

#  Write to index.

   write_entry "INCLUDE-$include", $path;

#  Index source (e.g. for #defines).

   index_source $path, $file, 'h';
}


########################################################################
sub indexinc_dir {

#  Examine and index a directory of files containing include files.

#  Arguments.

   my $path = shift;      #  Logical pathname of directory to be indexed.
   my $dir = shift;       #  Directory to be indexed.

   pushd $dir;
   indexinc_list $path, <*>;
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
         write_entry "INCLUDE-$file", "$path$file";
      }
      elsif ($file !~ /\./) {             #  Fortran type header file.
         write_entry "INCLUDE-" . uc ($file), "$path$file";
      }
   }
}
   

########################################################################
sub write_entry {

#  Write index entry to database.

#  Arguments.

   my $name = shift;      #  Name of module.
   my $location = shift;  #  Logical pathname of module.

#  Tidy path string.

   $location =~ s%([#>/])\./%$1%g;
   $location =~ s%//+%/%g;

#  Write entry to database.

   $func_index->put($name, $location);

#  Optionally log entry to stdout.

   if ($verbose) {
      printf "%-20s =>  %s\n", $name, $location;
   }
}


########################################################################
sub uniq {

#  Removes adjacent duplicate values from a list.

   my ($last, $val, @ans);
   foreach $val (@_) {
      push (@ans, $val) unless ($last && $val eq $last);
      $last = $val;
   }
   return @ans;
}


########################################################################
sub error {
   $_ = shift;
   chomp;
   die "$_\n";
}
