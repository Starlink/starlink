#!/usr/local/bin/perl -w

#+
#  Name:
#     scbindex.pl

#  Purpose:
#     Make index of all Starlink source code files.

#  Language:
#     Perl 5

#  Invocation:
#     scbindex.pl [package-location [package-location ...]]

#  Description:
#     This program attempts to find all the source code files which form
#     part of the USSC.  Given a base directory $srcdir, typically
#     /star/sources, it examines all files underneath it.
#
#     These files may be sitting 'naked' in one of the subordinate 
#     directories, or may be in a (compressed or uncompressed) tar file,
#     or may be in a tar file within such a tar file (nested to an 
#     arbitrary level, although not usually deeper than 2).
#     For every file, its name and location are written to a StarIndex
#     object.
#
#     Other actions may be taken according to the kind of file:
#
#     If a language-specific tagging routine exists for that file type, 
#     it is tagged and any modules (functions or subroutines) defined 
#     in it are written by name and location of the file into a 
#     StarIndex object (a different one from the one in which filenames
#     are stored, so that the namespaces are independent).
#     
#     If a .hlp file is found, it is used to try to identify the names
#     of 'tasks' within the package.  Future versions may examine other
#     files to gain more indexable information about the packages.
#
#     Finally a list of packages, and tasks within each package,
#     is written out to a second file ($taskfile).
#     What constitutes a task is not well defined, but is something
#     like the name of an executable or alias which can be typed from
#     the user environment to do something useful in the package, and
#     for which an identifiable piece of source code exists.
#     As well as the method described above, some more poking around
#     in $bindir (typically /star/bin) is done to try to locate more
#     tasks.  The general philosophy for identifying tasks is to look
#     everywhere which might give an indication of task names, and 
#     store anything which looks as if it might be a task name in the 
#     global hash of lists %tasks.  At the end of the indexing, each 
#     task name candidate in %tasks for which a corresponding source 
#     module (i.e one in the same package with the same, or nearly
#     the same name) can be found is written out to the task list, 
#     and the others are discarded.
#
#     Each line in the task file is of the format:
#
#           package: task1 task2 task3 ...
#
#     with each indexed package for which any entries appear in a 
#     StarIndex object appearing exactly once.  Thus the tasks file
#     is the record of all the packages which have been indexed as
#     well as the record of all the task names.
#     Although the task file is plain text, some of the lines may be 
#     quite long.

#  Arguments:
#     package-location = string (optional).
#        Each package-location is either a directory called by the 
#        name of the package, or a tarfile called 'package.tar', 
#        'package.tar.Z' or 'package.tar.gz'.  For each package 
#        indexed in this way, any existing index entries concerning
#        it are first erased from the indices, and new entries are
#        written for all files and routines found.  Index entries
#        for other packages are not affected.
#
#     If no arguments are given, the indices are erased entirely and
#     built again for scratch from all the packages in $srcdir 
#     (typically /star/sources).  Additionally all the files in the 
#     include directory $incdir (typically /star/include) are indexed.

#  Notes:
#     Operation of this script will be affected by the build-time and 
#     run-time values of certain environment variables, as documented
#     in the module Scb.pm.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

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
$| = 1;

#  Required libraries.

use Cwd;
use Scb;
use StarIndex;

#  Declarations.

sub index_list;
sub index_pack;
sub index_includes;
sub index_dir;
sub index_tar;
sub index_hlp;
sub index_source;
sub index_files;
sub write_entry;
sub uniq;
sub tidyup;
sub error;

#  Set up scratch directory.

$tmpdir = $scbindex_tmpdir;
rmrf $tmpdir;
mkdirp $tmpdir, 0755;

#  Set up signal handler.  Note this is not entirely safe since tidyup
#  does non-trivial work and so (probably) calls some non-reentrant 
#  routines.  If the handler is called at an unlucky time this may 
#  result in an inelegant exit (core dump).

$SIG{'INT'} = sub {tidyup; exit;};

#  Initialise index objects.

my $fmode = @ARGV ? 'update' : 'new';

$func_index = StarIndex->new($func_indexfile, $fmode);
$file_index = StarIndex->new($file_indexfile, $fmode);

#  If task file exists, read values from it into @tasks.

if (open TASKS, "$taskfile") {
   my ($pack, $tasks);
   while (<TASKS>) {
      ($pack, $tasks) = /^ *(\S+) *: *(.*)$/;
      ${tasks{$pack}} = [ split ' ', $tasks ];
   }
   close TASKS;
}

#  If packages are named on the command line, index just those.

if (@ARGV) {
   foreach $package_file (@ARGV) {
      index_pack $package_file;
   }
}

#  If no packages are named on the command line, index all packages 
#  under $srcdir, and include files under $incdir.

else {
   index_includes $incdir, "INCLUDE";
   foreach $package_file (glob "$srcdir/*") {
      index_pack $package_file;
   }
}

#  Write checked task names out to text file.
#  Currently, a task is identified as anything which has been identified
#  as a candidate for taskhood by other routines (stored in global hash
#  %tasks) for which a module of the same name (possibly with appended 
#  underscore) exists in the same package.

my ($line, $module);
open TASKS, ">$taskfile" or error "Couldn't open $taskfile\n";
foreach $pack (sort keys %tasks) {
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
}
close TASKS;

#  Terminate processing.

tidyup;

exit;

########################################################################
#  Subroutines.
########################################################################

########################################################################
sub index_pack {

#+
#  Name:
#     index_pack

#  Purpose:
#     Index all files and modules found in a given package.

#  Language:
#     Perl 5

#  Invocation:
#     index_pack ($pack_file);

#  Description:
#     Given the location (tarfile or directory) of a Starlink package,
#     index all indexable modules and files in it by writing entries 
#     to StarIndex objects.

#  Arguments:
#     $pack_file = string.
#        Location of package.  Either the pathname of a directory having 
#        the name of the package, or the pathname of a tarfile. 
#     $package = string (optional).
#        Name of package.  If absent it will be inferred from $pack_file
#        which is assumed to be of the form 'package', 'package.tar',
#        'package.tar.Z' or 'package.tar.gz'.  If present the package 
#        is forced to be named as given.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my $pack_file = shift;
   $package = shift;

#  Locate package: if it is a single filename (i.e. not a path) and doesn't
#  exist in the current directory, look for it in $srcdir.  If it doesn't
#  exist anywhere, generate an error.

   if (!-e $pack_file) {
      if (index ('/',$pack_file) == -1 && -e "$srcdir/$pack_file") {
         $pack_file = "$srcdir/$pack_file";
      }
      else {
         error "Package '$pack_file' not found\n";
      }
   }

#  Get name of package.

   $pack_file =~ m%^(.*/)?([^.]+)(\.tar.*)?%;
   my ($dir, $tarext) = ($1, $3);
   $package ||= $2;
   print "\nPACKAGE: $package\n";

#  If any records for this package already exist in the function index, 
#  or file index, delete them.

   my ($key, $value);
   $func_index->delpack($package);
   $file_index->delpack($package);

#  If any tasks exist for this package delete them.

   $tasks{$package} = [ ];

#  Get fully qualified pathname of package location and store in file index.

   my $fqpack_file = $pack_file;
   $fqpack_file =~ s%^(?!/)% cwd . '/' %e;
   $file_index->put("$package#", $fqpack_file);

#  Perform the indexing.

   if (-d $pack_file) {
      index_dir "$package#", $pack_file;
   }
   elsif ($tarext) {
      index_tar "$package#", $pack_file;
   }
   else {
      error "Arguments must be package tar files or directories\n";
   }

#  Index expected files sitting naked in source directory.  These may
#  not have been picked up by the file indexing routine, since only
#  files within tar files are indexed by filename (to avoid indexing
#  many unwanted object files etc).

   if (-d $pack_file) {
      my ($file, @files);
      pushd $pack_file;
      foreach $file (glob "mk makefile *CONDITIONS *.news *.tex *.ps *.eps") {
         push @files, $file if (-f $file);
      }
      index_files "$package#", @files;
      popd;
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

#+
#  Name:
#     index_list

#  Purpose:
#     Perform appropriate indexing operations on a list of files.

#  Language:
#     Perl 5

#  Invocation:
#     index_list ($path, @files);

#  Description:
#     This routine examines a list of files in a Starlink package;
#     each file is identified as a naked file of an interesting type,
#     a tar file, or a directory, and handed off to a routine designed
#     to deal with that type.  If it falls into none of these 
#     categories it is ignored.

#  Arguments:
#     $path = string.
#        Logical pathname of current directory.
#     @files = list of strings.
#        Each element is a filename in the current directory to be indexed.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my ($path, @files) = @_;

   my ($file, $ext);
   foreach $file (@files) {
      $ext = '';
      $ext = $1 if ($file =~ /\.([^.]+)$/);
      if (-d $file) {                    #  directory.
         index_dir "$path$file/", $file; 
      }
      elsif ($file =~ /\.tar\b/) {       #  tar archive (possibly compressed)
         index_tar "$path$file>", $file;
      }
      elsif ($ext eq 'hlp') {            #  starlink help file
         index_hlp "$path$file", $file;
      }
      elsif (defined $tagger{$ext}) {    #  source file in taggable language
         index_source "$path$file", $file;
      }
   }
}


########################################################################
sub index_dir {

#+
#  Name:
#     index_dir

#  Purpose:
#     Perform appropriate indexing operations on a directory.

#  Language:
#     Perl 5

#  Invocation:
#     index_dir $path, $file;

#  Description:
#     This routine indexes all files in a directory.

#  Arguments:
#     $path = string.
#        Logical pathname of directory.
#     $dir = string.
#        Name of directory in current directory to be indexed.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my ($path, $dir) = @_;

#  Change directory, and pass list of files in (now) current directory
#  to index_list.

   pushd $dir;
   index_list $path, glob "*";
   popd;
}

########################################################################
sub index_tar {

#+
#  Name:
#     index_tar

#  Purpose:
#     Perform appropriate indexing operations on a tar file.

#  Language:
#     Perl 5

#  Invocation:
#     index_tar ($path, $tarfile);

#  Description:
#     This routine extracts all files from a (possibly compressed) tar 
#     file and indexes them.  As well as passing them to the source code
#     tagging and indexing routine, which attempts to tag them in a 
#     language-specific way, this routine is where all the filenames
#     are indexed in the 'file' StarIndex object.  It is done here rather
#     than elsewhere since it is expected that all the files worth
#     indexing will be in at least one level of tarfile, and conversely
#     most things which are not in a tarfile will be not worth indexing,
#     e.g. large numbers of object files generated by package building.

#  Arguments:
#     $path = string.
#        Logical pathname of tarfile.
#     $tarfile = string.
#        Name of tarfile in current directory to be indexed.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my ($path, $tarfile) = @_;

#  Define fully qualified pathname for file.

   $tarfile = cwd . "/$tarfile" unless ($tarfile =~ m%^/%);

#  Change to scratch directory.

   pushd $tmpdir;

#  Unpack tar file.

   print "*** Unpacking $tarfile ***\n" if ($verbose);
   my @files = tarxf $tarfile;

#  Pass list of files to file indexing routine.

   index_files $path, @files;

#  Pass list of files to function indexing routine.

   index_list $path, @files;
   
#  Tidy up.

   unlink @files;
   popd;
}


########################################################################
sub index_source {

#+
#  Name:
#     index_source

#  Purpose:
#     Perform appropriate indexing on a source file.

#  Language:
#     Perl 5

#  Invocation:
#     index_source $path, $file;

#  Description:
#     This routine passes a source file to a language-specific tagging
#     routine, and examines the tagged result for the names of any
#     modules (functions or subroutines) defined in the file.  If any
#     are found they are written to the 'func' StarIndex object.

#  Arguments:
#     $path = string.
#        Logical pathname of source file to be indexed.
#     $file = string.
#        Name of source file in current directory.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my ($path, $file) = @_;

#  Get file extension.

   $file =~ /\.([^.]+)$/;
   my $ext = $1;

#  Get language-specific tagging routine.

   (ref ($rtagger = $tagger{$ext}) eq 'CODE') 
      or error "Language-specific tagging routine doesn't seem to exist\n";

#  Tag source file using language-specific tagging routine.

   unless (open SOURCE, $file) {
      print "Failed to open $file in directory ".cwd."\n";
      return;
   }
   my $tagged = &$rtagger (join ('', <SOURCE>), $ext);
   close SOURCE;

#  Write index entries for all the "<a name=''>" type tags.

   while ($tagged =~ /(<[^>]+>)/g) {
      %tag = parsetag $1;
      if (($tag{'Start'} eq 'a') && $tag{'name'}) {
         write_entry $tag{'name'}, $path;
      }
   }

}


########################################################################
sub index_hlp {

#+
#  Name:
#     index_hlp

#  Purpose:
#     Perform appropriate indexing on a Starlink HLP file.

#  Language:
#     Perl 5

#  Invocation:
#     index_hlp ($path, $file);

#  Description:
#     This routine pokes around in a Starlink .hlp file for clues to
#     what might be a task name.  It adopts a fairly unsophisticated
#     approach: any help topic which appears at the base level of 
#     a .hlp file is taken as a candidate, on the basis that probably
#     there will be a help topic for each task.  Many of these will be 
#     false candidates, but they should get weeded out later, when the
#     tasks file is being written.
#
#     Starlink HLP files are not the only things with .hlp extensions,
#     IRAF help files have them too, so an attempt is made to check
#     that we're looking at the right thing.

#  Arguments:
#     $path = string.
#        Logical pathname of help file to be indexed.
#     $file = string.
#        Name of help file in current directory.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my ($path, $file) = @_;

   unless (open HLP, $file) {
      print "Couldn't open $file in directory ".cwd."\n";
      return;
   }
   my ($level, $baselevel);
   while (<HLP>) {

#     Bail out if we've picked up an IRAF help file instead.

      last if (/^\.help/);

#     Examine only header lines from file.

      next unless (/^([0-9]) *(\S+)/); 

#     Identify the base level of the help file.

      ($level, $topic) = ($1, $2);
      unless (defined $baselevel) {
         $baselevel = $level;
      }

#     Store any topic just above the base level as a candidate task.

      if ($level == $baselevel+1) {
         push @{$tasks{$package}}, lc ($topic);
      }
   }
   close HLP;
}


########################################################################
sub index_includes {

#+
#  Name:
#     index_includes

#  Purpose:
#     Index all files in Starlink include directory.

#  Language:
#     Perl 5

#  Invocation:
#     index_includes ($dir, $packname);

#  Description:
#     This routine goes through a named directory supposed to contain 
#     a set of files to be indexed, and indexes them under the package
#     name given.  It is designed to be used for the $incdir (typically
#     /star/include) directory.

#  Arguments:
#     $dir = string.
#        Name of directory containing files.
#     $packname = string.
#        Name of package under which to index them.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Get arguments.

   my ($dir, $packname) = @_;

#  Erase old entries.

   $file_index->delpack($packname);

#  Store location of directory in file StarIndex object.

   $file_index->put("$packname#", $dir);

#  Index all files in directory.

   pushd $dir;
   index_files "$packname#", glob "*";
   popd;
}


#######################################################################
sub write_entry {

#+
#  Name:
#     write_entry

#  Purpose:
#     Write entry to StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     write_entry ($name, $location);

#  Description:
#     Writes an entry to the func StarIndex object and logs to standard
#     output.

#  Arguments:
#     $name = string.
#        Key of record (name of module).
#     $location = string.
#        Value of record (logical pathname of module).

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Arguments.

   my ($name, $location) = @_;

#  Tidy path string.

   $location =~ s%([#>/])\./%$1%g;
   $location =~ s%//+%/%g;

#  Write entry to StarIndex object.

   $func_index->put($name, $location);

#  Optionally log entry to stdout.

   printf "%-20s =>  %s\n", $name, $location if ($verbose);
   
}


########################################################################
sub index_files {

#+
#  Name:
#     index_files

#  Purpose:
#     Writes locations of files to StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     index_files ($path, @files);

#  Description:
#     Takes a list of files and writes index entries for their locations
#     to the file StarIndex object.

#  Arguments:
#     $path = string.
#        Logical pathname head of files.
#     @files = list of strings.
#        Filenames within $path to be indexed.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-


#  Get parameters.

   my ($path, @files) = @_;

   my ($file, $dir, $name, $location);
   foreach $file (@files) {

#     Split file location into path head and actual name.

      $file =~ m%^(.*/)?([^/]+)%;
      ($dir, $name) = ($1, $2);
      $location = "$path$file";

#     Write entry

      $file_index->put($name, $location);

#     Optionally log entry to stdout.

      printf "%-20s =>  %s\n", $name, $location if ($verbose);
   }
}


########################################################################
sub uniq {

#+
#  Name:
#     uniq

#  Purpose:
#     Remove adjacent duplicate entries from a list.

#  Language:
#     Perl 5

#  Invocation:
#     @list = uniq @dlist;

#  Description:
#     This routine removes adjacent duplicate entries from a list, 
#     analogously to the Unix command of the same name.

#  Arguments:
#     @list = list of strings.
#        Sorted list of items to be deduplicated.

#  Return value:
#     @dlist = list of strings.
#        Same as list but without adjacent duplicate items.

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   my ($last, $val, @ans);
   foreach $val (@_) {
      push (@ans, $val) unless ($last && $val eq $last);
      $last = $val;
   }
   return @ans;
}


########################################################################
sub tidyup {

#+
#  Name:
#     tidyup

#  Purpose:
#     Tidy up on exit from source code indexer.

#  Language:
#     Perl 5

#  Invocation:
#     tidyup;

#  Description:
#     If any temporary files have been created, remove them, and ensure
#     that the StarIndex objects are properly synced to disk.  This
#     routine should only be necessary in an abnormal exit situation.

#  Arguments:

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Sync StarIndex objects to disk.

   $func_index->finish();
   $file_index->finish();

#  Remove any straggling temporary files.

   chdir "/";
   rmrf $tmpdir;
}

########################################################################
sub error {

#+
#  Name:
#     error

#  Purpose:
#     Present error text and exit cleanly.

#  Language:
#     Perl 5

#  Invocation:
#     error (@messages).

#  Description:
#     Prints an error message to standard error, tidies up, and exits 
#     with an error status.

#  Arguments:
#     @messages = list of strings.
#        Error text to be printed.

#  Return value:

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     05-OCT-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Do things which have to be done before exiting.

   tidyup;

#  Print error text and exit.

   die @_;
}
# $Id$
