package StarIndex;

#+
#  Name:
#     StarIndex.pm

#  Purpose:
#     Object class for database of names in Starlink packages.

#  Language:
#     Perl 5

#  Invocation:
#     use StarIndex;

#  Description:
#     This module provides an object class for accessing a disk-based
#     set of entries indexed by keyname and Starlink package name,
#     hence something like a hash of hashes.  The keyname is typically
#     the name of a source file or of a function, and the value of each
#     entry is a file location, stored as a "logical pathname".
#
#     Internally, this is implemented as a hash in which each key is
#     the keyname (file or function name), and each value is a space-
#     separated list of locations, not more than one location per package.
#     Thus flattened from a hash of hashes to a hash of strings, it
#     can be tied to a DBM file.
#     The task of turning the space-separated list of locations into
#     a hash is dealt with by routines in this module at such time as
#     they are read or written.
#
#     Logical pathnames (location format):
#        A logical pathname is a generalisation of a Unix pathname; most
#        characters have their own literal meaning, but some metacharacters
#        are defined:
#
#           '/'  signifies inclusion in a directory (as in a Unix pathname)
#           '>'  signifies inclusion in a (possibly compressed) tar archive
#           '#'  signifies inclusion in a Starlink package
#
#        The sequence 'package#' may only appear at the start of the
#        logical path.  The others may be nested arbitrarily.  Example
#        logical pathnames are thus:
#
#           ast#ast_source.tar>frame.f
#           figaro#figaro_applic.tar.Z>applic/bclean.f
#
#        The meaning of inclusion in a directory or in a tarfile should be
#        obvious.  Inclusion in a Starlink package is normally interpreted
#        as inclusion in a directory, or in a (possibly compressed) tarfile
#        under the Starlink source directory, so that:
#
#           package#
#
#        is equivalent to one of
#
#           $srcdir/package/
#           $srcdir/package.tar>
#           $srcdir/package.tar.Z>
#           $srcdir/package.tar.gz>
#
#        $srcdir is a global variable defined in Scb.pm, and typically
#        has a value of '/star/sources'.
#
#        As a special case however, partly to accomodate pseudo-packages
#        (such as the include directory, which is likely to be at
#        /star/include and not under $srcdir), if the string 'package#'
#        is a key in the StarIndex object stored in $file_indexfile,
#        then the value associated with it may be used as the meaning
#        of the 'package#' prefix.  Thus if the $file_indexfile StarIndex
#        object contains the (key => value) pair
#
#           'INCLUDE#'  =>  '/star/include'
#
#        then the logical pathname 'INCLUDE#f77.h' may be taken to mean
#        '/star/include/f77.h'.
#        Although documented here, this mapping is not carried out in
#        this module.  It is up to the application to decide in which
#        order these options should be used to find a meaning for the
#        'package#' prefix.

#  Implementation:
#     The hash is tied to a disk-based DBM file of some kind to implement
#     the index.  Depending on the DBM library used, this sometimes
#     imposes some undesirable limits; in particular maximum size of
#     key+value of one entry in the hash.  This module will use
#     GDBM_File or DB_File if they exist, which do not suffer from
#     these limits.  Otherwise it will fall back on one of the others
#     (NDBM_File or SDBM_File - the latter guaranteed to exist), and
#     large writes will fail.

#  Arguments:

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

#  Declare all variables explicitly.

use strict 'vars';

#  Required libraries.

use Scb qw/:DEFAULT error/;
use Fcntl;

#  Manipulate the AnyDBM_File preference list of what database access to
#  use - by default NDBM_File is first preference.  We prefer GDBM_File
#  and DB_File because they have no limits on key+value size, while the
#  others tend to have a limit of around 1024 bytes, which can cause
#  writes to fail.  Then set $Dbmtype to the type of access actually used.

BEGIN { @AnyDBM_File::ISA = qw/DB_File GDBM_File NDBM_File SDBM_File/ }
use AnyDBM_File;
my $Dbmtype = $AnyDBM_File::ISA[0];


########################################################################
sub new {

#+
#  Name:
#     StarIndex::new

#  Purpose:
#     Create a new StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     $index = StarIndex::new($indexfile, $access);

#  Description:
#     Creates a new StarIndex object, either referring to an existing
#     disk file or creating a new one.

#  Arguments:
#     $indexfile = string.
#        Name of the disk file for storing the data (possibly excluding
#        file extension - this depends on the implementation of the DBM).
#     $access = string.
#        Access mode for the file: 'read', 'update' or 'new'.

#  Return value:
#     $index = StarIndex object.

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

   my ($class, $indexfile, $access) = @_;

#  Set up mapping of requested access type to DBM access mode.  According
#  to what type of DBM access we are using, the flags may be different.

   my %fmode = ($Dbmtype eq 'GDBM_File')
      ?
         ( 'read'    => &GDBM_File::GDBM_READER(),
           'update'  => &GDBM_File::GDBM_WRCREAT(),
           'new'     => &GDBM_File::GDBM_NEWDB() )
      :
         ( 'read'    => O_RDONLY,
           'update'  => O_RDWR | O_CREAT,
           'new'     => O_RDWR | O_CREAT | O_TRUNC )
      ;

#  Tie the StarIndex object, which is a hash, to the DBM file.

   my %locate;
   unless (tie %locate, AnyDBM_File, $indexfile, $fmode{$access}, 0644) {

#     Set up default error message.

      my $error = "Unidentified problem opening DBM file '$indexfile'";

#     Try to determine what went wrong with the tie.
#     First of all try to work out the actual name of the file(s) - not so
#     easy, since the name it is given will depend on the implementation
#     of the DBM library (e.g. it might be called just plain $indexfile, or
#     "$indexfile.db", or be a pair of files $indexfile.{dir,pag}, or ....
#     We make a reasonably inclusive assumption about the form of the file
#     name(s).

      my @indexfiles =
         grep /^$indexfile(\.(db\w*|dir|pag))?$/, glob ("$indexfile*");
      $indexfile =~ m%(.*)/%;
      my $indexdir = $1 || '.';

      if (@indexfiles == 0) {

#        Index file does not already exist.

         if ($access eq 'read') {
            $error = "DBM file '$indexfile' does not seem to exist";
         }
         elsif (!-w $indexdir) {
            $error = "Directory '$indexdir' is not writeable";
         }
      }
      else {

#        Index file does exist.

         if (grep ((! -f), @indexfiles)) {
            $error = "DBM file '$indexfile' apparently not regular file"
         }
         elsif (grep ((! -r), @indexfiles)) {
            $error = "DBM file '$indexfile' apparently not readable";
         }
         elsif (($access =~ 'update|new') && grep ((! -w), @indexfiles)) {
            $error = "DBM file '$indexfile' apparently not writable";
         }
         elsif ($access eq 'read' && grep ((-z), @indexfiles)) {
            $error = "DBM file '$indexfile' apparently empty";
         }
      }

      error "$error\n";
   }

   else {

#     All OK - return the object blessed into this class.

      return bless \%locate, $class;

   }
}


########################################################################
sub finish {

#+
#  Name:
#     StarIndex::finish

#  Purpose:
#     Signal end of use of StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     $starindex->finish();

#  Description:
#     Tidies up after use of a StarIndex object, in particular ensuring
#     that updates have been flushed from memory to disk.  This method
#     does not need to be called if the program is exited normally, but
#     in the event of an abnormal exit, some DBM libraries (e.g. GDBM
#     in GDBM_FAST mode, and NDBM on Linux, which is effectively the
#     same thing) will leave the DBM file corrupted.  Thus it is a good
#     idea to call it.  After this call the StarIndex object can no
#     longer be used.

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

   my ($rlocate) = @_;

#  Untie object from index.

   untie %$rlocate;
}

########################################################################
sub get {

#+
#  Name:
#     StarIndex::get

#  Purpose:
#     Retrieve a value from a StarIndex object by key.

#  Language:
#     Perl 5

#  Invocation:
#     @locations = $starindex->get($name);
#     $location  = $starindex->get($name);
#     $location  = $starindex->get($name, packpref => $package);
#     $location  = $starindex->get($name, packmust => $package);

#  Description:
#     Retrieves a value from a StarIndex object given the key and,
#     optionally, a required or a hinted Starlink package name.
#
#     If only the name is supplied, then in array context a list of
#     the entries for that name in all packages is returned, and in
#     scalar context just one of them (effectively at random).
#     A null value (undef or an empty list according to context) is
#     returned if the key does not exist.
#
#     If the 'packmust => $package' option is given, then if a value
#     in the specified package exists for the specified key, it is
#     returned, else undef is returned.
#
#     If the 'packpref => $package' option is given, then if a value
#     in the specified package exists for the specified key it is
#     returned, if it only exists in some other package then that is
#     returned, otherwise undef is returned.

#  Arguments:
#     $name = string.
#        Name of key to look up in index.
#     $package = string (optional).
#        Starlink package name - see Description above.

#  Return value:
#     Location of the requested key (a virtual pathname).

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

   my ($rlocate, $name, %options) = @_;

#  Get string containing locations in all packages for requested name,
#  and exit with a null value if there are no locations (probably
#  because the key does not exist).

   my $locations = $rlocate->{$name} || return wantarray ? () : undef;

#  Turn the string into a list of locations.

   my @locations = split ' ', $locations;

#  If no package is specified, return a list of them all.

   if (!$options{'packmust'} && !$options{'packpref'}) {
      return (wantarray ? @locations : shift (@locations));
   }

#  Otherwise look through all of them, returning if we find a suitable one.

   my $packmust = $options{'packmust'} || '';
   my $packpref = $options{'packpref'} || '';
   my ($loc, $pack, $last);
   foreach $loc (@locations) {
      $pack = starpack $loc;
      return $loc if ($pack eq $packmust || $pack eq $packpref);
      $last = $loc;
   }

#  Finally, return any (in fact the last) if the requested package was
#  merely suggested, or null value if the requested one was required.

   return ($packmust ? undef : $last);
}


########################################################################
sub tarlevel {

#+
#  Name:
#     StarIndex::tarlevel

#  Purpose:
#     Determine depth in tarfiles of a logical path name.

#  Language:
#     Perl 5

#  Invocation:
#     $level = tarlevel $location;

#  Description:
#     Given a logical path name, this routine counts how many nested
#     tarfiles deep the specified file is.  Since inclusion in a tar
#     file is indicated by the delimiter '>', this is equivalent to
#     counting the number of '>' characters in the argument string.

#  Arguments:
#     $location = string.
#        Logical path to be examined.

#  Return value:
#     $level = integer.
#        Number of tarfiles deep file is.

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

#  Use side-effect of tr to count number of '>' characters in string.

   return $_[0] =~ tr/>/>/;
}


########################################################################
sub put {

#+
#  Name:
#     StarIndex::put

#  Purpose:
#     Add entry to StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     $starindex->put($name, $location);

#  Description:
#     Writes an entry to the StarIndex object with the specified name
#     and location.  If 'better' entry for the same package under the
#     same name already exists, no action is taken, otherwise the new
#     entry is written to the index.

#  Arguments:
#     $name = string.
#        Key in index.
#     $location = string.
#        Logical pathname of item's location.

#  Return value:
#     Returns true (1) if the write was a success or false (0) if it
#     failed.

#  Notes:
#     Depending on the implementation the of DBM library underlying
#     the Perl tie() function, writing records can generate an error
#     if the size of the key plus value exceeds a block size.
#     This limiting size is 1024 bytes for genuine NDBM (used on
#     Solaris and Digital Unix).  There is no limit for the GNU
#     implementation GDBM, which underlies the NDBM implementation on
#     Linux.  If GDBM were used on the other platforms this would not
#     be a problem, but it does not exist by default on Solaris and
#     Digital Unix.

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

   my ($rlocate, $name, $location) = @_;

   my $newloc;
   if ($rlocate->{$name}) {

#     Value for given name already exists: create hash %loc containing
#     locations keyed by package.

      my ($loc, %loc, $oldloc, $lev, $olev);
      foreach $loc (split ' ', $rlocate->{$name}) {
         $loc{starpack $loc} = $loc;
      }
      my $package = starpack $location;
      $oldloc = $loc{$package};

#     If no location for this package exists, or if the new one is better
#     than the old one, replace it in the index.  Otherwise leave the
#     index entry as it is.
#     One location is better than another if it is deeper nested in tar
#     files (since this gives a better indication of where it comes from
#     in the package distribution) or if it is a plain fortran (.f) file
#     rather than a generic fortran (.gen) file, since function names in
#     the latter may have been generated spuriously by the tagging routine.

      if (!$oldloc ||
          (($olev = tarlevel $oldloc) < ($lev = tarlevel $location)) ||
          ($olev == $lev && $oldloc =~ /\.gen$/ && $location !~ /\.gen$/)
         ) {
         $loc{$package} = $location;
         $newloc = join ' ', values %loc;
      }
   }
   else {

#     No value for given name exists, new entry simply becomes the given
#     location.

      $newloc = $location;
   }

#  If there is no write to be done, return successfully.

   return 1 unless ($newloc);

#  Attempt the write.

   eval { $rlocate->{$name} = $newloc };

#  Set return value appropriately, and return.

   if ($@) {
      (tied %$rlocate)->clearerr();
      return 0;
   }
   else {
      (tied %$rlocate)->sync() if ($Dbmtype eq 'DB_File');
      return 1;
   }

}


########################################################################
sub delete {

#+
#  Name:
#     StarIndex::delete

#  Purpose:
#     Remove an entry from a StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     $starindex->delete($name, $package);
#     $starindex->delete($name);

#  Description:
#     Removes an entry with a given name, and optionally a given package,
#     from a StarIndex object.  If the package argument is not given,
#     then the whole name entry is deleted.  It is not an error to
#     call this method when the item specified does not exist.

#  Arguments:
#     $name = string.
#        Name of index key.
#     $package = string (optional).
#        Starlink package name from which to remove entry.

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

   my ($rlocate, $name, $package) = @_;

#  If no entry exists for given name then exit.

   my $locations = $rlocate->{$name} || return;

#  If no package has been specified then delete the whole entry and exit.

   unless ($package) {
      delete $rlocate->{$name};
      return;
   }

#  Otherwise delete only entries for the package in question; make a
#  list of all the entries that are to be retained, that is all except
#  the one from the specified package.

   my ($loc, @loc, $changed);
   foreach $loc (split ' ', $locations) {
      if (starpack ($loc) eq $package) {
         $changed = 1;
      }
      else {
         push @loc, $loc;
      }
   }

#  If no change, return without altering the record.

   return unless $changed;

#  If there are still entries in the record rewrite the modified record.

   if (@loc) {
      $rlocate->{$name} = join ' ', @loc;
   }

#  If it is now empty remove the record altogether.

   else {
      delete $rlocate->{$name};
   }
}


########################################################################

use vars qw/@each_loc $each_key/;

sub each {

#+
#  Name:
#     StarIndex::each

#  Purpose:
#     Iterator for StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     ($name, $location) = $starindex->each();
#     ($name, $location) = $starindex->each($package);

#  Description:
#     Iterates through the StarIndex object, returning a ($name, $location)
#     entry each time it is called.  If called with no argument, it
#     will return all such pairs, so that for each name key, it may
#     return more than one pair if that name exists in more than one
#     package.  If called with the package argument, it will return
#     each name only once, and only if that name exists in the given
#     package.

#  Arguments:
#     $package = string (optional).
#        Starlink package from which names are to be found.  If absent,
#        a pair is returned for each package which contains each name.

#  Return value:
#     $name = string.
#        Name key of the record.
#     $location = string.
#        Logical pathname of the item's location.

#  Notes:
#     The iterator is based on Perl's core 'each' function, and is
#     subject to the restrictions of that.  In particular the order in
#     which the pairs are returned is undefined, only one iterator
#     can exist for each object, and new entries must not be added to
#     the object while the iterator is running.  The Perl documentation
#     suggests that it should be permissible to delete entries while
#     the iterator is running, but at least with the tie implemented
#     using NDBM_File, this seems to cause problems.  The 'delpack'
#     method is supplied instead for this purpose.

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

   my ($rlocate, $package) = @_;

   my ($key, $value, @loc, $loc);

   if ($package) {

#     If package name has been given, go through hash record by record
#     until one is found with an entry for the requested package (there
#     is never more than one entry per package per record).

      while (($key, $value) = CORE::each %$rlocate) {
         foreach $loc (split ' ', $value) {
            return ($key, $loc) if ((starpack ($loc) || '') eq $package);
         }
      }

#     None found - return empty list to signal that iterator has reached
#     the end of the index.

      return ();
   }
   else {

#     If no package name is given, go through every record of hash,
#     returning once per entry in each record.  Entries from the last-
#     read record are kept in the global list @each_loc.  Only read
#     a new record when @each_loc is empty.

      unless (@each_loc) {
         (($each_key, $value) = CORE::each %$rlocate) || return ();
         @each_loc = split ' ', $value;
      }
      return $each_key, shift (@each_loc);
   }
}


########################################################################
sub delpack {

#+
#  Name:
#     StarIndex::delpack

#  Purpose:
#     Remove all entries from a given package in a StarIndex object.

#  Language:
#     Perl 5

#  Invocation:
#     $starindex->delpack($package);

#  Description:
#     Remove all entries which are in a given package from a StarIndex
#     object.

#  Arguments:
#     $package = string.
#        Starlink package name for which to remove all entries.

#  Return value:

#  Notes:
#     The functionality of this routine could *in principle* be
#     obtained by using a short loop with StarIndex::each and
#     StarIndex::delete; however, the NDBM_File hash tie implementation
#     doesn't seem to like mixing the each() and delete() functions.
#     Thus it is implemented here by separately assembling the list of
#     entries to delete and deleting them all at once.

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

   my ($rlocate, $package) = @_;

#  Assemble list of names which contain entries for the chosen package.

   my ($name, @names, $value);
   while (($name, $value) = $rlocate->each($package)) {
      push @names, $name;
   }

#  Delete each name.

   foreach $name (@names) {
      $rlocate->delete($name, $package);
   }

#  Delete package location entry.

   $rlocate->delete("$package#");
}


########################################################################
sub duplicate {

#+
#  Name:
#     StarIndex::duplicate

#  Purpose:
#     Copy a StarIndex object to a new one.

#  Language:
#     Perl 5

#  Invocation:
#     $newindex = $oldindex->duplicate($newfile);

#  Description:
#     Create a new StarIndex object which is a copy of an old one.
#     This may be better than simply copying the files which hold them,
#     since (depending on the DBM implementation) these files may be
#     sparse, and a normal copy will probably fail to retain the holes
#     as holes, making a destination file which is very much larger.

#  Arguments:
#     $newfile = string.
#        Name of the disk file for storing the data (possibly excluding
#        file extension - this depends on the implementation of the DBM).

#  Return value:
#     $newindex = StarIndex object.
#        The index will have been opened with access mode 'new'.

#  Notes:

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}

#  History:
#     03-DEC-1998 (MBT):
#       Initial revision.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#  Get arguments.

   my ($roldlocate, $newfile) = @_;

#  Create new (empty) object with the right file name.

   my $rnewlocate = new (ref ($roldlocate), $newfile, 'new');

#  Copy records from old object to new object one by one.

   my ($key, $value);
   while (($key, $value) = CORE::each (%$roldlocate)) {
      $rnewlocate->{$key} = $value;
   }

#  Return (already blessed) object.

   return $rnewlocate;
}


1;
# $Id$
