#!/usr/local/bin/perl -w

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
#     As currently implemented this package uses the NDBM_File package,
#     which is available on most Perl5/Unix platforms.  It could trivially
#     be rewritten to use one of the other DBM-like packages for 
#     reasons of portability or performance, although the different
#     types do not yield binary-compatible index files.  As long as
#     the same implementation of this package is used for writing and
#     subsequently reading the same index files though there should be
#     no problem.

#  Arguments:

#  Notes:

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

use Scb qw/:DEFAULT error/;
use Fcntl;
use NDBM_File;


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

#  Set up mapping of requested access type to Fcntl type access mode.

   %fmode = ( 'read'   => O_RDONLY,
              'update' => O_RDWR | O_CREAT,
              'new'    => O_RDWR | O_CREAT | O_TRUNC,
            );

#  Tie the StarIndex object, which is a hash, to the DBM file.

   my %locate;
   tie %locate, NDBM_File, $indexfile, $fmode{$access}, 0644
      or error "Failed to open dbm file $indexfile - may be corrupted.\n";

#  Return the object blessed into this class.

   return bless \%locate, $class;
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
#     in GDBM_FAST mode, and NDBM on Linux) will leave the DBM file 
#     corrupted.  Thus it is a good idea to call it.  After this call
#     the StarIndex object can no longer be used.

#  Arguments:

#  Return value:

#  Notes:

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
#     $location = $starindex->get($name);
#     $location = $starindex->get($name, packpref => $package);
#     $location = $starindex->get($name, packmust => $package);

#  Description:
#     Retrieves a value from a StarIndex object given the key and,
#     optionally, a required or a hinted Starlink package name.
#     If only the name is supplied, then an entry for that key in any
#     package is returned (or undef if the key does not exist).
#     If the 'packmust => $package' option is given, then if a value
#     in the specified package exists for the specified key, it is
#     returned, else undef is returned.
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
#  and turn it into a list of locations.

   my $locations = $rlocate->{$name} || return undef;
   my @locations = split ' ', $locations;

#  If any package will do, just return the first one we find.

   if (!$options{'packmust'} && !$options{'packpref'}) {
      return shift @locations;
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

#  Notes:

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
#     A location is better than another if it is deeper nested in tar
#     files (since this gives a better indication of where it comes from
#     in the package distribution) or if it is a plain fortran (.f) file
#     rather than a generic fortran (.gen) file, since function names in
#     the latter may have been generated spuriously by the tagging routine.

      if (!$oldloc ||
          (($olev = tarlevel $oldloc) < ($lev = tarlevel $location)) ||
          ($olev == $lev && $oldloc =~ /\.gen$/ && $location !~ /\.gen$/)
         ) {
         $loc{$package} = $location;
         $rlocate->{$name} = join ' ', values %loc;
      }
   }
   else {

#     No value for given name exists, new entry simply becomes the given 
#     location.

      $rlocate->{$name} = $location;
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
#     $starindex->delete();

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
#     The iterator is based on Perl's native 'each' function, and is 
#     subject to the restrictions of that.  In particular the order in
#     which the pairs are returned is undefined, only one iterator 
#     can exist for each object, and new entries must not be added to
#     the object while the iterator is running.  The Perl documentation
#     suggests that it should be permissible to delete entries while
#     the iterator is running, but at least with the tie implemented
#     using NDBM_File, this seems to cause problems.  The 'delpack' 
#     method is supplied instead for this purpose.

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

      while (($key, $value) = each %$rlocate) {
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
         (($each_key, $value) = each %$rlocate) || return ();
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

   my ($name, @names);
   while (($name, $value) = $rlocate->each($package)) {
      push @names, $name;
   }

#  Delete each name.

   foreach $name (@names) {
      $rlocate->delete($name, $package);
   }
}


1;
