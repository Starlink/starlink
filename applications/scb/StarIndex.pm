#!/usr/local/bin/perl -w

package Directory;

use Scb;
use NDBM_File;


########################################################################
sub new {

   my ($pkg, $indexfile, $mode) = @_;
   my %locate;

   tie %locate, NDBM_File, $indexfile, $mode, 0644
      or die "Failed to open dbm file $indexfile - may be corrupted.\n";

   return bless \%locate, $pkg;
}


########################################################################
sub get {

   my ($rlocate, $name, %options) = @_;

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

   return $_[0] =~ tr/>/>/;
}


########################################################################
sub put {

   my ($rlocate, $name, $location) = @_;

#  Write entry to database if there is not already a better entry for
#  that module name in the same package.

   my $package = starpack $location;
   if ($rlocate->{$name}) {
      my ($loc, %loc, $oldloc, $lev, $olev);
      foreach $loc (split ' ', $rlocate->{$name}) {
         $loc{starpack $loc} = $loc;
      }
      $oldloc = $loc{$package};
      $loc{$package} = $location
         if (!$oldloc ||
             (($olev = tarlevel $oldloc) < ($lev = tarlevel $location)) ||
             ($olev == $lev && $oldloc =~ /\.gen$/ && $location !~ /\.gen$/)
            );
      $rlocate->{$name} = join ' ', values %loc;
   }
   else {
      $rlocate->{$name} = $location;
   }
}


########################################################################
sub delete {

   my ($rlocate, $name, $package) = @_;

   my $locations = $rlocate->{$name} || return;

#  If no package specified delete the whole entry.

   unless ($package) {
      delete $rlocate->{$name};
      return;
   }

#  Otherwise delete only entries for the package in question.

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

   my ($rlocate, $package) = @_;

   return (each %$rlocate) unless ($package);

   my ($key, $value, @loc, $loc);
   while (($key, $value) = each %$rlocate) {
      @loc = split ' ', $value;
      foreach $loc (split ' ', $value) {
         return ($key, $loc) if ((starpack ($loc) || '') eq $package);
      }
   }
   return ();
}


1;
