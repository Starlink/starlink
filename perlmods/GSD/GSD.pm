package GSD;
require 5.001;

use strict;
use Carp;

use Exporter;
use DynaLoader;

use vars qw/$VERSION @ISA @EXPORT/;

 
@ISA = qw(Exporter DynaLoader); 
 
@EXPORT = qw( gsdOpenRead gsdClose gsdFind gsdItem gsdInqSize
              gsdGet0d gsdGet0r gsdGet0i gsdGet0l gsdGet0c gsdGet0b
	      gsdGet0w
	      gsdGet1d gsdGet1r gsdGet1i gsdGet1c
	      gsdGet1dp gsdGet1rp gsdGet1ip);

'$Revision$ ' =~ /.*:\s(.*)\s\$/ && ($VERSION = "$1");

bootstrap GSD  $VERSION;


1;
__END__
# Documentation

=head1 NAME

GSD - A module to allow read access to JCMT GSD data.

=head1 SYNOPSIS

  use GSD;

  $status = gsdOpenRead($file,$version,$label,$no_items,$fptr,$file_dsc,
			$item_dsc,$data_ptr);

  $status = gsdGet0c();

=head1 DESCRIPTION

This module add the ability to read JCMT GSD data from perl. Since the
GSD library is read only, GSD files can not be written by this module.

The following library calls are implemented:

=over 4

=item File inquiry

gsdOpenRead gsdClose gsdFind gsdItem gsdInqSize

=item Scalar access

gsdGet0d gsdGet0r gsdGet0i gsdGet0l gsdGet0c gsdGet0b gsdGet0w

=item Array access

gsdGet1d gsdGet1r gsdGet1i gsdGet1c

=item Array access via string

gsdGet1dp gsdGet1rp gsdGet1ip

=back


=head1 NOTES
 
The gsdGet1xp routines return a packed string rather than an array.
This is far more convenient for packages such as PDL which need
to manipulate the array without a perl array overhead.

This library is read-only. 

=head1 AUTHOR
 
This module was written by Tim Jenness, timj@jach.hawaii.edu.
 
=head1 ACKNOWLEDGMENTS

This module would not have been possible without the help
of Karl Glazebrook and Frossie Economou (frossie@jach.hawaii.edu).


=cut
