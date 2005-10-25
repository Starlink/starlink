# -*- perl -*-
#
# Test::AutoBuild::Output::EmailAlert by Daniel Berrange <dan@berrange.com>
#
# Copyright (C) 2002-2004 Daniel Berrange <dan@berrange.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
# $Id$

=pod

=head1 NAME

Test::AutoBuild::Output::EmailAlert - Sends email alerts with build status

=head1 SYNOPSIS

  use Test::AutoBuild::Output::EmailAlert


=head1 DESCRIPTION

This module generates email alerts at the end of a build containing
status information. They can be sent on every cycle, or just when
the cycle has a failure.

=head1 METHODS

=over 4

=cut

package Test::AutoBuild::Output::EmailAlert;

use strict;
use Carp qw(confess);
use Test::AutoBuild::Output;
use Net::SMTP;
use IO::Scalar;
use Template;

use vars qw(@ISA);

@ISA = qw(Test::AutoBuild::Output);


=pod

=item my $mod = Test::AutoBuild::Output::EmailAlert->new(  );

=cut

sub new {
    my $proto = shift;
    my $class = ref($proto) || $proto;
    my $self = $class->SUPER::new(@_);

    bless $self, $class;

    return $self;
}


sub process {
    my $self = shift;
    my $modules = shift;

    # Set up options from configuration file
    my $email_addresses = $self->option("addresses");
    confess "addresses parameter is required" unless (defined $email_addresses);

    my $smtp_server = $self->option("smtp_server");
    confess "smtp_server parameter is required" unless (defined $smtp_server);

    my $sender = $self->option("sender");
    confess "sender parameter is required" unless (defined $sender);

    my $url = $self->option("url");
    my $send_on_success = $self->option("send-on-success");

    my $path = $self->option("template-dir");
    my %config = (
                  INCLUDE_PATH => $path
                  );
    my $template = Template->new(\%config);

    # Local vars
    my $success = 1;
    my $mods = [];

    # Grab data from modules
    foreach my $name (keys %{$modules}) {

        my $this_url = $url;
        $this_url =~ s/\%m/$name/;

        my $mod = {
            'label' => $modules->{$name}->label(),
            'status' => $modules->{$name}->build_status(),
            'buildLog' => $modules->{$name}->build_log(),
            'URL' => $this_url
            };

        push @{$mods}, $mod;

        if ($modules->{$name}->build_status() ne 'success' &&
            $modules->{$name}->build_status() ne 'cache' &&
            ( ! grep { $_ eq $name } split(',',$self->option('ignore-modules')))) {
            # A module failed
            $success = 0;
        }
    }

    if ( ($success == 0) ||
         (lc($send_on_success) eq 'true')) {

      # Either a module failed, or the user requested to receive alerts for successful builds.
      # So, let's send the emails.
      foreach my $email_address (split ',', $email_addresses) {
        my $smtp = Net::SMTP->new($smtp_server);
        die "Couldn't connect to server $smtp_server" unless $smtp;

        $smtp->mail($sender);
        $smtp->to($email_address);

        my %vars = (
                    'recipient' => $email_address,
                    'sender' => $sender,
                    'modules' => $mods
                  );

        my $body;
        $template->process("email.txt", \%vars, IO::Scalar->new(\$body));

        $smtp->data();
        $smtp->datasend ($body);
        $smtp->dataend();

        $smtp->quit();
      }
    }

}



1 # So that the require or use succeeds.

__END__

=back 4

=head1 AUTHORS

Daniel Berrange <dan@berrange.com>

=head1 COPYRIGHT

Copyright (C) 2002-2004 Daniel Berrange <dan@berrange.com>

=head1 SEE ALSO

L<perl(1)>

=cut
