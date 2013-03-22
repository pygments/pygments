#!/usr/bin/env perl

use strict;
use warnings;
use autodie;
use feature qw(say);
use utf8;
use charnames ();

use Encode qw(decode_utf8);
use File::Slurp qw(read_file);
use File::Spec;

my $PYTHON = 'python';
if(system('which python2 1>/dev/null 2>/dev/null') == 0) {
    $PYTHON = 'python2';
}

binmode STDOUT, ':encoding(utf8)';

sub decolorize {
    my ( $line ) = @_;

    $line =~ s/\e\[[\d;]*m//g;

    return $line;
}

sub pygmentize {
    my ( $filename ) = @_;

    my @lines;

    my ( $read, $write );
    pipe $read, $write;
    my $pid = fork;

    if($pid) {
        close $write;
        binmode $read, ':encoding(utf8)';
        while(<$read>) {
            push @lines, $_;
        }
        close $read;
        waitpid $pid, 0;
    } else {
        close $read;
        open STDOUT, '>&', $write;
        open STDERR, '>', '/dev/null';

        exec $PYTHON, './pygmentize', '-l', 'perl6', '-O', 'outencoding=utf-8', $filename;
    }

    return @lines;
}

sub find_difference {
    my ( $lhs, $rhs ) = @_;

    my $index = 0;

    while($index < length($lhs) &&
          $index < length($rhs) &&
          substr($lhs, $index, 1) eq substr($rhs, $index, 1)) {

        $index++;
    }

    my $lhs_char = $index > length($lhs) ? '(none)' : sprintf('0x%x', ord(substr($lhs, $index, 1)));
    my $rhs_char = $index > length($rhs) ? '(none)' : sprintf('0x%x', ord(substr($rhs, $index, 1)));

    return ( $index, $lhs_char, $rhs_char );
}

sub compare_lines {
    my ( $format, $offset, $old_lines, $new_lines ) = @_;

    foreach my $line_no ( 0 .. $#$old_lines ) {
        my $old = $old_lines->[$line_no];
        my $new = $new_lines->[$line_no];

        unless($old eq $new) {
            my ( $pos, $old_char, $new_char ) = find_difference($old, $new);
            $pos = length(decolorize(substr($old, 0, $pos)));

            printf $format . "\n", $line_no + $offset + 1;
            say "Old: '$old_char' (" . charnames::viacode($old_char) . ')';
            say "New: '$new_char' (" . charnames::viacode($new_char) . ')';
            print $old;
            say ' ' x $pos, '↑';
            say ' ' x $pos, '↓';
            print $new;

            return;
        }
    }

    return 1;
}

my @files = grep {
    $_->[0] !~ /^#/
} map {
    chomp;
    [ split /:/, $_ ]
} read_file('p6-scripts/check-files');

foreach my $pair (@files) {
    my ( $filename, $good_line ) = @$pair;
    my $last_version_filename = File::Spec->catfile('p6-scripts/.last-version', $filename);

    unless(-e $last_version_filename) {
        say "'$last_version_filename' does not exist; skipping '$filename'";
        next;
    }

    my @old_lines = map { decode_utf8($_) } read_file($last_version_filename);
    my @new_lines = pygmentize(File::Spec->catfile('tests/examplefiles', $filename));

    unless(@new_lines == @old_lines) {
        say "$filename: line count mismatch";
        next;
    }

    if(defined $good_line) {
        splice @old_lines, 0, $good_line;
        splice @new_lines, 0, $good_line;
    }

    next unless compare_lines("Text for line %d of file $filename does not match",
                              $good_line || 0,
                              [ map { decolorize($_) } @old_lines ],
                              [ map { decolorize($_) } @new_lines ]);

    next unless compare_lines("Colors for line %d of file $filename do not match",
                              $good_line || 0,
                              \@old_lines,
                              \@new_lines);
}
