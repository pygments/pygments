#!/usr/bin/env perl

use strict;
use warnings;
use autodie;
use feature qw(say);

use File::Slurp qw(read_file);
use File::Spec;

my $PYTHON = 'python';
if(system('which python2 1>/dev/null 2>/dev/null') == 0) {
    $PYTHON = 'python2';
}

my @files = grep {
    !/^#/
} map {
    chomp;
    (split /:/, $_)[0]
} read_file('p6-scripts/check-files');

unless(-d 'p6-scripts/.last-version') {
    mkdir('p6-scripts/.last-version');
}
foreach my $filename (@files) {
    my $last_version_filename = File::Spec->catfile('p6-scripts/.last-version', $filename);

    my $fh;

    open $fh, '>', $last_version_filename;

    my $pid = fork;

    if($pid) {
        waitpid $pid, 0;
    } else {
        open STDOUT, '>&', $fh;
        open STDERR, '>', '/dev/null';
        exec $PYTHON, './pygmentize', '-l', 'perl6', '-O', 'outencoding=utf-8', File::Spec->catfile('tests/examplefiles', $filename);
    }
}
