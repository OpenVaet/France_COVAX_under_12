#!/usr/bin/perl
use strict;
use warnings;
use v5.30;
use Data::Dumper;
use Data::Printer;
binmode STDOUT, ":utf8";
use utf8;
use JSON;
use Math::Round qw(nearest);
use Encode;
use Encode::Unicode;
use Time::Piece;
use Time::Seconds;
use FindBin;
use lib "$FindBin::Bin/../../lib";

my $deaths_file = 'data/insee/insee_deaths_data.json';
my %deaths      = ();
my %statistics  = ();
load_deaths();

for my $file (sort keys %deaths) {
	for my $line_num (sort{$a <=> $b} keys %{$deaths{$file}}) {
		my $age_in_years = $deaths{$file}->{$line_num}->{'age_in_years'} // die;
		my $death_year   = $deaths{$file}->{$line_num}->{'death_year'}   // die;
		my $death_date   = $deaths{$file}->{$line_num}->{'death_date'}   // die;
		next if $death_year < 2010;
		if ($age_in_years >= 5 && $age_in_years < 12) {
			# say "$death_year - $age_in_years";
			$statistics{$death_date}++;			
		}
	}
}

open my $out, '>', 'data/insee/insee_daily_deaths_5_to_11.csv';
say $out "date,total_deaths";
for my $date (sort keys %statistics) {
	my $total_deaths = $statistics{$date} // die;
	say $out "$date,$total_deaths";
}
close $out;

sub load_deaths {
	open my $in, '<:utf8', $deaths_file;
	my $json;
	while (<$in>) {
		chomp $_;
		$json .= $_;
	}
	close $in;
	$json = decode_json($json);
	%deaths = %$json;
}
