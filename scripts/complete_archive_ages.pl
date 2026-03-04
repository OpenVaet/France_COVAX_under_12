#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use Data::Printer;
use Data::Dumper;
use File::Path qw(make_path);
use Text::CSV qw( csv );
use Math::Round qw(nearest);
use Date::DayOfWeek;
use Date::WeekNumber qw/ iso_week_number /;
use Encode;
use Encode::Unicode;
use JSON;
use FindBin;
use Scalar::Util qw(looks_like_number);
use File::stat;
use lib "$FindBin::Bin/lib";
use JSON;
use File::Path qw(make_path);
use HTTP::Tiny;

my %archive_data = ();

my $archive_file = 'data/foreign_archive_fr.json';

load_archive();

# ---- Ollama age extraction ----

my $ollama_model                = 'llama3.1:8b';          # must match `ollama list`
my $ollama_url                  = 'http://127.0.0.1:11434/api/generate';

my $missing_ages_processed_file = 'data/age_from_text_ollama.csv';

my %missing_ages_processed      = ();
load_missing_ages_processed();

my ($current, $total) = (0, 0);

for my $vaers_id (sort{$a <=> $b} keys %archive_data) {
	my $age_group_name = $archive_data{$vaers_id}->{'age_group_name'};
	next if exists $missing_ages_processed{$vaers_id};
	$total++;
}

for my $vaers_id (sort{$a <=> $b} keys %archive_data) {
	my $age_group_name = $archive_data{$vaers_id}->{'age_group_name'};
	next if exists $missing_ages_processed{$vaers_id};
	$current++;
	STDOUT->printflush("\rProcessing missing ages - [$current / $total]");

	my $symptoms_text = $archive_data{$vaers_id}->{'symptom_text'} // die;

	# say "$reception_year-$reception_month | $vaers_id | $symptoms_text";

	my $res = extract_age_with_ollama($symptoms_text);

	append_missing_age_processed(
		$vaers_id,
		$res->{age},         # int or undef
		$res->{status},      # ok / ambiguous / missing / error
		$res->{evidence},
	);

	# say " -> age: " . (defined $res->{age} ? $res->{age} : 'null') . " | status: $res->{status}";
}

# p%stats;

sub load_archive {
	open my $in, '<', $archive_file or die $!;
	my $json;
	while (<$in>) {
		$json .= $_;
	}
	close $in;
	$json = decode_json($json);
	%archive_data = %$json;
}

sub load_missing_ages_processed {
	if (-f $missing_ages_processed_file) {
		open my $in, '<:utf8', $missing_ages_processed_file;
		while (<$in>) {
			chomp;
			next unless length $_;
			# now: vaers_id;age;status;evidence
			my ($vaers_id, $age, $status) = split ';', $_;
			$missing_ages_processed{$vaers_id} = 1;
		}
		close $in;
	}
}

sub append_missing_age_processed {
	my ($vaers_id, $age, $status, $evidence) = @_;

	$status   //= 'error';
	$evidence //= '';

	# Normalize age to integer or literal 'null'
	my $age_out = 'null';
	if (defined $age && $age =~ /^\d+$/) {
		$age_out = int($age);
	}

	# Keep CSV safe with ';' delimiter and single line
	for ($status, $evidence) {
		$_ //= '';
		s/[\r\n]+/ /g;
		s/;/,/g;
		s/\s{2,}/ /g;
		s/^\s+|\s+$//g;
	}

	my ($dir) = $missing_ages_processed_file =~ m{^(.*)/[^/]+$};
	if ($dir && !-d $dir) {
		make_path($dir);
	}

	# If file doesn't exist yet, write a header (optional but useful)
	my $needs_header = !-f $missing_ages_processed_file;

	open my $out, '>>:utf8', $missing_ages_processed_file
		or die "Cannot open $missing_ages_processed_file for append: $!";

	if ($needs_header) {
		print $out "vaers_id;age;status;evidence\n";
	}

	print $out "$vaers_id;$age_out;$status;$evidence\n";
	close $out;

	$missing_ages_processed{$vaers_id} = 1;
}

sub extract_age_with_ollama {
	my ($symptoms_text) = @_;
	$symptoms_text //= '';

	# Keep prompt size reasonable
	my $MAX_CHARS = 6000;
	$symptoms_text = substr($symptoms_text, 0, $MAX_CHARS) if length($symptoms_text) > $MAX_CHARS;

	# Instruction (kept short; schema does the heavy lifting)
	my $prompt = join "\n",
		'Extract the PATIENT age in years from the text.',
		'If multiple patients are mentioned OR multiple different ages are mentioned OR unclear who the age belongs to: status="ambiguous" and age=null.',
		'If no patient age is explicitly present: status="missing" and age=null.',
		'If exactly one patient and exactly one explicit age in years: status="ok" and age=<integer>.',
		'Evidence: short snippet (<=120 chars) supporting the decision; empty if none.',
		'Text:',
		$symptoms_text;

	# JSON schema: forces keys + types
	my $schema = {
		type       => "object",
		properties => {
			age => { anyOf => [ { type => "integer" }, { type => "null" } ] },
			status => {
				type => "string",
				enum => ["ok","ambiguous","missing","error"],
			},
			evidence => { type => "string" },
		},
		required => ["age","status","evidence"],
		additionalProperties => JSON::false,
	};

	my $req = {
		model  => $ollama_model,
		prompt => $prompt,
		stream => JSON::false,
		format => $schema,
		# Optional: unload model after each request (set to "0" if you want that)
		keep_alive => "5m",
	};

	my $http = HTTP::Tiny->new(
		timeout => 120,  # avoid infinite hangs
	);

	my $resp = $http->post(
		$ollama_url,
		{
			headers => { 'content-type' => 'application/json' },
			content => encode_json($req),
		}
	);

	if (!$resp->{success}) {
		warn "Ollama HTTP error: $resp->{status} $resp->{reason}\n";
		return { age => undef, status => 'error', evidence => '' };
	}

	my $body = $resp->{content} // '';
	my $api;
	eval { $api = decode_json($body); 1 } or do {
		warn "Ollama API JSON parse failed. Body was:\n$body\n";
		return { age => undef, status => 'error', evidence => '' };
	};

	# /api/generate returns { response: "<text>", ... } for non-stream
	my $model_text = $api->{response} // '';

	$model_text =~ s/^\s+//;
	$model_text =~ s/\s+$//;

	# Because we used `format` with a schema, response should already be JSON.
	my $data;
	eval { $data = decode_json($model_text); 1 } or do {
		# In case something odd happens, attempt salvage
		if ($model_text =~ /(\{.*\})/s) {
			my $cand = $1;
			eval { $data = decode_json($cand); 1 } or do {};
		}
		if (!$data) {
			warn "Model JSON parse failed. Model output was:\n$model_text\n";
			return { age => undef, status => 'error', evidence => '' };
		}
	};

	my $age      = $data->{age};
	my $status   = $data->{status}   // 'error';
	my $evidence = $data->{evidence} // '';

	# Normalize age (integer or undef)
	if (defined $age) {
		if ($age =~ /^\d+$/) {
			$age = int($age);
			# sanity bounds
			if ($age < 0 || $age > 120) {
				$age = undef;
				$status = 'ambiguous' if $status eq 'ok';
			}
		} else {
			$age = undef;
		}
	}

	# If status isn't ok, force null age
	if ($status ne 'ok') {
		$age = undef;
	}

	# Clamp evidence (and keep it single-line friendly)
	$evidence =~ s/[\r\n]+/ /g;
	$evidence =~ s/\s{2,}/ /g;
	$evidence = substr($evidence, 0, 140);

	return { age => $age, status => $status, evidence => $evidence };
}