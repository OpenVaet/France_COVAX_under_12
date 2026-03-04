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

# ---- Ollama age extraction ----
my $ollama_model                = 'llama3.1:8b';          # must match `ollama list`
my $ollama_url                  = 'http://127.0.0.1:11434/api/generate';

my %stats                       = ();
my %archive_data                = ();
my %missing_ages_processed      = ();
my %vax_verif_processed         = ();

my $archive_file                = 'data/foreign_archive_fr.json';
my $missing_ages_processed_file = 'data/age_from_text_ollama.csv';
my $vax_verif_processed_file    = 'data/vax_from_text_ollama.csv';

# Loads vax verification data.
load_vax_verif_processed();

# Loads archive & ages processing.
load_archive();
load_missing_ages_processed();

process_vax_verifications();

load_vax_verif_processed();

my %covid_reports     = ();
my $vaccines_rescued  = 0;
STDOUT->printflush("\rMerging & Exporting");
for my $vaers_id (sort{$a <=> $b} keys %archive_data) {
	my $age_group_name       = $archive_data{$vaers_id}->{'age_group_name'};
	my $symptoms_text        = $archive_data{$vaers_id}->{'symptom_text'}         // die;
	my $vaers_reception_date = $archive_data{$vaers_id}->{'vaers_reception_date'} // die;
	my $age_years            = $archive_data{$vaers_id}->{'age_years'}            // $missing_ages_processed{$vaers_id} // '';
	$age_years               = '' if $age_years eq 'null';

	my $age_group_id;
	if (length $age_years >= 1 && !$age_group_name) {
		($age_group_id,
			$age_group_name) = age_to_age_group($age_years);
	}

	# say "$vaers_reception_date | $vaers_id | $age_years | $symptoms_text";
	my $has_covid_vax      = 0;
	for my $vaccine_data (@{$archive_data{$vaers_id}->{'vaccines_listed'}}) {
		my $drug_category  = %$vaccine_data{'drug_category'} // die;
		if ($drug_category =~ /COVID-19/) {
			$has_covid_vax = 1;
		}
	}
	my $vax_rescued = 0;
	if (exists $vax_verif_processed{$vaers_id} && ($vax_verif_processed{$vaers_id})) {
		$vaccines_rescued++;
		$has_covid_vax = 1;
		$vax_rescued = 1;
		if ($age_group_name) {
			$stats{$age_group_name}++;
		} else {
			$stats{''}++;
		}
	}
	my $age_rescued = 0;
	if (exists $missing_ages_processed{$vaers_id} && (length $age_years > 0)) {
		$age_rescued = 1;
	}
	if ($has_covid_vax == 1) {
		%{$covid_reports{$vaers_id}} = %{$archive_data{$vaers_id}};
		$covid_reports{$vaers_id}->{'age_group_name'} = $age_group_name;
		$covid_reports{$vaers_id}->{'age_rescued'}    = $age_rescued;
		$covid_reports{$vaers_id}->{'vax_rescued'}    = $vax_rescued;
	}
}
say "";

# -------------------------------------------------
# Export %covid_reports to data/covid_vax_reports.csv
# -------------------------------------------------

my $csv_file = 'data/covid_vax_reports.csv';
my ($dir) = $csv_file =~ m{^(.*)/[^/]+$};
make_path($dir) if $dir && !-d $dir;

my $csv = Text::CSV->new({
    binary   => 1,
    eol      => "\n",
    sep_char => ';',
});

open my $out, '>:utf8', $csv_file
    or die "Cannot open $csv_file: $!";

# Define flat column order
my @columns = (
    'vaers_id',
    'age_group_name',
    'age_rescued',
    'age_years',
    'comp_date',
    'deceased_date',
    'hospitalized',
    'imm_project_number',
    'life_threatening',
    'onset_date',
    'patient_died',
    'permanent_disability',
    'sex_name',
    'symptom_text',
    'symptoms_listed',
    'vaccines_listed',
    'vaers_reception_date',
    'vax_administered_by',
    'vax_date',
    'vax_rescued',
);

$csv->print($out, \@columns);

for my $vaers_id (sort { $a <=> $b } keys %covid_reports) {

    my %row = %{ $covid_reports{$vaers_id} };

    my @values;
    for my $col (@columns) {

        if ($col eq 'vaers_id') {
            push @values, $vaers_id;
            next;
        }

        my $value = $row{$col};

        # JSON encode nested structures
        if (ref $value eq 'ARRAY' || ref $value eq 'HASH') {
            $value = encode_json($value);
        }

        $value //= '';
        push @values, $value;
    }

    $csv->print($out, \@values);
}

close $out;

say "Export complete: $csv_file";

p%covid_reports;

say "vaccines_rescued : $vaccines_rescued";

p%stats;

sub load_vax_verif_processed {
	if (-f $vax_verif_processed_file) {
		open my $in, '<:utf8', $vax_verif_processed_file;
		while (<$in>) {
			chomp;
			next unless length $_;
			my ($vaers_id, $vaccine, $status) = split ';', $_;
			$vax_verif_processed{$vaers_id} = $vaccine;
		}
		close $in;
	}
}

sub age_to_age_group {
	my ($age_years) = @_;
	return (0, 'Unknown') unless defined $age_years && length $age_years >= 1;
	my ($age_group_id, $age_group_name);
	if ($age_years <= 1.99) {
		$age_group_id = '1';
		$age_group_name       = '<2 Years';
	} elsif ($age_years >= 2 && $age_years <= 4.99) {
		$age_group_id = '2';
		$age_group_name = '2 - 4 Years';
	} elsif ($age_years >= 5 && $age_years <= 11.99) {
		$age_group_id = '3';
		$age_group_name = '5 - 11 Years';
	} elsif ($age_years >= 12 && $age_years <= 17.99) {
		$age_group_id = '4';
		$age_group_name = '12 - 17 Years';
	} elsif ($age_years >= 18 && $age_years <= 24.99) {
		$age_group_id = '5';
		$age_group_name = '18 - 24 Years';
	} elsif ($age_years >= 25 && $age_years <= 49.99) {
		$age_group_id = '6';
		$age_group_name = '25 - 49 Years';
	} elsif ($age_years >= 50 && $age_years <= 64.99) {
		$age_group_id = '6';
		$age_group_name = '50 - 64 Years';
	} elsif ($age_years >= 65) {
		$age_group_id = '7';
		$age_group_name = '65+ Years';
	} else {
		die "age_years : $age_years";
	}
	return ($age_group_id, $age_group_name);
}

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
			$missing_ages_processed{$vaers_id} = $age;
		}
		close $in;
	}
}

sub process_vax_verifications {
	my %to_process        = ();
	my ($current, $total) = (0, 0);
	for my $vaers_id (sort{$a <=> $b} keys %archive_data) {
		my $comp_date          = $archive_data{$vaers_id}->{'comp_date'}            // die;
		my $has_covid_vax      = 0;
		for my $vaccine_data (@{$archive_data{$vaers_id}->{'vaccines_listed'}}) {
			my $drug_category  = %$vaccine_data{'drug_category'} // die;
			if ($drug_category =~ /COVID-19/) {
				$has_covid_vax = 1;
			}
		}
		if ($has_covid_vax == 0 && $comp_date >= 20201201) {
			next if exists $vax_verif_processed{$vaers_id};
			$total++;
			$to_process{$vaers_id} = 1;
		}
	}
	for my $vaers_id (sort{$a <=> $b} keys %to_process) {
		$current++;
		STDOUT->printflush("\rProcessing missing ages - [$current / $total]");
		my $symptoms_text        = $archive_data{$vaers_id}->{'symptom_text'}         // die;

		# say "$vaers_reception_date | $vaers_id | $age_years | $symptoms_text";


		my $res = verify_covid_vax($symptoms_text);

		append_vax_verif_processed(
			$vaers_id,
			$res->{vaccine},     # varchar or undef
			$res->{status},      # ok / ambiguous / missing / error
			$res->{evidence},
		);

		# say " -> vaccine: " . (defined $res->{vaccine} ? $res->{vaccine} : 'null') . " | status: $res->{status}";
	}
}

sub verify_covid_vax {
	my ($symptoms_text) = @_;
	$symptoms_text //= '';

	# Keep prompt size reasonable
	my $MAX_CHARS = 6000;
	$symptoms_text = substr($symptoms_text, 0, $MAX_CHARS) if length($symptoms_text) > $MAX_CHARS;

	# Instruction (kept short; schema does the heavy lifting)
	my $prompt = join "\n",
		'Verify if a COVID vaccine has been administered to the patient from the text.',
		'If multiple vaccines are mentioned OR multiple patients are mentioned OR if it is unclear which vaccine was administered: status="ambiguous" and vaccine=null.',
		'If no vaccine is explicitly present: status="missing" and vaccine=null.',
		'If exactly one patient and a COVID vaccine is mentioned: status="ok" and vaccine=<varchar>.',
		'The vaccine entry shall be vaccine=Pfizer if BNT162b2, Comirnaty or Pfizer COVID vaccine is mentioned.',
		'The vaccine entry shall be vaccine=Moderna if SpikeVax, Moderna COVID vaccine is mentioned.',
		'The vaccine entry shall be vaccine=Astrazeneca if Astrazeneca COVID vaccine is mentioned.',
		'Evidence: short snippet (<=120 chars) supporting the decision; empty if none.',
		'Text:',
		$symptoms_text;

	# JSON schema: forces keys + types
	my $schema = {
		type       => "object",
		properties => {
			vaccine => { anyOf => [ { type => "string" }, { type => "null" } ] },
			status => {
				type => "string",
				enum => ["ok","ambiguous","missing","error"],
			},
			evidence => { type => "string" },
		},
		required => ["vaccine","status","evidence"],
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
		return { vaccine => undef, status => 'error', evidence => '' };
	}

	my $body = $resp->{content} // '';
	my $api;
	eval { $api = decode_json($body); 1 } or do {
		warn "Ollama API JSON parse failed. Body was:\n$body\n";
		return { vaccine => undef, status => 'error', evidence => '' };
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
			return { vaccine => undef, status => 'error', evidence => '' };
		}
	};

	my $vaccine  = $data->{vaccine};
	my $status   = $data->{status}   // 'error';
	my $evidence = $data->{evidence} // '';

	# If status isn't ok, force null vaccine
	if ($status ne 'ok') {
		$vaccine = undef;
	}

	# Clamp evidence (and keep it single-line friendly)
	$evidence =~ s/[\r\n]+/ /g;
	$evidence =~ s/\s{2,}/ /g;
	$evidence = substr($evidence, 0, 140);

	return { vaccine => $vaccine, status => $status, evidence => $evidence };
}

sub append_vax_verif_processed {
	my ($vaers_id, $vaccine, $status, $evidence) = @_;

	$status   //= 'error';
	$evidence //= '';
	$vaccine  //= '';

	# Keep CSV safe with ';' delimiter and single line
	for ($status, $evidence) {
		$_ //= '';
		s/[\r\n]+/ /g;
		s/;/,/g;
		s/\s{2,}/ /g;
		s/^\s+|\s+$//g;
	}

	my ($dir) = $vax_verif_processed_file =~ m{^(.*)/[^/]+$};
	if ($dir && !-d $dir) {
		make_path($dir);
	}

	# If file doesn't exist yet, write a header (optional but useful)
	my $needs_header = !-f $vax_verif_processed_file;

	open my $out, '>>:utf8', $vax_verif_processed_file
		or die "Cannot open $vax_verif_processed_file for append: $!";

	if ($needs_header) {
		print $out "vaers_id;vaccine;status;evidence\n";
	}

	print $out "$vaers_id;$vaccine;$status;$evidence\n";
	close $out;

	$missing_ages_processed{$vaers_id} = 1;
}