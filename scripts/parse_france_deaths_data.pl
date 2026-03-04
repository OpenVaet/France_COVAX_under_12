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

# Deaths data are grabbed on https://www.insee.fr/fr/information/4190491 & https://www.insee.fr/fr/information/4769950
# Population data are grabbed on https://www.insee.fr/fr/statistiques/1893198 -> Estimation de population par région, sexe et âge quinquennal - Années 1975 à 2022
# This script assumes that the .CSV files, cleansed from major encoding impurities, are placed in the following folder.
my $deaths_folder = "data/insee/deaths_data";

my %statistics    = ();
my %raw_data      = ();
my %data_labels   = ();

parse_files();

open my $out, '>:utf8', 'data/insee/insee_deaths_data.json' or die $!;
print $out encode_json\%raw_data;
close $out;

sub parse_files {
	for my $file (glob "$deaths_folder/*") {
		%data_labels  = ();
		my $expected_values = 0;
		say $file;
		my $line_num   = 0;
		open my $in, '<:utf8', $file;
		while (<$in>) {
			chomp $_;

			# Cleaning various formatting errors ; as we can expect from a file compiled by the french administration ...
			$_ =~ s/\¤/n/g;
			$_ =~ s/‚cŠs/CSS/g;
			$_ =~ s/\²/2/g;
			$_ =~ s/\°/\)/g;
			$_ =~ s/\¨/A/g;
			$_ =~ s/Â\)/A/g;
			$_ =~ s/å/a/g;
			$_ =~ s/Â/a/g;
			$_ =~ s/à/a/g;
			$_ =~ s/ã/a/g;
			$_ =~ s/Ç/C/g;
			$_ =~ s/¿/C/g;
			$_ =~ s/ïC½/e/g;
			$_ =~ s/Ã©/e/g;
			$_ =~ s/ÃA/e/g;
			$_ =~ s/Ë/E/g;
			$_ =~ s/é/e/g;
			$_ =~ s/è/e/g;
			$_ =~ s/Ö/o/g;
			$_ =~ s/ø/o/g;
			$_ =~ s/µ/u/g;
			$_ =~ s/Ü/U/g;
			$_ =~ s/ý/y/g;
			$_ =~ s/¥/y/g;
			$_ =~ s/­//g;
			$_ =~ s/ //g;
			$_ =~ s///g;
			$_ =~ s///g;
			# say '$_ : ' . $_;
			$line_num++;

			# Verifying line.
			my $line = $_;
			$line = decode("ascii", $line);
			for (/[^\n -~]/g) {
				say $line;
			    printf "In [$file], bad character: %02x\n", ord $_;
			    die;
			}

			$line = strip_missformattings($line);
			$line =~ s/\"//g;
			# say "line : $line";

			# First row = line labels.
			if ($line_num == 1) {
				my @labels;
				if ($line =~ /.*;.*;.*;.*;/) {
					@labels = split ';', $line;
				} else {
					@labels = split ',', $line;
				}
				my $label_num = 0;
				for my $label (@labels) {
					$data_labels{$label_num} = $label;
					$label_num++;
				}
				$expected_values = keys %data_labels;
			} else {

				my (
					$name, $sex,
					$birth_city, $birth_date,
					$birth_place, $birth_country,
					$death_date, $death_place,
					$last_name, $first_name,
					$age_in_days, $age_in_years,
					$sex_name, $death_year
				) = values_from_line($line);
				next unless $sex_name;
				my $insee_age_group     = age_group_from_age($age_in_days, $age_in_years);
				my (undef, $death_month, $death_day, $death_quarter) = quarter_from_date($death_date);
				# say "death_year       : $death_year";
				# say "name             : $name";
				# say "sex_name         : $sex_name";
				# say "first_name       : $first_name";
				# say "last_name        : $last_name";
				# say "birth_date       : $birth_date";
				# say "death_date       : $death_date";
				# say "quarter          : $quarter";
				# say "age_in_days      : $age_in_days";
				# say "age_in_years     : $age_in_years";
				# say "insee_age_group  : $insee_age_group";
				$raw_data{$file}->{$line_num}->{'name'} = $name;
				$raw_data{$file}->{$line_num}->{'sex'} = $sex;
				$raw_data{$file}->{$line_num}->{'birth_city'} = $birth_city;
				$raw_data{$file}->{$line_num}->{'birth_date'} = $birth_date;
				$raw_data{$file}->{$line_num}->{'birth_place'} = $birth_place;
				$raw_data{$file}->{$line_num}->{'birth_country'} = $birth_country;
				$raw_data{$file}->{$line_num}->{'death_date'} = $death_date;
				$raw_data{$file}->{$line_num}->{'death_place'} = $death_place;
				$raw_data{$file}->{$line_num}->{'last_name'} = $last_name;
				$raw_data{$file}->{$line_num}->{'first_name'} = $first_name;
				$raw_data{$file}->{$line_num}->{'age_in_days'} = $age_in_days;
				$raw_data{$file}->{$line_num}->{'age_in_years'} = $age_in_years;
				$raw_data{$file}->{$line_num}->{'sex_name'} = $sex_name;
				$raw_data{$file}->{$line_num}->{'death_year'} = $death_year;
				$raw_data{$file}->{$line_num}->{'death_month'} = $death_month;
				$raw_data{$file}->{$line_num}->{'death_day'} = $death_day;
				$raw_data{$file}->{$line_num}->{'death_quarter'} = $death_quarter;
				$raw_data{$file}->{$line_num}->{'insee_age_group'} = $insee_age_group;
			}
		}
		close $in;
		# say "line_num : $line_num";
	}
}

sub values_from_line {
	my ($line) = @_;
	my @values;
	if ($line =~ /.*;.*;.*;.*;/) {
		@values = split ';', $line;
	} else {
		@values = split ',', $line;
	}
	my $val_n  = 0;
	my %values = ();
	for my $value (@values) {
		my $label = $data_labels{$val_n};
		unless ($label) {
			die "line : $line";
		}
		$values{$label} = $value;
		$val_n++;
	}
	my $name          = $values{'nomprenom'} // die "line : $line" . p@values;
	my $sex           = $values{'sexe'}      // die "line : $line";
	my $birth_city    = $values{'commnaiss'} // die "line : $line";
	my $birth_date    = $values{'datenaiss'} // die "line : $line";
	my ($birth_year,
		$birth_month,
		$birth_day)   = $birth_date =~ /(....)(..)(..)/;
	die unless $birth_year && $birth_month && $birth_day;
	if ($birth_month  eq '00') {
		$birth_month  = '01';
	}
	if ($birth_day    eq '00') {
		$birth_day    = '01';
	}
	$birth_date       = "$birth_year-$birth_month-$birth_day";
	if ($birth_date  eq '1898-02-29') {
		$birth_day    = '28';
		$birth_month  = '02';
		$birth_year   = '1898';
	}
	my $birth_place   = $values{'lieunaiss'} // die "line : $line";
	my $birth_country = $values{'paysnaiss'} // die "line : $line";
	my $death_date    = $values{'datedeces'} // die "line : $line";
	my ($death_year,
		$deathMonth,
		$deathDay)   = $death_date =~ /(....)(..)(..)/;
	if (!$deathDay && $death_date =~ /....../) {
		($death_year,
		$deathMonth) = $death_date =~ /(....)(..)/;
		$deathDay    = '01';
	}
	die "death_date : $death_date" unless $death_year && $deathMonth && $deathDay;
	if ($deathMonth  eq '00') {
		$deathMonth  = '01';
	}
	if ($deathDay    eq '00') {
		$deathDay    = '01';
	}
	$death_date       = "$death_year-$deathMonth-$deathDay";
	if ($birth_year < 1900) {
		$statistics{'invalidBirthYear'}++;
		return;
	}
	if ($death_year < 2010) {
		$statistics{'outOfScope'}++;
		return;
	}
	my $death_place   = $values{'lieudeces'} // die "line : $line";
	my ($last_name, $first_name) = split '\*', $name;
	if ($name eq 'MARIE ANTOINE JOSEPH DIT MARIE ANTOINE JOSEPH ALIAS SOOSAIRAJ DIT AUSSI MARIE J/') {
		$first_name = 'MARIE ANTOINE';
		$last_name  = 'JOSEPH';
	}
	die "name : [$name]" unless $first_name;
	$first_name       =~ s/\/$//;
	# say "first_name    : $first_name";
	# say "last_name     : $last_name";
	# say "sex           : $sex";
	# say "birth_city    : $birth_city";
	# say "birth_date    : $birth_date";
	# say "birth_place   : $birth_place";
	# say "birth_country : $birth_country";
	# say "death_date    : $death_date";
	# say "death_place   : $death_place";
	my ($age_in_days, $age_in_years) = calculate_age($birth_date . ' 12:00:00', $death_date . ' 12:00:00');
	my $sex_name;
	if ($sex == 1) {
		$sex_name = 'Male';
	} else {
		$sex_name = 'Female';
	}
	return (
		$name, $sex,
		$birth_city, $birth_date,
		$birth_place, $birth_country,
		$death_date, $death_place,
		$last_name, $first_name,
		$age_in_days, $age_in_years,
		$sex_name, $death_year
	);
}

sub calculate_age {
    my ($birth, $death) = @_;

    my $birth_tp = Time::Piece->strptime($birth, "%Y-%m-%d %H:%M:%S");
    my $death_tp = Time::Piece->strptime($death, "%Y-%m-%d %H:%M:%S");

    my $seconds = $death_tp - $birth_tp;

    my $age_in_days  = int($seconds / ONE_DAY);
    my $age_in_years = int($age_in_days / 365.2425);  # average year length

    return ($age_in_days, $age_in_years);
}

sub age_group_from_age {
	my ($age_in_days, $age_in_years) = @_;
	$age_in_years = int($age_in_years * 100) / 100;
	$age_in_years =~ s/-//;
	my $insee_age_group;
	if ($age_in_years >= 0 && $age_in_years <= 0.99) {
		$insee_age_group = '0 to 1 Years Old';
	} elsif ($age_in_years >= 1 && $age_in_years <= 4.99) {
		$insee_age_group = '1 to 4 Years Old';
	} elsif ($age_in_years >= 5 && $age_in_years <= 9.99) {
		$insee_age_group = '5 to 9 Years Old';
	} elsif ($age_in_years >= 10 && $age_in_years <= 14.99) {
		$insee_age_group = '10 to 14 Years Old';
	} elsif ($age_in_years >= 15 && $age_in_years <= 19.99) {
		$insee_age_group = '15 to 19 Years Old';
	} elsif ($age_in_years >= 20 && $age_in_years <= 24.99) {
		$insee_age_group = '20 to 24 Years Old';
	} elsif ($age_in_years >= 25 && $age_in_years <= 29.99) {
		$insee_age_group = '25 to 29 Years Old';
	} elsif ($age_in_years >= 30 && $age_in_years <= 34.99) {
		$insee_age_group = '30 to 34 Years Old';
	} elsif ($age_in_years >= 35 && $age_in_years <= 39.99) {
		$insee_age_group = '35 to 39 Years Old';
	} elsif ($age_in_years >= 40 && $age_in_years <= 44.99) {
		$insee_age_group = '40 to 44 Years Old';
	} elsif ($age_in_years >= 45 && $age_in_years <= 49.99) {
		$insee_age_group = '45 to 49 Years Old';
	} elsif ($age_in_years >= 50 && $age_in_years <= 54.99) {
		$insee_age_group = '50 to 54 Years Old';
	} elsif ($age_in_years >= 55 && $age_in_years <= 59.99) {
		$insee_age_group = '55 to 59 Years Old';
	} elsif ($age_in_years >= 60 && $age_in_years <= 64.99) {
		$insee_age_group = '60 to 64 Years Old';
	} elsif ($age_in_years >= 65 && $age_in_years <= 69.99) {
		$insee_age_group = '65 to 69 Years Old';
	} elsif ($age_in_years >= 70 && $age_in_years <= 74.99) {
		$insee_age_group = '70 to 74 Years Old';
	} elsif ($age_in_years >= 75 && $age_in_years <= 79.99) {
		$insee_age_group = '75 to 79 Years Old';
	} elsif ($age_in_years >= 80 && $age_in_years <= 84.99) {
		$insee_age_group = '80 to 84 Years Old';
	} elsif ($age_in_years >= 85 && $age_in_years <= 89.99) {
		$insee_age_group = '85 to 89 Years Old';
	} elsif ($age_in_years >= 90 && $age_in_years <= 94.99) {
		$insee_age_group = '90 to 94 Years Old';
	} elsif ($age_in_years >= 95 && $age_in_years <= 125) {
		$insee_age_group = '95 Years Old & Older';
	} else {
		say "age_in_days  : $age_in_days";
		die "age_in_years : $age_in_years";
	}
	return $insee_age_group;
}

sub strip_missformattings {
	my ($line) = @_;
	$line =~ s/AMAHRAD, AIT MAJTEN, OULTANA/AMAHRAD - AIT MAJTEN - OULTANA/g;
	$line =~ s/SINTRA, SAN MARTINHO/SINTRA - SAN MARTINHO/g;
	$line =~ s/CASTANHEIRA DO VOUGA, AGUEDA/CASTANHEIRA DO VOUGA - AGUEDA/g;
	$line =~ s/PHILIPPEVILLE, DEPARTEMENT DE/PHILIPPEVILLE - DEPARTEMENT DE/g;
	$line =~ s/AVIANO, PROVINCE DE UDINE/AVIANO - PROVINCE DE UDINE/g;
	$line =~ s/AIN KERMA, CONSTANTINE/AIN KERMA - CONSTANTINE/g;
	$line =~ s/ABIDJAN, COMMUNE DE COCODY/ABIDJAN - COMMUNE DE COCODY/g;
	$line =~ s/MARBURG A\.D; LAHN/MARBURG A\.D LAHN/g;
	$line =~ s/TONGRES, PROVINCE DE LIMBOURG/TONGRES - PROVINCE DE LIMBOURG/g;
	$line =~ s/LLOMBAY; PROVINCE/LLOMBAY - PROVINCE/g;
	$line =~ s/COLERE, PROVINCE DE BERGAME/COLERE - PROVINCE DE BERGAME/g;
	$line =~ s/IRLANDE, OU EIRE/IRLANDE - OU EIRE/g;
	$line =~ s/IMRABTEN, AL HOCEIMA/IMRABTEN - AL HOCEIMA/g;
	$line =~ s/VINH, PROVINCE DE NGHE AN/VINH - PROVINCE DE NGHE AN/g;
	$line =~ s/CANAVESES,VALPACOS/CANAVESES - VALPACOS/g;
	$line =~ s/NICASTRO, DESORMAIS LAMEZIA TE/NICASTRO - DESORMAIS LAMEZIA TE/g;
	$line =~ s/AIT MIMOUN, KEMISSET/AIT MIMOUN - KEMISSET/g;
	$line =~ s/AVIANO, PROVINCE DE PORDENONE/AVIANO - PROVINCE DE PORDENONE/g;
	$line =~ s/SNOUL\/KRATIE, PNOM PENH/SNOUL\/KRATIE - PNOM PENH/g;
	$line =~ s/DASHTE BARCHI, KABOUL/DASHTE BARCHI - KABOUL/g;
	$line =~ s/OUTETTA, MATEUR/OUTETTA - MATEUR/g;
	$line =~ s/SOUTHWARK, LONDRES/SOUTHWARK - LONDRES/g;
	$line =~ s/PINHO, BOTICAS/PINHO - BOTICAS/g;
	$line =~ s/AIN MANSOUR, BENI GORFET/AIN MANSOUR - BENI GORFET/g;
	$line =~ s/PESO, COVILHA/PESO - COVILHA/g;
	$line =~ s/TEIXEIRO, COMMUNE DE BAIAO/TEIXEIRO - COMMUNE DE BAIAO/g;
	$line =~ s/COBLENCE, RHENANIE/COBLENCE - RHENANIE/g;
	$line =~ s/MASCARA, AIN FRASS/MASCARA - AIN FRASS/g;
	$line =~ s/KOZLJE, NOVI PAZAR/KOZLJE - NOVI PAZAR/g;
	$line =~ s/DAVOS, CANTON DES GRISONS/DAVOS - CANTON DES GRISONS/g;
	$line =~ s/TOMELLOSO, CIUDAD REAL/TOMELLOSO - CIUDAD REAL/g;
	$line =~ s/TROVISCOSO, MONCAO/TROVISCOSO - MONCAO/g;
	$line =~ s/IFIGHA, COMMUNE DE AZAZGA, GRA/IFIGHA - COMMUNE DE AZAZGA - GRA/g;
	$line =~ s/ARGEL, KOTAYK/ARGEL - KOTAYK/g;
	$line =~ s/PUSSEMANGE, PROVINCE DE LUXEMB/PUSSEMANGE - PROVINCE DE LUXEMB/g;
	$line =~ s/FORT BAYARD, TERRITOIRE DE KOU/FORT BAYARD - TERRITOIRE DE KOU/g;
	$line =~ s/OSILO, PROVINCE DE SASSARI/OSILO - PROVINCE DE SASSARI/g;
	$line =~ s/CALENGIANUS, SARDAIGNE/CALENGIANUS - SARDAIGNE/g;
	$line =~ s/AIN BEIDA, CONSTANTINE/AIN BEIDA - CONSTANTINE/g;
	$line =~ s/ZAOUIAT CHEIKH, BENI MELLLAL/ZAOUIAT CHEIKH - BENI MELLLAL/g;
	$line =~ s/PEDRONERAS, CUENCA/PEDRONERAS - CUENCA/g;
	$line =~ s/ALGINET, PROVINCE DE  VALENCIA/ALGINET - PROVINCE DE  VALENCIA/g;
	$line =~ s/AIT MOULI, AIN LEUH/AIT MOULI - AIN LEUH/g;
	$line =~ s/ALAVERA LA REINA, PROVINCE DE/ALAVERA LA REINA - PROVINCE DE/g;
	$line =~ s/ALGER, EL BIAR/ALGER - EL BIAR/g;
	$line =~ s/PUENTE-GENIL, CORDOUE/PUENTE-GENIL - CORDOUE/g;
	$line =~ s/CURUCA, ETAT DU PARA/CURUCA - ETAT DU PARA/g;
	$line =~ s/SAO FRANCISCO DA SERRA, SANTIA/SAO FRANCISCO DA SERRA - SANTIA/g;
	$line =~ s/DIVIGNANO, PROVINCE DE NOVARA/DIVIGNANO - PROVINCE DE NOVARA/g;
	$line =~ s/LAUSANNE, VAUD/LAUSANNE - VAUD/g;
	$line =~ s/VALEA SEACA, BACAU/VALEA SEACA - BACAU/g;
	$line =~ s/CHENE-BOUGERIES, CANTON DE GEN/CHENE-BOUGERIES - CANTON DE GEN/g;
	$line =~ s/DONAUESCHINGEN, BADE/DONAUESCHINGEN - BADE/g;
	$line =~ s/RASSKAZOVO, REGION DE TAMBOV/RASSKAZOVO - REGION DE TAMBOV/g;
	$line =~ s/RMADATE OULED ALIANE, TISSA/RMADATE OULED ALIANE - TISSA/g;
	$line =~ s/GRANJAL, SERNANCELHE/GRANJAL - SERNANCELHE/g;
	$line =~ s/MAHAMASINA NORD, TANANARIVE/MAHAMASINA NORD - TANANARIVE/g;
	$line =~ s/GRILO, COMMUNE DE BAIAO/GRILO - COMMUNE DE BAIAO/g;
	$line =~ s/CATADAU, VALENCE/CATADAU - VALENCE/g;
	$line =~ s/CAPINHA, COMMUNE DE FUNDAO/CAPINHA - COMMUNE DE FUNDAO/g;
	$line =~ s/DJEMILA, DEPARTEMENT CONSTANTI/DJEMILA - DEPARTEMENT CONSTANTI/g;
	$line =~ s/FARAFANGANA, DISTRICT DU DIT,",/FARAFANGANA - DISTRICT DU DIT,/g;
	$line =~ s/SIDI BELYOUT, CASABLANCA/SIDI BELYOUT - CASABLANCA/g;
	$line =~ s/SANTO ILDEFONSO, PORTO/SANTO ILDEFONSO - PORTO/g;
	$line =~ s/MAKHINJAURI KHELVACHAURI, GEOR/MAKHINJAURI KHELVACHAURI - GEOR/g;
	$line =~ s/DOUAR IGHAZOUN, NKOB, ZAGORA/DOUAR IGHAZOUN - NKOB - ZAGORA/g;
	$line =~ s/TBILISSI, GEORGIE/TBILISSI - GEORGIE/g;
	$line =~ s/GRAZALEMA, PROVINCE DE CADIX/GRAZALEMA - PROVINCE DE CADIX/g;
	$line =~ s/OULED M'HAMED, M'SAADA, KENITR/OULED M'HAMED - M'SAADA - KENITR/g;
	$line =~ s/SANTIAGO DE LITEM, POMBAL/SANTIAGO DE LITEM - POMBAL/g;
	$line =~ s/SULZBACH, SARRE/SULZBACH - SARRE/g;
	$line =~ s/BOUZAREA, ALGER/BOUZAREA - ALGER/g;
	$line =~ s/MALCATA, SABUGAL/MALCATA - SABUGAL/g;
	$line =~ s/LYEPYEL, VITEBSK OBLAST/LYEPYEL - VITEBSK OBLAST/g;
	$line =~ s/SEVILLEJA DE LA JARA, PROVINCE/SEVILLEJA DE LA JARA - PROVINCE/g;
	$line =~ s/CAPOVALLE, DEPARTEMENT DE BRES/CAPOVALLE - DEPARTEMENT DE BRES/g;
	$line =~ s/SEGNACCO, COMMUNE DE TARCENTO/SEGNACCO - COMMUNE DE TARCENTO/g;
	$line =~ s/MARTIN DE YELTES, SALAMANCA/MARTIN DE YELTES - SALAMANCA/g;
	$line =~ s/CHAMARTIN DE LA ROSA, PROVINCE/CHAMARTIN DE LA ROSA - PROVINCE/g;
	$line =~ s/MAIORGA, COMMUNE DE ALCOBACA/MAIORGA - COMMUNE DE ALCOBACA/g;
	$line =~ s/CABECA GORDA, BEJA/CABECA GORDA - BEJA/g;
	$line =~ s/LESAK, LEPOSAVIC/LESAK - LEPOSAVIC/g;
	$line =~ s/BENI-MESTER, DEPARTEMENT DE TL/BENI-MESTER - DEPARTEMENT DE TL/g;
	$line =~ s/SERRADIFALCO, PROVINCE DE CALT/SERRADIFALCO - PROVINCE DE CALT/g;
	$line =~ s/AIN JEMAA, MEKNES/AIN JEMAA - MEKNES/g;
	$line =~ s/DIAKON, BAFOULABE/DIAKON - BAFOULABE/g;
	$line =~ s/COTOBADE, PONTEVEDRA/COTOBADE - PONTEVEDRA/g;
	$line =~ s/OULED LAOUAR, MESSAAD/OULED LAOUAR - MESSAAD/g;
	$line =~ s/VEERSSEN, UELZEN/VEERSSEN - UELZEN/g;
	$line =~ s/RAVANUSA, PROVINCE D'AGRIGENTO/RAVANUSA - PROVINCE D'AGRIGENTO/g;
	$line =~ s/OULED TAYEB, OULIA SAIS \(FES\)/OULED TAYEB - OULIA SAIS \(FES\)/g;
	$line =~ s/CURTLOW, ENNISCORTHY, COMTE WE/CURTLOW - ENNISCORTHY - COMTE WE/g;
	$line =~ s/MEINEDO, LOUSADA/MEINEDO - LOUSADA/g;
	$line =~ s/THAR ES-SOUK, TAOUNATE/THAR ES-SOUK - TAOUNATE/g;
	$line =~ s/BARGAS, PROVINCE DE TOLEDE/BARGAS - PROVINCE DE TOLEDE/g;
	$line =~ s/SEDIELOS, PESO DA REGUA/SEDIELOS - PESO DA REGUA/g;
	$line =~ s/GOLUNGO ALTO, ANGOLA/GOLUNGO ALTO - ANGOLA/g;
	$line =~ s/RESENDE, RIO DE JANEIRO/RESENDE - RIO DE JANEIRO/g;
	$line =~ s/MEKELE, ADDIS-ABEBA/MEKELE - ADDIS-ABEBA/g;
	$line =~ s/CORRERAH, BOKE/CORRERAH - BOKE/g;
	$line =~ s/SEMIDE, MIRANDA DO CORVO/SEMIDE - MIRANDA DO CORVO/g;
	$line =~ s/LONG TRUONG, BIEN HOA/LONG TRUONG - BIEN HOA/g;
	$line =~ s/TLETA LOUTA, NADOR/TLETA LOUTA - NADOR/g;
	$line =~ s/IBADISSEN, COMMUNE DE OUADHIA/IBADISSEN - COMMUNE DE OUADHIA/g;
	$line =~ s/JOANE, VILA NOVA DE FAMALICA/JOANE - VILA NOVA DE FAMALICA/g;
	$line =~ s/IFERHOUNENE, WILAYA DE TIZI OU/IFERHOUNENE - WILAYA DE TIZI OU/g;
	$line =~ s/ARRUFINA, GUIMARAES/ARRUFINA - GUIMARAES/g;
	$line =~ s/HOA-DA, BINH-THUAN/HOA-DA - BINH-THUAN/g;
	$line =~ s/CERVERA DEL RIO ALHAMA, LA RIO/CERVERA DEL RIO ALHAMA - LA RIO/g;
	$line =~ s/SAO SEBASTIAO DE PEDREIRA, COM/SAO SEBASTIAO DE PEDREIRA - COM/g;
	$line =~ s/TAYA SELIB, OUED CHERF/TAYA SELIB - OUED CHERF/g;
	$line =~ s/KATINKA, VIROVITICA/KATINKA - VIROVITICA/g;
	$line =~ s/MOURE, POVOA DE LANHOSO/MOURE - POVOA DE LANHOSO/g;
	$line =~ s/OULED BELLAAGUID, SIDI BOUSTHM/OULED BELLAAGUID - SIDI BOUSTHM/g;
	$line =~ s/AKRAKER, RGHIOUA/AKRAKER - RGHIOUA/g;
	$line =~ s/SUTTON, COMTE DE KINGSTON/SUTTON - COMTE DE KINGSTON/g;
	$line =~ s/KSAR, KEBIR/KSAR - KEBIR/g;
	$line =~ s/GIOIA DEL COLLE, PROVINCE DE B/GIOIA DEL COLLE - PROVINCE DE B/g;
	$line =~ s/BRISBANE, QUEENSLAND/BRISBANE - QUEENSLAND/g;
	$line =~ s/MEIXEDO, VIANA DO CASTELO/MEIXEDO - VIANA DO CASTELO/g;
	$line =~ s/BAGHAGHA, ZIGUINCHOR/BAGHAGHA - ZIGUINCHOR/g;
	$line =~ s/AZGHAR, AIT YOUSSEF OU ALI/AZGHAR - AIT YOUSSEF OU ALI/g;
	$line =~ s/WOOLWICH, COMTE DE LONDRES/WOOLWICH - COMTE DE LONDRES/g;
	$line =~ s/ER RAHEL, ORAN/ER RAHEL - ORAN/g;
	$line =~ s/AHL OUST SKOURA, AIN CHOCK/AHL OUST SKOURA - AIN CHOCK/g;
	$line =~ s/ALCAINS, CASTELO BRANCO/ALCAINS - CASTELO BRANCO/g;
	$line =~ s/REBORDOES, SANTO TIRSO/REBORDOES - SANTO TIRSO/g;
	$line =~ s/CIUDAD RODRIGO, PROVINCE DE SA/CIUDAD RODRIGO - PROVINCE DE SA/g;
	$line =~ s/LAS VESGAS, PROVINCE DE BURGOS/LAS VESGAS - PROVINCE DE BURGOS/g;
	$line =~ s/SANTA CLARA LA LAGUNA, SOLOLA/SANTA CLARA LA LAGUNA - SOLOLA/g;
	$line =~ s/TISSA, OULED ALIANE/TISSA - OULED ALIANE/g;
	$line =~ s/SANTA LUCRECIA DE ALGERIZ, BRA/SANTA LUCRECIA DE ALGERIZ - BRA/g;
	$line =~ s/BENDADA, SABUGAL/BENDADA - SABUGAL/g;
	$line =~ s/CERDEIRA, SABUGAL/CERDEIRA - SABUGAL/g;
	$line =~ s/SAHUGO, PROVINCE DE SALAMANCA/SAHUGO - PROVINCE DE SALAMANCA/g;
	$line =~ s/SABINANIGO, PROVINCE DE HUESCA/SABINANIGO - PROVINCE DE HUESCA/g;
	$line =~ s/BANYOLES, PROVINCE DE GERONE/BANYOLES - PROVINCE DE GERONE/g;
	$line =~ s/MIMONGO, NGOUNIE/MIMONGO - NGOUNIE/g;
	$line =~ s/DOUAR ROUABAH, COMMUNE DE OULE/DOUAR ROUABAH - COMMUNE DE OULE/g;
	$line =~ s/GOGOSU, MEHEDINTI/GOGOSU - MEHEDINTI/g;
	$line =~ s/IRATO, CANTON D'ANTEKIROLA, DI/IRATO - CANTON D'ANTEKIROLA - DI/g;
	$line =~ s/TYLER, TEXAS/TYLER - TEXAS/g;
	$line =~ s/HAMMA BOUZIANE, CONSTANTINE/HAMMA BOUZIANE - CONSTANTINE/g;
	$line =~ s/SAO MIGUEL DA ACHA, IDANHA-A-N/SAO MIGUEL DA ACHA - IDANHA-A-N/g;
	$line =~ s/LENTELLA, PROVINCE DE CHIETI/LENTELLA - PROVINCE DE CHIETI/g;
	$line =~ s/FREGUESIA DE CASAIS, CONCELHO/FREGUESIA DE CASAIS - CONCELHO/g;
	$line =~ s/MELILLA, PROVINCE DE MALAGA/MELILLA - PROVINCE DE MALAGA/g;
	$line =~ s/GRAREM GOUGA, COMMUNE DE GAREM/GRAREM GOUGA - COMMUNE DE GAREM/g;
	$line =~ s/EL AKBIA, COMMUNE DE SIDI MARO/EL AKBIA - COMMUNE DE SIDI MARO/g;
	$line =~ s/VILLAVICIOSA, CORDOBA/VILLAVICIOSA - CORDOBA/g;
	$line =~ s/SERZEDELO, GUIMARAES/SERZEDELO - GUIMARAES/g;
	$line =~ s/ACORES, MADERE/ACORES - MADERE/g;
	$line =~ s/MONTMAGNY, QUEBEC/MONTMAGNY - QUEBEC/g;
	$line =~ s/GUYOTVILLE, ALGER/GUYOTVILLE - ALGER/g;
	$line =~ s/TEFESCHOUN, DEPARTEMENT D'ALGE/TEFESCHOUN - DEPARTEMENT D'ALGE/g;
	$line =~ s/DONG CU, THAI BINH/DONG CU - THAI BINH/g;
	$line =~ s/SAMEIRO, MONTEIGAS/SAMEIRO - MONTEIGAS/g;
	$line =~ s/BALAZAR, GUIMARAES/BALAZAR - GUIMARAES/g;
	$line =~ s/CARBONARA, PROVINCE DE BARI/CARBONARA - PROVINCE DE BARI/g;
	$line =~ s/BOVA, PROVINCE DE REGGIO CALAB/BOVA - PROVINCE DE REGGIO CALAB/g;
	$line =~ s/BEDEVLIA, TYACHIV/BEDEVLIA - TYACHIV/g;
	$line =~ s/SELENICE, VLORE/SELENICE - VLORE/g;
	$line =~ s/SAO PAIO DE MERELIM, BRAGA/SAO PAIO DE MERELIM - BRAGA/g;
	$line =~ s/SIMMELKAER, HERNING/SIMMELKAER - HERNING/g;
	$line =~ s/BENI CHIKER, NADOR/BENI CHIKER - NADOR/g;
	$line =~ s/ALDEIA DO SOUTO, COVILHA/ALDEIA DO SOUTO - COVILHA/g;
	$line =~ s/MARNIA, REGION TLECEM/MARNIA - REGION TLECEM/g;
	$line =~ s/ARCE, PROVINCE DE FROSINONE/ARCE - PROVINCE DE FROSINONE/g;
	$line =~ s/OLIVENSA, PROVINCE DE BADAJOZ/OLIVENSA - PROVINCE DE BADAJOZ/g;
	$line =~ s/CAP MATIFOU, ALGER/CAP MATIFOU - ALGER/g;
	$line =~ s/MOLFETTA, BARI/MOLFETTA - BARI/g;
	$line =~ s/CASTROVERDE, PROVINCE DE LUGO/CASTROVERDE - PROVINCE DE LUGO/g;
	$line =~ s/NEUNKIRCHEN, SARRE/NEUNKIRCHEN - SARRE/g;
	$line =~ s/AFGAN, KARAMAN/AFGAN - KARAMAN/g;
	$line =~ s/TOUVEDO, SAO LOURENCO PONTE DA/TOUVEDO - SAO LOURENCO PONTE DA/g;
	$line =~ s/SIDI OKBA, BISKRA/SIDI OKBA - BISKRA/g;
	$line =~ s/TRES PASSOS, ETAT DE RIO GRAND/TRES PASSOS - ETAT DE RIO GRAND/g;
	$line =~ s/COMMUNE DE DRAGOESTI, DEPARTEM/COMMUNE DE DRAGOESTI - DEPARTEM/g;
	$line =~ s/NAPA, ETAT DE CALIFORNIE/NAPA - ETAT DE CALIFORNIE/g;
	$line =~ s/BARRANQUILLA, DEPARTEMENT ATLA/BARRANQUILLA - DEPARTEMENT ATLA/g;
	$line =~ s/SABBAH, DISTRICT DE JEZZINE/SABBAH - DISTRICT DE JEZZINE/g;
	$line =~ s/AKBOU, BEJAIA/AKBOU - BEJAIA/g;
	$line =~ s/COUCIEIRO, VILA VERDE/COUCIEIRO - VILA VERDE/g;
	$line =~ s/MONTE SANT'ANGELO , FOGGIA/MONTE SANT'ANGELO - FOGGIA/g;
	$line =~ s/WENZHOU, PROVINCE DU ZHEJIANG/WENZHOU - PROVINCE DU ZHEJIANG/g;
	$line =~ s/AICHIE, JEZZINE/AICHIE - JEZZINE/g;
	$line =~ s/OUHAI, WENZHOU/OUHAI - WENZHOU/g;
	$line =~ s/TORRE, VIANA DO CASTELO/TORRE - VIANA DO CASTELO/g;
	$line =~ s/CARANGUEJEIRA, LEIRIA/CARANGUEJEIRA - LEIRIA/g;
	$line =~ s/ANLUNG ROMIET, KANDAL STOEUNG,\",/ANLUNG ROMIET - KANDAL STOEUNG,/g;
	$line =~ s/CHITATAMERA, VILA FLOR/CHITATAMERA - VILA FLOR/g;
	$line =~ s/NOUVELLE-ORLEANS, ETAT DE LOUI/NOUVELLE-ORLEANS - ETAT DE LOUI/g;
	$line =~ s/NYON, CANTON DE VAUD/NYON - CANTON DE VAUD/g;
	$line =~ s/RUIAN, PROVINCE DE ZHEJIANG/RUIAN - PROVINCE DE ZHEJIANG/g;
	$line =~ s/TINMKOUL, OUZIOUA/TINMKOUL - OUZIOUA/g;
	$line =~ s/GARFE, COMMUNE DE POVOA DE LAN/GARFE - COMMUNE DE POVOA DE LAN/g;
	$line =~ s/KONTELA, KAYE/KONTELA - KAYE/g;
	$line =~ s/TOMAR, SANTAREM/TOMAR - SANTAREM/g;
	$line =~ s/TIKHORETSK, KRAI DE KRASNODAR/TIKHORETSK - KRAI DE KRASNODAR/g;
	$line =~ s/NIMIS, PROVINCE D'UDINE/NIMIS - PROVINCE D'UDINE/g;
	$line =~ s/CORGO, CELORICO DE BASTO/CORGO - CELORICO DE BASTO/g;
	$line =~ s/COMAYAGUELA, REPUBLIQUE DE HON/COMAYAGUELA - REPUBLIQUE DE HON/g;
	$line =~ s/SHERBROOKE, PROVINCE DE QUEBEC/SHERBROOKE - PROVINCE DE QUEBEC/g;
	$line =~ s/KOBILJDO, COMMUNE DE ILIDZA/KOBILJDO - COMMUNE DE ILIDZA/g;
	$line =~ s/RICHE EN EAU, SUGAR ESTATE/RICHE EN EAU - SUGAR ESTATE/g;
	$line =~ s/BENI DRAR, OUJDA/BENI DRAR - OUJDA/g;
	$line =~ s/POLJE BIJELA,KONJIC/POLJE BIJELA - KONJIC/g;
	$line =~ s/IZEDA, BRAGANCA/IZEDA - BRAGANCA/g;
	$line =~ s/IZARAZENE, AZEFFOUN/IZARAZENE - AZEFFOUN/g;
	$line =~ s/MOTRIL, GRANADA/MOTRIL - GRANADA/g;
	$line =~ s/SKOPJE, MACEDOINE/SKOPJE - MACEDOINE/g;
	$line =~ s/YONGJIA, PROVINCE DE ZHEJIANG/YONGJIA - PROVINCE DE ZHEJIANG/g;
	$line =~ s/ABIDJAN, COMMUNE DE MARCORY/ABIDJAN - COMMUNE DE MARCORY/g;
	$line =~ s/SAO GIAO, OLIVEIRA DO HOSPITAL/SAO GIAO - OLIVEIRA DO HOSPITAL/g;
	$line =~ s/CAMPANHA, PORTO/CAMPANHA - PORTO/g;
	$line =~ s/BENI-MENIR, COMMUNE DE NEDROMA/BENI-MENIR - COMMUNE DE NEDROMA/g;
	$line =~ s/ROMAN, MEAMT/ROMAN - MEAMT/g;
	$line =~ s/AIN-FAKROUN, AIN-M'LILA/AIN-FAKROUN - AIN-M'LILA/g;
	$line =~ s/NKOMBASSI, NKELAFAMBA/NKOMBASSI - NKELAFAMBA/g;
	$line =~ s/CASAMASSIMA, PROVINCE DE BARI/CASAMASSIMA - PROVINCE DE BARI/g;
	$line =~ s/MEERUT, UTTAR PRADESH/MEERUT - UTTAR PRADESH/g;
	$line =~ s/MAGOUDOUBOUA, ISSIA/MAGOUDOUBOUA - ISSIA/g;
	$line =~ s/MOSSOUL, NINIVE/MOSSOUL - NINIVE/g;
	$line =~ s/SOCUELLAMOS, PROVINCE DE CIUDA/SOCUELLAMOS - PROVINCE DE CIUDA/g;
	$line =~ s/LIAOYANG, LIAONING/LIAOYANG - LIAONING/g;
	$line =~ s/PEDERNEIRA, FATIMA/PEDERNEIRA - FATIMA/g;
	$line =~ s/MILA, AIN-TINN/MILA - AIN-TINN/g;
	$line =~ s/COVOA DO LOBO, VAGOS/COVOA DO LOBO - VAGOS/g;
	$line =~ s/SAO BARNABE, ALMODOVAR/SAO BARNABE - ALMODOVAR/g;
	$line =~ s/VENTOSA, VIEIRA DO MINHO/VENTOSA - VIEIRA DO MINHO/g;
	$line =~ s/ALDEIA DO CARVALHO, COVILHA/ALDEIA DO CARVALHO - COVILHA/g;
	$line =~ s/VALE DE PRAZERES, FUNDAO/VALE DE PRAZERES - FUNDAO/g;
	$line =~ s/AGUAS, COMMUNE DE PENAMACOR/AGUAS - COMMUNE DE PENAMACOR/g;
	$line =~ s/VALE DE PRADOS, MACEDO CAVALEI/VALE DE PRADOS - MACEDO CAVALEI/g;
	$line =~ s/LERON-BUGUEY, PROVINCE DE CAGA/LERON-BUGUEY - PROVINCE DE CAGA/g;
	$line =~ s/FARAVOHITRA, TANANARIVE/FARAVOHITRA - TANANARIVE/g;
	$line =~ s/POTO-POTO, BRAZZAVILLE/POTO-POTO - BRAZZAVILLE/g;
	$line =~ s/OLALHAS, TOMAR/OLALHAS - TOMAR/g;
	$line =~ s/SAO CLEMENTE, LOULE/SAO CLEMENTE - LOULE/g;
	$line =~ s/BOVA MARINA, PROVINCE DE REGGI/BOVA MARINA - PROVINCE DE REGGI/g;
	$line =~ s/SAO BARTOLOMEU DOS GALEGOS, LO/SAO BARTOLOMEU DOS GALEGOS - LO/g;
	$line =~ s/SANTA SUSANA, ALCACER DO SAL/SANTA SUSANA - ALCACER DO SAL/g;
	$line =~ s/HAI CHAU, DA NANG/HAI CHAU - DA NANG/g;
	$line =~ s/IMERIMANDROSO, DISTRICT DE AMB/IMERIMANDROSO - DISTRICT DE AMB/g;
	$line =~ s/TLAT JBEL, BENI SIDEL/TLAT JBEL - BENI SIDEL/g;
	$line =~ s/GRANJA, MOURAO/GRANJA - MOURAO/g;
	$line =~ s/DOUAR SOUR JDID, AMIZMIZ/DOUAR SOUR JDID - AMIZMIZ/g;
	$line =~ s/ABACAS, VILA REAL/ABACAS - VILA REAL/g;
	$line =~ s/WINDSOR, ESSEX COMTE, TORONTO,\",/WINDSOR - ESSEX COMTE - TORONTO,/g;
	$line =~ s/HOHENEMS, VORALBERG/HOHENEMS - VORALBERG/g;
	$line =~ s/ZAHANA, DEPARTEMENT D'ORAN/ZAHANA - DEPARTEMENT D'ORAN/g;
	$line =~ s/JONESBORO, COMTE DE CRAIGHEAD,\",/JONESBORO - COMTE DE CRAIGHEAD,/g;
	$line =~ s/SEATTLE, COMTE DE KING, ETAT D/SEATTLE - COMTE DE KING - ETAT D/g;
	$line =~ s/NEDROMA, ORAN/NEDROMA - ORAN/g;
	$line =~ s/GIOIA TAURO, PROVINCE DE REGGI/GIOIA TAURO - PROVINCE DE REGGI/g;
	$line =~ s/TOMELLOSO, PROVINCE DE CIUDAD/TOMELLOSO - PROVINCE DE CIUDAD/g;
	$line =~ s/SANTA COLOMA DE GRAMANET, BARC/SANTA COLOMA DE GRAMANET - BARC/g;
	$line =~ s/BONNIER, ORAN/BONNIER - ORAN/g;
	$line =~ s/VEGA DE PAS, PROVINCE DE SANTA/VEGA DE PAS - PROVINCE DE SANTA/g;
	$line =~ s/ENSHEIM, JETZT\. SAARBRUCKEN/ENSHEIM - JETZT\. SAARBRUCKEN/g;
	$line =~ s/EL GHRAR, JEMMAPES/EL GHRAR - JEMMAPES/g;
	$line =~ s/BOUFFIOULX, HAINAUT/BOUFFIOULX - HAINAUT/g;
	$line =~ s/ROVOLON, PROVINCE DE PADOVA/ROVOLON - PROVINCE DE PADOVA/g;
	$line =~ s/NADOR,ORAN/NADOR - ORAN/g;
	$line =~ s/SOZA, VAGOS/SOZA - VAGOS/g;
	$line =~ s/GRAREM GOUGA,WILAYA DE MILA/GRAREM GOUGA - WILAYA DE MILA/g;
	$line =~ s/LAMA, BARCELOS/LAMA - BARCELOS/g;
	$line =~ s/CORATO, PROVINCE DE BARI/CORATO - PROVINCE DE BARI/g;
	$line =~ s/A VER-O-MAR, POVOA DE VARZIM/A VER-O-MAR - POVOA DE VARZIM/g;
	$line =~ s/SERINA, PROVINCE DE BERGAMO/SERINA - PROVINCE DE BERGAMO/g;
	$line =~ s/HORNIJA, PROVINCE DE LEON/HORNIJA - PROVINCE DE LEON/g;
	$line =~ s/PALESTREO, DEPARTEMENT TIZI-OU/PALESTREO - DEPARTEMENT TIZI-OU/g;
	$line =~ s/DOUAR MENGUELLET, AIN EL HERMA/DOUAR MENGUELLET - AIN EL HERMA/g;
	$line =~ s/PENZA, DISTRICT DE PENZA/PENZA - DISTRICT DE PENZA/g;
	$line =~ s/OUREM, VILA NOVA DE OUREM/OUREM - VILA NOVA DE OUREM/g;
	$line =~ s/SAO GENS, FAFE/SAO GENS - FAFE/g;
	$line =~ s/LECA DA PALMEIRA, MATOSINHOS/LECA DA PALMEIRA - MATOSINHOS/g;
	$line =~ s/MERUFE, MONCAO/MERUFE - MONCAO/g;
	$line =~ s/VALE DE BOURO, CELORICO DE BAS/VALE DE BOURO - CELORICO DE BAS/g;
	$line =~ s/LONGROIVA, MEDA/LONGROIVA - MEDA/g;
	$line =~ s/SAINTE MARIE DU ZIT, CIRCONSCR/SAINTE MARIE DU ZIT - CIRCONSCR/g;
	$line =~ s/FORJAES, ESPOSENDE/FORJAES - ESPOSENDE/g;
	$line =~ s/IMMOULA, AKBOU/IMMOULA - AKBOU/g;
	$line =~ s/LAMON, PROVINCE DE BELLUNO/LAMON - PROVINCE DE BELLUNO/g;
	$line =~ s/MIRAGAIA, PORTO/MIRAGAIA - PORTO/g;
	$line =~ s/DEHESAS-PONFERRADA, PROVINCE D/DEHESAS-PONFERRADA - PROVINCE D/g;
	$line =~ s/VACOAS, PLAINES WILHEMS/VACOAS - PLAINES WILHEMS/g;
	$line =~ s/LEWES, COMTE DE SUSSEX/LEWES - COMTE DE SUSSEX/g;
	$line =~ s/IAANOUBA, TAJAJT/IAANOUBA - TAJAJT/g;
	$line =~ s/SOBREIRA FARMOSA, PROENCA-A-NO/SOBREIRA FARMOSA - PROENCA-A-NO/g;
	$line =~ s/JOVIM, GONDOMAR/JOVIM - GONDOMAR/g;
	$line =~ s/TIZI NDOUBDI, AIT AHMED, ANZI,\",/TIZI NDOUBDI - AIT AHMED - ANZI,/g;
	$line =~ s/PHUOC-QUA, HUONG THUY, THUA TH/PHUOC-QUA - HUONG THUY - THUA TH/g;
	$line =~ s/IKEDJANE, AKFADOU/IKEDJANE - AKFADOU/g;
	$line =~ s/SAO PEDRO, MANTEIGAS/SAO PEDRO - MANTEIGAS/g;
	$line =~ s/EL BARRACO, PROVINCE DE AVILA/EL BARRACO - PROVINCE DE AVILA/g;
	$line =~ s/NEW BEDFORT, ETAT DU MASSACHUS/NEW BEDFORT - ETAT DU MASSACHUS/g;
	$line =~ s/SOUTELO, CHAVES/SOUTELO - CHAVES/g;
	$line =~ s/APOUTAGBO, GRAND-POPO/APOUTAGBO - GRAND-POPO/g;
	$line =~ s/FOLHADA, MARCO DE CANAVEZES/FOLHADA - MARCO DE CANAVEZES/g;
	$line =~ s/HOMBO, ANJOUAN/HOMBO - ANJOUAN/g;
	$line =~ s/DOMONI, ANJOUAN/DOMONI - ANJOUAN/g;
	$line =~ s/SIMA, ANJOUAN/SIMA - ANJOUAN/g;
	$line =~ s/TACHETA, LES BRAZ/TACHETA - LES BRAZ/g;
	$line =~ s/BROKOPONDO, DISTRICT SIPALIWIN/BROKOPONDO - DISTRICT SIPALIWIN/g;
	$line =~ s/ALBINA, DISTRICT MAROWIJNE/ALBINA - DISTRICT MAROWIJNE/g;
	$line =~ s/NORMAND, PALINE/NORMAND - PALINE/g;
	$line =~ s/SAIGON, CHOLON/SAIGON - CHOLON/g;
	$line =~ s/GUIDO, SOUS PREFECTURE DE DUEK/GUIDO - SOUS PREFECTURE DE DUEK/g;
	$line =~ s/TATTOTTOUCALAVATCHERY, OULGARE/TATTOTTOUCALAVATCHERY - OULGARE/g;
	$line =~ s/CASAL DE CINZA, GUARDA/CASAL DE CINZA - GUARDA/g;
	$line =~ s/MELOULECH, DJEBINIANA/MELOULECH - DJEBINIANA/g;
	$line =~ s/BORE, PROVINCE DE PARME/BORE - PROVINCE DE PARME/g;
	$line =~ s/NOWE MIASTO, VOIVODIE DE CIECH/NOWE MIASTO - VOIVODIE DE CIECH/g;
	$line =~ s/CHA, MONTALEGRE/CHA - MONTALEGRE/g;
	$line =~ s/KERSIGNAGNE, YELIMANE/KERSIGNAGNE - YELIMANE/g;
	$line =~ s/ZAKHO, DEHOK/ZAKHO - DEHOK/g;
	$line =~ s/IROUFLENE, COMMUNE SIDI AICH \(/IROUFLENE - COMMUNE SIDI AICH/g;
	$line =~ s/LAC-DES-SEIZE-ILES, QUEBEC/LAC-DES-SEIZE-ILES - QUEBEC/g;
	$line =~ s/PIRADA, GABU/PIRADA - GABU/g;
	$line =~ s/TADMAIT, TIZI-OUZOU/TADMAIT - TIZI-OUZOU/g;
	$line =~ s/HAD GHARBIA, TANGER, ASSILAH/HAD GHARBIA - TANGER - ASSILAH/g;
	$line =~ s/DOUAR ZAOUIT SIDI OUAGGAG, AGL/DOUAR ZAOUIT SIDI OUAGGAG - AGL/g;
	$line =~ s/TAFRAOUTE, TIZNIT/TAFRAOUTE - TIZNIT/g;
	$line =~ s/AIT LAHCENE, BENI YENNI/AIT LAHCENE - BENI YENNI/g;
	$line =~ s/OUTEIRO, CABECEIRAS DE BASTO/OUTEIRO - CABECEIRAS DE BASTO/g;
	$line =~ s/MAAMAR, COMMUNE DE DRAA-EL-MIZ/MAAMAR - COMMUNE DE DRAA-EL-MIZ/g;
	$line =~ s/VIANA DO CASTELO, PAROISSE DE/VIANA DO CASTELO - PAROISSE DE/g;
	$line =~ s/VILLAGE D'OPICHNIA, DISTRICT Z/VILLAGE D'OPICHNIA - DISTRICT Z/g;
	$line =~ s/BARCOUCO, COMMUNUE DE MEALHADA/BARCOUCO - COMMUNUE DE MEALHADA/g;
	$line =~ s/FATIMA, VILA NOVA DE OUREM/FATIMA - VILA NOVA DE OUREM/g;
	$line =~ s/POUTKAK NGAMBE, SANAGA-MARITIM/POUTKAK NGAMBE - SANAGA-MARITIM/g;
	$line =~ s/AIN TAGHROUT, SETIF/AIN TAGHROUT - SETIF/g;
	$line =~ s/KOLARI, REPUBLIQUE DE SERBIE/KOLARI - REPUBLIQUE DE SERBIE/g;
	$line =~ s/VILLAGE DE BREZOAIA, DISTRICT/VILLAGE DE BREZOAIA - DISTRICT/g;
	$line =~ s/RUFISQUE, REGION DE DAKAR/RUFISQUE - REGION DE DAKAR/g;
	$line =~ s/LANHESES, VIANA DO CASTELO/LANHESES - VIANA DO CASTELO/g;
	$line =~ s/SAO JOAO BATISTA, COMMUNE DE P/SAO JOAO BATISTA - COMMUNE DE P/g;
	$line =~ s/ALBERGARIA DOS DOZE, POMBAL/ALBERGARIA DOS DOZE - POMBAL/g;
	$line =~ s/CERTEZE, SATU MARE/CERTEZE - SATU MARE/g;
	$line =~ s/ANJANAHARY, TANANARIVE/ANJANAHARY - TANANARIVE/g;
	$line =~ s/BORDUL, COMILLA/BORDUL - COMILLA/g;
	$line =~ s/DJEBALA, NEDROMA/DJEBALA - NEDROMA/g;
	$line =~ s/VALFOR, COMMUNE DE MEDA/VALFOR - COMMUNE DE MEDA/g;
	$line =~ s/AZZEFOUN, TIZI OUZOU/AZZEFOUN - TIZI OUZOU/g;
	$line =~ s/AIT AMIRA CHTOUKA AIT BAHA, AG/AIT AMIRA CHTOUKA AIT BAHA - AG/g;
	$line =~ s/GUAGUA, PAMPANGA/GUAGUA - PAMPANGA/g;
	$line =~ s/MESNIL PHOENIX, PLAINES WILHEM/MESNIL PHOENIX - PLAINES WILHEM/g;
	$line =~ s/COUSSO, MELGACO/COUSSO - MELGACO/g;
	$line =~ s/WENZHOU, PROVINCE DE ZHEJIANG/WENZHOU - PROVINCE DE ZHEJIANG/g;
	$line =~ s/KENDIRA, BEJAIA/KENDIRA - BEJAIA/g;
	$line =~ s/AGUAS BELAS, SABUGAL/AGUAS BELAS - SABUGAL/g;
	$line =~ s/OLIVEIRA SAO MATEUS, VILA NOVA/OLIVEIRA SAO MATEUS - VILA NOVA/g;
	$line =~ s/PEGA, GUARDA/PEGA - GUARDA/g;
	$line =~ s/TREICHVILLE, ABIDJAN/TREICHVILLE - ABIDJAN/g;
	$line =~ s/EL OULDJA, COLLO/EL OULDJA - COLLO/g;
	$line =~ s/VAQUEIROS, SANTAREM/VAQUEIROS - SANTAREM/g;
	$line =~ s/ESPIEL, CORDOBA/ESPIEL - CORDOBA/g;
	$line =~ s/VILA OROVA, NOSSA SENHORA DA G/VILA OROVA - NOSSA SENHORA DA G/g;
	$line =~ s/FATOLA, KAYES/FATOLA - KAYES/g;
	$line =~ s/AIT-ABDELKRIM, OUADHIA/AIT-ABDELKRIM - OUADHIA/g;
	$line =~ s/CARNIDE, POMBAL/CARNIDE - POMBAL/g;
	$line =~ s/ALCOCHETE, SETUBAL/ALCOCHETE - SETUBAL/g;
	$line =~ s/MACAS DE DONA MARIA, ALVAIAZER/MACAS DE DONA MARIA - ALVAIAZER/g;
	$line =~ s/GUIMARAES, SAO PAIO/GUIMARAES - SAO PAIO/g;
	$line =~ s/TSARALALANA, TANANARIVE/TSARALALANA - TANANARIVE/g;
	$line =~ s/QIXIA, PROVINCE DU SHANDONG/QIXIA - PROVINCE DU SHANDONG/g;
	$line =~ s/TALIN, REGION D'ARAGATSOTN/TALIN - REGION D'ARAGATSOTN/g;
	$line =~ s/SAN PEDRO SOLOMA, HUEHUETENANG/SAN PEDRO SOLOMA - HUEHUETENANG/g;
	$line =~ s/VILLALGORDO DEL CABRIEL, VALEN/VILLALGORDO DEL CABRIEL - VALEN/g;
	$line =~ s/DOUAR TIZNIT OUAMAZER, IDA-GOU/DOUAR TIZNIT OUAMAZER - IDA-GOU/g;
	$line =~ s/KALUTARA, KALUTARA, PROVINCE D/KALUTARA - KALUTARA, PROVINCE D/g;
	$line =~ s/NAMDINH-VILLE, PROVINCE DE NAM/NAMDINH-VILLE - PROVINCE DE NAM/g;
	$line =~ s/KALUTARA - KALUTARA, PROVINCE D/KALUTARA - KALUTARA - PROVINCE D/g;
	$line =~ s/GAME, ZIO/GAME - ZIO/g;
	$line =~ s/AIN CHOCK, CASABLANCA/AIN CHOCK - CASABLANCA/g;
	$line =~ s/COTONOU, DAHOMEY/COTONOU - DAHOMEY/g;
	$line =~ s/GOURDIOUMA, KAEDI/GOURDIOUMA - KAEDI/g;
	$line =~ s/GUARICO, MORAN, ETAT DE LARA/GUARICO - MORAN - ETAT DE LARA/g;
	$line =~ s/ARHBAL, OUJDA/ARHBAL - OUJDA/g;
	$line =~ s/EL HAOUANET, NEDROMA/EL HAOUANET - NEDROMA/g;
	$line =~ s/TAOURIRT, MOUSSA/TAOURIRT - MOUSSA/g;
	$line =~ s/ARROIOS, VILA REAL/ARROIOS - VILA REAL/g;
	$line =~ s/SIDI BEL ABBES, ORAN/SIDI BEL ABBES - ORAN/g;
	$line =~ s/BAB OUENDER, RGHIOUA, PROVINCE/BAB OUENDER - RGHIOUA - PROVINCE/g;
	$line =~ s/NDAABED; AIT/NDAABED - AIT/g;
	$line =~ s/TOFAHA, MEDJEZ EL BAD/TOFAHA - MEDJEZ EL BAD/g;
	$line =~ s/MEDEA, ALGER/MEDEA - ALGER/g;
	$line =~ s/ROUCAS, MELGACO/ROUCAS - MELGACO/g;
	$line =~ s/LOREGGIA, PROVINCE DE PADOUE/LOREGGIA - PROVINCE DE PADOUE/g;
	$line =~ s/CORRELHA, PONTE DE LIMA/CORRELHA - PONTE DE LIMA/g;
	$line =~ s/CHIKER; NADOR/CHIKER - NADOR/g;
	$line =~ s/PALMEIRA ; MATOSINHOS/PALMEIRA, MATOSINHOS/g;
	$line =~ s/FAFE ; FAFE/FAFE - FAFE/g;
	$line =~ s/TUNIS, FOCHVILLE/TUNIS - FOCHVILLE/g;
	$line =~ s/BENESPERA ; GUARDA/BENESPERA - GUARDA/g;
	$line =~ s/VILARINHO; SANTO/VILARINHO - SANTO/g;
	$line =~ s/NICASTRO, LAMEZIA TERME/NICASTRO - LAMEZIA TERME/g;
	$line =~ s/DEUX PONTS ; PALATINAT/DEUX PONTS - PALATINAT/g;
	$line =~ s/CARGEDOLO; PROVINCE/CARGEDOLO - PROVINCE/g;
	$line =~ s/BOUAIR ; COMMU/BOUAIR - COMMU/g;
	$line =~ s/AMEUR SEFLIA, KENITRA/AMEUR SEFLIA - KENITRA/g;
	$line =~ s/KHLALFA, ZRIRER/KHLALFA - ZRIRER/g;
	$line =~ s/SANDIM; VILA/SANDIM, VILA/g;
	$line =~ s/BARROSO; BOTICAS/BARROSO - BOTICAS/g;
	$line =~ s/MAKATI, RIZAL/MAKATI - RIZAL/g;
	$line =~ s/CASTANHEIRA; CARR/CASTANHEIRA - CARR/g;
	$line =~ s/IKDMANE ; AIT/IKDMANE - AIT/g;
	$line =~ s/PORTILLO ; VALLADOLID/PORTILLO - VALLADOLID/g;
	$line =~ s/FRIO ; ARCOS/FRIO - ARCOS/g;
	$line =~ s/GOUVINHAS; ARRONDISSEM/GOUVINHAS - ARRONDISSEM/g;
	$line =~ s/IDAES; FELGUEIRAS/IDAES - FELGUEIRAS/g;
	$line =~ s/ALTURAS ; BOTICAS/ALTURAS - BOTICAS/g;
	$line =~ s/RHUMEL ; CONSTAN/RHUMEL - CONSTAN/g;
	$line =~ s/CARREIRAS; PORTALEGRE/CARREIRAS - PORTALEGRE/g;
	$line =~ s/LA CHAUX-DE-FONDS ; LE LOCLE/LA CHAUX-DE-FONDS - LE LOCLE/g;
	$line =~ s/ODIVELAS; LOURES/ODIVELAS - LOURES/g;
	$line =~ s/CAMPO; CASTELO/CAMPO - CASTELO/g;
	$line =~ s/KOJCINOVAC, BOSNIE-HERZEGOVINE/KOJCINOVAC - BOSNIE-HERZEGOVINE/g;
	$line =~ s/BARACAL; CELORICO/BARACAL - CELORICO/g;
	$line =~ s/UENDER RERHIOUA ; TAOUNA/UENDER RERHIOUA - TAOUNA/g;
	$line =~ s/HENCHIR KHANGA; TABARKA/HENCHIR KHANGA - TABARKA/g;
	$line =~ s/SAMANO; PROVINCE DE SANTANDER/SAMANO - PROVINCE DE SANTANDER/g;
	$line =~ s/SASSA ; COMMUNE/SASSA - COMMUNE/g;
	$line =~ s/BREMBILLA ; PROVINCE/BREMBILLA - PROVINCE/g;
	return $line;
}

sub quarter_from_date {
	my ($date) = @_;
	my ($y, $m, $d) = split '-', $date;
	die unless $y && $m && $d;
	my $quarter;
	if ($m >= 1 && $m <= 3) {
		$quarter = 1;
	} elsif ($m >= 4 && $m <= 6) {
		$quarter = 2;
	} elsif ($m >= 7 && $m <= 9) {
		$quarter = 3;
	} elsif ($m >= 10 && $m <= 12) {
		$quarter = 4;
	} else {
		die "date : [$date]";
	}
	return ($y, $m, $d, $quarter);
}