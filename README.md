# Pipeline for analysing audio files for bird identification using BirdNET

In partnership with the <a href="http://ngarrindjeri.com.au/">Ngarrindjeri Aboriginal Corporation</a> and the Raukkan Rangers, Flinders University (<a href="http://globalecologyflinders.com/">Global Ecology Laboratory</a>) under the auspices of the Australian Research Council <a href="http://ciehf.au">Centre of Excellence for Indigenous and Environmental Histories and Futures</a> (CIEHF) have set up an initial array of 5 passive acoustic recorders to document the change in bird diversity in recently restored wetlands within the <a href="https://www.environment.sa.gov.au/topics/water-and-river-murray/projects-plans-security-and-legislation/water-projects/coorong/current-projects/on-ground-works/ogw-teringie">Teringie Wetlands</a> complex in South Australia. We are comparing these records to existing wetland complexes and control saltponds devoid of most birdlife (control). These data belong to the Ngarrindjeri Nation.

The audio-file repository is available at <a href="https://api.ecosounds.org/projects/1281">EcoSounds</a> (but not publicly available).

## Workflow

R-based BirdNET workflow for:

1. processing a single audio file
2. processing a large `.tar.zst` archive one `.flac` at a time
3. converting `.flac` to `.wav`
4. filtering BirdNET predictions with a repository-local species list
5. writing per-file prediction summaries and rolling progress reports

## Repository layout

```text
birdnetRpredict/
├── README.md
├── data/
│   └── species_lists/
│       ├── reference/
│       └── regional/
│           └── lower_murray/
└── scripts/
    ├── birdnet_helpers.R
    ├── birdnetID.R
    └── process_tar_archive.R
```

## What the pipeline does

### Single-file workflow

`scripts/birdnetID.R`:

1. loads the shared helper functions
2. points to a single `.wav` file
3. loads a species CSV from this repository
4. builds BirdNET models
5. extracts date/time and coordinates from the file name where available
6. uses the BirdNET range model to narrow candidate species for that file
7. runs BirdNET on the audio
8. writes:
   - a filtered prediction CSV
   - a cleaned species summary CSV

### Archive workflow

`scripts/process_tar_archive.R`:

1. opens a source `.tar.zst`
2. lists `.flac` members inside the archive
3. processes files one at a time
4. extracts a single `.flac` while preserving the internal archive path
5. converts that `.flac` to `.wav` with `ffmpeg`
6. runs the same BirdNET summary workflow used by the single-file script
7. writes per-file CSV outputs
8. deletes the temporary extracted `.flac` and `.wav`
9. moves to the next archive member until the archive is complete

This avoids unpacking the entire archive at once.

## How coordinates and date are determined

The pipeline first tries to parse metadata from the audio file name.

For a name like:

```text
20251123T080000+0930_REC_-31.52235+152.10576.flac
```

the scripts derive:

- date/time: `2025-11-23 08:00:00 +0930`
- latitude: `-35.52235`
- longitude: `139.10576`

The archive pipeline extracts `.flac`, converts it to `.wav`, and keeps the same basename, so the parsed coordinates and timestamp continue to apply during BirdNET processing.

If a file name cannot be parsed, the scripts can fall back to user-defined default latitude, longitude, and date values.

## Species lists

Species list files are stored in this repository under:

- `data/species_lists/reference/`
- `data/species_lists/regional/lower_murray/`

The current scripts use:

```text
data/species_lists/regional/lower_murray/BirdNet_SA_LowerMurray_Tolderol_matches.csv
```

That file is combined with the BirdNET location/week range model to reduce false positives before prediction summaries are written.

## Requirements

You need:

- R
- the `birdnetR` package
- `ffmpeg`
- `tar` with `--zstd` support
- `zstd`

The current environment also expects BirdNET's Python dependencies to be installable through `birdnetR`.

## User-defined settings

The main user-editable settings are at the top of the scripts.

### `scripts/birdnetID.R`

Edit:

- `audio_file`
- `species_csv`
- `fallback_latitude`
- `fallback_longitude`
- `prediction_min_confidence`
- `summary_confidence_threshold`

### `scripts/process_tar_archive.R`

Edit:

- `archive_file`
- `species_csv`
- `fallback_latitude`
- `fallback_longitude`
- `prediction_min_confidence`
- `summary_confidence_threshold`

These control which archive is processed, which species filter is used, and how strict the prediction summaries are.

## How to run

### Single file

```bash
Rscript scripts/birdnetID.R
```

### Archive pipeline

```bash
Rscript scripts/process_tar_archive.R
```

## Outputs

### Per audio file

For each processed recording, the pipeline writes:

- `*_birdnet_predictions.csv`
- `*_birdnet_species_summary.csv`

The summary CSV contains:

- `date_time`
- `scientific_name`
- `common_name`
- `confidence`
- `cumulative_number_of_new_species_detected`
- `total_number_of_species_identified`

### Archive-level outputs

For archive runs, output is written under:

```text
out/<archive_name>_birdnet_output/
```

The archive workflow writes:

- `*_processing_manifest.csv`  
  machine-readable log of file-by-file outcomes

- `*_file_results.txt`  
  continually updated text summary by file, including status, timing, coordinates, outputs, and any errors

- `*_summary_of_summaries.txt`  
  continually updated overall run summary, including progress, current file, current phase, elapsed time, ETA, and cumulative species count

All archive outputs are written to the local repository drive, not back to the source archive drive.

## Console progress during archive runs

When `Rscript scripts/process_tar_archive.R` is running, the console reports:

- current file index and percent complete
- current archive member being processed
- extraction step
- `.flac` to `.wav` conversion step
- BirdNET range-filter step
- BirdNET prediction step
- output-writing step
- cleanup step
- per-file elapsed time
- estimated time remaining

## Resume behavior

The archive script is restart-friendly.

If a file's summary CSV already exists, that file is skipped and logged as:

```text
skipped_existing
```

This allows rerunning the script after interruption without reprocessing every file.

## Contingencies and failure behaviour

### 1. No coordinates in file name

The script uses fallback latitude/longitude if configured. If neither filename coordinates nor fallback coordinates are available, processing stops for that file.

### 2. No parseable date/time in file name

The script uses the fallback date if supplied. If not, processing stops for that file.

### 3. Empty species filter

If the repository species CSV and BirdNET range-model output do not overlap, processing stops for that file with an explicit error.

### 4. No usable detections

If BirdNET returns no usable detections after cleaning, the script writes empty summary outputs and records the file as:

- `no_usable_detections`

### 5. No detections pass the summary threshold

If BirdNET runs successfully but no predictions meet `summary_confidence_threshold`, the script writes empty summary outputs and records the file as:

- `no_summary_detections`

### 6. Conversion failure

If `ffmpeg` fails to convert a `.flac`, the file is recorded as:

- `error`

and processing continues to the next archive member.

### 7. Archive extraction failure

If `tar --zstd` fails for a specific member, that file is recorded as:

- `error`

and processing continues.

### 8. Interrupted runs

If the process is interrupted, rerun:

```bash
Rscript scripts/process_tar_archive.R
```

Files with existing summary outputs skipped automatically.

## Notes

- temporary extracted `.flac` and converted `.wav` files deleted after each file is processed
- helper functions live in `scripts/birdnet_helpers.R`
- archive processor mirrors the archive subdirectory structure in the output folder when writing per-file CSV results
