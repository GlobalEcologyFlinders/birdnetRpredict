# Pipeline for analysing audio files for bird identification using <a href="https://birdnet.cornell.edu">BirdNET</a>

In partnership with the <a href="http://ngarrindjeri.com.au/">Ngarrindjeri Aboriginal Corporation</a> and the Raukkan Rangers, Flinders University (<a href="http://globalecologyflinders.com/">Global Ecology Laboratory</a>) under the auspices of the Australian Research Council <a href="http://ciehf.au">Centre of Excellence for Indigenous and Environmental Histories and Futures</a> (CIEHF) have set up an initial array of 5 passive acoustic recorders to document the change in bird diversity in recently restored wetlands within the <a href="https://www.environment.sa.gov.au/topics/water-and-river-murray/projects-plans-security-and-legislation/water-projects/coorong/current-projects/on-ground-works/ogw-teringie">Teringie Wetlands</a> complex in South Australia. We are comparing these records to existing wetland complexes and control saltponds devoid of most birdlife (control). These data belong to the Ngarrindjeri Nation.

The audio-file repository is available at <a href="https://api.ecosounds.org/projects/1281">EcoSounds</a> (but not publicly available).

## Workflow

R-based <a href="https://birdnet.cornell.edu">BirdNET</a> workflow for:

1. processing a single audio file
2. processing a large `.tar.zst` archive one `.flac` at a time
3. converting `.flac` to `.wav`
4. filtering BirdNET predictions with a repository-local species list
5. writing per-file prediction summaries and rolling progress reports
6. post-processing existing summary CSVs into plots and aggregate tables

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
    ├── analyse_birdnet_output.R
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
2. streams the archive sequentially instead of doing a full pre-scan
3. starts processing as soon as the next `.flac` is encountered
4. extracts a single `.flac` while preserving the internal archive path
5. converts that `.flac` to `.wav` with `ffmpeg`
6. runs the same BirdNET summary workflow used by the single-file script
7. writes per-file CSV outputs
8. deletes the temporary extracted `.flac` and `.wav`
9. moves to the next archive member until the archive is complete

This avoids unpacking the entire archive at once and avoids waiting for a full member enumeration before processing starts.
macOS sidecar entries such as `._*.flac` and `__MACOSX/` metadata are skipped during archive streaming.

### Post-processing analysis workflow

`scripts/analyse_birdnet_output.R`:

1. searches recursively under `out/` for existing `*_birdnet_species_summary.csv` files
2. combines the summary CSVs that are already present and readable
3. filters detections by a user-defined minimum confidence threshold
4. bins detections into a user-defined time step (default `60` minutes)
5. writes aggregate CSV tables plus plots for:
    - identifications over time
    - cumulative new species over time
    - identifications per species
    - temporal autocorrelation, partial autocorrelation, spectral periodicity, and Ljung-Box periodicity tests for both detections-per-bin and unique-species-per-bin

This script is intended to work while archive processing is still incomplete. You can rerun it at any time and it will analyse whatever summary CSVs currently exist in `out/`.

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

- R packages: <code>birdnetR</code>, <code>processx</code>, <code>callr</code>
- casks: <code>ffmpeg</code>, <code>tar</code> with <code>--zstd</code> support, <code>zstd</code>

The current environment also expects BirdNET's Python dependencies to be installable through <a href="https://github.com/birdnet-team/birdnetR"><code>birdnetR</code></a>.

## User-defined settings

Main user-editable settings are at the top of the scripts.

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
- `stage_heartbeat_seconds`
- `stage_timeout_seconds`

These control which archive is processed, which species filter is used, and how strict the prediction summaries are.

### `scripts/analyse_birdnet_output.R`

Edit the user-defined settings directly near the top of the script:

- `summary_root`
- `output_root`
- `analysis_timezone`
- `bin_minutes`
- `top_species_time_bin_minutes`
- `min_confidence`
- `periodicity_max_lag_bins`
- `show_plots_in_session`

These control which existing summary CSVs are included, where the analysis outputs are written, the temporal bin size used by the plots, and the minimum confidence required for a detection to be counted.

## How to run

### Single file

```bash
Rscript scripts/birdnetID.R
```

### Archive pipeline

```bash
Rscript scripts/process_tar_archive.R
```

### Post-processing analysis

Open `scripts/analyse_birdnet_output.R` in RStudio, VS Code, or another R editor, adjust the settings block if needed, then run the script inside R.

The script is intended to be run as a standalone analysis file rather than driven by command-line arguments.

It uses `ggplot2` for all figures.
By default, the figures are shown in the active R graphics session and also saved as `.png` files.

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

### Analysis outputs

Post-processing outputs are written under:

```text
out/analysis/confidence_<threshold>_bin_<minutes>min/
```

The analysis workflow writes:

- `birdnet_analysis_summary.txt`  
  text summary of the current analysis run, including skipped or incomplete input files

- `birdnet_analysis_input_files.csv`  
  one row per discovered summary CSV, showing whether it was loaded successfully

- `birdnet_analysis_filtered_detections.csv`  
  all retained detections after applying the chosen minimum confidence threshold

- `birdnet_identifications_by_time_bin.csv`  
  identifications per time bin, plus unique species count per bin

- `birdnet_identifications_by_time_bin_by_recorder.csv`  
  identifications per time bin for each recorder separately

- `birdnet_top_10_species_detections_through_time.csv`  
  detections through time for the 10 most detected species across all currently analysed recorders

- `birdnet_top_10_species_detections_through_time_by_recorder.csv`  
  detections through time for the 10 most detected species within each recorder

- `birdnet_cumulative_new_species_by_time_bin.csv`  
  newly detected species per time bin and cumulative species richness through time

- `birdnet_cumulative_new_species_by_time_bin_by_recorder.csv`  
  cumulative new-species summaries calculated separately for each recorder

- `birdnet_identifications_by_species.csv`  
  species ranked from most frequently identified to least frequently identified

- `birdnet_identifications_by_species_by_recorder.csv`  
  species-frequency summaries calculated separately for each recorder

- `birdnet_identifications_by_species_by_month.csv`  
  species ranked by identification frequency within each month of the year

- `birdnet_identifications_by_species_by_month_by_recorder.csv`  
  month-by-species summaries calculated separately for each recorder

- `birdnet_monthly_diversity_metrics.csv`  
  monthly recorder-level diversity metrics calculated from detections-as-abundance, including Shannon index, Simpson index, and Hill numbers for <em>q</em> = 1 and <em>q</em> = 2

- `birdnet_monthly_diversity_metrics_overall.csv`  
  monthly diversity metrics after combining detections across all currently analysed recorders

- `birdnet_identification_acf.csv`  
  detection-count autocorrelation values by temporal lag, retained for backward compatibility

- `birdnet_identification_spectrum.csv`  
  detection-count spectral-density summary for inspecting periodicity, retained for backward compatibility

- `birdnet_identification_periodicity_by_recorder.csv`  
  recorder-specific detection-count temporal diagnostics used by the recorder comparison figure

- `birdnet_temporal_diagnostics.csv`  
  combined temporal diagnostics for both detections per bin and unique species identified per bin, including autocorrelation (ACF), partial autocorrelation (PACF), and spectral-density curves

- `birdnet_temporal_periodicity_tests.csv`  
  Ljung-Box test summaries at key lags for both detections per bin and unique species identified per bin

- `birdnet_temporal_spectral_peaks.csv`  
  ranked dominant spectral peaks, including the strongest candidate periods and their relative power

- `birdnet_temporal_diagnostics_by_recorder.csv`  
  recorder-specific ACF, PACF, and spectral-density values for both detections per bin and unique species identified per bin

- `birdnet_temporal_periodicity_tests_by_recorder.csv`  
  recorder-specific Ljung-Box test summaries at key lags

- `birdnet_temporal_spectral_peaks_by_recorder.csv`  
  recorder-specific dominant spectral peaks for easier interpretation of likely recurring periods

- `birdnet_identifications_over_time.png`
- `birdnet_top_10_species_detections_through_time.png`
- `birdnet_cumulative_new_species.png`
- `birdnet_identifications_by_species.png`
- `birdnet_identifications_by_species_by_month.png`
- `birdnet_monthly_diversity_metrics.png`
- `birdnet_periodicity.png`  
  overall temporal-diagnostics figure combining all recorders currently present in the analysis, with ACF, PACF, and spectral-density panels for detections and unique species

- `birdnet_identifications_over_time_by_recorder.png`
- `birdnet_top_10_species_detections_through_time_by_recorder.png`
- `birdnet_cumulative_new_species_by_recorder.png`
- `birdnet_identifications_by_species_by_recorder.png`
- `birdnet_identifications_by_species_by_month_by_recorder.png`
- `birdnet_monthly_diversity_metrics_by_recorder.png`
- `birdnet_periodicity_by_recorder.png`  
  multi-panel recorder-comparison temporal-diagnostics figure with detection and species-richness periodicity panels

- `recorders/<RECORDER_ID>/...`  
  recorder-specific figures written for each recorder that currently has usable detections (for example `recorders/GEL_A/`)

In the species-frequency plots, the identification axis is shown on a log<sub>10</sub> scale, and common names are displayed in lowercase except where proper nouns remain capitalised. Latin names are italicised in the species-axis labels.
The root-level analysis figures are the combined overall results across all recorders currently present in `out/`. Additional recorder-comparison figures are written as multi-panel plots, and recorder-specific figures are written into the `recorders/` subdirectory as each recorder becomes available.
Monthly diversity metrics treat the number of detections per species as the abundance proxy for Shannon, Simpson, and Hill-number calculations, and are produced as overall combined figures, recorder-specific figures, and recorder-comparison figures.
The top-species time-series plots default to 24-hour bins through `top_species_time_bin_minutes <- 24 * 60`, but that bin size can be changed directly in `scripts/analyse_birdnet_output.R`.
In the recorder-comparison diversity figure, each recorder-by-metric panel now uses its own y-axis range so Shannon, Simpson, and Hill-number panels are scaled to their local maxima.
The temporal periodicity figures now show both detections per time bin and unique species identified per time bin. ACF and PACF panels include approximate significance bands (±1.96/√N), spectral-density panels mark the strongest candidate periods, and the companion Ljung-Box CSV outputs provide a compact test summary at key lags for easier interpretation of recurring temporal structure.

### Diversity-metric calculations

For each recorder and each calendar month, the analysis script:

1. filters detections to those meeting `min_confidence`
2. groups the remaining detections by recorder and month
3. counts the number of detections for each species within that recorder-month
4. treats those species-level detection counts as the abundance vector
5. converts counts to relative abundance:

<code><em>p<sub>i</sub></em> = <em>n<sub>i</sub></em> / <em>N</em></code>

where:

- <code><em>n<sub>i</sub></em></code> = number of detections for species <code><em>i</em></code>
- <code><em>N</em></code> = total number of detections across all species in that recorder-month
- <code><em>p<sub>i</sub></em></code> = relative abundance of species <code><em>i</em></code>

The diversity metrics are then calculated as follows:

#### Shannon diversity index

<code><em>H</em><sup>'</sup> = -Σ(<em>p<sub>i</sub></em> log<sub><em>e</em></sub> <em>p<sub>i</sub></em>)</code>

#### Simpson diversity index

The script reports the Gini-Simpson form:

<code>1 - Σ (<em>p<sub>i</sub></em><sup>2</sup>)</code>

#### Hill number, <em>q</em> = 1

This is the exponential of Shannon diversity:

<code><sup>1</sup><em>D</em> = <em>e</em><sup>H'</sup></code>

#### Hill number, <em>q</em> = 2

This is the inverse Simpson concentration:


<code><sup>2</sup><em>D</em> = 1 / Σ <em>p<sub>i</sub></em><sup>2</sup>
</code>

Interpretation in this workflow:

- larger Shannon and Hill <em>q</em> = 1 indicate greater effective diversity with sensitivity to both common and less-common species
- larger Simpson and Hill <em>q</em>  = 2 indicate greater diversity with stronger weighting toward the most frequently detected species
- because the pipeline uses detections rather than direct counts of individuals, these are diversity estimates based on the assumption that detection frequency is a reasonable proxy for relative abundance

## Console progress during archive runs

When `Rscript scripts/process_tar_archive.R` is running, the console reports:

- current file index and percent complete
- current per-file stage percent
- current archive member being processed
- archive streaming progress
- extraction/download step
- `.flac` to `.wav` conversion step
- BirdNET range-filter step
- BirdNET prediction step
- output-writing step
- cleanup step
- per-file elapsed time
- estimated time remaining

Archive streaming, extraction, and conversion stages are monitored through `processx`, so they emit recurring heartbeat updates instead of staying silent until the subprocess returns.

BirdNET analysis is also run in a monitored child R process through <code>callr</code>, so TensorFlow/TFLite warnings should no longer make the main console progress appear frozen.

## Resume behaviour

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

### 8. Slow extraction from `.tar.zst`

This workflow extracts one archive member at a time. For compressed `.tar.zst` archives, that can still be slow because `tar` may need to scan or decompress a large portion of the archive to reach a later member.

That means a file can legitimately spend a long time in the extraction/download stage even when it is not frozen. The script now emits heartbeat updates during that stage so you can tell the process is still alive.

### 9. Slow BirdNET inference

BirdNET model startup and inference can also take a long time, especially on the first files of a run while Python/model dependencies initialize. The archive runner now polls that stage from a child R process and keeps updating console and text progress during analysis.

### 10. Interrupted runs

If the process is interrupted, rerun:

```bash
Rscript scripts/process_tar_archive.R
```

Files with existing summary outputs skipped automatically.

## Notes

- temporary extracted `.flac` and converted `.wav` files deleted after each file is processed
- helper functions live in `scripts/birdnet_helpers.R`
- archive processor mirrors the archive subdirectory structure in the output folder when writing per-file CSV results
