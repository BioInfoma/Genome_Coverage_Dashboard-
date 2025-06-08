# Genome_Coverage_Dashboard-
Dashboard

# Genome Coverage Visualizer

A user-friendly Shiny web application for visualizing genome coverage from TSV files. Ideal for researchers analyzing NGS data (e.g., WGS/WES) and tracking depth of coverage across reference genomes.

---

## Features

* Upload one or more `.tsv` files (with `Reference`, `Position`, `Depth` columns)
* Set a coverage depth threshold
* Customize line color and plot title
* Select specific sample when multiple files are uploaded
* Download the generated plot as a PNG image

---

## Input File Format

Each uploaded file should be a tab-separated `.tsv` file with **three columns**:
