# Nkhotakota_camera_traps
code and data for Appel et al. (Ecological Applications)


data folder: 
- predictions: outputs from YOLO inference (as downloaded from Njobvu-AI)
- review_summaries: summary files after projects have been reviewed (as downloaded from Njobvu-AI)

scripts folder:
- 00_parse_files.R: parses files in 'predictions' and 'review_summaries' folders
- 01_model_evaluation_match.R:
- 02_model_evaluation_compare.R
- 03_model_evaluation_counts.Rmd
- 04_model_evaluation_species_richness.R
- 05_megadetector_comparison.R

(also need: 
- YOLO model file for Nkhotakota
- script to perform inference with YOLO model outside Njobvu-AI
- script to turn those predictions into Njobvu-AI projects for review
)