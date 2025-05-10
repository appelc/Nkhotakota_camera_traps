# Nkhotakota camera traps

## Code and data for Appel et al. (Ecological Applications)

This repository contains code and data to evaluate a YOLOv4 multiclass detector model trained on wildlife species from camera trap images in Nkhotakota Wildlife Reserve, Malawi. Annotation, model training, and review were performed using the [Njobvu-AI](https://github.com/sullichrosu/Njobvu-AI) software.

The model weights file and inference script are also available (see YOLO folder).

Images and annotations are published in the **Nkhotakota Camera Traps** project on [LILA.science](https://lila.science/).

### Directory contents:

#### data

-   **megadetector**: outputs from [MegaDetector v5](https://github.com/agentmorris/MegaDetector) inference

-   **predictions**: outputs from YOLO inference (as downloaded from Njobvu-AI)

-   **review_summaries**: summary files after projects have been reviewed (as downloaded from Njobvu-AI)

-   **training_summaries**: summaries of training data by class, hexagon, and site

#### code

R scripts to conduct model evaluation presented in the manuscript

-   `00_parse_files.R` parses files in **predictions** and **review_summaries** folders

-   `01_summarize.R` summarizes data splits by class

-   `02_model_evaluation_metrics.R` calculates precision, recall, and F1 metrics

-   `03_model_evaluation_counts.R` compares true vs. predicted per-image animal counts

-   `04_model_evaluation_species_richness.R` compares true vs. predicted species richness

-   `05_model_evaluation_md_comparison.R` compares YOLO vs. MegaDetector predictions

#### YOLO:

Python code and model file to perform inference using the Nkhotakota YOLOv4 multiclass detector

-   model weights file: model weights file (trained from YOLOv4 base weights using [darknet](https://github.com/AlexeyAB/darknet) implemented in [Njobvu-AI](https://github.com/sullichrosu/Njobvu-AI))

-   `labelBatchPhotosArray.py` to generate predictions on new data

-   Njobvu-AI projects script: to create Njobvu-AI projects for review of model predictions

Example implementation with darknet installed locally:

1.  open `labelBatchPhotosArray.py` in a text editor and change the following directories: ...
2.  run the following code in a terminal

```         
python labelBatchPhotosArray.py path/to/image/folder --device 1
```

```         
python create_njobvu_project_single.py path/to/OUTfile.txt
```

|                  |                       |
|------------------|-----------------------|
| ![](eland.png)   | ![](honey_badger.png) |
| ![](leopard.png) | ![](zebra.png)        |

*Example images from Nkhotakota Camera Traps*
