# Nkhotakota camera traps

## Code and data for Appel et al. (Ecological Applications)

This repository contains code and data to evaluate a YOLOv4 multiclass detector model trained on wildlife species from camera trap images in Nkhotakota Wildlife Reserve (NWR), Malawi. Annotation, model training, and review were performed using the [Njobvu-AI](https://github.com/sullichrosu/Njobvu-AI) software.

The NWR_YOLO_v1 model weights file is available to download here: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15392141.svg)](https://doi.org/10.5281/zenodo.15392141) and an inference script is included [below](#example-implementation-of-NWR_YOLO-model).

Images and annotations are published in the **Nkhotakota Camera Traps** project on [LILA.science](https://lila.science/).

### Directory contents:

#### code

R scripts to conduct model evaluation presented in the manuscript

-   `00_parse_files.R` parses files in **predictions** and **review_summaries** folders

-   `01_summarize.R` summarizes data splits by class

-   `02_model_evaluation_metrics.R` calculates precision, recall, and F1 metrics

-   `03_model_evaluation_counts.R` compares true vs. predicted per-image animal counts

-   `04_model_evaluation_species_richness.R` compares true vs. predicted species richness

-   `05_model_evaluation_md_comparison.R` compares YOLO vs. MegaDetector predictions

#### data

-   **megadetector**: outputs from [MegaDetector v5](https://github.com/agentmorris/MegaDetector) inference

-   **predictions**: outputs from YOLO inference (as downloaded from Njobvu-AI)

-   **review_summaries**: summary files after projects have been reviewed (as downloaded from Njobvu-AI)

-   **training_summaries**: summaries of training data by class, hexagon, and site

#### YOLO_NWR

Python code and model files to perform inference using the Nkhotakota YOLOv4 multiclass detector

-   `inference_yolo.py` to generate predictions on new data

-   `njobvu_project.py` to create a Njobvu-AI project for review of model predictions

### Example implementation of NWR_YOLO model {#example-implementation-of-nwr_yolo-model}

1.  Download the [NWR_YOLO](https://github.com/appelc/Nkhotakota_camera_traps/tree/main/YOLO) folder from this repository
2.  Download the [NWR_YOLO_v1 model from Zenodo](https://doi.org/10.5281/zenodo.15392141), unzip the folder, and place it in the NWR_YOLO directory from Step 1
3.  Ensure [darknet](https://github.com/AlexeyAB/darknet) is installed locally
4.  Open ``` inference_yolo``.py ``` in a text editor and update **darknet_path** (line 15) with the installation location from Step 3
5.  Ensure image data are organized in the following way: *base_dir/image_folder/file1.JPG*, etc.
6.  Open a terminal, navigate to the location of your local **NWR_YOLO** folder
7.  To perform inference (generate predictions on new data using the NWR_YOLO model):

```         
python inference_yolo.py path/to/base_dir/image_folder --device 1
```

*OPTIONAL:* To create a Njobvu-AI project using the predictions from Step 7

8.  Ensure [Njobvu-AI](https://github.com/sullichrosu/Njobvu-AI) is installed locally
9.  Open `njobvu_project.py` in a text editor and update **njobvu_dir** (line 12) with the location of your local Njobvu-AI-main folder
10. Run the code below, then open the Njobvu-AI program to view the project

```         
python njobvu_project.py path/to/base_dir/image_folder/OUTfile.txt
```

|                  |                       |
|------------------|-----------------------|
| ![](eland.png)   | ![](honey_badger.png) |
| ![](leopard.png) | ![](zebra.png)        |

*Example images from Nkhotakota Camera Traps*
