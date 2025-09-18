# Nkhotakota camera traps

## Code and data for [Appel et al. 2025](https://doi.org/10.1002/eap.70096) (*Ecological Applications*)

🖥️ This repository contains code and data to evaluate a YOLOv4 multiclass detector model trained on wildlife species from camera trap images in Nkhotakota Wildlife Reserve (NWR), Malawi. Annotation, model training, and review were performed using the [Njobvu-AI](https://github.com/sullichrosu/Njobvu-AI) software.

🔗 The model weights file is available to download here: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15994922.svg)](https://doi.org/10.5281/zenodo.15994922) and instructions for performing inference are included below.

🐘 Images and annotations are published in the [Nkhotakota Camera Traps](https://lila.science/datasets/nkhotakota-camera-traps/) project on LILA BC.

------------------------------------------------------------------------

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

    -   "training_data_bboxes.csv" contains the annotations used to train the model

#### YOLO_NWR

Python code to perform inference using the Nkhotakota YOLOv4 multiclass detector

-   `inference_yolo.py` to generate predictions on new data

-   `njobvu_project.py` to create an Njobvu-AI project for review of model predictions

------------------------------------------------------------------------

### Example implementation of NWR_YOLO model

#### OPTION 1: using Njobvu-AI

1.  Download the [NWR_YOLO_v1 model files from Zenodo](https://zenodo.org/records/15994922) and unzip the folder

2.  Ensure [Njobvu-AI](https://github.com/sullichrosu/Njobvu-AI) is installed locally

3.  Ensure [darknet](https://github.com/AlexeyAB/darknet) is installed locally

4.  Follow the instructions to create a project with optional image classification from the [Njobvu-AI documentation](https://github.com/sullichrosu/Njobvu-AI?tab=readme-ov-file#installation)

    *NOTE:* the input with model weights and configuration files will be "model_files.zip" in the folder downloaded in Step 1

#### OPTION 2: using the command line

1.  Download the [NWR_YOLO](https://github.com/appelc/Nkhotakota_camera_traps/tree/main/NWR_YOLO) folder from this repository
2.  Download the [NWR_YOLO_v1 model from Zenodo](https://doi.org/10.5281/zenodo.15392141). Unzip the contents of "model_files.zip" and place these 3 files in the NWR_YOLO directory from Step 1
3.  Ensure [darknet](https://github.com/AlexeyAB/darknet) is installed locally
4.  Open `inference_yolo.py` in a text editor and update **darknet_path** (line 15) with the installation location from Step 3
5.  Ensure image data are organized in the following way: *base_dir/image_folder/file1.JPG*, etc.
6.  Open a terminal, navigate to the location of the **NWR_YOLO** folder
7.  To generate predictions on new data using the NWR_YOLO model, run the following:

```         
python inference_yolo.py path/to/base_dir/image_folder --device 0
```

*NOTE:* the `--device` argument calls NVIDIA GPUs (highly recommended). If enabled on your system, reference the desired device here (e.g., 0, 1, 2). Note that CUDA must be enabled with darknet. If not using NVIDIA GPUs, omit the `--device` argument to run with CPUs.

To create an Njobvu-AI project using the predictions from Step 7:

8.  Ensure [Njobvu-AI](https://github.com/sullichrosu/Njobvu-AI) is installed locally
9.  Open `njobvu_project.py` in a text editor and update **njobvu_dir** (line 12) with the location of your local **Njobvu-AI-main** folder
10. Run the code below, replacing "OUTfile.txt" with the name of the output file generated in Step 7. Then open the Njobvu-AI program to view the project.

```         
python njobvu_project.py path/to/base_dir/image_folder/OUTfile.txt
```

|                  |                       |
|------------------|-----------------------|
| ![](eland.png)   | ![](honey_badger.png) |
| ![](leopard.png) | ![](zebra.png)        |

*Example images from Nkhotakota Camera Traps*

------------------------------------------------------------------------

### Citation:

Appel, Cara L., Ashwin Subramanian, Jonathan S. Koning, Marnet Ngosi, Christopher M. Sullivan, Taal Levi, and Damon B. Lesmeister. 2025. “Developing Custom Computer Vision Models with Njobvu-AI: A Collaborative, User-Friendly Platform for Ecological Research.” Ecological Applications 35(6): e70096. [doi.org/10.1002/eap.70096](https://doi.org/10.1002/eap.70096)
