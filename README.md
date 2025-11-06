# Respiratory Health Dashboard

This interactive **Shiny dashboard** visualises and analyses **respiratory health indicators (COPD and Asthma)** across NHS regions in England, using publicly available datasets from **NHS Digital** and **Public Health England (PHE)**.

---

## Author
**Sakshi Sunil Dhavale**  
Postgraduate in Health Data Science, University of Manchester  

---

## Motivation & Context
This dashboard represents my interest in applying data science for real-world public health insights.  
Having previously worked on predictive modelling for **prostate cancer risk** and completed research on **colorectal cancer and the role of microbiota**, I’ve developed a strong passion for using data to understand and improve health outcomes.

The project reflects my commitment to **open, reproducible, and transparent research** the core principles of data-driven healthcare and population science.

---

## Project Overview
The dashboard was developed as part of a professional data visualisation project focusing on **COPD and Asthma outcomes** within the NHS.  
It demonstrates an end-to-end analytical workflow from **data wrangling, aggregation, and harmonisation** to **interactive visualisation** and **stakeholder interpretation** using R and Shiny.

### Key Highlights
- Compares **COPD and Asthma prevalence** and **emergency hospital admission rates** across NHS geographies (GP, PCN, CCG, ICB, Region)  
- Interactive **geographic visualisation** showing regional variation  
- Displays **achievement scores, emergency rates, and prevalence trends** to identify outliers and performance patterns  
- Designed for **scalability**, allowing integration of additional years or conditions  
- Fully reproducible pipeline demonstrating applied **data engineering and data governance principles**

---

## Technical Highlights
- **Languages & Libraries:** R, Shiny, dplyr, tidyr, ggplot2, plotly, leaflet, sf, viridis  
- **Data Engineering:** Structured ETL workflow for transforming NHS Digital data into analysis-ready format  
- **Quality Assurance:** Implemented missing value checks, weighted averages, and harmonisation across differing NHS geographies  
- **Visualisation:** Interactive maps (leaflet), responsive dashboards (shinydashboard), and dynamic summary statistics  
- **Scalability:** Modular structure allows addition of more datasets or health conditions in future analyses  

---

## Data Sources
- **Quality Outcomes Framework (QOF)** 2020–22 – NHS Digital  
- **GP Practice List and Locations** – NHS Digital  
- **Public Health England Fingertips Data** – Respiratory Health Indicators  

_All datasets used are publicly available and have been cleaned, standardised, and integrated for demonstration._

---

## Dashboard Preview

Below are sample views from the **Respiratory Health Dashboard**, showing interactive and analytical capabilities across tabs:

| **Overview Page** | **Geographic View** | **Detailed Analysis** | **Data Availability** |
|--------------------|---------------------|-----------------------|-----------------------|
| ![Overview Page](Images/GeneralOverview.png) | ![Geographic View](Images/GeographicView.png) | ![Detailed Analysis](Images/DetailedAnalysis.png) | ![Data Availability](Images/DataAvailability.png) |

Each section highlights a distinct part of the analysis pipeline  
from summarising national indicators and visualising regional variations,  
to performing detailed comparative analysis and ensuring transparency in data usage.

---

## How to Run the App

To run the app locally in **RStudio**:

```r
# Install required packages if needed
install.packages(c("shiny", "shinydashboard", "leaflet", "dplyr", 
                   "tidyr", "ggplot2", "plotly", "DT", "sf", "viridis", "geojsonio"))

# Launch the dashboard
library(shiny)
shiny::runApp("app.R")



```python

```
