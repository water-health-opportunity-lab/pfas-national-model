# pfas-national-model

**Code repository for RIA and CDRF project**
_"Innovative Transfer Learning for National Scale PFAS Prediction in Private Wells"_

This project develops reproducible, data-driven workflows to predict PFAS contamination in private wells across the United States using transfer learning techniques. We utilize public datasets, geospatial methods, and exploratory data analysis in R.

--- 
## 📁 Project Structure

```text
pfas-national-model/
├── LICENSE               # License information
├── pfas-national-model.Rproj  # RStudio project file
├── README.md             # Project documentation
├── renv.lock             # Records the exact versions of R and R packages to ensure reproducibility
├── renv/                 # Directory containing renv infrastructure for the project
├── data/                 # Directory containing input and output data, do not commit to git
└── script/               # Main analysis scripts
    ├── 1_access_data/                # Scripts to download or load raw data
    ├── 2_data_wrangling/             # Scripts for cleaning and transforming data
    ├── 3_exploratory_data_analysis/  # Exploratory data analysis and initial visualizations
    ├── 4_model_training/             # Scripts for training transfer learning models
    └── 5_model_validation/           # Scripts for validating model performance
```
## 🚀 Getting Started with renv
This project uses renv to manage R package dependencies, ensuring reproducibility. Here's how to get started:

Clone the Repository:
```text
git clone https://github.com/water-health-opportunity-lab/pfas-national-model.git # Replace with the actual URL
cd ncwell
```

Open the Project in RStudio: Open the pfas-national-model.Rproj file in RStudio. This will automatically activate the renv environment.
Restore Package Library: Run the following command in the R console to install the correct package versions:

```text
renv::restore()
```

This will install the exact package versions specified in the renv.lock file.
Confirm that the packages have been installed by running:

```text
renv::status()
```

This will show you the status of the packages in your project.
Work in the Project: You're now ready to work in the project! Any packages you install will be managed by renv.

## 🔧 Requirements
This project uses R.
To begin, open `pfas-national-model.Rproj` in RStudio and run scripts in the `script/` folder in order.

## 🔐 API Keys
Store your API keys (e.g., Census API) in an `.Renviron` file in your home directory:
```text
CENSUS_API_KEY=your_key_here
```
Do not commit your `.Renviron` file to Git.

## 🚧 Branching Guidelines
This project uses the Git Flow workflow. Please follow these conventions when contributing:

Start from the develop branch:
```text
git checkout develop
git pull origin develop
```

Create a new feature branch using the feat/ prefix:
```text
git checkout -b feat/short-description
```

Push your feature branch to GitHub:
```text
git push -u origin feat/short-description
```

Submit a pull request from your feature branch into develop.
Tag @xindyhu for review.

## 📌 Project Status
This project is in active development under the RIA and CDRF grant.
    
