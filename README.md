# CAR**T***math*

Immunotherapy has gained great momentum with chimeric antigen receptor T cell (CAR-T) therapy, in which patient’s T lymphocytes are genetically manipulated to recognize tumor-specific antigens, increasing tumor elimination efficiency. In recent years, CAR-T cell immunotherapy for hematological malignancies achieved a great response rate in patients and is a very promising therapy for several other malignancies. Each new CAR design requires a preclinical proof-of-concept experiment using immunodeficient mouse models. The absence of a functional immune system in these mice makes them simple and suitable for use as mathematical models. In this work, we develop a three-population mathematical model to describe tumor response to CAR-T cell immunotherapy in immunodeficient mouse models, encompassing interactions between a non-solid tumor and CAR-T cells (effector and long-term memory). We account for several phenomena, such as tumor-induced immunosuppression, memory pool formation, and conversion of memory into effector CAR-T cells in the presence of new tumor cells. Individual donor and tumor specificities are considered uncertainties in the model parameters. Our model is able to reproduce several CAR-T cell immunotherapy scenarios, with different CAR receptors and tumor targets reported in the literature. We found that therapy effectiveness mostly depends on specific parameters such as the differentiation of effector to memory CAR-T cells, CAR-T cytotoxic capacity, tumor growth rate, and tumor-induced immunosuppression. In summary, our model can contribute to reducing and optimizing the number of in vivo experiments with in silico tests to select specific scenarios that could be tested in experimental research. Such an in silico laboratory is an easy-to-run open-source simulator, built on a Shiny R-based platform called CARTmath. It contains the results of this work as examples and documentation. The developed model together with the CARTmath platform have potential use in assessing different CAR-T cell immunotherapy protocols and its associated efficacy, becoming an accessory for in silico trials.

![Graphical Abstract](https://drive.google.com/uc?export=view&id=1nkkMvrzF6AQXA8XO-eUGAaqmCBVttqdm)

## Requirements

* **R version > 3.5:**  
You can download the latest version of R for your operating system here: [https://www.r-project.org/](https://www.r-project.org/).

* **RStudio Desktop:**  
We also recommend the software RStudio Desktop to view, edit, and run R scripts. After installing R, you can get the latest version of RStudio Desktop here: [https://rstudio.com/](https://rstudio.com/).

## Installation

1. Clone this repository directly from terminal and enter its directory:  
```
$ git clone https:github.com/tmglncc/CARTmath.git
$ cd CARTmath/
```
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; **OR**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Download the *.zip* file and decompress it.

2. Open R or RStudio Desktop and install/update the following R packages:  
```R
> install.packages("shiny")
> install.packages("shinydashboard")
> install.packages("htmltools")
> install.packages("bsplus")
> install.packages("plotly")
> install.packages("devtools")
> require("devtools")
> install_version("shinydashboardPlus", version = "0.7.5")
> install.packages("numbers")
> install.packages("shinyjs")
> install.packages("DT")
> install.packages("shinyFeedback")
> install.packages("knitr")
> tinytex::install_tinytex() 
> install.packages("webshot")
> webshot::install_phantomjs()
```

3. For Linux system, install xclip via terminal by the command:
```
$ sudo apt-get install xclip
```

## Running CARTmath

1. Open the **app.R** file in R or RStudio Desktop.

2. Set the working directory with the path to **CARTmath** folder:
```R
> setwd("user/folder/CARTmath/")
```

3. Press <kbd>Ctrl</kbd> + <kbd>Shift</kbd> + <kbd>Enter</kbd> (for Windows and Linux users) or <kbd>Command</kbd> + <kbd>Shift</kbd> + <kbd>Enter</kbd> (for macOS users) until a second window is displayed.  
**OR**  
You can run directly in RStudio by clicking on **Run App** button.

4. **Optional:** Click on **Open in browser** button to open CARTmath app in browser.

5. In CARTmath app, follow **Manual** for further instructions.

## Cite as

Barros, L.R.C.; Paixão, E.A.; Valli, A.M.P.; Naozuka, G.T.; Fassoni, A.C.; Almeida, R.C. CART*math*—A Mathematical Model of CAR-T Immunotherapy in Preclinical Studies of Hematological Cancers. _Cancers_ **2021**, _13_, 2941. [https://doi.org/10.3390/cancers13122941](https://doi.org/10.3390/cancers13122941)
                              
Paixão, E.A.; Naozuka, G.T.; Valli, A.M.P., Barros, L.R.C.; Almeida, R.C. CAR**T***math*, 2020. Version 1.0. Available online: [https://github.com/tmglncc/CARTmath](https://github.com/tmglncc/CARTmath) (accessed on 01 June 2021), doi: [10.5281/zenodo.4450376](http://doi.org/10.5281/zenodo.4450376)
