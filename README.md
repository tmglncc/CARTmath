# CARTmath

It simulates a three-cell mathematical model to describe tumor response to CAR T cell immunotherapy in immunodeficient mouse models.

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
> install.packages("shinydashboardPlus")
> install.packages("numbers")
> install.packages("shinyjs")
> install.packages("DT")
> install.packages("shinyFeedback")
> install.packages("knitr")
> tinytex::install_tinytex() 
> install.packages("webshot")
> webshot::install_phantomjs()
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

1. E. A. Paixão, G. T. Naozuka, A. M. P. Valli, B. de J. Rodrigues, L. R. C. Barros, R. C. Almeida, CAR**T***math*. Available at [https://github.com/tmglncc/CARTmath](https://github.com/tmglncc/CARTmath) (2020)

2. tmglncc. (2021, January 19). tmglncc/CARTmath: CARTmath v1.0 (Version v1.0). Zenodo. [http://doi.org/10.5281/zenodo.4450377](http://doi.org/10.5281/zenodo.4450377)
