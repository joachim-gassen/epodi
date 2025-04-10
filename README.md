## Earnings Properties of a Dynamic Investment Model

### Idea

This repository contains a sketchy reanalysis of the findings of the working paper
"(Mis-)Matching and Earnings Properties: Implications of Dynamic Investments" by Matthias Breuer and David Windisch. By building on the [QuantEcon Python Package](https://quantecon.org), it uses a different code base than [Breuer and Windisch (JAR, 2019)](https://doi.org/10.1111/1475-679X.12253) so deviations from the original findings are to be expected. The goal is to provide an accessible and flexible implementation of the model, which can be used for further research and analysis.

It was inspired by a discussion that I provided for the above working paper. You can  find the discussion slides in the `static` folder in case you are interested.


### Reproduce simulation results and slide deck

To reproduce the slide deck, you need a reasonably recent R and quarto installation plus the following libraries:

- tidyverse
- duckdb
- arrow
- fixest
- logger

As a next step, you need to copy `_config.env` to `config.env` and add your WRDS credentials. You need access to Compustat, CRSP, and Compustat/CRSP merged. Do **not** commit the `config.env` file to the repository (it is included in `.gitignore` to avoid you doing this accidentally).

Assuming that you have maketools installed, you can run then run `make all`
in the project root to create the necessary data files and the slide deck. The data files will be stored in the `data/generated` folder and the slide deck will be stored in the `output` folder.

If you do not have make installed, you can try your luck by executing the following script.

```bash
# Copy the pre-computed dynamic investment model grid
copy data/grids/precomp_grid_600k_100z_600kmax_BW.csv \
    data/grids/grid_grid_600k_100z_600kmax_BW.csv

# Download WRDS data (takes ~30 minutes)
Rscript code/pull_wrds_data.R

# Create simulated samples (takes ~10 minutes)
Rscript code/create_sim_panels.R

# Create Compustat/CRSP sample
Rscript code/create_wrds_panel.R

# Render the slide deck
quarto docs/presentation.Rmd
``` 

### Re-estimate the dynamic investment model grid

As the code uses the Python quantecon package to do the heavy lifting around the dynamic programming algorithms needed to estimate the solution for the dynamic invest model, you need Python 3.8 or higher. In addition, you need an environment with a lot of RAM/swap space (about 350 GB). If you are short on RAM, you can adjust the size of the estimation grid by setting `k_size`and `z_size` in `code/di_model.py` to smaller values (e.g. 100/100).

To setup the virtual Python environment and install the required packages, run the following command in your terminal:

```bash
# Create a virtual environment
python3 -m venv .venv
# Activate the virtual environment
# On Windows
.venv\Scripts\activate
# On MacOS/Linux
source .venv/bin/activate
# Install the required packages
pip install -r requirements.txt
```

Then, you can estimate the parameter grid of the model by running `python code/di_model.py`. Feel free to play with the model parameters to see how the resulting simulated sample properties change.


### License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


### Acknowledgements

This project has been developed jointly with the [Open Science Data Center](https://www.accounting-for-transparency.de/projects/open-science-data-center/) of the [TRR 266 "Accounting for Transparency"](https://www.accounting-for-transparency.de) and its repository is inspired by their [treat repository](https://github.com/trr266/treat).
