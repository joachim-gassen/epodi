# If you are new to Makefiles: https://makefiletutorial.com

# Commands

RSCRIPT := Rscript --encoding=UTF-8
PYTHON := python3

# Main targets

PRESENTATION := output/presentation.pdf
	
	
# Data Targets

DIM_GRID_PRECOMP := data/grids/precomp_grid_600k_100z_600kmax_BW.csv
DIM_GRID := data/grids/grid_600k_100z_600kmax_BW.csv
WRDS_DATA := data/wrds/comp/company.parquet \
	data/wrds/comp/funda.parquet \
	data/wrds/crsp/msf.parquet \
	data/wrds/crsp/ccmxpf_linktable.parquet
PANELS_SIM := data/generated/panels_600k_100z_600kmax_BW_4000firms_25years.rds
PANEL_WRDS := data/generated/panel_wrds.rds

ALL_TARGETS := $(PRESENTATION) $(PANELS_SIM) $(PANEL_WRDS)

PRES_MATERIALS := docs/materials/beamer_theme_trr266_16x9.sty \
	docs/materials/trr266_logo.eps \
	docs/materials/github_logo.png \
	docs/materials/github_repo_qr_code.png \

	
# Phony targets

.phony: all clean dist-clean

all: $(ALL_TARGETS) 

clean:
	rm -f $(ALL_TARGETS)
	rm -rf ouput/*
	
dist-clean: clean
	rm -f $(DIM_GRID) 
	rm -f $(WRDS_DATA)

config.env:
	@echo "No config.env found. Please copy _config.env to config.env and edit it. Refer to the README for more details."
	@exit 1
	
# Recipes

$(WRDS_DATA): code/pull_wrds_data.R config.env
	$(RSCRIPT) code/pull_wrds_data.R

$(DIM_GRID): code/di_model.py
	@if [ -f $(DIM_GRID_PRECOMP) ]; then \
		  echo "Copying precomputed grid file $(DIM_GRID_PRECOMP) into place. Rename or delete this file if you want to force re-estimation of the grid."; \
		  cp $(DIM_GRID_PRECOMP) $@; \
		else \
		  echo "Precomputed grid file not found Will start re-estimation. This requires around 350 GB of (swap) RAM and will take ~2 hours."; \
		  echo "To avoid this, interrupt this job and run 'git checkout $(DIM_GRID_PRECOMP)' to restore the precomputed grid file"; \
			$(PYTHON) code/di_model.py; \
		fi
	
$(PANELS_SIM): code/create_sim_panels.R $(DIM_GRID)
	$(RSCRIPT) code/create_sim_panels.R

$(PANEL_WRDS): code/create_wrds_panel.R $(WRDS_DATA)
	$(RSCRIPT) code/create_wrds_panel.R

$(PRESENTATION): $(PANELS_SIM) $(PANEL_WRDS) \
	data/grids/precomp_grid_600k_101z_600kmax_BWznorm.csv \
	code/objects_sim_data.R code/objects_wrds_data.R \
	$(PRES_MATERIALS) docs/presentation.qmd
	quarto render docs/presentation.qmd --quiet
	@rm -rf output/docs/presentation_files
	@mv output/docs/presentation.pdf output/
	@rm -rf output/docs
