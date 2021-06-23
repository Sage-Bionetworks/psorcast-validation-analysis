DATA_CURATION_PATH=curate_tables
FEATURE_EXTRACTION_PATH=feature_extraction
ANALYSIS_PATH=analysis

pipeline: update logs clean tables features markdown

update:
	git pull

logs:
	mkdir logs

clean:
	rm -f logs/pipeline.log;
	rm -f logs/error.log;


tables:
	Rscript $(DATA_CURATION_PATH)/MDJointCounting.R || exit 1
	Rscript $(DATA_CURATION_PATH)/jointCounting.R || exit 1
	Rscript $(DATA_CURATION_PATH)/MDJointSwelling.R || exit 1
	Rscript $(DATA_CURATION_PATH)/PPACMAN.R || exit 1
	Rscript $(DATA_CURATION_PATH)/digitalJarOpen.R || exit 1
	Rscript $(DATA_CURATION_PATH)/footImaging.R || exit 1
	Rscript $(DATA_CURATION_PATH)/psoriasisDraw.R || exit 1
	Rscript $(DATA_CURATION_PATH)/handImaging.R || exit 1
	Rscript $(DATA_CURATION_PATH)/psoriasisAreaPhoto.R || exit 1	
	Rscript $(DATA_CURATION_PATH)/walk30Seconds.R || exit 1	

features:
	Rscript $(FEATURE_EXTRACTION_PATH)/get_visit_summary.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/copy_site_images.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/digitalJarOpen_rotation_features.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/digitalJarOpen_sensor_features.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/PPACMAN_features.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/jointSummaries_features.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/psoriasis_draw_bsa_features.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/psoriasis_draw_per_body_zone.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/walk30s_features.R || exit 1
	Rscript $(FEATURE_EXTRACTION_PATH)/psorcast_merged_features.R || exit 1

misc:
	Rscript $(ANALYSIS_PATH)/handImaging_analysis/curateSwollenJointsDactyliticFingers.R || exit 1
	
markdown:
	Rscript $(ANALYSIS_PATH)/knit_markdowns.R || exit 1
