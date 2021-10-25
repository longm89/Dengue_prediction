dengue_features_train_url <- "https://drive.google.com/uc?id=1foEfxGUxz2p66R6EXL5Iu6WvVuauL_Cf&export=download"
dengue_features_train_saved_file <- "data/dengue_features_train.csv"
download.file(dengue_features_train_url, destfile = dengue_features_train_saved_file)

dengue_labels_train_url <- "https://drive.google.com/uc?id=1E1cecufN93UVt6GA4Pqp6BxOUK3Gx3_G&export=download"
dengue_labels_train_saved_file <- "data/dengue_labels_train.csv"
download.file(dengue_labels_train_url, destfile = dengue_labels_train_saved_file)

countries_data_url <- "https://drive.google.com/u/0/uc?id=1-2DR6B3EQXpaiUd5MEtod6UMg4DWImTs&export=download"
countries_data_saved_file <- "data/countries_data.csv"
download.file(countries_data_url, destfile = countries_data_saved_file)
