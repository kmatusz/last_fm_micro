# last_fm_micro
This repository contains analysis of last.fm artists data

#### Content

- 01_fetch_artists.R - make requests to last.fm api about polish artists. 10000 artists were obtained in total
- 02_fetch_artists_details.R - make requests to download details about artists (playcount, similar etc.). This script runs for ~4 hrs.
- 03_extract_data.R - extract needed information from raw JSON obtained through API. 
- 04_analysis.R - all analysis on clean datasets
- 05_artist_network.R - analysis using artists similarities graph
- 06_yule.R - testing yule distribution
- **09... - simulations report**
