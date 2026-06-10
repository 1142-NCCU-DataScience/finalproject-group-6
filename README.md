[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/xfVbwuLD)
# [Group 6] 從位置到多元球風：現代 NBA 球員角色分析
<img width="1482" height="150" alt="image" src="https://github.com/user-attachments/assets/aacc76cd-79ea-4a02-89a9-546718a522ac" />

The goals of this project is to analyze the transformation of modern nba player comparing with history data.

## Demo video and link

[youtube demo](https://youtu.be/sVdunvIftJI)
[our project link](https://bear7066.shinyapps.io/nba-clustering/)

## Contributors

|組員|系級|學號|工作分配|
|-|-|-|-|
|何大南|資科碩一|114753109|負責analyze and group data| 
|張小明|資科碩二|xxxxxxxxx|團隊的中流砥柱，一個人打十個|
|王冠智|資科碩ㄧ|115753205|負責 web frontend, visualization, github management|
|黃思璇|資科碩一|114753204|負責 Data Collection, Data Filtering, Data Preprocessing, Presentation Development|
|王瑜靖|土測四|111207430|負責topic framing, poster production, slides drafting|
|||||
|||||

## Quick start
###Set up

0. Go into code directory first.

1. Installation

```Rscript -e "install.packages(c('shiny','bslib','tidyverse','plotly','DT','bsicons'), repos='https://cloud.r-project.org')"```

2. Execute main app

```Rscript -e "shiny::runApp('c.R', host='127.0.0.1', port=3838, launch.browser=FALSE)"```

3. Kill the port

```lsof -tiTCP:3838 -sTCP:LISTEN | xargs kill```

## Folder organization and its related description
This idea is emerged by our discussion during rest time of the class.

### docs
* Your presentation, 1142_DS-FP_groupID.ppt/pptx/pdf (i.e.,1142_DS-FP_group1.ppt), by **06.09**
* Any related document for the project, i.e.,
  * discussion log
  * software user guide

### data
* Input
  * Source
  * Format
  * Size

### code
* Analysis steps
* Which method or package do you use?
* How do you perform training and evaluation?
  * Cross-validation, or extra separated data
* What is a null model for comparison?

### results
* What is your performance?
* Is the improvement significant?

## References
* Packages you use
* Related publications
