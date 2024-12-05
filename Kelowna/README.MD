## Kelowna Bus Network Analysis
Created by: Kenichiro Anderson, 60531845

This project analyzes the bus network in Kelowna, BC, using graph theory and centrality measures. The analysis focuses on identifying key bus stops based on their connectivity within the network. The project leverages centrality metrics such as degree centrality and eigenvector centrality to visualize and interpret the importance of bus stops in the transportation system.


## Libraries Used
#### R Packages:
- ggplot2: For visualization.
- dplyr: For data manipulation.
- sf: For spatial data handling.
- igraph: For graph creation and centrality analysis.
- viridis: For color scales in visualizations.


## How to Run
1. Clone the repository or download the files.
2. Install the required R packages: ggplot2, dplyr, sf, igraph, and viridis.
3. Place the data files (Kelowna_stops.csv and Kelowna_routes/) in the working directory.
4. Run the R scripts in the provided order.
##### Or alternatively users should be able to run this within the folder ensure you have R and R studio downloaded and then open the file within R and do not move any of the file folders.


## Reasoning behind the code:
- for some of the more complicated code such as displaying heat maps, re-importing the csv and shape files was done to ensure that if data was previously altered within the previous code cell then it would be fresh upon each new code cell when large analysis is required as R had large issues with this. 

## Data Soruce 
https://www.bctransit.com/open-data/