# Does Hospital Leadership Matter? Evidence from Pay-for-Performance

Welcome to the repository for my job market paper, which estimates the effect of clinically trained executives on hospital repsonse to a change in financial incentives on quality. 

This project requires the extraction of executive names and titles from publicly available Tax Form 990s, historically housed by ProPublica's Nonprofit Explorer. In the RScripts folder, you will find that the data1 - data7 files are devoted the extraction of this information. This includes using the Nonprofit Explorer API to locate the URLs of each nonprofit hospital's tax form PDF, downloading these PDFs, and using Optimal Character Recognition to extract the relevant text. This is all reproducable as the forms are publicly available. I match this data to the American Hospital Association Survey data, yielding a unique data set of approximately 800 nonprofit hospitals with executive information. 

All tables and figures produced in the paper can be recreated using the tables_and_figures.R file. 
