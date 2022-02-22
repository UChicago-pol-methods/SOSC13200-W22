# SOSC 13200: Social Science Inquiry II
## Section 3
### University of Chicago
### Winter Quarter 2022



The goal of this course has been to get you to approach data, and data analysis, thinking like a social scientist. That means thinking about where your data comes from and how it’s measured, and assessing and describing relationships among variables. We have used some statistical methods for quantitative analysis, and worked on coding data analysis and data visualizations in R. 

For your final project, you will apply this approach to a data set of your choosing. Each student will submit their own report and will be graded independently. Multiple students may use the same data set, and you are welcome to collaborate with other students in the class if you would like, but your grade will be based on what is represented in your own report. 

Your final project will consist of three components, submitted each week, as regular homework assignments. However, there are no solution sets, and final projects will be graded *comprehensively*; that is, all components will be graded together at the end of the quarter, as they are intended as complementary parts of a single project. 

For each component, as with regular homework assignments, submit both the R source file and the compiled pdf report. 

### Part I: Data set selection and descriptive analysis.
#### Due Monday 2/28 at 5pm

Select your data set from among those listed **below**, or propose a different data set that you would like to analyze. Your data set does not need to be directly related to social science. 

If your propose your own data set, consider that you will be asked to produce data visualizations and regression analysis on your data, so pick something with at least a couple of numeric variables. Please also select a data set with fewer than 1 million records. If you propose your own data set, *you must also share the data set with the instructor*–either email the data file, or provide a link to where the file may be downloaded. 

Write narrative and code answering the below questions. Your compiled report for this part of the project, including narrative and compiled code, should be about 2 pages long. You do not need to answer question by question, but use the questions below to guide your description. The objective of this first project component is to demonstrate that you have a good understanding of where your data comes from, and what is going on in your data set. This will require some *exploratory* analysis-look at your data, maybe make some plots, and make sure you understand what all of the variables represent. 

1. Describe where the data comes from, and how it was generated. Is it from a survey? A census? An experiment? Was it collected by researchers for an academic project, or is it administrative data that is routinely collected by a government or other organization? What else can you say about how the data was generated?
2. What does each observation describe in the data set? I.e., what does each row represent–a measurement from an individual person? Measurement from a specific geographic location? Are there multiple measurements over different periods in time or space? Is there anything weird or interesting about how your data is formatted–.e.g, is your data [*long* or *wide*](https://stats.oarc.ucla.edu/r/faq/how-can-i-reshape-my-data-in-r/)?
3. What is the population represented in the data set? Is it from a sample from a larger population, or does the data set itself include all relevant units from the population? If you were to analyze the data set, would you expect the results to be relevant for other units or groups?
4. Provide some descriptive summary of the dataset using code. I do *not* want to see a full page that is just `head(data)`, but consider summary statistics that can help a reader understand, at a high level, what is going on in the data set. How many rows and columns are there? Is there missing data? Are variables mostly numeric, character, or are there different data types represented? Do you have categorical variables, that represent different levels of something? Are these levels ordered (e.g., grade levels) or unordered (e.g., colors). Is there anything else relevant about how your data is coded?




### Part II: Multiple linear regression and data visualizations. 
#### Due Monday 3/7 at 5pm

[Description TBA]

### Part III: Finalize report and discussion. 
#### Due Friday 3/11 at 5pm 
**Note the short turn-around between parts II and III**

[Description TBA]



## Data sets you can choose from: [list in progress]

- New York Times COVID data sets
    - source: [https://github.com/nytimes/covid-19-data](https://github.com/nytimes/covid-19-data)
    - files: There are a number of different files available for download at the site, including [national](https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv), [state](https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv), and [county](https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv)-level data.  

- Replication data for Gerber, Alan S., Donald P. Green, and Christopher W. Larimer, 2008, “Social Pressure and Voter Turnout: Evidence from a Large-Scale Field Experiment.” 
    - source: [ISPS Data Archive](http://hdl.handle.net/10079/c7507a0d-097a-4689-873a-7424564dfc82) 
    - file: **ggl_household.csv**
    
- Replication data for Fowler, Anthony and Andrew Hall, 2017, "Do Shark Attacks Influence Presidential Elections? Reassessing a Prominent Finding on Voter Competence." 
    - source: [Harvard Dataverse](https://doi.org/10.7910/DVN/AE5BPK/H8QVC8)
    - file: **SharkAttacksElectionsCleaned.csv**

- Replication data for LaLonde, Robert. 1986, "Evaluating the Econometric Evaluations of Training Programs."
    - source: [http://sekhon.berkeley.edu/matching/lalonde.html](http://sekhon.berkeley.edu/matching/lalonde.html)
    - file: **lalonde.csv**

- Popular baby names by state, as collected by the US Social Security Agency   
    - source: [https://www.ssa.gov](https://www.ssa.gov/oact/babynames/limits.html) 
    - files: **namesbystate/**; includes state-specific files

- Pokemon dataset on all 802 Pokemon from all Seven Generations of Pokemon/ 
    - source: [https://www.kaggle.com](https://www.kaggle.com/rounakbanik/pokemon)
    - file: **pokemon.csv**

-  Life Expectancy at Birth by Sex for Illinois, Chicago and Illinois Counties: 1989-1991, 1999-2001 and 2009-2011
    - source: [https://data.illinois.gov](https://data.illinois.gov/dataset/426idph_life_expectancy_at_birth_by_sex_for_illinois_chicago_and_illinois_counties_19891991_19992001)
    - file: **life.csv**


- Chicago speed camera violations, as collected by the City of Chicago
    - source: [https://data.cityofchicago.org](https://data.cityofchicago.org/Transportation/Speed-Camera-Violations/hhkd-xvj4) Chicago Data Portal
    - file: **traffic_camera.csv** 
    
   

## Other resources for data sets

- The [Data is Plural website](https://www.data-is-plural.com/) and [archive](https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/edit#gid=0) also have a number of interesting data sets. 

- [FiveThirtyEight](https://data.fivethirtyeight.com/) shares the data (and some code) used to generate some of the graphs and figures on their website. 

- [Buzz feed](https://github.com/BuzzFeedNews) shares some data sets from Buzz Feed News. 

- [ProPublica](https://www.propublica.org/datastore/datasets) hosts a number of unique and custom data sets relevant to their projects on investigative journalism for public interest. 

- [Kaggle](https://www.kaggle.com/datasets) has a vast collection of user-submitted data sets. 

