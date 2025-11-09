# Power Pulse  

**Power Pulse** is a project designed to track great power transition through **economic statecraft**.  

The first version focuses on combining **public opinion data** from **Nigeria** and **Kenya** toward their major donor countries. The repository contains Python scripts (keep updating), each contributing to the construction, processing, and integration of datasets. It also has multilevel tests in R code as well as some visualization results (in PNG or HTML format). 

---

## Python File Naming & Workflow  

- **Files with `0714`**  
  Initial processing of **public opinion datasets** from Pew Research Center:  
  - Counting respondents by country and year  
  - Applying weights to survey responses  
  - Transforming qualitative answers into numeric values  
  - Generating visualizations  

- **Files with `0721` and `0724`**  
  - Refine the **regional categorization of respondents**  
  - Provide more accurate **geolocation context** for analysis  

- **Files with `0730`**  
  - Process **party affiliation data** for Nigeria  

- **Files with `0813`**  
  - Merge **Afrobarometer data** with Pew Research data  
  - Focus on integrating survey questions with **similar design**  

- **Files with `0814`**  
  - Construct the **final dataset** for analysis by combining:  
    - Public opinion data  
    - Foreign aid allocations to each region

- **Files with `1030` and `1103` and `1109`**  
  - Identify causal relations under a multilevel model:
    - Staggered DID
    - Continuous DID   


