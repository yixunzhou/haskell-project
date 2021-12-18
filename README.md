# haskell-project

This is a group coursewrok of module ECS713P Functional Programming.  
Group members:  
**Kang Lyujun**[@ec21009](https://github.research.its.qmul.ac.uk/ec21109)  
**Yang Fanyuqian**[@jp2016213454](https://github.research.its.qmul.ac.uk/jp2016213454)  
**Zheng Qingyuan**[@jp2016213096](https://github.research.its.qmul.ac.uk/jp2016213096)  
**Zhou Yixun**[@jp2016213369](https://github.research.its.qmul.ac.uk/jp2016213369)  

### Data  

Our [data](https://datahub.io/core/population/r/0.html) is about population for countries, regions and the world.  

### Main  features  

1. **Download data**: This function downloads the population data all around the world, parses it into out formats and stores it in database finally. Besides, all original data including .json and .sqlite files are stored in path ‘data/’.  

2. **Search by country name**: This function queries all the population data by the country name which is typed by the user. The output will be displayed on screen and be stored as a .csv file in the ‘out/country_name.scv’.  

   Examples input: China, United Kingdom, Euro area, South Asia. Etc.

3. **Search by year (1960-2018)**: This function queries all the population in the given year according to the user input. The output will be displayed on screen and be stored as a .csv file in the ‘out/year.scv’.  

4. **Search by year and order data (1960-2018)**: This function queries all the population in the given year in order according to the user input. The output will be displayed on screen and be stored as a .csv file in the ‘out/year_order.scv’.  

5. **Search world total population by year**: This function queries the total population of all countries in the world for the given year.  

6. **Insert new data to database (Using admin code: ec21)**: This function aims at inserting new population data into database by valid admin code(ec21).  

