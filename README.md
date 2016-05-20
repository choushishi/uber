# uber

This is the DOC



## Architecture

```
Uber
	
	|--theBigTable.R 			 <|

	|--weather.R -> weather.csv --|

	|- order.R -> order.csv     --|

	|--poi.R     				--|

	|...

	| test_set_1
		|some data

	| training_data
		|some data

```
## DF format

`Date, TS, Dest, Features, ...`

Example data structure:

```
 Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	190080 obs. of  6 variables:
 $ Date       : chr  "2016-01-01" "2016-01-01" "2016-01-01" "2016-01-01" ...
 $ TS         : int  1 2 3 4 5 6 7 8 9 10 ...
 $ Dest       : int  1 1 1 1 1 1 1 1 1 1 ...
 $ Weather    : num  1 1 1 1 1 1 1 1 1 1 ...
 $ temperature: num  3.5 3 3 3 3 3 3 3 3 3 ...
 $ PM2.5      : num  177 177 177 177 177 177 177 177 177 177 ...

```

> Please make sure to match the column names