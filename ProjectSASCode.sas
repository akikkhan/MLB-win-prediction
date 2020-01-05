PROC IMPORT OUT= Baseball
            DATAFILE= "C:\Users\akhan12\Desktop\BB2008.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

PROC PRINT DATA=Baseball;
RUN;

/*

Working with MLB dataset

* analyze each column
* check and see if they fit the normal distribution
* is there any relation between wins and other columns
* divide the teams in two groups, higher wins and lower wins. then check if two groups have the same error percentage
* find summary statistics of each column
* a guess that erros falls into a poisson distribution
* t-tests, p-value, etc...
* for ttest, you need the side-by-side box plot (just like lab 8)

*/


*******************************************************************
* analyze each column;
*ODS select BasicMeasures;
PROC UNIVARIATE DATA=Baseball;
VAR Runs;
RUN;

*ODS select BasicMeasures;
PROC UNIVARIATE DATA=Baseball;
VAR Hits;
RUN;

*ODS select BasicMeasures;
PROC UNIVARIATE DATA=Baseball;
VAR Walks;
RUN;

*ODS select BasicMeasures;
PROC UNIVARIATE DATA=Baseball;
VAR Errors;
RUN;

*ODS select BasicMeasures;
PROC UNIVARIATE DATA=Baseball;
VAR Saves;
RUN;


******************************************************************
* check and see if they fit the normal distribution;
TITLE “How Normal is the Runs Histogram?”;
Ods select Histogram ParameterEstimates GoodnessOfFit FitQuantiles Bins;
PROC UNIVARIATE DATA = Baseball;
HISTOGRAM Runs/ normal(percents=20 40 60 80 midpercents);
INSET n normal(ksdpval) / pos = ne format =6.3;
RUN;


TITLE “How Normal is the Hits Histogram?”;
Ods select Histogram ParameterEstimates GoodnessOfFit FitQuantiles Bins;
PROC UNIVARIATE DATA = Baseball;
HISTOGRAM Hits/ normal(percents=20 40 60 80 midpercents);
INSET n normal(ksdpval) / pos = ne format =6.3;
RUN;


TITLE “How Normal is the Walks Histogram?”;
Ods select Histogram ParameterEstimates GoodnessOfFit FitQuantiles Bins;
PROC UNIVARIATE DATA = Baseball;
HISTOGRAM Walks/ normal(percents=20 40 60 80 midpercents);
INSET n normal(ksdpval) / pos = ne format =6.3;
RUN;



*BLOCK START;

TITLE “How Normal is the Errors Histogram?”;
Ods select Histogram ParameterEstimates GoodnessOfFit FitQuantiles Bins;
PROC UNIVARIATE DATA = Baseball;
HISTOGRAM Errors/ normal(percents=20 40 60 80 midpercents);
INSET n normal(ksdpval) / pos = ne format =6.3;
RUN;

*THIS CAN ALSO BE DONE FOR NORMALITY TEST;
/*TITLE "Summary statistics for Errors";
PROC UNIVARIATE Data=Baseball plot;
var Errors;
RUN;*/

*BLOCK END;


TITLE “How Normal is the Saves Histogram?”;
Ods select Histogram ParameterEstimates GoodnessOfFit FitQuantiles Bins;
PROC UNIVARIATE DATA = Baseball;
HISTOGRAM Saves/ normal(percents=20 40 60 80 midpercents);
INSET n normal(ksdpval) / pos = ne format =6.3;
RUN;


**********************************************************;
TITLE ‘Scatterplot Matrix for Runs vs Wins’;
PROC GPLOT DATA = Baseball; 
	PLOT Runs * Wins;
RUN;


TITLE ‘Scatterplot Matrix for Hits vs Wins’;
PROC GPLOT DATA = Baseball; 
	PLOT Hits * Wins;
RUN;


TITLE ‘Scatterplot Matrix for Walks vs Wins’;
PROC GPLOT DATA = Baseball; 
	PLOT Walks * Wins;
RUN;


TITLE ‘Scatterplot Matrix for Errors vs Wins’;
PROC GPLOT DATA = Baseball; 
	PLOT Errors * Wins;
RUN;


TITLE ‘Scatterplot Matrix for Saves vs Wins’;
PROC GPLOT DATA = Baseball; 
	PLOT Saves * Wins;
RUN;


*********************************************************************
* divide the teams in two groups, higher wins and lower wins.
then check if two groups have the same error percentage;

****USED TO CHECK THE MIDDLE POINT****;
PROC UNIVARIATE DATA=Baseball;
VAR Wins;
RUN;
**************************************;

/*
*******WE NEED TO CREATE DATASET WITH NEW COLUMN, NOT TWO SEPARATE DATASETS;
ONE GROUP WILL HAVE TEAMS WITH NUMBER OF WINS <= 83 (83 IS THE MEDIAN), AND OTHER
GROUP WILL HAVE TEAMS MORE THAN 83 WINS. WinningTeam IS THE NEW VARIABLE
THAT WILL RECORD IF A TEAM IS 'WINNER' OR 'LOSER'
*/

*CREATING DATASET;
DATA BaseballWins;
SET Baseball;
IF (Wins <= 83)
	THEN WinningTeam = 'N';  *TEAMS CATAGORIZED AS LOSERS;
ELSE WinningTeam = 'Y';  *TEAMS CATAGORIZED AS WINNERS;
RUN;


PROC SORT DATA=BaseballWins;
BY WinningTeam;
RUN;


PROC PRINT DATA=BaseballWins;
RUN;

*COMPARISON FOR ERRORS;
TITLE "T-test comparison between Errors for different groups";
ods graphics on;
PROC TTEST Data=BaseballWins cochran ci=equal umpu;
class WinningTeam;
var Errors;
RUN;


*COMPARISON FOR SAVES;
TITLE "T-test comparison between Errors for different groups";
ods graphics on;
PROC TTEST Data=BaseballWins cochran ci=equal umpu;
class WinningTeam;
var Saves;
RUN;


*COMPARISON FOR RUNS;
TITLE "T-test comparison between Errors for different groups";
ods graphics on;
PROC TTEST Data=BaseballWins cochran ci=equal umpu;
class WinningTeam;
var Runs;
RUN;


*COMPARISON FOR HITS;
TITLE "T-test comparison between Errors for different groups";
ods graphics on;
PROC TTEST Data=BaseballWins cochran ci=equal umpu;
class WinningTeam;
var Hits;
RUN;


*COMPARISON FOR WALKS;
TITLE "T-test comparison between Errors for different groups";
ods graphics on;
PROC TTEST Data=BaseballWins cochran ci=equal umpu;
class WinningTeam;
var Walks;
RUN;





*************************************;;;;;;;;;;;;;;;;;;;
proc freq data=Baseball;
   tables Errors / out=FreqOut plots=FreqPlot(scale=percent);
run;

/* 1. Estimate the rate parameter with PROC GENMOD: 
     http://support.sas.com/kb/24/166.html */
proc genmod data=Baseball;
   model Errors = / dist=poisson;
   output out=PoissonFit p=lambda;
run;

/* 2. Compute Poisson density for estimated parameter value */
/* 2.1 Create macro variable with parameter estimate */ 
data _null_;
set PoissonFit;
call symputx("Lambda", Lambda);
stop;
run;
 
/* 2.2 Use PDF function for range of x values */
data PMF;
do t = 67 to 117; /* 0 to max(x) */
   Y = pdf("Poisson", t, &Lambda);
   output;
end;
run;

/* 3. Use bar chart to plot data. To overlay a bar chart and 
      scatter plot, use the VBARPARM stmt instead of VBAR. */
data Discrete;
merge FreqOut PMF;
Prop = Percent / 100; /* convert to same scale as PDF */
run;
 
/* 3.2 Overlay VBARPARM and scatter plot of (x, pdf(x)) */
proc sgplot data=Discrete; /* VBARPARM is SAS 9.3 stmt */
   vbarparm category=Errors response=Prop / legendlabel='Sample';
   scatter x=T y=Y / legendlabel='PMF'
      markerattrs=GraphDataDefault(symbol=CIRCLEFILLED size=10);
   title "Emails per 30-Minute Period and Poisson Distribution";
run;
