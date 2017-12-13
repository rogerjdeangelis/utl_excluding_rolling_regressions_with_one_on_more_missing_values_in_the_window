Excluding rolling regressions with one on more missing values in the window

Original Topic: Rolling_regression_of width_4_with_conditions

The question  seems to need a little more clarity.

see
https://goo.gl/5JZJvt
https://communities.sas.com/t5/General-SAS-Programming/Rolling-regression-with-conditions/m-p/420755

INPUT
======

 SD1.HAVE  (window of width=4)
                                              RULES
                               |
   Obs       X          Y      |    HASMIS    INTERCEPT      SLOPE
                               |
     1     9.3468    23.5175   |       .         .           .
     2    10.5181    18.9444   |       .         .           .
     3    11.5574    19.4658   |       .         .           .
     4     9.9558    17.9594   |       0       33.8303     -1.33970   see below for check
                               |
     5    11.4485      .       |       1        9.3345      0.88558  * window of 4 has a missing
     6    10.2309    19.1357   |       1       11.2213      0.72130  * window of 4 has a missing
     7    12.0835    16.7304   |       1       26.9750     -0.83977  * window of 4 has a missing
     8     8.7243    18.0235   |       1       22.3025     -0.41941  * window of 4 has a missing
     9     9.5593    16.9296   |       0       20.6283     -0.28805  OK we have 4 without a missing
    10    13.3407    21.2756   |       0       12.0622      0.56535  OK we have 4 without a missing

   proc reg data=sd1.have(obs=4);     Variable     DF      Parameter
     model y=x;                       Intercept     1       33.83031  ** same as above
   run;quit;                          Slope         1       -1.33970


WORKING CODE
============

   mis<-roll_sum(is.na(have$Y), 4);
   reg<-rollapply(have, width = 4,
      function(x) coef(lm(Y ~ X, data = as.data.frame(x))),
      by.column = FALSE, align = "right");

OUTPUT
======


WORK.WANTWPS total obs=10

  Obs    HASMIS    INTERCEPT      SLOPE

    1       .         .           .
    2       .         .           .
    3       .         .           .
    4       0       33.8303     -1.33970
    5       1        9.3345      0.88558
    6       1       11.2213      0.72130
    7       1       26.9750     -0.83977
    8       1       22.3025     -0.41941
    9       0       20.6283     -0.28805
   10       0       12.0622      0.56535

*                _              _       _
 _ __ ___   __ _| | _____    __| | __ _| |_ __ _
| '_ ` _ \ / _` | |/ / _ \  / _` |/ _` | __/ _` |
| | | | | | (_| |   <  __/ | (_| | (_| | || (_| |
|_| |_| |_|\__,_|_|\_\___|  \__,_|\__,_|\__\__,_|

;

* http://support.sas.com/kb/25/008.html;
* mu1=10 mu2=20 var1=4 var2=9 rho=.5;
options validvarname=upcase;
libname sd1 "d:/sd1";
data sd1.have;
  keep x y;
  mu1=10; mu2=20; var1=4; var2=9; rho=.5;
  std1=sqrt(var1);  std2=sqrt(var2);
  c=sqrt(1-rho**2);
    do i = 1 to 10;
       x = rannor(123);
       y = rho*x+c*rannor(123);
       x = mu1 + sqrt(var1)*x;
       y = mu2 + sqrt(var2)*y;
       if i in (5) then y=.;
       output;
    end;
run;quit;

*          _       _   _
 ___  ___ | |_   _| |_(_) ___  _ __
/ __|/ _ \| | | | | __| |/ _ \| '_ \
\__ \ (_) | | |_| | |_| | (_) | | | |
|___/\___/|_|\__,_|\__|_|\___/|_| |_|

;
%utl_submit_wps64('
libname sd1 sas7bdat "d:/sd1";
options set=R_HOME "C:/Program Files/R/R-3.3.2";
libname wrk sas7bdat "%sysfunc(pathname(work))";
proc r;
submit;
source("C:/Program Files/R/R-3.3.2/etc/Rprofile.site", echo=T);
library(haven);
library(zoo);
library(RcppRoll);
have<-read_sas("d:/sd1/have.sas7bdat");
mis<-roll_sum(is.na(have$Y), 4);
reg<-rollapply(have, width = 4,
   function(x) coef(lm(Y ~ X, data = as.data.frame(x))),
   by.column = FALSE, align = "right");
want<-cbind(rep(NA,3),rep(NA,3),rep(NA,3));
want<-as.data.frame(rbind(want,cbind(mis,reg)));
colnames(want)<-c("HASMIS","INTERCEPT","SLOPE");
endsubmit;
import r=want data=wrk.wantwps;
run;quit;
');


WORK.WANTWPS total obs=10

  Obs    HASMIS    INTERCEPT      SLOPE

    1       .         .           .
    2       .         .           .
    3       .         .           .
    4       0       33.8303     -1.33970
    5       1        9.3345      0.88558
    6       1       11.2213      0.72130
    7       1       26.9750     -0.83977
    8       1       22.3025     -0.41941
    9       0       20.6283     -0.28805
   10       0       12.0622      0.56535



