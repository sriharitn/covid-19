data temp;
		input tourist @@;
		date = intnx( 'month', '1jan2001'd, _n_-1 );
		format date monyy7.;
		datalines;
42145 67639 65836 45493 51158 45751 48326 52961 48068 38025 38798 41373 43948 44463 60429 43690 45751
41115 36738 46266 52446 51416 58111 58626 52188 54506 59914 13047  1974  6866 15107 24635 24635 31587
33133 22317 27210 30815 26180 34420 34935 34935 40085 49613 51416 53733 50128 50643 47038 61459 51931
51931 56309 46008 51158
;

run;


data alldata;
		set temp;


			if '01apr2003'd <= date <= '01jun2003'd then ls = 1;
			else ls = 0;


			if date = '01jul2003'd then tc = 1;
			else tc=0;

run;


ods html style=statistical;
ods graphics on;

proc arima data=alldata	plots = all;
      identify p = 1 var=tourist crosscorr=(ls tc);
      estimate p = 1  input = (ls  /(1)tc) method=ml;
	  forecast id=date interval=month back = 8 lead = 8 printall out=b;
run;
quit;

ods graphics off;
ods html close;

