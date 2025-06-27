
%macro combine_trial;
%let min=1;
%let max=140;
%do k=&min %to &max;

data asthma_a.patient_cov_tr&k.;
set asthma_a.patient_cov_tr&k.;
	drop usualgpstaffid emis_ddate patienttypeid;
	source="AURUM";
run;

data asthma_g.patient_cov_tr&k.;
set asthma_g.patient_cov_tr&k.;
	drop vmid marital famnum CHSreg CHSdate prescr capsup frd regstat reggap internal tod toreason;
	rename crd=regstartdate;
	rename deathdate=cprd_ddate;
	rename accept=accpetable;
	rename medcode=medcodeid;
	rename prodcode=prodcodeid;
	rename qty=quantity;
	source="GOLD";
run;

data asthma_c.patient_cov_tr&k.;
set asthma_a.patient_cov_tr&k. asthma_g.patient_cov_tr&k.;
run;

%end;
%mend;

%combine_trial;

data asthma_c.patient_tr_all;
set asthma_c.patient_cov_tr1-asthma_c.patient_cov_tr140;
run;

proc sql;
    create table asthma_c.Count_all as
    select catx('_', 'group', exp) as Group, 
           count(patid) as IDCount,
           sum(o_parksen) as NumParkin,  /* Count non-missing values of o_parkin */
           mean(fu_parksen)/365.25 as AvgFuPark     /* Calculate average of fu_park */
    from asthma_c.patient_tr_all
    group by exp;
quit;

proc sql;
    create table asthma_c.Count_tr_uniq as
    select catx('_', 'group', exp) as Group, 
           count(distinct patid) as UniqueIDCount 
    from asthma_c.patient_tr_all
    group by exp
    union all
    select 'Total' as Group, 
           count(distinct patid) as UniqueIDCount 
    from asthma_c.patient_tr_all;
quit;

/*ps-matching macro*/
%MACRO MATCH(CASE=,CONTROL=,IDCA=,IDCO=,MVARS=,WTS=,DMAXK=,DMAX=,
             NCONTLS=, TIME=,
             METHOD=,SEEDCA=,SEEDCO=,MAXITER=100000,PRINT=y,
             OUT=__OUT,OUTNMCA=__NMCA,OUTNMCO=__NMCO,MINCONT=,MAXCONT=);

   %LET BAD=0;
   %IF %LENGTH(&CASE)=0 %THEN %DO;
      %PUT ERROR: NO CASE DATASET SUPPLIED;
      %LET BAD=1;
   %END;
   %IF %LENGTH(&CONTROL)=0 %THEN %DO;
      %PUT ERROR: NO CONTROL DATASET SUPPLIED;
      %LET BAD=1;
   %END;
   %IF %LENGTH(&IDCA)=0 %THEN %DO;
      %PUT ERROR: NO IDCA VARIABLE SUPPLIED;
      %LET BAD=1;
   %END;
   %IF %LENGTH(&IDCO)=0 %THEN %DO;
      %PUT ERROR: NO IDCO VARIABLE SUPPLIED;
      %LET BAD=1;
   %END;
   %IF %LENGTH(&MVARS)=0 %THEN %DO;
      %PUT ERROR: NO MATCHING VARIABLES SUPPLIED;
      %LET BAD=1;
   %END;
   %IF %LENGTH(&WTS)=0 %THEN %DO;
      %PUT ERROR: NO WEIGHTS SUPPLIED;
      %LET BAD=1;
   %END;
   %IF %UPCASE(&METHOD)=GREEDY %THEN %DO;
      %IF %LENGTH(&SEEDCA)=0 %THEN %DO;
         %PUT ERROR: NO SEEDCA VALUE SUPPLIED;
         %LET BAD=1;
      %END;
      %IF %LENGTH(&SEEDCO)=0 %THEN %DO;
         %PUT ERROR: NO SEEDCO VALUE SUPPLIED;
         %LET BAD=1;
      %END;
   %END;
   %IF %LENGTH(&OUT)=0 %THEN %DO;
      %PUT ERROR: NO OUTPUT DATASET SUPPLIED;
      %LET BAD=1;
   %END;
   %IF %UPCASE(&METHOD)^=GREEDY & %UPCASE(&METHOD)^=OPTIMAL %THEN %DO;
      %PUT ERROR: METHOD MUST BE GREEDY OR OPTIMAL;
      %LET BAD=1;
   %END;
   %IF (&MINCONT=  AND &MAXCONT^= ) OR (&MINCONT^=  AND &MAXCONT= )
   %THEN %DO;
      %PUT ERROR: MINCONT AND MAXCONT MUST BOTH BE SPECIFIED;
      %LET BAD=1;
   %END;
   %LET NVAR=0;
   %DO %UNTIL(%SCAN(&MVARS,&NVAR+1,' ')= );
      %LET NVAR=%EVAL(&NVAR+1);
   %END;
   %LET NWTS=0;
   %DO %UNTIL(%QSCAN(&WTS,&NWTS+1,' ')= );
      %LET NWTS=%EVAL(&NWTS+1);
   %END;
   %IF &NVAR^= &NWTS %THEN %DO;
      %PUT ERROR: #VARS MUST EQUAL #WTS;
      %LET BAD=1;
   %END;
   %LET NK=0;
   %IF %QUOTE(&DMAXK)^=  %THEN %DO %UNTIL(%QSCAN(&DMAXK,&NK+1,' ')= );
      %LET NK=%EVAL(&NK+1);
   %END;
   %IF &NK>&NVAR %THEN %LET NK=&NVAR;
   %DO I=1 %TO &NVAR;
      %LET V&I=%SCAN(&MVARS,&I,' ');
   %END;
   %IF &NWTS>0 %THEN %DO;
        DATA _NULL_;
        %DO I=1 %TO &NWTS;
             %LET W&I=%SCAN(&WTS,&I,' ');
             IF &&W&I<0 THEN DO;
                  PUT 'ERROR: WEIGHTS MUST BE NON-NEGATIVE';
                  CALL SYMPUT('BAD','1');
             END;
        %END;
        RUN;
   %END;
   %IF &NK>0 %THEN %DO;
        DATA _NULL_;
        %DO I=1 %TO &NK;
             %LET K&I=%SCAN(&DMAXK,&I,' ');
             IF &&K&I<0 THEN DO;
                  PUT 'ERROR: DMAXK VALUES MUST BE NON-NEGATIVE';
                  CALL SYMPUT('BAD','1');
             END;
        %END;
        RUN;
   %END;
   %MACRO DIJ;
      %DO I=1 %TO &NVAR-1;
         &&W&I*ABS(__CA&I-__CO&I) +
      %END;
      &&W&NVAR*ABS(__CA&NVAR-__CO&NVAR);
   %MEND DIJ;
   %MACRO MAX1;
      %IF &DMAX^= %THEN %DO;
         & __D<=&DMAX
      %END;
      %DO I=1 %TO &NK;
         & ABS(__CA&I-__CO&I)<=&&K&I
      %END;
   %MEND MAX1;
   %MACRO MAX2;
      %IF &DMAX= & &NK=0 %THEN %DO;
         %IF &time^= %then %do;
            if __cotime>__catime then
         %end;
         output;
      %end;
      %IF &DMAX^= & &NK=0 %THEN %DO;
         IF _COST_<=&DMAX
         %if &time^= %then %do;
            & __cotime>__catime
         %end;
         THEN OUTPUT;
      %END;
      %IF &DMAX= & &NK>0 %THEN %DO;
         IF ABS(__CA1-__CO1)<=&K1
         %DO I=2 %TO &NK;
            & ABS(__CA&I-__CO&I)<=&&K&I
         %END;
         %if &time^= %then %do;
            & __cotime>__catime
         %end;
         THEN OUTPUT;
      %END;
      %IF &DMAX^= & &NK>0 %THEN %DO;
         IF _COST_<=&DMAX
         %DO I=1 %TO &NK;
            & ABS(__CA&I-__CO&I)<=&&K&I
         %END;
         %if &time^= %then %do;
            & __cotime>__catime
         %end;
         THEN OUTPUT;
      %END;
   %MEND MAX2;
   %MACRO LBLS;
      %DO I=1 %TO &NVAR;
         __CA&I="&&V&I/CASE"
         __CO&I="&&V&I/CONTROL"
         __DIF&I="&&V&I/ABS. DIFF "
         __WT&I="&&V&I/WEIGHT"
      %END;
   %MEND LBLS;
   %MACRO VBLES;
      %DO I=1 %TO &NVAR;
         __DIF&I
      %END;
      %DO I=1 %TO &NVAR;
         __CA&I __CO&I
      %END;
   %MEND VBLES;
   %MACRO GREEDY;
    %GLOBAL BAD2;
      DATA __CASE; SET &CASE;
           %DO I=1 %TO &NVAR;
                %LET MISSTEST=%SCAN(&MVARS,&I,' ');
                IF &MISSTEST=. THEN DELETE;
           %END;
           %IF &TIME^= %THEN %DO;
                IF &TIME=. THEN DELETE;
           %END;
      DATA __CASE; SET __CASE END=EOF;
       KEEP __IDCA __CA1-__CA&NVAR __R &mvars
       %if &time^= %then %do;
             __catime
          %end;
          ;
         __IDCA=&IDCA;
         %if &time^= %then %do;
            __catime=&time;
         %end;
         %DO I=1 %TO &NVAR;
            __CA&I=&&V&I;
         %END;
         SEED=&SEEDCA;
         __R=RANUNI( SEED  );
         IF EOF THEN CALL SYMPUT('NCA',_N_);
      PROC SORT; BY __R __IDCA;
      DATA __CONT; SET &CONTROL;
           %DO I=1 %TO &NVAR;
                %LET MISSTEST=%SCAN(&MVARS,&I,' ');
                IF &MISSTEST=. THEN DELETE;
           %END;
           %IF &TIME^= %THEN %DO;
                IF &TIME=. THEN DELETE;
           %END;
      DATA __CONT; SET __CONT END=EOF;
       KEEP __IDCO __CO1-__CO&NVAR __R &mvars
        %if &time^= %then %do;
           __cotime
        %end;
        ;
         __IDCO=&IDCO;
         %if &time^= %then %do;
            __cotime=&time;
         %end;
         %DO I=1 %TO &NVAR;
            __CO&I=&&V&I;
         %END;
         SEED=&SEEDCO;
         __R=RANUNI( SEED  );
         IF EOF THEN CALL SYMPUT('NCO',_N_);
      RUN;
      %LET BAD2=0;
      %IF &NCO < %EVAL(&NCA*&NCONTLS) %THEN %DO;
         %PUT ERROR: NOT ENOUGH CONTROLS TO MAKE REQUESTED MATCHES;
         %LET BAD2=1;
      %END;
      %IF &BAD2=0 %THEN %DO;
         PROC SORT; BY __R __IDCO;
         DATA __MATCH;
          KEEP __IDCA __CA1-__CA&NVAR __DIJ __MATCH __CONT_N
          %if &time^= %then %do;
             __catime __cotime
          %end;
          ;
          ARRAY __USED(&NCO) $ 1 _TEMPORARY_;
            DO __I=1 TO &NCO;
               __USED(__I)='0';
            END;
            DO __I=1 TO &NCONTLS;
               DO __J=1 TO &NCA;
                  SET __CASE POINT=__J;
                  __SMALL=.;
                  __MATCH=.;
                  DO __K=1 TO &NCO;
                     IF __USED(__K)='0' THEN DO;
                        SET __CONT POINT=__K;
                        __D=%DIJ
                        IF __d^=. & (__SMALL=. | __D<__SMALL) %MAX1
                        %if &time^= %then %do;
                           & __cotime > __catime
                        %end;
                        THEN DO;
                           __SMALL=__D;
                           __MATCH=__K;
                           __DIJ=__D;
                           __CONT_N=__I;
                        END;
                     END;
                  END;
                  IF __MATCH^=. THEN DO;
                     __USED(__MATCH)='1';
                     OUTPUT;
                  END;
               END;
            END;
            STOP;
         DATA &OUT;
          SET __MATCH;
          SET __CONT POINT=__MATCH;
          KEEP __IDCA __IDCO __CONT_N __DIJ __CA1-__CA&NVAR
               __CO1-__CO&NVAR __DIF1-__DIF&NVAR __WT1-__WT&NVAR
               %if &time^= %then %do;
                  __catime __cotime
               %end;
        ;
          LABEL __IDCA="&IDCA/CASE"
                __IDCO="&IDCO/CONTROL"
                %if &time^= %then %do;
                   __catime="&time/CASE"
                   __cotime="&time/CONTROL"
                %end;
                __CONT_N='CONTROL/NUMBER'
                __DIJ='DISTANCE/D_IJ'
                %LBLS;
             %DO I=1 %TO &NVAR;
                __DIF&I=abs(__CA&I-__CO&I);
                __WT&I=&&W&I;
             %END;
      %END;
   %MEND GREEDY;
   %MACRO OPTIMAL;
    %GLOBAL BAD2;
      DATA __CASE; SET &CASE;
           %DO I=1 %TO &NVAR;
                %LET MISSTEST=%SCAN(&MVARS,&I,' ');
                IF &MISSTEST=. THEN DELETE;
           %END;
           %IF &TIME^= %THEN %DO;
                IF &TIME=. THEN DELETE;
           %END;
      DATA __CASE; SET __CASE END=EOF;
       KEEP __IDCA __CA1-__CA&NVAR &mvars
         %if &time^= %then %do;
            __catime
         %end;
         ;
         __IDCA=&IDCA;
         %if &time^= %then %do;
            __catime=&time;
         %end;
         %DO I=1 %TO &NVAR;
            __CA&I=&&V&I;
         %END;
         IF EOF THEN CALL SYMPUT('NCA',_N_);
      DATA __CONT; SET &CONTROL;
           %DO I=1 %TO &NVAR;
                %LET MISSTEST=%SCAN(&MVARS,&I,' ');
                IF &MISSTEST=. THEN DELETE;
           %END;
           %IF &TIME^= %THEN %DO;
                IF &TIME=. THEN DELETE;
           %END;
      DATA __CONT; SET __CONT END=EOF;
       KEEP __IDCO __CO1-__CO&NVAR &mvars
         %if &time^= %then %do;
            __cotime
         %end;
         ;
         __IDCO=&IDCO;
         %if &time^= %then %do;
            __cotime=&time;
         %end;
         %DO I=1 %TO &NVAR;
            __CO&I=&&V&I;
         %END;
         IF EOF THEN CALL SYMPUT('NCO',_N_);
      RUN;
      %LET BAD2=0;
      %IF &NCO < %EVAL(&NCA*&NCONTLS) %THEN %DO;
         %PUT ERROR: NOT ENOUGH CONTROLS TO MAKE REQUESTED MATCHES;
         %LET BAD2=1;
      %END;
      %IF &BAD2=0 %THEN %DO;
         DATA __DIST1;
          SET __CASE;
          LENGTH __FROM __TO $ 80;
            DO I=1 TO &NCO;
               SET __CONT POINT=I;
               _COST_=%DIJ;
               __FROM=left(__IDCA);
               __TO=left(trim(__IDCO) || '_co');
               _CAPAC_=1;
               IF _COST_^=. THEN DO;
                  %MAX2
               END;
            END;
            DATA __GOODCO;
             SET __DIST1;
             KEEP __IDCO;
            PROC SORT; BY __IDCO;
            DATA __GOODCO;
             SET __GOODCO; BY __IDCO;
               IF FIRST.__IDCO;
            data _null_;
               i=1;
               set __goodco point=i nobs=n;
               call symput('newcont',n);
               stop;
            DATA __DIST2;
             LENGTH __FROM __TO $ 80;
               DO I=1 TO N;
                  SET __GOODCO POINT=I NOBS=N;
                  __FROM=left(trim(__IDCO) || '_co');
                  __TO='SK';
                  _COST_=0;
                  _CAPAC_=1;
                  OUTPUT;
               END;
            STOP;
            DATA __GOODCA;
             SET __DIST1;
             KEEP __IDCA;
            PROC SORT; BY __IDCA;
            DATA __GOODCA;
             SET __GOODCA; BY __IDCA;
               IF FIRST.__IDCA;
            DATA __DIST3;
             LENGTH __FROM __TO $ 80;
               DO I=1 TO N;
                  SET __GOODCA POINT=I NOBS=N;
                  __FROM='SC';
                  __TO=left(__idca);
                  _COST_=0;
                  %if &mincont= %then %do;
                     _CAPAC_=&NCONTLS;
                  %end;
                  %else %do;
                     _capac_=&mincont;
                  %end;
                  OUTPUT;
               END;
               %if &mincont^= %then %do;
                  __from='SC';
                  __to='EXTRA';
                  _capac_=&newcont-&mincont*n;
                  _cost_=0;
                  output;
                  do i=1 to n;
                     set __goodca point=i;
                     __from='EXTRA';
                     __to=left(__idca);
                     _cost_=0;
                     _capac_=&maxcont-&mincont;
                     output;
                  end;
               %end;
               CALL SYMPUT('NEWCASE',N);
            STOP;
            DATA __DIST;
             SET __DIST1 __DIST2 __DIST3;
         %LET DEM=%EVAL(&NEWCASE*&NCONTLS);
         PROC NETFLOW
            MAXIT1=&MAXITER
            %if &mincont= %then %do;
               DEMAND=&DEM
            %end;
            %else %do;
               demand=&newcont
            %end;
            SOURCENODE='SC'
            SINKNODE='SK'
            ARCDATA=__DIST
            ARCOUT=__MATCH;
          TAIL __FROM;
          HEAD __TO;
         DATA __OUT;
          SET __MATCH;
            IF _FLOW_>0 & __FROM^in ('SC' 'EXTRA') & __TO^='SK';
            __DIJ=_FCOST_;
            %DO I=1 %TO &NVAR;
               __DIF&I=abs(__CA&I-__CO&I);
               __WT&I=&&W&I;
            %END;
         PROC SORT; BY __IDCA __DIJ;
         DATA &OUT;
          SET __OUT; BY __IDCA;
            drop __from -- _status_;
            IF FIRST.__IDCA THEN __CONT_N=0;
            __CONT_N+1;
          LABEL __IDCA="&IDCA/CASE"
                __IDCO="&IDCO/CONTROL"
                %if &time^= %then %do;
                   __catime="&time/CASE"
                   __cotime="&time/CONTROL"
                %end;
                __CONT_N='CONTROL/NUMBER'
                __DIJ='DISTANCE/D_IJ'
                %LBLS;
      %END;
   %MEND OPTIMAL;
   %IF &BAD=0 %THEN %DO;
      %IF %UPCASE(&METHOD)=GREEDY %THEN %DO;
         %GREEDY
      %END;
      %ELSE %DO;
         %OPTIMAL
      %END;
      %IF &BAD2=0 %THEN %DO;
         PROC SORT DATA=&OUT; BY __IDCA __CONT_N;
         proc sort data=__case; by __IDCA;
         data &outnmca; merge __case
              &out(in=__inout where=(__cont_n=1)); by __idca;
              if __inout=0; **non-matches;

         proc sort data=__cont; by __IDCO;
         proc sort data=&out; by __IDCO;
         data &outnmco; merge __cont
              &out(in=__inout); by __idco;
              if __inout=0; **non-matched controls;
         proc sort data=&out; by __IDCA; **re-sort by case id;

         %if %upcase(&print)=Y %then %do;
         PROC PRINT data=&out LABEL SPLIT='/';
          VAR __IDCA __IDCO __CONT_N
          %if &time^= %then %do;
             __catime __cotime
          %end;
          __DIJ %VBLES;
          sum __dij;
         title9'Data listing for matched cases and controls';
         footnote
    "match macro: case=&case control=&control idca=&idca idco=&idco";
         footnote2
"   mvars=&mvars  wts=&wts dmaxk=&dmaxk dmax=&dmax ncontls=&ncontls";
         %if &time^= %then %do;
  footnote3"time=&time  method=&method  seedca=&seedca  seedco=&seedco";
         %end;
         %else %do;
           footnote3"   method=&method  seedca=&seedca  seedco=&seedco";
         %end;
         footnote4"   out=&out   outnmca=&outnmca  outnmco=&outnmco";
         run;
         title9'Summary data for matched cases and controls';
         proc means data=&out n mean sum min max; class __cont_n;
          var __dij
           %if &nvar >=2 %then %do; __dif1-__dif&nvar  __ca1-__ca&nvar
                             %if &time^= %then %do;
                                __catime
                             %end;
                             __co1-__co&nvar
                             %if &time^= %then %do;
                                __cotime
                             %end;
                             ;
           %end;
           %else %do;
                             __dif1 __ca1
                             %if &time^= %then %do;
                                __catime
                             %end;
                             __co1
                             %if &time^= %then %do;
                                __cotime
                             %end;
                             ;
           %end;
         run;
         proc means data=&outnmca n mean sum min max; var &mvars;
         title9'Summary data for NON-matched cases';
         run;
         proc means data=&outnmco n mean sum min max; var &mvars;
         title9'Summary data for NON-matched controls';
         run;
         %end;
      %END;
   %END;
    title9; footnote;
    run;
%MEND MATCH;

%macro seq_mtch;
%let min=1;
%let max=140;
%do j=&min %to &max;

/*calculate PS from your dataset*/
proc logistic descending data=asthma_c.patient_cov_tr&j.;
	class exp gender calendar 
cov1 cov2 cov3 cov4 cov5 cov6 cov13 cov14
cov15 cov16 cov17 cov18 cov19 cov20 cov21 cov22 cov23 cov24 cov25 cov26 cov27 cov28 cov29
cov30 cov31 cov32 cov33 cov34 cov35 cov36 cov37 cov38 cov39 cov40 cov41 cov42 cov43 cov44
cov45 cov46 cov47 cov48 cov49 cov50;
	model exp = age gender calendar 
dur cov9 cov10 cov11 age*age dur*dur cov9*cov9 cov10*cov10 cov11*cov11
cov1 cov2 cov3 cov4 cov5 cov6 cov13 cov14
cov15 cov16 cov17 cov18 cov19 cov20 cov21 cov22 cov23 cov24 cov25 cov26 cov27 cov28 cov29
cov30 cov31 cov32 cov33 cov34 cov35 cov36 cov37 cov38 cov39 cov40 cov41 cov42 cov43 cov44
cov45 cov46 cov47 cov48 cov49 cov50;
	output out=out_ps prob=ps xbeta=logit_ps;
run;

/*data manipulation*/
data case;
set out_ps;
	if exp=1 ;
	keep patid ps;
run;

data case1;
set case;
	caseid=_n_;
run;

data control;
set out_ps;
	if exp=0 ;
	keep patid ps;
run;

data control1;
set control;
	ctlid=_n_;
run;

%match(case=case1,control=control1,idca=caseid,idco=ctlid,
              mvars=ps,maxiter=100000,
              wts=1, dmaxk=0.05,out=mtch,ncontls=1, /*weight, caliper, matching ratio*/
              method=greedy, seedco=123, seedca=321);


/*data cleaning*/
data mtchall;set mtch;run;

data mtchcase;set mtchall;
rename __IDCA=caseid;
keep  __IDCA ;
run;
proc sort data=mtchall;by __IDCA;run;

data mtchctl;set mtchall;
rename __IDCO=ctlid;
keep  __IDCO  ;
run;
proc sort data=mtchctl;by ctlid;run;

proc sort data=case1;by caseid;run;
proc sort data=mtchcase;by caseid;run;

data mtchcase1;merge mtchcase(in=inmtchcase) case1;
by caseid;
if inmtchcase;
keep patid ;run;

proc sort data=mtchcase1;by patid;run;
data mtchcase1f;set mtchcase1;
by patid;
if first.patid;
group=1;
run;

proc sort data=control1;by ctlid;run;
proc sort data=mtchctl;by ctlid;run;
data mtchctl1;merge mtchctl(in=inmtchctl) control1;
by ctlid;
if inmtchctl;
keep patid ;
run;

proc sort data=mtchctl1;by patid;run;
data mtchctl1f;set mtchctl1;
by patid;
if first.patid;
group=0;
run;

data mtchfinal;set mtchcase1f mtchctl1f;
run;

proc sql;
create table psmtch_analysis_tr&j. as select * from out_ps where patid in (select patid from mtchfinal);
quit;

proc sql;
    create table mtchID as
    select distinct a.__IDCA,a.__IDCO,b.patid as patid_CA,c.patid as patid_CO
    from mtchall as a
    left join case1 as b 
        on a.__IDCA = b.caseid
	left join control1 as c 
        on a.__IDCO = c.ctlid;
quit;

proc sql;
    create table psmtch_analysis_tr&j. as
    select distinct a.*,b.patid_CA as patid_mtch
    from psmtch_analysis_tr&j. as a
    left join mtchID as b 
        on a.patid = b.patid_CO;
quit;

data asthma_c.psmtch_analysis_tr&j.;
set psmtch_analysis_tr&j.;
	if patid_mtch="" then patid_mtch=patid;
run;

%end;
%mend;

%seq_mtch;

%macro label;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_c.psmtch_analysis_tr&i.;
set asthma_c.psmtch_analysis_tr&i.;
	trial=&i.;
run;

%end;
%mend;

%label;

data asthma_c.psmtch_analysis_all;
set asthma_c.psmtch_analysis_tr1-asthma_c.psmtch_analysis_tr140;
run;
proc phreg data=asthma_c.psmtch_analysis_all;
    class exp (ref="0") patid;
    model fu_parksen*o_parksen(0) = exp;
run;
proc freq data=asthma_c.psmtch_analysis_all;
	tables exp*o_parksen exp*o_parkin exp*o_parkgp exp*o_parkhosp;
run;
proc freq data=asthma_c.psmtch_analysis_all;
	tables exp*o_anxi exp*o_depress exp*o_psycho exp*o_sleep exp*o_death;
run;

data asthma_c.psmtch_analysis_all;
set asthma_c.psmtch_analysis_all;
	if o_death=1 or o_parksen=1 then o_comp=1;
	else o_comp=0;
run;
proc freq data=asthma_c.psmtch_analysis_all;
	tables exp*o_comp;
run;
proc sql;
    create table asthma_c.Count_mtch_uniq as
    select catx('_', 'group', exp) as Group, 
           count(distinct patid) as UniqueIDCount 
    from asthma_c.psmtch_analysis_all
    group by exp
    union all
    select 'Total' as Group, 
           count(distinct patid) as UniqueIDCount 
    from asthma_c.psmtch_analysis_all;
quit;
proc sql;
    create table asthma_c.Count_all_mtch as
    select catx('_', 'group', exp) as Group, 
           count(patid) as IDCount,
           sum(o_parksen) as NumParkin,  /* Count non-missing values of o_parkin */
           mean(fu_parksen)/365.25 as AvgFuPark     /* Calculate average of fu_park */
    from asthma_c.psmtch_analysis_all
    group by exp;
quit;

%macro describe_trials;
%do i = 1 %to 140;
proc sql;
    create table asthma_c.Count_uniq_&i. as
    select catx('_', 'group', exp) as Group, 
           count(distinct patid) as UniqueIDCount,
           sum(o_parksen) as NumParkin,  /* Count non-missing values of o_parkin */
           mean(fu_parksen)/365.25 as AvgFuPark     /* Calculate average of fu_park */
    from asthma_c.patient_cov_tr&i.
    group by exp;
quit;
%end;
%mend;
%describe_trials;
%macro merge_datasets;
data asthma_c.All_Counts;
    length Table_Number $5;
    retain Table_Number;
    %do i = 1 %to 140;
        set asthma_c.Count_uniq_&i. (in=a);
        if a then Table_Number = &i.;
        output;
    %end;
run;
%mend;
%merge_datasets;
proc sort data=asthma_c.All_Counts;
	by Table_Number;
run;

%macro describe_trials_mtch;
%do i = 1 %to 140;
proc sql;
    create table asthma_c.Count_mtch_uniq_&i. as
    select catx('_', 'group', exp) as Group, 
           count(distinct patid) as UniqueIDCount,
           sum(o_parksen) as NumParkin,  /* Count non-missing values of o_parkin */
           mean(fu_parksen)/365.25 as AvgFuPark     /* Calculate average of fu_park */
    from asthma_c.psmtch_analysis_tr&i.
    group by exp;
quit;
%end;
%mend;
%describe_trials_mtch;
%macro merge_mtch;
data asthma_c.All_Counts_mtch;
    length Table_Number $5;
    retain Table_Number;
    %do i = 1 %to 140;
        set asthma_c.Count_mtch_uniq_&i. (in=a);
        if a then Table_Number = &i.;
        output;
    %end;
run;
%mend;
%merge_mtch;
proc sort data=asthma_c.All_Counts_mtch;
	by Table_Number;
run;

%macro separate;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_a.psmtch_analysis_tr&i. asthma_g.psmtch_analysis_tr&i.;
set asthma_c.psmtch_analysis_tr&i.;
	if source = "AURUM" then output asthma_a.psmtch_analysis_tr&i. ;
	if source = "GOLD" then output asthma_g.psmtch_analysis_tr&i. ;
run;
%end;
%mend;

%separate;

%macro anxi_baseline;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table anxi_tr&i. as 
	select * from asthma_g.anxi_dx 
	where patid in (select distinct patid from asthma_g.psmtch_analysis_tr&i.)
	order by patid,eventdate;
quit;
data anxi_tr&i. (keep=patid anxidate);
set anxi_tr&i.;
	by patid;
		if first.patid;
			rename eventdate=anxidate;
run;
data indexdate;
set asthma_g.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql; 
	create table cov_anxi_tr&i. as
	select distinct *
	from indexdate as a
	left join anxi_tr&i. as b on a.patid=b.patid
	order by patid,anxidate;
quit;
data cov_anxi_tr&i.;
set cov_anxi_tr&i.;
	if anxidate<=indexdate and anxidate~=.;
run;
data cov_anxi_tr&i.;
set cov_anxi_tr&i.;
	by patid;
		if first.patid;
		cov_anxi=1;
run;
data cov_anxi_tr&i.;
set cov_anxi_tr&i.;
	keep patid cov_anxi;
run;
proc sql;
	create table asthma_g.psmtch_analysis_tr&i. as
	select *
	from asthma_g.psmtch_analysis_tr&i. as a
	left join cov_anxi_tr&i. as b on a.patid=b.patid;
quit;

%end;
%mend;

%anxi_baseline;

%macro cum_dose;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	if dod~=. and dod<=enddate then o_death=1;
		else o_death=0;
		fu_death=enddate-indexdate;
run;

/*Long data format*/
data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	surv_death=floor(fu_death/91.3125);
	surv_parkin=floor(fu_parkin/91.3125)-1;
	surv_parkhosp=floor(fu_parkhosp/91.3125)-1;
	surv_parksen=floor(fu_parksen/91.3125)-1;
	surv_parkgp=floor(fu_parkgp/91.3125)-1;
	surv_anxi=floor(fu_anxi/91.3125)-1;
	surv_sleep=floor(fu_sleep/91.3125)-1;
	surv_depress=floor(fu_depress/91.3125)-1;
	surv_psycho=floor(fu_psycho/91.3125)-1;
run;

* Expanding dataset;
data asthma_a.psmtch_surv_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	do loop= 0 to surv_death;
		output;
	end;
run;

* Creating variable for time;
proc sort data=asthma_a.psmtch_surv_tr&i.;
	by patid;
run;
data asthma_a.psmtch_surv_tr&i.;
set asthma_a.psmtch_surv_tr&i.;
	retain t;
	by patid;
	if first.patid then t=0;
	else t=t+1;
run;

* Creating event variable;
data asthma_a.psmtch_surv_tr&i.;
set asthma_a.psmtch_surv_tr&i.;
	ev_death=(o_death=1 and t=surv_death);
	ev_parkin=(o_parkin=1 and t=surv_parkin);
	ev_parkhosp=(o_parkhosp=1 and t=surv_parkhosp);
	ev_parksen=(o_parksen=1 and t=surv_parksen);
	ev_parkgp=(o_parkgp=1 and t=surv_parkgp);
	ev_anxi=(o_anxi=1 and t=surv_anxi);
	ev_sleep=(o_sleep=1 and t=surv_sleep);
	ev_depress=(o_depress=1 and t=surv_depress);
	ev_psycho=(o_psycho=1 and t=surv_psycho);
run;
proc sql;
 	create table LTRA_Rx_tr&i. as 
	select distinct * from asthma_a.LTRA_Rx 
	where patid in (select patid from asthma_a.psmtch_analysis_tr&i.);
quit;
data indexdate_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table LTRA_Rx_tr&i. as
	select *
	from LTRA_Rx_tr&i. as a
	left join indexdate_tr&i. as b on a.patid=b.patid
	where indexdate<=issuedate
	order by patid,issuedate;
quit;
data LTRA_Rx_tr&i. (keep=patid t ltra);
set LTRA_Rx_tr&i.;
	t=floor((issuedate-indexdate)/91.3125);
	ltra=1;
run;
proc sort data=LTRA_Rx_tr&i. nodupkey;
	by patid t;
run;
proc sql;
	create table asthma_a.psmtch_cumdose_tr&i. as
	select *
	from asthma_a.psmtch_surv_tr&i. as a
	left join LTRA_Rx_tr&i. as b on a.patid=b.patid and a.t=b.t
	order by patid,t;
quit;
data asthma_a.psmtch_cumdose_tr&i.;
set asthma_a.psmtch_cumdose_tr&i.;
	if ltra=. then ltra=0;
run;
data asthma_a.psmtch_cumdose_tr&i.;
set asthma_a.psmtch_cumdose_tr&i.;
	by patid;
	retain cum_LTRA;
	if first.patid then cum_LTRA = LTRA;
	else cum_LTRA = cum_LTRA + LTRA;
run;

%end;
%mend;

%cum_dose;

data asthma_a.psmtch_cumdose_all;
set asthma_a.psmtch_cumdose_tr1-asthma_a.psmtch_cumdose_tr140;
run;

%macro cum_dose;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	if dod~=. and dod<=enddate then o_death=1;
		else o_death=0;
		fu_death=enddate-indexdate;
run;

/*Long data format*/
data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	surv_death=floor(fu_death/91.3125);
	surv_parkin=floor(fu_parkin/91.3125)-1;
	surv_parkhosp=floor(fu_parkhosp/91.3125)-1;
	surv_parksen=floor(fu_parksen/91.3125)-1;
	surv_parkgp=floor(fu_parkgp/91.3125)-1;
	surv_anxi=floor(fu_anxi/91.3125)-1;
	surv_sleep=floor(fu_sleep/91.3125)-1;
	surv_depress=floor(fu_depress/91.3125)-1;
	surv_psycho=floor(fu_psycho/91.3125)-1;
run;

* Expanding dataset;
data asthma_g.psmtch_surv_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	do loop= 0 to surv_death;
		output;
	end;
run;

* Creating variable for time;
proc sort data=asthma_g.psmtch_surv_tr&i.;
	by patid;
run;
data asthma_g.psmtch_surv_tr&i.;
set asthma_g.psmtch_surv_tr&i.;
	retain t;
	by patid;
	if first.patid then t=0;
	else t=t+1;
run;

* Creating event variable;
data asthma_g.psmtch_surv_tr&i.;
set asthma_g.psmtch_surv_tr&i.;
	ev_death=(o_death=1 and t=surv_death);
	ev_parkin=(o_parkin=1 and t=surv_parkin);
	ev_parkhosp=(o_parkhosp=1 and t=surv_parkhosp);
	ev_parksen=(o_parksen=1 and t=surv_parksen);
	ev_parkgp=(o_parkgp=1 and t=surv_parkgp);
	ev_anxi=(o_anxi=1 and t=surv_anxi);
	ev_sleep=(o_sleep=1 and t=surv_sleep);
	ev_depress=(o_depress=1 and t=surv_depress);
	ev_psycho=(o_psycho=1 and t=surv_psycho);
run;
proc sql;
 	create table LTRA_Rx_tr&i. as 
	select distinct * from asthma_g.LTRA_Rx 
	where patid in (select patid from asthma_g.psmtch_analysis_tr&i.);
quit;
data indexdate_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table LTRA_Rx_tr&i. as
	select *
	from LTRA_Rx_tr&i. as a
	left join indexdate_tr&i. as b on a.patid=b.patid
	where indexdate<=eventdate
	order by patid,eventdate;
quit;
data LTRA_Rx_tr&i. (keep=patid t ltra);
set LTRA_Rx_tr&i.;
	t=floor((eventdate-indexdate)/91.3125);
	ltra=1;
run;
proc sort data=LTRA_Rx_tr&i. nodupkey;
	by patid t;
run;
proc sql;
	create table asthma_g.psmtch_cumdose_tr&i. as
	select *
	from asthma_g.psmtch_surv_tr&i. as a
	left join LTRA_Rx_tr&i. as b on a.patid=b.patid and a.t=b.t
	order by patid,t;
quit;
data asthma_g.psmtch_cumdose_tr&i.;
set asthma_g.psmtch_cumdose_tr&i.;
	if ltra=. then ltra=0;
run;
data asthma_g.psmtch_cumdose_tr&i.;
set asthma_g.psmtch_cumdose_tr&i.;
	by patid;
	retain cum_LTRA;
	if first.patid then cum_LTRA = LTRA;
	else cum_LTRA = cum_LTRA + LTRA;
run;

%end;
%mend;

%cum_dose;

data asthma_g.psmtch_cumdose_all;
set asthma_g.psmtch_cumdose_tr1-asthma_g.psmtch_cumdose_tr140;
run;

data asthma_c.psmtch_cumdose_all;
set asthma_g.psmtch_cumdose_all asthma_a.psmtch_cumdose_all;
run;

proc sort data=asthma_c.psmtch_cumdose_all;
	by trial patid t;
run;

data asthma_c.psmtch_cumdose_all_death; 
set asthma_c.psmtch_cumdose_all;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_parksen; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_parksen then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_parkhosp; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_parkhosp then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_parkin; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_parkin then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_parkgp; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_parkgp then delete;
	if t>47 then delete;
run;

data asthma_c.psmtch_cumdose_anxi; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_anxi then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_depress; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_depress then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_sleep; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_sleep then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_psycho; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_psycho then delete;
	if t>47 then delete;
run;

%MACRO ITT_model(in_data=, outcome=, absolute_risk=, interval=, out_data=);
/*ITT model*/
proc logistic data= &in_data. desc outmodel=outmod;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = exp|t|t;
run;

* Creating dataset to store results;
data riskset ;
	do t = 0 to 47 ;
		exp = 0 ;
		output ;
		exp = 1;
		output ;
	end;	
run;

* assignment of estimated hazard to each person-month;
proc logistic inmodel=outmod;
	score data=riskset out=results  (keep = t exp exp P_1 rename =(P_1=hazard));
run;
proc sort data = results; 
	by exp t; 
run;

* computation of survival for each person-month;
data results ;
set results ;
	by exp ;
	retain surv ;
	if first.exp then surv = 1 ;
	surv = surv * (1-hazard);
	risk = 1 - surv;
run;
proc print data=results;
	var exp t risk;
run;

* Creating separate datasets for strategy = 0 and strategy = 1;
data results0 results1 ;
set results ;
	if exp = 0 then output results0 ;
	if exp = 1 then output results1 ;
	keep t risk;
run;
		
* Merging datasets for strategy = 0 and strategy = 1;
data merge1 ;
merge results0 (rename = (risk = risk0)) results1 (rename = (risk = risk1));
	by t;
	rd = risk1 - risk0;
	rr = risk1 / risk0;
run;

* Edit data frame to reflect that risks are estimated at the END of each interval;
data zero;
	t = 0;
	risk0 = 0;
	risk1 = 0;
run;
data plot; 
set merge1; 
	t = t+1; 
run;
data plot;
merge plot zero;
	by t;
run;
data plot;
set plot;
	month=t*3;
run;
data &out_data.; set plot; run;

* Creating plot;
proc sgplot data = plot;
	step x = month y = risk0 /legendlabel='No LTRA initiation' lineattrs=(pattern=1 color=ROSE thickness=2pt);
	step x = month y = risk1 /legendlabel='LTRA initiation' lineattrs=(pattern=1 color=STEEL thickness=2pt);
	yaxis label = 'Cumulative Incidence'  values = (0 to &absolute_risk. by &interval.) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	xaxis label = 'Months'  values = (0 to 144 by 36) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	keylegend/noborder title=' ' valueattrs=(size=12 weight = bold);
run;
title;

proc logistic data= &in_data. desc outmodel=outmod_hr;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = exp t|t;
 	estimate intercept 0 exp 1 / exp;	
run;

%mend;

%MACRO AT_model(in_data=, outcome=, absolute_risk=, interval=, out_data=);
/*Cumulative dose model*/
proc logistic data= &in_data. desc outmodel=outmod;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = cum_LTRA|cum_LTRA|t|t;
run;

* Creating dataset to store results;
data riskset ;
	do t = 0 to 47 ;
		exp = 0 ;
		cum_LTRA = 0 ;
		output ;
		exp = 1;
		cum_LTRA = t+1 ;
		output ;
	end;	
run;

* assignment of estimated hazard to each person-month;
proc logistic inmodel=outmod;
	score data=riskset out=results  (keep = t exp cum_LTRA P_1 rename =(P_1=hazard));
run;
proc sort data = results; 
	by exp t; 
run;

* computation of survival for each person-month;
data results ;
set results ;
	by exp ;
	retain surv ;
	if first.exp then surv = 1 ;
	surv = surv * (1-hazard);
	risk = 1 - surv;
run;
proc print data=results;
	var exp cum_LTRA t risk;
run;

* Creating separate datasets for strategy = 0 and strategy = 1;
data results0 results1 ;
set results ;
	if exp = 0 then output results0 ;
	if exp = 1 then output results1 ;
	keep t risk;
run;
		
* Merging datasets for strategy = 0 and strategy = 1;
data merge1 ;
merge results0 (rename = (risk = risk0)) results1 (rename = (risk = risk1));
	by t;
	rd = risk1 - risk0;
	rr = risk1 / risk0;
run;

* Edit data frame to reflect that risks are estimated at the END of each interval;
data zero;
	t = 0;
	risk0 = 0;
	risk1 = 0;
run;
data plot; 
set merge1; 
	t = t+1; 
run;
data plot;
merge plot zero;
	by t;
run;
data plot;
set plot;
	month=t*3;
run;
data &out_data.; set plot; run;

* Creating plot;
proc sgplot data = plot;
	step x = month y = risk0 /legendlabel='Never treated by LTRA' lineattrs=(pattern=1 color=ROSE thickness=2pt);
	step x = month y = risk1 /legendlabel='Always treated by LTRA' lineattrs=(pattern=1 color=STEEL thickness=2pt);
	yaxis label = 'Cumulative Incidence'  values = (0 to &absolute_risk. by &interval.) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	xaxis label = 'Months'  values = (0 to 144 by 36) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	keylegend/noborder title=' ' valueattrs=(size=12 weight = bold);
run;
title;

proc logistic data= &in_data. desc outmodel=outmod_hr;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = cum_LTRA t|t;
 	estimate 'Hazard ratio of 0 vs. 30 for cum_LTRA' intercept 0 cum_LTRA 30 / exp;	
run;

%mend;

%ITT_model(in_data=asthma_c.psmtch_cumdose_parkin, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parkin_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkin, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parkin_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parksen, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parksen_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parksen, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parksen_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parkgp, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parkgp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkgp, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parkgp_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parkhosp, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parkhosp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkhosp, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=asthma_c.parkhosp_AT);

%ITT_model(in_data=asthma_c.psmtch_cumdose_all_death, outcome=ev_death, absolute_risk=0.30, interval=0.10, out_data=asthma_c.death_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_all_death, outcome=ev_death, absolute_risk=0.30, interval=0.10, out_data=asthma_c.death_AT);

/*Composite of death and Parkinson's*/
data asthma_c.psmtch_cumdose_parksen; 
set asthma_c.psmtch_cumdose_parksen;
	ev_death=(o_death=1 and t=(surv_death-1));
	if ev_parksen=1 or ev_death=1 then ev_comp=1;
	else ev_comp=0;
run;
proc freq data=asthma_c.psmtch_cumdose_parksen; 
	tables exp*ev_comp;
run;

%ITT_model(in_data=asthma_c.psmtch_cumdose_parksen, outcome=ev_comp, absolute_risk=0.30, interval=0.10, out_data=asthma_c.comp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parksen, outcome=ev_comp, absolute_risk=0.30, interval=0.10, out_data=asthma_c.comp_AT);

/*No reinitiators*/
data psmtch_ltra; 
set asthma_c.psmtch_analysis_all;
	if exp=1;
run;
proc sort data=psmtch_ltra nodupkey out=psmtch_ltra_noreini;
	by patid;
run;
proc sql;
    create table reini as 
    select a.*
    from psmtch_ltra as a
    left join psmtch_ltra_noreini as b
    on a.patid = b.patid and a.trial = b.trial
    where b.patid is null and b.trial is null;
quit;
data psmtch_noltra; 
set asthma_c.psmtch_analysis_all;
	if exp=0;
run;
proc sql;
    create table psmtch_noltra_noreini as 
    select a.*
    from psmtch_noltra as a
    left join reini as b
    on a.trial = b.trial and a.patid_mtch = b.patid_mtch
    where b.trial is null and b.patid_mtch is null;
quit;
data asthma_c.psmtch_analysis_noreini;
set psmtch_noltra_noreini psmtch_ltra_noreini;
run;
proc sort data=asthma_c.psmtch_analysis_noreini;
	by patid trial;
run;
proc sql;
    create table asthma_c.psmtch_cumdose_noreini as 
    select a.*
    from asthma_c.psmtch_cumdose_all as a
    inner join asthma_c.psmtch_analysis_noreini as b
    on a.patid = b.patid and a.trial = b.trial;
quit;

data asthma_c.psmtch_cumdose_noreini_parksen; 
set asthma_c.psmtch_cumdose_noreini;
	if t>surv_parksen then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_noreini_parkhosp; 
set asthma_c.psmtch_cumdose_noreini;
	if t>surv_parkhosp then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_noreini_parkin; 
set asthma_c.psmtch_cumdose_noreini;
	if t>surv_parkin then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_noreini_parkgp; 
set asthma_c.psmtch_cumdose_noreini;
	if t>surv_parkgp then delete;
	if t>47 then delete;
run;

%ITT_model(in_data=asthma_c.psmtch_cumdose_noreini_parkin, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_noreini_parkin, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_noreini_parksen, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=parksen_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_noreini_parksen, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=parksen_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_noreini_parkgp, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=parkgp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_noreini_parkgp, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=parkgp_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_noreini_parkhosp, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=parkhosp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_noreini_parkhosp, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=parkhosp_AT);

/*No baseline NP patients*/
data asthma_c.psmtch_cumdose_sen_anxi;
set asthma_c.psmtch_cumdose_anxi;
	if cov_anxi=1 then delete;
run;
data asthma_c.psmtch_cumdose_sen_sleep;
set asthma_c.psmtch_cumdose_sleep;
	if cov39=1 then delete;
run;
data asthma_c.psmtch_cumdose_sen_depress;
set asthma_c.psmtch_cumdose_depress;
	if cov36=1 then delete;
run;
data asthma_c.psmtch_cumdose_sen_psycho;
set asthma_c.psmtch_cumdose_psycho;
	if cov37=1 then delete;
run;

proc freq data=asthma_c.psmtch_cumdose_depress;
	tables ev_depress; run;

%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_anxi, outcome=ev_anxi, absolute_risk=0.15, interval=0.05, out_data=asthma_c.anxi_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_anxi, outcome=ev_anxi, absolute_risk=0.15, interval=0.05, out_data=asthma_c.anxi_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_sleep, outcome=ev_sleep, absolute_risk=0.15, interval=0.05, out_data=asthma_c.sleep_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_sleep, outcome=ev_sleep, absolute_risk=0.15, interval=0.05, out_data=asthma_c.sleep_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_depress, outcome=ev_depress, absolute_risk=0.15, interval=0.05, out_data=asthma_c.depress_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_depress, outcome=ev_depress, absolute_risk=0.15, interval=0.05, out_data=asthma_c.depress_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_psycho, outcome=ev_psycho, absolute_risk=0.15, interval=0.05, out_data=asthma_c.psycho_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_psycho, outcome=ev_psycho, absolute_risk=0.15, interval=0.05, out_data=asthma_c.psycho_AT);


/*3-year lag*/
data asthma_c.psmtch_cumdose_parkin_lag;
set asthma_c.psmtch_cumdose_parkin;
	if t<=12 then ev_parkin=0;
run;
data asthma_c.psmtch_cumdose_parksen_lag;
set asthma_c.psmtch_cumdose_parksen;
	if t<=12 then ev_parksen=0;
run;
data asthma_c.psmtch_cumdose_parkgp_lag;
set asthma_c.psmtch_cumdose_parkgp;
	if t<=12 then ev_parkgp=0;
run;
data asthma_c.psmtch_cumdose_parkhosp_lag;
set asthma_c.psmtch_cumdose_parkhosp;
	if t<=12 then ev_parkhosp=0;
run;

data psmtch_analysis_lag;
set asthma_c.psmtch_analysis_all;
	if o_parkin=1;
run;
data psmtch_analysis_lag;
set psmtch_analysis_lag;
	if fu_parkin>1095.75;
run;
proc freq data=psmtch_analysis_lag;
	tables exp;
run;

%ITT_model(in_data=asthma_c.psmtch_cumdose_parkin_lag, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkin_lag, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parksen_lag, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=parksen_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parksen_lag, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=parksen_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parkgp_lag, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=parkgp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkgp_lag, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=parkgp_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parkhosp_lag, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=parkhosp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkhosp_lag, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=parkhosp_AT);

/*Autonomic presentations*/
proc sql;
 	create table asthma_a.anos_Dx as 
	select * from asthma_a.clinical 
	where medcodeid in (select medcodeid from asthma_c.code_aurum_anos);
quit;
proc sql;
 	create table asthma_g.anos_Dx as 
	select * from asthma_g.Clinical 
	where medcode in (select medcode from asthma_c.code_gold_anos);
quit;
proc sql;
 	create table asthma_a.const_Dx as 
	select * from asthma_a.clinical 
	where medcodeid in (select medcodeid from asthma_c.code_aurum_const);
quit;
proc sql;
 	create table asthma_g.const_Dx as 
	select * from asthma_g.Clinical 
	where medcode in (select medcode from asthma_c.code_gold_const);
quit;
proc sql;
 	create table asthma_a.dizzi_Dx as 
	select * from asthma_a.clinical 
	where medcodeid in (select medcodeid from asthma_c.code_aurum_dizzi);
quit;
proc sql;
 	create table asthma_g.dizzi_Dx as 
	select * from asthma_g.Clinical 
	where medcode in (select medcode from asthma_c.code_gold_dizzi);
quit;
proc sql;
 	create table asthma_a.urini_Dx as 
	select * from asthma_a.clinical 
	where medcodeid in (select medcodeid from asthma_c.code_aurum_urini);
quit;
proc sql;
 	create table asthma_g.urini_Dx as 
	select * from asthma_g.Clinical 
	where medcode in (select medcode from asthma_c.code_gold_urini);
quit;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table anos_tr&i. as 
	select * from asthma_g.anos_dx 
	where patid in (select distinct patid from asthma_g.psmtch_analysis_tr&i.)
	order by patid,eventdate;
quit;
data anos_tr&i. (keep=patid anosdate);
set anos_tr&i.;
	rename eventdate=anosdate;
run;
data indexdate;
set asthma_g.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table anos_tr&i. as
	select *
	from anos_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where anosdate>(indexdate+91.3125)
	order by patid,anosdate;
quit;
data anos_tr&i._first (keep=patid anosdate);
set anos_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_g.psmtch_analysis_tr&i. as
	select *
	from asthma_g.psmtch_analysis_tr&i. as a
	left join anos_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	if anosdate>enddate then anosdate=.;
	if anosdate<=indexdate then anosdate=.;
	if anosdate~=. and anosdate>indexdate then o_anos=1;
		else o_anos=0;
run;

%end;
%mend;

%auto_outcomes;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table anos_tr&i. as 
	select * from asthma_a.anos_dx 
	where patid in (select distinct patid from asthma_a.psmtch_analysis_tr&i.)
	order by patid,obsdate;
quit;
data anos_tr&i. (keep=patid anosdate);
set anos_tr&i.;
	rename obsdate=anosdate;
run;
data indexdate;
set asthma_a.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table anos_tr&i. as
	select *
	from anos_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where anosdate>(indexdate+91.3125)
	order by patid,anosdate;
quit;
data anos_tr&i._first (keep=patid anosdate);
set anos_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_a.psmtch_analysis_tr&i. as
	select *
	from asthma_a.psmtch_analysis_tr&i. as a
	left join anos_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	if anosdate>enddate then anosdate=.;
	if anosdate<=indexdate then anosdate=.;
	if anosdate~=. and anosdate>indexdate then o_anos=1;
		else o_anos=0;
run;

%end;
%mend;

%auto_outcomes;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table const_tr&i. as 
	select * from asthma_g.const_dx 
	where patid in (select distinct patid from asthma_g.psmtch_analysis_tr&i.)
	order by patid,eventdate;
quit;
data const_tr&i. (keep=patid constdate);
set const_tr&i.;
	rename eventdate=constdate;
run;
data indexdate;
set asthma_g.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table const_tr&i. as
	select *
	from const_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where constdate>(indexdate+91.3125)
	order by patid,constdate;
quit;
data const_tr&i._first (keep=patid constdate);
set const_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_g.psmtch_analysis_tr&i. as
	select *
	from asthma_g.psmtch_analysis_tr&i. as a
	left join const_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	if constdate>enddate then constdate=.;
	if constdate<=indexdate then constdate=.;
	if constdate~=. and constdate>indexdate then o_const=1;
		else o_const=0;
run;

%end;
%mend;

%auto_outcomes;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table const_tr&i. as 
	select * from asthma_a.const_dx 
	where patid in (select distinct patid from asthma_a.psmtch_analysis_tr&i.)
	order by patid,obsdate;
quit;
data const_tr&i. (keep=patid constdate);
set const_tr&i.;
	rename obsdate=constdate;
run;
data indexdate;
set asthma_a.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table const_tr&i. as
	select *
	from const_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where constdate>(indexdate+91.3125)
	order by patid,constdate;
quit;
data const_tr&i._first (keep=patid constdate);
set const_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_a.psmtch_analysis_tr&i. as
	select *
	from asthma_a.psmtch_analysis_tr&i. as a
	left join const_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	if constdate>enddate then constdate=.;
	if constdate<=indexdate then constdate=.;
	if constdate~=. and constdate>indexdate then o_const=1;
		else o_const=0;
run;

%end;
%mend;

%auto_outcomes;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table dizzi_tr&i. as 
	select * from asthma_g.dizzi_dx 
	where patid in (select distinct patid from asthma_g.psmtch_analysis_tr&i.)
	order by patid,eventdate;
quit;
data dizzi_tr&i. (keep=patid dizzidate);
set dizzi_tr&i.;
	rename eventdate=dizzidate;
run;
data indexdate;
set asthma_g.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table dizzi_tr&i. as
	select *
	from dizzi_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where dizzidate>(indexdate+91.3125)
	order by patid,dizzidate;
quit;
data dizzi_tr&i._first (keep=patid dizzidate);
set dizzi_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_g.psmtch_analysis_tr&i. as
	select *
	from asthma_g.psmtch_analysis_tr&i. as a
	left join dizzi_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	if dizzidate>enddate then dizzidate=.;
	if dizzidate<=indexdate then dizzidate=.;
	if dizzidate~=. and dizzidate>indexdate then o_dizzi=1;
		else o_dizzi=0;
run;

%end;
%mend;

%auto_outcomes;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table dizzi_tr&i. as 
	select * from asthma_a.dizzi_dx 
	where patid in (select distinct patid from asthma_a.psmtch_analysis_tr&i.)
	order by patid,obsdate;
quit;
data dizzi_tr&i. (keep=patid dizzidate);
set dizzi_tr&i.;
	rename obsdate=dizzidate;
run;
data indexdate;
set asthma_a.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table dizzi_tr&i. as
	select *
	from dizzi_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where dizzidate>(indexdate+91.3125)
	order by patid,dizzidate;
quit;
data dizzi_tr&i._first (keep=patid dizzidate);
set dizzi_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_a.psmtch_analysis_tr&i. as
	select *
	from asthma_a.psmtch_analysis_tr&i. as a
	left join dizzi_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	if dizzidate>enddate then dizzidate=.;
	if dizzidate<=indexdate then dizzidate=.;
	if dizzidate~=. and dizzidate>indexdate then o_dizzi=1;
		else o_dizzi=0;
run;

%end;
%mend;

%auto_outcomes;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table urini_tr&i. as 
	select * from asthma_g.urini_dx 
	where patid in (select distinct patid from asthma_g.psmtch_analysis_tr&i.)
	order by patid,eventdate;
quit;
data urini_tr&i. (keep=patid urinidate);
set urini_tr&i.;
	rename eventdate=urinidate;
run;
data indexdate;
set asthma_g.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table urini_tr&i. as
	select *
	from urini_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where urinidate>(indexdate+91.3125)
	order by patid,urinidate;
quit;
data urini_tr&i._first (keep=patid urinidate);
set urini_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_g.psmtch_analysis_tr&i. as
	select *
	from asthma_g.psmtch_analysis_tr&i. as a
	left join urini_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	if urinidate>enddate then urinidate=.;
	if urinidate<=indexdate then urinidate=.;
	if urinidate~=. and urinidate>indexdate then o_urini=1;
		else o_urini=0;
run;

%end;
%mend;

%auto_outcomes;

%macro auto_outcomes;
%let min=1;
%let max=140;
%do i=&min %to &max;

proc sql;
 	create table urini_tr&i. as 
	select * from asthma_a.urini_dx 
	where patid in (select distinct patid from asthma_a.psmtch_analysis_tr&i.)
	order by patid,obsdate;
quit;
data urini_tr&i. (keep=patid urinidate);
set urini_tr&i.;
	rename obsdate=urinidate;
run;
data indexdate;
set asthma_a.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table urini_tr&i. as
	select *
	from urini_tr&i. as a
	left join indexdate as b on a.patid=b.patid
	where urinidate>(indexdate+91.3125)
	order by patid,urinidate;
quit;
data urini_tr&i._first (keep=patid urinidate);
set urini_tr&i.;
	by patid;
		if first.patid;
run;
proc sql;
	create table asthma_a.psmtch_analysis_tr&i. as
	select *
	from asthma_a.psmtch_analysis_tr&i. as a
	left join urini_tr&i._first as b on a.patid=b.patid;
quit;
data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	if urinidate>enddate then urinidate=.;
	if urinidate<=indexdate then urinidate=.;
	if urinidate~=. and urinidate>indexdate then o_urini=1;
		else o_urini=0;
run;

%end;
%mend;

%auto_outcomes;


%macro cum_dose;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	if dod~=. and dod<=enddate then o_death=1;
		else o_death=0;
		fu_death=enddate-indexdate;
run;
data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	if o_anos=1 then fu_anos=enddate-anosdate;
		else fu_anos=enddate-indexdate;
	if o_const=1 then fu_const=enddate-constdate;
		else fu_const=enddate-indexdate;
	if o_dizzi=1 then fu_dizzi=enddate-dizzidate;
		else fu_dizzi=enddate-indexdate;
	if o_urini=1 then fu_urini=enddate-urinidate;
		else fu_urini=enddate-indexdate;
run;

/*Long data format*/
data asthma_a.psmtch_analysis_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	surv_death=floor(fu_death/91.3125);
	surv_anos=floor(fu_anos/91.3125)-1;
	surv_const=floor(fu_const/91.3125)-1;
	surv_dizzi=floor(fu_dizzi/91.3125)-1;
	surv_urini=floor(fu_urini/91.3125)-1;
run;

* Expanding dataset;
data asthma_a.psmtch_surv_sen_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	do loop= 0 to surv_death;
		output;
	end;
run;

* Creating variable for time;
proc sort data=asthma_a.psmtch_surv_sen_tr&i.;
	by patid;
run;
data asthma_a.psmtch_surv_sen_tr&i.;
set asthma_a.psmtch_surv_sen_tr&i.;
	retain t;
	by patid;
	if first.patid then t=0;
	else t=t+1;
run;

* Creating event variable;
data asthma_a.psmtch_surv_sen_tr&i.;
set asthma_a.psmtch_surv_sen_tr&i.;;
	ev_anos=(o_anos=1 and t=surv_anos);
	ev_const=(o_const=1 and t=surv_const);
	ev_dizzi=(o_dizzi=1 and t=surv_dizzi);
	ev_urini=(o_urini=1 and t=surv_urini);
run;
proc sql;
 	create table LTRA_Rx_tr&i. as 
	select distinct * from asthma_a.LTRA_Rx 
	where patid in (select patid from asthma_a.psmtch_analysis_tr&i.);
quit;
data indexdate_tr&i.;
set asthma_a.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table LTRA_Rx_tr&i. as
	select *
	from LTRA_Rx_tr&i. as a
	left join indexdate_tr&i. as b on a.patid=b.patid
	where indexdate<=issuedate
	order by patid,issuedate;
quit;
data LTRA_Rx_tr&i. (keep=patid t ltra);
set LTRA_Rx_tr&i.;
	t=floor((issuedate-indexdate)/91.3125);
	ltra=1;
run;
proc sort data=LTRA_Rx_tr&i. nodupkey;
	by patid t;
run;
proc sql;
	create table asthma_a.psmtch_cumdose_sen_tr&i. as
	select *
	from asthma_a.psmtch_surv_sen_tr&i. as a
	left join LTRA_Rx_tr&i. as b on a.patid=b.patid and a.t=b.t
	order by patid,t;
quit;
data asthma_a.psmtch_cumdose_sen_tr&i.;
set asthma_a.psmtch_cumdose_sen_tr&i.;
	if ltra=. then ltra=0;
run;
data asthma_a.psmtch_cumdose_sen_tr&i.;
set asthma_a.psmtch_cumdose_sen_tr&i.;
	by patid;
	retain cum_LTRA;
	if first.patid then cum_LTRA = LTRA;
	else cum_LTRA = cum_LTRA + LTRA;
run;

%end;
%mend;

%cum_dose;

%macro cum_dose;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	if dod~=. and dod<=enddate then o_death=1;
		else o_death=0;
		fu_death=enddate-indexdate;
run;
data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	if o_anos=1 then fu_anos=enddate-anosdate;
		else fu_anos=enddate-indexdate;
	if o_const=1 then fu_const=enddate-constdate;
		else fu_const=enddate-indexdate;
	if o_dizzi=1 then fu_dizzi=enddate-dizzidate;
		else fu_dizzi=enddate-indexdate;
	if o_urini=1 then fu_urini=enddate-urinidate;
		else fu_urini=enddate-indexdate;
run;
run;

/*Long data format*/
data asthma_g.psmtch_analysis_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	surv_death=floor(fu_death/91.3125);
	surv_anos=floor(fu_anos/91.3125)-1;
	surv_const=floor(fu_const/91.3125)-1;
	surv_dizzi=floor(fu_dizzi/91.3125)-1;
	surv_urini=floor(fu_urini/91.3125)-1;
run;

* Expanding dataset;
data asthma_g.psmtch_surv_sen_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	do loop= 0 to surv_death;
		output;
	end;
run;

* Creating variable for time;
proc sort data=asthma_g.psmtch_surv_sen_tr&i.;
	by patid;
run;
data asthma_g.psmtch_surv_sen_tr&i.;
set asthma_g.psmtch_surv_sen_tr&i.;
	retain t;
	by patid;
	if first.patid then t=0;
	else t=t+1;
run;

* Creating event variable;
data asthma_g.psmtch_surv_sen_tr&i.;
set asthma_g.psmtch_surv_sen_tr&i.;
	ev_anos=(o_anos=1 and t=surv_anos);
	ev_const=(o_const=1 and t=surv_const);
	ev_dizzi=(o_dizzi=1 and t=surv_dizzi);
	ev_urini=(o_urini=1 and t=surv_urini);
run;
proc sql;
 	create table LTRA_Rx_tr&i. as 
	select distinct * from asthma_g.LTRA_Rx 
	where patid in (select patid from asthma_g.psmtch_analysis_tr&i.);
quit;
data indexdate_tr&i.;
set asthma_g.psmtch_analysis_tr&i.;
	keep patid indexdate;
run;
proc sql;
	create table LTRA_Rx_tr&i. as
	select *
	from LTRA_Rx_tr&i. as a
	left join indexdate_tr&i. as b on a.patid=b.patid
	where indexdate<=eventdate
	order by patid,eventdate;
quit;
data LTRA_Rx_tr&i. (keep=patid t ltra);
set LTRA_Rx_tr&i.;
	t=floor((eventdate-indexdate)/91.3125);
	ltra=1;
run;
proc sort data=LTRA_Rx_tr&i. nodupkey;
	by patid t;
run;
proc sql;
	create table asthma_g.psmtch_cumdose_sen_tr&i. as
	select *
	from asthma_g.psmtch_surv_sen_tr&i. as a
	left join LTRA_Rx_tr&i. as b on a.patid=b.patid and a.t=b.t
	order by patid,t;
quit;
data asthma_g.psmtch_cumdose_sen_tr&i.;
set asthma_g.psmtch_cumdose_sen_tr&i.;
	if ltra=. then ltra=0;
run;
data asthma_g.psmtch_cumdose_sen_tr&i.;
set asthma_g.psmtch_cumdose_sen_tr&i.;
	by patid;
	retain cum_LTRA;
	if first.patid then cum_LTRA = LTRA;
	else cum_LTRA = cum_LTRA + LTRA;
run;

%end;
%mend;

%cum_dose;
data asthma_a.psmtch_cumdose_sen_all;
set asthma_a.psmtch_cumdose_sen_tr1-asthma_a.psmtch_cumdose_sen_tr140;
run;
data asthma_g.psmtch_cumdose_sen_all;
set asthma_g.psmtch_cumdose_sen_tr1-asthma_g.psmtch_cumdose_sen_tr140;
run;

data asthma_c.psmtch_cumdose_sen_all;
set asthma_g.psmtch_cumdose_sen_all asthma_a.psmtch_cumdose_sen_all;
run;

proc sort data=asthma_c.psmtch_cumdose_sen_all;
	by trial patid t;
run;

data asthma_c.psmtch_cumdose_sen_anos; 
set asthma_c.psmtch_cumdose_sen_all;
	if t>surv_anos then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_sen_const; 
set asthma_c.psmtch_cumdose_sen_all;
	if t>surv_const then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_sen_dizzi; 
set asthma_c.psmtch_cumdose_sen_all;
	if t>surv_dizzi then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_sen_urini; 
set asthma_c.psmtch_cumdose_sen_all;
	if t>surv_urini then delete;
	if t>47 then delete;
run;

proc freq data=asthma_c.psmtch_analysis_all;
	tables exp*o_anos exp*o_const exp*o_dizzi exp*o_urini;
run;
%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_anos, outcome=ev_anos, absolute_risk=0.50, interval=0.1, out_data=anos_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_anos, outcome=ev_anos, absolute_risk=0.50, interval=0.1, out_data=anos_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_const, outcome=ev_const, absolute_risk=0.50, interval=0.1, out_data=const_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_const, outcome=ev_const, absolute_risk=0.50, interval=0.1, out_data=const_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_dizzi, outcome=ev_dizzi, absolute_risk=0.50, interval=0.1, out_data=dizzi_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_dizzi, outcome=ev_dizzi, absolute_risk=0.50, interval=0.1, out_data=dizzi_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_sen_urini, outcome=ev_urini, absolute_risk=0.50, interval=0.1, out_data=urini_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_sen_urini, outcome=ev_urini, absolute_risk=0.50, interval=0.1, out_data=urini_AT);


/*Age and sex subgroups*/
data psmtch_cumdose_parkin_sub1;
set asthma_c.psmtch_cumdose_parkin;
	if age>=65;
run;
data psmtch_cumdose_parkin_sub2;
set asthma_c.psmtch_cumdose_parkin;
	if age<65;
run;
data psmtch_cumdose_parkin_sub3;
set asthma_c.psmtch_cumdose_parkin;
	if gender=1;
run;
data psmtch_cumdose_parkin_sub4;
set asthma_c.psmtch_cumdose_parkin;
	if gender=2;
run;

data asthma_c.psmtch_cumdose_parkin;
set asthma_c.psmtch_cumdose_parkin;
	if age>=65 then age_group=1;
	else age_group=0;
run;

proc logistic data=asthma_c.psmtch_cumdose_parkin desc;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class exp age_group gender;
	model ev_parkin = exp t|t gender exp*gender;
 	estimate intercept 0 exp 1 / exp;	
run;
proc logistic data=asthma_c.psmtch_cumdose_parkin desc;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	class exp age_group gender;
	model ev_parkin = cum_LTRA t|t age_group cum_LTRA*age_group;
 	estimate 'Hazard ratio of 0 vs. 30 for cum_LTRA' intercept 0 cum_LTRA 30 / exp;	
run;

%ITT_model(in_data=psmtch_cumdose_parkin_sub1, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_ITT_sub1);
%AT_model(in_data=psmtch_cumdose_parkin_sub1, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_AT_sub1);

%ITT_model(in_data=psmtch_cumdose_parkin_sub2, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_ITT_sub2);
%AT_model(in_data=psmtch_cumdose_parkin_sub2, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_AT_sub2);

%ITT_model(in_data=psmtch_cumdose_parkin_sub3, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_ITT_sub3);
%AT_model(in_data=psmtch_cumdose_parkin_sub3, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_AT_sub3);

%ITT_model(in_data=psmtch_cumdose_parkin_sub4, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_ITT_sub4);
%AT_model(in_data=psmtch_cumdose_parkin_sub4, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_AT_sub4);


/*Record history of 5 years*/
%macro sen_cleaning;
%let min=1;
%let max=140;
%do k=&min %to &max;

data asthma_c.patient_cov_tr&k._sen; 
set asthma_c.patient_cov_tr&k.;
	if indexdate<(regstartdate+1825) then delete;
run;
%end;
%mend;

%sen_cleaning (n=140);

%macro seq_mtch;
%let min=1;
%let max=140;
%do j=&min %to &max;

/*calculate PS from your dataset*/
proc logistic descending data=asthma_c.patient_cov_tr&j._sen;
	class exp gender calendar 
cov1 cov2 cov3 cov4 cov5 cov6 cov13 cov14
cov15 cov16 cov17 cov18 cov19 cov20 cov21 cov22 cov23 cov24 cov25 cov26 cov27 cov28 cov29
cov30 cov31 cov32 cov33 cov34 cov35 cov36 cov37 cov38 cov39 cov40 cov41 cov42 cov43 cov44
cov45 cov46 cov47 cov48 cov49 cov50;
	model exp = age gender calendar 
dur cov9 cov10 cov11 age*age dur*dur cov9*cov9 cov10*cov10 cov11*cov11
cov1 cov2 cov3 cov4 cov5 cov6 cov13 cov14
cov15 cov16 cov17 cov18 cov19 cov20 cov21 cov22 cov23 cov24 cov25 cov26 cov27 cov28 cov29
cov30 cov31 cov32 cov33 cov34 cov35 cov36 cov37 cov38 cov39 cov40 cov41 cov42 cov43 cov44
cov45 cov46 cov47 cov48 cov49 cov50;
	output out=out_ps prob=ps xbeta=logit_ps;
run;

/*data manipulation*/
data case;
set out_ps;
	if exp=1 ;
	keep patid ps;
run;

data case1;
set case;
	caseid=_n_;
run;

data control;
set out_ps;
	if exp=0 ;
	keep patid ps;
run;

data control1;
set control;
	ctlid=_n_;
run;

%match(case=case1,control=control1,idca=caseid,idco=ctlid,
              mvars=ps,maxiter=100000,
              wts=1, dmaxk=0.05,out=mtch,ncontls=1, /*weight, caliper, matching ratio*/
              method=greedy, seedco=123, seedca=321);


/*data cleaning*/
data mtchall;set mtch;run;

data mtchcase;set mtchall;
rename __IDCA=caseid;
keep  __IDCA ;
run;
proc sort data=mtchall;by __IDCA;run;

data mtchctl;set mtchall;
rename __IDCO=ctlid;
keep  __IDCO  ;
run;
proc sort data=mtchctl;by ctlid;run;

proc sort data=case1;by caseid;run;
proc sort data=mtchcase;by caseid;run;

data mtchcase1;merge mtchcase(in=inmtchcase) case1;
by caseid;
if inmtchcase;
keep patid ;run;

proc sort data=mtchcase1;by patid;run;
data mtchcase1f;set mtchcase1;
by patid;
if first.patid;
group=1;
run;

proc sort data=control1;by ctlid;run;
proc sort data=mtchctl;by ctlid;run;
data mtchctl1;merge mtchctl(in=inmtchctl) control1;
by ctlid;
if inmtchctl;
keep patid ;
run;

proc sort data=mtchctl1;by patid;run;
data mtchctl1f;set mtchctl1;
by patid;
if first.patid;
group=0;
run;

data mtchfinal;set mtchcase1f mtchctl1f;
run;

proc sql;
create table psmtch_analysis_tr&j. as select * from out_ps where patid in (select patid from mtchfinal);
quit;

proc sql;
    create table mtchID as
    select distinct a.__IDCA,a.__IDCO,b.patid as patid_CA,c.patid as patid_CO
    from mtchall as a
    left join case1 as b 
        on a.__IDCA = b.caseid
	left join control1 as c 
        on a.__IDCO = c.ctlid;
quit;

proc sql;
    create table psmtch_analysis_tr&j. as
    select distinct a.*,b.patid_CA as patid_mtch
    from psmtch_analysis_tr&j. as a
    left join mtchID as b 
        on a.patid = b.patid_CO;
quit;

data asthma_c.psmtch_analysis_tr&j._sen;
set psmtch_analysis_tr&j.;
	if patid_mtch="" then patid_mtch=patid;
run;

%end;
%mend;

%seq_mtch;

%macro label;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_c.psmtch_analysis_tr&i._sen;
set asthma_c.psmtch_analysis_tr&i._sen;
	trial=&i.;
run;

%end;
%mend;

%label;

%macro combine_datasets;
    data asthma_c.psmtch_analysis_all_sen;
        set 
        %do i = 1 %to 140;
            asthma_c.psmtch_analysis_tr&i._sen
        %end;;
    run;
%mend;

%combine_datasets;

%macro separate;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_a.psmtch_analysis_tr&i._sen asthma_g.psmtch_analysis_tr&i._sen;
set asthma_c.psmtch_analysis_tr&i._sen;
	if source = "AURUM" then output asthma_a.psmtch_analysis_tr&i._sen ;
	if source = "GOLD" then output asthma_g.psmtch_analysis_tr&i._sen ;
run;
%end;
%mend;

%separate;

%macro cum_dose;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_a.psmtch_analysis_tr&i._sen;
set asthma_a.psmtch_analysis_tr&i._sen;
	if dod~=. and dod<=enddate then o_death=1;
		else o_death=0;
		fu_death=enddate-indexdate;
run;

/*Long data format*/
data asthma_a.psmtch_analysis_tr&i._sen;
set asthma_a.psmtch_analysis_tr&i._sen;
	surv_death=floor(fu_death/91.3125);
	surv_parkin=floor(fu_parkin/91.3125)-1;
	surv_parkhosp=floor(fu_parkhosp/91.3125)-1;
	surv_parksen=floor(fu_parksen/91.3125)-1;
	surv_parkgp=floor(fu_parkgp/91.3125)-1;
	surv_anxi=floor(fu_anxi/91.3125)-1;
	surv_sleep=floor(fu_sleep/91.3125)-1;
	surv_depress=floor(fu_depress/91.3125)-1;
	surv_psycho=floor(fu_psycho/91.3125)-1;
run;

* Expanding dataset;
data asthma_a.psmtch_surv_tr&i._sen;
set asthma_a.psmtch_analysis_tr&i._sen;
	do loop= 0 to surv_death;
		output;
	end;
run;

* Creating variable for time;
proc sort data=asthma_a.psmtch_surv_tr&i._sen;
	by patid;
run;
data asthma_a.psmtch_surv_tr&i._sen;
set asthma_a.psmtch_surv_tr&i._sen;
	retain t;
	by patid;
	if first.patid then t=0;
	else t=t+1;
run;

* Creating event variable;
data asthma_a.psmtch_surv_tr&i._sen;
set asthma_a.psmtch_surv_tr&i._sen;
	ev_death=(o_death=1 and t=surv_death);
	ev_parkin=(o_parkin=1 and t=surv_parkin);
	ev_parkhosp=(o_parkhosp=1 and t=surv_parkhosp);
	ev_parksen=(o_parksen=1 and t=surv_parksen);
	ev_parkgp=(o_parkgp=1 and t=surv_parkgp);
	ev_anxi=(o_anxi=1 and t=surv_anxi);
	ev_sleep=(o_sleep=1 and t=surv_sleep);
	ev_depress=(o_depress=1 and t=surv_depress);
	ev_psycho=(o_psycho=1 and t=surv_psycho);
run;
proc sql;
 	create table LTRA_Rx_tr&i. as 
	select distinct * from asthma_a.LTRA_Rx 
	where patid in (select patid from asthma_a.psmtch_analysis_tr&i._sen);
quit;
data indexdate_tr&i.;
set asthma_a.psmtch_analysis_tr&i._sen;
	keep patid indexdate;
run;
proc sql;
	create table LTRA_Rx_tr&i. as
	select *
	from LTRA_Rx_tr&i. as a
	left join indexdate_tr&i. as b on a.patid=b.patid
	where indexdate<=issuedate
	order by patid,issuedate;
quit;
data LTRA_Rx_tr&i. (keep=patid t ltra);
set LTRA_Rx_tr&i.;
	t=floor((issuedate-indexdate)/91.3125);
	ltra=1;
run;
proc sort data=LTRA_Rx_tr&i. nodupkey;
	by patid t;
run;
proc sql;
	create table asthma_a.psmtch_cumdose_tr&i._sen as
	select *
	from asthma_a.psmtch_surv_tr&i._sen as a
	left join LTRA_Rx_tr&i. as b on a.patid=b.patid and a.t=b.t
	order by patid,t;
quit;
data asthma_a.psmtch_cumdose_tr&i._sen;
set asthma_a.psmtch_cumdose_tr&i._sen;
	if ltra=. then ltra=0;
run;
data asthma_a.psmtch_cumdose_tr&i._sen;
set asthma_a.psmtch_cumdose_tr&i._sen;
	by patid;
	retain cum_LTRA;
	if first.patid then cum_LTRA = LTRA;
	else cum_LTRA = cum_LTRA + LTRA;
run;

%end;
%mend;

%cum_dose;

%macro combine_datasets;
    data asthma_a.psmtch_cumdose_all_sen;
        set 
        %do i = 1 %to 140;
            asthma_a.psmtch_cumdose_tr&i._sen
        %end;;
    run;
%mend;

%combine_datasets;

%macro cum_dose;
%let min=1;
%let max=140;
%do i=&min %to &max;

data asthma_g.psmtch_analysis_tr&i._sen;
set asthma_g.psmtch_analysis_tr&i._sen;
	if dod~=. and dod<=enddate then o_death=1;
		else o_death=0;
		fu_death=enddate-indexdate;
run;

/*Long data format*/
data asthma_g.psmtch_analysis_tr&i._sen;
set asthma_g.psmtch_analysis_tr&i._sen;
	surv_death=floor(fu_death/91.3125);
	surv_parkin=floor(fu_parkin/91.3125)-1;
	surv_parkhosp=floor(fu_parkhosp/91.3125)-1;
	surv_parksen=floor(fu_parksen/91.3125)-1;
	surv_parkgp=floor(fu_parkgp/91.3125)-1;
	surv_anxi=floor(fu_anxi/91.3125)-1;
	surv_sleep=floor(fu_sleep/91.3125)-1;
	surv_depress=floor(fu_depress/91.3125)-1;
	surv_psycho=floor(fu_psycho/91.3125)-1;
run;

* Expanding dataset;
data asthma_g.psmtch_surv_tr&i._sen;
set asthma_g.psmtch_analysis_tr&i._sen;
	do loop= 0 to surv_death;
		output;
	end;
run;

* Creating variable for time;
proc sort data=asthma_g.psmtch_surv_tr&i._sen;
	by patid;
run;
data asthma_g.psmtch_surv_tr&i._sen;
set asthma_g.psmtch_surv_tr&i._sen;
	retain t;
	by patid;
	if first.patid then t=0;
	else t=t+1;
run;

* Creating event variable;
data asthma_g.psmtch_surv_tr&i._sen;
set asthma_g.psmtch_surv_tr&i._sen;
	ev_death=(o_death=1 and t=surv_death);
	ev_parkin=(o_parkin=1 and t=surv_parkin);
	ev_parkhosp=(o_parkhosp=1 and t=surv_parkhosp);
	ev_parksen=(o_parksen=1 and t=surv_parksen);
	ev_parkgp=(o_parkgp=1 and t=surv_parkgp);
	ev_anxi=(o_anxi=1 and t=surv_anxi);
	ev_sleep=(o_sleep=1 and t=surv_sleep);
	ev_depress=(o_depress=1 and t=surv_depress);
	ev_psycho=(o_psycho=1 and t=surv_psycho);
run;
proc sql;
 	create table LTRA_Rx_tr&i. as 
	select distinct * from asthma_g.LTRA_Rx 
	where patid in (select patid from asthma_g.psmtch_analysis_tr&i._sen);
quit;
data indexdate_tr&i.;
set asthma_g.psmtch_analysis_tr&i._sen;
	keep patid indexdate;
run;
proc sql;
	create table LTRA_Rx_tr&i. as
	select *
	from LTRA_Rx_tr&i. as a
	left join indexdate_tr&i. as b on a.patid=b.patid
	where indexdate<=eventdate
	order by patid,eventdate;
quit;
data LTRA_Rx_tr&i. (keep=patid t ltra);
set LTRA_Rx_tr&i.;
	t=floor((eventdate-indexdate)/91.3125);
	ltra=1;
run;
proc sort data=LTRA_Rx_tr&i. nodupkey;
	by patid t;
run;
proc sql;
	create table asthma_g.psmtch_cumdose_tr&i._sen as
	select *
	from asthma_g.psmtch_surv_tr&i._sen as a
	left join LTRA_Rx_tr&i. as b on a.patid=b.patid and a.t=b.t
	order by patid,t;
quit;
data asthma_g.psmtch_cumdose_tr&i._sen;
set asthma_g.psmtch_cumdose_tr&i._sen;
	if ltra=. then ltra=0;
run;
data asthma_g.psmtch_cumdose_tr&i._sen;
set asthma_g.psmtch_cumdose_tr&i._sen;
	by patid;
	retain cum_LTRA;
	if first.patid then cum_LTRA = LTRA;
	else cum_LTRA = cum_LTRA + LTRA;
run;

%end;
%mend;

%cum_dose;

%macro combine_datasets;
    data asthma_g.psmtch_cumdose_all_sen;
        set 
        %do i = 1 %to 140;
            asthma_g.psmtch_cumdose_tr&i._sen
        %end;;
    run;
%mend;

%combine_datasets;


data asthma_c.psmtch_cumdose_all_sen;
set asthma_g.psmtch_cumdose_all_sen asthma_a.psmtch_cumdose_all_sen;
run;

proc sort data=asthma_c.psmtch_cumdose_all_sen;
	by trial patid t;
run;

data asthma_c.psmtch_cumdose_parksen_sen; 
set asthma_c.psmtch_cumdose_all_sen;
	if t>surv_parksen then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_parkhosp_sen; 
set asthma_c.psmtch_cumdose_all;
	if t>surv_parkhosp then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_parkin_sen; 
set asthma_c.psmtch_cumdose_all_sen;
	if t>surv_parkin then delete;
	if t>47 then delete;
run;
data asthma_c.psmtch_cumdose_parkgp_sen; 
set asthma_c.psmtch_cumdose_all_sen;
	if t>surv_parkgp then delete;
	if t>47 then delete;
run;

%MACRO ITT_model(in_data=, outcome=, absolute_risk=, interval=, out_data=);
/*ITT model*/
proc logistic data= &in_data. desc outmodel=outmod;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = exp|t|t;
run;

* Creating dataset to store results;
data riskset ;
	do t = 0 to 47 ;
		exp = 0 ;
		output ;
		exp = 1;
		output ;
	end;	
run;

* assignment of estimated hazard to each person-month;
proc logistic inmodel=outmod;
	score data=riskset out=results  (keep = t exp exp P_1 rename =(P_1=hazard));
run;
proc sort data = results; 
	by exp t; 
run;

* computation of survival for each person-month;
data results ;
set results ;
	by exp ;
	retain surv ;
	if first.exp then surv = 1 ;
	surv = surv * (1-hazard);
	risk = 1 - surv;
run;
proc print data=results;
	var exp t risk;
run;

* Creating separate datasets for strategy = 0 and strategy = 1;
data results0 results1 ;
set results ;
	if exp = 0 then output results0 ;
	if exp = 1 then output results1 ;
	keep t risk;
run;
		
* Merging datasets for strategy = 0 and strategy = 1;
data merge1 ;
merge results0 (rename = (risk = risk0)) results1 (rename = (risk = risk1));
	by t;
	rd = risk1 - risk0;
	rr = risk1 / risk0;
run;

* Edit data frame to reflect that risks are estimated at the END of each interval;
data zero;
	t = 0;
	risk0 = 0;
	risk1 = 0;
run;
data plot; 
set merge1; 
	t = t+1; 
run;
data plot;
merge plot zero;
	by t;
run;
data plot;
set plot;
	month=t*3;
run;
data &out_data.; set plot; run;

* Creating plot;
proc sgplot data = plot;
	step x = month y = risk0 /legendlabel='No LTRA initiation' lineattrs=(pattern=1 color=ROSE thickness=2pt);
	step x = month y = risk1 /legendlabel='LTRA initiation' lineattrs=(pattern=1 color=STEEL thickness=2pt);
	yaxis label = 'Cumulative Incidence'  values = (0 to &absolute_risk. by &interval.) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	xaxis label = 'Months'  values = (0 to 144 by 36) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	keylegend/noborder title=' ' valueattrs=(size=12 weight = bold);
run;
title;

proc logistic data= &in_data. desc outmodel=outmod_hr;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = exp t|t;
 	estimate intercept 0 exp 1 / exp;	
run;

%mend;

%MACRO AT_model(in_data=, outcome=, absolute_risk=, interval=, out_data=);
/*Cumulative dose model*/
proc logistic data= &in_data. desc outmodel=outmod;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = cum_LTRA|cum_LTRA|t|t;
run;

* Creating dataset to store results;
data riskset ;
	do t = 0 to 47 ;
	exp = 0 ;
		cum_LTRA = 0 ;
		output ;
		exp = 1;
		cum_LTRA = t+1 ;
		output ;
	end;	
run;

* assignment of estimated hazard to each person-month;
proc logistic inmodel=outmod;
	score data=riskset out=results  (keep = t exp cum_LTRA P_1 rename =(P_1=hazard));
run;
proc sort data = results; 
	by exp t; 
run;

* computation of survival for each person-month;
data results ;
set results ;
	by exp ;
	retain surv ;
	if first.exp then surv = 1 ;
	surv = surv * (1-hazard);
	risk = 1 - surv;
run;
proc print data=results;
	var exp cum_LTRA t risk;
run;

* Creating separate datasets for strategy = 0 and strategy = 1;
data results0 results1 ;
set results ;
	if exp = 0 then output results0 ;
	if exp = 1 then output results1 ;
	keep t risk;
run;
		
* Merging datasets for strategy = 0 and strategy = 1;
data merge1 ;
merge results0 (rename = (risk = risk0)) results1 (rename = (risk = risk1));
	by t;
	rd = risk1 - risk0;
	rr = risk1 / risk0;
run;

* Edit data frame to reflect that risks are estimated at the END of each interval;
data zero;
	t = 0;
	risk0 = 0;
	risk1 = 0;
run;
data plot; 
set merge1; 
	t = t+1; 
run;
data plot;
merge plot zero;
	by t;
run;
data plot;
set plot;
	month=t*3;
run;
data &out_data.; set plot; run;

* Creating plot;
proc sgplot data = plot;
	step x = month y = risk0 /legendlabel='Never treated by LTRA' lineattrs=(pattern=1 color=ROSE thickness=2pt);
	step x = month y = risk1 /legendlabel='Always treated by LTRA' lineattrs=(pattern=1 color=STEEL thickness=2pt);
	yaxis label = 'Cumulative Incidence'  values = (0 to &absolute_risk. by &interval.) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	xaxis label = 'Months'  values = (0 to 144 by 36) labelattrs=(size=12 weight = bold) valueattrs=(size=12 weight = bold);
	keylegend/noborder title=' ' valueattrs=(size=12 weight = bold);
run;
title;

proc logistic data= &in_data. desc outmodel=outmod_hr;
	ods exclude ClassLevelInfo Type3 Association FitStatistics GlobalTests;
	model &outcome. = cum_LTRA t|t;
 	estimate 'Hazard ratio of 0 vs. 30 for cum_LTRA' intercept 0 cum_LTRA 30 / exp;	
run;

%mend;

proc freq data=asthma_c.psmtch_analysis_all_sen;
	tables exp*o_parksen exp*o_parkin exp*o_parkgp exp*o_parkhosp;
run;

%ITT_model(in_data=asthma_c.psmtch_cumdose_parkin_sen, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkin_sen, outcome=ev_parkin, absolute_risk=0.03, interval=0.01, out_data=parkin_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parksen_sen, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=parksen_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parksen_sen, outcome=ev_parksen, absolute_risk=0.03, interval=0.01, out_data=parksen_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parkgp_sen, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=parkgp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkgp_sen, outcome=ev_parkgp, absolute_risk=0.03, interval=0.01, out_data=parkgp_AT);
%ITT_model(in_data=asthma_c.psmtch_cumdose_parkhosp_sen, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=parkhosp_ITT);
%AT_model(in_data=asthma_c.psmtch_cumdose_parkhosp_sen, outcome=ev_parkhosp, absolute_risk=0.03, interval=0.01, out_data=parkhosp_AT);
