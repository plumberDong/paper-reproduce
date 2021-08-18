*****2005, Subjective Wellbeing of different SES groups******
*******************************************************
use "cgss2005_14.dta",clear


set more off
tab1 qc13 qe01 qe02 qe03 qe05
tabulate qe03 qs2c,column  /*rural and urban happiness*/


gen happy=0 if qe03==1 | qe03==2 | qe03==3 /*generate binary happiness variable*/
replace happy=1 if qe03==4 | qe03==5

gen peerh=0  /*peer comparison with 3 categories,reference 'so-so'*/
replace peerh=1 if qe01==1
gen peerl=0
replace peerl=1 if qe01==3

gen peer=1 if qe01==3  /*As three level dep var*/
  replace peer=2 if (qe01==2 | qe01==4)
  replace peer=3 if qe01==1

  
  
gen up=0  /*self comparison with 3 categories,reference 'so-so'*/
replace up=1 if qe02==1
gen down=0
replace down=1 if qe02==3



gen self=1 if qe02==3  /*As three level dep var*/
  replace self=2 if (qe02==2 | qe02==4)
  replace self=3 if qe02==1
  
  

gen close=0  /*Close contact with relatives and friends*/
replace close=1 if qe05==4 | qe05==5

gen health=0  /*HEALTHY*/
replace health=1 if qd1==1 |qd1==2 |qd1==3

gen male=0   /*MALE VS. FEMALE*/
replace male=1 if qa2_01==1

gen age=2005-qa3_01  /*AGE & AGE SQUARE*/
gen age2=(age*age)/100
gen age1=.
replace age1=age if age>24 & age<70


gen han=0   /*HAN VS. MINORITIES*/
replace han=1 if qa4_01==1

gen urban=0  /*URBAN VS. Rural residence*/
replace urban=1 if qs2c==1

gen marry=0  /*IN MARRIAGE*/
replace marry=1 if qb01==2 | qb02==4 | qb02==6

gen party=0  /*CCP MEMBER*/
replace party=1 if qb04a==1

gen hedu=0  /*HIGHER EDUCATION*/
replace hedu=1 if qb03b>16 & qb03b<23

gen edu=1 if qb03b>0 & qb03b<9 /*four categories of edu*/
replace edu=2 if qb03b>8 & qb03b<12
replace edu=3 if qb03b>11 & qb03b<17
replace edu=4 if qb03b>16 & qb03b<23

gen edu_1=0
   replace edu_1=1 if edu==1  
gen edu_2=0
   replace edu_2=1 if edu==2
gen edu_3=0
   replace edu_3=1 if edu==3
gen edu_4=0
   replace edu_4=1 if edu==4

gen schy=0 if qb03b==1 | qb03b==2  /*schooling years*/
  replace schy=qb03b-2 if qb03b>2 & qb03b<15
  replace schy=qb03b-3 if qb03b==15 | qb03b==17 | qb03b==19
  replace schy=qb03b-4 if qb03b==16 | qb03b==18 | qb03b==20
  replace schy=qb03b-2 if qb03b==21
  replace schy=qb03b-3 if qb03b==22 


gen perin=qc09/(12*qc08)   /*NATURAL LOG OF PER FAMILEY MEMBER MONTHLY INCOME*/
gen lnperin=ln(perin)


/*OCCUPATIONAL CLASSES*/
*tab qb09b
*  tab qb09b, nolabel
*  tab qa7_01  /*employment situation*/

gen occ=.
replace occ=1 if (qb09b>9 & qb09b<51) | (qb09b>109 & qb09b<311)  
replace occ=2 if (qb09b>310 & qb09b<391) | qb09b==601  
replace occ=3 if qb09b==60
replace occ=4 if (qb09b>399 & qb09b<491)  
replace occ=5 if (qb09b>601 & qb09b<879) |(qb09b>880 & qb09b<899) | qb09b==999 ///
                 | qb09b==880 | qb09b==993   
replace occ=6 if (qb09b>509 & qb09b<593) | qb09b==500 | qa7_01==7 
replace occ=7 if (qb09b>5 & qb09b<8) | qb09b==9 | qa7_01==5 
/*delete qa7_01=8, qb09_b=997 who never worked, students or soliders*/

label variable occ "occupational class"
label values occ occ5
label define occ5 1 "administrator/professional" 2 "clerk" 5 "worker" ///
                  4 "server" 3 "self-employed" 6 "peasant" 7 "unemployed"

gen adpr=0
replace adpr=1 if occ==1
gen clerk=0
replace clerk=1 if occ==2
gen slemp=0
replace slemp=1 if occ==3
gen serve=0
replace serve=1 if occ==4
gen worker=0
replace worker=1 if occ==5
gen farm=0
replace farm=1 if occ==6
gen unemp=0
replace unemp=1 if occ==7


/*Wealth, estate*/
gen house=.
replace house=1 if qc02==4 | qc02==5 | qc02==6
replace house=0 if qc02<4 | qc02>6        /*1=have properties, 0= none*/
gen house1=house
replace house1=2 if house==1 & qc05a==2   /*0=none, 1=1 property, 2=more than 1*/


gen he=edu_4
gen ss=edu_3
gen js=edu_2
gen pe=edu_1
gen swb=qe03

/*LISTWISE MISSING DATA*/
mark nomiss
markout nomiss qe03 male han age1 age2 occ ///
        party lnperin up down peerh peerl urban close edu

******Check the ignorability of case-wise deletion********
ttest qe03 if age1!=.,by(nomiss) unequal

tab occ if nomiss==1 



logit happy male han age age2 occ_1 occ_2 occ_4 occ_5 occ_6 if nomiss==1, nolog
outreg using 13a, replace
logit happy male han age age2 occ_1 occ_2 occ_4 occ_5 occ_6 close if nomiss==1, nolog
outreg using 13a, append
logit happy male han age age2 occ_1 occ_2 occ_4 occ_5 occ_6 close urban party ///
      lnperin hedu if nomiss==1, nolog
outreg using 13a, append
logit happy male han age age2 occ_1 occ_2 occ_4 occ_5 occ_6 close urban party ///
      lnperin hedu up down peerh peerl subc_1 subc_2 subc_4 if nomiss==1, nolog
outreg using 13a, append

/*Compare the results between OLS and ordinal logit models*/
reg qe03 male han age age2 occ_1 occ_2 occ_4 occ_5 occ_6 ///
    close urban party lnperin hedu up down peerh peerl if nomiss==1
ologit qe03 male han age age2 occ_1 occ_2 occ_4 occ_5 occ_6 ///
    close urban party lnperin hedu up down peerh peerl if nomiss==1,nolog
/*No differences between the two models*/


/*OLS: Class differences in SWB, hierarchical models*/
reg qe03 male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp if nomiss==1,robust
outreg2 using 13b.doc, replace

reg qe03 male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp ///
    edu_1 edu_3 edu_4 lnperin if nomiss==1,robust
outreg2 using 13b.doc, append
reg qe03 male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp ///
    edu_1 edu_3 edu_4 lnperin b0.house1 if nomiss==1,robust
    
reg qe03 male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp ///
    up down peerh peerl if nomiss==1,robust
    
reg qe03 male han age age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp ///
    edu_1 edu_3 edu_4 lnperin up down peerh peerl if nomiss==1,robust
outreg2 using 13b.doc, append

/*determinants of peer comparison*/
ologit peer male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp if nomiss==1, robust

ologit peer male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp ///
       edu_1 edu_2 edu_4 lnperin if nomiss==1, robust


/*determinants of SELF comparison*/
ologit self male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp if nomiss==1, robust

ologit self male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp ///
       edu_1 edu_2 edu_4 lnperin if nomiss==1, robust

/**/

gen y13=0

keep swb male han age1 age2 urban party health marry close adpr clerk slemp ///
     serve worker farm unemp occ he ss js pe lnperin ///
     up down peerh peerl peer self state y13 nomiss
save t2, replace

/****HLM****/
/*Generate the tax burber var on province level
Proporation of budgetary revenue over GDP by region*/
gen tax=17.5
replace tax=14.35 if qs2a==12
replace tax=8.805 if qs2a==13
replace tax=15.035 if qs2a==14
replace tax=10.605 if qs2a==15
replace tax=13.295 if qs2a==21
replace tax=10.395 if qs2a==22
replace tax=11.31 if qs2a==23
replace tax=23.51 if qs2a==31
replace tax=11.92 if qs2a==32
replace tax=12.635 if qs2a==33
replace tax=10.175 if qs2a==34
replace tax=10.175 if qs2a==35
replace tax=9.3 if qs2a==36
replace tax=9.255 if qs2a==37
replace tax=8.115 if qs2a==41
replace tax=9.605 if qs2a==42
replace tax=10.045 if qs2a==43
replace tax=12.785 if qs2a==44
replace tax=10.505 if qs2a==45
replace tax=11.235 if qs2a==46
replace tax=12.115 if qs2a==50
replace tax=9.685 if qs2a==51
replace tax=15.98 if qs2a==52
replace tax=19.725 if qs2a==53
replace tax=12.575 if qs2a==61
replace tax=11.76 if qs2a==62
replace tax=12.67 if qs2a==65


egen meaninc=mean(lnperin),by(qs2a)
gen continc=lnperin-meaninc
gen tax_inc=tax*continc

/*random intercept model*/
xtmixed qe03 male han age age2 urban party health marry close edu_1 edu_3 edu_4 ///
        continc || qs2a: if nomiss==1,var
/*random coefficient model*/
xtmixed qe03 male han age age2 urban party health marry close edu_1 edu_3 edu_4 ///
        continc || qs2a: continc if nomiss==1,var
/*intercept and income slope as outcome model*/
xtmixed qe03 male han age age2 urban party health marry close edu_1 edu_3 edu_4 ///
        continc tax tax_inc || qs2a: continc if nomiss==1,var
xtmixed qe03 male han age age2 urban party health marry close edu_1 edu_3 edu_4 ///
        adpr clerk slemp serve worker unemp ///
        continc tax tax_inc || qs2a: continc if nomiss==1,var

/*Redistribution power, indicated by the percentage of education, health care etc.
in the public expenditure. average of 2004-2005.*/

gen red=29.00
replace red=33.07 if qs2a==12
replace red=34.81 if qs2a==13
replace red=35.34 if qs2a==14
replace red=25.43 if qs2a==15
replace red=35.08 if qs2a==21
replace red=38.00 if qs2a==22
replace red=36.43 if qs2a==23
replace red=22.64 if qs2a==31
replace red=29.56 if qs2a==32
replace red=32.13 if qs2a==33
replace red=34.24 if qs2a==34
replace red=32.14 if qs2a==35
replace red=34.30 if qs2a==36
replace red=31.00 if qs2a==37
replace red=34.32 if qs2a==41
replace red=36.02 if qs2a==42
replace red=33.46 if qs2a==43
replace red=25.67 if qs2a==44
replace red=32.50 if qs2a==45
replace red=33.35 if qs2a==46
replace red=30.19 if qs2a==50
replace red=31.32 if qs2a==51
replace red=34.61 if qs2a==52
replace red=32.53 if qs2a==53
replace red=35.27 if qs2a==61
replace red=36.55 if qs2a==62
replace red=29.06 if qs2a==65

gen red_inc=red*continc

/*intercept and income slope as outcome model*/
xtmixed qe03 male han age age2 urban party health marry close edu_1 edu_3 edu_4 ///
        continc red red_inc || qs2a: continc if nomiss==1,var
xtmixed qe03 male han age age2 urban party health marry close edu_1 edu_3 edu_4 ///
        adpr clerk slemp serve worker unemp ///
        continc red red_inc || qs2a: continc if nomiss==1,var


************************************************************
************************************************************
*****2013, Subjective Wellbeing of different SES groups******
************************************************************
use "/Users/hongyb/Documents/DATA/CGSS/CGSS2013/CGSS2013.dta",clear

cd "/Users/hongyb/Documents/DATA/CGSS/CGSS2013/"


gen swb=a36                           /*subjective wellbeing*/
replace swb=. if a36==-2 | a36==-3    /*missing data*/

gen male=0  
replace male=1 if a2==1

gen han=.                    /*ethnicity*/
replace han=1 if a4==1
replace han=0 if a4>1 & a4<9

gen age=.
replace age=2013-a3a if a3b<9 & a3b>0
replace age=2012-a3a if a3b>8

gen age1=.
replace age1=age if age<70 & age>24   /*focus on those aged 25-69*/

gen age2=(age*age)/100

gen party=0
replace party=1 if a10==1

gen urban=.
replace urban=0 if a18==1 | a18==3 | a18==4
replace urban=1 if a18==2 | a18==5

gen health=.                 /*self-reported health*/
replace health=0 if a15==1 | a15==2 | a15==3
replace health=1 if a15==4 | a15==5 

gen marry=.                  /*cohabitation is considered as in marriage*/
replace marry=1 if a69==3 | a69==4 | a69==2
replace marry=0 if a69==1 | a69==5 | a69==6 | a69==7

gen close=.
replace close=1 if b5==4 | b5==5
replace close=0 if b5==1 | b5==2 | b5==3

/*Four education levels*/
gen he=0
replace he=1 if a7a>8 & a7a<15
gen ss=0
replace ss=1 if a7a>4 & a7a<9
gen js=0
replace js=1 if a7a==4
gen pe=0
replace pe=1 if a7a<4 & a7a>0

*recode a7a (1=0)(2=3)(3=6)(4=9)(5=12)(6=12)(7=12)(8=12)(9=15) ///
*           (10=15)(11=16)(12=16)(13=19)(14=.),gen(schy)


/*monthly family income*/
gen perin=.
replace perin=a62/(12*a63) if a62<9999997 & a63>0
gen lnperin=ln(perin+0.5)



/*Current occupations*/
gen isco=.
replace isco=iscorp1 if iscorp1<9999
replace isco=iscorp2 if a54==7

gen occ=.
replace occ=6 if a58==2 | a58==3 | a58==4    /*peasants*/
replace occ=7 if (a54>1 & a54<7) | a54==8    /*unemployed*/

replace occ=1 if isco<4000 & isco>0          /*managers and professionals*/
replace occ=2 if isco>3999 & isco<5000       /*clerks*/
replace occ=4 if (isco>4999 & isco<6000) | (isco>8999 & isco<9154)   /*servers*/
replace occ=5 if (isco>5999 & isco<9000) | (isco>9153 & isco<9999)   /*workers*/

replace occ=3 if a59a==2 | a59a==8           /*self-employed*/

gen adpr=occ
replace adpr=0 if occ>1
gen clerk=occ
replace clerk=0 if occ==1 | occ>2
replace clerk=1 if occ==2
gen slemp=occ
replace slemp=0 if occ<3 | occ>3
replace slemp=1 if occ==3
gen serve=occ
replace serve=0 if occ<4 | occ>4
replace serve=1 if occ==4
gen worker=occ
replace worker=0 if occ<5 | occ>5
replace worker=1 if occ==5
gen farm=occ
replace farm=0 if occ<6 | occ==7
replace farm=1 if occ==6
gen unemp=occ
replace unemp=0 if occ<7
replace unemp=1 if occ==7

/*Wealth, estate*/
gen house=.
replace house=0 if a65==0
replace house=1 if a65>0 & a65<11
gen house1=house
replace house1=2 if a65>1 & a65<11


/*Peer comparison*/
gen peerh=.
replace peerh=1 if b1==1
replace peerh=0 if b1==2 | b1==3 | b1==9
gen peerl=.
replace peerl=1 if b1==3
replace peerl=0 if b1==1 | b1==2 | b1==9

recode b1(1=3)(2=2)(3=1)(9=2),gen(peer)

/*self comparison*/
gen up=.
replace up=1 if b2==1
replace up=0 if b2==2 | b2==3 | b2==9
gen down=.
replace down=1 if b2==3
replace down=0 if b2==1 | b2==2 | b2==9

recode b2(1=3)(2=2)(3=1)(9=2),gen(self)


/*casewise deletion*/
mark nomiss
markout nomiss swb male han age1 urban party health marry close occ ///
        he ss js pe lnperin up down peerh peerl

******Check the ignorability of case-wise deletion********
ttest swb if age1!=.,by(nomiss) unequal
ttest male,by(nomiss) unequal
ttest han,by(nomiss) unequal
ttest age,by(nomiss) unequal
ttest urban,by(nomiss) unequal
ttest party,by(nomiss) unequal
ttest health,by(nomiss) unequal
ttest marry,by(nomiss) unequal
ttest close,by(nomiss) unequal
ttest lnperin,by(nomiss) unequal

tab1 he ss js pe if nomiss==0


/*OLS on SWB*/
reg swb male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp if nomiss==1, robust

reg swb male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp he ss pe lnperin if nomiss==1, robust

reg swb male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp he ss pe lnperin b1.house1 if nomiss==1, robust

reg swb male han age1 age2 urban party health marry close ///
    adpr clerk slemp serve worker unemp up down peerh peerl if nomiss==1, robust

/*determinants of peer comparison*/
ologit peer male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp if nomiss==1, robust

ologit peer male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp ///
       he ss pe lnperin if nomiss==1, robust


/*determinants of SELF comparison*/
ologit self male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp if nomiss==1, robust

ologit self male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp ///
       he ss pe lnperin if nomiss==1, robust



/**Interactions & OGLM**/
gen y13=1
keep swb male han age1 age2 urban party health marry close adpr clerk slemp ///
     serve worker farm unemp occ he ss js pe lnperin ///
     up down peerh peerl state peer self state y13 nomiss
save t1, replace
append using t2
save h05_13,replace

use h05_13,clear

gen adpr13=adpr*y13
gen clerk13=clerk*y13
gen slemp13=slemp*y13
gen serve13=serve*y13
gen work13=worker*y13
gen farm13=farm*y13
gen unemp13=unemp*y13


ologit peer male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp y13 ///
       adpr13 clerk13 slemp13 serve13 work13 unemp13 if nomiss==1, robust

oglm peer male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp y13 ///
       adpr13 clerk13 slemp13 serve13 work13 unemp13 if nomiss==1, ///
       scale(y13) robust

ologit self male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp y13 ///
       adpr13 clerk13 slemp13 serve13 work13 unemp13 if nomiss==1, robust

oglm self male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp y13 ///
       adpr13 clerk13 slemp13 serve13 work13 unemp13 if nomiss==1, ///
       scale(y13) robust

reg swb male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp y13 ///
       adpr13 clerk13 slemp13 serve13 work13 unemp13 if nomiss==1, robust
ologit swb male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp y13 ///
       adpr13 clerk13 slemp13 serve13 work13 unemp13 if nomiss==1, robust
oglm swb male han age1 age2 urban party health marry close ///
       adpr clerk slemp serve worker unemp y13 ///
       adpr13 clerk13 slemp13 serve13 work13 unemp13 if nomiss==1, ///
       scale(y13) robust

/*T-test*/
ttest swb if nomiss==1 & adpr==1,by(y13) unequal
ttest swb if nomiss==1 & clerk==1,by(y13) unequal
ttest swb if nomiss==1 & slemp==1,by(y13) unequal
ttest swb if nomiss==1 & serve==1,by(y13) unequal
ttest swb if nomiss==1 & work==1,by(y13) unequal
ttest swb if nomiss==1 & farm==1,by(y13) unequal
ttest swb if nomiss==1 & unemp==1,by(y13) unequal

/*Descriptives*/
sum swb male han age1 urban party health marry close lnperin ///
    if nomiss==1 & y13==0
tab1 occ he ss js pe peer self if nomiss==1 & y13==0

sum swb male han age1 urban party health marry close lnperin ///
    if nomiss==1 & y13==1
tab1 occ he ss js pe peer self if nomiss==1 & y13==1

tabstat swb if nomiss==1 & y13==0,stats(mean) by(occ)
tabstat swb if nomiss==1 & y13==1,stats(mean) by(occ)



/****HLM****/
/*Generate the tax burber var on province level
Proporation of budgetary revenue over GDP by region*/
gen tax=23.9
replace tax=18.90 if s41==12
replace tax=11.45 if s41==13
replace tax=18.80 if s41==14
replace tax=13.75 if s41==15
replace tax=17.15 if s41==21
replace tax=13.70 if s41==22
replace tax=13.25 if s41==23
replace tax=29.20 if s41==31
replace tax=15.55 if s41==32
replace tax=15.50 if s41==33
replace tax=14.75 if s41==34
replace tax=13.10 if s41==35
replace tax=14.40 if s41==36
replace tax=11.65 if s41==37
replace tax=10.00 if s41==41
replace tax=12.50 if s41==42
replace tax=12.25 if s41==43
replace tax=16.30 if s41==44
replace tax=12.60 if s41==45
replace tax=17.35 if s41==50
replace tax=13.85 if s41==51
replace tax=21.00 if s41==52
replace tax=22.95 if s41==53
replace tax=16.85 if s41==61
replace tax=16.05 if s41==62
replace tax=15.25 if s41==63
replace tax=17.25 if s41==64


egen meaninc=mean(lnperin),by(s41)
gen continc=lnperin-meaninc
gen tax_inc=tax*continc

/*random intercept model*/
xtmixed swb male han age age2 urban party health marry close pe ss he ///
        continc || s41: if nomiss==1,var
/*random coefficient model*/
xtmixed swb male han age age2 urban party health marry close pe ss he ///
        continc || s41: continc if nomiss==1,var
/*intercept and income slope as outcome model*/
xtmixed swb male han age age2 urban party health marry close pe ss he ///
        continc tax tax_inc || s41: continc if nomiss==1,var
xtmixed swb male han age age2 urban party health marry close pe ss he ///
        adpr clerk slemp serve worker unemp ///
        continc tax tax_inc || s41: continc if nomiss==1,var

/*Redistribution power, indicated by the percentage of education, health care etc.
in the public expenditure*/

gen red=44.14
replace red=37.39 if s41==12
replace red=42.81 if s41==13
replace red=42.82 if s41==14
replace red=34.19 if s41==15
replace red=38.79 if s41==21
replace red=39.25 if s41==22
replace red=39.55 if s41==23
replace red=38.08 if s41==31
replace red=38.81 if s41==32
replace red=42.59 if s41==33
replace red=41.94 if s41==34
replace red=38.97 if s41==35
replace red=40.71 if s41==36
replace red=43.03 if s41==37
replace red=45.90 if s41==41
replace red=41.74 if s41==42
replace red=41.28 if s41==43
replace red=41.40 if s41==44
replace red=41.32 if s41==45
replace red=36.76 if s41==50
replace red=41.54 if s41==51
replace red=36.87 if s41==52
replace red=40.13 if s41==53
replace red=44.04 if s41==61
replace red=42.27 if s41==62
replace red=34.61 if s41==63
replace red=31.48 if s41==64

gen red_inc=red*continc

/*intercept and income slope as outcome model*/
xtmixed swb male han age age2 urban party health marry close pe ss he ///
        continc red red_inc || s41: continc if nomiss==1,var
xtmixed swb male han age age2 urban party health marry close pe ss he ///
        adpr clerk slemp serve worker unemp ///
        continc red red_inc || s41: continc if nomiss==1,var
