
**表1**
use "E:\share\hospital.dta",clear
tabstat chouzi time chouzi1 c_time rate ///
shouru medicalfee self_fee health adl ///
wealth disease healthchild healthshock ge010_7 smoke drink ///
renshu kids older age edu marriage xrgender ///
medicalnum jj016 jd003,s(mean sd min max) c(s)


***表2***
use "E:\share\total.dta",clear

foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' ,by(xinnonghe) 
}

foreach v of varlist disease healthchild wealth {
  ttest `v' ,by(xinnonghe) 
}

foreach v of varlist medicalfee self_fee health adl shouru {
  ttest `v' ,by(xinnonghe) unequal
}


**表3**
use "E:\share\hospital.dta",clear
foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' ,by(xinnonghe) 
}

foreach v of varlist medicalfee health adl healthchild {
  ttest `v' ,by(xinnonghe) 
}

foreach v of varlist self_fee shouru disease wealth {
  ttest `v' ,by(xinnonghe) unequal
}

**表4**
foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' if age>=60 ,by(xinnonghe) 
}

foreach v of varlist health adl healthchild {
  ttest `v' if age>=60 ,by(xinnonghe) 
}

foreach v of varlist medicalfee self_fee shouru disease wealth {
  ttest `v' if age>=60 ,by(xinnonghe) unequal
}

foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' if age<60 ,by(xinnonghe) 
}

foreach v of varlist health adl healthchild {
  ttest `v' if age<60 ,by(xinnonghe) 
}

foreach v of varlist medicalfee self_fee shouru disease wealth {
  ttest `v' if age<60 ,by(xinnonghe) unequal
}

**表5**
gen bad=1 if da007_4_==1 | da007_7_ ==1 | da007_8_==1 | da007_11_==1 | da007_12_==1
gen severe=1 if da007_4_==1 | da007_7_ ==1 | da007_8_==1 | da007_11_==1 | da007_12_==1	 
replace severe=1 if disease>1

foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' if disease<=1 & bad!=1 ,by(xinnonghe) 
}

foreach v of varlist health shouru disease healthchild {
  ttest `v' if disease<=1 & bad!=1 ,by(xinnonghe) 
}

foreach v of varlist medicalfee self_fee adl wealth {
  ttest `v' if disease<=1 & bad!=1 ,by(xinnonghe) unequal
}

foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' if severe==1 ,by(xinnonghe) 
}

foreach v of varlist shouru disease {
  ttest `v' if severe==1 ,by(xinnonghe) unequal
}

foreach v of varlist medicalfee self_fee health adl healthchild wealth {
  ttest `v' if severe==1 ,by(xinnonghe) 
}

**表6**
egen gwealth11=median(wealth) if year==2011
egen gwealth13=median(wealth) if year==2013

gen gwealth=1 if wealth>=gwealth11 & year==2011
replace gwealth=1 if wealth>=gwealth13 & year==2013
replace gwealth=0 if gwealth==.


****较富裕群体中是否参加新农合均值检验
foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' if gwealth==1 ,by(xinnonghe) 
}

foreach v of varlist adl disease wealth {
  ttest `v' if gwealth==1 ,by(xinnonghe) unequal
}

foreach v of varlist medicalfee self_fee health shouru healthchild {
  ttest `v' if gwealth==1 ,by(xinnonghe) 
}

****不富裕群体中是否参加新农合均值检验
foreach v of varlist medicalfee self_fee health adl shouru disease healthchild wealth {
  sdtest `v' if gwealth==0 ,by(xinnonghe) 
}

foreach v of varlist self_fee shouru {
  ttest `v' if gwealth==0 ,by(xinnonghe) unequal
}

foreach v of varlist medicalfee health adl disease healthchild wealth {
  ttest `v' if gwealth==0 ,by(xinnonghe) 
}

**表7**
use "E:\share\total.dta",clear

gen hospital=1 if  medicalfee!=0			
replace hospital=0 if hospital==.

sort ID
set seed 10101
gen random=1+int((10000-1)*uniform())
sort random

mat attrl=J(6,2,0)
local j=1
local k=1

foreach v of varlist hospital medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year, ///
         n(3) logit ate ties common out(`v')
         mat attrl[`k',1+3*(`j'-1)]=r(att)
         mat attrl[`k',2+3*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}


**图2**
use "E:\share\hospital.dta",clear
sort ID
set seed 10101
gen random=1+int((10000-1)*uniform())
sort random


mat attrl=J(5,6,0)
local j=1
local k=1

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year, ///
         n(3) logit ate ties common out(`v')
         mat attrl[`k',1+3*(`j'-1)]=r(att)
         mat attrl[`k',2+3*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}
mat list attrl
pstest               c_time rate chouzi1 medicalnum ///
marriage age xrgender kids older disease wealth renshu,both graph //检验匹配的平衡性语句

**表8**
use "E:\share\hospital.dta",clear
sort ID
set seed 10101
gen random=1+int((10000-1)*uniform())
sort random


mat attrl=J(5,6,0)
local j=1
local k=1

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year, ///
         n(3) logit ate ties common out(`v')
         mat attrl[`k',1+3*(`j'-1)]=r(att)
         mat attrl[`k',2+3*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}
mat list attrl


*************************
*         年龄          *
*************************

*2.年龄较大群体
local j=2
local k=1		

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year if age>=60, ///
         n(3) logit ate ties out(`v')
mat attrl[`k',1+2*(`j'-1)]=r(att)
mat attrl[`k',2+2*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}
mat list attrl

*3.年龄较小群体		 
local j=3
local k=1		

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year if age<60, ///
         n(3) logit ate ties common out(`v')
mat attrl[`k',1+2*(`j'-1)]=r(att)
mat attrl[`k',2+2*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}
mat list attrl
svmat attrl 

*************************
*         财富          *
*************************
use "E:\share\hospital.dta",clear

egen gwealth11=median(wealth) if year==2011
egen gwealth13=median(wealth) if year==2013

gen gwealth=1 if wealth>=gwealth11 & year==2011
replace gwealth=1 if wealth>=gwealth13 & year==2013
replace gwealth=0 if gwealth==.


sort ID
set seed 10101
gen random=1+int((10000-1)*uniform())
sort random


mat attrl=J(5,4,0)
local j=1
local k=1

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year if gwealth==0, ///
         n(3) logit ate ties out(`v')
         mat attrl[`k',1+2*(`j'-1)]=r(att)
         mat attrl[`k',2+2*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}


*5.财富较高群体
local j=2
local k=1

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year if gwealth==1, ///
         n(3) logit ate ties out(`v')
         mat attrl[`k',1+2*(`j'-1)]=r(att)
         mat attrl[`k',2+2*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}
mat list attrl

*************************
*       患病程度        *
*************************

*6.患病较轻群体
use "E:\share\hospital.dta",clear
gen bad=1 if da007_4_==1 | da007_7_ ==1 | da007_8_==1 | da007_11_==1 | da007_12_==1
 
keep if disease<=1 & bad!=1

set seed 10101
gen random=1+int((10000-1)*uniform())
sort random


mat attrl=J(5,2,0)
local j=1
local k=1

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year, ///
         n(3) logit ate ties common out(`v')
         mat attrl[`k',1+3*(`j'-1)]=r(att)
         mat attrl[`k',2+3*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}
mat list attrl
svmat attrl //导出到数据库


*6.患病较重群体
use "E:\share\hospital.dta",clear

gen bad=1 if da007_4_==1 | da007_8_==1 | da007_11_==1 | da007_12_==1
replace bad=1 if disease>1
keep if bad==1

set seed 10101
gen random=1+int((10000-1)*uniform())
sort random


mat attrl=J(5,2,0)
local j=1
local k=1

foreach v of varlist medicalfee self_fee health adl shouru {
psmatch2 xinnonghe age marriage chouzi1 xrgender i.edu wealth kids older disease ///
         renshu medicalnum c_time rate i.areatype i.prov i.year, ///
         n(3) logit ate ties out(`v')
         mat attrl[`k',1+3*(`j'-1)]=r(att)
         mat attrl[`k',2+3*(`j'-1)]=r(att)/r(seatt)
local k=`k'+1
}
mat list attrl


**表9**
use "E:\share\hospital.dta",clear

reg health time healthchild disease ///
wealth medicalnum smoke drink marriage age kids older renshu ///
xrgender jj016 jd003 i.edu i.areatype i.prov i.year,r

reg adl time healthchild disease ///
wealth medicalnum smoke drink marriage age kids older renshu ///
xrgender jj016 jd003 i.edu i.areatype i.prov i.year,r


**表10**

  **总医疗支出
use "E:\share\hospital.dta",clear
egen gwealth11=median(wealth) if year==2011
egen gwealth13=median(wealth) if year==2013

gen gwealth=1 if wealth>=gwealth11 & year==2011
replace gwealth=1 if wealth>=gwealth13 & year==2013
replace gwealth=0 if gwealth==.

gen bad=1 if da007_4_==1 | da007_7_ ==1 | da007_8_==1 | da007_11_==1 | da007_12_==1
gen severe=1 if da007_4_==1 | da007_7_ ==1 | da007_8_==1 | da007_11_==1 | da007_12_==1	 
replace severe=1 if disease>1

reg medicalfee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year,r

reg medicalfee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60,r

reg medicalfee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60,r 

reg medicalfee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1,r

reg medicalfee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1,r 

reg medicalfee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0,r 

reg medicalfee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1,r
est store m7ols

*2.自付医疗支出

reg self_fee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year,r


reg self_fee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60,r


reg self_fee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60,r


reg self_fee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1,r


reg self_fee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1,r


reg self_fee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0,r


reg self_fee xinnonghe wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1,r

     *自评健康

reg health xinnonghe healthchild smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year,r


reg health xinnonghe healthchild smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60,r


reg health xinnonghe healthchild smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60,r


reg health xinnonghe healthchild smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1,r


reg health xinnonghe healthchild smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1,r


reg health xinnonghe healthchild smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0,r


reg health xinnonghe healthchild smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1,r

  *ADL
reg adl xinnonghe healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year,r


reg adl xinnonghe healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60,r

reg adl xinnonghe healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60,r


reg adl xinnonghe healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1,r


reg adl xinnonghe healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1,r


reg adl xinnonghe healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0,r


reg adl xinnonghe healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1,r


  **个人收入
reg shouru xinnonghe renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age  xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year,r


reg shouru xinnonghe renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if age>=60,r


reg shouru xinnonghe renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if age<60,r


reg shouru xinnonghe renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if disease<=1 & bad!=1,r


reg shouru xinnonghe renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if severe==1,r


reg shouru xinnonghe renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if gwealth==0,r


reg shouru xinnonghe renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if gwealth==1,r


**表11**
use "E:\share\hospital.dta",clear
egen gwealth11=median(wealth) if year==2011
egen gwealth13=median(wealth) if year==2013

gen gwealth=1 if wealth>=gwealth11 & year==2011
replace gwealth=1 if wealth>=gwealth13 & year==2013
replace gwealth=0 if gwealth==.


gen bad=1 if da007_4_==1 | da007_7_ ==1 | da007_8_==1 | da007_11_==1 | da007_12_==1
gen severe=1 if da007_4_==1 | da007_7_ ==1 | da007_8_==1 | da007_11_==1 | da007_12_==1	 
replace severe=1 if disease>1


xi:ivreg2 medicalfee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year


xi:ivreg2 medicalfee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60


xi:ivreg2 medicalfee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60


xi:ivreg2 medicalfee (xinnonghe= c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1


xi:ivreg2 medicalfee (xinnonghe=c_time chouzi1) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1


xi:ivreg2 medicalfee (xinnonghe=c_time chouzi1) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0



xi:ivreg2 medicalfee (xinnonghe=c_time chouzi1) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1


*2.自付医疗支出

xi:ivreg2 self_fee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year


xi:ivreg2 self_fee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60
 

xi:ivreg2 self_fee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7 ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60


xi:ivreg2 self_fee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1


xi:ivreg2 self_fee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1


xi:ivreg2 self_fee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0


xi:ivreg2 self_fee (xinnonghe=c_time chouzi1 rate) wealth ///
disease healthchild healthshock smoke drink ge010_7  ///
renshu marriage age kids older xrgender /// 
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1


     *自评健康

xi:ivreg2 health (xinnonghe=c_time chouzi1 rate) healthchild  smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year


xi:ivreg2 health (xinnonghe=c_time chouzi1 rate) healthchild  smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60


xi:ivreg2 health (xinnonghe=c_time chouzi1 rate) healthchild  smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60


xi:ivreg2 health (xinnonghe=c_time chouzi1 rate) healthchild  smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1


xi:ivreg2 health (xinnonghe=c_time chouzi1 rate) healthchild  smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1


xi:ivreg2 health (xinnonghe=c_time chouzi1 rate) healthchild  smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0


xi:ivreg2 health (xinnonghe=c_time chouzi1 rate) healthchild  smoke drink disease ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1


  *ADL

xi:ivreg2 adl (xinnonghe=c_time chouzi1 rate) healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year 


xi:ivreg2 adl (xinnonghe=c_time chouzi1 rate) healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age>=60


xi:ivreg2 adl (xinnonghe=c_time chouzi1 rate) healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if age<60


xi:ivreg2 adl (xinnonghe=c_time chouzi1 rate) healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if disease<=1 & bad!=1


xi:ivreg2 adl (xinnonghe=c_time chouzi1 rate) healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if severe==1


xi:ivreg2 adl (xinnonghe=c_time chouzi1 rate) healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==0


xi:ivreg2 adl (xinnonghe=c_time chouzi1 rate) healthchild disease smoke drink ///
wealth xrgender marriage age kids older renshu ///
medicalnum jj016 jd003 i.edu i.prov i.areatype i.year if gwealth==1 


  **个人收入

xi:ivreg2 shouru (xinnonghe=c_time chouzi1 rate)renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year


xi:ivreg2 shouru (xinnonghe=c_time chouzi1 rate)renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if age>=60


xi:ivreg2 shouru (xinnonghe=c_time chouzi1 rate)renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if age<60


xi:ivreg2 shouru (xinnonghe=c_time chouzi1 rate)renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if disease<=1 & bad!=1


xi:ivreg2 shouru (xinnonghe=c_time chouzi1 rate)renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if severe==1


xi:ivreg2 shouru (xinnonghe=c_time chouzi1 rate)renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if gwealth==0


xi:ivreg2 shouru (xinnonghe=c_time chouzi1 rate)renshu kids older wealth ///
smoke drink disease healthshock healthchild ///
marriage age xrgender i.edu ///
jj016 jd003 i.prov i.areatype i.year if gwealth==1










