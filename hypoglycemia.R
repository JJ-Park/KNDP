setwd('/Users/blue0plus/project/KNDP')

library(data.table)
library(ggplot2)
library(reshape2)

#저혈당 질병 코드
HYPOGLYCEMIA.CODE = c('E1063', 'E1163', 'E1263', 'E1363', 'E1463', 'E160', 'E161', 'E162')

dt.raw = read.csv('data/hira_1%/nps_diabete')
dt.raw = data.table(dt.raw)

dt.dbt = dt.raw[ MAIN_SICK %like% 'E1[0-4][0-9]*' | SUB_SICK %like% 'E1[0-4][0-9]*']

#첫 저혈당 진단 날짜 데이터에 추가하기
dt.hypo = dt.dbt[ MAIN_SICK %in% HYPOGLYCEMIA.CODE | SUB_SICK %in% HYPOGLYCEMIA.CODE , ]
first.date.hypo = tapply(dt.hypo$RECU_FR_DT, dt.hypo$NO, min)

#dt.dbt$FIRST_DATE_HYPO = first.date.hypo[as.character(dt.dbt$NO)]		시스템 다운...왜;;

getFirstDateHypo = function(id){
	first.date.hypo[ as.character(id) ]
}
dt.dbt$FIRST_DATE_HYPO = sapply(dt.dbt$NO, getFirstDateHypo)

#저혈당 발생 그룹과 저혈당 비발생 그룹으로 나눔
NO.hypo = unique(dt.hypo$NO)
dt.hypo = dt.dbt[ NO %in% NO.hypo ]					#dt.hypo overwrite
dt.not.hypo = dt.dbt[ ! NO %in% NO.hypo ]

#환자 수 비교
length(unique(dt.dbt$NO))							#당뇨환자 수
length(unique(dt.hypo$NO))							#저혈당 환자 수
length(unique(dt.not.hypo$NO))						#비저혈당 환자 수

#나이 비교
age.hypo = dt.hypo[ , list(AGE=mean(age)), by=NO ]
age.not.hypo = dt.not.hypo[ , list(AGE=mean(age)), by=NO ]
ggdata.age = stack(list(hypo=age.hypo[ , AGE ], non_hypo=age.not.hypo[ , AGE ]))

boxplot.age = ggplot(ggdata.age, aes(x=ind, y=values, fill=ind)) + geom_boxplot()
boxplot.age + labs(title='Age',  y='Age') + theme(axis.title.x=element_blank()) + guides(fill=F) + annotate('text', x=0.7, y=90, label=paste('mean=', format(mean(age.hypo$AGE), digits=3), sep='')) + annotate('text', x=2.3, y=40, label=paste('mean=', format(mean(age.not.hypo$AGE), digits=3), sep='')) # + coord_cartesian(ylim = quantile(age.not.hypo$AGE, c(0.05, 0.95)))
 
t.test(age.hypo$AGE, age.not.hypo$AGE)

###
#		저혈당 환자 의료비 비교
###

#의료비 비교 - 환자 기준
cost.person.hypo = dt.hypo[ , list(COST=sum(DMD_TRAMT)), by=NO ]
cost.person.not.hypo = dt.not.hypo[ , list(COST=sum(DMD_TRAMT)), by=NO ]

ggdata.cost.person = stack(list(hypo=cost.person.hypo[ , COST ], non_hypo=cost.person.not.hypo[ , COST ]))
boxplot.cost.person = ggplot(ggdata.cost.person, aes(x=ind, y=values, fill=ind)) + geom_boxplot(outlier.shape=NA)
boxplot.cost.person + labs(title='Medical costs/person',  y='Cost') + theme(axis.title.x=element_blank()) + guides(fill=F) + annotate('text', x=0.7, y=3000000, label=paste('mean=', format(mean(cost.person.hypo$COST), big.mark=','))) + annotate('text', x=2.3, y=mean(cost.person.not.hypo$COST), label=paste('mean=', format(mean(cost.person.not.hypo$COST), big.mark=','))) + coord_cartesian(ylim = c(0, quantile(cost.person.not.hypo$COST, 0.95)))

t.test(values~ind, data=ggdata.cost.person)

#의료비 비교 - 치료 기준
nrow(dt.hypo)/length(unique(dt.hypo$NO))								#저혈당 평균 처방건 수
nrow(dt.not.hypo)/length(unique(dt.not.hypo$NO))		#비저혈당 평균 처방건 수

ggdata.cost.treatment = stack(list(hypo=dt.hypo$DMD_TRAMT, non_hypo=dt.not.hypo$DMD_TRAMT))
boxplot.cost.treatment = ggplot(ggdata.cost.treatment, aes(x=ind, y=values, fill=ind)) + geom_boxplot(outlier.shape=NA)
boxplot.cost.treatment + labs(title='Medical costs/treatment',  y='Cost') + theme(axis.title.x=element_blank()) + guides(fill=F) + annotate('text', x=0.7, y=60000, label=paste('mean=', format(mean(dt.hypo$DMD_TRAMT), digits=3, big.mark=','))) + annotate('text', x=2.3, y=45000, label=paste('mean=', format(mean(dt.not.hypo$DMD_TRAMT), digits=3, big.mark=','))) + coord_cartesian(ylim = c(0, quantile(dt.not.hypo$DMD_TRAMT, 0.9)))

t.test(values~ind, data=ggdata.cost.treatment)


###
#		병원지속 이용률과 저혈당
###

dt.hypo[RECU_FR_DT<FIRST_DATE_HYPO, DT_BF_HYPO:=max(RECU_FR_DT), by=NO]		#저혈당 진단 직전 날짜 추가

#저혈당 발생 직전 병원 입내원 일 수
VSCN.before.hypo = dt.hypo[RECU_FR_DT==DT_BF_HYPO, max(VSCN), by=NO]$V1				#같은 날짜에 명세서가 둘 이상일 경우 입내원 일 수가 긴 것 선택
VSCN.general = dt.dbt$VSCN

t.test(VSCN.before.hypo, VSCN.general)	

#저혈당 발생 직전 요양 일 수
RECN.before.hypo = dt.hypo[RECU_FR_DT==DT_BF_HYPO, max(RECN), by=NO]$V1
RECN.general = dt.dbt$RECN

t.test(RECN.before.hypo, dt.dbt$RECN)		#저혈당 진단 직전 vs 당뇨 평균 요양 일
t.test(dt.hypo[RECU_FR_DT<FIRST_DATE_HYPO, RECN], dt.hypo[RECU_FR_DT>=FIRST_DATE_HYPO, RECN])		#저혈당 발생 그룹에서 저혈당 진단 이전 vs 진단 이후 평균 요양 일

ggdata.VSCN.before.hypo = stack(list(hypoglycemia=VSCN.before.hypo, general=VSCN.general))

boxplot.VSCN.before.hypo = ggplot(ggdata.VSCN.before.hypo, aes(x=ind, y=values, fill=ind)) + geom_boxplot(outlier.shape=NA)
boxplot.VSCN.before.hypo + labs(title='hospital adherence comparison',  y='Days') + theme(axis.title.x=element_blank()) + guides(fill=F) + annotate('text', x=0.7, y=mean(VSCN.before.hypo), label=paste('mean=', format(mean(VSCN.before.hypo), big.mark=','))) + annotate('text', x=2.3, y=mean(VSCN.general), label=paste('mean=', format(mean(VSCN.general), big.mark=','))) + coord_cartesian(ylim = c(0, quantile(VSCN.general, 0.95)))


#입원환자 외래환자 구분: FST_IN_PAT_DT(최초입원일) 이 0 이면 외래 아니면 입원
